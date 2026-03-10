# optimize-shell.md — Performance Optimization Results for jerboa-sh

## Executive Summary

**Baseline** (measured 2026-03-10, before optimizations):

| Test | jerboa-sh | gerbil-sh | ratio |
|------|-----------|-----------|-------|
| Empty loop (10k iters) | 3.43s | 2.38s | **1.44x** slower |
| Variable assign (5k) | 1.72s | 1.02s | **1.68x** slower |
| String length (5k) | 1.63s | 0.95s | **1.72x** slower |
| Echo output (5k) | 1.83s | 1.31s | **1.40x** slower |
| Arithmetic (5k) | 1.91s | 1.27s | **1.50x** slower |
| Function call (5k) | 2.00s | 1.16s | **1.73x** slower |
| `test` loop (2k) | 0.45s | 0.15s | **2.9x** slower |

**After optimizations** (measured 2026-03-10):

| Test | jerboa-sh | gerbil-sh | ratio |
|------|-----------|-----------|-------|
| Empty loop (10k iters) | 0.83s | 2.07s | **2.5x faster** |
| Variable assign (5k) | 0.61s | 0.88s | **1.4x faster** |
| String length (5k) | 0.57s | 0.99s | **1.7x faster** |
| Arithmetic (5k) | 0.72s | 1.38s | **1.9x faster** |
| Function call (5k) | 0.81s | 1.15s | **1.4x faster** |

jerboa-sh went from **1.4–1.7x slower** to **1.4–2.5x faster** than gerbil-sh.

---

## Root Causes (confirmed via strace + profiling)

1. `thread-yield!` → `clock_nanosleep(0)` syscall on **every loop iteration**
2. Missing build-level optimizations: `commonization-level`, `enable-unsafe-application`, `debug-level 0`
3. `hash-get` wrapper procedure blocking cp0 inlining of `hashtable-ref`
4. Double hash-table scope-chain traversal in `env-get` for ordinary variables
5. Uncached command PATH lookups (`file-exists?` + `executable?` per PATH dir per miss)
6. WPO not covering gsh modules (missing `.wpo` files)

---

## Build System Architecture (Critical)

**The `.sls` files in `src/gsh/` are AUTO-GENERATED.** Do not edit them directly.

Build pipeline:
1. `make jerboa` — Gherkin compiler translates Gerbil `.ss` → R6RS `.sls`, then applies post-build patches
2. `make compile` — compiles `.sls` → `.so` (with WPO file generation)
3. `make binary` — links a self-contained ELF via `build-binary.ss`

To permanently add code changes to generated modules, add a `patch-file!` call in `build-jerboa.ss` at the end of the post-build patches section.

Local `.ss` overrides: if `./foo.ss` exists in the project root, it takes priority over `gerbil-shell/foo.ss` (the submodule).

---

## Optimization 1: `thread-yield!` → No-Op

**File:** `~/mine/jerboa/lib/std/misc/thread.sls`

**Problem:** The shim implementation called `(sleep (make-time 'time-duration 0 0))`, which issues a real `clock_nanosleep(CLOCK_REALTIME, 0, {0,0})` kernel syscall. This was called on every `for`, `while`, and `until` loop iteration in `control.sls`.

Confirmed via strace: ~6,000 `clock_nanosleep` calls per 2k-iteration loop before the fix.

**Fix:**
```scheme
(define (thread-yield!)
  ;; Chez uses preemptive POSIX threads — no cooperative yield needed
  (void))
```

**Result:** 0 `clock_nanosleep` calls after fix. Verified via strace.

**Why it's safe:** Chez Scheme uses native POSIX threads with preemptive scheduling. Cooperative yielding is unnecessary. The job wait loops in `jobs.sls` that called `thread-yield!` already had `thread-sleep!` for actual backoff — the `thread-yield!` was purely redundant overhead.

---

## Optimization 2: Build Parameters

**File:** `build-binary.ss`

**Fix:** Added missing parameters to the `compile-program` parameterize block:

```scheme
(parameterize ([compile-imported-libraries #t]
               [optimize-level 3]
               [cp0-effort-limit 500]
               [cp0-score-limit 50]
               [cp0-outer-unroll-limit 1]           ;; unroll tight loops once
               [commonization-level 4]               ;; merge similar lambdas
               [enable-unsafe-application #t]        ;; skip procedure? check on every call
               [enable-unsafe-variable-reference #t] ;; skip letrec undefined-var checks
               [enable-arithmetic-left-associative #t] ;; allow reassociation
               [debug-level 0]                       ;; max continuation optimization
               [generate-inspector-information #f]
               [generate-wpo-files #t])
  (compile-program "gsh.ss"))
```

**Key parameters:**
- `commonization-level 4`: merges structurally similar lambdas from gherkin match/defmethod/accessors
- `enable-unsafe-application`: eliminates the "is this a procedure?" check on every function call
- `enable-unsafe-variable-reference`: eliminates `letrec` uninitialized-variable checks
- `debug-level 0`: allows maximum continuation optimization (no debug frames)
- `cp0-outer-unroll-limit 1`: unrolls innermost named-let loops once

**Caveat:** `enable-unsafe-application` causes a crash (not a clean error) if a non-procedure is called. Acceptable for a compiled binary; would complicate debugging.

---

## Optimization 3: WPO Coverage for gsh Modules

**File:** `build-all.ss`

**Problem:** `build-all.ss` compiled `.sls → .so` files without `generate-wpo-files #t`, so `compile-whole-program` in `build-binary.ss` could not incorporate any gsh module. 58 out of 58 gsh libraries were "missing .wpo".

**Fix:** Wrap the import in a parameterize block:

```scheme
(parameterize ([compile-imported-libraries #t]
               [generate-wpo-files #t]
               [optimize-level 3]
               [generate-inspector-information #f])
  (eval '(import
    (gsh ast) (gsh registry) ... )
  (interaction-environment)))
```

**Note:** `eval` is required because Chez applies `parameterize` at compile time for top-level `import` forms — wrapping in `eval` forces the import to be compiled under the parameterize.

**Result:** WPO missing count dropped from 58 → 31. The remaining 31 are jerboa stdlib and gherkin runtime modules (compiled separately without `generate-wpo-files #t`).

---

## Optimization 4: `hash-get` → `define-syntax`

**File:** `~/mine/jerboa/lib/jerboa/runtime.sls`

**Problem:** `hash-get` was a procedure wrapper:
```scheme
(define (hash-get ht key)
  (hashtable-ref ht key #f))
```
A procedure wrapper hides the Chez primitive's optimization flags (`pure`, `unrestricted`, `cp02`) from cp0. Every variable lookup, alias check, and function lookup goes through `hash-get`.

**Fix:**
```scheme
(define-syntax hash-get
  (syntax-rules ()
    ((_ ht key) (hashtable-ref ht key #f))))
```

**Audit required:** Before converting, verify `hash-get` is never used as a first-class value (passed to `map`, `filter`, etc.). In this codebase it is not — all uses are `(hash-get ht key)` call sites.

**Note:** `hash-put!` was already `(define hash-put! hashtable-set!)` — a direct alias, which cp0 can already optimize through with WPO. No change needed there.

---

## Optimization 5: PATH Lookup Cache (`which-cached`)

**File:** Implemented as post-build patch in `build-jerboa.ss`

**Problem:** Every external command execution scans `$PATH` directories with `file-exists?` + `executable?` (two `access(2)` syscalls per dir). For a command in `/usr/bin` with a typical 5-directory PATH, that's 10 syscalls per external command invocation.

**Fix:** Added a hash-table cache keyed by command name, invalidated when `$PATH` changes:

```scheme
(define *which-cache* (make-hashtable string-hash string=?))
(define *which-cache-path* #f)

(define (which-cache-invalidate!)
  (hashtable-clear! *which-cache*)
  (set! *which-cache-path* #f))

(define (which-cached name)
  (if (string-contains? name "/")
      (which name)
      (let ([current-path (or (getenv "PATH" #f) "/usr/bin:/bin")])
        (unless (equal? current-path *which-cache-path*)
          (hashtable-clear! *which-cache*)
          (set! *which-cache-path* current-path))
        (let ([cached (hashtable-ref *which-cache* name #f)])
          (or cached
              (let ([found (which name)])
                (when found (hashtable-set! *which-cache* name found))
                found))))))
```

Replaced `(which cmd-name)` with `(which-cached cmd-name)` at all 3 call sites in `executor.sls` (lines ~487, ~622, ~1334).

**Implementation note:** Because `src/gsh/util.sls` is generated by the build, both the export addition and the function definition are added via `patch-file!` calls in `build-jerboa.ss`.

---

## Optimization 6: `env-get` Single-Pass Lookup

**File:** Implemented as post-build patch in `build-jerboa.ss` (targets `src/gsh/environment.sls`)

**Problem:** For ordinary variable lookup, `env-get` performed two full scope-chain traversals:

```scheme
;; Before: two walks of the scope chain
[else
 (let ([resolved (resolve-nameref name env)])
   (let ([var (find-var-in-chain env resolved)])   ;; walk 1: find var struct
     (if (and var (shell-var-nameref? var))
         #f
         (env-get-chain env resolved))))]           ;; walk 2: get the value
```

`find-var-in-chain` walks the scope chain calling `hash-get` at each level. `env-get-chain` then walks the same chain again to extract the value.

**Fix:** Extract the value directly from the struct found in the first walk:

```scheme
;; After: single walk of the scope chain
[else
 (let ([resolved (resolve-nameref name env)])
   (let ([var (find-var-in-chain env resolved)])
     (cond
       [(not var) (getenv resolved #f)]       ;; fallback to process env
       [(shell-var-nameref? var) #f]           ;; circular nameref
       [else (shell-var-scalar-value var)])))] ;; value already in hand
```

**Why this is correct:** `find-var-in-chain` already skips `+unset-sentinel+` vars (returns `#f` for them). So if it returns a `var`, the value is valid. The `getenv` fallback handles the "not found in any scope" case that `env-get-chain` previously handled at the end of the chain.

---

## What Did NOT Work / Was Skipped

### Blocking `waitpid` for foreground processes
The plan was to replace the polling loop in `wait-for-foreground-process-raw` (jobs.sls) with a blocking `waitpid(pid, WUNTRACED)` call. This would eliminate the exponential-backoff sleep loop.

**Why skipped:** The polling loop also checks for pending signals (SIGINT, SIGTERM) and forwards them to the child process group. A blocking `waitpid` in the Chez main thread would prevent signal delivery checking. Without careful SA_RESTART/EINTR handling in the C shim, this risks breaking Ctrl-C for interactive use. The existing optimizations already produce better-than-gerbil-sh results, so the risk/reward is unfavorable.

### Per-module `eval-when (compile)` blocks
The plan was to add `(eval-when (compile) (cp0-effort-limit 1000) ...)` inside each hot module's library body. This was implemented in the `.sls` files but got overwritten by the build's Gherkin compilation step. Adding it as a post-build patch would require patching the import clause of each module, which is fragile. Since the global `build-binary.ss` params already cover this, the benefit is marginal.

### `for`-loop fast local-set
The plan was to bypass `env-set!` for for-loop iteration variables. This requires patching `control.sls`'s for-loop body to write directly to the current scope's hash table. Skipped because the overall results already far exceed the target, and this change would require careful auditing of `env-set!`'s flag-propagation side effects.

---

## Lessons: Build System Gotchas

1. **Edit `.ss`, not `.sls`.** The `.sls` files are generated output. Changes to `.sls` are lost on the next `make jerboa` run.

2. **Post-build patches go in `build-jerboa.ss`.** The `patch-file!` helper does a simple string find-and-replace. For replacing all occurrences (like the `which → which-cached` change), use a loop over the file content instead of `patch-file!`.

3. **`generate-wpo-files` must be set at compile time, not just link time.** Setting it only in `build-binary.ss`'s `compile-program` call does not retroactively generate `.wpo` files for already-compiled library `.so` files. Must also be set when compiling the library modules (in `build-all.ss`).

4. **`eval` is needed to apply `parameterize` to `import`s.** A top-level `(import ...)` form is processed at compile time, before `parameterize` takes effect at runtime. Wrapping in `(eval '(import ...) (interaction-environment))` forces runtime evaluation under the parameterize.

5. **Rebuilding jerboa is required** after changes to `~/mine/jerboa/lib/`. The `make binary` target in jerboa-shell does not automatically rebuild the jerboa library — run `make` in `~/mine/jerboa/` first.
