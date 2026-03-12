# jsh Improvement Plan: Leveraging Jerboa Features

This plan identifies concrete improvements to jerboa-shell using Jerboa's standard library features. Items are ordered by implementation complexity (easiest first) and grouped by theme. Each item references the exact source files to modify and the Jerboa APIs to use.

---

## Phase 1: Quick Wins (1-2 hours each)

### 1.1 `,sb` Meta-Command — Human-Friendly Sandboxed Script Execution [DONE]

**Problem:** Running a script in a sandbox requires writing Scheme: `,use sandbox.ss` or typing `(with-sandbox #:allow-read '("/tmp") ... (run-script "evil.sh"))`. No shell user will do this.

**Solution:** Add a `,sb` (sandbox) meta-command with intuitive shell-like syntax:

```
,sb script.sh                          # run with default safe policy
,sb -r /tmp -r ~/.config script.sh     # allow reading /tmp and ~/.config
,sb -w /tmp script.sh                  # allow writing to /tmp
,sb -x /usr/bin/git script.sh          # allow executing git
,sb --no-net script.sh                 # deny network (default)
,sb --net script.sh                    # allow network
,sb -t 5000 script.sh                  # 5 second timeout
,sb -r /tmp -w /tmp -x git make.sh     # combined
```

**Files to modify:**
- `jsh.ss:294-329` — add `,sb` case to `*meta-command-handler*` cond chain
- `src/jsh/sandbox.sls` — add `parse-sb-args` helper that converts CLI-style flags to opts list

**Implementation sketch:**
```scheme
;; In jsh.ss, add before the else clause:
[(string-prefix? "sb " expr-str)
 (let* ((args (string-split (substring expr-str 3 ...) " "))
        (parsed (parse-sb-args args))
        (script (sb-parsed-script parsed))
        (opts   (sb-parsed-opts parsed)))
   (let ((policy (build-policy opts))
         (timeout (extract-timeout opts))
         (env (*current-jsh-env*)))
     (let ((thunk (lambda () (execute-script script '() env))))
       (jsh-sandbox-run opts thunk)))
   (cons "" 0))]
```

Also add `,sb --help` that prints usage.

**Jerboa API:** `(std capability sandbox)` — already imported.

---

### 1.2 `,sb` for Inline Commands (not just scripts) [DONE]

Extend `,sb` to also sandbox inline shell commands:

```
,sb -c "rm -rf /"              # sandboxed inline command (blocked by policy)
,sb -r /etc -c "cat /etc/passwd"  # allowed: reading /etc
```

**Implementation:** When `-c` flag is present, use `run-cmd` instead of `run-script`.

---

### 1.3 `,profile` Meta-Command — Script Performance Tracing

**Problem:** `bash -x` shows what commands run but not how long each takes. No shell has built-in profiling.

**Solution:** Add `,profile` that instruments the executor to time every command:

```
,profile script.sh              # run script with per-command timing
,profile -c "for i in $(seq 100); do echo $i | wc -c; done"
```

Output:
```
=== jsh profile: script.sh ===
  0.002s  export PATH=/usr/bin:$PATH
  0.001s  cd /tmp
  1.234s  find . -name "*.log" -exec grep error {} \;
  0.003s  echo "done"

--- Summary ---
  Total:     1.240s
  Commands:  4
  Slowest:   find ... (1.234s, 99.5%)
  Pipeline:  0 pipes
```

**Files to modify:**
- `jsh.ss` — add `,profile` to meta-command handler
- `src/jsh/executor.sls` — add `*profile-mode*` parameter; wrap `execute-command` to record timing when active

**Jerboa API:** `(std dev profile)` — `time-thunk`, `profile-start!`, `profile-report`

**Implementation sketch:**
```scheme
;; New parameter in executor.sls
(define *profile-mode* (make-parameter #f))
(define *profile-entries* (make-parameter '()))

;; Wrap execute-command to record timing when profiling
(define (execute-command/profiled cmd env)
  (if (*profile-mode*)
    (let* ((text (ast->command-text cmd))
           (start (cpu-time))
           (result (execute-command cmd env))
           (elapsed (- (cpu-time) start)))
      (*profile-entries*
        (cons (list elapsed text result) (*profile-entries*)))
      result)
    (execute-command cmd env)))
```

---

### 1.4 `,trace` Meta-Command — Pipeline Visualization

**Problem:** `set -x` output is noisy and doesn't show pipeline structure or fd wiring.

**Solution:** A structured trace that shows the pipeline DAG:

```
,trace -c "cat file | grep error | sort | uniq -c | head"
```

Output:
```
Pipeline: 5 stages
  [1] cat file        → stdout→pipe1
  [2] grep error      ← pipe1  → stdout→pipe2
  [3] sort            ← pipe2  → stdout→pipe3
  [4] uniq -c         ← pipe3  → stdout→pipe4
  [5] head            ← pipe4  → stdout

Status: 0|0|0|0|0  (all exited 0)
Wall time: 0.045s
```

**Files to modify:**
- `jsh.ss` — add `,trace` meta-command
- `src/jsh/pipeline.sls` — add `*trace-mode*` parameter; log pipe fd assignments
- `src/jsh/executor.sls` — propagate trace mode

**Jerboa API:** `(std log)` — structured logging with fields

---

### 1.5 Structured Logging with `(std log)` — Expand JSH_DEBUG

**Problem:** `JSH_DEBUG=1` exists but only logs in executor. Other modules (pipeline, jobs, redirect, expander) have no tracing.

**Solution:** Add structured logging to all major modules, filterable by component:

```bash
JSH_DEBUG=executor,pipeline,jobs ./jsh     # filter by component
JSH_DEBUG=all ./jsh                         # everything
```

**Files to modify:** All `src/jsh/*.sls` — add `(only (std log) make-logger log-info log-debug)` import and `*jsh-debug-logger*` to each module.

---

## Phase 2: Moderate Effort (half day each)

### 2.1 STM for Job Table — Eliminate Signal Races

**Problem:** `jobs.sls:152-171` has `*job-table*`, `*next-job-id*`, `*current-job*`, `*previous-job*` as bare mutable globals accessed by both the main thread and signal handlers. Classic TOCTOU race.

**Current code (jobs.sls:152-171):**
```scheme
(define *job-table*-cell (vector (list)))
(define-syntax *job-table*
  (identifier-syntax
    [id (vector-ref *job-table*-cell 0)]
    [(set! id v) (vector-set! *job-table*-cell 0 v)]))
;; same pattern for *next-job-id*, *current-job*, *previous-job*
```

**Solution:** Replace with STM TVars:

```scheme
(import (std stm))

(define *jobs* (make-tvar (list)))
(define *next-id* (make-tvar 1))
(define *current* (make-tvar #f))
(define *previous* (make-tvar #f))

(define (job-table-add! processes command-text . rest)
  (atomically
    (let* ((id (tvar-ref *next-id*))
           (job (make-job id ...)))
      (tvar-set! *jobs* (append (tvar-ref *jobs*) (list job)))
      (tvar-set! *previous* (tvar-ref *current*))
      (tvar-set! *current* id)
      (tvar-set! *next-id* (+ id 1))
      job)))
```

**Files to modify:**
- `src/jsh/jobs.sls` — replace 4 identifier-syntax cells with TVars; wrap all reads/writes in `atomically`

**Jerboa API:** `(std stm)` — `make-tvar`, `tvar-ref`, `tvar-set!`, `atomically`

---

### 2.2 `define-ffi-library` — Replace Manual foreign-procedure Declarations

**Problem:** `src/jsh/ffi.sls` has 50+ individual `foreign-procedure` calls with hand-written type signatures. Error-prone and verbose.

**Current code (ffi.sls:74-76):**
```scheme
(define c-ffi-do-waitpid (foreign-procedure "ffi_do_waitpid" (int int) int))
(define c-ffi-get-waitpid-status (foreign-procedure "ffi_get_waitpid_status" () int))
```

**Solution:** Use `define-ffi-library` for declarative binding:

```scheme
(import (std foreign))

(define-ffi-library gsh-ffi
  (shared-lib "libgsh-ffi.so")
  ;; process
  (bind ffi-do-waitpid    (int int) -> int)
  (bind ffi-get-waitpid-status () -> int)
  (bind ffi-fork          ()       -> int)
  (bind ffi-exit          (int)    -> void)
  ;; fd ops
  (bind ffi-dup           (int)     -> int)
  (bind ffi-dup2          (int int) -> int)
  (bind ffi-close-fd      (int)     -> int)
  ...)
```

**Files to modify:** `src/jsh/ffi.sls` — rewrite using `define-ffi-library`

**Jerboa API:** `(std foreign)` — `define-ffi-library`, `define-foreign/check`

---

### 2.3 Gradual Typing on Hot Paths

**Problem:** Type errors in shell internals (e.g., passing a number where a string is expected in word expansion) surface as cryptic Chez errors at runtime.

**Solution:** Add `define/t` annotations to the most error-prone functions:

```scheme
(import (std typed))

(define/t (expand-word [word : pair] [env : (? shell-environment?)]) : list
  ...)

(define/t (env-get [env : (? shell-environment?)] [name : string]) : (or string #f)
  ...)
```

In debug mode, these catch type mismatches with clear messages. In release mode (`(*typed-mode* 'release)`), zero overhead.

**Files to modify:**
- `src/jsh/expander.sls` — annotate `expand-word`, `expand-word-nosplit`, etc.
- `src/jsh/environment.sls` — annotate `env-get`, `env-set!`, `env-push-scope`
- `src/jsh/executor.sls` — annotate `execute-command`, `execute-simple-command`

**Jerboa API:** `(std typed)` — `define/t`, `*typed-mode*`

---

### 2.4 `(jerboa build)` — Replace Hand-Written Build Scripts

**Problem:** `build-binary-jsh.ss` is 500+ lines of hand-written compilation pipeline that duplicates what `(jerboa build)` already provides: boot file embedding, C generation, linking.

**Solution:** Replace with `build-binary` from `(jerboa build)`:

```scheme
(import (jerboa build) (jerboa cache))

(build-binary "jsh.ss" "./jsh"
  'release: #t
  'optimize-level: 3)
```

For the FFI shim, keep the gcc step but integrate it into the pipeline.

**Files to modify:**
- `build-binary-jsh.ss` — replace with `(jerboa build)` API calls
- `Makefile` — simplify binary target

**Jerboa API:** `(jerboa build)` — `build-binary`, `build-project`, `compile-modules-parallel`, `build-release`; `(jerboa cache)` — `with-compilation-cache`

---

## Phase 3: Significant Features (1-2 days each)

### 3.1 Sealed AST Hierarchy + Exhaustive Match

**Problem:** `executor.sls:69-120` has a 20-arm `cond` chain dispatching on AST node types. If a new node type is added, the compiler gives no warning — you get the runtime fallthrough `"gsh: unknown command type"`.

**Current code (executor.sls:69-120):**
```scheme
(define (execute-command cmd env)
  (cond
    [(not cmd) 0]
    [(redirected-command? cmd) ...]
    [(simple-command? cmd) ...]
    [(ast-pipeline? cmd) ...]
    [(and-or-list? cmd) ...]
    ...
    [else (fprintf (current-error-port) "gsh: unknown command type~n") 1]))
```

**Solution:** Register AST types as a sealed hierarchy and use `match/strict`:

```scheme
(import (std match2))

(define-sealed-hierarchy shell-node
  (simple-command simple-command? simple-command-name simple-command-args ...)
  (ast-pipeline ast-pipeline? ast-pipeline-commands ast-pipeline-bang?)
  (and-or-list and-or-list? and-or-list-left and-or-list-op and-or-list-right)
  (if-command if-command? ...)
  (for-command for-command? ...)
  ...)

(define (execute-command cmd env)
  (match/strict 'shell-node cmd
    [(simple-command name args _) (execute-simple-command cmd env)]
    [(ast-pipeline cmds bang?) ...]
    ...))
;; Compile-time warning if a variant is missing
```

**Files to modify:**
- `src/jsh/ast.sls` — add `define-sealed-hierarchy` after struct definitions
- `src/jsh/executor.sls` — replace `cond` chain with `match/strict`

**Jerboa API:** `(std match2)` — `define-sealed-hierarchy`, `match/strict`, `define-match-type`

---

### 3.2 Hot Reload for `.jshrc`

**Problem:** Changing your shell config requires restarting the shell.

**Solution:** Watch `~/.jshrc` and reload on change:

```
$ vim ~/.jshrc     # edit in another terminal
# jsh automatically detects the change:
[jshrc reloaded]
$
```

**Implementation:**
```scheme
(import (std dev reload))

(define jshrc-reloader
  (make-reloader (path-expand "~/.jshrc")
    #:on-reload (lambda ()
                  (execute-script (path-expand "~/.jshrc") '() env)
                  (display "[jshrc reloaded]\n"))
    #:on-error (lambda (exn)
                 (fprintf (current-error-port)
                          "jshrc error: ~a~n" exn))))

;; In the REPL loop, before reading input:
(reloader-check! jshrc-reloader)
```

**Files to modify:**
- `src/jsh/main.sls` — add reloader init after startup, check in REPL loop

**Jerboa API:** `(std dev reload)` — `make-reloader`, `reloader-check!`

---

### 3.3 Lazy Sequences for `,` Eval Pipeline Integration

**Problem:** When using the `,` eval prefix to mix Scheme with shell output, the entire command output is materialized into memory as a string. For large outputs (e.g., `find /`), this is wasteful.

**Solution:** Expose command output as lazy sequences in the `,` eval environment:

```scheme
;; In ,eval mode:
;; Get first 10 lines matching "error" from a huge log — lazy, no full materialization
(lazy->list
  (lazy-take 10
    (lazy-filter (lambda (line) (string-contains line "error"))
      (cmd->lazy "journalctl --since today"))))

;; Count files without loading them all into memory
(lazy-count (cmd->lazy "find / -name '*.log'"))
```

**Implementation:** Add `cmd->lazy` that spawns the process and returns a lazy sequence reading lines on demand:

```scheme
(import (std seq))

(define (cmd->lazy cmd-string)
  (let* ((port (open-process-port cmd-string)))
    (let read-next ()
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin (close-port port) (lazy-nil))
          (lazy-cons line (read-next)))))))
```

**Files to modify:**
- `jsh.ss:34-93` (ensure-gerbil-env!) — add `cmd->lazy` and `(std seq)` exports to eval environment

**Jerboa API:** `(std seq)` — `lazy-cons`, `lazy-nil`, `lazy-filter`, `lazy-take`, `lazy->list`, `lazy-map`

---

### 3.4 `,bench` Meta-Command — Comparative Shell Benchmarking

**Problem:** Comparing command performance requires external tools (`hyperfine`, `time`). No shell has built-in comparative benchmarking.

**Solution:** Built-in benchmarking with statistical output:

```
,bench 100 "echo hello > /dev/null"     # run 100 iterations
```

Output:
```
=== Benchmark: echo hello > /dev/null (100 iterations) ===
  Mean:    0.234ms  (stddev: 0.012ms)
  Median:  0.231ms
  Min:     0.218ms
  Max:     0.289ms
  p95:     0.256ms
  Total:   23.4ms
  Alloc:   1.2 KB/iter
```

Compare two commands:
```
,bench 50 "grep -r error /var/log" vs "rg error /var/log"
```

Output:
```
=== Comparison (50 iterations each) ===
  grep:  mean 234.5ms  (stddev 12.3ms)
  rg:    mean  45.2ms  (stddev  3.1ms)
  Winner: rg (5.2x faster)
```

**Files to modify:**
- `jsh.ss` — add `,bench` meta-command
- Use `(std dev profile)` for timing and Chez `(statistics)` for allocation tracking

**Jerboa API:** `(std dev profile)` — `time-thunk`; `(std stm)` for collecting stats atomically

---

## Phase 4: Ambitious / Differentiating Features (multi-day)

### 4.1 Effects-Based Pipeline I/O (Experimental)

**Problem:** `pipeline.sls:83-100` uses `*pipeline-fd-mutex*`, `ffi-dup`/`ffi-dup2` to wire fd 0/1 for nested pipelines. This is fragile and untestable — you can't unit-test pipeline wiring without forking real processes.

**Current code (pipeline.sls:83-100):**
```scheme
(define *pipeline-fd-mutex* (make-mutex 'pipeline-fd))
;; ...
(mutex-lock! *pipeline-fd-mutex*)
(when outer-in (ffi-dup2 outer-in 0))
(when outer-out (ffi-dup2 outer-out 1))
```

**Solution:** For Scheme-internal commands (builtins, functions), model I/O as effects:

```scheme
(import (std effect))

(defeffect ShellIO
  (read-line)
  (write-line str)
  (write-err str))

;; Pipe handler: left's write-line feeds right's read-line
(define (pipe-effect left right)
  (let ((ch (make-channel 64)))
    (fork-thread
      (lambda ()
        (with-handler
          ([ShellIO (write-line (k str)
                     (channel-put ch str)
                     (resume k (void)))])
          (left))
        (channel-close ch)))
    (with-handler
      ([ShellIO (read-line (k)
                 (let ((v (channel-get ch)))
                   (resume k (if (eof-object? v) v v))))])
      (right))))
```

This is testable without fork/exec. External commands still use real pipes.

**Files to modify:**
- `src/jsh/pipeline.sls` — add effects-based path for builtin-to-builtin pipes
- `src/jsh/builtins.sls` — make builtins perform `ShellIO` effects instead of direct port I/O

**Jerboa API:** `(std effect)` — `defeffect`, `with-handler`, `resume`, `perform`

---

### 4.2 `,watch` Meta-Command — Live File/Command Monitoring

**Problem:** `watch` command exists but is external and limited. No shell has built-in reactive file watching.

**Solution:**
```
,watch script.sh              # re-run script.sh when any source file changes
,watch -f "*.log" -c "tail -5"  # when .log files change, run tail
,watch -i 2 "df -h"           # like watch(1) but built-in, 2s interval
```

Uses `inotify` via jerboa's capabilities or polling. Can also integrate with `,profile` to show timing trends over time.

**Jerboa API:** `(std dev reload)` — file watching primitives

---

### 4.3 Time-Travel Debugger for Shell Scripts

**Problem:** Debugging shell scripts means adding `echo` statements or using `set -x`. Both are noisy and non-interactive.

**Solution:** Record script execution and allow post-mortem inspection:

```
,debug script.sh
```

After the script runs (or crashes):
```
=== jsh debugger: 47 events recorded ===
debug> rewind
  [0] cd /tmp                           # status=0
debug> step 5
  [5] result=$(curl -s http://api.example.com/data)   # status=0
  vars: result="{"items":[]}"
debug> locals
  PATH=/usr/bin:/bin
  result={"items":[]}
  i=3
debug> step
  [6] if [ -z "$result" ]; then         # branch: false
```

**Implementation:** Wrap `execute-command` in `(std dev debug)` recording. Each command execution becomes a trace event with:
- The AST node / command text
- Environment snapshot (or diff from previous)
- Exit status
- Wall time

**Files to modify:**
- `src/jsh/executor.sls` — instrument `execute-command` with `trace-call!`/`trace-return!`
- `jsh.ss` — add `,debug` meta-command with interactive navigation

**Jerboa API:** `(std dev debug)` — `with-recording`, `instrument`, `debug-rewind`, `debug-step`, `debug-locals`, `trace-event!`

---

### 4.4 Per-Command Resource Tracking (`,profile` advanced mode)

Extend `,profile` to track not just time but also:
- Memory allocated per command (via `(std dev profile)` alloc tracking)
- Subprocess count (forks)
- File descriptors opened/closed
- Bytes read/written through pipes

```
,profile --detailed build.sh
```

Output:
```
=== Detailed Profile: build.sh ===
  Command                Time     Memory   Forks  FDs   Pipe I/O
  ─────────────────────────────────────────────────────────────
  gcc -c main.c         2.340s   45 MB    3      12    0 B
  find . -name "*.o"    0.012s   1.2 MB   1      4     4.2 KB
  ar rcs libfoo.a ...   0.089s   8.3 MB   1      6     0 B

  Total: 2.441s  |  54.5 MB peak  |  5 forks  |  4.2 KB piped
```

**Jerboa API:** `(std dev profile)` — `alloc-profile-start!`, `alloc-results`, `time-thunk`

---

## Phase 5: Future Vision

### 5.1 Actors for Job Control
Replace the job table with an actor per background job. `fg`/`bg`/`kill` become message sends. Supervisor handles cleanup on unexpected exit. (See jerboa-advantages.md section 4.)

### 5.2 Compile-Time Regex for Glob Patterns
Use `(std staging)` to compile common glob patterns to DFA at macro expansion time. Benefits `glob.sls` which currently interprets patterns at runtime.

### 5.3 Transducers for Streaming Shell Data
Replace the pattern of `string-split → filter → map → string-join` with transducer pipelines that process data in a single pass without intermediate allocations.

### 5.4 Capability-Based Script Permissions
Extend `,sb` to support persistent per-script policies stored in `~/.config/jsh/policies/`:

```
,sb --save myproject build.sh -r /src -w /build -x make,gcc
# Next time:
,sb build.sh    # loads saved policy automatically
```

---

## Summary: What Makes jsh Superior to bash/zsh

| Feature | bash/zsh | jsh |
|---------|----------|-----|
| Script sandboxing | None | `,sb script.sh -r /tmp --no-net` |
| Per-command profiling | `time` (one cmd) | `,profile script.sh` (all commands) |
| Pipeline visualization | `set -x` (noisy) | `,trace` (structured DAG) |
| Built-in benchmarking | None (need hyperfine) | `,bench 100 "cmd1" vs "cmd2"` |
| Script debugging | `set -x` + echo | `,debug` with rewind/step/locals |
| Heap introspection | None | `,room` (CL-style) |
| Live config reload | Restart shell | Automatic `.jshrc` hot reload |
| Scheme eval with lazy I/O | None | `(lazy-take 10 (cmd->lazy "find /"))` |
| Race-free job table | Fragile signals | STM-backed atomic updates |
| Type-checked internals | Crash at runtime | Gradual typing with clear errors |

---

## Implementation Priority

| # | Item | Effort | Impact | Dependencies |
|---|------|--------|--------|--------------|
| 1 | `,sb` meta-command | 2h | High — unique feature, already have sandbox.sls | None |
| 2 | `,profile` meta-command | 3h | High — no shell has this | None |
| 3 | `,trace` pipeline viz | 3h | Medium — nicer than `set -x` | None |
| 4 | `,bench` benchmarking | 3h | Medium — replaces hyperfine | None |
| 5 | Expand JSH_DEBUG logging | 2h | Medium — aids development | None |
| 6 | STM job table | 4h | High — eliminates race bugs | None |
| 7 | `define-ffi-library` rewrite | 4h | Medium — cleaner code | None |
| 8 | Hot reload `.jshrc` | 3h | Medium — nice UX | None |
| 9 | Sealed AST + match/strict | 6h | High — compile-time safety | None |
| 10 | `cmd->lazy` for eval | 3h | Medium — lazy streaming | None |
| 11 | Gradual typing on hot paths | 4h | Medium — better errors | None |
| 12 | `(jerboa build)` migration | 8h | Medium — simpler build | None |
| 13 | `,debug` time-travel | 8h | Very High — killer feature | `,profile` |
| 14 | Effects-based pipeline I/O | 2d | High — testability | Sealed AST |
