# Chez Scheme Optimization Techniques for Gherkin

Techniques from the Chez Scheme compiler (v10.4.0) that can improve performance of Gerbil-to-Chez converted code.

## 1. Compiler Parameter Tuning

The most impactful knobs for compiled Gherkin output:

| Parameter | Default | Recommendation |
|-----------|---------|----------------|
| `optimize-level` | 2 | Use 3 for well-tested code (removes all type checks) |
| `cp0-effort-limit` | 200 | Increase to 500+ for generated code with deep nesting |
| `cp0-score-limit` | 20 | Increase to 50 to allow more aggressive inlining |
| `enable-type-recovery` | #t | Keep enabled — helps recover types after Gerbil wrapping layers |
| `enable-cross-library-optimization` | #t | Critical since Gherkin emits many small R6RS libraries |

Set these per-file in the compilation pipeline via `(eval-when (compile) ...)` for hot modules.

## 2. Record Type Flags

When compiling `defstruct`/`defclass` to Chez records, emit:

- **`(sealed #t)`** — prevents subclassing, enables direct field access without vtable indirection
- **`(immutable)` on fields** where Gerbil code never uses `set!` on slots — cp0 can then fold field accesses
- **`(nongenerative <uid>)`** — enables cross-library type identity and optimization

Currently the MOP uses `gerbil-struct` wrappers. For performance-critical structs, consider emitting native Chez `define-record-type` directly with these flags.

## 3. Numeric Specialization

Chez unboxes flonums at level 3 but only with explicit flonum ops:

```scheme
;; Generic (boxes intermediate results, GC pressure)
(+ (* x y) z)

;; Specialized (raw FPU instructions, no allocation)
(fl+ (fl* x y) z)
```

The compiler could emit `fx+`/`fl+` when type annotations are available from Gerbil's `def` forms (e.g., `(def (f (x : fixnum)) ...)`).

## 4. Local Binding Optimization

Chez's cp0 aggressively inlines **locally-bound, unassigned procedures**. Top-level `define`s across library boundaries are harder to optimize. This matters because Gherkin emits many small libraries.

**Actionable**: When a Gerbil helper function is only used in one place, emit it as a local `let` binding rather than a top-level `define`.

## 5. Type Guard Patterns

cp0 + cptypes recognize predicate-guarded branches:

```scheme
;; cptypes knows x is a pair in the consequent
(if (pair? x) (car x) ...)

;; cptypes knows x is a fixnum
(if (fixnum? x) (fx+ x 1) ...)
```

The `match` compiler already emits type tests — make sure the pattern is `(if (predicate? x) <then> <else>)` rather than intermediate wrappers that obscure the type information from cptypes.

## 6. Avoid the WPO Trap

WPO breaks `identifier-syntax` mutable export cells. But for pure-Scheme modules without mutable exports, WPO could still be applied selectively. Consider a build flag that enables WPO per-module.

## 7. Letrec Decomposition

Chez's `cpletrec` pass decomposes `letrec*` into simpler forms. Since Gerbil's top-level `def` forms compile to `letrec*` bodies in R6RS libraries, this pass is already working for you. But ordering matters — put pure definitions before impure ones to help cpletrec separate them.

## 8. Primitive Inlining Flags

Each Chez primitive has optimization flags (`pure`, `unrestricted`, `cp02`, `arith-op`, etc.) in `primdata.ss`. When the compat layer wraps Chez primitives (e.g., `hash-ref` wrapping `hashtable-ref`), the wrapper obscures these flags from the optimizer.

**Actionable**: For hot-path compat wrappers, consider:
- Using `define-syntax` + `syntax-rules` instead of `define` to make them transparent to cp0
- Or mark them with integration info if applicable

## 9. Closure Avoidance in Loops

Chez doesn't optimize closures allocated inside loops. The `for`/`for/collect` compilation should ensure lambda bodies are hoisted when they don't capture loop variables.

## 10. Quick Wins Checklist

- [ ] Add `(optimize-level 3)` option to gxc for release builds
- [ ] Emit `sealed` records for `defstruct` without known subclasses
- [ ] Emit `immutable` fields for non-mutated struct slots
- [ ] Emit `fx+`/`fx-` for integer arithmetic in type-annotated code
- [ ] Ensure `match` type tests are direct `(if (pred? x) ...)` — not wrapped
- [ ] Increase `cp0-effort-limit` for generated code (deep nesting from macro expansion)
- [ ] Consider selective WPO for pure modules without mutable exports
- [ ] Profile the compat wrappers (hash, MOP dispatch) — these are likely the hottest paths

---

## 11. Chez Scheme Optimization Functions & APIs

### `expand/optimize` — Inspect Optimizer Output

Use this to see what Chez's optimizer actually does with generated code. Invaluable for checking whether compat wrappers are being inlined or blocking optimization:

```scheme
(expand/optimize '(let ([y '(3 . 4)]) (+ (car y) (cdr y))))
;=> 7   (fully folded at compile time)

(print-gensym #f)
(expand/optimize
  '(let ([y '(3 . 4)])
     (lambda (x) (* (+ (car y) (cdr y)) x))))
;=> (lambda (x) (#2%* 7 x))
```

### `compile-whole-program` — Whole-Program Optimization

The most aggressive optimization Chez offers. Discards unused code and optimizes across all library boundaries:

```scheme
(parameterize ([generate-wpo-files #t])
  (compile-program "main.ss" "main.so"))
(compile-whole-program "main.wpo" "main-opt.so")
```

Returns a list of libraries that couldn't be incorporated (missing `.wpo` files). Breaks `identifier-syntax` mutable export cells — use only for modules without mutable exports.

### `compile-whole-library` — Same for Libraries

Like `compile-whole-program` but for library bundles. Produces both `.so` and `.wpo` output.

### `$primitive` / `#%` Syntax — Force Original Primitives

Bypass any user redefinitions and control per-call optimization level:

```scheme
(#3%car x)     ; car at optimize-level 3 (no type check)
(#2%+ a b)     ; + at optimize-level 2 (with checks)
(#%vector-ref v i)  ; original primitive, current optimize level
```

## 12. Complete Parameter Reference

| Parameter | Default | Effect |
|-----------|---------|--------|
| `optimize-level` | 0 | 0-2 = safe; **3 = unsafe** (no type/bounds checks) |
| `run-cp0` | 2-pass | Controls the source optimizer; can disable or run N passes |
| `cp0-effort-limit` | 200 | Max work per inlining attempt |
| `cp0-score-limit` | 20 | Max code growth per inlining |
| `cp0-outer-unroll-limit` | 0 | Loop unrolling (set to 1+ to enable) |
| `enable-type-recovery` | #t | cptypes pass — removes redundant type checks |
| `enable-cross-library-optimization` | #t | Propagate constants/inlining across libraries |
| `enable-unsafe-application` | #f | Skip procedure? check on calls (even below level 3) |
| `enable-unsafe-variable-reference` | #f | Skip letrec undefined-var checks |
| `enable-arithmetic-left-associative` | #f | Allow arithmetic reassociation |
| `commonization-level` | 0 | 0-9; merges structurally similar lambdas (reduces code size) |
| `generate-wpo-files` | #f | Emit `.wpo` files for whole-program optimization |
| `generate-inspector-information` | #t | Set #f to reduce object file size |
| `debug-level` | 1 | 0 = max optimization, 3 = max debuggability |
| `compile-interpret-simple` | #t | Interpret simple expressions instead of compiling |

## 13. Customizing the cp0 Pass

The default runs cp0 **twice** (second pass catches things the first missed after inlining). Customizable via `run-cp0`:

```scheme
;; Disable cp0 entirely (fastest compile, no optimization)
(run-cp0 (lambda (cp0 x) x))

;; Run once (faster compile, slightly less optimization)
(run-cp0 (lambda (cp0 x) (cp0 x)))

;; Run 3 times (slower compile, more aggressive)
(run-cp0 (lambda (cp0 x) (cp0 (cp0 (cp0 x)))))
```

## 14. Commonization — Code Size Reduction for Generated Code

The `commonization-level` parameter (0-9) merges structurally similar lambda expressions by abstracting differences at leaf positions (constants, variable references, primitive references).

```scheme
(eval-when (compile) (commonization-level 5))
```

**Especially relevant for Gherkin**: the compiler generates many structurally similar lambdas (match arms, method dispatchers, accessor functions). Setting this to 3-5 could significantly reduce code size and improve instruction cache utilization. Trade-off: can undo some cp0 inlining and complicate debugging.

## 15. Per-File Optimization Directives

Apply optimization settings to individual files without affecting the rest of the build:

```scheme
(eval-when (compile)
  (optimize-level 3)
  (cp0-effort-limit 500)
  (generate-inspector-information #f)
  (commonization-level 3))
```

This only affects compilation (via `compile-file`), not source loading.

## 16. Recommendations Specific to Gherkin

1. **`expand/optimize`** — Use to inspect whether compat wrappers (hash-ref, slot-ref) are being inlined or blocking optimization. If they're opaque, convert to `define-syntax` wrappers.

2. **`commonization-level` 3-5** — High value for Gherkin since the compiler generates many structurally similar lambdas from match, defmethod, and accessor compilation.

3. **`compile-whole-program`** — For modules without mutable exports, this gives the biggest wins. Worth investigating per-module WPO enablement.

4. **`enable-unsafe-application`** — A middle ground: skip the "is this a procedure?" check without going full optimize-level 3. Safe for generated code where every call target is known to be a procedure.

5. **`generate-inspector-information #f`** — For release builds, disabling this reduces `.so` file sizes.

6. **`debug-level 0`** — For release builds, allows maximum continuation optimization.

## 17. Empirical Results (gherkin-shell benchmarks)

Tested on gherkin-shell using shellbench. All numbers are executions/second (higher = better).

| Optimization | Avg Improvement | Best Test | Binary Size |
|-------------|----------------|-----------|-------------|
| Baseline (default) | — | — | 6,543 KB |
| optimize-level 3 (shell only) | +3.2% | +6.3% | 6,499 KB |
| opt3 (runtime + shell) | +5.2% | +6.4% | 6,475 KB |
| opt3 + tuned cp0 + no inspector | +5.0% | +6.3% | 6,003 KB (-8.3%) |
| opt3 + tuned cp0 + partial WPO | +8.7% | +16.3% | 7,075 KB |
| **opt3 + tuned cp0 + full WPO** | **+9.6%** | **+20.3%** | 6,891 KB (+5.3%) |

Key findings:
- **WPO works** despite `identifier-syntax` mutable exports (they survived WPO in this project)
- **Full WPO requires `.wpo` files** from both gherkin runtime and gherkin-shell compat layers
- **cp0 tuning** (effort 500, score 50) adds ~1-2% on top of opt3 alone
- **`generate-inspector-information #f`** reduces binary size significantly (-8.3%) with negligible performance impact
- **Biggest wins** on comparison operations (`cmp: [ ]` +20.3%) and arithmetic (`count: typeset -i` +17.9%)

## Summary

The biggest gains will come from **optimize-level 3** (+3-5%), **WPO** (+4-5% additional), and **tuned cp0 limits** (+1-2%). Record type flags (sealed/immutable) and commonization remain untested but should provide further gains. Use `expand/optimize` to verify that optimizations are actually taking effect on generated code.
