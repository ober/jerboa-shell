# jerboa-shell

A POSIX-compatible shell (`jsh`) built with [Jerboa](https://github.com/ober/jerboa) — a Chez Scheme standard library providing Gerbil-compatible APIs.

**Status:** 1056/1179 (90%) Oils POSIX compat tests passing · 66/66 unit tests passing

## Architecture

`jsh` runs shell source code (originally from [gerbil-shell](https://github.com/ober/gerbil-shell)) translated to R6RS `.sls` libraries that run natively on Chez Scheme using:

1. **Jerboa** — provides `(std ...)` and `(jerboa ...)` modules (sort, format, transducers, logging, threads, pattern matching, etc.)
2. **Gherkin runtime** — MOP/class system and Gerbil compiler (for in-shell `eval` of Gerbil forms)
3. **C FFI shim** — POSIX system calls (fork, exec, signals, termios, etc.)
4. **Self-contained binary** — boot files (petite, scheme, jsh) embedded as C byte arrays; fully portable single ELF

### Enhancements over gerbil-shell

| Feature | Implementation |
|---------|---------------|
| History search | Transducer pipeline `(std transducer)`: prefix filter + seen-set dedup + take |
| Debug logging | Structured log via `(std log)`, activated by `JSH_DEBUG=1` |
| Binary portability | memfd program loading (avoids Chez boot-file thread limitation) |

## Prerequisites

- [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.x
- [Jerboa](https://github.com/ober/jerboa) at `~/mine/jerboa`
- [Gherkin](https://github.com/ober/gherkin) at `~/mine/gherkin` (compiler runtime only)
- GCC

## Building

```bash
# Build standalone jsh binary (recommended)
make jsh

# Compile modules only (no binary)
make jsh-compile

# Run interpreted (no binary needed)
make jsh-run
```

## Testing

```bash
# Unit tests (66 tests across all core modules)
make test

# Full Oils POSIX compat report vs gsh reference
make compat-test
```

See [docs/test-status.md](docs/test-status.md) for per-suite results.

## Configuration

```makefile
JERBOA  ?= $(HOME)/mine/jerboa/lib   # Jerboa runtime path
GHERKIN ?= $(HOME)/mine/gherkin/src  # Gherkin compiler path
```

## Source Layout

```
jsh.ss              # Entry point (Chez script)
jsh-main.c          # C main (embeds boot files + program via memfd)
ffi-shim.c          # POSIX FFI (fork, exec, signals, termios, ptrace)
build-jsh.ss        # Module compilation driver
build-binary-jsh.ss # 7-step binary build (WPO → boot → C headers → link)
src/
  compat/gambit.sls # Gambit→Chez compat shims
  jsh/              # 31 translated shell modules (ast, lexer, parser, executor, ...)
shell/              # Original gerbil-shell .ss source files (reference)
test/
  test-jsh.ss       # Unit test suite (66 tests)
docs/
  test-status.md    # Per-suite compat results
  optimization.md   # Chez compiler optimization notes
```
