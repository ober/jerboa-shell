# jerboa-shell

A POSIX-compatible shell (`gsh`) built with [Jerboa](https://github.com/ober/jerboa) — a Gerbil Scheme compatibility layer for Chez Scheme.

## Architecture

jerboa-shell takes [gerbil-shell](https://github.com/ober/gerbil-shell) (a shell written in Gerbil Scheme) and compiles it to run on Chez Scheme using:

1. **Gherkin compiler** — translates Gerbil `.ss` source files to R6RS `.sls` libraries
2. **Jerboa runtime** — provides Gerbil-compatible APIs (hash tables, threads, pattern matching, etc.) on native Chez Scheme
3. **C FFI shim** — POSIX system calls (fork, exec, signals, termios, etc.)

### vs gherkin-shell

| | gherkin-shell | jerboa-shell |
|---|---|---|
| Runtime | gherkin's `(compat ...)` / `(runtime ...)` | jerboa's `(jerboa ...)` / `(std ...)` |
| Threading | `(compat threading)` | `(std misc thread)` |
| Stdlib | gherkin's compat shims | jerboa's native implementations |
| Compiler | gherkin `(compiler compile)` | gherkin `(compiler compile)` (same) |

## Prerequisites

- [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.x
- [Jerboa](https://github.com/ober/jerboa) (`~/mine/jerboa`)
- [Gherkin](https://github.com/ober/gherkin) (`~/mine/gherkin`) — compiler only
- GCC (for FFI shim)

## Building

```bash
# Full build: FFI → translate → compile
make all

# Run interpreted
make run

# Build standalone binary
make binary
```

## Configuration

```makefile
JERBOA  ?= $(HOME)/mine/jerboa/lib   # Jerboa runtime path
GHERKIN ?= $(HOME)/mine/gherkin/src  # Gherkin compiler path
```
# jerboa-shell
