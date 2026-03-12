# Jerboa-Shell (jsh) Test Status

Tracking POSIX compatibility of `jsh` against the Oils spec suite (1179 tests).

## Summary

| Metric | Value |
|--------|-------|
| **jsh passing** | **1056/1179 (90%)** |
| **gsh (Gerbil native) baseline** | 1143/1179 (97%) |
| **Unit tests** | 66/66 (100%) |
| **Last updated** | 2026-03-11 |

---

## Architecture

`jsh` is a POSIX shell built on [Jerboa](../mine/jerboa) (Chez Scheme standard library) and
[Gherkin](../mine/gherkin) (Gerbil-to-Chez compiler runtime). It was migrated from the
`gerbil-shell` submodule in 2026-03-11 and no longer depends on Gerbil/Gambit.

Key enhancements over the previous `gsh` build:
- **Transducer-based history search** — `history-search` uses a composable pipeline from
  `(std transducer)`: `filtering` (prefix match) + seen-hashtable dedup + `taking 200`
- **Structured debug logging** — executor uses `(std log)` for structured output when
  `JSH_DEBUG=1` is set

## Per-Suite Results (as of 2026-03-11)

### Tier 0 — Core
| Suite | jsh | gsh | Total |
|-------|-----|-----|-------|
| smoke | 18 | 17 | 18 |
| pipeline | 21 | 22 | 26 |
| redirect | 36 | 38 | 41 |
| redirect-multi | 12 | 11 | 13 |
| builtin-eval-source | 23 | 23 | 23 |
| command-sub | 30 | 30 | 30 |
| comments | 2 | 2 | 2 |
| exit-status | 11 | 11 | 11 |

### Tier 1 — Expansion & Variables
| Suite | jsh | gsh | Total |
|-------|-----|-----|-------|
| here-doc | 36 | 36 | 36 |
| quote | 35 | 35 | 35 |
| word-eval | 8 | 8 | 8 |
| word-split | 55 | 55 | 55 |
| var-sub | 6 | 6 | 6 |
| var-sub-quote | 41 | 41 | 41 |
| var-num | 7 | 7 | 7 |
| var-op-test | 37 | 37 | 37 |
| var-op-strip | 28 | 28 | 29 |
| var-op-len | 9 | 9 | 9 |
| assign | 48 | 48 | 48 |
| tilde | 13 | 13 | 14 |

### Tier 2 — Builtins & Advanced
| Suite | jsh | gsh | Total |
|-------|-----|-----|-------|
| arith | 72 | 74 | 74 |
| glob | 39 | 39 | 39 |
| brace-expansion | 55 | 55 | 55 |
| case_ | 13 | 13 | 13 |
| if_ | 5 | 5 | 5 |
| loop | 29 | 29 | 29 |
| for-expr | 9 | 9 | 9 |
| subshell | 2 | 2 | 2 |
| sh-func | 12 | 12 | 12 |
| builtin-echo | 27 | 27 | 27 |
| builtin-printf | 53 | 60 | 63 |
| builtin-read | 57 | 64 | 64 |
| builtin-cd | 29 | 29 | 30 |
| builtin-set | 21 | 21 | 24 |
| builtin-type | 6 | 6 | 6 |
| builtin-trap | 32 | 32 | 33 |
| builtin-bracket | 52 | 52 | 52 |
| builtin-misc | 7 | 7 | 7 |
| builtin-process | 24 | 24 | 26 |
| background | 23 | 25 | 27 |
| command-parsing | 5 | 5 | 5 |
| var-op-bash | 25 | 25 | 27 |
| var-op-slice | 22 | 22 | 22 |
| assign-extended | 39 | 39 | 39 |

---

## Unit Tests

`make test` runs `test/test-jsh.ss` — 66 tests covering all core modules:

| Module | Tests | Coverage |
|--------|-------|----------|
| `(std transducer)` | 7 | filtering, deduplicate, taking, windowing, flat-mapping, mapping, into |
| `(std log)` | 7 | make-logger, logger?, logger-level, log-level? |
| `(jsh history)` | 10 | init, add, count, search (prefix + dedup), unique-commands |
| `(jsh ast)` | 6 | token, simple-command |
| `(jsh lexer)` | 5 | tokenize |
| `(jsh environment)` | 6 | make-shell-environment, env-set!, env-get, last-status |
| `(jsh arithmetic)` | 12 | +, -, *, /, **, %, &, \|, ^, >, ==  |
| `(jsh glob)` | 3 | glob-expand |
| `(jsh fuzzy)` | 3 | fuzzy-match-score |
| `(jsh registry)` | 1 | *gsh-tier* |

---

## Known Remaining Issues

### `builtin-read` — 7 remaining failures
- Intermittent pipe fd race under heavy sequential pipeline load.
- Root cause: Chez thread scheduling interacting with the pipe fd lifecycle.
- Affects: `IFS='x '` tests (#56–62), backslash tests (#63).

### `builtin-printf` — 10 remaining failures
- `%x` lowercase: `number->string 16` on Chez returns uppercase.
- `%c` with unicode: first byte extraction.
- `%b` with `\NNN` octal.

### `background` — 4 remaining failures
- `wait` returns wrong exit status (already-reaped child race with SIGCHLD handler).

### `redirect` — 5 remaining failures
- Redirect persistence in subshells.

### `arith` — 2 remaining failures
- Edge cases in arithmetic expansion.

---

## How to Run Tests

```bash
# Unit tests
make test

# Single compat suite
make compat-test SUITE=builtin-read

# Full compat report vs gsh reference
make compat-test

# Build jsh binary
make jsh

# Run interpreted (no binary build)
make jsh-run
```
