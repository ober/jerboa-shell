# Jerboa-Shell Test Status

Tracking parity with gerbil-shell (Gerbil/Gambit reference implementation).
Tests run against the Oils spec suite.

## Summary

| Metric | Value |
|--------|-------|
| **Total passing** | ~470/531 (~89%) |
| **Baseline (gerbil-shell)** | ~530/531 (~99%) |
| **Session start** | 1003/1179 (85.1%) |
| **Target** | Match gerbil-shell |

> Note: builtin-read results are non-deterministic (55–64/64) due to a remaining
> pipe-fd race condition under heavy sequential pipeline load. Other suites are stable.

---

## Per-Suite Results (as of 2026-03-10)

| Suite | Passing | Total | Notes |
|-------|---------|-------|-------|
| builtin-echo | **27** | 27 | ✅ Full parity |
| builtin-printf | 53 | 63 | printf %x lowercase, %c/%b raw bytes |
| builtin-read | ~56 | 64 | Non-deterministic; pipe fd race remains |
| builtin-cd | 26 | 30 | |
| builtin-trap | 28 | 33 | |
| builtin-bracket | 50 | 52 | |
| builtin-misc | 4 | 7 | |
| background | 14 | 27 | Background job wait/status |
| redirect | 33 | 41 | |
| tilde | 9 | 14 | |
| glob | 34 | 39 | |
| quote | 32 | 35 | |
| brace-expansion | 45 | 55 | |

---

## Fixes Applied This Session

### 1. `printf` — Latin-1 FFI decoding and raw byte I/O
**Files:** `ffi-shim.c`, `src/gsh/ffi.sls`, `src/compat/gambit.sls`

- Added `ffi_copy_read_buf()` C function for raw byte extraction from read buffer,
  bypassing Chez's UTF-8 locale decoding (which turned 0xCE → U+FFFD).
- Rewrote `ffi-read-all-from-fd` to use Latin-1 mapping (byte→char via `integer->char`),
  matching Gambit's `char-string` semantics.
- Rewrote `open-output-u8vector` with chunk-based accumulator using
  `make-custom-textual-output-port` + `*u8vector-raw-writers*` hashtable.
- Fixed `write-u8` to check raw-writers hashtable before falling back to fd write.
- Fixed `write-subu8vector` to write raw bytes via fd instead of `utf8->string`
  (which corrupted partial UTF-8 sequences like lone 0xCE).

**Impact:** builtin-printf 51/63 → 53/63

### 2. `fd-read-char` — Gerbil vs Chez `fdread` API mismatch
**Files:** `gerbil-shell/builtins.ss`

- Gerbil's `fdread` API: `(fdread fd buf start end) → count` (writes into provided buffer)
- Chez's `fdread` API: `(fdread fd count) → bytevector` (returns new bytevector)
- The compiled `builtins.sls` called `fdread` with 4 args (Gerbil style), which raised
  an arity error on Chez, silently returning empty and causing all pipe-based reads to fail.
- Fix: use `ffi-fdread` which has consistent `(fd count) → bytevector` API on both platforms.

**Impact:** builtin-read 42/64 → 57/64

### 3. Pipe fd buffer-mode race condition
**Files:** `src/compat/gambit.sls`

- `open-input-file` for `/dev/fd/N` paths was using `(buffer-mode block)`.
- Chez's block-buffered textual port can eagerly consume bytes from the pipe fd into its
  internal buffer, consuming data that `ffi-fdread` (used by the read builtin) also reads
  from the same raw fd, causing intermittent data loss in sequential pipelines.
- Fix: changed to `(buffer-mode none)` for `/dev/fd/N` ports.

**Impact:** Reduced intermittent failures in builtin-read and IFS-splitting tests.

### 4. Expander octal/hex escape PUA encoding
**Files:** `gerbil-shell/expander.ss`

- In `$'\NNN'` and `$'\xNN'` syntax, bytes ≥ 128 must use PUA encoding
  (`byte->raw-char`) so they survive as raw bytes through string operations.
- Extended octal handler from `\0NNN` to `\NNN` (any octal digit as first char).
- Both octal and hex handlers now use `byte->raw-char` for bytes ≥ 128.

---

## Known Remaining Issues

### `builtin-read` — pipe fd race (intermittent)
- Under heavy sequential pipeline load (4+ `echo | (read ...)` in one script),
  `ffi-fdread` occasionally returns EOF prematurely.
- Root cause not fully identified. Likely related to Chez thread scheduling
  interacting with the pipe fd lifecycle.
- Affects: `IFS='x '` tests (#56–62), backslash tests (#63).

### `builtin-printf` — 10 remaining failures
- `%x` lowercase: `number->string 16` on Chez returns uppercase. Fix: add `string-downcase`.
- `%c` with unicode: first byte extraction.
- `%b` with `\NNN` octal.

### `background` — 13 remaining failures
- `wait` returns wrong exit status (already-reaped child race with SIGCHLD handler).
- Builtins/compound commands in background produce no output (stdout routing).

### `redirect` — 8 remaining failures
- Redirect persistence in subshells.

### `brace-expansion` — 10 remaining failures
- Various edge cases.

### `tilde` — 5 remaining failures
- Tilde expansion edge cases.

---

## How to Run Tests

```bash
# Single suite
python3 gerbil-shell/test/run_spec.py \
  /home/jafourni/mine/gerbil-shell/_vendor/oils/spec/builtin-read.test.sh \
  ./gsh

# Full compat report vs gerbil-shell
python3 gerbil-shell/test/gen_compat_report.py \
  /home/jafourni/mine/gerbil-shell/_vendor/oils \
  ./gsh \
  gerbil-shell/.gerbil/bin/gsh
```
