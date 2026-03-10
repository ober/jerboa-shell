# Fix Plan: Jerboa-Shell Compat Test Parity with Gerbil-Shell

## Current Status

| Shell | Passed | Total | Rate | Timeouts |
|-------|--------|-------|------|----------|
| Jerboa-shell (Chez) | 1003 | 1179 | 85.1% | 0 |
| Gerbil-shell | 1063 | 1179 | 90.2% | 0 |
| **Delta** | **-60** | | **-5.1pp** | |

## Regression Summary by Spec File

| Spec File | Regressions | Priority |
|-----------|-------------|----------|
| builtin-read | 22 | P0 |
| builtin-printf | 14 | P1 |
| builtin-echo | 9 | P1 |
| background | 9 | P1 |
| redirect | 4 | P2 |
| tilde | 4 | P2 |
| builtin-cd | 4 | P2 |
| builtin-trap | 3 | P2 |
| builtin-bracket | 2 | P3 |
| brace-expansion | 2 | P3 |
| glob | 2 | P3 |
| quote | 1 | P3 |
| builtin-eval-source | 1 | P3 |
| case_ | 1 | P3 |
| loop | 1 | P3 |
| builtin-misc | 1 | P3 |

---

## P0: `read` Builtin — 22 Regressions

### Root Cause Analysis

The `read` builtin implementation in `gerbil-shell/builtins.ss:1008` has multiple issues centered around the Chez port's I/O layer differences. The core problem is that `port-or-fd-read-char` uses `fdread` for pipeline stdin, but many `read` options don't work correctly through this code path.

### Issue 1: `read` with zero args returns status 1 (test #4)

**File:** `gerbil-shell/builtins.ss:1215-1219`

When `read` is called with no variable names, `vars` is `[]` and `array-name` is `#f`. The code falls into `use-reply?` path (line 1232) which stores into `$REPLY`. The success paths all return `(if got-eof? 1 0)`. However, when reading from a pipe with no trailing newline, `got-eof?` is set true and status 1 is returned even when non-empty data was successfully read. Bash returns 0 when it successfully reads data, even without a trailing newline, when no var names given.

**Fix:** When `read` with no args successfully reads a non-empty line (even at EOF), return 0. Change the return logic to `(if (and got-eof? (string=? line "")) 1 0)` in the use-reply? path.

### Issue 2: `read -n N` not reading from pipe (tests #5, #10, #11, #13, #52)

**File:** `gerbil-shell/builtins.ss:1067-1118`

`read -n N` uses `port-or-fd-read-char` which goes through `fdread` on the raw pipeline fd. The `fdread` path reads one byte at a time. When combined with `-d` (delimiter) the logic is correct in structure, but:

1. The `pipe-fd` may be `#f` when input comes from a heredoc or pipe redirect (not pipeline stdin)
2. `port-or-fd-read-char` falls back to `read-char` on the Gambit port, which may buffer differently on Chez
3. `-n` combined with `-d` doesn't respect the delimiter correctly in some edge cases

**Fix:**
- Ensure `pipe-fd` is set correctly for all input sources (not just pipeline stdin)
- When `read -n N` reads from a pipe, use `read-char` on the port (not `fdread`) since Chez ports handle pipe EOF correctly
- Fix `-n` + `-d` interaction: delimiter should stop reading even when count not reached

### Issue 3: `read -d ''` null delimiter (tests #31, #46)

**File:** `gerbil-shell/builtins.ss:1069-1071, 1120-1123`

The code at lines 1069-1071 and 1120-1123 converts empty `-d ''` to NUL char:
```scheme
(delim-ch (if (string=? delim "")
            (integer->char 0)     ;; Convert empty string to NUL char
            (string-ref delim 0)))
```

**Semantic mismatch:** POSIX/bash treats `-d ''` as "read NUL-separated records" (stop at NUL byte). The conversion to `(integer->char 0)` is correct in principle, but `port-or-fd-read-char` may not handle NUL bytes correctly through the Chez character port — Chez's `read-char` may skip or mishandle NUL.

**Fix:** For null delimiter mode, use byte-level I/O (`get-u8` / `fdread`) instead of `read-char`, since NUL is a valid delimiter byte but problematic as a Scheme character in some implementations. Compare each byte to 0 rather than each char to `#\nul`.

### Issue 4: `read -s` from pipe (test #23)

**File:** `gerbil-shell/builtins.ss:1030-1032`

`-s` (silent) only disables echo on TTYs via `tty-mode-set!`. The test has `-s` reading from a pipe, which isn't a TTY, so `-s` should be a no-op. The actual failure is likely in the reading logic when `-s` interacts with pipe-fd. The test expects `read -s -n2 var` to read 2 chars from pipe — same as Issue 2.

**Fix:** Same as Issue 2 — fix `-n` reading from pipes.

### Issue 5: `read -p` prompt fd check (test #43)

**File:** `gerbil-shell/builtins.ss:1019-1022`

```scheme
(when (and (> (string-length prompt) 0)
           (= (ffi-isatty (or fd 0)) 1))
  (display prompt (current-error-port))
  (force-output (current-error-port)))
```

**Problem:** The isatty check is on the wrong fd. It checks `ffi-isatty(0)` (stdin), but bash checks if **stderr** (fd 2, where the prompt is displayed) is a tty. The prompt should be shown if stderr is a tty, regardless of whether stdin is a pipe.

The actual test #43 mismatch (`got: 'hi\nhi\n'` vs `expected: 'hi\nh\n'`) also involves a second `read -n1` not reading correctly — related to Issue 2.

**Fix:** Change isatty check to `(= (ffi-isatty 2) 1)` to check stderr instead of stdin. Also fix the -n reading issue (Issue 2).

### Issue 6: `read` without -r, backslash handling inconsistency (tests #14, #54, #63, #64)

**File:** `gerbil-shell/builtins.ss:1182-1211`

**Critical inconsistency across modes:**

- **Default line read** (lines 1204-1207): Keeps BOTH backslash and char: `(display #\\ buf) (display next buf)`
- **`-d` mode** (line 1159-1160): Keeps only the char: `(display next buf)`
- **`-n` mode** (line 1113-1114): Keeps only the char: `(display next buf)`

The default mode keeps backslashes because "backslash removal happens during IFS split" (comment at line 1205). But this means the `read-strip-backslashes` function (line 1423) must be called on the result before storing into variables. The issue is:
- `read` without args → `use-reply?` path (line 1233) calls `read-strip-backslashes` ✓
- `read` with vars → IFS split path (line 1245-1247) calls `read-ifs-split` which should handle backslashes ✓
- But `-d` and `-n` modes strip backslashes during reading, then `read-strip-backslashes` is applied AGAIN, double-stripping

**Fix:**
- Make backslash handling consistent across all modes
- Ensure `read-strip-backslashes` is applied exactly once in all non-raw paths
- Fix backslash-newline continuation when `-d delim` is set: continuation should only apply when the delimiter is newline

### Issue 7: IFS splitting with non-default delimiters (tests #56-59, #61-64)

**File:** `gerbil-shell/builtins.ss:1440-1448` (`read-ifs-split-raw`)

Multiple IFS edge cases fail:
- `IFS='x '` with `read -a`: trailing delimiters should create empty fields
- Multi-character IFS with mixed whitespace/non-whitespace
- Backslash + IFS interactions in non-raw mode

The `read-ifs-split-raw` and `read-ifs-split` functions have subtle differences from bash's IFS splitting:
- Non-whitespace IFS chars are "hard" delimiters (each one creates a field boundary)
- Whitespace IFS chars are "soft" (collapse together)
- When `max-fields` is 0 (unlimited, for `-a`), trailing non-whitespace delimiters should create empty trailing fields
- Backslash-escaped IFS chars should not split

**Fix:** Rewrite `read-ifs-split` and `read-ifs-split-raw` to match bash's exact IFS splitting semantics. Test edge cases with `IFS='x '` extensively.

### Issue 8: Smooshed option parsing (test #45)

**File:** `gerbil-shell/builtins.ss:1269-1415`

`read -rn1` with smooshed flags: `-rn1` should set `raw?=#t` and `nchars=1`. The smooshed flag parser at line 1274 looks correct in structure but may have an issue with the order of flag processing when `-n` consumes the rest of the arg.

**Fix:** Verify that `-rn1` correctly parses as `-r -n 1` and that the remaining characters after `-n` are treated as the count, not as more flags.

### Implementation Plan

1. Fix `port-or-fd-read-char` to work correctly on Chez for pipe input (biggest impact — fixes tests #5, #10, #11, #13, #23, #43, #45, #52)
2. Fix null-delimiter byte-level reading (fixes #31, #46)
3. Fix IFS splitting for non-default delimiters (fixes #56-59, #61-64)
4. Fix backslash handling in non-raw mode (fixes #14, #54, #63, #64)
5. Fix zero-args return status (fixes #4)

**Files to modify:**
- `gerbil-shell/builtins.ss` — read builtin and IFS split functions

---

## P1: `printf` — 14 Regressions

### Issue 1: `%x` always outputs uppercase (tests #14, #16, #22, #23, #26, #29, #45, #62)

**File:** `gerbil-shell/builtins.ss:3569-3570`

```scheme
(raw (number->string n 16))
(raw (if (char=? spec #\X) (string-upcase raw) raw))
```

The code uses `number->string n 16` which on Chez Scheme produces uppercase hex digits (e.g., `"2A"` instead of `"2a"`). The code only calls `string-upcase` for `%X`, assuming `number->string` returns lowercase. On Chez, `number->string` with radix 16 returns uppercase.

**Fix:** Add `(raw (string-downcase raw))` after `number->string` to normalize to lowercase, then upcase only for `%X`:

```scheme
(raw (string-downcase (number->string n 16)))
(raw (if (char=? spec #\X) (string-upcase raw) raw))
```

### Issue 2: `printf %c` crashes with status 1 (test #38)

**File:** `gerbil-shell/builtins.ss:3581-3594`

The `%c` handler uses `open-output-u8vector` and `get-output-u8vector` which are Gambit-specific. On Chez (via jerboa compat), these may not exist or behave differently, causing an exception caught by the outer handler returning status 1.

**Fix:** Replace Gambit-specific u8vector port operations with Chez-compatible bytevector I/O:
```scheme
((#\c)
 (when (and (string? arg) (> (string-length arg) 0))
   (let* ((ch (string-ref arg 0))
          (cp (char->integer ch)))
     (write-u8 cp buf)))
 (values (+ i 1) rest))
```
For multi-byte chars, use `string->utf8` from Chez to get the first byte.

### Issue 3: `printf %c` with unicode only prints char, not first byte (test #39)

Related to Issue 2. Bash's `%c` outputs the first **byte** of the UTF-8 encoding. The fix for Issue 2 should handle this — extract first byte via `(bytevector-u8-ref (string->utf8 (string ch)) 0)`.

### Issue 4: `printf %b` not handling `\NNN` octal without leading 0 (tests #58, #59)

**File:** `gerbil-shell/builtins.ss:3742-3787` (`printf-interpret-b-escapes`)

The `%b` handler delegates non-octal escapes to `printf-escape` (line 3784). `printf-escape` handles `\NNN` (octal without leading 0) at lines 3728-3737. But the issue is that `printf-interpret-b-escapes` checks for `\0NNN` and `\1`-`\7` starts — it does handle both forms. The actual bug may be in the `write-u8` call going to `buf` which is a u8vector output port, and the raw byte may not be flushed correctly.

**Fix:** Verify that `write-u8` works correctly on Chez u8vector output ports. If not, adapt the byte writing to use Chez-compatible APIs.

### Issue 5: `printf %b` with `\044` (dollar sign) outputs empty (test #55)

Test expects `printf '%b' '\044'` to output `$`. The `\0` prefix in `\044` triggers the `\0NNN` path which reads up to 3 octal digits after the 0: `44` → value 36 → byte 0x24 = `$`. This should work, but the `write-u8` on a u8vector port may fail on Chez.

**Fix:** Same as Issue 4 — verify u8vector port `write-u8` compatibility.

### Issue 6: Invalid UTF-8 byte handling in `printf '%c'` (test #27)

The test uses `printf '%x' "'$byte"` where `$byte` is a raw byte like `\xce`. On Chez, single-byte characters above 127 may be treated differently. Chez's `char->integer` on invalid UTF-8 gives the Unicode replacement character U+FFFD.

**Fix:** When the `'char` form encounters raw bytes (from `$'\xNN'` syntax), extract the raw byte value rather than the Unicode code point. This may require checking for Private Use Area encoding used by the shell's raw-byte mechanism.

### Implementation Plan

1. Fix `number->string` lowercase for `%x` (fixes 8 tests — biggest impact)
2. Fix `%c` to use Chez-compatible byte extraction (fixes 2 tests)
3. Fix `%b` u8vector port compatibility (fixes 3 tests)
4. Fix raw byte handling for `'char` printf argument (fixes 1 test)

**Files to modify:**
- `gerbil-shell/builtins.ss` — printf format handlers

---

## P1: `echo -e` — 9 Regressions

### Root Cause: Raw byte output ordering

**File:** `gerbil-shell/builtins.ss:3283-3375` (`echo-expand-escapes`, `display-raw-bytes`)

All 9 echo failures show the same pattern: the raw byte appears at the wrong position in the output. For example, `echo -e 'ab\x63d'` outputs `abcdf\ne` instead of `abcdef\n`. The character 'e' (0x65) and 'f' appear swapped with the escaped character.

The `echo-expand-escapes` function returns a list of segments (strings and fixnums for raw bytes). The segments are accumulated in reverse order using `acc` and then reversed at the end. The issue is in how `flush-buf!` interacts with the accumulator:

```scheme
(loop j (cons byte (flush-buf! acc)))
```

This flushes the current string buffer and then adds the byte. But `flush-buf!` returns `(cons current-string acc)` where `acc` is in reverse order. The byte gets consed onto this, so it ends up in the right position in the reversed list... but there may be a subtle bug in the `flush-buf!` + `cons` sequence when the `get-output-string` side-effects interact with Chez.

**Confirmed Chez-specific issue:** `get-output-string` on Chez does NOT reset the output port. On Gambit, `get-output-string` drains the buffer and resets it. On Chez, subsequent writes to `buf` include old content, causing all raw-byte escapes to produce corrupted output — bytes appear at wrong positions because the string buffer accumulates text from before AND after the byte.

The `flush-buf!` function at line 3298 calls `get-output-string buf` but doesn't reset the port:
```scheme
(define (flush-buf! acc)
  (let ((s (get-output-string buf)))
    (if (string=? s "") acc (cons s acc))))
```

After this call, `buf` still contains the old text. When the loop continues and writes more chars to `buf`, they get prepended with the already-flushed content.

**Fix:** After `get-output-string buf`, explicitly reinitialize `buf`:
```scheme
(define (flush-buf! acc)
  (let ((s (get-output-string buf)))
    (set! buf (open-output-string))  ;; reset — critical for Chez
    (if (string=? s "") acc (cons s acc))))
```

This same pattern also affects `printf-escape` and `printf-interpret-b-escapes` which use `write-u8` to a u8vector output port — verify those ports also handle the Chez `get-output-string`/`get-output-u8vector` semantics correctly.

### Implementation Plan

1. Fix `get-output-string` buffer reset in `echo-expand-escapes` (fixes all 9 tests)

**Files to modify:**
- `gerbil-shell/builtins.ss` — `echo-expand-escapes` function
- Possibly `src/compat/gambit.sls` — if `get-output-string` needs a compat wrapper

---

## P1: Background Jobs & `wait` — 9 Regressions

### Issue 1: `wait` returns 0 instead of process exit status (tests #8, #16, #17, #21)

**File:** `gerbil-shell/jobs.ss:268-341` (`job-wait`)

`wait $PID` calls `job-table-get` to look up the job by PID. For external processes launched via `ffi-fork-exec`, the PID in the job table is the real PID. But `job-table-get` may not find the job if it was already cleaned up or if the PID format doesn't match (string vs number comparison).

When `job-table-get` returns `#f`, the wait builtin returns 127 (line 1952). But tests show status 0, suggesting the job IS found but `job-wait` returns 0.

Root cause: In `job-wait`, the `ffi-waitpid-pid` call with `WNOHANG` may return 0 (child not yet exited) initially, then on retry the child has already been reaped by SIGCHLD handler. When `ffi-waitpid-pid` returns -1 (error, ECHILD because child already reaped), the code falls to the `else` branch (line 328) which sets `last-exit-code` to 0.

**Fix:** When `waitpid` returns -1 (ECHILD), check if the job's thread has a saved exit status. Alternatively, save exit status in the job-process struct when SIGCHLD is received, so `job-wait` can retrieve it even after the process is reaped.

### Issue 2: Builtins/compound commands in background produce no output (tests #9, #19)

**File:** `gerbil-shell/executor.ss:1104-1120` (`launch-background`)

For non-simple commands (compound/builtins), `launch-background` spawns a thread:
```scheme
(th (spawn (lambda ()
  (parameterize ((*in-subshell* #t))
    (execute-command cmd child-env)))))
```

The thread runs in the same process and inherits stdout via closure. However:
1. **No `force-output` before thread exit** — output is buffered and lost when thread is GC'd
2. **No explicit port parameterization** — unlike `launch-thread-piped` in `pipeline.ss:295-311` which explicitly creates and parameterizes output ports
3. The `*in-subshell*` flag may cause output routing issues if it affects how builtins write

**Contrast with correct pattern** in `pipeline.ss:311` which calls `force-output` before closing the port:
```scheme
(force-output out-port)
(close-output-port out-port)
```

**Fix:**
- Add `force-output` in the thread after `execute-command` returns (in a `dynamic-wind` cleanup)
- Ensure background threads explicitly parameterize `current-output-port` to the parent's actual stdout
- For background for-loops (`control.ss:39-88`), add `force-output` after each loop iteration body

### Issue 3: `wait -n` returns 0 instead of first-finished exit status (test #18)

**File:** `gerbil-shell/jobs.ss:346-417` (`job-wait-any`, `job-check-finished?`)

`job-wait-any` polls running jobs via `job-check-finished?` (lines 377-417) which uses non-blocking `ffi-waitpid-pid ... WNOHANG`. **Two problems:**

1. `job-check-finished?` detects completion but does NOT update the job's status — it only returns `#t`/`#f`
2. After detecting completion, the code calls `job-wait` which does ANOTHER `waitpid` — but the process was already reaped by the first check, causing `waitpid` to return -1 (ECHILD), which falls to the `else` branch (line 328) that sets `last-exit-code` to 0

**Race condition:** Between `job-check-finished?` (non-blocking check) and `job-wait` (blocking wait), the child is reaped by the first call, so the second call fails and returns 0.

**Fix:** In `job-check-finished?`, when `waitpid` returns > 0, immediately save the exit status in the `job-process` struct. Then `job-wait` should check for the saved status before calling `waitpid` again. Alternatively, make `job-check-finished?` update the job status atomically when it discovers completion.

### Issue 4: Trap not cleared in background subshell (trap test #26)

**File:** `gerbil-shell/executor.ss:1108-1119`

Bash clears traps in child processes started with `&`. The thread-based "subshell" for compound commands doesn't clear the trap table in the cloned environment.

**Fix:** In `launch-background`, after cloning the environment, clear `*trap-table*` in the child thread's dynamic scope.

### Implementation Plan

1. Fix `waitpid`/ECHILD handling to preserve exit status (fixes 4 tests)
2. Fix background thread stdout routing (fixes 2 tests)
3. Fix `wait -n` polling to actually check process status (fixes 1 test)
4. Clear traps in background subshells (fixes 1 test, also fixes trap test)
5. Fix `wait` for all background jobs to properly collect statuses (fixes 1 test)

**Files to modify:**
- `gerbil-shell/jobs.ss` — job-wait, job-wait-any
- `gerbil-shell/executor.ss` — launch-background
- `gerbil-shell/signals.ss` — trap clearing for subshells

---

## P2: Redirections — 4 Regressions

### Issue 1: Writing to fd 3/4 multiple times loses first write (tests #17, #18)

**Expected:** Two `echo` writes to `exec 3>file` should produce both lines.
**Got:** Only the last line appears.

**File:** `gerbil-shell/redirect.ss:173-175, 671-678`

When `exec 3>file` is applied, `set-port-for-fd!` (line 671) opens the file and creates a NEW Gambit port. This port is set via `(current-output-port port)` parameterization. When another redirect targets the same fd (e.g., `echo foo >&3`), a new port is created that replaces the previous one. The old port's buffered content is lost.

**Fix:** For persistent redirections (`exec N>file`), open the fd once and track it in the shell's fd table. Subsequent writes to fd N should reuse the existing fd/port rather than reopening the file. Ensure `set-port-for-fd!` checks for existing persistent fds before creating new ports.

### Issue 2: `1>&2-` (move fd) not working (test #27)

The `N>&M-` syntax means "dup fd M to fd N, then close M". This may not be implemented in the Chez redirect layer.

**Fix:** Implement fd-move semantics in the redirect handler: `dup2(M, N)` followed by `close(M)`.

### Issue 3: `<>` read/write mode not preserving position (test #29)

`<> file` opens file for both reading and writing. After reading, the write position should be where the read left off. The Chez port may open separate read/write ports or not handle bidirectional fd correctly.

**Fix:** Use a single fd opened with `O_RDWR` and ensure read/write share the same file offset. May require FFI `open()` with `O_RDWR` flag.

**Files to modify:**
- `src/gsh/redirect.sls`
- `gerbil-shell/redirect.ss`

---

## P2: Tilde Expansion — 4 Regressions

### Issue 1: `~nonexistent` expands to $HOME instead of literal (test #6)

**File:** `gerbil-shell/expander.ss:720-724`

```scheme
(else
 (with-catch
  (lambda (e) (values (substring str i end) end))
  (lambda ()
    (values (user-info-home (user-info prefix)) end))))
```

When `user-info` throws for a nonexistent user, the catch should return the literal `~nonexistent`. But on Chez, `user-info` might not throw — it might return a default or the current user's info. Or the Chez compat `user-info` implementation falls back differently.

**Fix:** Check the `user-info` implementation in the compat layer. Ensure it throws when the user doesn't exist. If it silently returns current user info, add an explicit check.

### Issue 2: `${x//~/~root}` not expanding tilde in replacement (test #8)

**File:** `gerbil-shell/expander.ss`

In `${var//pattern/replacement}`, tilde in the replacement should expand. The current code may not run tilde expansion on the replacement string.

**Fix:** Apply tilde expansion to the replacement string in `${var//pat/repl}` before substitution.

### Issue 3: `x=foo:~` tilde in colon-separated values (test #9)

**File:** `gerbil-shell/expander.ss:461-470`

In assignment context, `~` should expand after `:` in values like `foo:~`. The `expand-assignment-value` function should handle this. Test shows `foo:~,` is incorrectly expanding the tilde (expected `foo:~,` to NOT expand because `,` follows `~` without `/`).

**Fix:** Tilde after `:` in assignment context should only expand when followed by `/` or end of string, not arbitrary chars.

### Issue 4: Temp assignment `x=~` with `env` (test #14)

Tilde expansion in temp assignments before commands (e.g., `x=~root:~ env`) should expand `~root` to root's home. Related to user-info lookup (Issue 1).

**Fix:** Same as Issue 1 — fix `user-info` compat to correctly resolve other users.

**Files to modify:**
- `gerbil-shell/expander.ss` — `expand-tilde-in`, `expand-assignment-value`
- `src/compat/gambit.sls` — `user-info` implementation

---

## P2: `cd` Builtin — 4 Regressions

### Issue 1: `cd` with strict_arg_parse (test #3)

`cd --` should succeed (status 0), but returns 1. The option parser may treat `--` as an error when no directory follows, rather than as "cd to $HOME".

**Fix:** In `cd` option parsing, `cd --` (with no further args) should cd to `$HOME`, same as bare `cd`.

### Issue 2: `pwd` in symlinked dir (test #15)

When the shell starts in a directory that's a symlink, `pwd` should show the symlink path (logical). The Chez port may resolve symlinks eagerly.

**Fix:** Initialize `$PWD` from the environment's `PWD` variable (if it points to the correct directory) rather than using `current-directory` which may resolve symlinks.

### Issue 3: `cd` with inherited PWD disagreement (tests #25, #26)

When `PWD` is inherited but doesn't match the actual directory, `cd` should still work. The Chez port may not handle the case where `$PWD` disagrees with `getcwd()`.

**Fix:** In `cd`, when the inherited `$PWD` disagrees with `getcwd()`, update `$PWD` to reflect the actual directory before attempting relative path resolution.

**Files to modify:**
- `gerbil-shell/builtins.ss` — cd builtin
- `gerbil-shell/main.ss` — PWD initialization

---

## P2: `trap` — 3 Regressions

### Issue 1: Traps not cleared in subshell via `&` (test #26)

Background subshells (`cmd &`) should start with an empty trap table. Currently thread-based subshells inherit the parent's `*trap-table*`.

**Fix:** In `launch-background`, parameterize `*trap-table*` with a fresh hash-table in the child thread.

### Issue 2: trap USR1 + sleep not working non-interactively (test #27)

`trap 'echo usr1' USR1; kill -USR1 $$; sleep 0.1` should print "usr1". The signal may not be delivered or the trap handler may not execute during `sleep`.

**Fix:** Ensure `pending-signals!` is checked after `sleep` completes, and that signal flag checking works for USR1 in non-interactive mode.

### Issue 3: trap EXIT + sleep + SIGINT (test #29)

Similar to Issue 2 — EXIT trap should fire when the shell receives SIGINT during sleep.

**Fix:** Ensure EXIT trap fires on all exit paths including signal-induced exits.

**Files to modify:**
- `gerbil-shell/signals.ss` — signal delivery in non-interactive mode
- `gerbil-shell/executor.ss` — launch-background trap clearing

---

## P3: `file-info` Compat Layer — 4 Regressions (across bracket, cd)

### Root Cause: Stubbed-out stat fields

**File:** `src/compat/gambit.sls:282-284`

```scheme
(make-file-info-rec type (if (< size 0) 0 size) mode
  0 0     ;; device/inode — use real stat if needed
  0 0     ;; owner/group
  ...)
```

`device`, `inode`, `owner`, and `group` are all hardcoded to 0. This breaks:
- `test -G` (effective group ownership) — always fails (test #37)
- `test -O` (effective user ownership) — always fails (test #37)
- `test -ef` (same file by device+inode) — can't compare because both are 0 (test #42)

### Fix

Add FFI functions to the C shim (`ffi-shim.c`) to extract full stat fields:

```c
int ffi_file_uid(const char *path) { struct stat st; return stat(path, &st) == 0 ? st.st_uid : -1; }
int ffi_file_gid(const char *path) { struct stat st; return stat(path, &st) == 0 ? st.st_gid : -1; }
long long ffi_file_dev(const char *path) { struct stat st; return stat(path, &st) == 0 ? (long long)st.st_dev : -1; }
long long ffi_file_ino(const char *path) { struct stat st; return stat(path, &st) == 0 ? (long long)st.st_ino : -1; }
```

Then update `file-info` to use them:

```scheme
(define c-ffi-file-uid (foreign-procedure "ffi_file_uid" (string) int))
(define c-ffi-file-gid (foreign-procedure "ffi_file_gid" (string) int))
(define c-ffi-file-dev (foreign-procedure "ffi_file_dev" (string) long-long))
(define c-ffi-file-ino (foreign-procedure "ffi_file_ino" (string) long-long))

(define (file-info path . follow?)
  (let* ((follow (if (pair? follow?) (car follow?) #t))
         (type-int (c-ffi-file-type path (if follow 1 0))))
    (if (= type-int -1)
      (error 'file-info "cannot stat file" path)
      (make-file-info-rec
        (file-type-int->symbol type-int)
        (let ((s (c-ffi-file-size path))) (if (< s 0) 0 s))
        (c-ffi-file-mode path)
        (c-ffi-file-dev path)
        (c-ffi-file-ino path)
        (c-ffi-file-uid path)
        (c-ffi-file-gid path)
        (make-time 'time-utc 0 (let ((m (c-ffi-file-mtime path))) (if (< m 0) 0 m)))
        (make-time 'time-utc 0 0)))))
```

**Files to modify:**
- `ffi-shim.c` — add uid/gid/dev/ino FFI functions
- `src/compat/gambit.sls` — update `file-info` to use them

---

## P3: Glob — 2 Regressions

### Issue 1: Unicode char in glob pattern (test #31)

`echo __?__` should match both `__a__` and `__μ__`. The `?` glob should match any single character, including multi-byte UTF-8 characters. The Chez glob implementation may treat `?` as matching a single byte instead of a single character.

**Fix:** In `gerbil-shell/glob.ss`, ensure glob `?` matches a single Unicode character, not a single byte. The regex `[^/]` generated for `?` should be `[^/]` with Unicode mode enabled.

### Issue 2: `shopt -u globskipdots` (test #39)

`shopt -u globskipdots` should make `*` match `.` and `..`. This shopt option may not be implemented.

**Fix:** Add `globskipdots` to the shopt handling. When disabled, glob patterns should include dotfiles including `.` and `..`.

**Files to modify:**
- `gerbil-shell/glob.ss`
- `gerbil-shell/builtins.ss` — shopt handler

---

## P3: Brace Expansion — 2 Regressions

### Issue 1: Tilde in brace expansion (test #30)

`echo ~bob/src{,~root}` should expand to `/home/bob/src /root`. Tilde at the start of brace elements should expand.

**Fix:** Apply tilde expansion to each brace-expanded result, not just the original word.

### Issue 2: Side effect ordering in `{a,b,c}` (test #53)

`echo {a,b,c}-$((i++))` should produce `a-0 b-1 c-2` (left-to-right evaluation). Currently produces `a-1 b-2 c-0`, suggesting the arithmetic expression is evaluated first for all expansions, then assigned.

**Fix:** Evaluate `$((i++))` for each brace-expanded word in left-to-right order, not all at once.

**Files to modify:**
- `gerbil-shell/expander.ss` — brace expansion and tilde interaction

---

## P3: Quote — 1 Regression

### Issue: `$'\377'` octal in ANSI-C quoting (test #28)

`$'\377'` should produce byte 0xFF. The output shows the byte appears but at the wrong position, similar to the echo-e issue.

**Fix:** Same root cause as echo-e — `get-output-string` buffer reset issue in the ANSI-C quote expander. Apply the same fix.

**Files to modify:**
- `gerbil-shell/expander.ss` — ANSI-C quoting handler

---

## P3: `source` Along PATH — 1 Regression

### Issue: Source doesn't skip directories in PATH (test #23)

`source myfile` should search PATH and skip entries that are directories. `find-file-in-path` at `gerbil-shell/util.ss:164` calls `file-directory?` which may not work correctly through the Chez compat layer.

**Fix:** Verify `file-directory?` works correctly on Chez. It may need to use the stat-based FFI rather than Chez's built-in `file-directory?` which might have different semantics.

**Files to modify:**
- `gerbil-shell/util.ss` — `find-file-in-path`
- `src/compat/gambit.sls` — verify `file-directory?`

---

## P3: `case` — 1 Regression

### Issue: Matching byte 0xFF against empty string (test #10)

`case $'\xff' in '') echo a;; *) echo b;; esac` should match `*` (not empty), outputting `b`. Chez may represent the 0xFF byte differently, making the case variable appear empty.

**Fix:** Ensure raw bytes from `$'\xff'` are preserved through variable assignment and case matching. Check the PUA (Private Use Area) encoding scheme for raw bytes.

**Files to modify:**
- `gerbil-shell/expander.ss` — case pattern matching with raw bytes

---

## P3: `while` in Pipeline — 1 Regression

### Issue: Variable not visible after while-in-pipe (test #12)

`echo 1 2 3 | while read x; do ((n++)); done; echo $n` — expects `$n` to be 3. In bash with `lastpipe` enabled, the last command in a pipeline runs in the current shell. Without it, pipeline components run in subshells and variable changes are lost.

**Fix:** Check if `lastpipe` shopt is enabled (it should be in this context). If the last pipeline component is a builtin/compound command, run it in the current shell rather than a subshell.

**Files to modify:**
- `gerbil-shell/executor.ss` — pipeline execution, lastpipe handling

---

## P3: `time` Pipeline — 1 Regression

### Issue: `time` with pipeline returns status 1 (test #4)

`time ls | cat` returns status 1 instead of 0. The `time` keyword wraps a pipeline, but the status may not propagate correctly from the timed pipeline.

**Fix:** In `execute-time-command` (`gerbil-shell/executor.ss:946`), ensure the pipeline's exit status is returned, not an error status from the timing code. Check for exceptions in `fl-` or `cpu-time` on Chez.

**Files to modify:**
- `gerbil-shell/executor.ss` — `execute-time-command`

---

## Implementation Order (Recommended)

### Phase 1: Quick Wins (26 tests, ~2 days)

1. **printf %x lowercase** — Add `string-downcase` after `number->string` (8 tests)
2. **echo-e buffer reset** — Fix `get-output-string` in `echo-expand-escapes` (9 tests)
3. **file-info stat fields** — Add uid/gid/dev/ino FFI and update compat (4 tests)
4. **printf %c** — Chez-compatible byte extraction (2 tests)
5. **$'\377' quoting** — Same buffer reset fix as echo-e (1 test)
6. **source PATH directories** — Verify file-directory? compat (1 test)
7. **time pipeline status** — Fix status propagation (1 test)

### Phase 2: Medium Effort (22 tests, ~3 days)

8. **read -n from pipe** — Fix port-or-fd-read-char on Chez (6 tests)
9. **read IFS splitting** — Rewrite read-ifs-split for bash compat (8 tests)
10. **read backslash handling** — Fix non-raw mode (4 tests)
11. **read null delimiter** — Byte-level I/O for -d '' (2 tests)
12. **read misc** — Zero args status, smooshed opts (2 tests)

### Phase 3: Structural Fixes (12 tests, ~4 days)

13. **Background job wait** — Fix waitpid/ECHILD, save exit status (4 tests)
14. **Background stdout** — Fix thread output routing (2 tests)
15. **Tilde expansion** — Fix ~user, assignment context, replacement (4 tests)
16. **Redirect persistence** — Fix exec N>file fd management (3 tests)

### Phase 4: Edge Cases (6 tests, ~2 days)

17. **cd improvements** — PWD init, symlinks, arg parsing (4 tests)
18. **wait -n** — Non-blocking poll for process completion (1 test)
19. **trap in subshells** — Clear traps, signal delivery (3 tests)
20. **glob unicode** — Fix ? to match chars not bytes (1 test)
21. **brace+tilde** — Tilde in brace elements (1 test)
22. **Misc** — case 0xff, while-in-pipe lastpipe, brace side-effects, globskipdots (4 tests)

### Total: ~60 test regressions addressed across ~11 days of work

---

## Verification

After each fix, run the comparison:

```bash
python3 /tmp/compare_compat.py
```

Or test a single spec:

```bash
python3 gerbil-shell/test/run_spec.py -v \
  /home/jafourni/mine/gerbil-shell/_vendor/oils/spec/SPECNAME.test.sh \
  /home/jafourni/mine/jerboa-shell/gsh
```

Target: **1063/1179 (90.2%)** — parity with gerbil-shell.

---

## Improvements to Preserve

Jerboa-shell already passes 19 tests that gerbil-shell fails. These should not regress:

| Spec File | Tests | Count |
|-----------|-------|-------|
| exit-status | #1, #3, #4, #7, #8 | 5 |
| redirect-multi | #7, #12, #13 | 3 |
| builtin-set | #6, #7, #8 | 3 |
| pipeline | #6, #12, #23 | 3 |
| builtin-process | #23, #26 | 2 |
| smoke | #15 | 1 |
| arith | #14 | 1 |
| var-op-bash | #19 | 1 |

These represent areas where the Chez port has better behavior (likely due to different default behaviors in Chez's process handling, signal management, or numeric operations). Guard these with explicit regression tests.
