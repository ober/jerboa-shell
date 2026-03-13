#!chezscheme
;;; build-jsh-musl.ss — Build a fully static jsh binary using musl libc
;;;
;;; Usage: scheme -q --libdirs src:<jerboa-lib>:<gherkin-src> < build-jsh-musl.ss
;;;
;;; This script:
;;;   1. Compiles jsh modules (using stock scheme with glibc)
;;;   2. Creates boot file + optimized program .so
;;;   3. Generates C files with embedded boot data
;;;   4. Compiles C with musl-gcc against musl-built Chez's scheme.h
;;;   5. Links fully static binary with libkernel.a from musl-built Chez
;;;
;;; The resulting jsh-musl binary has zero runtime dependencies.

(import
  (except (chezscheme) void box box? unbox set-box!
          andmap ormap iota last-pair find
          1+ 1- fx/ fx1+ fx1-
          error error? raise with-exception-handler identifier?
          hash-table? make-hash-table)
  (jerboa build)
  (jerboa build musl))

;; ========== Validate musl setup ==========

(let ([result (validate-musl-setup)])
  (unless (eq? (car result) 'ok)
    (printf "Error: ~a~n" (cdr result))
    (printf "~nTo build Chez Scheme with musl:~n")
    (printf "  cd ~/mine/ChezScheme~n")
    (printf "  ./configure --threads --static CC=musl-gcc --installprefix=$HOME/chez-musl~n")
    (printf "  make -j$(nproc) && make install~n")
    (exit 1)))

(printf "musl Chez found: ~a~n~n" (musl-chez-lib-dir))

;; ========== Locate directories ==========

(define jerboa-dir
  (or (getenv "JERBOA_DIR")
      (format "~a/mine/jerboa/lib" (getenv "HOME"))))

;; Base jerboa directory (parent of lib/) — for support/ files
(define jerboa-dir-base
  (or (getenv "JERBOA_BASE_DIR")
      (format "~a/mine/jerboa" (getenv "HOME"))))

(define gherkin-dir
  (or (getenv "GHERKIN_DIR")
      (format "~a/mine/gherkin/src" (getenv "HOME"))))

;; ========== Step 1: Compile jsh modules ==========

(printf "[1/7] Compiling jsh modules...~n")

(define (compile-jsh-module name)
  (let ((sls (string-append "src/jsh/" name ".sls")))
    (if (file-exists? sls)
      (begin
        (printf "  Compiling ~a...~n" sls)
        (compile-library sls))
      (printf "  SKIP (not found): ~a~n" sls))))

(parameterize ([optimize-level 2]
               [generate-inspector-information #f]
               [compile-imported-libraries #t])
  ;; Compat layer first
  (compile-jsh-module "../compat/gambit")
  ;; FFI module (must be compiled even for static builds — load-shared-object
  ;; calls happen at runtime, not compile time)
  (for-each compile-jsh-module '("ffi"))
  ;; Tier 1: no deps
  (for-each compile-jsh-module '("ast" "registry"))
  ;; Tier 2+
  (for-each compile-jsh-module '("macros" "util"))
  (for-each compile-jsh-module
    '("environment" "lexer" "arithmetic" "glob" "fuzzy" "history"
      "pregexp-compat" "static-compat" "stage"))
  (for-each compile-jsh-module '("parser" "functions" "signals" "expander"))
  (for-each compile-jsh-module '("redirect" "control" "jobs" "builtins"))
  (for-each compile-jsh-module '("pipeline" "executor" "completion" "prompt"))
  (for-each compile-jsh-module '("lineedit" "fzf" "script" "startup" "sandbox" "main")))

;; ========== Step 2: Compile program ==========

(printf "~n[2/7] Compiling jsh.ss (optimize-level 3)...~n")
;; IMPORTANT: Do NOT use compile-imported-libraries here — that would
;; recompile modules and create different compilation instances than
;; what step 1 produced. The boot file references step-1 .so files,
;; so the program must match them.
(parameterize ([optimize-level 3]
               [cp0-effort-limit 500]
               [cp0-score-limit 50]
               [cp0-outer-unroll-limit 1]
               [commonization-level 4]
               [enable-unsafe-application #t]
               [enable-unsafe-variable-reference #t]
               [enable-arithmetic-left-associative #t]
               [debug-level 0]
               [generate-inspector-information #f])
  (compile-program "jsh.ss"))

;; ========== Step 3: Skip WPO for musl builds ==========
;; WPO requires all .wpo files with matching compilation instances,
;; which is fragile. Use the direct jsh.so from compile-program instead.
(printf "[3/7] Skipping WPO (using jsh.so directly)...~n")
(define program-so "jsh.so")

;; ========== Step 4: Create libs-only boot file ==========
;; NOTE: The program is NOT in the boot file — it's loaded separately
;; via Sscheme_script to preserve threading support.

(printf "[4/7] Creating libs-only boot file...~n")
(apply make-boot-file "jsh.boot" '("scheme" "petite")
  (append
    ;; Jerboa runtime + stdlib
    (map (lambda (m) (format "~a/~a.so" jerboa-dir m))
      '("jerboa/core"
        "jerboa/runtime"
        "std/error"
        "std/format"
        "std/sort"
        "std/pregexp"
        "std/sugar"
        "std/misc/string"
        "std/misc/list"
        "std/misc/alist"
        "std/misc/thread"
        "std/stm"
        "std/foreign"
        "std/os/path"
        "std/os/signal"
        "std/os/fdio"
        "std/transducer"
        "std/log"
        "std/capability"
        "std/capability/sandbox"
        "std/os/landlock"
        "std/os/sandbox"))
    ;; Gherkin runtime (MOP chain + compiler)
    (map (lambda (m) (format "~a/~a.so" gherkin-dir m))
      '("compat/types"
        "compat/gambit-compat"
        "runtime/util"
        "runtime/table"
        "runtime/c3"
        "runtime/mop"
        "runtime/error"
        "runtime/hash"
        "runtime/syntax"
        "runtime/eval"
        "reader/reader"
        "compiler/compile"
        "boot/gherkin"))
    ;; Local compat layer
    (list "src/compat/gambit.so")
    ;; jsh modules
    (map (lambda (m) (format "src/jsh/~a.so" m))
      '("ffi" "pregexp-compat" "stage" "static-compat"
        "ast" "registry" "macros" "util"
        "environment" "lexer" "arithmetic" "glob" "fuzzy" "history"
        "parser" "functions" "signals" "expander"
        "redirect" "control" "jobs" "builtins"
        "pipeline" "executor" "completion" "prompt"
        "lineedit" "fzf" "script" "startup" "sandbox" "main"))))

;; ========== Step 5: Generate C with embedded data ==========

(printf "[5/7] Generating C with embedded boot files + program...~n")

(define build-dir "/tmp/jerboa-musl-jsh-build")
(system (format "rm -rf '~a' && mkdir -p '~a'" build-dir build-dir))

(define musl-lib-dir (musl-chez-lib-dir))
(define gcc (musl-gcc-path))
(define scheme-h-dir musl-lib-dir)

;; Get boot file paths from musl Chez installation
(define musl-boots (musl-boot-files))
(define petite-boot-path (cdr (assoc "petite" musl-boots)))
(define scheme-boot-path (cdr (assoc "scheme" musl-boots)))

;; Generate static_boot.c: embeds petite.boot + scheme.boot + jsh.boot
(define static-boot-c (format "~a/static_boot.c" build-dir))
(call-with-output-file static-boot-c
  (lambda (out)
    (display "#include \"scheme.h\"\n\n" out)
    ;; Embed boot files
    (display (file->c-array petite-boot-path "petite_boot") out)
    (newline out)
    (display (file->c-array scheme-boot-path "scheme_boot") out)
    (newline out)
    (display (file->c-array "jsh.boot" "jsh_boot") out)
    (newline out)
    ;; static_boot_init for Chez's main.o
    (display "void static_boot_init(void) {\n" out)
    (display "    Sregister_boot_file_bytes(\"petite\", petite_boot, petite_boot_len);\n" out)
    (display "    Sregister_boot_file_bytes(\"scheme\", scheme_boot, scheme_boot_len);\n" out)
    (display "    Sregister_boot_file_bytes(\"jsh\", jsh_boot, jsh_boot_len);\n" out)
    (display "}\n" out))
  'replace)

;; Generate jsh_program_embed.c: embeds the optimized program .so
;; and provides a custom main that:
;; - Saves args to env vars (bypass Chez arg parsing)
;; - Calls static_boot_init + Sbuild_heap
;; - Loads program via memfd + Sscheme_script (threading workaround)
(define program-c (format "~a/jsh_main_musl.c" build-dir))
(call-with-output-file program-c
  (lambda (out)
    (display "#define _GNU_SOURCE\n" out)
    (display "#include <stdlib.h>\n" out)
    (display "#include <string.h>\n" out)
    (display "#include <stdio.h>\n" out)
    (display "#include <unistd.h>\n" out)
    (display "#include <sys/mman.h>\n" out)
    (display "#include <sys/types.h>\n" out)
    (display "#include <sys/stat.h>\n" out)
    (display "#include <fcntl.h>\n" out)
    (display "#include <signal.h>\n" out)
    (display "#include <sys/wait.h>\n" out)
    (display "#include <termios.h>\n" out)
    (display "#include \"scheme.h\"\n\n" out)
    ;; Embed program .so
    (display (file->c-array program-so "jsh_program_data") out)
    (newline out)
    ;; Declare static_boot_init (defined in static_boot.c)
    (display "extern void static_boot_init(void);\n\n" out)
    ;; Declare all FFI functions from ffi-shim.c
    (display "/* FFI symbols from ffi-shim.c */\n" out)
    (for-each
      (lambda (name) (fprintf out "extern void ~a();\n" name))
      '("ffi_do_waitpid" "ffi_get_waitpid_status"
        "ffi_byte_ready" "ffi_read_byte" "ffi_write_byte"
        "ffi_do_read_all" "ffi_copy_read_buf"
        "ffi_fdread" "ffi_fdwrite"
        "ffi_do_pipe" "ffi_pipe_read_fd" "ffi_pipe_write_fd"
        "ffi_do_execve" "ffi_fork_exec"
        "ffi_signal_was_ignored" "ffi_signal_set_ignore"
        "ffi_signal_set_default"
        "ffi_sigpipe_block" "ffi_sigpipe_unblock"
        "ffi_sigchld_block" "ffi_sigchld_unblock"
        "ffi_signal_flag_install" "ffi_signal_flag_check"
        "ffi_termios_save" "ffi_termios_restore" "ffi_set_raw_mode"
        "ffi_get_winsize" "ffi_ws_col" "ffi_ws_row"
        "ffi_do_strftime"
        "ffi_file_type" "ffi_file_mode" "ffi_file_size" "ffi_file_mtime"
        "ffi_file_uid" "ffi_file_gid" "ffi_file_dev" "ffi_file_ino"
        "ffi_getrlimit_soft" "ffi_getrlimit_hard" "ffi_setrlimit"
        "ffi_landlock_abi_version" "ffi_landlock_sandbox"
        "ffi_sandbox_fork_exec"
        "jerboa_landlock_abi_version" "jerboa_landlock_sandbox"))
    (newline out)
    ;; Wrapper functions for variadic/macro POSIX functions
    ;; (must appear before register_ffi_symbols which takes their address)
    (display "/* Wrappers for variadic/macro POSIX functions */\n" out)
    (display "static int wrap_open(const char *path, int flags, int mode) { return open(path, flags, mode); }\n" out)
    (display "static int wrap_fcntl(int fd, int cmd, int arg) { return fcntl(fd, cmd, arg); }\n" out)
    (display "static int wrap_mkfifo(const char *path, int mode) { return mkfifo(path, mode); }\n" out)
    (display "static int wrap_umask(int mask) { return (int)umask((mode_t)mask); }\n\n" out)
    ;; Register all FFI symbols so Chez foreign-procedure can find them
    ;; (load-shared-object is disabled in static builds)
    (display "static void register_ffi_symbols(void) {\n" out)
    ;; ffi-shim.c functions
    (for-each
      (lambda (name) (fprintf out "    Sforeign_symbol(\"~a\", (void*)~a);\n" name name))
      '("ffi_do_waitpid" "ffi_get_waitpid_status"
        "ffi_byte_ready" "ffi_read_byte" "ffi_write_byte"
        "ffi_do_read_all" "ffi_copy_read_buf"
        "ffi_fdread" "ffi_fdwrite"
        "ffi_do_pipe" "ffi_pipe_read_fd" "ffi_pipe_write_fd"
        "ffi_do_execve" "ffi_fork_exec"
        "ffi_signal_was_ignored" "ffi_signal_set_ignore"
        "ffi_signal_set_default"
        "ffi_sigpipe_block" "ffi_sigpipe_unblock"
        "ffi_sigchld_block" "ffi_sigchld_unblock"
        "ffi_signal_flag_install" "ffi_signal_flag_check"
        "ffi_termios_save" "ffi_termios_restore" "ffi_set_raw_mode"
        "ffi_get_winsize" "ffi_ws_col" "ffi_ws_row"
        "ffi_do_strftime"
        "ffi_file_type" "ffi_file_mode" "ffi_file_size" "ffi_file_mtime"
        "ffi_file_uid" "ffi_file_gid" "ffi_file_dev" "ffi_file_ino"
        "ffi_getrlimit_soft" "ffi_getrlimit_hard" "ffi_setrlimit"
        "ffi_landlock_abi_version" "ffi_landlock_sandbox"
        "ffi_sandbox_fork_exec"
        "jerboa_landlock_abi_version" "jerboa_landlock_sandbox"))
    ;; POSIX functions used via foreign-procedure in ffi.sls
    ;; These are real C functions (not macros) from musl libc
    (for-each
      (lambda (name) (fprintf out "    Sforeign_symbol(\"~a\", (void*)~a);\n" name name))
      '("fork" "_exit" "close" "dup" "dup2" "read" "write" "lseek" "access"
        "unlink" "getpid" "getppid" "kill" "sysconf" "waitpid"
        "setpgid" "getpgid" "tcsetpgrp" "tcgetpgrp" "setsid"
        "getuid" "geteuid" "getegid" "isatty" "unsetenv"))
    ;; Variadic/macro POSIX functions need wrappers (defined above)
    (for-each
      (lambda (name) (fprintf out "    Sforeign_symbol(\"~a\", (void*)wrap_~a);\n" name name))
      '("open" "fcntl" "mkfifo" "umask"))
    (display "}\n\n" out)
    ;; Custom main
    (display "int main(int argc, char *argv[]) {\n" out)
    (display "    /* Save args in env vars (bypass Chez arg parsing) */\n" out)
    (display "    char buf[32];\n" out)
    (display "    snprintf(buf, sizeof(buf), \"%d\", argc - 1);\n" out)
    (display "    setenv(\"JSH_ARGC\", buf, 1);\n" out)
    (display "    for (int i = 1; i < argc; i++) {\n" out)
    (display "        snprintf(buf, sizeof(buf), \"JSH_ARG%d\", i - 1);\n" out)
    (display "        setenv(buf, argv[i], 1);\n" out)
    (display "    }\n\n" out)
    (display "    /* Resolve real exe path for $SHELL */\n" out)
    (display "    {\n" out)
    (display "        char exe_buf[4096];\n" out)
    (display "        ssize_t len = readlink(\"/proc/self/exe\", exe_buf, sizeof(exe_buf) - 1);\n" out)
    (display "        if (len > 0) { exe_buf[len] = '\\0'; setenv(\"JSH_EXE\", exe_buf, 1); }\n" out)
    (display "    }\n\n" out)
    (display "    /* Initialize Chez + register embedded boot files */\n" out)
    (display "    Sscheme_init(NULL);\n" out)
    (display "    static_boot_init();\n" out)
    (display "    Sbuild_heap(NULL, NULL);\n\n" out)
    (display "    /* Register FFI symbols after heap is built */\n" out)
    (display "    register_ffi_symbols();\n\n" out)
    (display "    /* Load program via memfd (threading workaround) */\n" out)
    (display "    int fd = memfd_create(\"jsh-program\", 1 /* MFD_CLOEXEC */);\n" out)
    (display "    if (fd < 0) { perror(\"memfd_create\"); return 1; }\n" out)
    (display "    if (write(fd, jsh_program_data, jsh_program_data_len) != (ssize_t)jsh_program_data_len) {\n" out)
    (display "        perror(\"write\"); close(fd); return 1;\n" out)
    (display "    }\n" out)
    (display "    char prog_path[64];\n" out)
    (display "    snprintf(prog_path, sizeof(prog_path), \"/proc/self/fd/%d\", fd);\n\n" out)
    (display "    const char *script_args[] = { argv[0] };\n" out)
    (display "    int status = Sscheme_script(prog_path, 1, script_args);\n\n" out)
    (display "    close(fd);\n" out)
    (display "    Sscheme_deinit();\n" out)
    (display "    return status;\n" out)
    (display "}\n" out))
  'replace)

;; ========== Step 6: Compile C with musl-gcc ==========

(printf "[6/7] Compiling C with musl-gcc...~n")

(define (run-cmd cmd)
  (printf "  ~a~n" cmd)
  (unless (= 0 (system cmd))
    (error 'build-jsh-musl "Command failed" cmd)))

;; Compile static_boot.c
(run-cmd (format "~a -c -O2 -I'~a' -o '~a/static_boot.o' '~a'"
                 gcc scheme-h-dir
                 build-dir static-boot-c))

;; Compile jsh_main_musl.c
(run-cmd (format "~a -c -O2 -I'~a' -o '~a/jsh_main_musl.o' '~a'"
                 gcc scheme-h-dir
                 build-dir program-c))

;; Compile ffi-shim.c with musl-gcc
(run-cmd (format "~a -c -O2 -o '~a/ffi-shim.o' ffi-shim.c -Wall"
                 gcc build-dir))

;; Compile landlock-shim.c from jerboa
(run-cmd (format "~a -c -O2 -o '~a/landlock-shim.o' '~a/support/landlock-shim.c' -Wall"
                 gcc build-dir jerboa-dir-base))

;; ========== Step 7: Link static binary ==========

(printf "[7/7] Linking static jsh-musl binary...~n")

(let ([link-cmd (musl-link-command
                  "jsh-musl"
                  (list (format "~a/jsh_main_musl.o" build-dir)
                        (format "~a/static_boot.o" build-dir)
                        (format "~a/ffi-shim.o" build-dir)
                        (format "~a/landlock-shim.o" build-dir))
                  '())])  ;; no extra static libs beyond Chez's
  (run-cmd link-cmd))

;; Cleanup
(system (format "rm -rf '~a'" build-dir))

;; Summary
(printf "~n========================================~n")
(printf "Static binary created: jsh-musl~n~n")
(system "ls -lh jsh-musl")
(printf "~n")
(system "file jsh-musl")
(printf "~nTest: ./jsh-musl -c 'echo Hello from static jsh'~n")
