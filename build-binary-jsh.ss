#!chezscheme
;; Build a native jsh binary for jerboa-shell.
;;
;; Usage: cd jerboa-shell && make binary
;;
;; Produces: ./jsh (single ELF binary with embedded boot files + program)
;;
;; All boot files (petite.boot, scheme.boot, jsh.boot) are embedded as C byte
;; arrays via Sregister_boot_file_bytes — the binary is fully self-contained
;; and works from any directory.
;;
;; Threading workaround: Programs embedded in boot files (via make-boot-file)
;; cannot create threads — fork-thread threads block on an internal GC futex.
;; To fix this, the boot file contains only libraries. The program is compiled
;; separately, embedded as a C byte array, and loaded at runtime via
;; Sscheme_script on a memfd.

(import (chezscheme))

;; --- Helper: generate C header from binary file ---
(define (file->c-header input-path output-path array-name size-name)
  (let* ((port (open-file-input-port input-path))
         (data (get-bytevector-all port))
         (size (bytevector-length data)))
    (close-port port)
    (call-with-output-file output-path
      (lambda (out)
        (fprintf out "/* Auto-generated — do not edit */~n")
        (fprintf out "static const unsigned char ~a[] = {~n" array-name)
        (let loop ((i 0))
          (when (< i size)
            (when (= 0 (modulo i 16)) (fprintf out "  "))
            (fprintf out "0x~2,'0x" (bytevector-u8-ref data i))
            (when (< (+ i 1) size) (fprintf out ","))
            (when (= 15 (modulo i 16)) (fprintf out "~n"))
            (loop (+ i 1))))
        (fprintf out "~n};~n")
        (fprintf out "static const unsigned int ~a = ~a;~n" size-name size))
      'replace)
    (printf "  ~a: ~a bytes~n" output-path size)))

;; --- Locate Chez install directory ---
(define chez-dir
  (or (getenv "CHEZ_DIR")
      (let* ((mt (symbol->string (machine-type)))
             (home (getenv "HOME"))
             (lib-dir (format "~a/.local/lib" home))
             (csv-dir
               (let lp ((dirs (guard (e (#t '())) (directory-list lib-dir))))
                 (cond
                   ((null? dirs) #f)
                   ((and (> (string-length (car dirs)) 3)
                         (string=? "csv" (substring (car dirs) 0 3)))
                    (format "~a/~a/~a" lib-dir (car dirs) mt))
                   (else (lp (cdr dirs)))))))
        (and csv-dir
             (file-exists? (format "~a/main.o" csv-dir))
             csv-dir))))

(unless chez-dir
  (display "Error: Cannot find Chez install dir. Set CHEZ_DIR.\n")
  (exit 1))

;; --- Locate jerboa library directory ---
(define jerboa-dir
  (or (getenv "JERBOA_DIR")
      (format "~a/mine/jerboa/lib" (getenv "HOME"))))

(unless (file-exists? (format "~a/jerboa/runtime.so" jerboa-dir))
  (printf "Error: Cannot find jerboa runtime at ~a~n" jerboa-dir)
  (printf "Set JERBOA_DIR or build jerboa.~n")
  (exit 1))

;; --- Locate gherkin runtime (needed for MOP + compiler) ---
(define gherkin-dir
  (or (getenv "GHERKIN_DIR")
      (format "~a/mine/gherkin/src" (getenv "HOME"))))

(unless (file-exists? (format "~a/compat/types.so" gherkin-dir))
  (printf "Error: Cannot find gherkin runtime at ~a~n" gherkin-dir)
  (printf "Set GHERKIN_DIR or rebuild gherkin.~n")
  (exit 1))

(printf "Chez dir:    ~a~n" chez-dir)
(printf "Jerboa dir:  ~a~n" jerboa-dir)
(printf "Gherkin dir: ~a~n" gherkin-dir)

;; --- Step 1: Compile all modules + entry point ---
(printf "~n[1/7] Compiling all modules (optimize-level 3, tuned cp0, WPO)...~n")
(parameterize ([compile-imported-libraries #t]
               [optimize-level 3]
               [cp0-effort-limit 500]
               [cp0-score-limit 50]
               [cp0-outer-unroll-limit 1]
               [commonization-level 4]
               [enable-unsafe-application #t]
               [enable-unsafe-variable-reference #t]
               [enable-arithmetic-left-associative #t]
               [debug-level 0]
               [generate-inspector-information #f]
               [generate-wpo-files #t])
  (compile-program "jsh.ss"))

;; --- Step 2: Whole-program optimization ---
(printf "[2/7] Running whole-program optimization...~n")
(let ((missing (compile-whole-program "jsh.wpo" "jsh-all.so")))
  (unless (null? missing)
    (printf "  WPO: ~a libraries not incorporated (missing .wpo):~n" (length missing))
    (for-each (lambda (lib) (printf "    ~a~n" lib)) missing)))

;; --- Step 3: Make libs-only boot file ---
;; NOTE: The program is NOT included in the boot file. Programs in boot files
;; cannot use fork-thread (Chez Scheme bug — threads block on internal GC futex).
;; The program is loaded separately via Sscheme_script at runtime.
(printf "[3/7] Creating libs-only boot file...~n")
(apply make-boot-file "jsh.boot" '("scheme" "petite")
  (append
    ;; Jerboa runtime + stdlib (dependency order: core first, then modules)
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
        "std/os/path"
        "std/os/signal"
        "std/os/fdio"
        "std/transducer"
        "std/log"
        "std/capability"
        "std/capability/sandbox"))
    ;; Gherkin runtime (MOP chain — needed by compiled Gerbil code)
    (map (lambda (m) (format "~a/~a.so" gherkin-dir m))
      '("compat/types"
        "compat/gambit-compat"
        "runtime/util"
        "runtime/table"
        "runtime/c3"
        "runtime/mop"
        ;; Gherkin compiler + reader (for Gerbil eval in shell)
        "runtime/error"
        "runtime/hash"
        "runtime/syntax"
        "runtime/eval"
        "reader/reader"
        "compiler/compile"
        "boot/gherkin"))
    ;; Local compat layer
    (list "src/compat/gambit.so")
    ;; GSH modules (dependency order)
    (map (lambda (m) (format "src/jsh/~a.so" m))
      '("ffi" "pregexp-compat" "stage" "static-compat"
        "ast" "registry" "macros" "util"
        "environment" "lexer" "arithmetic" "glob" "fuzzy" "history"
        "parser" "functions" "signals" "expander"
        "redirect" "control" "jobs" "builtins"
        "pipeline" "executor" "completion" "prompt"
        "lineedit" "fzf" "script" "startup" "sandbox" "main"))))

;; --- Step 4: Generate C headers with embedded data ---
(printf "[4/7] Embedding boot files + program as C headers...~n")
(file->c-header "jsh-all.so" "jsh_program.h"
                "jsh_program_data" "jsh_program_size")
(file->c-header (format "~a/petite.boot" chez-dir) "jsh_petite_boot.h"
                "petite_boot_data" "petite_boot_size")
(file->c-header (format "~a/scheme.boot" chez-dir) "jsh_scheme_boot.h"
                "scheme_boot_data" "scheme_boot_size")
(file->c-header "jsh.boot" "jsh_jsh_boot.h"
                "jsh_boot_data" "jsh_boot_size")

;; --- Step 5: Compile C sources ---
(printf "[5/7] Compiling C sources...~n")
;; FFI shim
(unless (= 0 (system "gcc -c -fPIC -O2 -o ffi-shim.o ffi-shim.c -Wall -Wextra 2>&1"))
  (display "Error: FFI shim compilation failed\n")
  (exit 1))
;; Custom main (loads libs from boot, program from embedded memfd)
(let ((cmd (format "gcc -c -O2 -o jsh-main.o jsh-main.c -I~a -I. -Wall 2>&1" chez-dir)))
  (unless (= 0 (system cmd))
    (display "Error: jsh-main.c compilation failed\n")
    (exit 1)))

;; --- Step 6: Link native binary ---
(printf "[6/7] Linking native binary...~n")
(let ((cmd (format "gcc -rdynamic -o jsh jsh-main.o ffi-shim.o -L~a -lkernel -llz4 -lz -lm -ldl -lpthread -luuid -lncurses -Wl,-rpath,~a"
             chez-dir chez-dir)))
  (printf "  ~a~n" cmd)
  (unless (= 0 (system cmd))
    (display "Error: Link failed\n")
    (exit 1)))

;; --- Step 7: Clean up intermediate files ---
(printf "[7/7] Cleaning up...~n")
(for-each (lambda (f)
            (when (file-exists? f) (delete-file f)))
  '("jsh-main.o" "ffi-shim.o" "jsh_program.h"
    "jsh_petite_boot.h" "jsh_scheme_boot.h" "jsh_jsh_boot.h"
    "jsh-all.so" "jsh.so" "jsh.wpo" "jsh.boot"))

(printf "~n========================================~n")
(printf "Build complete!~n~n")
(printf "  Binary: ./jsh  (self-contained ELF, ~a KB)~n"
  (quotient (file-length (open-file-input-port "jsh")) 1024))
(printf "~nRun:~n")
(printf "  ./jsh              # interactive shell~n")
(printf "  ./jsh -c 'echo hi' # run command~n")
(printf "  cp jsh /tmp/ && /tmp/jsh -c 'echo works from anywhere'~n")
