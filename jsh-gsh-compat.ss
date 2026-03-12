#!chezscheme
;; Entry point for jerboa-shell
(import (chezscheme) (jsh main) (except (jsh builtins) list-head)
        (jsh registry) (jsh script)
        (only (compiler compile) gerbil-compile-top)
        (only (reader reader) gerbil-read))

;; Set tier and force builtin registration
(*jsh-tier* "small")
;; Touch a builtins export to force Chez to invoke (jsh builtins),
;; which runs the defbuiltin registration side effects.
(let () special-builtin? (void))

;; Helper: capture output to string (Chez native)
(define (output-to-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (get-output-string p)))

;; Read Gerbil forms using the Gerbil reader (handles [...], {...}, keywords)
(define (gerbil-read-all port)
  (let lp ((forms '()))
    (let ((datum (gerbil-read port)))
      (if (eof-object? datum)
        (reverse forms)
        (lp (cons datum forms))))))

(define (gerbil-read-all-from-string str)
  (gerbil-read-all (open-input-string str)))

;; Pre-load Gerbil runtime into eval environment on first use.
;; Also installs Gambit-compatible shims for primitives used by Gerbil code.
(define *gerbil-env-ready* #f)
(define (ensure-gerbil-env!)
  (unless *gerbil-env-ready*
    (set! *gerbil-env-ready* #t)
    (let ((env (interaction-environment)))
      ;; Import Jerboa runtime modules
      (for-each
        (lambda (lib) (eval `(import ,lib) env))
        '((jerboa runtime)
          (std error)
          (std misc string)
          (std misc list)
          (std os path)
          (std format)
          (std sort)
          (std pregexp)))
      ;; Gambit f64vector shims (Chez uses flvector)
      (eval '(define (make-f64vector n . rest)
               (if (null? rest) (make-flvector n)
                 (make-flvector n (car rest)))) env)
      (eval '(define f64vector-ref flvector-ref) env)
      (eval '(define f64vector-set! flvector-set!) env)
      ;; Gambit ##process-statistics shim
      ;; Returns an flvector compatible with Gambit layout:
      ;; [0]=user-cpu [2]=real-time [5]=gc-time
      (eval `(define (,(string->symbol "##process-statistics"))
               (let ((v (make-flvector 14 0.0)))
                 (flvector-set! v 0 (/ (cpu-time) 1000.0))
                 (flvector-set! v 2 (/ (real-time) 1000.0))
                 v)) env)
      ;; Gambit threading shims — import from (compat threading) for proper
      ;; SRFI-18 threads with lock-free current-thread and mailboxes
      (eval '(import (only (std misc thread)
               make-thread thread-start! thread-join!
               thread-yield! thread-sleep!
               current-thread thread-name thread?
               thread-send thread-receive)) env)
      ;; Gambit SMP primitives — Chez has real SMP with pthreads
      (eval `(define (,(string->symbol "##set-parallelism-level!") n) (void)) env)
      (eval `(define (,(string->symbol "##startup-parallelism!")) (void)) env)
      ;; Report real CPU count for SMP-aware code
      (eval `(define ,(string->symbol "##current-vm-processor-count")
               (let ((count ,(let ((c-sysconf (foreign-procedure "sysconf" (int) long)))
                               (let ((result (c-sysconf 84)))
                                 (if (> result 0) result 1)))))
                 (lambda () count))) env)
      ;; Gambit I/O shims
      (eval '(define (force-output . args)
               (flush-output-port
                 (if (null? args) (current-output-port) (car args)))) env))))

;; Compile and eval Gerbil forms in the interaction-environment.
;; Skips (export ...) and (import ...) forms (Gerbil std imports aren't
;; available as Chez libraries; runtime is pre-loaded by ensure-gerbil-env!).
(define (gerbil-eval-forms gerbil-forms)
  (ensure-gerbil-env!)
  (let ((env (interaction-environment)))
    (let loop ((fs gerbil-forms) (last (void)))
      (if (null? fs) last
        (let ((form (car fs)))
          (cond
            ;; Skip (export ...) — meaningless in interactive env
            ((and (pair? form) (eq? (car form) 'export))
             (loop (cdr fs) last))
            ;; Skip (import ...) — Gerbil std modules aren't Chez libraries;
            ;; runtime already loaded via ensure-gerbil-env!
            ((and (pair? form) (eq? (car form) 'import))
             (loop (cdr fs) last))
            (else
             (let ((chez-form (gerbil-compile-top form)))
               (loop (cdr fs) (eval chez-form env))))))))))

;; Format a result value for display
(define (format-result result)
  (cond
    [(eq? result (void)) ""]
    [(or (pair? result) (vector? result))
     (output-to-string (lambda (port) (pretty-print result port)))]
    [else
     (output-to-string (lambda (port) (write result port)))]))

;; ,use file.ss — read a Gerbil source file, compile through Gherkin, and eval
(define (handle-use-command path-str)
  (let ((path (string-trim-whitespace path-str)))
    (unless (file-exists? path)
      (error 'use (string-append "file not found: " path)))
    (let* ((port (open-input-file path))
           (forms (gerbil-read-all port)))
      (close-input-port port)
      (let ((result (gerbil-eval-forms forms)))
        (fprintf (current-error-port) "loaded: ~a (~a forms)~n"
                 path (length forms))
        result))))

(define (string-trim-whitespace s)
  (let* ((len (string-length s))
         (start (let lp ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (lp (+ i 1)) i)))
         (end (let lp ((i len))
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                  (lp (- i 1)) i))))
    (substring s start end)))

(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;; --- (room) — Common Lisp-style heap/GC introspection ---

(define (format-bytes n)
  ;; Format byte count as human-readable
  (cond
    [(>= n (* 1024 1024 1024))
     (format "~a GB" (exact->inexact (/ (round (* (/ n (* 1024 1024 1024)) 100)) 100)))]
    [(>= n (* 1024 1024))
     (format "~a MB" (exact->inexact (/ (round (* (/ n (* 1024 1024)) 100)) 100)))]
    [(>= n 1024)
     (format "~a KB" (exact->inexact (/ (round (* (/ n 1024) 10)) 10)))]
    [else (format "~a B" n)]))

(define (format-time t)
  ;; Format a Chez time object as seconds with ms precision
  (let ((secs (time-second t))
        (ns (time-nanosecond t)))
    (format "~a.~3,'0ds" secs (quotient ns 1000000))))

(define (room . args)
  (let ((verbose? (and (pair? args) (car args)))
        (port (current-output-port)))
    ;; Force a GC to get accurate numbers
    (collect)
    (let ((stats (statistics))
          (max-gen (collect-maximum-generation)))

      ;; Header
      (fprintf port "~n=== Room: Chez Scheme Heap Report ===~n~n")

      ;; Memory overview
      (fprintf port "--- Memory ---~n")
      (fprintf port "  Current heap:    ~12a  (~a)~n"
        (current-memory-bytes) (format-bytes (current-memory-bytes)))
      (fprintf port "  Maximum heap:    ~12a  (~a)~n"
        (maximum-memory-bytes) (format-bytes (maximum-memory-bytes)))
      (fprintf port "  Bytes allocated: ~12a  (~a)~n"
        (bytes-allocated) (format-bytes (bytes-allocated)))

      ;; GC info
      (fprintf port "~n--- Garbage Collector ---~n")
      (fprintf port "  Collections:     ~a~n" (collections))
      (fprintf port "  GC CPU time:     ~a~n" (format-time (sstats-gc-cpu stats)))
      (fprintf port "  GC real time:    ~a~n" (format-time (sstats-gc-real stats)))
      (fprintf port "  GC bytes freed:  ~a  (~a)~n"
        (sstats-gc-bytes stats) (format-bytes (sstats-gc-bytes stats)))
      (fprintf port "  Trip bytes:      ~a  (~a)~n"
        (collect-trip-bytes) (format-bytes (collect-trip-bytes)))
      (fprintf port "  Max generation:  ~a~n" max-gen)
      (fprintf port "  Gen radix:       ~a~n" (collect-generation-radix))

      ;; Per-generation breakdown
      (fprintf port "~n--- Generations ---~n")
      (do ((g 0 (+ g 1)))
          ((> g max-gen))
        (let ((gb (bytes-allocated g)))
          (fprintf port "  Gen ~a: ~12a  (~a)~n" g gb (format-bytes gb))))

      ;; Process times
      (fprintf port "~n--- Process ---~n")
      (fprintf port "  CPU time:        ~a~n" (format-time (sstats-cpu stats)))
      (fprintf port "  Real time:       ~a~n" (format-time (sstats-real stats)))

      ;; SMP info
      (let ((cpu-count (let ((c-sysconf (foreign-procedure "sysconf" (int) long)))
                         (let ((result (c-sysconf 84)))
                           (if (> result 0) result 1)))))
        (fprintf port "  CPU cores:       ~a~n" cpu-count))

      ;; Stack
      (fprintf port "~n--- Stack ---~n")
      (let ((oc (object-counts)))
        (for-each (lambda (entry)
          (when (eq? (car entry) 'stack)
            (for-each (lambda (gen-info)
              (fprintf port "  Gen ~a: count=~a  bytes=~a  (~a)~n"
                (car gen-info) (cadr gen-info) (cddr gen-info)
                (format-bytes (cddr gen-info))))
              (cdr entry))))
          oc))

      ;; Object type breakdown (verbose or top types)
      (let ((oc (object-counts)))
        ;; Aggregate per type
        (let ((summary
                (filter (lambda (s) (> (caddr s) 0))
                  (map (lambda (entry)
                    (let ((type (car entry))
                          (gens (cdr entry)))
                      (let loop ((gs gens) (tc 0) (tb 0))
                        (if (null? gs) (list type tc tb)
                          (loop (cdr gs) (+ tc (cadr (car gs))) (+ tb (cddr (car gs))))))))
                    oc))))
          (let ((sorted (list-sort (lambda (a b) (> (caddr a) (caddr b))) summary)))
            (if verbose?
              ;; Full listing
              (begin
                (fprintf port "~n--- All Object Types ---~n")
                (fprintf port "  ~30a ~10a ~12a~n" "Type" "Count" "Bytes")
                (fprintf port "  ~30a ~10a ~12a~n" "----" "-----" "-----")
                (for-each (lambda (s)
                  (fprintf port "  ~30a ~10a ~12a  (~a)~n"
                    (car s) (cadr s) (caddr s) (format-bytes (caddr s))))
                  sorted))
              ;; Top 15 types
              (begin
                (fprintf port "~n--- Top Object Types ---~n")
                (fprintf port "  ~30a ~10a ~12a~n" "Type" "Count" "Bytes")
                (fprintf port "  ~30a ~10a ~12a~n" "----" "-----" "-----")
                (let loop ((rest sorted) (n 0))
                  (when (and (pair? rest) (< n 15))
                    (let ((s (car rest)))
                      (fprintf port "  ~30a ~10a ~12a  (~a)~n"
                        (car s) (cadr s) (caddr s) (format-bytes (caddr s))))
                    (loop (cdr rest) (+ n 1)))))))))

      (fprintf port "~n")
      (void))))

;; Install Gerbil eval handler with ,use / ,room / ,exports meta-commands
(*meta-command-handler*
  (lambda (expr-str)
    (guard (exn
             [#t (cons
                   (output-to-string
                     (lambda (port)
                       (display "Gerbil error: " port)
                       (display-condition exn port)))
                   1)])
      (cond
        ;; ,use file.ss — compile and load a Gerbil source file
        [(string-prefix? "use " expr-str)
         (let ((result (handle-use-command
                         (substring expr-str 4 (string-length expr-str)))))
           (cons (format-result result) 0))]
        ;; ,room — Common Lisp-style heap/GC report
        [(or (string=? "room" expr-str) (string=? "room " expr-str))
         (room)
         (cons "" 0)]
        ;; ,room #t — verbose report with all object types
        [(string-prefix? "room " expr-str)
         (room #t)
         (cons "" 0)]
        ;; Normal Gerbil eval
        [else
         (let* ((gerbil-forms (gerbil-read-all-from-string expr-str))
                (result (gerbil-eval-forms gerbil-forms)))
           (cons (format-result result) 0))]))))

;; Get args from GSH_ARGC/GSH_ARGn env vars (set by jsh-main.c)
;; or fall back to (command-line) for interpreted mode.
(define (get-real-args)
  (let ((argc-str (getenv "GSH_ARGC")))
    (if argc-str
      ;; Binary mode: custom main saved args in env vars
      (let ((argc (string->number argc-str)))
        (let loop ((i 0) (acc '()))
          (if (>= i argc)
            (reverse acc)
            (let ((val (getenv (format "GSH_ARG~a" i))))
              (loop (+ i 1) (cons (or val "") acc))))))
      ;; Interpreted mode (--program): use (command-line)
      (let ((cmdline (command-line)))
        (if (pair? cmdline) (cdr cmdline) '())))))

(apply main (get-real-args))
