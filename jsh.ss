#!chezscheme
;; Entry point for jerboa-shell
(import (chezscheme) (jsh main) (except (jsh builtins) list-head)
        (jsh registry) (jsh script) (jsh sandbox)
        (only (jsh executor) *jsh-profile-mode* profile-reset! profile-get-data
              ast->command-text)
        (only (jsh parser) parse-complete-command)
        (only (jsh ast) ast-pipeline? ast-pipeline-commands ast-pipeline-bang?)
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
          (std pregexp)
          (except (std capability) with-sandbox)
          (except (std capability sandbox) with-sandbox)))
      ;; Shell helpers: place run-cmd / run-script in the interaction env by name
      ;; using define-top-level-value (bypasses WPO and program-namespace isolation).
      ;; Using mangled names avoids Gherkin seeing them as macro candidates.
      (define-top-level-value '|jsh:run-cmd|    run-cmd    (interaction-environment))
      (define-top-level-value '|jsh:run-script| run-script (interaction-environment))
      ;; Now inject into Gerbil env using a plain-symbol alias (no raw proc values)
      (eval '(define run-cmd    |jsh:run-cmd|)    env)
      (eval '(define run-script |jsh:run-script|) env)
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
                 (if (null? args) (current-output-port) (car args)))) env)
      (void))))

;; Handle (with-sandbox kw: val ... body) intercepted before Gerbil compilation.
;; Parses opts from the Gerbil AST, compiles body as a thunk, calls jsh-sandbox-run.
(define (handle-with-sandbox-form form)
  (ensure-gerbil-env!)
  ;; form is (with-sandbox kw: v1 kw2: v2 ... body)
  ;; all args except last are keyword/value pairs; last is the body
  (let* ((args (cdr form))
         (n    (length args)))
    (when (< n 1)
      (error 'with-sandbox "expected at least a body expression"))
    (let* ((opts-flat (list-head args (- n 1)))
           (body      (list-ref  args (- n 1)))
           ;; Build opts as list of (kw val) pairs
           (opts (let loop ((ls opts-flat))
                   (if (or (null? ls) (null? (cdr ls)))
                     '()
                     (cons (list (car ls) (cadr ls))
                           (loop (cddr ls))))))
           ;; Compile body as a Gerbil thunk
           (thunk (let ((env (interaction-environment)))
                    (eval (gerbil-compile-top `(lambda () ,body)) env))))
      (jsh-sandbox-run opts thunk))))

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

;; Split a string into whitespace-delimited tokens
(define (simple-tokenize str)
  (let lp ((i 0) (start 0) (tokens '()))
    (cond
      ((= i (string-length str))
       (if (> i start)
         (reverse (cons (substring str start i) tokens))
         (reverse tokens)))
      ((char-whitespace? (string-ref str i))
       (if (> i start)
         (lp (+ i 1) (+ i 1) (cons (substring str start i) tokens))
         (lp (+ i 1) (+ i 1) tokens)))
      (else
       (lp (+ i 1) start tokens)))))

;; Join a list of strings with separator
(define (simple-join lst sep)
  (if (null? lst) ""
    (let loop ((rest (cdr lst)) (acc (car lst)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

;; --- Profile report printer ---
(define (print-profile-report data source-name)
  (let ((port (current-output-port)))
    (fprintf port "~n=== jsh profile: ~a ===~n" source-name)
    (if (null? data)
      (fprintf port "  (no commands recorded)~n")
      (let* ((total   (apply + (map car data)))
             (n       (length data))
             (sorted  (list-sort (lambda (a b) (> (car a) (car b))) data))
             (slowest (car sorted)))
        ;; Per-command listing (in execution order)
        (for-each
          (lambda (entry)
            (let* ((elapsed-ms (car entry))
                   (text       (cadr entry))
                   (secs       (quotient  elapsed-ms 1000))
                   (frac       (remainder elapsed-ms 1000))
                   (display-text (if (> (string-length text) 60)
                                   (string-append (substring text 0 57) "...")
                                   text)))
              (fprintf port "  ~a.~3,'0ds  ~a~n" secs frac display-text)))
          data)
        ;; Summary
        (let* ((total-secs (quotient  total 1000))
               (total-frac (remainder total 1000))
               (slow-secs  (quotient  (car slowest) 1000))
               (slow-frac  (remainder (car slowest) 1000))
               (slow-pct   (if (> total 0)
                             (inexact (round (* 100 (/ (car slowest) total))))
                             0)))
          (fprintf port "~n--- Summary ---~n")
          (fprintf port "  Total:    ~a.~3,'0ds~n" total-secs total-frac)
          (fprintf port "  Commands: ~a~n" n)
          (fprintf port "  Slowest:  ~a (~a.~3,'0ds, ~a%)~n"
                   (cadr slowest) slow-secs slow-frac slow-pct))))
    (fprintf port "~n")))

;; --- Trace report printer ---
;; ast: parsed AST or #f; start/end-ms: real-time in ms
(define (print-trace-report cmd-str ast start-ms end-ms status)
  (let ((port (current-output-port))
        (elapsed (- end-ms start-ms)))
    (fprintf port "~n=== Trace: ~a ===~n" cmd-str)
    (if (and ast (ast-pipeline? ast))
      (let* ((commands (ast-pipeline-commands ast))
             (n        (length commands))
             (bang?    (ast-pipeline-bang? ast)))
        (fprintf port "Pipeline: ~a stage~a~a~n"
                 n (if (= n 1) "" "s") (if bang? " (negated)" ""))
        (let loop ((cmds commands) (i 1))
          (unless (null? cmds)
            (let ((text (guard (e (#t "?")) (ast->command-text (car cmds)))))
              (cond
                ((= n 1)
                 (fprintf port "  [~a] ~a~n" i text))
                ((= i 1)
                 (fprintf port "  [~a] ~a  → stdout→pipe~a~n" i text i))
                ((= i n)
                 (fprintf port "  [~a] ~a  ← pipe~a → stdout~n" i text (- i 1)))
                (else
                 (fprintf port "  [~a] ~a  ← pipe~a → stdout→pipe~a~n"
                          i text (- i 1) i))))
            (loop (cdr cmds) (+ i 1)))))
      (fprintf port "  ~a~n" cmd-str))
    (fprintf port "~nStatus: ~a~n" status)
    (fprintf port "Wall time: ~a.~3,'0ds~n~n"
             (quotient elapsed 1000) (remainder elapsed 1000))))

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
        ;; ,trace — pipeline structure visualization with timing
        ;; Usage: ,trace [-c cmd | script.sh]
        [(or (string=? expr-str "trace")
             (string-prefix? "trace " expr-str))
         (let* ((rest (if (string=? expr-str "trace") ""
                          (substring expr-str 6 (string-length expr-str))))
                (args (simple-tokenize rest)))
           (cond
             ((or (null? args) (member "--help" args))
              (display "Usage: ,trace [-c cmd | script.sh]\n")
              (cons "" 0))
             ((string=? (car args) "-c")
              (if (pair? (cdr args))
                (let* ((cmd-str (simple-join (cdr args) " "))
                       (ast     (guard (e (#t #f))
                                  (parse-complete-command cmd-str #f #f)))
                       (start   (real-time))
                       (status  (run-cmd cmd-str))
                       (end     (real-time)))
                  (print-trace-report cmd-str ast start end status)
                  (cons "" (if (integer? status) status 1)))
                (begin (display "jsh: ,trace -c: missing command\n")
                       (cons "" 1))))
             (else
              (let* ((script  (car args))
                     (start   (real-time))
                     (status  (run-script script))
                     (end     (real-time)))
                ;; Scripts contain multiple commands; no single-AST analysis
                (print-trace-report script #f start end status)
                (cons "" (if (integer? status) status 1))))))]
        ;; ,profile — per-command timing for scripts and inline commands
        ;; Usage: ,profile [-c cmd | script.sh]
        [(or (string=? expr-str "profile")
             (string-prefix? "profile " expr-str))
         (let* ((rest (if (string=? expr-str "profile") ""
                          (substring expr-str 8 (string-length expr-str))))
                (args (simple-tokenize rest)))
           (cond
             ((or (null? args) (member "--help" args))
              (display "Usage: ,profile [-c cmd | script.sh]\n")
              (cons "" 0))
             ((string=? (car args) "-c")
              (if (pair? (cdr args))
                (let* ((cmd-str (simple-join (cdr args) " ")))
                  (profile-reset!)
                  (let ((status (parameterize ((*jsh-profile-mode* #t))
                                  (run-cmd cmd-str))))
                    (print-profile-report (profile-get-data)
                                          (string-append "-c " cmd-str))
                    (cons "" (if (integer? status) status 1))))
                (begin (display "jsh: ,profile -c: missing command\n")
                       (cons "" 1))))
             (else
              (let ((script (car args)))
                (profile-reset!)
                (let ((status (parameterize ((*jsh-profile-mode* #t))
                                (run-script script))))
                  (print-profile-report (profile-get-data) script)
                  (cons "" (if (integer? status) status 1)))))))]
        ;; ,sb — sandboxed script/command execution
        ;; Usage: ,sb [options] [-c cmd | script.sh]
        ;; Options: -r path, -w path, -x cmd, --net, --no-net, -t ms
        [(or (string=? expr-str "sb")
             (string-prefix? "sb " expr-str))
         (let* ((args-str (if (string=? expr-str "sb") ""
                              (substring expr-str 3 (string-length expr-str))))
                (args (simple-tokenize args-str)))
           (if (or (null? args) (member "--help" args))
             (begin
               (display "Usage: ,sb [options] [-c cmd | script.sh]\n")
               (display "Options:\n")
               (display "  -r path    allow reading path\n")
               (display "  -w path    allow writing path\n")
               (display "  -x cmd     allow executing command\n")
               (display "  --net      allow network access\n")
               (display "  --no-net   deny network (default)\n")
               (display "  -t ms      timeout in milliseconds\n")
               (display "  -c cmd     run inline shell command\n")
               (cons "" 0))
             (let* ((parsed  (parse-sb-args args))
                    (script  (sb-parsed-script parsed))
                    (cmd-str (sb-parsed-cmd    parsed))
                    (opts    (sb-parsed-opts   parsed)))
               (cond
                 (cmd-str
                  (let* ((thunk  (lambda () (run-cmd cmd-str)))
                         (status (jsh-sandbox-run opts thunk)))
                    (cons "" (if (integer? status) status 1))))
                 (script
                  (let* ((thunk  (lambda () (run-script script)))
                         (status (jsh-sandbox-run opts thunk)))
                    (cons "" (if (integer? status) status 1))))
                 (else
                  (display "jsh: ,sb: no script or command specified\n")
                  (cons "" 1))))))]
        ;; Normal Gerbil eval (with special handling for with-sandbox)
        [else
         (let* ((gerbil-forms (gerbil-read-all-from-string expr-str))
                (result
                  ;; Intercept top-level (with-sandbox ...) before Gerbil compilation
                  ;; so we can call jsh-sandbox-run directly with a compiled thunk.
                  (if (and (= 1 (length gerbil-forms))
                           (pair? (car gerbil-forms))
                           (eq? (caar gerbil-forms) 'with-sandbox))
                    (handle-with-sandbox-form (car gerbil-forms))
                    (gerbil-eval-forms gerbil-forms))))
           (cons (format-result result) 0))]))))

;; Get args from JSH_ARGC/JSH_ARGn env vars (set by jsh-main.c)
;; or fall back to (command-line) for interpreted mode.
(define (get-real-args)
  (let ((argc-str (getenv "JSH_ARGC")))
    (if argc-str
      ;; Binary mode: custom main saved args in env vars
      (let ((argc (string->number argc-str)))
        (let loop ((i 0) (acc '()))
          (if (>= i argc)
            (reverse acc)
            (let ((val (getenv (format "JSH_ARG~a" i))))
              (loop (+ i 1) (cons (or val "") acc))))))
      ;; Interpreted mode (--program): use (command-line)
      (let ((cmdline (command-line)))
        (if (pair? cmdline) (cdr cmdline) '())))))

(apply main (get-real-args))
