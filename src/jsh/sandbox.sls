#!chezscheme
;;; (jsh sandbox) — Shell sandbox integration
;;;
;;; Wires the ,sb meta-command to jerboa's (std os sandbox) which provides
;;; real Landlock enforcement. Forks a child, applies kernel restrictions,
;;; runs the command, parent is never affected.

(library (jsh sandbox)
  (export *current-jsh-env* run-script run-cmd jsh-sandbox-run
          parse-sb-args sb-parsed-script sb-parsed-cmd sb-parsed-opts)

  (import (chezscheme) (jsh script) (std os sandbox))

  ;; Current shell environment, set by (jsh main) at startup
  (define *current-jsh-env* (make-parameter #f))

  ;; Run a shell script file inside the current jsh session (unsandboxed)
  (define (run-script filename . args)
    (let ((env (*current-jsh-env*)))
      (if env
        (execute-script filename args env)
        (error 'run-script "shell not initialized"))))

  ;; Run an inline shell command string inside the current jsh session (unsandboxed)
  (define (run-cmd cmd)
    (let ((env (*current-jsh-env*)))
      (if env
        (execute-string cmd env)
        (error 'run-cmd "shell not initialized"))))

  ;; Extract a named opt from the parsed opts list
  (define (opt-ref opts name)
    (let loop ((opts opts))
      (cond
        ((null? opts) #f)
        ((and (pair? (car opts)) (string=? (car (car opts)) name))
         (cadr (car opts)))
        (else (loop (cdr opts))))))

  ;; Main sandbox entry point.
  ;; Delegates to (std os sandbox) for real Landlock enforcement.
  (define (jsh-sandbox-run opts thunk)
    (let ((read-paths  (or (opt-ref opts "allow-read") '()))
          (write-paths (or (opt-ref opts "allow-write") '()))
          (exec-paths  (or (opt-ref opts "allow-exec") '())))
      (sandbox-run read-paths write-paths exec-paths thunk)))

  ;; --- ,sb argument parser ---

  (define (make-sb-parsed script inline-cmd opts)
    (list 'sb-parsed script inline-cmd opts))
  (define (sb-parsed-script p) (cadr p))
  (define (sb-parsed-cmd    p) (caddr p))
  (define (sb-parsed-opts   p) (cadddr p))

  (define (sb-string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))

  (define (sb-string-join lst sep)
    (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  (define (build-sb-opts allow-read allow-write allow-exec net no-net timeout)
    (append
      (if (pair? allow-read)  (list (list "allow-read"  (reverse allow-read)))  '())
      (if (pair? allow-write) (list (list "allow-write" (reverse allow-write))) '())
      (if (pair? allow-exec)  (list (list "allow-exec"  (reverse allow-exec)))  '())
      (if net    (list (list "allow-net" #t)) '())
      (if no-net (list (list "deny-net"  #t)) '())
      (if timeout (list (list "timeout-ms" timeout)) '())))

  (define (parse-sb-args arg-list)
    (let loop ((args      arg-list)
               (allow-read  '())
               (allow-write '())
               (allow-exec  '())
               (net    #f)
               (no-net #f)
               (timeout #f)
               (inline-cmd #f))
      (cond
        ((null? args)
         (make-sb-parsed #f inline-cmd
           (build-sb-opts allow-read allow-write allow-exec net no-net timeout)))
        ((string=? (car args) "-r")
         (if (pair? (cdr args))
           (loop (cddr args) (cons (cadr args) allow-read)
                 allow-write allow-exec net no-net timeout inline-cmd)
           (error 'parse-sb-args "missing path after -r")))
        ((string=? (car args) "-w")
         (if (pair? (cdr args))
           (loop (cddr args) allow-read (cons (cadr args) allow-write)
                 allow-exec net no-net timeout inline-cmd)
           (error 'parse-sb-args "missing path after -w")))
        ((string=? (car args) "-x")
         (if (pair? (cdr args))
           (loop (cddr args) allow-read allow-write (cons (cadr args) allow-exec)
                 net no-net timeout inline-cmd)
           (error 'parse-sb-args "missing command after -x")))
        ((string=? (car args) "--net")
         (loop (cdr args) allow-read allow-write allow-exec #t #f timeout inline-cmd))
        ((string=? (car args) "--no-net")
         (loop (cdr args) allow-read allow-write allow-exec #f #t timeout inline-cmd))
        ((string=? (car args) "-t")
         (if (and (pair? (cdr args)) (string->number (cadr args)))
           (loop (cddr args) allow-read allow-write allow-exec net no-net
                 (string->number (cadr args)) inline-cmd)
           (error 'parse-sb-args "missing or invalid timeout after -t")))
        ((string=? (car args) "-c")
         (if (pair? (cdr args))
           (make-sb-parsed #f (sb-string-join (cdr args) " ")
             (build-sb-opts allow-read allow-write allow-exec net no-net timeout))
           (error 'parse-sb-args "missing command after -c")))
        ((not (sb-string-prefix? "-" (car args)))
         (make-sb-parsed (car args) inline-cmd
           (build-sb-opts allow-read allow-write allow-exec net no-net timeout)))
        (else
         (loop (cdr args) allow-read allow-write allow-exec net no-net timeout inline-cmd)))))

) ;; end library
