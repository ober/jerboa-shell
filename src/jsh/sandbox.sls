#!chezscheme
;;; (jsh sandbox) — Shell sandbox integration
;;;
;;; Provides run-script, run-cmd, and jsh-sandbox-run for use from
;;; the interactive , eval environment. The *current-jsh-env*
;;; parameter is set by (jsh main) after creating the shell environment.

(library (jsh sandbox)
  (export *current-jsh-env* run-script run-cmd jsh-sandbox-run
          parse-sb-args sb-parsed-script sb-parsed-cmd sb-parsed-opts)

  (import (chezscheme)
          (except (std capability sandbox) with-sandbox)
          (jsh script))

  ;; Current shell environment, set by (jsh main) at startup
  (define *current-jsh-env* (make-parameter #f))

  ;; Run a shell script file inside the current jsh session
  (define (run-script filename . args)
    (let ((env (*current-jsh-env*)))
      (if env
        (execute-script filename args env)
        (error 'run-script "shell not initialized — *current-jsh-env* is #f"))))

  ;; Run an inline shell command string inside the current jsh session
  (define (run-cmd cmd)
    (let ((env (*current-jsh-env*)))
      (if env
        (execute-string cmd env)
        (error 'run-cmd "shell not initialized — *current-jsh-env* is #f"))))

  ;; Convert any keyword-like value to its base string name.
  ;; Handles Gerbil #:name keywords, Chez name: keywords, and plain symbols.
  (define (kw->name k)
    (let ((s (format "~a" k)))
      (cond
        ;; Gerbil keyword: #:name
        ((and (>= (string-length s) 2)
              (string=? (substring s 0 2) "#:"))
         (substring s 2 (string-length s)))
        ;; Chez trailing-colon keyword: name:
        ((and (> (string-length s) 0)
              (char=? #\: (string-ref s (- (string-length s) 1))))
         (substring s 0 (- (string-length s) 1)))
        (else s))))

  ;; Build a sandbox policy from the opts list.
  ;; opts is a list of (kw-value val) pairs, e.g.:
  ;;   (list (list #:allow-read '("/tmp")) (list #:deny-net #t))
  (define (build-policy opts)
    (let ((p (make-sandbox-policy)))
      (policy-allow! p 'arithmetic)
      (policy-allow! p 'string-ops)
      (for-each
        (lambda (pair)
          (let ((name (kw->name (car pair)))
                (val  (cadr pair)))
            (cond
              ((string=? name "allow-read")
               (policy-allow! p 'filesystem))
              ((string=? name "allow-write")
               (policy-allow! p 'filesystem))
              ((string=? name "allow-exec")
               (policy-allow! p 'exec))
              ((and (string=? name "deny-net") val)
               (policy-deny! p 'network))
              ((and (string=? name "allow-net") val)
               (policy-allow! p 'network))
              ;; timeout-ms handled separately
              )))
        opts)
      p))

  ;; Extract timeout-ms from opts, or return #f
  (define (extract-timeout opts)
    (let loop ((opts opts))
      (if (null? opts) #f
        (let ((name (kw->name (caar opts)))
              (val  (cadar opts)))
          (if (string=? name "timeout-ms")
            val
            (loop (cdr opts)))))))

  ;; Main sandbox execution entry point.
  ;; Called by the with-sandbox macro:
  ;;   (jsh-sandbox-run '((kw val) ...) thunk)
  (define (jsh-sandbox-run opts thunk)
    (let ((policy     (build-policy opts))
          (timeout-ms (extract-timeout opts)))
      (let ((result (if timeout-ms
                      (sandbox-run/timeout policy thunk timeout-ms)
                      (sandbox-run policy thunk))))
        (when (condition? result)
          (display "sandbox error: ")
          (display-condition result (current-error-port))
          (newline (current-error-port)))
        result)))

  ;; --- ,sb meta-command argument parser ---

  ;; Parsed result record (as tagged list)
  (define (make-sb-parsed script inline-cmd opts)
    (list 'sb-parsed script inline-cmd opts))
  (define (sb-parsed-script p) (cadr p))
  (define (sb-parsed-cmd    p) (caddr p))
  (define (sb-parsed-opts   p) (cadddr p))

  ;; Local string helpers (avoid extra imports)
  (define (sb-string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))

  (define (sb-string-join lst sep)
    (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  ;; Build opts list from parsed flags.
  ;; Returns list of (key val) pairs compatible with build-policy / extract-timeout.
  (define (build-sb-opts allow-read allow-write allow-exec net no-net timeout)
    (append
      (if (pair? allow-read)  (list (list "allow-read"  (reverse allow-read)))  '())
      (if (pair? allow-write) (list (list "allow-write" (reverse allow-write))) '())
      (if (pair? allow-exec)  (list (list "allow-exec"  (reverse allow-exec)))  '())
      (if net    (list (list "allow-net" #t)) '())
      (if no-net (list (list "deny-net"  #t)) '())
      (if timeout (list (list "timeout-ms" timeout)) '())))

  ;; Parse CLI-style flag list for the ,sb meta-command.
  ;; arg-list: list of string tokens (already split on whitespace)
  ;; Returns (make-sb-parsed script inline-cmd opts)
  ;;   script: file path string, or #f if inline command
  ;;   inline-cmd: command string from -c flag, or #f
  ;;   opts: list of (key val) pairs for build-policy / extract-timeout
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
        ;; No more args
        ((null? args)
         (make-sb-parsed #f inline-cmd
           (build-sb-opts allow-read allow-write allow-exec net no-net timeout)))
        ;; -r path: allow reading path
        ((string=? (car args) "-r")
         (if (pair? (cdr args))
           (loop (cddr args) (cons (cadr args) allow-read)
                 allow-write allow-exec net no-net timeout inline-cmd)
           (error 'parse-sb-args "missing path after -r")))
        ;; -w path: allow writing path
        ((string=? (car args) "-w")
         (if (pair? (cdr args))
           (loop (cddr args) allow-read (cons (cadr args) allow-write)
                 allow-exec net no-net timeout inline-cmd)
           (error 'parse-sb-args "missing path after -w")))
        ;; -x cmd: allow executing command
        ((string=? (car args) "-x")
         (if (pair? (cdr args))
           (loop (cddr args) allow-read allow-write (cons (cadr args) allow-exec)
                 net no-net timeout inline-cmd)
           (error 'parse-sb-args "missing command after -x")))
        ;; --net: allow network access
        ((string=? (car args) "--net")
         (loop (cdr args) allow-read allow-write allow-exec #t #f timeout inline-cmd))
        ;; --no-net: deny network access
        ((string=? (car args) "--no-net")
         (loop (cdr args) allow-read allow-write allow-exec #f #t timeout inline-cmd))
        ;; -t ms: set timeout in milliseconds
        ((string=? (car args) "-t")
         (if (and (pair? (cdr args)) (string->number (cadr args)))
           (loop (cddr args) allow-read allow-write allow-exec net no-net
                 (string->number (cadr args)) inline-cmd)
           (error 'parse-sb-args "missing or invalid timeout after -t")))
        ;; -c cmd: inline command — all remaining tokens become the command string
        ((string=? (car args) "-c")
         (if (pair? (cdr args))
           (make-sb-parsed #f (sb-string-join (cdr args) " ")
             (build-sb-opts allow-read allow-write allow-exec net no-net timeout))
           (error 'parse-sb-args "missing command after -c")))
        ;; Non-flag argument: treat as script path
        ((not (sb-string-prefix? "-" (car args)))
         (make-sb-parsed (car args) inline-cmd
           (build-sb-opts allow-read allow-write allow-exec net no-net timeout)))
        ;; Unknown flag: skip
        (else
         (loop (cdr args) allow-read allow-write allow-exec net no-net timeout inline-cmd)))))

) ;; end library
