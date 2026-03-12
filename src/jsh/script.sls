#!chezscheme
(library (jsh script)
  (export *meta-command-handler* *gerbil-eval-initialized*
    ensure-gerbil-eval! fmt-bytes handle-room-command
    eval-scheme-expr scheme-eval-line? extract-scheme-expr
    execute-script source-file! execute-string
    execute-shell-lines process-pending-traps! strip-shebang
    read-file-to-string)
  (import
   (except (chezscheme) box box? unbox set-box! andmap ormap
    iota last-pair find \x31;+ \x31;- fx/ fx1+ fx1- error? raise
    with-exception-handler identifier? hash-table?
    make-hash-table sort sort! path-extension printf fprintf
    file-directory? file-exists? getenv close-port void
    open-output-file open-input-file)
   (except (jerboa runtime) bind-method! call-method ~ void
     cons* make-list)
   (runtime mop)
   (except (runtime util) last-pair iota \x31;- \x31;+
     displayln make-keyword)
   (except (compat gambit) number->string make-mutex
     with-output-to-string string->bytes bytes->string thread?)
   (except (std error) with-exception-handler error-trace
     error-irritants error-message)
   (except (std misc string) string-join string-split
     string-index string-empty?)
   (except (std misc list) take drop filter-map)
   (except (std misc alist) pget pgetv pgetq aget agetv agetq)
   (except
     (std os path)
     path-expand
     path-normalize
     path-absolute?)
   (except (std format) format) (std sort) (std pregexp)
   (std sugar)
   (except (jsh util) string-index string-join file-directory?
     string-join string-index string-downcase file-regular?
     string-upcase)
   (jsh ast) (jsh environment) (jsh functions) (jsh lexer)
   (jsh parser) (jsh executor) (jsh signals)
   (except (jsh jobs) any every find) (jsh static-compat)
   (jsh registry))
  (define *meta-command-handler* (make-parameter #f))
  (define *gerbil-eval-initialized*-cell (vector #f))
  (define-syntax *gerbil-eval-initialized*
    (identifier-syntax
      [id (vector-ref *gerbil-eval-initialized*-cell 0)]
      [(set! id v) (vector-set!
                     *gerbil-eval-initialized*-cell
                     0
                     v)]))
  (define (ensure-gerbil-eval!)
    "Initialize the Gerbil expander on first use so eval supports full\n   Gerbil syntax (def, defstruct, hash, match, import, etc.).\n   Called lazily to avoid ~100ms startup cost for normal shell operations.\n   Blocked in the 'tiny' tier which has no eval support."
    (when (string=? (*jsh-tier*) "tiny")
      (error 'gerbil
        "Gerbil eval not available in this build (tier: tiny). Rebuild with GSH_TIER=small or higher"))
    (unless *gerbil-eval-initialized*
      (set! *gerbil-eval-initialized* #t)
      (ensure-static-compat!)
      (__load-gxi)
      (when (scm-only-load-module-active?)
        (patch-loader-post-gxi!))))
  (define (fmt-bytes b)
    "Format a byte count as a human-readable string (B/KB/MB/GB)."
    (cond
      [(>= b (* 1024 1024 1024))
       (string-append
         (number->string
           (/ (floor (* (/ b (* 1024 1024 1024)) 100)) 100.0))
         " GB")]
      [(>= b (* 1024 1024))
       (string-append
         (number->string
           (/ (floor (* (/ b (* 1024 1024)) 100)) 100.0))
         " MB")]
      [(>= b 1024)
       (string-append
         (number->string (/ (floor (* (/ b 1024) 100)) 100.0))
         " KB")]
      [else
       (string-append
         (number->string (inexact->exact (floor b)))
         " B")]))
  (define (handle-room-command)
    "Display GC, heap, and runtime information (like Common Lisp's ROOM)."
    (guard (__exn
             [#t
              ((lambda (e)
                 (cons
                   (call-with-output-string
                     (lambda (port)
                       (display "Error: " port)
                       (display-exception e port)))
                   1))
                __exn)])
      (\x23;\x23;gc)
      (let* ([ps (\x23;\x23;process-statistics)])
        (let* ([user-cpu (f64vector-ref ps 0)])
          (let* ([sys-cpu (f64vector-ref ps 1)])
            (let* ([real-time (f64vector-ref ps 2)])
              (let* ([gc-user (f64vector-ref ps 3)])
                (let* ([gc-sys (f64vector-ref ps 4)])
                  (let* ([gc-real (f64vector-ref ps 5)])
                    (let* ([num-gcs (inexact->exact
                                      (floor (f64vector-ref ps 6)))])
                      (let* ([heap-size (f64vector-ref ps 7)])
                        (let* ([nb-minor-faults (inexact->exact
                                                  (floor
                                                    (f64vector-ref
                                                      ps
                                                      8)))])
                          (let* ([nb-major-faults (inexact->exact
                                                    (floor
                                                      (f64vector-ref
                                                        ps
                                                        9)))])
                            (let* ([alloc-total (f64vector-ref ps 15)])
                              (let* ([reclaimed (f64vector-ref ps 16)])
                                (let* ([live-heap (f64vector-ref ps 17)])
                                  (let* ([movable (f64vector-ref ps 18)])
                                    (let* ([still (f64vector-ref ps 19)])
                                      (cons
                                        (call-with-output-string
                                          (lambda (port)
                                            (display
                                              "--- GC & Heap ---\n"
                                              port)
                                            (display
                                              "  Heap size:       "
                                              port)
                                            (display
                                              (fmt-bytes heap-size)
                                              port)
                                            (newline port)
                                            (display
                                              "  Live after GC:   "
                                              port)
                                            (display
                                              (fmt-bytes live-heap)
                                              port)
                                            (newline port)
                                            (display
                                              "    Movable:       "
                                              port)
                                            (display
                                              (fmt-bytes movable)
                                              port)
                                            (newline port)
                                            (display
                                              "    Still:         "
                                              port)
                                            (display
                                              (fmt-bytes still)
                                              port)
                                            (newline port)
                                            (display
                                              "  Total allocated: "
                                              port)
                                            (display
                                              (fmt-bytes alloc-total)
                                              port)
                                            (newline port)
                                            (display
                                              "  Total reclaimed: "
                                              port)
                                            (display
                                              (fmt-bytes reclaimed)
                                              port)
                                            (newline port)
                                            (display
                                              "  GC runs:         "
                                              port)
                                            (display num-gcs port)
                                            (newline port)
                                            (display
                                              "  GC time:         "
                                              port)
                                            (display
                                              (/ (floor (* gc-real 1000))
                                                 1.0)
                                              port)
                                            (display " ms real, " port)
                                            (display
                                              (/ (floor (* gc-user 1000))
                                                 1.0)
                                              port)
                                            (display " ms cpu" port)
                                            (newline port)
                                            (display
                                              "  Live percent:    "
                                              port)
                                            (display
                                              (\x23;\x23;get-live-percent)
                                              port)
                                            (display "%" port)
                                            (newline port)
                                            (newline port)
                                            (display
                                              "--- Process ---\n"
                                              port)
                                            (display
                                              "  CPU time:        "
                                              port)
                                            (display
                                              (/ (floor (* user-cpu 1000))
                                                 1.0)
                                              port)
                                            (display " ms user, " port)
                                            (display
                                              (/ (floor (* sys-cpu 1000))
                                                 1.0)
                                              port)
                                            (display " ms sys" port)
                                            (newline port)
                                            (display
                                              "  Real time:       "
                                              port)
                                            (display
                                              (/ (floor (* real-time 1000))
                                                 1.0)
                                              port)
                                            (display " ms" port)
                                            (newline port)
                                            (display
                                              "  Page faults:     "
                                              port)
                                            (display nb-minor-faults port)
                                            (display " minor, " port)
                                            (display nb-major-faults port)
                                            (display " major" port)
                                            (newline port)
                                            (display
                                              "  CPU cache:       "
                                              port)
                                            (display
                                              (fmt-bytes
                                                (\x23;\x23;cpu-cache-size))
                                              port)
                                            (newline port)
                                            (newline port)
                                            (display
                                              "--- Runtime ---\n"
                                              port)
                                            (display
                                              "  Gambit:          "
                                              port)
                                            (display
                                              (\x23;\x23;system-version-string)
                                              port)
                                            (newline port)
                                            (display
                                              "  Platform:        "
                                              port)
                                            (display
                                              (\x23;\x23;system-type-string)
                                              port)))
                                        0)))))))))))))))))))
  (define (eval-scheme-expr expr-str)
    (cond
      [(string=? expr-str "room") (handle-room-command)]
      [else
       (let ([handler (*meta-command-handler*)])
         (or (and handler (handler expr-str))
             (begin
               (ensure-gerbil-eval!)
               (guard (__exn
                        [#t
                         ((lambda (e)
                            (cons
                              (call-with-output-string
                                (lambda (port)
                                  (display "Scheme error: " port)
                                  (display-exception e port)))
                              1))
                           __exn)])
                 (let* ([expr (call-with-input-string expr-str read)])
                   (let* ([result (eval expr)])
                     (cons
                       (cond
                         [(eq? result (void)) ""]
                         [(or (pair? result) (vector? result))
                          (call-with-output-string
                            (lambda (port) (pretty-print result port)))]
                         [else
                          (call-with-output-string
                            (lambda (port) (write result port)))])
                       0)))))))]))
  (define (scheme-eval-line? line)
    (and (> (string-length line) 0)
         (char=? (string-ref line 0) #\,)))
  (define (extract-scheme-expr line)
    (let* ([without-comma (substring
                            line
                            1
                            (string-length line))])
      (let* ([start 0])
        (let* ([end (string-length without-comma)])
          (let loop-start ([i 0])
            (if (and (< i end)
                     (char-whitespace? (string-ref without-comma i)))
                (loop-start (+ i 1))
                (substring without-comma i end)))))))
  (define (execute-script filename args env)
    (if (not (file-exists? filename))
        (begin
          (fprintf
            (current-error-port)
            "gsh: ~a: No such file or directory~n"
            filename)
          127)
        (guard (__exn
                 [#t
                  ((lambda (e)
                     (cond
                       [(break-exception? e) 0]
                       [(continue-exception? e) 0]
                       [(subshell-exit-exception? e)
                        (subshell-exit-exception-status e)]
                       [(nounset-exception? e)
                        (nounset-exception-status e)]
                       [else
                        (fprintf
                          (current-error-port)
                          "gsh: ~a: ~a~n"
                          filename
                          (exception-message e))
                        1]))
                    __exn)])
          (let* ([content (read-file-to-string filename)])
            (let* ([script-content (strip-shebang content)])
              (let* ([script-env (env-push-scope env)])
                (env-set-shell-name! script-env filename)
                (env-set-positional! script-env args)
                (env-set! script-env "LINENO" "0")
                (parameterize ([*current-source-file* filename])
                  (execute-string script-content script-env))))))))
  (define (source-file! filename env)
    (if (not (file-exists? filename))
        (begin
          (fprintf
            (current-error-port)
            "gsh: ~a: No such file or directory~n"
            filename)
          1)
        (guard (__exn
                 [#t
                  ((lambda (e)
                     (cond
                       [(break-exception? e) (raise e)]
                       [(continue-exception? e) (raise e)]
                       [(return-exception? e) (return-exception-status e)]
                       [(errexit-exception? e) (raise e)]
                       [(subshell-exit-exception? e) (raise e)]
                       [(nounset-exception? e) (raise e)]
                       [else
                        (fprintf
                          (current-error-port)
                          "gsh: ~a: ~a~n"
                          filename
                          (exception-message e))
                        1]))
                    __exn)])
          (let* ([content (read-file-to-string filename)])
            (let* ([script-content (strip-shebang content)])
              (parameterize ([*current-source-file* filename])
                (execute-string script-content env)))))))
  (define execute-string
    (case-lambda
      [(input env)
       (let* ([interactive? #f])
         (let ([lines (let ([str input]
                            [sep (if (char? #\newline)
                                     #\newline
                                     (string-ref #\newline 0))])
                        (let split-lp ([i 0] [start 0] [acc '()])
                          (cond
                            [(= i (string-length str))
                             (reverse (cons (substring str start i) acc))]
                            [(char=? (string-ref str i) sep)
                             (split-lp
                               (+ i 1)
                               (+ i 1)
                               (cons (substring str start i) acc))]
                            [else (split-lp (+ i 1) start acc)])))])
           (let line-loop ([remaining-lines lines]
                           [status 0]
                           [shell-buffer '()])
             (cond
               [(null? remaining-lines)
                (if (null? shell-buffer)
                    status
                    (let ([shell-input (let ([strs (reverse shell-buffer)]
                                             [sep "\n"])
                                         (if (null? strs)
                                             ""
                                             (let lp ([result (car strs)]
                                                      [rest (cdr strs)])
                                               (if (null? rest)
                                                   result
                                                   (lp (string-append
                                                         result
                                                         sep
                                                         (car rest))
                                                       (cdr rest))))))])
                      (execute-shell-lines
                        shell-input
                        env
                        interactive?
                        status)))]
               [(scheme-eval-line? (car remaining-lines))
                (let* ([shell-status (if (null? shell-buffer)
                                         status
                                         (let ([shell-input (let ([strs (reverse
                                                                          shell-buffer)]
                                                                  [sep "\n"])
                                                              (if (null?
                                                                    strs)
                                                                  ""
                                                                  (let lp ([result (car strs)]
                                                                           [rest (cdr strs)])
                                                                    (if (null?
                                                                          rest)
                                                                        result
                                                                        (lp (string-append
                                                                              result
                                                                              sep
                                                                              (car rest))
                                                                            (cdr rest))))))])
                                           (execute-shell-lines
                                             shell-input
                                             env
                                             interactive?
                                             status)))])
                  (let* ([expr-str (extract-scheme-expr
                                     (car remaining-lines))])
                    (let* ([result-status (eval-scheme-expr expr-str)])
                      (let* ([result (car result-status)])
                        (let* ([scheme-status (cdr result-status)])
                          (unless (string=? result "")
                            (display result)
                            (newline))
                          (env-set-last-status! env scheme-status)
                          (line-loop
                            (cdr remaining-lines)
                            scheme-status
                            '()))))))]
               [else
                (line-loop
                  (cdr remaining-lines)
                  status
                  (cons (car remaining-lines) shell-buffer))]))))]
      [(input env interactive?)
       (let ([lines (let ([str input]
                          [sep (if (char? #\newline)
                                   #\newline
                                   (string-ref #\newline 0))])
                      (let split-lp ([i 0] [start 0] [acc '()])
                        (cond
                          [(= i (string-length str))
                           (reverse (cons (substring str start i) acc))]
                          [(char=? (string-ref str i) sep)
                           (split-lp
                             (+ i 1)
                             (+ i 1)
                             (cons (substring str start i) acc))]
                          [else (split-lp (+ i 1) start acc)])))])
         (let line-loop ([remaining-lines lines]
                         [status 0]
                         [shell-buffer '()])
           (cond
             [(null? remaining-lines)
              (if (null? shell-buffer)
                  status
                  (let ([shell-input (let ([strs (reverse shell-buffer)]
                                           [sep "\n"])
                                       (if (null? strs)
                                           ""
                                           (let lp ([result (car strs)]
                                                    [rest (cdr strs)])
                                             (if (null? rest)
                                                 result
                                                 (lp (string-append
                                                       result
                                                       sep
                                                       (car rest))
                                                     (cdr rest))))))])
                    (execute-shell-lines
                      shell-input
                      env
                      interactive?
                      status)))]
             [(scheme-eval-line? (car remaining-lines))
              (let* ([shell-status (if (null? shell-buffer)
                                       status
                                       (let ([shell-input (let ([strs (reverse
                                                                        shell-buffer)]
                                                                [sep "\n"])
                                                            (if (null?
                                                                  strs)
                                                                ""
                                                                (let lp ([result (car strs)]
                                                                         [rest (cdr strs)])
                                                                  (if (null?
                                                                        rest)
                                                                      result
                                                                      (lp (string-append
                                                                            result
                                                                            sep
                                                                            (car rest))
                                                                          (cdr rest))))))])
                                         (execute-shell-lines
                                           shell-input
                                           env
                                           interactive?
                                           status)))])
                (let* ([expr-str (extract-scheme-expr
                                   (car remaining-lines))])
                  (let* ([result-status (eval-scheme-expr expr-str)])
                    (let* ([result (car result-status)])
                      (let* ([scheme-status (cdr result-status)])
                        (unless (string=? result "")
                          (display result)
                          (newline))
                        (env-set-last-status! env scheme-status)
                        (line-loop
                          (cdr remaining-lines)
                          scheme-status
                          '()))))))]
             [else
              (line-loop
                (cdr remaining-lines)
                status
                (cons (car remaining-lines) shell-buffer))])))]))
  (define (execute-shell-lines input env interactive?
           initial-status)
    (let ([lexer (make-shell-lexer
                   input
                   (env-shopt? env "extglob"))])
      (let loop ([status initial-status])
        (let ([cmd (guard (__exn
                            [#t
                             ((lambda (e)
                                (fprintf
                                  (current-error-port)
                                  "gsh: syntax error: ~a~n"
                                  (exception-message e))
                                'error)
                               __exn)])
                     (lexer-extglob?-set! lexer (env-shopt? env "extglob"))
                     (let ([alias-fn (and (env-shopt? env "expand_aliases")
                                          (lambda (word)
                                            (alias-get env word)))])
                       (parse-one-line
                         lexer
                         (env-shopt? env "extglob")
                         alias-fn)))])
          (cond
            [(eq? cmd 'error) 2]
            [(not cmd) status]
            [(lexer-want-more? lexer)
             (fprintf
               (current-error-port)
               "gsh: syntax error: unexpected end of file~n")
             (env-set-last-status! env 2)
             2]
            [else
             (let ([new-status (guard (__exn
                                        [#t
                                         ((lambda (e)
                                            (cond
                                              [(nounset-exception? e)
                                               (if interactive?
                                                   (nounset-exception-status
                                                     e)
                                                   (raise e))]
                                              [(errexit-exception? e)
                                               (errexit-exception-status
                                                 e)]
                                              [(break-exception? e)
                                               (raise e)]
                                              [(continue-exception? e)
                                               (raise e)]
                                              [(subshell-exit-exception? e)
                                               (raise e)]
                                              [(return-exception? e)
                                               (raise e)]
                                              [else
                                               (let ([msg (exception-message
                                                            e)])
                                                 (guard (__exn
                                                          [#t
                                                           ((lambda (_)
                                                              (%%void))
                                                             __exn)])
                                                   (fprintf
                                                     (current-error-port)
                                                     "gsh: ~a~n"
                                                     msg))
                                                 (if (and (string? msg)
                                                          (or (let ([pfx "parse error"]
                                                                    [str msg])
                                                                (let ([plen (string-length
                                                                              pfx)])
                                                                  (and (<= plen
                                                                           (string-length
                                                                             str))
                                                                       (string=?
                                                                         pfx
                                                                         (substring
                                                                           str
                                                                           0
                                                                           plen)))))
                                                              (let ([pfx "bad substitution: unclosed"]
                                                                    [str msg])
                                                                (let ([plen (string-length
                                                                              pfx)])
                                                                  (and (<= plen
                                                                           (string-length
                                                                             str))
                                                                       (string=?
                                                                         pfx
                                                                         (substring
                                                                           str
                                                                           0
                                                                           plen)))))))
                                                     2
                                                     1))]))
                                           __exn)])
                                 (execute-command cmd env))])
               (guard (__exn [#t ((lambda (_) (%%void)) __exn)])
                 (flush-output-port (current-output-port))
                 (flush-output-port (current-error-port)))
               (env-set-last-status! env new-status)
               (process-pending-traps! env)
               (if (and (not (= new-status 0))
                        (env-option? env "errexit")
                        (not (*in-condition-context*)))
                   new-status
                   (loop new-status)))])))))
  (define (process-pending-traps! env)
    (let ([signals (pending-signals!)])
      (for-each
        (lambda (sig-name)
          (cond
            [(string=? sig-name "CHLD")
             (job-update-status!)
             (job-notify!)]
            [else (%%void)])
          (let ([action (trap-get sig-name)])
            (cond
              [(and action (string? action))
               (let ([saved-status (shell-environment-last-status env)]
                     [exec-fn (*execute-input*)])
                 (when exec-fn (exec-fn action env))
                 (env-set-last-status! env saved-status))]
              [(and (not action)
                    (member sig-name '("INT" "TERM" "HUP" "XFSZ")))
               (let ([signum (signal-name->number sig-name)])
                 (raise
                   (make-subshell-exit-exception
                     (+ 128 (or signum 2)))))])))
        signals)))
  (define (strip-shebang content)
    (if (and (>= (string-length content) 2)
             (char=? (string-ref content 0) #\#)
             (char=? (string-ref content 1) #\!))
        (let loop ([i 0])
          (cond
            [(>= i (string-length content)) ""]
            [(char=? (string-ref content i) #\newline)
             (substring content i (string-length content))]
            [else (loop (+ i 1))]))
        content))
  (define (read-file-to-string filename)
    (call-with-input-file
      filename
      (lambda (port)
        (let ([out (open-output-string)])
          (let loop ()
            (let ([ch (read-char port)])
              (unless (eof-object? ch) (write-char ch out) (loop))))
          (get-output-string out))))))
