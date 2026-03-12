;;; script.ss — Script execution for jsh
;;; Handles running script files and sourcing files into the current environment.

(export #t)
(import :std/sugar
        :std/format
        :gerbil/runtime/init
        :jsh/util
        :jsh/ast
        :jsh/environment
        :jsh/functions
        :jsh/lexer
        :jsh/parser
        :jsh/executor
        :jsh/signals
        :jsh/jobs
        :jsh/static-compat
        :jsh/registry)

;;; --- Meta-command handler (set by main.ss to wire up ,compile etc.) ---

(def *meta-command-handler* (make-parameter #f))

;;; --- Gerbil Expander (lazy init) ---

(def *gerbil-eval-initialized* #f)

(def (ensure-gerbil-eval!)
  "Initialize the Gerbil expander on first use so eval supports full
   Gerbil syntax (def, defstruct, hash, match, import, etc.).
   Called lazily to avoid ~100ms startup cost for normal shell operations.
   Blocked in the 'tiny' tier which has no eval support."
  (when (string=? (*jsh-tier*) "tiny")
    (error "Gerbil eval not available in this build (tier: tiny). Rebuild with GSH_TIER=small or higher"))
  (unless *gerbil-eval-initialized*
    (set! *gerbil-eval-initialized* #t)
    (ensure-static-compat!)
    (__load-gxi)
    ;; After __load-gxi, re-patch load-module for the Gerbil expander context
    (when (scm-only-load-module-active?)
      (patch-loader-post-gxi!))))

;;; --- Scheme Evaluation Helpers ---

(def (fmt-bytes b)
  "Format a byte count as a human-readable string (B/KB/MB/GB)."
  (cond
    ((>= b (* 1024 1024 1024))
     (string-append (number->string (/ (floor (* (/ b (* 1024 1024 1024)) 100)) 100.0)) " GB"))
    ((>= b (* 1024 1024))
     (string-append (number->string (/ (floor (* (/ b (* 1024 1024)) 100)) 100.0)) " MB"))
    ((>= b 1024)
     (string-append (number->string (/ (floor (* (/ b 1024) 100)) 100.0)) " KB"))
    (else
     (string-append (number->string (inexact->exact (floor b))) " B"))))

(def (handle-room-command)
  "Display GC, heap, and runtime information (like Common Lisp's ROOM)."
  (with-catch
   (lambda (e)
     (cons (call-with-output-string
            (lambda (port) (display "Error: " port) (display-exception e port)))
           1))
   (lambda ()
     (##gc)
     (let* ((ps (##process-statistics))
            (user-cpu (f64vector-ref ps 0))
            (sys-cpu (f64vector-ref ps 1))
            (real-time (f64vector-ref ps 2))
            (gc-user (f64vector-ref ps 3))
            (gc-sys (f64vector-ref ps 4))
            (gc-real (f64vector-ref ps 5))
            (num-gcs (inexact->exact (floor (f64vector-ref ps 6))))
            (heap-size (f64vector-ref ps 7))
            (nb-minor-faults (inexact->exact (floor (f64vector-ref ps 8))))
            (nb-major-faults (inexact->exact (floor (f64vector-ref ps 9))))
            (alloc-total (f64vector-ref ps 15))
            (reclaimed (f64vector-ref ps 16))
            (live-heap (f64vector-ref ps 17))
            (movable (f64vector-ref ps 18))
            (still (f64vector-ref ps 19)))
       (cons
        (call-with-output-string
         (lambda (port)
           (display "--- GC & Heap ---\n" port)
           (display "  Heap size:       " port) (display (fmt-bytes heap-size) port) (newline port)
           (display "  Live after GC:   " port) (display (fmt-bytes live-heap) port) (newline port)
           (display "    Movable:       " port) (display (fmt-bytes movable) port) (newline port)
           (display "    Still:         " port) (display (fmt-bytes still) port) (newline port)
           (display "  Total allocated: " port) (display (fmt-bytes alloc-total) port) (newline port)
           (display "  Total reclaimed: " port) (display (fmt-bytes reclaimed) port) (newline port)
           (display "  GC runs:         " port) (display num-gcs port) (newline port)
           (display "  GC time:         " port)
           (display (/ (floor (* gc-real 1000)) 1.0) port) (display " ms real, " port)
           (display (/ (floor (* gc-user 1000)) 1.0) port) (display " ms cpu" port) (newline port)
           (display "  Live percent:    " port) (display (##get-live-percent) port) (display "%" port) (newline port)
           (newline port)
           (display "--- Process ---\n" port)
           (display "  CPU time:        " port)
           (display (/ (floor (* user-cpu 1000)) 1.0) port) (display " ms user, " port)
           (display (/ (floor (* sys-cpu 1000)) 1.0) port) (display " ms sys" port) (newline port)
           (display "  Real time:       " port)
           (display (/ (floor (* real-time 1000)) 1.0) port) (display " ms" port) (newline port)
           (display "  Page faults:     " port) (display nb-minor-faults port)
           (display " minor, " port) (display nb-major-faults port) (display " major" port) (newline port)
           (display "  CPU cache:       " port) (display (fmt-bytes (##cpu-cache-size)) port) (newline port)
           (newline port)
           (display "--- Runtime ---\n" port)
           (display "  Gambit:          " port) (display (##system-version-string) port) (newline port)
           (display "  Platform:        " port) (display (##system-type-string) port)))
        0)))))

(def (eval-scheme-expr expr-str)
  ;; Evaluate a Gerbil Scheme expression string and return (cons result-string status)
  ;; Status: 0 = success, 1 = error
  ;; Handle built-in meta-commands that don't need any tier
  (cond
   ((string=? expr-str "room") (handle-room-command))
   (else
    ;; Check for meta-commands (,compile, ,load, ,use, ,exports)
    (let ((handler (*meta-command-handler*)))
      (or (and handler (handler expr-str))
          ;; Normal Scheme eval
        (begin
          (ensure-gerbil-eval!)
          (with-catch
           (lambda (e)
             (cons (call-with-output-string
                    (lambda (port)
                      (display "Scheme error: " port)
                      (display-exception e port)))
                   1))
           (lambda ()
             (let* ((expr (call-with-input-string expr-str read))
                    (result (eval expr)))
               (cons
                (cond
                  ;; void: no output
                  ((eq? result (void)) "")
                  ;; Multiline results: use pretty-print
                  ((or (pair? result) (vector? result))
                   (call-with-output-string
                    (lambda (port)
                      (pretty-print result port))))
                  ;; Simple values: use write for unambiguous output
                  (else
                   (call-with-output-string
                    (lambda (port)
                      (write result port)))))
                0))))))))))

(def (scheme-eval-line? line)
  ;; Check if line starts with comma meta-command
  (and (> (string-length line) 0)
       (char=? (string-ref line 0) #\,)))

(def (extract-scheme-expr line)
  ;; Strip leading comma and whitespace
  (let* ((without-comma (substring line 1 (string-length line)))
         (start 0)
         (end (string-length without-comma)))
    ;; Trim leading whitespace
    (let loop-start ((i 0))
      (if (and (< i end) (char-whitespace? (string-ref without-comma i)))
        (loop-start (+ i 1))
        (substring without-comma i end)))))

;;; --- Public interface ---

;; Execute a script file with arguments.
;; Sets $0 to filename, $1.. to args.
;; Returns exit status.
(def (execute-script filename args env)
  (if (not (file-exists? filename))
    (begin
      (fprintf (current-error-port) "jsh: ~a: No such file or directory~n" filename)
      127)
    (with-catch
     (lambda (e)
       (cond
         ((break-exception? e) 0)
         ((continue-exception? e) 0)
         ((subshell-exit-exception? e) (subshell-exit-exception-status e))
         ((nounset-exception? e) (nounset-exception-status e))
         (else
          (fprintf (current-error-port) "jsh: ~a: ~a~n" filename (exception-message e))
          1)))
     (lambda ()
       (let* ((content (read-file-to-string filename))
              ;; Strip shebang if present
              (script-content (strip-shebang content))
              ;; Create child environment for script
              (script-env (env-push-scope env)))
         ;; Set positional parameters
         (env-set-shell-name! script-env filename)
         (env-set-positional! script-env args)
         ;; Set LINENO tracking
         (env-set! script-env "LINENO" "0")
         ;; Execute the script content
         (parameterize ((*current-source-file* filename))
           (execute-string script-content script-env)))))))

;; Source a file into the current environment (like bash's `source` or `.`)
;; Runs in the CURRENT environment, not a child.
;; Returns exit status.
(def (source-file! filename env)
  (if (not (file-exists? filename))
    (begin
      (fprintf (current-error-port) "jsh: ~a: No such file or directory~n" filename)
      1)
    (with-catch
     (lambda (e)
       (cond
         ;; break/continue must propagate to caller's loop
         ((break-exception? e) (raise e))
         ((continue-exception? e) (raise e))
         ;; return exits the sourced file, not the calling function
         ((return-exception? e) (return-exception-status e))
         ((errexit-exception? e) (raise e))
         ((subshell-exit-exception? e) (raise e))
         ((nounset-exception? e) (raise e))
         (else
          (fprintf (current-error-port) "jsh: ~a: ~a~n" filename (exception-message e))
          1)))
     (lambda ()
       (let* ((content (read-file-to-string filename))
              (script-content (strip-shebang content)))
         (parameterize ((*current-source-file* filename))
           (execute-string script-content env)))))))

;;; --- String execution ---

;; Parse and execute a string of shell commands.
;; Used by both execute-script and source-file!
;; Lines starting with comma (,) are evaluated as Scheme instead of being parsed as shell.
(def (execute-string input env (interactive? #f))
  ;; Split input into lines for preprocessing
  (let ((lines (string-split input #\newline)))
    (let line-loop ((remaining-lines lines) (status 0) (shell-buffer '()))
      (cond
        ;; No more lines - execute any pending shell commands
        ((null? remaining-lines)
         (if (null? shell-buffer)
           status
           (let ((shell-input (string-join (reverse shell-buffer) "\n")))
             (execute-shell-lines shell-input env interactive? status))))
        ;; Scheme eval line (starts with comma)
        ((scheme-eval-line? (car remaining-lines))
         ;; First, execute any accumulated shell commands
         (let* ((shell-status (if (null? shell-buffer)
                                status
                                (let ((shell-input (string-join (reverse shell-buffer) "\n")))
                                  (execute-shell-lines shell-input env interactive? status))))
                ;; Then evaluate the Scheme expression
                (expr-str (extract-scheme-expr (car remaining-lines)))
                (result-status (eval-scheme-expr expr-str))
                (result (car result-status))
                (scheme-status (cdr result-status)))
           ;; Display result if non-empty
           (unless (string=? result "")
             (display result)
             (newline))
           (env-set-last-status! env scheme-status)
           (line-loop (cdr remaining-lines) scheme-status '())))
        ;; Regular shell line - accumulate
        (else
         (line-loop (cdr remaining-lines) status (cons (car remaining-lines) shell-buffer)))))))

;; Execute accumulated shell lines using the lexer/parser
(def (execute-shell-lines input env interactive? initial-status)
  (let ((lexer (make-shell-lexer input (env-shopt? env "extglob"))))
    (let loop ((status initial-status))
      (let ((cmd (with-catch
                  (lambda (e)
                    (fprintf (current-error-port) "jsh: syntax error: ~a~n"
                             (exception-message e))
                    'error)
                  (lambda ()
                    ;; Update lexer extglob flag in case shopt changed it
                    (set! (lexer-extglob? lexer) (env-shopt? env "extglob"))
                    ;; Build alias lookup: checks expand_aliases shopt, returns value or #f
                    (let ((alias-fn (and (env-shopt? env "expand_aliases")
                                        (lambda (word) (alias-get env word)))))
                      (parse-one-line lexer (env-shopt? env "extglob") alias-fn))))))
        (cond
          ((eq? cmd 'error) 2)  ;; syntax error
          ((not cmd) status)     ;; end of input
          ;; Unterminated quote/construct after parsing — syntax error
          ((lexer-want-more? lexer)
           (fprintf (current-error-port)
                    "jsh: syntax error: unexpected end of file~n")
           (env-set-last-status! env 2)
           2)
          (else
           (let ((new-status
                  (with-catch
                   (lambda (e)
                     (cond
                       ((nounset-exception? e)
                        ;; In interactive mode, nounset only aborts current line
                        (if interactive?
                          (nounset-exception-status e)
                          (raise e)))
                       ((errexit-exception? e)
                        (errexit-exception-status e))
                       ((break-exception? e) (raise e))
                       ((continue-exception? e) (raise e))
                       ((subshell-exit-exception? e) (raise e))
                       ((return-exception? e) (raise e))
                       (else
                        ;; Catch-all: print error and continue
                        (let ((msg (exception-message e)))
                          (with-catch (lambda (_) #!void)
                            (lambda ()
                              (fprintf (current-error-port) "jsh: ~a~n" msg)))
                          ;; POSIX: syntax errors / unclosed bad substitution → exit code 2
                          (if (and (string? msg)
                                   (or (string-prefix? "parse error" msg)
                                       (string-prefix? "bad substitution: unclosed" msg)))
                            2 1)))))
                   (lambda ()
                     (execute-command cmd env)))))
             ;; Flush stdout/stderr so builtin output appears before next command
             (with-catch (lambda (_) #!void)
               (lambda ()
                 (force-output (current-output-port))
                 (force-output (current-error-port))))
             (env-set-last-status! env new-status)
             ;; Process pending signals between commands
             (process-pending-traps! env)
             ;; If errexit triggered, stop executing further commands
             (if (and (not (= new-status 0))
                      (env-option? env "errexit")
                      (not (*in-condition-context*)))
               new-status
               (loop new-status)))))))))

;; Process pending signals and execute trap commands
;; Lightweight version for script.ss (avoids circular import with main.ss)
;; Traps execute with $? isolated — they don't affect the main script's $?
(def (process-pending-traps! env)
  ;; No sleep needed — C-level signal flags are checked synchronously
  ;; in pending-signals! via ffi-signal-flag-check
  (let ((signals (pending-signals!)))
    (for-each
     (lambda (sig-name)
       (cond
         ((string=? sig-name "CHLD")
          (job-update-status!)
          (job-notify!))
         (else #!void))
       (let ((action (trap-get sig-name)))
         (cond
           ;; Signal has a trap command — execute it
           ((and action (string? action))
            ;; Save and restore $? so trap doesn't affect main flow
            (let ((saved-status (shell-environment-last-status env))
                  (exec-fn (*execute-input*)))
              (when exec-fn
                (exec-fn action env))
              (env-set-last-status! env saved-status)))
           ;; Fatal signal with no trap — exit the script
           ;; (POSIX: default action for INT/TERM/HUP/XFSZ is to terminate)
           ((and (not action)
                 (member sig-name '("INT" "TERM" "HUP" "XFSZ")))
            (let ((signum (signal-name->number sig-name)))
              (raise (make-subshell-exit-exception (+ 128 (or signum 2)))))))))
     signals)))

;;; --- Helpers ---

(def (strip-shebang content)
  ;; Replace #! line with blank line (preserves line numbering for extdebug)
  (if (and (>= (string-length content) 2)
           (char=? (string-ref content 0) #\#)
           (char=? (string-ref content 1) #\!))
    ;; Find end of first line and replace with empty
    (let loop ((i 0))
      (cond
        ((>= i (string-length content)) "")
        ((char=? (string-ref content i) #\newline)
         (substring content i (string-length content)))
        (else (loop (+ i 1)))))
    content))

(def (read-file-to-string filename)
  ;; Read entire file contents as a string
  (call-with-input-file filename
    (lambda (port)
      (let ((out (open-output-string)))
        (let loop ()
          (let ((ch (read-char port)))
            (unless (eof-object? ch)
              (write-char ch out)
              (loop))))
        (get-output-string out)))))
