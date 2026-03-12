;;; control.ss -- Compound commands (if/for/while/until/case/select) for gsh

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/environment
        :gsh/expander
        :gsh/glob
        :gsh/functions)

;; Wrap a thunk in a loop context (increments loop depth)
(def (with-loop-context thunk)
  (parameterize ((*loop-depth* (+ (*loop-depth*) 1)))
    (thunk)))

;;; --- Public interface ---
;;; All functions take an execute-fn callback to avoid circular dependency.
;;; execute-fn: (lambda (ast env) -> exit-status)

;; Execute an if-command
(def (execute-if cmd env execute-fn)
  (let loop ((clauses (if-command-clauses cmd)))
    (if (null? clauses)
      ;; No clause matched -- try else
      (if (if-command-else-part cmd)
        (execute-fn (if-command-else-part cmd) env)
        0)
      (let* ((clause (car clauses))
             (condition (car clause))
             (body (cdr clause))
             (test-status (parameterize ((*in-condition-context* #t))
                            (execute-fn condition env))))
        (if (= test-status 0)
          (execute-fn body env)
          (loop (cdr clauses)))))))

;; Execute a for-command
(def (execute-for cmd env execute-fn)
  (let* ((var-name (for-command-var cmd)))
    ;; Validate variable name
    (if (not (valid-identifier? var-name))
      (begin
        (fprintf (current-error-port) "gsh: `~a': not a valid identifier~n" var-name)
        2)
      (let* ((word-list (for-command-words cmd))
             ;; If words is #f, use positional params ("$@")
             (items (if word-list
                      (expand-words word-list env)
                      (env-at env))))
    (with-loop-context
     (lambda ()
       ;; Use iterative structure so raise from break/continue handler
       ;; propagates to the CALLER, not to a parent iteration's with-catch.
       (let ((remaining items)
             (status 0))
         (let loop ()
           (if (null? remaining)
             status
             (begin
               ;; Yield to signal handlers and process pending signals
               (thread-yield!)
               (let ((trap-fn (*process-traps-fn*)))
                 (when trap-fn (trap-fn env)))
               (env-set! env var-name (car remaining))
               (set! remaining (cdr remaining))
               (let ((caught
                      (with-catch
                       (lambda (e)
                         (cond
                           ((break-exception? e) (cons 'break e))
                           ((continue-exception? e) (cons 'continue e))
                           (else (raise e))))
                       (lambda ()
                         (set! status (execute-fn (for-command-body cmd) env))
                         #f))))
                 (cond
                   ((not caught) (loop))
                   ((eq? (car caught) 'break)
                    (let ((levels (break-exception-levels (cdr caught))))
                      (if (> levels 1)
                        (raise (make-break-exception (- levels 1)))
                        (shell-environment-last-status env))))
                   ((eq? (car caught) 'continue)
                    (let ((levels (continue-exception-levels (cdr caught))))
                      (if (> levels 1)
                        (raise (make-continue-exception (- levels 1)))
                        (loop))))))))))))))))

;; Execute a while-command
;; Uses iterative structure so raise from break/continue handler
;; propagates to the CALLER, not to a parent iteration's with-catch.
(def (execute-while cmd env execute-fn)
  (with-loop-context
   (lambda ()
     (let ((status 0))
       (let loop ()
         ;; Yield to signal handlers and process pending signals
         (thread-yield!)
         (let ((trap-fn (*process-traps-fn*)))
           (when trap-fn (trap-fn env)))
         ;; Evaluate test condition with break/continue handling
         (let ((test-caught
                (with-catch
                 (lambda (e)
                   (cond
                     ((break-exception? e) (cons 'break e))
                     ((continue-exception? e) (cons 'continue e))
                     (else (raise e))))
                 (lambda ()
                   (let ((ts (parameterize ((*in-condition-context* #t))
                               (execute-fn (while-command-test cmd) env))))
                     (cons 'test ts))))))
           (cond
             ;; break in condition
             ((eq? (car test-caught) 'break)
              (let ((levels (break-exception-levels (cdr test-caught))))
                (if (> levels 1)
                  (raise (make-break-exception (- levels 1)))
                  status)))
             ;; continue in condition: re-test
             ((eq? (car test-caught) 'continue)
              (let ((levels (continue-exception-levels (cdr test-caught))))
                (if (> levels 1)
                  (raise (make-continue-exception (- levels 1)))
                  (loop))))
             ;; Normal test result
             (else
              (let ((test-status (cdr test-caught)))
                (if (= test-status 0)
                  ;; Test passed — execute body
                  (let ((body-caught
                         (with-catch
                          (lambda (e)
                            (cond
                              ((break-exception? e) (cons 'break e))
                              ((continue-exception? e) (cons 'continue e))
                              (else (raise e))))
                          (lambda ()
                            (set! status (execute-fn (while-command-body cmd) env))
                            #f))))
                    (cond
                      ((not body-caught) (loop))
                      ((eq? (car body-caught) 'break)
                       (let ((levels (break-exception-levels (cdr body-caught))))
                         (if (> levels 1)
                           (raise (make-break-exception (- levels 1)))
                           (shell-environment-last-status env))))
                      ((eq? (car body-caught) 'continue)
                       (let ((levels (continue-exception-levels (cdr body-caught))))
                         (if (> levels 1)
                           (raise (make-continue-exception (- levels 1)))
                           (loop))))))
                  ;; Test failed — exit loop
                  status))))))))))

;; Execute an until-command
;; Uses iterative structure so raise from break/continue handler
;; propagates to the CALLER, not to a parent iteration's with-catch.
(def (execute-until cmd env execute-fn)
  (with-loop-context
   (lambda ()
     (let ((status 0))
       (let loop ()
         ;; Yield to signal handlers and process pending signals
         (thread-yield!)
         (let ((trap-fn (*process-traps-fn*)))
           (when trap-fn (trap-fn env)))
         (let ((test-status (parameterize ((*in-condition-context* #t))
                              (execute-fn (until-command-test cmd) env))))
           (if (not (= test-status 0))
             ;; Test still failing — execute body
             (let ((caught
                    (with-catch
                     (lambda (e)
                       (cond
                         ((break-exception? e) (cons 'break e))
                         ((continue-exception? e) (cons 'continue e))
                         (else (raise e))))
                     (lambda ()
                       (set! status (execute-fn (until-command-body cmd) env))
                       #f))))
               (cond
                 ((not caught) (loop))
                 ((eq? (car caught) 'break)
                  (let ((levels (break-exception-levels (cdr caught))))
                    (if (> levels 1)
                      (raise (make-break-exception (- levels 1)))
                      (shell-environment-last-status env))))
                 ((eq? (car caught) 'continue)
                  (let ((levels (continue-exception-levels (cdr caught))))
                    (if (> levels 1)
                      (raise (make-continue-exception (- levels 1)))
                      (loop))))))
             ;; Test succeeded — exit loop
             status)))))))

;; Execute a case-command
(def (execute-case cmd env execute-fn)
  (let* ((word (expand-word-nosplit (case-command-word cmd) env))
         (clauses (case-command-clauses cmd)))
    (let loop ((clauses clauses))
      (if (null? clauses)
        0
        (let* ((clause (car clauses))
               (patterns (case-clause-patterns clause))
               (body (case-clause-body clause))
               (terminator (case-clause-terminator clause)))
          (if (any-pattern-matches? patterns word env)
            (let ((status (if body (execute-fn body env) 0)))
              (case terminator
                ;; ;; -- break
                ((break) status)
                ;; ;& -- fallthrough (execute next clause bodies unconditionally)
                ;; Continue falling through while clauses end with ;&
                ((fallthrough)
                 (let fall ((rest (cdr clauses)) (last-status status))
                   (if (null? rest)
                     last-status
                     (let* ((next-clause (car rest))
                            (next-body (case-clause-body next-clause))
                            (next-term (case-clause-terminator next-clause))
                            (next-status (if next-body
                                           (execute-fn next-body env)
                                           last-status)))
                       (if (eq? next-term 'fallthrough)
                         (fall (cdr rest) next-status)
                         next-status)))))
                ;; ;;& -- test-next (continue checking patterns)
                ((test-next)
                 (let ((rest-status (loop (cdr clauses))))
                   (if (= rest-status 0) status rest-status)))
                (else status)))
            (loop (cdr clauses))))))))

;; Execute a select-command
(def (execute-select cmd env execute-fn)
  (let* ((var-name (select-command-var cmd))
         (word-list (select-command-words cmd))
         (items (expand-words word-list env))
         (ps3 (or (env-get env "PS3") "#? ")))
    ;; Print numbered menu
    (let menu-loop ((status 0))
      ;; Display choices
      (let ((i 1))
        (for-each
         (lambda (item)
           (fprintf (current-error-port) "~a) ~a~n" i item)
           (set! i (+ i 1)))
         items))
      ;; Read selection
      (display ps3 (current-error-port))
      (force-output (current-error-port))
      (let ((line (read-line)))
        (if (eof-object? line)
          status
          (let ((n (string->number (string-trim-ws line))))
            (if (and n (> n 0) (<= n (length items)))
              (begin
                (env-set! env var-name (list-ref items (- n 1)))
                (env-set! env "REPLY" line)
                (with-catch
                 (lambda (e)
                   (cond
                     ((break-exception? e) status)
                     (else (raise e))))
                 (lambda ()
                   (let ((new-status (execute-fn (select-command-body cmd) env)))
                     (menu-loop new-status)))))
              (begin
                (env-set! env var-name "")
                (env-set! env "REPLY" line)
                (let ((new-status (execute-fn (select-command-body cmd) env)))
                  (menu-loop new-status))))))))))

;;; --- Helpers ---

(def (any-pattern-matches? patterns word env)
  (let loop ((pats patterns))
    (if (null? pats)
      #f
      (let ((pat (expand-word-as-pattern (car pats) env)))
        (if (or (string=? pat "*")
                (glob-match? pat word #f (env-shopt? env "extglob")))
          #t
          (loop (cdr pats)))))))

(def (valid-identifier? name)
  ;; A valid shell variable name: starts with letter/underscore,
  ;; followed by letters, digits, or underscores
  (and (string? name)
       (> (string-length name) 0)
       (let ((ch0 (string-ref name 0)))
         (or (char-alphabetic? ch0) (char=? ch0 #\_)))
       (let loop ((i 1))
         (if (>= i (string-length name))
           #t
           (let ((ch (string-ref name i)))
             (if (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))
               (loop (+ i 1))
               #f))))))

(def (string-trim-ws str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))
