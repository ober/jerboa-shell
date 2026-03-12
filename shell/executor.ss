;;; executor.ss — Command dispatch and execution for gsh
;;; Walks the AST and executes commands.

(export #t)
(import :std/sugar
        :std/format
        ./pregexp-compat
        :gsh/ast
        :gsh/environment
        :gsh/expander
        :gsh/registry
        :gsh/builtins
        :gsh/functions
        :gsh/pipeline
        :gsh/redirect
        :gsh/control
        :gsh/jobs
        :gsh/util
        :gsh/arithmetic
        :gsh/ffi
        :gsh/glob
        :gsh/signals)

;;; --- Public interface ---

;; Execute a command AST node, return exit status (0-255)
(def (execute-command cmd env)
  (cond
    ((not cmd) 0)
    ((redirected-command? cmd)
     (let ((s (execute-redirected-command cmd env)))
       (env-set-last-status! env s)
       (check-errexit! env s)
       s))
    ((simple-command? cmd) (execute-simple-command cmd env))
    ((ast-pipeline? cmd)
     (let ((s (execute-ast-pipeline cmd env)))
       ;; ! prefix suppresses errexit; non-bang pipelines need check
       (unless (ast-pipeline-bang? cmd)
         (check-errexit! env s))
       s))
    ((and-or-list? cmd) (execute-and-or cmd env))
    ((command-list? cmd) (execute-command-list cmd env))
    ((subshell? cmd)
     (let ((s (execute-subshell cmd env)))
       (check-errexit! env s)
       s))
    ((brace-group? cmd) (execute-brace-group cmd env))
    ((if-command? cmd) (execute-if cmd env execute-command))
    ((for-command? cmd) (execute-for cmd env execute-command))
    ((while-command? cmd) (execute-while cmd env execute-command))
    ((until-command? cmd) (execute-until cmd env execute-command))
    ((case-command? cmd) (execute-case cmd env execute-command))
    ((select-command? cmd) (execute-select cmd env execute-command))
    ((cond-command? cmd)
     (let ((s (execute-cond-command cmd env)))
       (env-set-last-status! env s)
       (check-errexit! env s)
       s))
    ((arith-command? cmd)
     (let ((s (execute-arith-command cmd env)))
       (env-set-last-status! env s)
       (check-errexit! env s)
       s))
    ((arith-for-command? cmd)
     (let ((s (execute-arith-for cmd env execute-command)))
       (env-set-last-status! env s) s))
    ((coproc-command? cmd)
     (let ((s (execute-coproc cmd env)))
       (env-set-last-status! env s) s))
    ((function-def? cmd) (execute-function-def cmd env))
    ((time-command? cmd) (execute-time-command cmd env))
    (else
     (fprintf (current-error-port) "gsh: unknown command type~n")
     1)))

;;; --- Assignment helper ---

(def (apply-assignment! asgn env)
  (let* ((raw-name (assignment-name asgn))
         ;; Resolve nameref: assignments through a nameref go to the target
         (name (resolve-nameref raw-name env))
         (index (assignment-index asgn))
         (raw-value (assignment-value asgn))
         (op (assignment-op asgn)))
    (cond
      ;; Compound array assignment: arr=(val1 val2 ...) or arr+=(val1 val2 ...)
      ;; Use expand-word (not nosplit) so glob patterns expand (e.g. a=(*.txt))
      ;; Note: raw-value is a list (possibly empty for a=())
      ((list? raw-value)
       (let ((expanded (append-map (lambda (v) (expand-word v env)) raw-value)))
         (if (eq? op '+=)
           (env-array-append-compound! env name expanded)
           (env-array-set-compound! env name expanded
                                   ;; Check if already declared as assoc
                                   (let ((var (env-get-var env name)))
                                     (and var (shell-var-assoc? var)))))))
      ;; Array element assignment: arr[idx]=val
      (index
       (let ((expanded-index (expand-word-nosplit index env))
             (val (expand-assignment-value raw-value env)))
         (if (eq? op '+=)
           (let ((old (env-array-get env name expanded-index)))
             (env-array-set! env name expanded-index (string-append old val)))
           (env-array-set! env name expanded-index val))))
      ;; Scalar assignment (existing behavior)
      (else
       (let ((val (expand-assignment-value raw-value env)))
         (if (eq? op '+=)
           (let ((old (or (env-get env name) "")))
             (env-set! env name (string-append old val)))
           (env-set! env name val)))))))

;;; --- Temp environment for command-scoped assignments ---
;; FOO=bar cmd → FOO must be exported so external commands see it.
;; Each binding is visible to subsequent bindings in the same prefix.
;; After the command returns, the bindings are discarded.
(def (apply-temp-assignments assignments env)
  ;; Filter out array subscript and compound array assignments in env prefix.
  ;; Bash silently ignores b[0]=2 in prefix position (doesn't export it).
  (let* ((filtered (filter (lambda (asgn)
                             (and (not (assignment-index asgn))    ;; no arr[idx]=val
                                  (not (list? (assignment-value asgn))))) ;; no arr=(...)
                           assignments))
         (child (env-push-scope env)))
    (for-each
     (lambda (asgn)
       (let* ((raw-name (assignment-name asgn))
              (name (resolve-nameref raw-name env))
              (raw-value (assignment-value asgn))
              (op (assignment-op asgn))
              (val (expand-assignment-value raw-value child))
              (final-val (if (eq? op '+=)
                           (string-append (or (env-get child name) "") val)
                           val)))
         ;; Set directly in child scope as exported var
         ;; (make-shell-var value exported? readonly? local? integer? uppercase? lowercase? nameref? array? assoc?)
         (hash-put! (shell-environment-vars child) name
                    (make-shell-var final-val #t #f #f #f #f #f #f #f #f))))
     filtered)
    child))

;;; --- Simple command execution ---

(def (execute-simple-command cmd env)
  (let* ((assignments (simple-command-assignments cmd))
         (raw-words (simple-command-words cmd))
         (redirections (simple-command-redirections cmd)))
    ;; If no command words, apply assignments and redirections
    ;; Exit status is that of the last command substitution, or 0
    (if (null? raw-words)
      (begin
        ;; Expand assignments. Don't reset $? before expansion so that
        ;; status=$? captures the previous command's exit status.
        ;; Track whether any command substitution ran during expansion.
        (parameterize ((*command-sub-ran* #f))
          (for-each (lambda (asgn) (apply-assignment! asgn env)) assignments)
          ;; Process redirections even without command (e.g. "> file" creates empty file)
          (let ((status (if (pair? redirections)
                          (with-catch
                           (lambda (e)
                             (fprintf (current-error-port) "gsh: ~a~n" (exception-message e))
                             1)
                           (lambda ()
                             (let ((saved (apply-redirections redirections env)))
                               (restore-redirections saved)
                               ;; If command sub ran, use its status; otherwise 0
                               (if (*command-sub-ran*)
                                 (shell-environment-last-status env)
                                 0))))
                          ;; No redirections: use command sub status or 0
                          (if (*command-sub-ran*)
                            (shell-environment-last-status env)
                            0))))
            (env-set-last-status! env status)
            (check-errexit! env status)
            status)))
      ;; Expand words (process substitutions handled by expand-word)
      (parameterize ((*procsub-cleanups* []))
        (unwind-protect
         (let* ((expanded (with-catch
                           (lambda (e)
                             ;; failglob: command aborted, return status 1
                             (if (and (pair? e) (eq? (car e) 'failglob))
                               #f
                               (raise e)))
                           (lambda ()
                             ;; For declaration builtins, expand assignment args without splitting
                             ;; Only apply for LITERAL declaration keywords, not dynamic ($var)
                             (if (and (pair? raw-words)
                                      (literal-declaration-builtin? (car raw-words)))
                               (let ((first-word (expand-word-nosplit (car raw-words) env)))
                                 (cons first-word
                                       (expand-declaration-args (cdr raw-words) env)))
                               (expand-words raw-words env)))))
                (cmd-name (if (and expanded (pair? expanded)) (car expanded) #f))
                (args (if (and expanded (pair? expanded)) (cdr expanded) [])))
           (if (not expanded)
             ;; Expansion failed (failglob) → return 1
             (begin (env-set-last-status! env 1) 1)
           (if (or (not cmd-name) (string=? cmd-name ""))
             ;; No command or empty after expansion
             ;; Check if it's a literal empty string ('' or "")
             (if (and cmd-name (string=? cmd-name "")
                      (word-is-literal-empty? raw-words))
               ;; Literal '' or "" → command not found
               (begin
                 (fprintf (current-error-port) "gsh: : command not found~n")
                 127)
               ;; Expansion produced empty (e.g. $(true)) → preserve $?
               (shell-environment-last-status env))
             ;; Normal command execution
             (begin
               ;; Apply xtrace if enabled
               (when (env-option? env "xtrace")
                 (let ((ps4 (or (env-get env "PS4") "+ ")))
                   (fprintf (current-error-port) "~a~a~n"
                            ps4 (string-join-words expanded))))
               ;; Apply command-scoped assignments
               ;; These must be exported so external commands see them.
               ;; Exception: declaration builtins (local, declare, etc.)
               ;; ignore prefix assignments — they operate on the current scope.
               (let* ((temp-env (if (and (pair? assignments)
                                         (not (declaration-builtin? cmd-name)))
                                  (apply-temp-assignments assignments env)
                                  env))
                      (status
                       (with-catch
                        (lambda (e)
                          (cond
                            ((return-exception? e) (raise e))
                            ((break-exception? e) (raise e))
                            ((continue-exception? e) (raise e))
                            ((errexit-exception? e) (raise e))
                            ((nounset-exception? e) (raise e))
                            ((subshell-exit-exception? e) (raise e))
                            (else
                             (fprintf (current-error-port) "gsh: ~a~n" (exception-message e))
                             1)))
                        (lambda ()
                          ;; Special handling for exec builtin
                          (if (string=? cmd-name "exec")
                            (execute-exec args redirections temp-env)
                            (let ((saved (apply-redirections redirections temp-env)))
                              (unwind-protect
                               (cond
                                 ;; Check for shell function
                                 ((function-lookup env cmd-name)
                                  => (lambda (func)
                                       (let* ((func-redirs (shell-function-redirections func))
                                              (func-saved (if (pair? func-redirs)
                                                            (apply-redirections func-redirs temp-env)
                                                            []))
                                              (result (function-call func args temp-env execute-command)))
                                         (when (pair? func-saved)
                                           (restore-redirections func-saved))
                                         result)))
                                 ;; Check for builtin
                                 ((builtin-lookup cmd-name)
                                  => (lambda (handler)
                                       (handler args temp-env)))
                                 ;; External command
                                 (else
                                  (execute-external cmd-name args temp-env)))
                               ;; Always restore redirections, even on exception
                               (restore-redirections saved))))))))
                 ;; Set last status and PIPESTATUS
                 (env-set-last-status! env status)
                 (env-array-set-compound! env "PIPESTATUS"
                                          [(number->string status)] #f)
                 ;; Check errexit
                 (check-errexit! env status)
                 status)))))
         (run-procsub-cleanups!))))))

;; Check if a string contains a character (executor-local helper)
(def (has-equals-sign? str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond ((>= i len) #f)
            ((char=? (string-ref str i) #\=) #t)
            (else (loop (+ i 1)))))))

;; Check if a command name is a declaration builtin
;; (export, declare, typeset, local, readonly)
;; For these, assignment-like args (containing =) should not be word-split
(def (declaration-builtin? name)
  (member name '("export" "declare" "typeset" "local" "readonly")))

;; Check if a raw word is a LITERAL declaration builtin name.
;; Raw words from the parser are strings (token values from the lexer).
;; Dynamic command names ($myvar expanding to "typeset") should NOT suppress splitting —
;; those are NOT strings at this point, they're words with variable refs.
(def (literal-declaration-builtin? raw-word)
  (and (string? raw-word)
       (member raw-word '("export" "declare" "typeset" "local" "readonly"))
       #t))

;; Expand args for declaration builtins.
;; Flag args (starting with - or +) and assignment args (containing =)
;; are expanded without word splitting. Other args are expanded normally.
;; For literal assignment args (name=value), expands tilde in the value part
;; so builtin handlers don't need to re-expand (which would double-expand
;; tilde from variable references like readonly "$binding").
(def (expand-declaration-args words env)
  (append-map
   (lambda (w)
     (let ((s (if (string? w) w (expand-word-nosplit w env))))
       (cond
         ;; Flag argument starting with - or +
         ((and (string? s) (> (string-length s) 0)
               (or (char=? (string-ref s 0) #\-)
                   (char=? (string-ref s 0) #\+)))
          ;; Check for multi-word array expansion (e.g. -"${a[@]}" → -x foo=F bar=B).
          ;; expand-words handles IFS splitting and globbing, which is correct for
          ;; the flag case since flags don't have assignment values to protect.
          (let ((multi (expand-words [w] env)))
            (if (> (length multi) 1)
              multi  ;; Array in flag produced multiple words
              [s]))) ;; Normal flag: reuse nosplit result (no double-expansion)
         ;; Literal assignment argument (contains =) - expand value with
         ;; assignment tilde rules (tilde at start and after colons)
         ((and (string? w) (has-equals-sign? w))
              (let* ((eq-pos (string-find-char* w #\=))
                     (raw-val (and eq-pos (substring w (+ eq-pos 1) (string-length w)))))
                ;; Compound array assignment (value starts with "(")
                (if (and raw-val (> (string-length raw-val) 0)
                         (char=? (string-ref raw-val 0) #\())
                  (let* ((name-part (expand-word-nosplit (substring w 0 (+ eq-pos 1)) env))
                         ;; Strip outer parens
                         (inner (if (and (> (string-length raw-val) 1)
                                         (char=? (string-ref raw-val (- (string-length raw-val) 1)) #\)))
                                  (substring raw-val 1 (- (string-length raw-val) 1))
                                  (substring raw-val 1 (string-length raw-val))))
                         ;; Parse raw elements preserving quoting structure
                         (raw-elems (parse-array-compound-raw inner))
                         ;; Brace-expand each element (e.g. {1..9} → "1" "2" ... "9")
                         (brace-elems (if (env-option? env "braceexpand")
                                        (append-map brace-expand raw-elems)
                                        raw-elems))
                         ;; Expand each element individually, re-quote for reassembly
                         (expanded-elems (map (lambda (e) (expand-word-nosplit e env)) brace-elems))
                         ;; Re-quote each element with declare-quote-value so empty strings
                         ;; survive as "" and special chars are escaped
                         (quoted-elems (map declare-quote-value expanded-elems))
                         ;; Reassemble: name=("elem1" "elem2" ...)
                         (rebuilt (string-append "(" (string-join quoted-elems " ") ")")))
                    [(string-append name-part rebuilt)])
                  ;; For literal assignments: split raw name=value BEFORE expansion,
                  ;; then expand name and value separately to avoid double quote removal.
                  (let* ((raw-eq-pos (string-find-char* w #\=))
                         (raw-name (and raw-eq-pos (substring w 0 (+ raw-eq-pos 1))))
                         (raw-val (and raw-eq-pos (substring w (+ raw-eq-pos 1) (string-length w)))))
                    (if raw-eq-pos
                      (let* ((expanded-name (expand-word-nosplit raw-name env))
                             (expanded-val (expand-assignment-value raw-val env)))
                        [(string-append expanded-name expanded-val)])
                      [(expand-word-nosplit w env)])))))
         ;; Regular arg - normal expansion with splitting
         (else (expand-word w env)))))
   words))

;; Check if raw-words contain only literal empty strings ('' or "")
;; These should result in "command not found" rather than preserving $?
(def (word-is-literal-empty? raw-words)
  (and (pair? raw-words)
       (let ((first-word (car raw-words)))
         (and (string? first-word)
              (let ((len (string-length first-word)))
                (or (and (= len 2)
                         (char=? (string-ref first-word 0) #\')
                         (char=? (string-ref first-word 1) #\'))
                    (and (= len 2)
                         (char=? (string-ref first-word 0) #\")
                         (char=? (string-ref first-word 1) #\"))))))))

;;; --- External command execution ---

(def (execute-external cmd-name args env)
  (let* ((path (which cmd-name))
         ;; Resolve relative paths to absolute for open-process (OS cwd may
         ;; differ from Gambit's current-directory)
         (exec-path (and path (if (string-contains? path "/")
                               (path-expand path) path))))
    (if (not path)
      (begin
        (fprintf (current-error-port) "gsh: ~a: command not found~n" cmd-name)
        127)
      (with-catch
       (lambda (e)
         (fprintf (current-error-port) "gsh: ~a: ~a~n" cmd-name (exception-message e))
         126)
       (lambda ()
         ;; If running inside a pipeline thread, dup2 the pipe fds onto
         ;; fd 0/1 so the child process inherits them. Save/restore around
         ;; open-process to minimize the window of altered fds.
         ;; Lock the pipeline mutex to prevent other threads' nested pipelines
         ;; from modifying real fds 0/1 during our fork window.
         (let* ((pipe-in (*pipeline-stdin-fd*))
                (pipe-out (*pipeline-stdout-fd*))
                (in-pipeline? (or pipe-in pipe-out))
                (saved-0 (and pipe-in (ffi-dup 0)))
                (saved-1 (and pipe-out (ffi-dup 1))))
           (when in-pipeline? (mutex-lock! *pipeline-fd-mutex*))
           (when pipe-in (ffi-dup2 pipe-in 0))
           (when pipe-out (ffi-dup2 pipe-out 1))
           ;; Flush buffered Scheme output before forking so child
           ;; doesn't interleave with pending parent output
           (force-output)
           (force-output (current-error-port))
           ;; Sync fd 1/2 offsets to end-of-file after flushing.
           ;; The Gambit port may use a separate file description (independent
           ;; offset) from fd 1. After flushing, seek fd 1 to EOF so the
           ;; child process writes after the flushed data, not at offset 0.
           (ffi-lseek-end 1)
           (ffi-lseek-end 2)
           (let* ((packed-argv (pack-with-soh
                                (map string->c-safe (cons cmd-name args))))
                  (packed-env (pack-with-soh
                               (map string->c-safe (env-exported-alist env))))
                  (keep-fds (pack-fds-with-soh (*active-redirect-fds*)))
                  (pid (ffi-fork-exec (string->c-safe exec-path)
                                      packed-argv packed-env
                                      0  ;; pgid=0: foreground
                                      (*gambit-scheduler-rfd*)
                                      (*gambit-scheduler-wfd*)
                                      keep-fds
                                      (current-directory))))
             ;; Restore fds immediately after fork
             (when saved-1 (ffi-dup2 saved-1 1) (ffi-close-fd saved-1))
             (when saved-0 (ffi-dup2 saved-0 0) (ffi-close-fd saved-0))
             (when in-pipeline? (mutex-unlock! *pipeline-fd-mutex*))
             (if (< pid 0)
               (begin
                 (fprintf (current-error-port) "gsh: ~a: fork failed~n" cmd-name)
                 126)
               (let-values (((exit-code stopped?)
                             (wait-for-foreground-process-raw pid)))
                 ;; Unblock SIGCHLD now that we've reaped the child
                 (ffi-sigchld-unblock)
                 (if stopped?
                   ;; Ctrl-Z: add to job table as stopped
                   (let ((cmd-text (string-join-words (cons cmd-name args)))
                         (job (job-table-add! [(cons pid #f)] "" pid)))
                     (set! (job-command-text job) cmd-text)
                     (set! (job-status job) 'stopped)
                     (for-each (lambda (p) (set! (job-process-status p) 'stopped))
                               (job-processes job))
                     (fprintf (current-error-port) "~n[~a]+  Stopped                 ~a~n"
                              (job-id job) cmd-text)
                     (+ 128 20))  ;; 148 = 128 + SIGTSTP(20)
                   ;; Normal exit
                   (begin
                     ;; If child was killed by a signal (exit >= 128), clear the
                     ;; corresponding pending signal so the shell doesn't run the
                     ;; trap for it (bash behavior: trap only fires for signals
                     ;; received directly by the shell, not relayed from children)
                     (when (>= exit-code 128)
                       (let ((sig-name (signal-number->name (- exit-code 128))))
                         (when sig-name
                           (clear-pending-signal! sig-name))))
                     ;; Sync Gambit port position with fd 1 after child wrote.
                     ;; The child may have written via fd 1 (separate file
                     ;; description from the Gambit port), advancing fd 1's
                     ;; offset. Seek the Gambit port to EOF so subsequent
                     ;; builtins write after the child's output.
                     (let ((end-pos (ffi-lseek-end 1)))
                       (when (> end-pos 0)
                         (with-catch void
                           (lambda ()
                             (output-port-byte-position
                              (current-output-port) end-pos)))))
                     exit-code)))))))))))

;;; --- exec builtin ---

;; exec [-cl] [-a name] [command [args ...]]
;; With no command: apply redirections permanently
;; With command: replace shell process
(def (execute-exec args redirections env)
  ;; Apply redirections permanently (don't save/restore)
  (apply-redirections-permanent! redirections env)
  ;; Parse exec flags: -c (clear env), -l (login), -a name (argv[0])
  (let parse-flags ((args args) (argv0 #f) (clear-env? #f))
    (cond
      ((null? args)
       ;; No command — redirections persist
       0)
      ;; -- stops option processing
      ((string=? (car args) "--")
       (exec-command (cdr args) argv0 clear-env? env))
      ;; -a name
      ((string=? (car args) "-a")
       (if (pair? (cdr args))
         (parse-flags (cddr args) (cadr args) clear-env?)
         (begin
           (fprintf (current-error-port) "gsh: exec: -a: option requires an argument~n")
           1)))
      ;; -c (clear environment)
      ((string=? (car args) "-c")
       (parse-flags (cdr args) argv0 #t))
      ;; -l (login shell prefix)
      ((string=? (car args) "-l")
       (parse-flags (cdr args) (or argv0 'login) clear-env?))
      ;; Not a flag — it's the command
      (else
       (exec-command args argv0 clear-env? env)))))

(def (exec-command args argv0 clear-env? env)
  (if (null? args)
    0
    (let* ((cmd-name (car args))
           (cmd-args (cdr args))
           (path (which cmd-name)))
      (if (not path)
        (begin
          (fprintf (current-error-port) "gsh: exec: ~a: not found~n" cmd-name)
          127)
        (let* ((exec-path (if (string-contains? path "/")
                            (path-expand path) path))
               (env-alist (if clear-env? [] (env-exported-alist env))))
          ;; Use ffi-execve to truly replace the process image.
          ;; This is needed for exec -a (custom argv[0]) and is also
          ;; more correct than open-process + exit for regular exec.
          (let* ((actual-argv0 (cond
                                 ((eq? argv0 'login) (string-append "-" cmd-name))
                                 ((string? argv0) argv0)
                                 (else cmd-name)))
                 (argv-list (cons actual-argv0 cmd-args))
                 (packed-argv (pack-with-soh (map string->c-safe argv-list)))
                 (packed-env (pack-with-soh (map string->c-safe env-alist))))
            ;; Flush ports before execve replaces the process
            (force-output (current-output-port))
            (force-output (current-error-port))
            (let ((err (ffi-execve (string->c-safe exec-path) packed-argv packed-env)))
              ;; execve only returns on failure
              (fprintf (current-error-port) "gsh: exec: ~a: ~a~n" cmd-name
                       (cond ((= err 13) "Permission denied")
                             ((= err 2) "No such file or directory")
                             (else (string-append "errno " (number->string err)))))
              126)))))))

;;; --- Pipeline execution ---

(def (execute-ast-pipeline cmd env)
  (let* ((commands (ast-pipeline-commands cmd))
         (bang? (ast-pipeline-bang? cmd))
         (pipe-types (ast-pipeline-pipe-types cmd))
         ;; ! prefix suppresses errexit
         (exit-codes (if bang?
                       (parameterize ((*in-condition-context* #t))
                         (execute-pipeline commands env execute-command pipe-types))
                       (execute-pipeline commands env execute-command pipe-types)))
         (last-status (if (pair? exit-codes) (last-elem exit-codes) 0))
         ;; pipefail: use rightmost non-zero exit code from pipeline
         (status (if (env-option? env "pipefail")
                   (let loop ((codes exit-codes) (fail 0))
                     (if (null? codes) (if (= fail 0) last-status fail)
                       (loop (cdr codes) (if (not (= (car codes) 0)) (car codes) fail))))
                   last-status)))
    ;; Set PIPESTATUS array
    (env-array-set-compound! env "PIPESTATUS"
                             (map number->string exit-codes) #f)
    (let ((final (if bang? (if (= status 0) 1 0) status)))
      (env-set-last-status! env final)
      final)))

;;; --- And-or list execution ---

(def (execute-and-or cmd env)
  (let* ((rest-items (and-or-list-rest cmd))
         ;; If there are && or || operators, the first command is in condition context
         (first-in-condition? (pair? rest-items))
         (status (if first-in-condition?
                   (parameterize ((*in-condition-context* #t))
                     (execute-command (and-or-list-first cmd) env))
                   (execute-command (and-or-list-first cmd) env))))
    (let loop ((rest rest-items) (status status))
      (if (null? rest)
        (begin (env-set-last-status! env status) status)
        (let* ((item (car rest))
               (op (car item))
               (pipeline (cdr item))
               ;; All but the last element in && / || chain are condition context
               (is-condition? (pair? (cdr rest))))
          (cond
            ;; && — run next only if status = 0
            ((and (eq? op 'and) (= status 0))
             (if is-condition?
               (loop (cdr rest) (parameterize ((*in-condition-context* #t))
                                  (execute-command pipeline env)))
               (loop (cdr rest) (execute-command pipeline env))))
            ;; || — run next only if status != 0
            ((and (eq? op 'or) (not (= status 0)))
             (if is-condition?
               (loop (cdr rest) (parameterize ((*in-condition-context* #t))
                                  (execute-command pipeline env)))
               (loop (cdr rest) (execute-command pipeline env))))
            (else
             (loop (cdr rest) status))))))))

;;; --- Command list execution ---

(def (execute-command-list cmd env)
  (let loop ((items (command-list-items cmd)) (status 0))
    (if (null? items)
      (begin (env-set-last-status! env status) status)
      (let* ((item (car items))
             (mode (car item))
             (command (cdr item)))
        (case mode
          ((sequential)
           (let ((new-status (execute-command command env)))
             (env-set-last-status! env new-status)
             ;; Yield to let Gambit's signal handler threads run
             ;; and queue any pending signals to *pending-signals*
             (thread-yield!)
             ;; Process pending signals between commands (bash behavior)
             (let ((trap-fn (*process-traps-fn*)))
               (when trap-fn (trap-fn env)))
             (loop (cdr items) new-status)))
          ((background)
           ;; Launch command in background
           (let ((result (launch-background command env)))
             (env-set-last-bg-pid! env (car result))
             (let ((job (job-table-add! (cdr result)
                                        (or (ast->command-text command) "&")
                                        (car result))))
               ;; Only print job notifications in interactive mode
               (when (*interactive-shell*)
                 (fprintf (current-error-port) "[~a] ~a~n"
                          (job-id job) (car result))))
             (loop (cdr items) 0)))
          (else
           (loop (cdr items) (execute-command command env))))))))

;;; --- Compound commands ---

(def (execute-subshell cmd env)
  ;; Execute in a fully cloned environment — variable changes, cd, etc.
  ;; don't affect the parent.
  (let ((child-env (env-clone env))
        (saved-cwd (current-directory)))
    (let ((status (with-catch
                   (lambda (e)
                     (cond
                       ((subshell-exit-exception? e) (subshell-exit-exception-status e))
                       ((errexit-exception? e) (errexit-exception-status e))
                       ((nounset-exception? e) (nounset-exception-status e))
                       ;; break/continue/return inside subshell must NOT propagate
                       ;; to parent — they are contained within the subshell
                       ((break-exception? e) 0)
                       ((continue-exception? e) 0)
                       ((return-exception? e) (return-exception-status e))
                       (else (raise e))))
                   (lambda ()
                     (parameterize ((*in-subshell* #t)
                                    (*loop-depth* 0))
                       (execute-command (subshell-body cmd) child-env))))))
      ;; Restore parent's working directory
      (current-directory saved-cwd)
      (env-set-last-status! env status)
      status)))

(def (execute-redirected-command cmd env)
  (let ((inner (redirected-command-command cmd))
        (redirs (redirected-command-redirections cmd)))
    (with-catch
     (lambda (e)
       (cond
         ((return-exception? e) (raise e))
         ((break-exception? e) (raise e))
         ((continue-exception? e) (raise e))
         ((errexit-exception? e) (raise e))
         ((nounset-exception? e) (raise e))
         ((subshell-exit-exception? e) (raise e))
         (else
          (fprintf (current-error-port) "gsh: ~a~n" (exception-message e))
          1)))
     (lambda ()
       (let ((saved (apply-redirections redirs env)))
         (let ((result (execute-command inner env)))
           (restore-redirections saved)
           result))))))

(def (execute-brace-group cmd env)
  (execute-command (brace-group-body cmd) env))

(def (execute-function-def cmd env)
  (function-define! env
                    (function-def-name cmd)
                    (function-def-body cmd)
                    (function-def-redirections cmd)
                    (function-def-lineno cmd)
                    (*current-source-file*))
  0)

;;; --- [[ ]] conditional command ---

(def (execute-cond-command cmd env)
  (if (eval-cond-expr (cond-command-expr cmd) env) 0 1))

;; Evaluate a conditional expression node, returns #t or #f
(def (eval-cond-expr expr env)
  (cond
    ((cond-binary? expr)
     (let ((op (cond-binary-op expr)))
       (cond
         ((string=? op "&&")
          (and (eval-cond-expr (cond-binary-left expr) env)
               (eval-cond-expr (cond-binary-right expr) env)))
         ((string=? op "||")
          (or (eval-cond-expr (cond-binary-left expr) env)
              (eval-cond-expr (cond-binary-right expr) env)))
         (else #f))))
    ((cond-not? expr)
     (not (eval-cond-expr (cond-not-expr expr) env)))
    ((cond-unary-test? expr)
     (eval-cond-unary (cond-unary-test-op expr)
                      (expand-word-nosplit (cond-unary-test-arg expr) env)
                      env))
    ((cond-binary-test? expr)
     (eval-cond-binary-test (cond-binary-test-op expr)
                            (expand-word-nosplit (cond-binary-test-left expr) env)
                            (cond-binary-test-right expr)
                            env))
    ((cond-word? expr)
     ;; Bare word: true if non-empty after expansion
     (let ((val (expand-word-nosplit (cond-word-value expr) env)))
       (and val (> (string-length val) 0))))
    (else #f)))

;; Evaluate unary test operators
(def (eval-cond-unary op arg env)
  (cond
    ((string=? op "-z") (= (string-length arg) 0))
    ((string=? op "-n") (> (string-length arg) 0))
    ((string=? op "-e") (file-exists? arg))
    ((string=? op "-f") (file-regular? arg))
    ((string=? op "-d") (file-directory? arg))
    ((string=? op "-L") (file-symlink? arg))
    ((string=? op "-h") (eval-cond-unary "-L" arg env))
    ((string=? op "-r") (and (file-exists? arg) (cond-file-access? arg 4)))
    ((string=? op "-w") (and (file-exists? arg) (cond-file-access? arg 2)))
    ((string=? op "-x") (and (file-exists? arg) (cond-file-access? arg 1)))
    ((string=? op "-s") (file-nonempty? arg))
    ((string=? op "-p") (and (file-exists? arg)
                             (eq? (file-info-type (file-info arg)) 'fifo)))
    ((string=? op "-S") (and (file-exists? arg)
                             (eq? (file-info-type (file-info arg)) 'socket)))
    ((string=? op "-b") (and (file-exists? arg)
                             (eq? (file-info-type (file-info arg)) 'block-special)))
    ((string=? op "-c") (and (file-exists? arg)
                             (eq? (file-info-type (file-info arg)) 'character-special)))
    ((string=? op "-t") (let ((fd (string->number arg)))
                          (and fd (= (ffi-isatty fd) 1))))
    ((string=? op "-a") (file-exists? arg))  ;; -a same as -e in [[ ]]
    ((string=? op "-v") (let ((val (env-get env arg)))
                          (and val #t)))
    ((string=? op "-o") #f)  ;; shell option — stub
    ((string=? op "-g") (and (file-exists? arg)  ;; setgid
                             (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o2000)))))
    ((string=? op "-u") (and (file-exists? arg)  ;; setuid
                             (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o4000)))))
    ((string=? op "-k") (and (file-exists? arg)  ;; sticky
                             (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o1000)))))
    ((string=? op "-G") (and (file-exists? arg)  ;; owned by egid
                             (= (file-info-group (file-info arg)) (ffi-getegid))))
    ((string=? op "-O") (and (file-exists? arg)  ;; owned by euid
                             (= (file-info-owner (file-info arg)) (ffi-geteuid))))
    ((string=? op "-N") (and (file-exists? arg)  ;; modified since last read
                             (let ((fi (file-info arg)))
                               (> (time->seconds (file-info-last-modification-time fi))
                                  (time->seconds (file-info-last-access-time fi))))))
    ((string=? op "-R") (let ((var (env-get-var env arg)))
                          (and var (shell-var-nameref? var))))
    (else #f)))

;; Check file access using ffi-access
(def (cond-file-access? path mode)
  (= (ffi-access path mode) 0))

;; Evaluate binary test operators
(def (eval-cond-binary-test op left right-raw env)
  (cond
    ;; Regex match — right side is NOT expanded as normal word
    ((string=? op "=~")
     (let ((right (expand-word-nosplit right-raw env)))
       (with-catch
        (lambda (e)
          (fprintf (current-error-port) "gsh: [[ =~ ]]: invalid regex: ~a~n" right)
          #f)
        (lambda ()
          (let ((m (pregexp-match right left)))
            (if m
              (begin
                ;; Set BASH_REMATCH array
                (env-array-set-compound! env "BASH_REMATCH"
                  (map (lambda (v) (or v "")) m) #f)
                #t)
              (begin
                (env-array-set-compound! env "BASH_REMATCH" [] #f)
                #f)))))))
    ;; Pattern match (glob) — expand RHS to strip quotes, then glob-match
    ((or (string=? op "=") (string=? op "=="))
     (let ((right (expand-word-nosplit right-raw env)))
       (glob-match? right left #f (env-shopt? env "extglob"))))
    ((string=? op "!=")
     (let ((right (expand-word-nosplit right-raw env)))
       (not (glob-match? right left #f (env-shopt? env "extglob")))))
    ;; String comparison
    ((string=? op "<") (string<? left (expand-word-nosplit right-raw env)))
    ((string=? op ">") (string>? left (expand-word-nosplit right-raw env)))
    ;; Integer comparison
    ((string=? op "-eq") (cond-int-cmp = left right-raw env))
    ((string=? op "-ne") (cond-int-cmp (lambda (a b) (not (= a b))) left right-raw env))
    ((string=? op "-lt") (cond-int-cmp < left right-raw env))
    ((string=? op "-gt") (cond-int-cmp > left right-raw env))
    ((string=? op "-le") (cond-int-cmp <= left right-raw env))
    ((string=? op "-ge") (cond-int-cmp >= left right-raw env))
    ;; File comparison
    ((string=? op "-ef") (cond-same-file? left (expand-word-nosplit right-raw env)))
    ((string=? op "-nt") (cond-newer-than? left (expand-word-nosplit right-raw env)))
    ((string=? op "-ot") (cond-newer-than? (expand-word-nosplit right-raw env) left))
    (else #f)))

;; Evaluate a string as an integer for [[ ]] comparisons
;; Uses arithmetic evaluation to handle expressions and variable references
(def (arith-string-to-int str env)
  (let ((n (string->number str)))
    (if n n
      ;; Try arithmetic evaluation (handles "e" where e=1+2)
      (with-catch
       (lambda (e) #f)
       (lambda ()
         (arith-eval str (arith-env-getter env) (arith-env-setter env)))))))

(def (cond-int-cmp cmp left right-raw env)
  (let ((right (expand-word-nosplit right-raw env)))
    (let ((a (arith-string-to-int left env))
          (b (arith-string-to-int right env)))
      (and a b (cmp a b)))))

(def (cond-same-file? a b)
  (and (file-exists? a) (file-exists? b)
       (let ((fi-a (file-info a))
             (fi-b (file-info b)))
         (and (= (file-info-device fi-a) (file-info-device fi-b))
              (= (file-info-inode fi-a) (file-info-inode fi-b))))))

(def (cond-newer-than? a b)
  (and (file-exists? a) (file-exists? b)
       (> (time->seconds (file-info-last-modification-time (file-info a)))
          (time->seconds (file-info-last-modification-time (file-info b))))))

;; (( expr )) — arithmetic command
;; Returns 0 if expression result is non-zero (true), 1 if zero (false)
(def (execute-arith-command cmd env)
  (with-catch
   (lambda (e)
     (cond
       ((nounset-exception? e) (raise e))
       (else
        (fprintf (current-error-port) "gsh: ~a~n" (exception-message e))
        1)))
   (lambda ()
     (let* ((raw-expr (arith-command-expression cmd))
            ;; Expand in dquote context — arithmetic doesn't do word splitting
            (expr (parameterize ((*in-dquote-context* #t))
                    (expand-arith-expr raw-expr env)))
            (result (arith-eval expr
                                (arith-env-getter env)
                                (arith-env-setter env)
                                (and (env-option? env "nounset")
                                     (lambda (name)
                                       (nounset-error! name env))))))
       (if (= result 0) 1 0)))))

;; for (( init; test; update )) ; do body ; done
(def (execute-arith-for cmd env exec-fn)
  (let ((getter (arith-env-getter env))
        (setter (arith-env-setter env))
        ;; Expand in dquote context — arithmetic doesn't do word splitting
        (expand (lambda (expr) (if expr
                                 (parameterize ((*in-dquote-context* #t))
                                   (expand-arith-expr expr env))
                                 expr))))
    ;; Execute init expression
    (let ((init-expr (arith-for-command-init cmd)))
      (when (and init-expr (> (string-length init-expr) 0))
        (arith-eval (expand init-expr) getter setter)))
    ;; Loop: test, body, update (with loop context for break/continue)
    ;; Uses iterative structure so raise from break/continue handler
    ;; propagates to the CALLER, not to a parent iteration's with-catch.
    (parameterize ((*loop-depth* (+ (*loop-depth*) 1)))
      (let ((status 0))
        (let loop ()
          ;; Evaluate test
          (let* ((test-expr (arith-for-command-test cmd))
                 (test-val (if (and test-expr (> (string-length test-expr) 0))
                             (arith-eval (expand test-expr) getter setter)
                             1)))  ;; empty test = true
            (if (= test-val 0)
              status  ;; test is false, exit loop
              ;; Execute body with break/continue handling
              (let ((caught
                     (with-catch
                      (lambda (e)
                        (cond
                          ((break-exception? e) (cons 'break e))
                          ((continue-exception? e) (cons 'continue e))
                          (else (raise e))))
                      (lambda ()
                        (set! status (exec-fn (arith-for-command-body cmd) env))
                        ;; Execute update expression
                        (let ((update-expr (arith-for-command-update cmd)))
                          (when (and update-expr (> (string-length update-expr) 0))
                            (arith-eval (expand update-expr) getter setter)))
                        #f))))
                (cond
                  ((not caught) (loop))
                  ((eq? (car caught) 'break)
                   (let ((levels (break-exception-levels (cdr caught))))
                     (if (> levels 1)
                       (raise (make-break-exception (- levels 1)))
                       status)))
                  ((eq? (car caught) 'continue)
                   (let ((levels (continue-exception-levels (cdr caught))))
                     (if (> levels 1)
                       (raise (make-continue-exception (- levels 1)))
                       ;; Continue: run update, then loop
                       (begin
                         (let ((update-expr (arith-for-command-update cmd)))
                           (when (and update-expr (> (string-length update-expr) 0))
                             (arith-eval (expand update-expr) getter setter)))
                         (loop))))))))))))))

(def (check-errexit! env status)
  (when (and (not (= status 0))
             (env-option? env "errexit")
             (not (*in-condition-context*)))
    (raise (make-errexit-exception status))))

;;; --- time command ---

(def (execute-time-command cmd env)
  (let* ((posix? (time-command-posix? cmd))
         (pipeline (time-command-pipeline cmd))
         (start-real (real-time))
         (start-cpu (cpu-time))
         (status (if pipeline
                   (execute-command pipeline env)
                   0))
         (end-real (real-time))
         (end-cpu (cpu-time))
         (elapsed-real (fl- end-real start-real))
         (elapsed-user (fl- end-cpu start-cpu))
         ;; System time not available from Gambit, approximate as 0
         (elapsed-sys 0.0))
    (if posix?
      ;; POSIX format: real/user/sys in seconds
      (begin
        (fprintf (current-error-port) "real ~a~n" (format-time-posix elapsed-real))
        (fprintf (current-error-port) "user ~a~n" (format-time-posix elapsed-user))
        (fprintf (current-error-port) "sys ~a~n" (format-time-posix elapsed-sys)))
      ;; Default bash-like format
      (begin
        (fprintf (current-error-port) "~nreal\t~a~n" (format-time-bash elapsed-real))
        (fprintf (current-error-port) "user\t~a~n" (format-time-bash elapsed-user))
        (fprintf (current-error-port) "sys\t~a~n" (format-time-bash elapsed-sys))))
    (env-set-last-status! env status)
    status))

(def (format-time-bash secs)
  (let* ((mins (inexact->exact (floor (fl/ secs 60.0))))
         (remaining (fl- secs (fl* (exact->inexact mins) 60.0))))
    (format "~am~a.~as"
            mins
            (inexact->exact (floor remaining))
            (let ((frac (fl- remaining (flfloor remaining))))
              (let ((ms (inexact->exact (floor (fl* frac 1000.0)))))
                (string-append
                 (if (< ms 100) "0" "")
                 (if (< ms 10) "0" "")
                 (number->string ms)))))))

(def (format-time-posix secs)
  (let* ((mins (inexact->exact (floor (fl/ secs 60.0))))
         (remaining (fl- secs (fl* (exact->inexact mins) 60.0))))
    (format "~a.~a"
            mins
            (let ((cs (inexact->exact (floor (fl* remaining 100.0)))))
              (string-append
               (if (< cs 10) "0" "")
               (number->string cs))))))

;;; --- Helpers ---

(def (return-status env status)
  (env-set-last-status! env status)
  status)

(def (string-join-words words)
  (if (null? words) ""
      (call-with-output-string
        (lambda (port)
          (display (car words) port)
          (for-each (lambda (w)
                      (display " " port)
                      (display w port))
                    (cdr words))))))

;;; --- Coproc execution ---

;; coproc [NAME] command
;; Creates two pipes, launches command in background with its stdin/stdout
;; connected to the pipes. Sets ${NAME[0]} (parent reads) and ${NAME[1]}
;; (parent writes), and ${NAME_PID} to the child PID.
(def (execute-coproc cmd env)
  (let ((name (coproc-command-name cmd))
        (body (coproc-command-command cmd)))
    ;; Create two pipes: one for parent→child, one for child→parent
    ;; Pipe 1: parent writes → child reads (child stdin)
    ;; Pipe 2: child writes → parent reads (child stdout)
    (let-values (((p2c-read p2c-write) (ffi-pipe-raw))     ;; parent-to-child
                 ((c2p-read c2p-write) (ffi-pipe-raw)))    ;; child-to-parent
      ;; Save original fds 0 and 1
      (let ((saved-stdin (ffi-dup 0))
            (saved-stdout (ffi-dup 1)))
        ;; Redirect fd 0 to read-end of parent-to-child pipe
        (ffi-dup2 p2c-read 0)
        ;; Redirect fd 1 to write-end of child-to-parent pipe
        (ffi-dup2 c2p-write 1)

        ;; Launch the command in the background
        (let ((result
               (if (simple-command? body)
                 (launch-background-simple body env)
                 ;; Compound: run in thread with redirected fds
                 (let* ((thread-in-fd (ffi-dup 0))
                        (thread-out-fd (ffi-dup 1))
                        (exit-box (box 0)))
                   (let ((t (spawn
                             (lambda ()
                               (let ((in-port (open-input-file
                                               (string-append "/dev/fd/"
                                                              (number->string thread-in-fd))))
                                     (out-port (open-output-file
                                                (string-append "/dev/fd/"
                                                               (number->string thread-out-fd)))))
                                 (parameterize ((current-input-port in-port)
                                                (current-output-port out-port))
                                   (let ((status (execute-command body env)))
                                     (set-box! exit-box status)
                                     (force-output out-port)
                                     (close-port out-port)
                                     (ffi-close-fd thread-out-fd)
                                     (close-port in-port)
                                     (ffi-close-fd thread-in-fd))))))))
                     (cons 0 []))))))

          ;; Restore parent's fds 0 and 1
          (ffi-dup2 saved-stdin 0)
          (ffi-dup2 saved-stdout 1)
          (ffi-close-fd saved-stdin)
          (ffi-close-fd saved-stdout)

          ;; Close the pipe ends the child uses (parent doesn't need them)
          (ffi-close-fd p2c-read)
          (ffi-close-fd c2p-write)

          ;; Set the array variable: NAME[0] = read fd (from child), NAME[1] = write fd (to child)
          (env-array-set! env name 0 (number->string c2p-read))
          (env-array-set! env name 1 (number->string p2c-write))

          ;; Set NAME_PID
          (let ((pid (car result)))
            (env-set! env (string-append name "_PID") (number->string pid))
            (env-set-last-bg-pid! env pid)

            ;; Add to job table
            (let ((job (job-table-add! (cdr result)
                                       (string-append "coproc " (or (ast->command-text body)
                                                                    name))
                                       pid)))
              (when (*interactive-shell*)
                (fprintf (current-error-port) "[~a] ~a~n"
                         (job-id job) pid)))
            0))))))

;;; --- Background execution ---

;; Counter for fake PIDs for thread-based background jobs
(def *next-fake-pid* 100000)
(def (next-fake-pid!)
  (let ((pid *next-fake-pid*))
    (set! *next-fake-pid* (+ pid 1))
    pid))

;; Launch a command in the background.
;; Returns (pid . process-list) where process-list is for job-table-add!
;; For external commands: launches via open-process, returns real PID
;; For builtins/functions/compound: launches in a thread with fake PID
(def (launch-background cmd env)
  (if (simple-command? cmd)
    (launch-background-simple cmd env)
    ;; Compound command: run in thread (acts like subshell for exit/variable isolation)
    (let* ((fake-pid (next-fake-pid!))
           (child-env (env-clone env))
           (th (spawn (lambda ()
                        (with-catch
                         (lambda (e)
                           (cond
                             ((subshell-exit-exception? e) (subshell-exit-exception-status e))
                             ((errexit-exception? e) (errexit-exception-status e))
                             (else 1)))
                         (lambda ()
                           (parameterize ((*in-subshell* #t))
                             (execute-command cmd child-env))))))))
      (cons fake-pid [(cons fake-pid th)]))))

(def (launch-background-simple cmd env)
  (let* ((raw-words (simple-command-words cmd))
         ;; Quick peek at first word to decide external vs builtin/function
         ;; Use raw token value to avoid expansion side effects
         (first-raw (and (pair? raw-words) (if (token? (car raw-words))
                                             (token-value (car raw-words))
                                             (car raw-words))))
         ;; Only use raw value if it's a simple literal (no $ or ` that need expansion)
         (cmd-name (and first-raw (string? first-raw)
                        (not (string-index first-raw #\$))
                        (not (string-index first-raw #\`))
                        first-raw)))
    (if (and cmd-name
             (not (function-lookup env cmd-name))
             (not (builtin-lookup cmd-name)))
      ;; External command: launch without waiting via open-process
      ;; Expand args and set up env in parent (external command forks anyway)
      (let* ((assignments (simple-command-assignments cmd))
             (expanded (expand-words raw-words env))
             (actual-args (if (pair? expanded) (cdr expanded) []))
             (path (which cmd-name)))
        (if path
          (let* ((temp-env (if (pair? assignments)
                             (let ((child (env-push-scope env)))
                               (for-each
                                (lambda (asgn) (apply-assignment! asgn child))
                                assignments)
                               child)
                             env))
                 (exec-path (if (string-contains? path "/")
                              (path-expand path) path))
                 (_flush (begin (force-output) (force-output (current-error-port))))
                 (packed-argv (pack-with-soh
                               (map string->c-safe (cons cmd-name actual-args))))
                 (packed-env (pack-with-soh
                              (map string->c-safe (env-exported-alist temp-env))))
                 (keep-fds (pack-fds-with-soh (*active-redirect-fds*)))
                 (pid (ffi-fork-exec (string->c-safe exec-path)
                                     packed-argv packed-env
                                     -1  ;; pgid=-1: own process group
                                     (*gambit-scheduler-rfd*)
                                     (*gambit-scheduler-wfd*)
                                     keep-fds
                                     (current-directory))))
            ;; Unblock SIGCHLD — background jobs are reaped later via job-wait
            (ffi-sigchld-unblock)
            (if (< pid 0)
              ;; Fork failed — fall back to thread for error message
              (let* ((fake-pid (next-fake-pid!))
                     (th (spawn (lambda ()
                                  (fprintf (current-error-port) "gsh: ~a: fork failed~n" cmd-name)
                                  126))))
                (cons fake-pid [(cons fake-pid th)]))
              (cons pid [(cons pid #f)])))
          ;; Command not found — still launch in thread for error message
          (let* ((fake-pid (next-fake-pid!))
                 (child-env (env-clone env))
                 (th (spawn (lambda ()
                              (parameterize ((*in-subshell* #t))
                                (execute-command cmd child-env))))))
            (cons fake-pid [(cons fake-pid th)]))))
      ;; Builtin or function: run everything in thread (acts like subshell)
      ;; Expansion happens in the child so side effects don't leak to parent
      (let* ((fake-pid (next-fake-pid!))
             (child-env (env-clone env))
             (th (spawn (lambda ()
                          (with-catch
                           (lambda (e)
                             (cond
                               ((subshell-exit-exception? e) (subshell-exit-exception-status e))
                               ((errexit-exception? e) (errexit-exception-status e))
                               (else 1)))
                           (lambda ()
                             (parameterize ((*in-subshell* #t))
                               (execute-command cmd child-env))))))))
        (cons fake-pid [(cons fake-pid th)])))))

;; Extract command text from an AST for display in job listing
(def (ast->command-text cmd)
  (cond
    ((simple-command? cmd)
     (let ((words (simple-command-words cmd)))
       (if (pair? words)
         (string-join-words words)
         #f)))
    ((ast-pipeline? cmd)
     (let ((cmds (ast-pipeline-commands cmd)))
       (string-join-words
        (map (lambda (c) (or (ast->command-text c) "?")) cmds))))
    ((coproc-command? cmd)
     (string-append "coproc " (coproc-command-name cmd) " "
                    (or (ast->command-text (coproc-command-command cmd)) "...")))
    ((time-command? cmd)
     (string-append "time " (or (ast->command-text (time-command-pipeline cmd)) "")))
    (else #f)))
