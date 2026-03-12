;;; environment.ss — Shell variables & environment for gsh

(export #t)
(import :std/sugar
        :std/iter
        :std/format
        :std/sort
        :std/misc/hash
        :gsh/ffi)

;;; --- Callback parameter for execute-input ---
;;; Breaks circular dependency: builtins.ss and expander.ss need to call
;;; execute-input (defined in main.ss), but main.ss imports them.
;;; Solution: store it as a parameter, set it from main.ss at startup.
(def *execute-input* (make-parameter #f))

;;; --- Callback parameter for arithmetic evaluation ---
;;; Used by apply-var-attrs when integer attribute is set.
;;; Set from main.ss at startup to arith-eval.
(def *arith-eval-fn* (make-parameter #f))

;;; --- Interactive shell flag ---
;;; When #t, job notifications and other interactive-only output is enabled.
(def *interactive-shell* (make-parameter #f))

;;; --- Callback for processing pending signal traps between commands ---
;;; Set from main.ss at startup.
(def *process-traps-fn* (make-parameter #f))

;;; --- Current source file tracking (for extdebug) ---
(def *current-source-file* (make-parameter "main"))

;;; --- Condition context for errexit suppression ---
;;; When #t, errexit (set -e) does not trigger on command failure.
;;; Set to #t in: if-test, while/until-test, && / || LHS, ! prefix.
(def *in-condition-context* (make-parameter #f))

;;; --- Subshell context ---
;;; When #t, `exit` raises an exception instead of terminating the process.
(def *in-subshell* (make-parameter #f))

;;; --- Pipeline raw stdin fd ---
;;; When non-#f, holds the raw fd for the pipeline read-end.
;;; Used by builtins (read, mapfile) to read via fdread instead of
;;; Gambit character port (which doesn't handle pipe EOF properly).
(def *pipeline-stdin-fd* (make-parameter #f))

;;; When non-#f, holds the raw fd for the pipeline write-end.
;;; Used by execute-external to dup2 onto fd 1 before open-process
;;; so external commands in pipeline threads write to the pipe.
(def *pipeline-stdout-fd* (make-parameter #f))

;;; --- Gambit scheduler pipe fds ---
;;; Cached after move-internal-fds-high! so ffi-fork-exec can close them in child.
(def *gambit-scheduler-rfd* (make-parameter -1))
(def *gambit-scheduler-wfd* (make-parameter -1))

;;; --- Active redirect fds ---
;;; List of fd numbers that are currently active redirections.
;;; ffi-fork-exec uses this to know which fds the child needs to keep.
(def *active-redirect-fds* (make-parameter []))

;;; --- Internal logical PWD ---
;;; Tracks the shell's logical working directory independently of $PWD.
;;; This ensures that `pwd` returns the correct path even if the user
;;; manually sets $PWD to garbage. Only updated by cd builtin and init.
(def *internal-pwd* (make-parameter #f))

;;; --- Shell variable ---
;; Attributes: exported?, readonly?, local?, integer?, uppercase?, lowercase?, nameref?
;;             array?, assoc?
;; For scalar vars, value is a string.
;; For indexed arrays (array? = #t), value is a hash-table with integer keys.
;; For associative arrays (assoc? = #t), value is a hash-table with string keys.
(defstruct shell-var (value exported? readonly? local?
                      integer? uppercase? lowercase? nameref?
                      array? assoc?) transparent: #t)

;; Sentinel value for explicitly unset variables — prevents fallback to parent/OS env
(def +unset-sentinel+ (gensym 'unset))

;;; --- Shell environment ---
(defclass shell-environment (vars        ;; hash-table: name -> shell-var
                             parent      ;; parent environment or #f
                             functions   ;; hash-table: name -> function-def
                             aliases     ;; hash-table: name -> string
                             options     ;; hash-table: option-name -> bool
                             shopts      ;; hash-table: shopt-name -> bool
                             positional  ;; vector of positional params ($1..$N)
                             last-status ;; $? - exit status of last command
                             last-bg-pid ;; $! - PID of last background process
                             shell-pid   ;; $$ - PID of the shell
                             shell-name  ;; $0
                             start-time  ;; for $SECONDS
                             cmd-number  ;; for \# in prompt
                             traps       ;; hash-table: signal-name -> action
                             dir-stack   ;; list of directories (pushd/popd)
                             )
  constructor: :init!
  transparent: #t)

(defmethod {:init! shell-environment}
  (lambda (self parent: (parent #f) name: (name "gsh"))
    (set! self.vars (make-hash-table))
    (set! self.parent parent)
    (set! self.functions (if parent (shell-environment-functions parent) (make-hash-table)))
    (set! self.aliases (if parent (shell-environment-aliases parent) (make-hash-table)))
    (set! self.options (if parent (shell-environment-options parent) (make-hash-table)))
    (set! self.shopts (if parent (shell-environment-shopts parent) (make-hash-table)))
    (set! self.positional (if parent (shell-environment-positional parent) (vector)))
    (set! self.last-status 0)
    (set! self.last-bg-pid 0)
    (set! self.shell-pid (if parent (shell-environment-shell-pid parent) (##os-getpid)))
    (set! self.shell-name name)
    (set! self.start-time (if parent (shell-environment-start-time parent) (time->seconds (current-time))))
    (set! self.cmd-number (if parent (shell-environment-cmd-number parent) 0))
    (set! self.traps (if parent (shell-environment-traps parent) (make-hash-table)))
    (set! self.dir-stack (if parent (shell-environment-dir-stack parent) []))))

;;; --- Nameref resolution ---

;; Resolve a nameref chain: if `name` is a nameref variable, follow the chain
;; until we find a non-nameref (or hit the cycle guard). Returns the final name.
(def (resolve-nameref name env (depth 0))
  (if (> depth 10) name  ;; cycle guard — matches bash behavior
    (let ((var (find-var-in-chain env name)))
      (if (and var (shell-var-nameref? var) (string? (shell-var-value var)))
        (resolve-nameref (shell-var-value var) env (+ depth 1))
        name))))

;;; --- Variable operations ---

;; Get a variable's value, checking scope chain
(def (env-get env name)
  (cond
    ;; Special variables
    ((string=? name "?") (number->string (shell-environment-last-status env)))
    ((string=? name "$") (number->string (shell-environment-shell-pid env)))
    ((string=? name "!") (number->string (shell-environment-last-bg-pid env)))
    ((string=? name "#") (number->string (vector-length (shell-environment-positional env))))
    ((string=? name "@") (let ((pos (env-positional-list env)))
                           (if (null? pos) #f (string-join-with " " pos))))
    ((string=? name "*") (let ((pos (env-positional-list env)))
                           (if (null? pos) #f (env-star env))))
    ((string=? name "0") (shell-environment-shell-name env))
    ((string=? name "-") (options->flag-string env))
    ((string=? name "_") (env-get-local env "_"))  ;; last arg of prev command
    ((string=? name "RANDOM") (number->string (random-integer 32768)))
    ((string=? name "SECONDS")
     (number->string (inexact->exact
                      (floor (- (time->seconds (current-time))
                                (shell-environment-start-time env))))))
    ((string=? name "LINENO") (or (env-get-local env "LINENO") "0"))
    ;; Positional parameters $1..$N
    ((positional-index name)
     => (lambda (idx)
          (let ((pos (shell-environment-positional env)))
            (if (and (> idx 0) (<= idx (vector-length pos)))
              (vector-ref pos (- idx 1))
              ""))))
    ;; Regular variable lookup — resolve namerefs first
    (else
     (let ((resolved (resolve-nameref name env)))
       ;; If the resolved name is still a nameref, we hit a cycle — treat as unset
       (let ((var (find-var-in-chain env resolved)))
         (if (and var (shell-var-nameref? var))
           #f
           (env-get-chain env resolved)))))))

;; Get from local scope only
(def (env-get-local env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (and var (not (eq? (shell-var-value var) +unset-sentinel+))
         (shell-var-scalar-value var))))

;; Get from scope chain
(def (env-get-chain env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (if var
      ;; Check for unset sentinel — variable was explicitly unset, don't fall back
      (if (eq? (shell-var-value var) +unset-sentinel+)
        #f
        (shell-var-scalar-value var))
      (let ((parent (shell-environment-parent env)))
        (if parent
          (env-get-chain parent name)
          ;; Fall back to OS environment
          (getenv name #f))))))

;; Get the scalar value of a shell-var.
;; For arrays, returns element [0] (bash behavior: $arr == ${arr[0]}).
;; Returns #f if element [0] doesn't exist (e.g. a=() or a=([1]=x)).
(def (shell-var-scalar-value var)
  (let ((v (shell-var-value var)))
    (if (hash-table? v)
      (or (hash-get v 0) (hash-get v "0"))
      v)))

;; Apply variable attributes (integer, uppercase, lowercase) to a value
;; env is optional — when provided, arithmetic evaluation can reference variables
(def (apply-var-attrs var value (env #f))
  (let ((v (if (shell-var-integer? var)
             (let ((arith-fn (*arith-eval-fn*)))
               (if arith-fn
                 ;; Use arithmetic evaluation (handles expressions like "2+3")
                 (number->string
                  (arith-fn value
                            (if env
                              (lambda (name) (or (env-get env name) "0"))
                              (lambda (name) "0"))
                            (if env
                              (lambda (name val) (env-set! env name (number->string val)))
                              (lambda (name val) #!void))))
                 ;; Fallback: simple string->number
                 (let ((n (or (string->number value) 0)))
                   (number->string n))))
             value)))
    (cond
      ((shell-var-uppercase? var) (string-upcase v))
      ((shell-var-lowercase? var) (string-downcase v))
      (else v))))

;; Look up the shell-var struct from scope chain (not value, the struct itself)
(def (env-get-var env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (cond
      ((and var (eq? (shell-var-value var) +unset-sentinel+)) #f)
      (var var)
      (else
       (let ((parent (shell-environment-parent env)))
         (if parent (env-get-var parent name) #f))))))

;; Like env-get-var but also returns declared-but-unset variables (sentinel value)
(def (env-get-raw-var env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (cond
      (var var)
      (else
       (let ((parent (shell-environment-parent env)))
         (if parent (env-get-raw-var parent name) #f))))))

;; List all variable names matching a prefix, sorted.
;; Walks scope chain and OS environment.
(def (env-matching-names env prefix)
  (let ((result (make-hash-table)))
    ;; Walk scope chain
    (let walk ((e env))
      (when e
        (for ([k . v] (hash->list (shell-environment-vars e)))
          (when (and (string? k)
                     (string-prefix? prefix k)
                     (not (eq? (shell-var-value v) +unset-sentinel+)))
            (hash-put! result k #t)))
        (walk (shell-environment-parent e))))
    ;; Also check OS environment
    (for-each
     (lambda (pair)
       (when (string-prefix? prefix (car pair))
         (hash-put! result (car pair) #t)))
     (get-environment-variables))
    (sort! (hash-keys result) string<?)))

;; Set a variable — respects scope chain for non-local vars
;; Resolves namerefs: if `name` is a nameref, sets the target variable instead.
(def (env-set! env name value)
  (let* ((resolved (resolve-nameref name env))
         (existing (hash-get (shell-environment-vars env) resolved)))
    (cond
      ((and existing (shell-var-readonly? existing))
       (error (format "~a: readonly variable" name)))
      (existing
       (let ((final-value (apply-var-attrs existing value env)))
         (set! (shell-var-value existing) final-value)
         ;; If exported, also update OS env
         (when (shell-var-exported? existing)
           (setenv resolved final-value))))
      (else
       ;; Not in local scope — check parent chain
       (let ((owner (find-var-owner-in-chain (shell-environment-parent env) resolved)))
         (if owner
           ;; Variable exists in an ancestor scope — modify it there
           (let ((parent-var (hash-get (shell-environment-vars owner) resolved)))
             (when (and parent-var (shell-var-readonly? parent-var))
               (error (format "~a: readonly variable" resolved)))
             (let ((final-value (apply-var-attrs parent-var value env)))
               (set! (shell-var-value parent-var) final-value)
               (when (shell-var-exported? parent-var)
                 (setenv resolved final-value))))
           ;; Variable doesn't exist anywhere in the chain
           ;; Create in ROOT scope (bash behavior: vars in functions are global)
           (let* ((root (env-root env))
                  (os-val (getenv resolved #f))
                  (export? (or os-val (env-option? env "allexport"))))
             (if export?
               ;; Exists in OS env or allexport is on — create as exported
               (begin
                 (hash-put! (shell-environment-vars root) resolved
                            (make-shell-var value #t #f #f #f #f #f #f #f #f))
                 (setenv resolved value))
               (hash-put! (shell-environment-vars root) resolved
                          (make-shell-var value #f #f #f #f #f #f #f #f #f))))))))
    ;; If allexport is on, ensure the variable is exported
    (when (env-option? env "allexport")
      (let ((var (hash-get (shell-environment-vars env) resolved)))
        (when (and var (not (shell-var-exported? var)))
          (set! (shell-var-exported? var) #t)
          (when (shell-var-value var)
            (setenv resolved (shell-var-value var))))))))

;; Set the shell name ($0)
(def (env-set-shell-name! env name)
  (shell-environment-shell-name-set! env name))

;; Mark variable as exported
(def (env-export! env name (value #f))
  (when value (env-set! env name value))
  ;; Find the var wherever env-set! placed it (may be in parent/root scope)
  (let ((var (env-get-raw-var env name)))
    (if var
      (begin
        ;; Arrays cannot be exported — silently ignore (bash behavior)
        (when (or (shell-var-array? var) (shell-var-assoc? var))
          (set! (shell-var-exported? var) #f)
          (ffi-unsetenv name))
        (unless (or (shell-var-array? var) (shell-var-assoc? var))
          (set! (shell-var-exported? var) #t)
          (let ((v (shell-var-scalar-value var)))
            (when v (setenv name v)))))
      ;; No var exists anywhere — create exported in current scope
      (if value
        (begin
          (hash-put! (shell-environment-vars env) name
                     (make-shell-var value #t #f #f #f #f #f #f #f #f))
          (setenv name value))
        ;; export with no value: create unset-but-exported marker
        (hash-put! (shell-environment-vars env) name
                   (make-shell-var +unset-sentinel+ #t #f #f #f #f #f #f #f #f))))))

;; Unset a variable — resolves namerefs (unsets the target, not the ref)
;; Use env-unset-nameref! to unset the nameref itself.
(def (env-unset! env name)
  (let ((resolved (resolve-nameref name env)))
    ;; Walk the scope chain to find the nearest scope that actually has this variable
    ;; and remove it from THAT scope (bash dynamic-scope unset semantics)
    (let scope-loop ((e env))
      (if (not e)
        ;; Variable not found in any scope — just unsetenv if in OS env
        (when (getenv resolved #f)
          (ffi-unsetenv resolved))
        (let ((var (hash-get (shell-environment-vars e) resolved)))
          (if var
            (begin
              (when (shell-var-readonly? var)
                (error (format "~a: readonly variable" resolved)))
              ;; Remove from OS environment if it was exported
              (when (or (shell-var-exported? var)
                        (getenv resolved #f))
                (ffi-unsetenv resolved))
              ;; Remove from this scope — lookup will now fall through to parent
              (hash-remove! (shell-environment-vars e) resolved))
            ;; Not in this scope, check parent
            (scope-loop (shell-environment-parent e))))))))

;; Unset a nameref variable itself (not the target) — used by `unset -n`
(def (env-unset-nameref! env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (when (and var (shell-var-readonly? var))
      (error (format "~a: readonly variable" name)))
    (hash-remove! (shell-environment-vars env) name)))

;; Mark variable as readonly
(def (env-readonly! env name (value #f))
  (when value (env-set! env name value))
  ;; Find the var wherever env-set! placed it (may be in parent/root scope)
  (let ((var (env-get-raw-var env name)))
    (when var
      (set! (shell-var-readonly? var) #t))))

;; Get an alist of all exported variables for passing to child processes
(def (env-exported-alist env)
  (let ((result (make-hash-table)))
    ;; Start with OS environment
    (for-each (lambda (pair) (hash-put! result (car pair) (cdr pair)))
              (get-environment-variables))
    ;; Override with our exported vars
    (env-collect-exports! env result)
    (hash-map (lambda (k v) (string-append k "=" v)) result)))

;; Collect exported variables from scope chain into a hash table
(def (env-collect-exports! env target)
  (let ((parent (shell-environment-parent env)))
    (when parent (env-collect-exports! parent target)))
  (for ([name . var] (hash->list (shell-environment-vars env)))
    (when (shell-var-exported? var)
      (let ((v (shell-var-value var)))
        (unless (eq? v +unset-sentinel+)
          (hash-put! target name (or (shell-var-scalar-value var) "")))))))

;; Push a new scope (for function calls)
(def (env-push-scope env)
  (make-shell-environment parent: env name: (shell-environment-shell-name env)))

;; Deep clone an environment (for subshells)
;; Creates a fully independent copy — mutations don't affect the original.
(def (env-clone env)
  (let ((clone (make-shell-environment name: (shell-environment-shell-name env))))
    ;; Deep copy all hash tables so mutations are isolated
    (set! (shell-environment-vars clone) (clone-var-table env))
    (set! (shell-environment-functions clone) (hash-copy (shell-environment-functions env)))
    (set! (shell-environment-aliases clone) (hash-copy (shell-environment-aliases env)))
    (set! (shell-environment-options clone) (hash-copy (shell-environment-options env)))
    (set! (shell-environment-shopts clone) (hash-copy (shell-environment-shopts env)))
    (set! (shell-environment-positional clone) (vector-copy (shell-environment-positional env)))
    (set! (shell-environment-last-status clone) (shell-environment-last-status env))
    (set! (shell-environment-last-bg-pid clone) (shell-environment-last-bg-pid env))
    (set! (shell-environment-shell-pid clone) (shell-environment-shell-pid env))
    (set! (shell-environment-start-time clone) (shell-environment-start-time env))
    (set! (shell-environment-cmd-number clone) (shell-environment-cmd-number env))
    (set! (shell-environment-traps clone) (hash-copy (shell-environment-traps env)))
    (set! (shell-environment-dir-stack clone) (shell-environment-dir-stack env))
    clone))

;; Clone the var table from full scope chain into a flat hash table
(def (clone-var-table env)
  (let ((result (make-hash-table)))
    (clone-vars-from-chain! env result)
    result))

(def (clone-vars-from-chain! env target)
  (let ((parent (shell-environment-parent env)))
    (when parent (clone-vars-from-chain! parent target)))
  (for ([name . var] (hash->list (shell-environment-vars env)))
    (hash-put! target name
               (make-shell-var (let ((v (shell-var-value var)))
                                 ;; Deep-copy array hash-tables for subshell isolation
                                 (if (hash-table? v) (hash-copy v) v))
                               (shell-var-exported? var)
                               (shell-var-readonly? var)
                               (shell-var-local? var)
                               (shell-var-integer? var)
                               (shell-var-uppercase? var)
                               (shell-var-lowercase? var)
                               (shell-var-nameref? var)
                               (shell-var-array? var)
                               (shell-var-assoc? var)))))

;; Pop back to parent scope
(def (env-pop-scope env)
  (or (shell-environment-parent env)
      (error "cannot pop root environment")))

;;; --- Positional parameters ---

(def (env-set-positional! env args)
  (set! (shell-environment-positional env) (list->vector args)))

(def (env-positional-list env)
  (vector->list (shell-environment-positional env)))

;; $* — all positional params as single string separated by first char of IFS
(def (env-star env)
  (let* ((pos (env-positional-list env))
         (ifs (or (env-get env "IFS") " \t\n"))
         (sep (if (> (string-length ifs) 0)
                (string (string-ref ifs 0))
                "")))
    (string-join-with sep pos)))

;; $@ — all positional params as separate words
(def (env-at env)
  (env-positional-list env))

;;; --- Shell options ---

(def (env-option-set! env name value)
  (hash-put! (shell-environment-options env) name value))

(def (env-option? env name)
  (hash-get (shell-environment-options env) name))

(def (env-all-options env)
  ;; Return known options with their status
  (let ((opts (shell-environment-options env))
        (known '("allexport" "braceexpand" "emacs" "errexit" "errtrace"
                  "functrace" "hashall" "histexpand" "interactive-comments"
                  "keyword" "monitor" "noclobber" "noexec" "noglob"
                  "nolog" "notify" "nounset" "onecmd" "physical"
                  "pipefail" "posix" "privileged" "verbose" "vi" "xtrace")))
    (map (lambda (name)
           (cons name (and (hash-get opts name) #t)))
         known)))

(def (env-all-variables env)
  ;; Return all scalar variables sorted by name
  (let ((result []))
    (for ([name . var] (hash->list (shell-environment-vars env)))
      (when (and (shell-var? var)
                 (not (shell-var-array? var))
                 (not (shell-var-assoc? var))
                 (shell-var-value var))
        (set! result (cons (cons name (shell-var-value var)) result))))
    (sort result (lambda (a b) (string<? (car a) (car b))))))

(def (env-shopt-set! env name value)
  (hash-put! (shell-environment-shopts env) name value))

(def (env-shopt? env name)
  (hash-get (shell-environment-shopts env) name))

;;; --- Set last status ---

(def (env-set-last-status! env status)
  ;; Truncate to 8 bits (0-255) per POSIX
  (set! (shell-environment-last-status env) (bitwise-and status #xFF)))

(def (env-set-last-bg-pid! env pid)
  (set! (shell-environment-last-bg-pid env) pid))

(def (env-inc-cmd-number! env)
  (set! (shell-environment-cmd-number env)
        (+ 1 (shell-environment-cmd-number env))))

;;; --- Initialize environment from OS ---

(def (env-init! env)
  ;; Import all environment variables
  (for-each
   (lambda (pair)
     (hash-put! (shell-environment-vars env) (car pair)
                (make-shell-var (cdr pair) #t #f #f #f #f #f #f #f #f)))
   (get-environment-variables))
  ;; Set defaults if not present
  (unless (env-get-local env "IFS")
    (env-set! env "IFS" " \t\n"))
  (unless (env-get-local env "PS1")
    (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
  (unless (env-get-local env "PS2")
    (env-set! env "PS2" "> "))
  (unless (env-get-local env "PS4")
    (env-set! env "PS4" "+ "))
  (unless (env-get-local env "HISTFILE")
    (env-set! env "HISTFILE" (string-append (or (getenv "HOME" #f) ".") "/.gsh_history")))
  (unless (env-get-local env "HISTSIZE")
    (env-set! env "HISTSIZE" "1000"))
  (unless (env-get-local env "HISTFILESIZE")
    (env-set! env "HISTFILESIZE" "2000"))
  ;; Set PWD — use inherited PWD if it points to the same directory, else use physical path
  ;; Strip trailing slash (except for root /)
  ;; Validate inherited PWD by comparing device+inode to current directory
  (let* ((inherited-pwd (getenv "PWD" #f))
         (physical-pwd (let ((p (current-directory)))
                         (let ((len (string-length p)))
                           (if (and (> len 1) (char=? (string-ref p (- len 1)) #\/))
                             (substring p 0 (- len 1))
                             p))))
         (valid-inherited?
          (and inherited-pwd
               (> (string-length inherited-pwd) 0)
               (char=? (string-ref inherited-pwd 0) #\/)
               (with-catch
                (lambda (e) #f)
                (lambda ()
                  (let ((a (file-info inherited-pwd))
                        (b (file-info physical-pwd)))
                    (and (= (file-info-device a) (file-info-device b))
                         (= (file-info-inode a) (file-info-inode b)))))))))
    (let ((pwd-val (if valid-inherited? inherited-pwd physical-pwd)))
      (env-set! env "PWD" pwd-val)
      (*internal-pwd* pwd-val)))
  ;; Set SHLVL
  (let* ((shlvl-str (or (getenv "SHLVL" #f) "0"))
         (shlvl (or (string->number shlvl-str) 0)))
    (env-set! env "SHLVL" (number->string (+ shlvl 1)))
    (env-export! env "SHLVL"))
  ;; Set PPID
  (env-set! env "PPID" (number->string (##os-getppid)))
  ;; Set shell-specific vars
  (env-set! env "SHELL" (or (getenv "SHELL" #f) "/bin/gsh"))
  ;; Default shell options
  (env-option-set! env "hashall" #t)
  (env-option-set! env "braceexpand" #t)
  (env-option-set! env "interactive-comments" #t)
  ;; Default shopt options
  (env-shopt-set! env "cmdhist" #t)
  (env-shopt-set! env "complete_fullquote" #t)
  (env-shopt-set! env "extquote" #t)
  (env-shopt-set! env "force_fignore" #t)
  (env-shopt-set! env "hostcomplete" #t)
  (env-shopt-set! env "interactive_comments" #t)
  (env-shopt-set! env "progcomp" #t)
  (env-shopt-set! env "promptvars" #t)
  (env-shopt-set! env "sourcepath" #t)
  (env-shopt-set! env "globskipdots" #t)
  ;; Set terminal color variables
  (set-color-vars! env))

;;; --- Terminal color variables ---
;;; Pre-defined ANSI escape sequences available as shell variables.
;;; Usage in ~/.gshrc: PS1='\[$_fg_norm_green\]\u@\h\[$_ansi_reset\]:\w\$ '
(def (set-color-vars! env)
  (let* ((esc (string (integer->char 27)))
         (csi (string-append esc "[")))
    (def (ansi code) (string-append csi code "m"))
    (for-each
      (lambda (pair) (env-set! env (car pair) (cdr pair)))
      `(;; Reset & attributes
        ("_ansi_reset" . ,(ansi "0"))
        ("_ansi_bold"  . ,(ansi "1"))
        ;; Foreground — normal
        ("_fg_norm_black_"   . ,(ansi "30"))
        ("_fg_norm_red"      . ,(ansi "31"))
        ("_fg_norm_green"    . ,(ansi "32"))
        ("_fg_norm_yellow"   . ,(ansi "33"))
        ("_fg_norm_blue"     . ,(ansi "34"))
        ("_fg_norm_magenta"  . ,(ansi "35"))
        ("_fg_norm_cyan"     . ,(ansi "36"))
        ("_fg_norm_white"    . ,(ansi "37"))
        ;; Foreground — bright
        ("_fg_bright_black"   . ,(ansi "90"))
        ("_fg_bright_red"     . ,(ansi "91"))
        ("_fg_bright_green"   . ,(ansi "92"))
        ("_fg_bright_yellow"  . ,(ansi "93"))
        ("_fg_bright_blue"    . ,(ansi "94"))
        ("_fg_bright_magenta" . ,(ansi "95"))
        ("_fg_bright_cyan"    . ,(ansi "96"))
        ("_fg_bright_white"   . ,(ansi "97"))
        ;; Background — normal
        ("_bg_norm_black_"   . ,(ansi "40"))
        ("_bg_norm_red"      . ,(ansi "41"))
        ("_bg_norm_green"    . ,(ansi "42"))
        ("_bg_norm_yellow"   . ,(ansi "43"))
        ("_bg_norm_blue"     . ,(ansi "44"))
        ("_bg_norm_magenta"  . ,(ansi "45"))
        ("_bg_norm_cyan"     . ,(ansi "46"))
        ("_bg_norm_white"    . ,(ansi "47"))
        ;; Background — bright
        ("_bg_bright_black"   . ,(ansi "100"))
        ("_bg_bright_red"     . ,(ansi "101"))
        ("_bg_bright_green"   . ,(ansi "102"))
        ("_bg_bright_yellow"  . ,(ansi "103"))
        ("_bg_bright_blue"    . ,(ansi "104"))
        ("_bg_bright_magenta" . ,(ansi "105"))
        ("_bg_bright_cyan"    . ,(ansi "106"))
        ("_bg_bright_white"   . ,(ansi "107"))))))

;;; --- Helpers ---

(def (positional-index name)
  (and (> (string-length name) 0)
       (let loop ((i 0))
         (if (>= i (string-length name))
           (string->number name)
           (and (char-numeric? (string-ref name i))
                (loop (+ i 1)))))))

(def (find-var-in-chain env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (cond
      ((and var (eq? (shell-var-value var) +unset-sentinel+)) #f)
      (var var)
      (else
       (let ((parent (shell-environment-parent env)))
         (and parent (find-var-in-chain parent name)))))))

;; Like find-var-in-chain but also returns vars with unset sentinel
;; (used for write operations that need to find the var struct to mutate)
(def (find-var-in-chain-for-write env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (cond
      (var var)  ;; return even if unset sentinel
      (else
       (let ((parent (shell-environment-parent env)))
         (and parent (find-var-in-chain-for-write parent name)))))))

;; Get the root (outermost) environment scope
(def (env-root env)
  (let ((parent (shell-environment-parent env)))
    (if parent (env-root parent) env)))

;; Find the environment scope that owns a variable (for setting in parent chain)
(def (find-var-owner-in-chain env name)
  (and env
       (let ((var (hash-get (shell-environment-vars env) name)))
         (cond
           ((and var (eq? (shell-var-value var) +unset-sentinel+)) #f)
           (var env)
           (else (find-var-owner-in-chain (shell-environment-parent env) name))))))

(def (options->flag-string env)
  (let ((flags []))
    (when (env-option? env "errexit") (set! flags (cons #\e flags)))
    (when (env-option? env "noglob") (set! flags (cons #\f flags)))
    (when (env-option? env "hashall") (set! flags (cons #\h flags)))
    (when (env-option? env "monitor") (set! flags (cons #\m flags)))
    (when (env-option? env "noclobber") (set! flags (cons #\C flags)))
    (when (env-option? env "nounset") (set! flags (cons #\u flags)))
    (when (env-option? env "xtrace") (set! flags (cons #\x flags)))
    (when (env-option? env "verbose") (set! flags (cons #\v flags)))
    (list->string (reverse flags))))

(def (string-join-with sep lst)
  (if (null? lst) ""
      (call-with-output-string
        (lambda (port)
          (display (car lst) port)
          (for-each (lambda (s)
                      (display sep port)
                      (display s port))
                    (cdr lst))))))

;;; --- Array operations ---

;; Get an array element by index.
;; For indexed arrays, index should be an integer.
;; For assoc arrays, index should be a string.
(def (env-array-get env name index)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (if (and var (or (shell-var-array? var) (shell-var-assoc? var)))
      (let* ((tbl (shell-var-value var))
             (raw-key (if (shell-var-assoc? var)
                        index  ;; string key for assoc
                        (if (string? index) (or (string->number index) 0) index)))
             ;; Handle negative indexing for indexed arrays: -1 = last, -2 = second-to-last
             (key (if (and (not (shell-var-assoc? var)) (number? raw-key) (< raw-key 0))
                    (let ((max-key (hash-fold (lambda (k v mx) (if (> k mx) k mx)) -1 tbl)))
                      (+ max-key 1 raw-key))
                    raw-key)))
        (or (hash-get tbl key) ""))
      ;; Not an array — if index is 0, return scalar value
      (if (and var (or (equal? index "0") (equal? index 0)))
        (or (shell-var-scalar-value var) "")
        ""))))

;; Check if an array element is set (distinct from "empty").
;; Returns #t if the element exists in the array.
(def (env-array-element-set? env name index)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (cond
      ((not var) #f)
      ((or (shell-var-array? var) (shell-var-assoc? var))
       (let* ((tbl (shell-var-value var))
              (raw-key (if (shell-var-assoc? var)
                         index
                         (if (string? index) (or (string->number index) 0) index)))
              (key (if (and (not (shell-var-assoc? var)) (number? raw-key) (< raw-key 0))
                     (let ((max-key (hash-fold (lambda (k v mx) (if (> k mx) k mx)) -1 tbl)))
                       (+ max-key 1 raw-key))
                     raw-key)))
         (hash-key? tbl key)))
      ;; Scalar treated as single-element array at index 0
      ((or (equal? index "0") (equal? index 0))
       (not (eq? (shell-var-value var) +unset-sentinel+)))
      (else #f))))

;; Set an array element.
;; Creates the array if it doesn't exist.
(def (env-array-set! env name index value)
  (let* ((name (resolve-nameref name env))
         ;; Use for-write to find vars with unset sentinel (e.g. local x; x[3]=foo)
         (var (find-var-in-chain-for-write env name)))
    (cond
      ((and var (shell-var-readonly? var))
       (error (format "~a: readonly variable" name)))
      ((and var (or (shell-var-array? var) (shell-var-assoc? var)))
       ;; Existing array — set element
       (let* ((tbl (shell-var-value var))
              (raw-key (if (shell-var-assoc? var)
                         index
                         (if (string? index) (or (string->number index) 0) index)))
              ;; Handle negative indexing for indexed arrays
              (key (if (and (not (shell-var-assoc? var)) (number? raw-key) (< raw-key 0))
                     (let ((max-key (hash-fold (lambda (k v mx) (if (> k mx) k mx)) -1 tbl)))
                       (+ max-key 1 raw-key))
                     raw-key))
              (final-val (apply-var-attrs var value env)))
         (hash-put! tbl key final-val)))
      (var
       ;; Existing scalar (or unset-sentinel) — convert to indexed array
       (let ((tbl (make-hash-table))
             (key (if (string? index) (or (string->number index) 0) index)))
         (hash-put! tbl key (apply-var-attrs var value env))
         (set! (shell-var-value var) tbl)
         (set! (shell-var-array? var) #t)))
      (else
       ;; New variable — create indexed array in root scope
       (let* ((root (env-root env))
              (tbl (make-hash-table))
              (key (if (string? index) (or (string->number index) 0) index))
              (new-var (make-shell-var tbl #f #f #f #f #f #f #f #t #f)))
         (hash-put! tbl key value)
         (hash-put! (shell-environment-vars root) name new-var))))))

;; Get all values of an array (sorted by key for indexed arrays)
(def (env-array-values env name)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (if (and var (or (shell-var-array? var) (shell-var-assoc? var)))
      (let ((tbl (shell-var-value var)))
        (if (shell-var-assoc? var)
          ;; Assoc: values in insertion order (hash-table order)
          (hash-values tbl)
          ;; Indexed: values sorted by numeric key
          (let ((keys (sort! (hash-keys tbl) <)))
            (map (lambda (k) (hash-get tbl k)) keys))))
      ;; Not an array — return single value as list
      (if var [(shell-var-scalar-value var)] []))))

;; Get all keys/indices of an array
(def (env-array-keys env name)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (if (and var (or (shell-var-array? var) (shell-var-assoc? var)))
      (let ((tbl (shell-var-value var)))
        (if (shell-var-assoc? var)
          (map (lambda (k) (if (string? k) k (number->string k))) (hash-keys tbl))
          (let ((keys (sort! (hash-keys tbl) <)))
            (map number->string keys))))
      ;; Not an array — return ("0") if set
      (if (and var (shell-var-scalar-value var)) ["0"] []))))

;; Get array length (number of elements)
(def (env-array-length env name)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (if (and var (or (shell-var-array? var) (shell-var-assoc? var)))
      (hash-length (shell-var-value var))
      ;; Scalar — length is 1 if set, 0 if not
      (if (and var (shell-var-scalar-value var)) 1 0))))

;; Unset an array element
(def (env-array-unset-element! env name index)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain-for-write env name)))
    (when (and var (shell-var-readonly? var))
      (error (format "~a: readonly variable" name)))
    (cond
      ((not var) (void))  ;; variable doesn't exist - no-op
      ((or (shell-var-array? var) (shell-var-assoc? var))
       ;; Unset array element
       (let* ((tbl (shell-var-value var))
              (raw-key (if (shell-var-assoc? var)
                         index
                         ;; Use arith-eval for indexed array subscripts
                         (if (string? index)
                           (let ((n (string->number index)))
                             (if n n
                               ;; Try arithmetic evaluation for expressions like "i - 1"
                               (with-catch
                                (lambda (e) 0)
                                (lambda ()
                                  (let ((arith-fn (*arith-eval-fn*)))
                                    (if arith-fn
                                      (arith-fn index
                                                (lambda (nm) (or (env-get env nm) "0"))
                                                (lambda (nm val) (env-set! env nm (number->string val))))
                                      0))))))
                           index)))
              ;; Handle negative indexing: -1 = last, -2 = second-to-last, etc.
              (key (if (and (not (shell-var-assoc? var)) (number? raw-key) (< raw-key 0))
                     (let ((max-key (hash-fold (lambda (k v mx) (if (> k mx) k mx)) -1 tbl)))
                       (+ max-key 1 raw-key))
                     raw-key)))
         (hash-remove! tbl key)))
      (else
       ;; Variable exists but is not an array — error
       (error (format "~a: not an array variable" name))))))

;; Set a compound array value: arr=(val1 val2 val3)
;; For indexed arrays, assigns val1 to [0], val2 to [1], etc.
;; For items of the form [key]=val, uses explicit key.
(def (env-array-set-compound! env name values assoc?)
  (let* ((name (resolve-nameref name env))
         (var (or (find-var-in-chain env name)
                 (let ((new-var (make-shell-var (make-hash-table) #f #f #f #f #f #f #f
                                               (not assoc?) assoc?)))
                   (hash-put! (shell-environment-vars (env-root env)) name new-var)
                   new-var))))
    (when (shell-var-readonly? var)
      (error (format "~a: readonly variable" name)))
    ;; Reset to a fresh hash-table
    (let ((tbl (make-hash-table)))
      (set! (shell-var-value var) tbl)
      (set! (shell-var-array? var) (not assoc?))
      (set! (shell-var-assoc? var) assoc?)
      ;; Fill in values
      (let loop ((vals values) (auto-idx 0))
        (when (pair? vals)
          (let ((v (car vals)))
            (if (array-kv-pair? v)
              ;; [key]=val syntax
              (let-values (((key val) (parse-array-kv v)))
                (if assoc?
                  (hash-put! tbl key val)
                  (let ((idx (or (string->number key) auto-idx)))
                    (hash-put! tbl idx val)
                    (loop (cdr vals) (+ idx 1)))))
              ;; Plain value — use auto-incrementing index
              (begin
                (if assoc?
                  ;; Assoc arrays require [key]=val syntax; plain values ignored
                  #!void
                  (hash-put! tbl auto-idx v))
                (loop (cdr vals) (+ auto-idx 1))))))))))

;; Append to an array: arr+=(val1 val2)
(def (env-array-append-compound! env name values)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (when (and var (shell-var-readonly? var))
      (error (format "~a: readonly variable" name)))
    (if (and var (or (shell-var-array? var) (shell-var-assoc? var)))
      (let* ((tbl (shell-var-value var))
             (assoc? (shell-var-assoc? var))
             ;; For indexed arrays, find next available index
             (next-idx (if assoc? 0
                         (if (> (hash-length tbl) 0)
                           (+ 1 (apply max (hash-keys tbl)))
                           0))))
        (let loop ((vals values) (idx next-idx))
          (when (pair? vals)
            (let ((v (car vals)))
              (if (array-kv-pair? v)
                (let-values (((key val) (parse-array-kv v)))
                  (if assoc?
                    (hash-put! tbl key val)
                    (let ((kidx (or (string->number key) idx)))
                      (hash-put! tbl kidx val)
                      (loop (cdr vals) (+ kidx 1)))))
                (begin
                  (unless assoc?
                    (hash-put! tbl idx v))
                  (loop (cdr vals) (+ idx 1))))))))
      ;; Not yet an array — create one
      (env-array-set-compound! env name values #f))))

;; Check if a compound assignment value has [key]=val syntax
(def (array-kv-pair? str)
  (and (> (string-length str) 0)
       (char=? (string-ref str 0) #\[)
       (let ((close (string-find-char-in str #\])))
         (and close
              (< (+ close 1) (string-length str))
              (char=? (string-ref str (+ close 1)) #\=)))))

;; Parse [key]=val from a compound assignment element
(def (parse-array-kv str)
  (let* ((close (string-find-char-in str #\]))
         (key (substring str 1 close))
         (val (substring str (+ close 2) (string-length str))))
    (values key val)))

(def (string-find-char-in str ch)
  (let loop ((i 0))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))

;; Check if a variable is an array
(def (env-is-array? env name)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    (and var (or (shell-var-array? var) (shell-var-assoc? var)))))

;; Parse "name[idx]" → (values base-name index-string) or #f
(def (parse-arith-subscript name)
  (let ((bpos (string-find-char-in name #\[)))
    (if (and bpos (> bpos 0)
             (> (string-length name) (+ bpos 1))
             (char=? (string-ref name (- (string-length name) 1)) #\]))
      (values (substring name 0 bpos)
              (substring name (+ bpos 1) (- (string-length name) 1)))
      (values #f #f))))

;; Array-aware getter for arithmetic: handles "name[idx]" format
;; Also handles bare array names as array[0] (decay)
;; Returns #f for truly undefined variables (for nounset support)
(def (arith-env-getter env)
  (lambda (name)
    (let-values (((base idx) (parse-arith-subscript name)))
      (if base
        ;; Array subscript: name[idx]
        ;; Check if variable exists at all first
        (let ((var (env-get-var env base)))
          (if var
            (let ((key (or (string->number idx) idx)))
              (env-array-get env base key))
            #f))  ;; truly undefined → return #f for nounset
        ;; Plain name — but if it's an array, return element [0]
        (env-get env name)))))

;; Array-aware setter for arithmetic: handles "name[idx]" format
(def (arith-env-setter env)
  (lambda (name val)
    (let-values (((base idx) (parse-arith-subscript name)))
      (if base
        ;; Array subscript: name[idx]
        (let ((key (or (string->number idx) idx)))
          (env-array-set! env base key val))
        ;; Plain name
        (env-set! env name val)))))
