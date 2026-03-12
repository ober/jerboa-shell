;;; builtins.ss — Built-in command registry and implementations for gsh

(export #t)
(import :std/sugar
        :std/format
        :std/iter
        :std/sort
        ./pregexp-compat
        :std/os/signal
        :std/os/fdio
        :gsh/ast
        :gsh/ffi
        :gsh/environment
        :gsh/expander
        :gsh/functions
        :gsh/jobs
        :gsh/signals
        :gsh/history
        :gsh/util
        :gsh/registry
        :gsh/macros)

;; Parameter for execute-external — set from executor.ss to break circular dep
(def *execute-external-fn* (make-parameter #f))

;;; --- Pipeline-safe I/O helpers ---
;;; Gambit character ports opened via /dev/fd/N don't properly detect EOF
;;; on pipes (they block forever instead of returning #!eof). These helpers
;;; use fdread on the raw pipeline fd when available.

;; Read a single character from the pipeline stdin fd using ffi-fdread.
;; Returns a character or #!eof.
;; Uses ffi-fdread (fd count -> bytevector) which has consistent API on
;; both Gerbil and Chez, unlike fdread (Gerbil: 4-arg, Chez: 2-arg).
(def (fd-read-char rfd)
  (let ((bv (ffi-fdread rfd 1)))
    (if (= (u8vector-length bv) 0)
      #!eof
      (integer->char (u8vector-ref bv 0)))))

;; Smart read-char that uses fdread when in a pipeline, falling back
;; to regular read-char on the Gambit port.
(def (port-or-fd-read-char in-port pipe-fd)
  (if pipe-fd
    (fd-read-char pipe-fd)
    (read-char in-port)))

;;; --- Built-in implementations ---
;;; Each handler: (lambda (args env) -> exit-status)
;;; Using defbuiltin macro from :gsh/macros for cleaner syntax

;; : (colon) — no-op, always succeeds
(defbuiltin ":" 0)

;; true
(defbuiltin "true" 0)

;; false
(defbuiltin "false" 1)

;; echo [-neE] [args...]
(defbuiltin "echo"
    ;; Parse echo flags: -n, -e, -E, or combinations like -en, -neE
    (let loop ((args args) (newline? #t) (escape? #f))
      (if (and (pair? args)
               (> (string-length (car args)) 1)
               (char=? (string-ref (car args) 0) #\-)
               ;; All chars after - must be n, e, or E
               (let ((s (car args)))
                 (let valid? ((j 1))
                   (if (>= j (string-length s)) #t
                     (let ((c (string-ref s j)))
                       (and (or (char=? c #\n) (char=? c #\e) (char=? c #\E))
                            (valid? (+ j 1))))))))
        ;; Parse flags from this argument
        (let* ((s (car args))
               (has-n? (let find ((j 1)) (if (>= j (string-length s)) #f
                                           (or (char=? (string-ref s j) #\n) (find (+ j 1))))))
               (has-e? (let find ((j 1)) (if (>= j (string-length s)) #f
                                           (or (char=? (string-ref s j) #\e) (find (+ j 1))))))
               (has-E? (let find ((j 1)) (if (>= j (string-length s)) #f
                                           (or (char=? (string-ref s j) #\E) (find (+ j 1)))))))
          (loop (cdr args)
                (if has-n? #f newline?)
                (cond (has-E? #f) (has-e? #t) (else escape?))))
        ;; Print arguments (use call/cc for \c early termination)
        (call/cc
         (lambda (stop)
           (let arg-loop ((rest args) (first? #t))
             (when (pair? rest)
               (unless first? (display " "))
               (if escape?
                 (let ((result (echo-expand-escapes (car rest))))
                   (display-raw-bytes (car result))
                   (when (cdr result)  ;; \c encountered — stop all output
                     (force-output)
                     (stop 0)))
                 (shell-display (car rest)))
               (arg-loop (cdr rest) #f)))
           (when newline? (newline))
           (force-output)
           0)))))

;; printf [-v var] format [args...]
(defbuiltin "printf"
    (if (null? args)
      (begin (fprintf (current-error-port) "printf: usage: printf [-v var] format [arguments]~n") 2)
      ;; Parse -v option and --
      (let loop ((rest args) (var-name #f))
        (cond
          ((null? rest)
           (begin (fprintf (current-error-port) "printf: usage: printf [-v var] format [arguments]~n") 2))
          ((string=? (car rest) "--")
           (loop (cdr rest) var-name))
          ((and (not var-name)
                (>= (length rest) 2)
                (string=? (car rest) "-v"))
           (loop (cddr rest) (cadr rest)))
          (else
           (let ((fmt (car rest))
                 (fmt-args (cdr rest)))
             ;; Format with argument recycling: repeat format until all args consumed
             (parameterize ((*printf-conversion-error* #f))
               (let ((output (shell-printf fmt fmt-args)))
                 (if var-name
                   ;; -v: store as string in variable
                   (let ((str-output (u8vector->string-lossy output)))
                     (let ((bracket (string-index var-name #\[)))
                       (if bracket
                         ;; Validate array subscript syntax: must end with ]
                         (if (and (> (string-length var-name) (+ bracket 1))
                                  (char=? (string-ref var-name (- (string-length var-name) 1)) #\]))
                           (let ((arr-name (substring var-name 0 bracket))
                                 (idx-str (substring var-name (+ bracket 1)
                                                     (- (string-length var-name) 1))))
                             (env-array-set! env arr-name idx-str str-output)
                             (if (*printf-conversion-error*) 1 0))
                           (begin
                             (fprintf (current-error-port) "printf: `~a': not a valid identifier~n" var-name)
                             2))
                         (begin
                           (env-set! env var-name str-output)
                           (if (*printf-conversion-error*) 1 0)))))
                   ;; Normal: write raw bytes to stdout
                   (begin (write-subu8vector output 0 (u8vector-length output)
                                             (current-output-port))
                          (force-output)
                          (if (*printf-conversion-error*) 1 0)))))))))))

;; Helper: strip trailing slash (except for root /)
(def (strip-trailing-slash path)
  (let ((len (string-length path)))
    (if (and (> len 1) (char=? (string-ref path (- len 1)) #\/))
      (substring path 0 (- len 1))
      path)))

;; Helper: check if two paths refer to the same directory (by device+inode)
(def (same-directory? path-a path-b)
  (with-catch
   (lambda (e) #f)
   (lambda ()
     (let ((a (file-info path-a))
           (b (file-info path-b)))
       (and (= (file-info-device a) (file-info-device b))
            (= (file-info-inode a) (file-info-inode b)))))))

;; Helper: resolve logical path for cd
;; Given a base (logical pwd) and a relative target, compute the new logical path
;; Handles ".." by removing last component textually (not resolving symlinks)
(def (resolve-logical-path base target)
  (let ((path (if (and (> (string-length target) 0)
                       (char=? (string-ref target 0) #\/))
                target  ;; absolute path
                (string-append (strip-trailing-slash base) "/" target))))
    ;; Normalize: split on /, resolve . and .., rejoin
    (let* ((parts (string-split path #\/))
           (resolved
            (let loop ((ps parts) (acc []))
              (cond
                ((null? ps) (reverse acc))
                ((string=? (car ps) "") (loop (cdr ps) (if (null? acc) [""] acc)))
                ((string=? (car ps) ".") (loop (cdr ps) acc))
                ((string=? (car ps) "..")
                 (loop (cdr ps)
                       (if (and (pair? acc) (not (string=? (car acc) "")))
                         (cdr acc)
                         acc)))
                (else (loop (cdr ps) (cons (car ps) acc)))))))
      (let ((result (string-join resolved "/")))
        (if (string=? result "") "/" result)))))

;; Helper: search CDPATH for a relative directory
(def (search-cdpath target env)
  (let ((cdpath (env-get env "CDPATH")))
    (if (or (not cdpath) (string=? cdpath "")
            (and (> (string-length target) 0)
                 (char=? (string-ref target 0) #\/)))
      #f  ;; no CDPATH or absolute path
      (let loop ((dirs (string-split cdpath #\:)))
        (if (null? dirs) #f
          (let* ((base (if (string=? (car dirs) "") "." (car dirs)))
                 (full (string-append (strip-trailing-slash base) "/" target)))
            (if (file-exists? full)
              full
              (loop (cdr dirs)))))))))

;; cd [-L|-P] [--] [dir]
(defbuiltin "cd"
    ;; Parse options
    (let loop ((rest args) (physical? #f))
      (cond
        ;; No more options
        ((or (null? rest)
             (string=? (car rest) "--")
             (not (and (> (string-length (car rest)) 0)
                       (char=? (string-ref (car rest) 0) #\-)))
             (string=? (car rest) "-"))
         (let* ((remaining (if (and (pair? rest) (string=? (car rest) "--"))
                             (cdr rest)
                             rest)))
           (if (and (pair? remaining) (pair? (cdr remaining)))
             (begin
               (fprintf (current-error-port) "cd: too many arguments~n")
               1)
             (let* ((dir-arg (cond
                           ((null? remaining)
                            (let ((home (env-get env "HOME")))
                              (if (or (not home) (string=? home ""))
                                (begin
                                  (fprintf (current-error-port) "cd: HOME not set~n")
                                  #f)
                                home)))
                           ((string=? (car remaining) "-")
                            (let ((oldpwd (env-get env "OLDPWD")))
                              (if (not oldpwd)
                                (begin
                                  (fprintf (current-error-port) "cd: OLDPWD not set~n")
                                  #f)
                                oldpwd)))
                           (else (car remaining)))))
           (if (not dir-arg)
             1
             (let* ((expanded (expand-word-nosplit dir-arg env))
                    ;; Try CDPATH for relative paths
                    (actual-dir (or (and (not (and (> (string-length expanded) 0)
                                                   (char=? (string-ref expanded 0) #\/)))
                                        (search-cdpath expanded env))
                                   expanded))
                    (print-dir? (or (and (pair? remaining)
                                        (string=? (car remaining) "-"))
                                   (and (not (string=? actual-dir expanded))
                                        ;; CDPATH found a different dir
                                        #t))))
               (with-catch
                (lambda (e)
                  (fprintf (current-error-port) "cd: ~a: No such file or directory~n" expanded)
                  1)
                (lambda ()
                  (let ((old-pwd (or (*internal-pwd*)
                                     (strip-trailing-slash
                                      (or (env-get env "PWD")
                                          (strip-trailing-slash (current-directory)))))))
                    (let ((new-pwd (if physical?
                                     (begin
                                       (current-directory actual-dir)
                                       (strip-trailing-slash (current-directory)))
                                     ;; Logical mode: compute logical path, then chdir to it
                                     ;; First verify path is reachable physically
                                     (let ((logical (strip-trailing-slash
                                                     (resolve-logical-path old-pwd actual-dir))))
                                       ;; Validate path: try chdir to actual-dir first
                                       ;; (catches nonexistent components like cd BAD/..)
                                       (current-directory actual-dir)
                                       ;; Now chdir to the logical path
                                       (current-directory logical)
                                       logical))))
                      (env-set! env "OLDPWD" old-pwd)
                      (env-export! env "OLDPWD")
                      (env-set! env "PWD" new-pwd)
                      (env-export! env "PWD")
                      (*internal-pwd* new-pwd)
                      (when print-dir?
                        (displayln new-pwd))
                      0))))))))))
        ;; -L flag: logical (default)
        ((string=? (car rest) "-L")
         (loop (cdr rest) #f))
        ;; -P flag: physical
        ((string=? (car rest) "-P")
         (loop (cdr rest) #t))
        ;; Unknown option - treat as dir
        (else
         (loop (cons "--" rest) physical?)))))

;; pwd [-L|-P]
(defbuiltin "pwd"
    (let ((physical? (and (pair? args) (string=? (car args) "-P"))))
      (displayln (strip-trailing-slash
                  (if physical?
                    (current-directory)
                    ;; Logical mode: use internal tracked PWD (not $PWD which user can override)
                    (or (*internal-pwd*)
                        (strip-trailing-slash (current-directory)))))))
    0)

;; export [name[=value] ...]
(defbuiltin "export"
    ;; Parse flags: -p, -n
    (let ((print? #f) (remove? #f) (names []))
      (let loop ((args args))
        (when (pair? args)
          (let ((arg (car args)))
            (cond
              ((and (> (string-length arg) 1)
                    (char=? (string-ref arg 0) #\-)
                    (not (string=? arg "--")))
               (let floop ((i 1))
                 (when (< i (string-length arg))
                   (case (string-ref arg i)
                     ((#\p) (set! print? #t))
                     ((#\n) (set! remove? #t))
                     (else (void)))
                   (floop (+ i 1))))
               (loop (cdr args)))
              ((string=? arg "--")
               (set! names (append names (cdr args))))
              (else
               (set! names (append names (list arg)))
               (loop (cdr args)))))))
      (cond
        ;; export -p with no names (or no args at all): list all exported vars
        ((null? names)
         (let* ((all-vars (collect-all-vars env))
                (sorted (sort! (hash-keys all-vars) string<?)))
           (for-each
            (lambda (name)
              (let ((var (hash-get all-vars name)))
                (when (and var (shell-var-exported? var)
                           (not (eq? (shell-var-value var) +unset-sentinel+)))
                  (display-declare-var name var))))
            sorted))
         0)
        ;; export -p with names: print named exported vars
        (print?
         (for-each
          (lambda (name)
            (let ((var (env-get-var env name)))
              (when (and var (shell-var-exported? var)
                         (not (eq? (shell-var-value var) +unset-sentinel+)))
                (display-declare-var name var))))
          names)
         0)
        ;; Export/unexport variables
        (else
         (let ((status 0))
           (for-each
            (lambda (arg)
              (let ((eq-pos (string-find-char* arg #\=)))
                (if eq-pos
                  (let* ((name (substring arg 0 eq-pos))
                         (value (substring arg (+ eq-pos 1) (string-length arg))))
                    (if remove?
                      ;; export -n name=val: not allowed — report error
                      (begin
                        (fprintf (current-error-port)
                                 "gsh: export: ~a: not a valid identifier~n" arg)
                        (set! status 2))
                      (env-export! env name value)))
                  (if remove?
                    ;; export -n name: remove export flag and unsetenv
                    (let ((var (env-get-raw-var env arg)))
                      (when var
                        (set! (shell-var-exported? var) #f)
                        (ffi-unsetenv arg)))
                    (env-export! env arg)))))
            names)
           status)))))

;; unset [-fvn] name ...
(defbuiltin "unset"
    (let loop ((args args) (unset-func? #f) (unset-nameref? #f))
      (cond
        ((null? args) 0)
        ((string=? (car args) "-f")
         (loop (cdr args) #t #f))
        ((string=? (car args) "-v")
         (loop (cdr args) #f #f))
        ((string=? (car args) "-n")
         (loop (cdr args) #f #t))
        (else
         (let ((status 0))
           (for-each
            (lambda (name)
              (cond
                (unset-func?
                 (function-unset! env name))
                (unset-nameref?
                 ;; Validate variable name
                 (if (not (valid-name? name))
                   (begin
                     (fprintf (current-error-port) "gsh: unset: `~a': not a valid identifier~n" name)
                     (set! status 1))
                   ;; unset -n: unset the nameref itself, not the target
                   (with-catch
                    (lambda (e)
                      (fprintf (current-error-port) "gsh: unset: ~a: cannot unset~n" name)
                      (set! status 1))
                    (lambda () (env-unset-nameref! env name)))))
                (else
                 ;; Validate variable name (extract base name before [ for arrays)
                 (let* ((bracket-pos (string-find-char* name #\[))
                        (base-name (if bracket-pos (substring name 0 bracket-pos) name)))
                   (if (not (valid-name? base-name))
                     (begin
                       (fprintf (current-error-port) "gsh: unset: `~a': not a valid identifier~n" name)
                       (set! status 1))
                     (with-catch
                      (lambda (e)
                        (fprintf (current-error-port) "gsh: unset: ~a: cannot unset: readonly variable~n" name)
                        (set! status 1))
                      (lambda ()
                        (if (and bracket-pos
                                 (> bracket-pos 0)
                                 (let ((close (string-find-char* name #\])))
                                   (and close (= close (- (string-length name) 1)))))
                          ;; Unset array element
                          (let ((var-name (substring name 0 bracket-pos))
                                (index (substring name (+ bracket-pos 1)
                                                 (- (string-length name) 1))))
                            (env-array-unset-element! env var-name index))
                          ;; Unset whole variable (resolves namerefs)
                          ;; If no variable exists, also try to unset function (POSIX)
                          (let ((var (env-get-raw-var env name)))
                            (if var
                              (env-unset! env name)
                              (function-unset! env name)))))))))))
            args)
           status)))))

;; readonly [-aAp] [name[=value] ...]
(defbuiltin "readonly"
    ;; Parse flags
    (let ((print? #f) (array? #f) (assoc? #f) (names []))
      (let loop ((args args))
        (if (null? args)
          (void)
          (let ((arg (car args)))
            (cond
              ;; Flag arguments like -p, -a, -A, -pa etc.
              ((and (> (string-length arg) 1)
                    (char=? (string-ref arg 0) #\-))
               (let floop ((i 1))
                 (when (< i (string-length arg))
                   (case (string-ref arg i)
                     ((#\p) (set! print? #t))
                     ((#\a) (set! array? #t))
                     ((#\A) (set! assoc? #t))
                     (else (void)))
                   (floop (+ i 1))))
               (loop (cdr args)))
              (else
               (set! names (append names (list arg)))
               (loop (cdr args)))))))
      (cond
        ;; readonly -p with no names (or no args): list all readonly vars
        ((null? names)
         (let* ((all-vars (collect-all-vars env))
                (sorted (sort! (hash-keys all-vars) string<?)))
           (for-each
            (lambda (name)
              (let ((var (hash-get all-vars name)))
                (when (and var (shell-var-readonly? var)
                           (not (eq? (shell-var-value var) +unset-sentinel+)))
                  (display-declare-var name var))))
            sorted))
         0)
        ;; readonly -p with names: print named readonly vars
        (print?
         (for-each
          (lambda (name)
            (let ((var (env-get-var env name)))
              (when (and var (shell-var-readonly? var)
                         (not (eq? (shell-var-value var) +unset-sentinel+)))
                (display-declare-var name var))))
          names)
         0)
        ;; readonly with names: set readonly attribute
        (else
         (for-each
          (lambda (arg)
            (let ((eq-pos (string-find-char* arg #\=)))
              (if eq-pos
                (let* ((name (substring arg 0 eq-pos))
                       (value (substring arg (+ eq-pos 1) (string-length arg))))
                  ;; Handle -a/-A with compound array assignment
                  (if (and (or array? assoc?)
                           (> (string-length value) 0)
                           (char=? (string-ref value 0) #\())
                    ;; Compound array: parse and assign
                    (let* ((inner (if (and (> (string-length value) 1)
                                          (char=? (string-ref value (- (string-length value) 1)) #\)))
                                   (substring value 1 (- (string-length value) 1))
                                   (substring value 1 (string-length value))))
                           (tbl (make-hash-table))
                           (existing (or (hash-get (shell-environment-vars env) name)
                                         (let ((v (make-shell-var tbl #f #f #t #f #f #f #f array? assoc?)))
                                           (hash-put! (shell-environment-vars env) name v) v))))
                      (when (> (string-length inner) 0)
                        (let ((elems (parse-array-compound-elements inner)))
                          (if assoc?
                            (for-each (lambda (elem)
                              (let ((bracket-start (string-find-char* elem #\[)))
                                (when bracket-start
                                  (let* ((bracket-end (string-find-char-from elem #\] (+ bracket-start 1)))
                                         (key (substring elem (+ bracket-start 1) (or bracket-end (string-length elem))))
                                         (eq (string-find-char-from elem #\= (or bracket-end 0)))
                                         (val (if eq (substring elem (+ eq 1) (string-length elem)) "")))
                                    (hash-put! tbl key val)))))
                              elems)
                            (let loop ((es elems) (idx 0))
                              (when (pair? es)
                                (hash-put! tbl idx (car es))
                                (loop (cdr es) (+ idx 1)))))))
                      (set! (shell-var-value existing) tbl)
                      (set! (shell-var-array? existing) (not assoc?))
                      (set! (shell-var-assoc? existing) assoc?)
                      (set! (shell-var-readonly? existing) #t))
                    ;; Scalar value
                    (begin
                      (env-readonly! env name value)
                      ;; If -a/-A flag, also set the array attribute
                      (when (or array? assoc?)
                        (let ((var (env-get-raw-var env name)))
                          (when var
                            (when array? (set! (shell-var-array? var) #t))
                            (when assoc? (set! (shell-var-assoc? var) #t)
                                         (set! (shell-var-array? var) #f))))))))
                ;; No value — mark readonly, create array if -a/-A flag
                (let ((existing (hash-get (shell-environment-vars env) arg)))
                  (if existing
                    (begin
                      (set! (shell-var-readonly? existing) #t)
                      (when (and array? (not (shell-var-array? existing))
                                 (not (shell-var-assoc? existing)))
                        (set! (shell-var-array? existing) #t)
                        (when (eq? (shell-var-value existing) +unset-sentinel+)
                          (set! (shell-var-value existing) (make-hash-table))))
                      (when (and assoc? (not (shell-var-assoc? existing)))
                        (set! (shell-var-assoc? existing) #t)
                        (set! (shell-var-array? existing) #f)
                        (when (eq? (shell-var-value existing) +unset-sentinel+)
                          (set! (shell-var-value existing) (make-hash-table)))))
                    ;; Create new var — array/assoc hash or unset sentinel
                    (hash-put! (shell-environment-vars env) arg
                               (make-shell-var
                                (if (or array? assoc?) (make-hash-table) +unset-sentinel+)
                                #f #t #t #f #f #f #f array? assoc?)))))))
          names)
         0))))

;; exit [n]
(defbuiltin "exit"
    (if (and (pair? args) (pair? (cdr args)))
      ;; Too many arguments → fatal error
      (begin
        (fprintf (current-error-port) "gsh: exit: too many arguments~n")
        (if (*in-subshell*)
          (raise (make-subshell-exit-exception 2))
          (exit 2)))
      ;; Normal: 0 or 1 arguments
      (let ((code (if (pair? args)
                    (let ((n (string->number (car args))))
                      (cond
                        ((not n)
                         (fprintf (current-error-port) "gsh: exit: ~a: numeric argument required~n" (car args))
                         2)
                        ;; Reject values outside 32-bit signed integer range
                        ((or (> n 2147483647) (< n -2147483648))
                         (fprintf (current-error-port) "gsh: exit: ~a: expected a small integer~n" (car args))
                         1)
                        (else (bitwise-and n #xFF))))
                    (shell-environment-last-status env))))
        ;; In a subshell, raise exception instead of terminating
        (if (*in-subshell*)
          (raise (make-subshell-exit-exception code))
          (begin
            ;; Run EXIT trap if set — clear first to prevent re-entrancy
            (let ((exit-trap (trap-get "EXIT")))
              (when (and exit-trap (string? exit-trap))
                (trap-set! "EXIT" 'default)
                (let ((exec-fn (*execute-input*)))
                  (when exec-fn
                    (exec-fn exit-trap env)))))
            (history-save!)
            (exit code))))))

;; return [n]
(defbuiltin "return"
    (let ((code (if (pair? args)
                  (let ((n (string->number (car args))))
                    (cond
                      ;; Empty string → 0
                      ((string=? (car args) "") 0)
                      ((not n)
                       (fprintf (current-error-port) "gsh: return: ~a: numeric argument required~n" (car args))
                       2)
                      ;; Reject values outside 32-bit signed integer range
                      ((or (> n 2147483647) (< n -2147483648))
                       (fprintf (current-error-port) "gsh: return: ~a: expected a small integer~n" (car args))
                       1)
                      (else (bitwise-and n #xFF))))
                  (shell-environment-last-status env))))
      (shell-return! code)))

;; break [n]
(defbuiltin "break"
    (cond
      ;; Too many arguments — special builtin error is fatal (bash: exit 1)
      ((> (length args) 1)
       (fprintf (current-error-port) "gsh: break: too many arguments~n")
       (force-output (current-error-port))
       (if (*in-subshell*)
         (raise (make-subshell-exit-exception 1))
         (exit 1)))
      ((pair? args)
       (let ((n (string->number (car args))))
         (cond
           ((not n)
            ;; Non-numeric arg: print error, set error status, then break (bash compat)
            (fprintf (current-error-port) "gsh: break: ~a: numeric argument required~n" (car args))
            (env-set-last-status! env 1)
            (shell-break! 1))
           ((<= n 0)
            (fprintf (current-error-port) "gsh: break: ~a: loop count out of range~n" (car args))
            (env-set-last-status! env 1)
            (shell-break! 1))
           (else (shell-break! n)))))
      (else (shell-break! 1))))

;; continue [n]
(defbuiltin "continue"
    (cond
      ;; Too many arguments — special builtin error is fatal (bash: exit 1)
      ((> (length args) 1)
       (fprintf (current-error-port) "gsh: continue: too many arguments~n")
       (force-output (current-error-port))
       (if (*in-subshell*)
         (raise (make-subshell-exit-exception 1))
         (exit 1)))
      ((pair? args)
       (let ((n (string->number (car args))))
         (cond
           ((not n)
            ;; Non-numeric arg: print error, set error status, then continue (bash compat)
            (fprintf (current-error-port) "gsh: continue: ~a: numeric argument required~n" (car args))
            (env-set-last-status! env 1)
            (shell-continue! 1))
           ((<= n 0)
            (fprintf (current-error-port) "gsh: continue: ~a: loop count out of range~n" (car args))
            (env-set-last-status! env 1)
            (shell-continue! 1))
           (else (shell-continue! n)))))
      (else (shell-continue! 1))))

;; set [options] [-- args...]
(defbuiltin "set"
    (if (null? args)
      ;; No args: display all variables in re-evaluable format
      (begin
        (for-each
         (lambda (pair)
           (displayln (format "~a=~a" (car pair) (shell-quote-value (cdr pair)))))
         (env-all-variables env))
        0)
      (let loop ((args args))
        (cond
          ((null? args) 0)
          ((string=? (car args) "--")
           ;; Set positional parameters
           (env-set-positional! env (cdr args))
           0)
          ;; "set -" — equivalent to "set +xv" (turn off xtrace and verbose)
          ((string=? (car args) "-")
           (env-option-set! env "xtrace" #f)
           (env-option-set! env "verbose" #f)
           ;; Remaining args become positional parameters
           (when (pair? (cdr args))
             (env-set-positional! env (cdr args)))
           0)
          ;; -o / +o: check if next arg looks like an option name
          ((or (string=? (car args) "-o")
               (string=? (car args) "+o"))
           (let ((enable? (char=? (string-ref (car args) 0) #\-)))
             (if (and (pair? (cdr args))
                      ;; Next arg is an option name if it doesn't start with - or +
                      (not (string-prefix? "-" (cadr args)))
                      (not (string-prefix? "+" (cadr args))))
               ;; -o option-name / +o option-name
               (begin
                 (env-option-set! env (cadr args) enable?)
                 (loop (cddr args)))
               ;; -o / +o without argument: print option list
               (begin
                 (for-each
                  (lambda (opt)
                    (let ((name (car opt)) (on? (cdr opt)))
                      (if enable?
                        (displayln (format "~a\t~a" name (if on? "on" "off")))
                        (displayln (format "set ~ao ~a" (if on? "-" "+") name)))))
                  (env-all-options env))
                 0))))
          ;; Bare "+" is ignored (like a no-op flag)
          ((string=? (car args) "+")
           (loop (cdr args)))
          ((and (> (string-length (car args)) 1)
                (char=? (string-ref (car args) 0) #\-))
           (apply-set-options! env (car args) #t)
           (loop (cdr args)))
          ((and (> (string-length (car args)) 1)
                (char=? (string-ref (car args) 0) #\+))
           (apply-set-options! env (car args) #f)
           (loop (cdr args)))
          (else
           ;; Positional parameters
           (env-set-positional! env args)
           0)))))

(def (shell-quote-value val)
  ;; Check if value needs any quoting at all
  ;; Simple values (alphanumeric, _, -, ., /, :, =, +, @, %, ^, ~, ,) don't need quotes
  ;; Must scan the ENTIRE string for control chars before choosing quoting strategy
  (let* ((needs-dollar-quote? #f)
         (needs-any-quote?
          (if (= (string-length val) 0)
            #t  ;; empty string needs ''
            (let loop ((i 0) (needs? #f))
              (if (>= i (string-length val)) needs?
                (let ((ch (string-ref val i)))
                  (cond
                    ((or (char<? ch #\space) (char=? ch #\x7f))
                     (set! needs-dollar-quote? #t)
                     (loop (+ i 1) #t))
                    ((or (char-alphabetic? ch) (char-numeric? ch)
                         (memq ch '(#\_ #\- #\. #\/ #\: #\= #\+ #\@ #\% #\^ #\~ #\,)))
                     (loop (+ i 1) needs?))
                    (else (loop (+ i 1) #t))))))))) ;; needs quoting
    (cond
      (needs-dollar-quote?
       ;; Use $'...' quoting with escape sequences for control chars
       (let ((buf (open-output-string)))
         (display "$'" buf)
         (let loop ((i 0))
           (when (< i (string-length val))
             (let ((ch (string-ref val i)))
               (cond
                 ((char=? ch #\newline) (display "\\n" buf))
                 ((char=? ch #\tab) (display "\\t" buf))
                 ((char=? ch #\return) (display "\\r" buf))
                 ((char=? ch #\\) (display "\\\\" buf))
                 ((char=? ch #\') (display "\\'" buf))
                 ((or (char<? ch #\space) (char=? ch #\x7f))
                  ;; Use octal \NNN (bash uses octal for control chars in $'...')
                  (let ((oct (number->string (char->integer ch) 8)))
                    (display (string-append "\\" (make-string (- 3 (string-length oct)) #\0) oct) buf)))
                 (else (display ch buf))))
             (loop (+ i 1))))
         (display "'" buf)
         (get-output-string buf)))
      (needs-any-quote?
       ;; Normal single-quote wrapping
       (string-append "'" (string-replace-all val "'" "'\\''") "'"))
      (else val))))


;; shift [n]
(defbuiltin "shift"
    (if (pair? args)
      (let ((n (string->number (car args))))
        (cond
          ((not n)
           (fprintf (current-error-port) "gsh: shift: ~a: numeric argument required~n" (car args))
           2)
          ((< n 0)
           (fprintf (current-error-port) "gsh: shift: ~a: shift count out of range~n" (car args))
           1)
          (else
           (let ((pos (env-positional-list env)))
             (if (> n (length pos))
               1
               (begin
                 (env-set-positional! env (list-tail pos n))
                 0))))))
      (let ((pos (env-positional-list env)))
        (if (> 1 (length pos))
          1
          (begin
            (env-set-positional! env (list-tail pos 1))
            0)))))

;; eval [args...]
(defbuiltin "eval"
    ;; Skip leading -- (eval accepts/ignores it)
    (let ((args (if (and (pair? args) (string=? (car args) "--"))
                  (cdr args)
                  args)))
      (cond
        ((null? args) 0)
        ;; Check for invalid options (starts with - but isn't --)
        ((and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-)
              (not (string=? (car args) "--")))
         (fprintf (current-error-port)
                  "gsh: eval: ~a: invalid option~n" (car args))
         2)
        (else
         (let ((input (string-join-sp args))
               (exec-fn (*execute-input*)))
           (if exec-fn
             (exec-fn input env)
             (begin
               (fprintf (current-error-port) "gsh: eval: executor not initialized~n")
               1)))))))

;; test / [ — conditional expressions
(defbuiltin "test"
    (parameterize ((*test-var-fn* (lambda (name) (env-get env name)))
                   (*test-option-fn* (lambda (name) (env-option? env name))))
      (test-eval args)))

(defbuiltin "["
    (if (and (pair? args)
             (string=? (last-elem* args) "]"))
      (let ((inner (butlast args)))
        (parameterize ((*test-var-fn* (lambda (name) (env-get env name)))
                       (*test-option-fn* (lambda (name) (env-option? env name))))
          (with-catch
           (lambda (e) 2)  ;; Any evaluation error → exit 2
           (lambda () (test-eval inner)))))
      (begin
        (fprintf (current-error-port) "[: missing `]'~n")
        2)))

;; type [-afptP] name...
(defbuiltin "type"
    ;; Parse flags
    (let parse-flags ((args args) (flags []))
      (if (and (pair? args) (> (string-length (car args)) 0)
               (char=? (string-ref (car args) 0) #\-))
        (parse-flags (cdr args) (cons (car args) flags))
        (let loop ((args args) (status 0))
          (if (null? args)
            status
            (let ((name (car args)))
              (cond
                ;; Shell keywords
                ((shell-keyword? name)
                 (displayln (format "~a is a shell keyword" name))
                 (loop (cdr args) status))
                ;; Aliases
                ((alias-get env name)
                 => (lambda (expansion)
                      (displayln (format "~a is aliased to `~a'" name expansion))
                      (loop (cdr args) status)))
                ;; Shell functions
                ((function-lookup env name)
                 (displayln (format "~a is a function" name))
                 (loop (cdr args) status))
                ;; Builtins — distinguish special vs regular
                ((special-builtin? name)
                 (displayln (format "~a is a special shell builtin" name))
                 (loop (cdr args) status))
                ((builtin? name)
                 (displayln (format "~a is a shell builtin" name))
                 (loop (cdr args) status))
                ;; External command
                ((which name)
                 => (lambda (path)
                      (displayln (format "~a is ~a" name path))
                      (loop (cdr args) status)))
                ;; Not found — output to stderr
                (else
                 (fprintf (current-error-port) "~a: not found~n" name)
                 (loop (cdr args) 1)))))))))

(def (shell-keyword? name)
  (member name '("if" "then" "else" "elif" "fi" "case" "esac" "for" "while"
                  "until" "do" "done" "in" "function" "select" "time"
                  "{" "}" "!" "[[" "]]" "coproc")))

;; POSIX special builtins (must be handled differently from regular builtins)
(def (special-builtin? name)
  (and (member name '("." ":" "break" "continue" "eval" "exec" "exit"
                       "export" "readonly" "return" "set" "shift" "trap"
                       "unset"))
       (builtin? name)))

;; command [-pvV] cmd [args...]
(defbuiltin "command"
    (cond
      ((null? args) 0)
      ((string=? (car args) "-v")
       ;; Print command type: builtin name, function name, alias name=value, or path
       (if (pair? (cdr args))
         (let ((name (cadr args)))
           (cond
             ((builtin-lookup name) (displayln name) 0)
             ((function-lookup env name) (displayln name) 0)
             ((alias-get env name) => (lambda (v) (displayln (format "alias ~a='~a'" name v)) 0))
             ((which name) => (lambda (p) (displayln p) 0))
             (else (fprintf (current-error-port) "gsh: command: ~a: not found~n" name) 1)))
         1))
      ((string=? (car args) "-V")
       ;; Verbose type info
       (if (pair? (cdr args))
         (let ((name (cadr args)))
           (cond
             ((builtin-lookup name) (displayln (format "~a is a shell builtin" name)) 0)
             ((function-lookup env name) (displayln (format "~a is a function" name)) 0)
             ((which name) => (lambda (p) (displayln (format "~a is ~a" name p)) 0))
             (else (fprintf (current-error-port) "gsh: command: ~a: not found~n" name) 1)))
         1))
      ;; Strip -p flag (use default PATH) — we don't change PATH behavior
      ((string=? (car args) "-p")
       (if (pair? (cdr args))
         ((builtin-lookup "command") (cdr args) env)
         0))
      ;; Run command, skipping function lookup — dispatch to builtin or external
      (else
       (let* ((cmd-name (car args))
              (cmd-args (cdr args))
              (handler (builtin-lookup cmd-name)))
         (if handler
           (handler cmd-args env)
           ;; External command — use execute-external-fn parameter
           (let ((exec-ext (*execute-external-fn*)))
             (if exec-ext
               (exec-ext cmd-name cmd-args env)
               (begin
                 (fprintf (current-error-port) "gsh: ~a: command not found~n" cmd-name)
                 127))))))))

;; builtin name [args...]
;; Execute a shell builtin, bypassing function lookup
(defbuiltin "builtin"
    (if (null? args)
      0
      (let ((handler (builtin-lookup (car args))))
        (if handler
          (handler (cdr args) env)
          (begin
            (fprintf (current-error-port) "gsh: builtin: ~a: not a shell builtin~n" (car args))
            1)))))

;; alias [name[=value] ...]
(defbuiltin "alias"
    (let ((real-args (if (and (pair? args) (string=? (car args) "--"))
                       (cdr args) args)))
      (if (null? real-args)
        ;; List all aliases
        (begin
          (for-each
           (lambda (pair)
             (displayln (format "alias ~a='~a'" (car pair) (cdr pair))))
           (sort (alias-list env) (lambda (a b) (string<? (car a) (car b)))))
          0)
        (let ((status 0))
          (for-each
           (lambda (arg)
             (let ((eq-pos (string-find-char* arg #\=)))
               (if eq-pos
                 (alias-set! env
                            (substring arg 0 eq-pos)
                            (substring arg (+ eq-pos 1) (string-length arg)))
                 ;; Show single alias
                 (let ((val (alias-get env arg)))
                   (if val
                     (displayln (format "alias ~a='~a'" arg val))
                     (begin
                       (fprintf (current-error-port) "alias: ~a: not found~n" arg)
                       (set! status 1)))))))
           real-args)
          status))))

;; unalias [-a] name...
(defbuiltin "unalias"
    (cond
      ((null? args)
       (fprintf (current-error-port) "unalias: usage: unalias [-a] name [name ...]~n")
       2)
      ((and (pair? args) (string=? (car args) "-a"))
       (alias-clear! env) 0)
      (else
       (let ((real-args (if (and (pair? args) (string=? (car args) "--"))
                           (cdr args) args))
             (status 0))
         (for-each
          (lambda (name)
            (if (alias-get env name)
              (alias-unset! env name)
              (begin
                (fprintf (current-error-port) "unalias: ~a: not found~n" name)
                (set! status 1))))
          real-args)
         status))))

;; read [-r] [-p prompt] [-t timeout] [-d delim] [-n count] [-N count] [-s] [-a arr] [-u fd] var...
(defbuiltin "read"
    (let loop ((args args) (raw? #f) (silent? #f) (prompt "")
               (nchars #f) (nchars-raw? #f) (delim #f) (timeout #f)
               (fd #f) (array-name #f) (vars []))
      (cond
        ((null? args)
         ;; Validate timeout
         (when (and timeout (< timeout 0))
           (fprintf (current-error-port) "read: ~a: invalid timeout specification~n" timeout)
           (set! timeout #f))
         ;; Show prompt on stderr (only if input is a terminal)
         (when (and (> (string-length prompt) 0)
                    (= (ffi-isatty (or fd 0)) 1))
           (display prompt (current-error-port))
           (force-output (current-error-port)))
         ;; Set up input port
         (let* ((in-port (if fd
                           (open-input-file (string-append "/dev/fd/" (number->string fd)))
                           (current-input-port)))
                ;; Use raw fd for pipeline reads (Gambit ports can't detect pipe EOF)
                (pipe-fd (and (not fd) (*pipeline-stdin-fd*)))
                ;; Disable echo for -s (only on tty)
                (tty? (and silent? (not fd)
                           (with-catch (lambda (e) #f)
                             (lambda () (tty-mode-set! in-port #t #f #f #f 0) #t)))))
           (dynamic-wind
             (lambda () #!void)
             (lambda ()
               (if (and timeout (= timeout 0))
                 ;; -t 0: check if input is available without reading
                 ;; Use input-port-timeout with 0 and peek-char to test availability
                 ;; -t 0: check if input is available without reading
                 ;; Set 0 timeout, try peek-char: returns #t if data/eof available, #f on timeout
                 (begin
                   (input-port-timeout-set! in-port 0)
                   (if (with-catch (lambda (e) #f)
                         (lambda () (peek-char in-port) #t))
                     0 1))
               ;; Set timeout if requested
               (begin
                 (when timeout
                   (input-port-timeout-set! in-port timeout))
               (let* ((got-eof? #f)
                      (line (cond
                              ;; -N nchars: read exactly N bytes, ignore delimiters
                              ((and nchars nchars-raw?)
                               (let ((buf (open-output-string)))
                                 (let rloop ((count 0))
                                   (if (>= count nchars)
                                     (get-output-string buf)
                                     (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                       (if (eof-object? ch)
                                         (begin (set! got-eof? #t)
                                                (let ((s (get-output-string buf)))
                                                  (if (string=? s "") ch s)))
                                         (begin (display ch buf)
                                                (rloop (+ count 1)))))))))
                              ;; -n nchars: read N chars, respects delimiters
                              ;; In non-raw mode, count PROCESSED chars (after backslash removal)
                              (nchars
                               (let ((delim-ch (if delim
                                                 (if (string=? delim "")
                                                   (integer->char 0)
                                                   (string-ref delim 0))
                                                 #\newline))
                                     (buf (open-output-string)))
                                 (if raw?
                                   ;; Raw mode: just count chars
                                   (let rloop ((count 0))
                                     (if (>= count nchars)
                                       (get-output-string buf)
                                       (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                         (cond
                                           ((eof-object? ch)
                                            (set! got-eof? #t)
                                            (let ((s (get-output-string buf)))
                                              (if (string=? s "") ch s)))
                                           ((char=? ch delim-ch)
                                            (get-output-string buf))
                                           (else
                                            (display ch buf)
                                            (rloop (+ count 1)))))))
                                   ;; Non-raw mode: backslash-char counts as 1 processed char
                                   ;; backslash-newline is line continuation (swallowed, not counted)
                                   (let rloop ((count 0))
                                     (if (>= count nchars)
                                       (get-output-string buf)
                                       (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                         (cond
                                           ((eof-object? ch)
                                            (set! got-eof? #t)
                                            (let ((s (get-output-string buf)))
                                              (if (string=? s "") ch s)))
                                           ((char=? ch delim-ch)
                                            (get-output-string buf))
                                           ((char=? ch #\\)
                                            (let ((next (port-or-fd-read-char in-port pipe-fd)))
                                              (cond
                                                ((eof-object? next)
                                                 (set! got-eof? #t)
                                                 (get-output-string buf))
                                                ((char=? next #\newline)
                                                 ;; Line continuation: swallow, don't count
                                                 (rloop count))
                                                (else
                                                 ;; Escaped char: output the char, count as 1
                                                 (display next buf)
                                                 (rloop (+ count 1))))))
                                           (else
                                            (display ch buf)
                                            (rloop (+ count 1))))))))))
                              ;; -d delim: read until delimiter
                              (delim
                               (let ((delim-ch (if (string=? delim "")
                                                 (integer->char 0)
                                                 (string-ref delim 0)))
                                     (buf (open-output-string)))
                                 (if raw?
                                   ;; Raw mode: no backslash processing
                                   (let rloop ()
                                     (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch delim-ch)
                                          (get-output-string buf))
                                         (else
                                          (display ch buf)
                                          (rloop)))))
                                   ;; Non-raw mode: backslash-newline is continuation
                                   (let rloop ()
                                     (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch delim-ch)
                                          (get-output-string buf))
                                         ((char=? ch #\\)
                                          (let ((next (port-or-fd-read-char in-port pipe-fd)))
                                            (cond
                                              ((eof-object? next)
                                               (set! got-eof? #t)
                                               (get-output-string buf))
                                              ((char=? next #\newline)
                                               ;; Line continuation
                                               (rloop))
                                              (else
                                               ;; Keep escaped char (remove backslash)
                                               (display next buf)
                                               (rloop)))))
                                         (else
                                          (display ch buf)
                                          (rloop))))))))
                              ;; Default: read line
                              (else
                               (if raw?
                                 ;; Raw mode: read char-by-char to detect no-newline
                                 (let ((buf (open-output-string)))
                                   (let rloop ()
                                     (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch #\newline)
                                          (get-output-string buf))
                                         (else
                                          (display ch buf)
                                          (rloop))))))
                                 ;; Cooked mode: backslash-newline is continuation
                                 ;; Read char-by-char to detect no-newline EOF
                                 (let ((buf (open-output-string)))
                                   (let rloop ()
                                     (let ((ch (port-or-fd-read-char in-port pipe-fd)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch #\newline)
                                          (get-output-string buf))
                                         ((char=? ch #\\)
                                          (let ((next (port-or-fd-read-char in-port pipe-fd)))
                                            (cond
                                              ((eof-object? next)
                                               (set! got-eof? #t)
                                               (get-output-string buf))
                                              ((char=? next #\newline)
                                               ;; Line continuation
                                               (rloop))
                                              (else
                                               ;; Keep escaped char WITH backslash
                                               ;; (backslash removal happens during IFS split)
                                               (display #\\ buf)
                                               (display next buf)
                                               (rloop)))))
                                         (else
                                          (display ch buf)
                                          (rloop)))))))))))
                 ;; Reset timeout
                 (when timeout
                   (input-port-timeout-set! in-port +inf.0))
                 (if (eof-object? line)
                   1
                   (let* ((var-names (cond
                                      (array-name [array-name])
                                      ((null? vars) ["REPLY"])
                                      (else (reverse vars))))
                          (use-reply? (and (null? vars) (not array-name)))
                          (ifs (or (env-get env "IFS") " \t\n")))
                     (cond
                       ;; -N mode: no IFS splitting, assign directly to first var
                       (nchars-raw?
                        (env-set! env (car var-names) line)
                        ;; Clear remaining vars
                        (for-each (lambda (v) (env-set! env v "")) (cdr var-names))
                        (if got-eof? 1 0))
                       ;; No vars specified: store in REPLY without splitting
                       ;; In non-raw mode, strip backslashes for REPLY
                       (use-reply?
                        (let ((val (if raw? line (read-strip-backslashes line))))
                          (env-set! env "REPLY" val)
                          (if got-eof? 1 0)))
                       ;; Array mode: split into all fields with IFS
                       (array-name
                        (let ((fields (if raw?
                                       (read-ifs-split-raw line ifs 0)
                                       (read-ifs-split line ifs 0))))
                          (env-array-set-compound! env array-name fields #f)
                          (if got-eof? 1 0)))
                       ;; Regular variable assignment with IFS splitting
                       (else
                        (let ((fields (if raw?
                                       (read-ifs-split-raw line ifs (length var-names))
                                       (read-ifs-split line ifs (length var-names)))))
                          (let field-loop ((names var-names) (fields fields))
                            (cond
                              ((null? names) (if got-eof? 1 0))
                              ((null? fields)
                               (env-set! env (car names) "")
                               (field-loop (cdr names) []))
                              ((null? (cdr names))
                               ;; Last var gets remainder
                               (env-set! env (car names)
                                         (string-join-sp fields))
                               (if got-eof? 1 0))
                              (else
                               (env-set! env (car names) (car fields))
                               (field-loop (cdr names) (cdr fields)))))))))))))) ;; close begin + if -t 0
             (lambda ()
               ;; Restore echo if we disabled it
               (when tty?
                 (with-catch void
                   (lambda () (tty-mode-set! in-port #t #t #f #f 0))))
               ;; Close fd port if we opened one
               (when fd (close-input-port in-port))))))
        ;; Parse options — support smooshed flags like -rn5, -rd''
        ((and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-))
         (let* ((arg (car args))
                (rest (cdr args)))
           (let parse-flags ((i 1))
             (if (>= i (string-length arg))
               (loop rest raw? silent? prompt nchars nchars-raw? delim timeout fd array-name vars)
               (let ((ch (string-ref arg i)))
                 (case ch
                   ((#\r) (set! raw? #t) (parse-flags (+ i 1)))
                   ((#\s) (set! silent? #t) (parse-flags (+ i 1)))
                   ((#\e) (parse-flags (+ i 1))) ;; -e ignored (readline, not relevant)
                   ((#\n)
                    ;; -nNUM or -n (next arg is count)
                    (let ((num-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length num-str) 0)
                        (let ((n (string->number num-str)))
                          (cond
                            ((not n)
                             (fprintf (current-error-port) "read: ~a: invalid number~n" num-str)
                             2)
                            ((< n 0)
                             (fprintf (current-error-port) "read: ~a: invalid number~n" num-str)
                             2)
                            (else
                             (set! nchars n)
                             (loop rest raw? silent? prompt nchars nchars-raw?
                                   delim timeout fd array-name vars))))
                        (if (pair? rest)
                          (let ((n (string->number (car rest))))
                            (cond
                              ((not n)
                               (fprintf (current-error-port) "read: ~a: invalid number~n" (car rest))
                               2)
                              ((< n 0)
                               (fprintf (current-error-port) "read: ~a: invalid number~n" (car rest))
                               2)
                              (else
                               (set! nchars n)
                               (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\N)
                    ;; -NNUM or -N (next arg is count) — raw char read
                    (let ((num-str (substring arg (+ i 1) (string-length arg))))
                      (set! nchars-raw? #t)
                      (if (> (string-length num-str) 0)
                        (begin (set! nchars (or (string->number num-str) 1))
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! nchars (or (string->number (car rest)) 1))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\p)
                    ;; -p PROMPT (next arg or rest of this arg)
                    (let ((p-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length p-str) 0)
                        (begin (set! prompt p-str)
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! prompt (car rest))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\d)
                    ;; -d DELIM (next arg or rest of this arg)
                    (let ((d-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length d-str) 0)
                        (begin (set! delim d-str)
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! delim (car rest))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\t)
                    ;; -t TIMEOUT
                    (let ((t-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length t-str) 0)
                        (let ((t (string->number t-str)))
                          (if (and t (>= t 0))
                            (begin (set! timeout t)
                                   (loop rest raw? silent? prompt nchars nchars-raw?
                                         delim timeout fd array-name vars))
                            (begin
                              (fprintf (current-error-port) "read: ~a: invalid timeout specification~n" t-str)
                              2)))
                        (if (pair? rest)
                          (let ((t (string->number (car rest))))
                            (if (and t (>= t 0))
                              (begin (set! timeout t)
                                     (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                           delim timeout fd array-name vars))
                              (begin
                                (fprintf (current-error-port) "read: ~a: invalid timeout specification~n" (car rest))
                                2)))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\u)
                    ;; -u FD
                    (let ((u-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length u-str) 0)
                        (let ((n (string->number u-str)))
                          (if (and n (>= n 0))
                            (begin (set! fd n)
                                   (loop rest raw? silent? prompt nchars nchars-raw?
                                         delim timeout fd array-name vars))
                            (begin
                              (fprintf (current-error-port) "read: ~a: invalid file descriptor specification~n" u-str)
                              2)))
                        (if (pair? rest)
                          (let ((n (string->number (car rest))))
                            (if (and n (>= n 0))
                              (begin (set! fd n)
                                     (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                           delim timeout fd array-name vars))
                              (begin
                                (fprintf (current-error-port) "read: ~a: invalid file descriptor specification~n" (car rest))
                                2)))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\a)
                    ;; -a ARRAY
                    (let ((a-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length a-str) 0)
                        (begin (set! array-name a-str)
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! array-name (car rest))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   (else
                    ;; Unknown flag — error
                    (fprintf (current-error-port) "read: -~a: invalid option~n" ch)
                    2)))))))
        (else
         (loop (cdr args) raw? silent? prompt nchars nchars-raw?
               delim timeout fd array-name (cons (car args) vars))))))

;; Strip backslashes in non-raw read mode
;; In read without -r, backslash-char becomes just char (backslash removed)
;; This does NOT interpret C escapes (\n, \t, etc.) — only removes the backslash
(def (read-strip-backslashes s)
  (let ((len (string-length s))
        (buf (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string buf))
        ((and (char=? (string-ref s i) #\\) (< (+ i 1) len))
         (display (string-ref s (+ i 1)) buf)
         (loop (+ i 2)))
        ((and (char=? (string-ref s i) #\\) (= (+ i 1) len))
         ;; Trailing backslash - keep it
         (display #\\ buf)
         (loop (+ i 1)))
        (else
         (display (string-ref s i) buf)
         (loop (+ i 1)))))))

;; IFS splitting for read builtin (raw mode — no backslash processing)
;; Rules:
;; - IFS whitespace chars (space, tab, newline) collapse into single delimiters
;; - Non-whitespace IFS chars are individual delimiters (create empty fields)
;; - Leading IFS whitespace is stripped
;; - Trailing IFS whitespace is stripped
;; - When max-fields > 0 and reached, rest goes to last field
;; - When max-fields = 0, split into unlimited fields
(def (read-ifs-split-raw str ifs max-fields)
  (let ((len (string-length str)))
    (if (= len 0)
      []
      (let ((ifs-ws (ifs-whitespace-chars ifs))
            (ifs-nw (ifs-non-whitespace-chars ifs)))
        ;; Skip leading IFS whitespace
        (let skip-lead ((i 0))
          (if (and (< i len) (ifs-ws-member? (string-ref str i) ifs-ws))
            (skip-lead (+ i 1))
            ;; Now split from position i
            (read-ifs-split-from str i len ifs-ws ifs-nw max-fields)))))))

;; IFS splitting for read builtin (non-raw mode — backslash processing during split)
;; Backslash escapes the next character, preventing it from being treated as IFS
;; The backslash itself is removed from the output
(def (read-ifs-split str ifs max-fields)
  (let ((len (string-length str)))
    (if (= len 0)
      []
      (let ((ifs-ws (ifs-whitespace-chars ifs))
            (ifs-nw (ifs-non-whitespace-chars ifs)))
        ;; Skip leading IFS whitespace (backslash-space is NOT whitespace)
        (let skip-lead ((i 0))
          (if (and (< i len)
                   (not (char=? (string-ref str i) #\\))
                   (ifs-ws-member? (string-ref str i) ifs-ws))
            (skip-lead (+ i 1))
            ;; Now split with backslash processing
            (read-ifs-split-bs str i len ifs-ws ifs-nw max-fields)))))))

;; Helper: extract IFS whitespace characters (space, tab, newline in IFS)
(def (ifs-whitespace-chars ifs)
  (let ((buf (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length ifs))
        (get-output-string buf)
        (let ((ch (string-ref ifs i)))
          (when (or (char=? ch #\space) (char=? ch #\tab) (char=? ch #\newline))
            (display ch buf))
          (loop (+ i 1)))))))

;; Helper: extract IFS non-whitespace characters
(def (ifs-non-whitespace-chars ifs)
  (let ((buf (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length ifs))
        (get-output-string buf)
        (let ((ch (string-ref ifs i)))
          (when (not (or (char=? ch #\space) (char=? ch #\tab) (char=? ch #\newline)))
            (display ch buf))
          (loop (+ i 1)))))))

(def (ifs-ws-member? ch ws)
  (let loop ((i 0))
    (and (< i (string-length ws))
         (or (char=? ch (string-ref ws i))
             (loop (+ i 1))))))

(def (ifs-nw-member? ch nw)
  (let loop ((i 0))
    (and (< i (string-length nw))
         (or (char=? ch (string-ref nw i))
             (loop (+ i 1))))))

;; Raw IFS splitting (no backslash processing)
(def (read-ifs-split-from str start len ifs-ws ifs-nw max-fields)
  (let ((buf (open-output-string))
        (fields [])
        (field-count 1))
    (let loop ((i start))
      (cond
        ((>= i len)
         ;; End of string: strip trailing IFS for last field
         (let* ((s (get-output-string buf))
                (s (if (> max-fields 0)
                     (strip-trailing-ifs-ws s ifs-ws)
                     (strip-trailing-ifs-for-read s ifs-ws ifs-nw))))
           (reverse (cond
                      ((> (string-length s) 0) (cons s fields))
                      ;; For max-split, keep trailing empty field; for unlimited, drop it
                      ((pair? fields)
                       (if (> max-fields 0) (cons s fields) fields))
                      (else fields)))))
        ;; Non-whitespace IFS delimiter
        ((ifs-nw-member? (string-ref str i) ifs-nw)
         ;; Check if we're at max-fields — if so, include delimiter in current field
         (if (and (> max-fields 0) (>= field-count max-fields))
           (begin (display (string-ref str i) buf) (loop (+ i 1)))
           (let ((s (get-output-string buf)))
             ;; Strip trailing IFS whitespace from current field
             (set! fields (cons (strip-trailing-ifs-ws s ifs-ws) fields))
             (set! buf (open-output-string))
             (set! field-count (+ field-count 1))
             ;; Skip IFS whitespace after non-whitespace delimiter
             (let skip ((j (+ i 1)))
               (if (and (< j len) (ifs-ws-member? (string-ref str j) ifs-ws))
                 (skip (+ j 1))
                 ;; If NOW at max fields, take rest as last field
                 (if (and (> max-fields 0) (>= field-count max-fields))
                   (begin
                     (display (substring str j len) buf)
                     (let* ((s (get-output-string buf))
                            (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                       (reverse (cons s fields))))
                   (loop j)))))))
        ;; IFS whitespace
        ((ifs-ws-member? (string-ref str i) ifs-ws)
         ;; NOTE: get-output-string resets the port in Gambit, so save to var first
         (let ((buf-content (get-output-string buf)))
           (if (and (> max-fields 0) (>= field-count max-fields)
                    (> (string-length buf-content) 0))
             ;; At max fields: include buffered content + rest of string
             (let* ((full (string-append buf-content (substring str i len)))
                    (full (strip-trailing-ifs-ws full ifs-ws)))
               (reverse (cons full fields)))
             (let ((had-content? (> (string-length buf-content) 0)))
               (when had-content?
                 (set! fields (cons buf-content fields))
                 (set! field-count (+ field-count 1)))
               (set! buf (open-output-string))
               ;; Skip consecutive IFS whitespace
               (let skip ((j (+ i 1)))
                 (if (and (< j len) (ifs-ws-member? (string-ref str j) ifs-ws))
                   (skip (+ j 1))
                   ;; POSIX: absorb adjacent non-ws IFS delimiter when ws was a field separator
                   (let ((j (if (and had-content? (< j len)
                                     (ifs-nw-member? (string-ref str j) ifs-nw))
                              ;; Absorb non-ws delimiter + trailing whitespace
                              (let skip2 ((k (+ j 1)))
                                (if (and (< k len) (ifs-ws-member? (string-ref str k) ifs-ws))
                                  (skip2 (+ k 1))
                                  k))
                              j)))
                     ;; If NOW at max fields, take rest as last field
                     (if (and (> max-fields 0) (>= field-count max-fields))
                       (begin
                         (display (substring str j len) buf)
                         (let* ((s (get-output-string buf))
                                (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                           (reverse (cons s fields))))
                       (loop j)))))))))
        ;; Regular character
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1)))))))

;; Non-raw IFS splitting with backslash processing
(def (read-ifs-split-bs str start len ifs-ws ifs-nw max-fields)
  ;; Strategy: keep backslashes in buffer during splitting, so that escaped chars
  ;; don't get stripped by trailing-IFS removal. Do backslash removal AFTER stripping.
  ;; Helper to append raw rest of string (with backslashes) to buf
  (define (append-rest j buf)
    (let rloop ((j j))
      (when (< j len)
        (display (string-ref str j) buf)
        (rloop (+ j 1)))))
  ;; Finalize a last-field (unlimited split): strip trailing IFS (ws + nw), then backslashes
  (define (finalize-last-field raw-s)
    (read-strip-backslashes (strip-trailing-ifs-for-read raw-s ifs-ws ifs-nw)))
  ;; Finalize the last field in max-split mode: strip trailing IFS (ws + nw)
  (define (finalize-max-last-field raw-s)
    (read-strip-backslashes (strip-trailing-ifs-for-read raw-s ifs-ws ifs-nw)))
  ;; Finalize a non-last field: strip trailing IFS-ws, then remove backslashes
  (define (finalize-field raw-s)
    (read-strip-backslashes (strip-trailing-ifs-ws raw-s ifs-ws)))
  (let ((buf (open-output-string))
        (fields [])
        (field-count 1))
    (let loop ((i start))
      (cond
        ((>= i len)
         ;; End of string: strip trailing IFS for last field, then remove backslashes
         (let* ((raw-s (get-output-string buf))
                (s (if (> max-fields 0)
                     (finalize-max-last-field raw-s)
                     (finalize-last-field raw-s))))
           (reverse (cond
                      ((> (string-length s) 0) (cons s fields))
                      ;; For max-split, keep trailing empty field (named vars need it)
                      ;; For unlimited split, drop trailing empty fields
                      ((pair? fields)
                       (if (> max-fields 0) (cons s fields) fields))
                      (else fields)))))
        ;; Backslash: keep in buffer (prevents IFS matching of next char)
        ((and (char=? (string-ref str i) #\\) (< (+ i 1) len))
         (display #\\ buf)
         (display (string-ref str (+ i 1)) buf)
         (loop (+ i 2)))
        ;; Trailing backslash
        ((and (char=? (string-ref str i) #\\) (= (+ i 1) len))
         (display #\\ buf)
         (loop (+ i 1)))
        ;; Non-whitespace IFS delimiter
        ((ifs-nw-member? (string-ref str i) ifs-nw)
         ;; Check if we're at max-fields — if so, include delimiter in current field
         (if (and (> max-fields 0) (>= field-count max-fields))
           (begin (display (string-ref str i) buf) (loop (+ i 1)))
           (let ((raw-s (get-output-string buf)))
             (set! fields (cons (finalize-field raw-s) fields))
             (set! buf (open-output-string))
             (set! field-count (+ field-count 1))
             ;; Skip IFS whitespace after non-whitespace delimiter
             (let skip ((j (+ i 1)))
               (if (and (< j len)
                        (not (char=? (string-ref str j) #\\))
                        (ifs-ws-member? (string-ref str j) ifs-ws))
                 (skip (+ j 1))
                 ;; If NOW at max fields, append rest and finalize
                 (if (and (> max-fields 0) (>= field-count max-fields))
                   (begin
                     (append-rest j buf)
                     (let ((s (finalize-max-last-field (get-output-string buf))))
                       (reverse (cons s fields))))
                   (loop j)))))))
        ;; IFS whitespace
        ((ifs-ws-member? (string-ref str i) ifs-ws)
         ;; NOTE: get-output-string resets the port in Gambit, so save to var first
         (let ((buf-content (get-output-string buf)))
           (if (and (> max-fields 0) (>= field-count max-fields)
                    (> (string-length buf-content) 0))
             ;; At max fields: include buffered content + rest of string, finalize
             (let ((new-buf (open-output-string)))
               (display buf-content new-buf)
               (append-rest i new-buf)
               (let ((s (finalize-max-last-field (get-output-string new-buf))))
                 (reverse (cons s fields))))
             (let ((had-content? (> (string-length buf-content) 0)))
               (when had-content?
                 (set! fields (cons (read-strip-backslashes buf-content) fields))
                 (set! field-count (+ field-count 1)))
               (set! buf (open-output-string))
               ;; Skip consecutive IFS whitespace
               (let skip ((j (+ i 1)))
                 (if (and (< j len)
                          (not (char=? (string-ref str j) #\\))
                          (ifs-ws-member? (string-ref str j) ifs-ws))
                   (skip (+ j 1))
                   ;; POSIX: absorb adjacent non-ws IFS delimiter when ws was a field separator
                   (let ((j (if (and had-content? (< j len)
                                     (not (char=? (string-ref str j) #\\))
                                     (ifs-nw-member? (string-ref str j) ifs-nw))
                              ;; Absorb non-ws delimiter + trailing whitespace
                              (let skip2 ((k (+ j 1)))
                                (if (and (< k len)
                                         (not (char=? (string-ref str k) #\\))
                                         (ifs-ws-member? (string-ref str k) ifs-ws))
                                  (skip2 (+ k 1))
                                  k))
                              j)))
                     ;; If NOW at max fields, append rest and finalize
                     (if (and (> max-fields 0) (>= field-count max-fields))
                       (begin
                         (append-rest j buf)
                         (let ((s (finalize-max-last-field (get-output-string buf))))
                           (reverse (cons s fields))))
                       (loop j)))))))))
        ;; Regular character
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1)))))))

;; Strip trailing IFS whitespace from a string
;; Backslash-aware: stops stripping when a backslash precedes the char
(def (strip-trailing-ifs-ws s ws)
  (if (= (string-length ws) 0)
    s
    (let loop ((end (string-length s)))
      (if (and (> end 0)
               (ifs-ws-member? (string-ref s (- end 1)) ws)
               ;; Don't strip if preceded by backslash (escaped char)
               (not (and (> end 1) (char=? (string-ref s (- end 2)) #\\))))
        (loop (- end 1))
        (substring s 0 end)))))

;; Strip trailing IFS from the last field for read:
;; 1. Strip trailing IFS whitespace
;; 2. If the last char is a non-ws IFS char preceded by a non-IFS char
;;    (not IFS-whitespace and not IFS-non-whitespace), strip that one
;;    trailing non-ws IFS char and any IFS whitespace before it.
;;    This matches bash behavior: trailing non-ws IFS delimiters are only
;;    stripped when adjacent to real content, not when adjacent to other IFS chars.
(def (strip-trailing-ifs-for-read s ifs-ws ifs-nw)
  (let* ((s1 (strip-trailing-ifs-ws s ifs-ws))
         (len (string-length s1)))
    (if (and (> len 0)
             (ifs-nw-member? (string-ref s1 (- len 1)) ifs-nw)
             ;; Don't strip if preceded by backslash (escaped char)
             (not (and (> len 1) (char=? (string-ref s1 (- len 2)) #\\)))
             ;; Only strip if preceded by a non-IFS character (or at position 0)
             (or (= len 1)
                 (let ((prev (string-ref s1 (- len 2))))
                   (and (not (ifs-nw-member? prev ifs-nw))
                        (not (ifs-ws-member? prev ifs-ws))))))
      ;; Strip the trailing non-ws IFS char
      (strip-trailing-ifs-ws (substring s1 0 (- len 1)) ifs-ws)
      s1)))

;; trap ['command'] [signal ...]
(defbuiltin "trap"
    ;; Helper: print a trap entry with canonical signal display name
    (def (print-trap short-name action)
      (let ((display-name (signal-display-name short-name)))
        (if (string? action)
          (displayln (format "trap -- '~a' ~a" action display-name))
          (displayln (format "trap -- '' ~a" display-name)))))
    ;; Helper: normalize signal arg, print error if invalid
    (def (normalize-or-error sig)
      (let ((n (normalize-signal-arg sig)))
        (unless n
          (fprintf (current-error-port) "trap: ~a: invalid signal specification~n" sig))
        n))
    ;; Helper: check if string is an unsigned decimal integer (POSIX heuristic)
    ;; "0", "42", "07" are unsigned integers; "-1", " 42 ", "abc" are not
    (def (unsigned-integer? s)
      (and (> (string-length s) 0)
           (let loop ((i 0))
             (if (>= i (string-length s))
               #t
               (and (char-numeric? (string-ref s i))
                    (loop (+ i 1)))))))
    ;; Helper: set trap action for given signals
    (def (trap-set-action action signals)
      (let ((status 0))
        (for-each
         (lambda (sig)
           (let ((norm (normalize-or-error sig)))
             (if norm
               (cond
                 ((string=? action "-") (trap-set! norm 'default))
                 ((string=? action "") (trap-set! norm 'ignore))
                 (else (trap-set! norm action)))
               (set! status 1))))
         signals)
        status))
    ;; Main dispatch
    (cond
      ((null? args)
       ;; Print all traps
       (for-each
        (lambda (pair) (print-trap (car pair) (cdr pair)))
        (trap-list))
       0)
      ((string=? (car args) "-l")
       ;; List signal names
       (for-each displayln (signal-name-list))
       0)
      ((string=? (car args) "-p")
       ;; Print specific traps
       (let ((sigs (cdr args)))
         (for-each
          (lambda (sig)
            (let ((norm (normalize-or-error sig)))
              (when norm
                (let ((action (trap-get norm)))
                  (when action (print-trap norm action))))))
          (if (pair? sigs) sigs (map car (trap-list)))))
       0)
      ;; Unrecognized -X options (e.g. -1, -e) are errors
      ((and (> (string-length (car args)) 1)
            (char=? (string-ref (car args) 0) #\-)
            (not (string=? (car args) "-"))
            (not (string=? (car args) "--"))
            (not (string=? (car args) "-l"))
            (not (string=? (car args) "-p")))
       (fprintf (current-error-port) "trap: ~a: invalid signal specification~n" (car args))
       1)
      ((string=? (car args) "--")
       ;; Skip --, process rest
       (let ((rest (cdr args)))
         (if (null? rest)
           ;; trap -- alone = print traps
           (begin
             (for-each (lambda (pair) (print-trap (car pair) (cdr pair))) (trap-list))
             0)
           ;; Check if next is also a signal (single-arg clear) or action + signals
           (if (= (length rest) 1)
             ;; Could be: trap -- SIGNAL (clear) or trap -- action (with no signals = error)
             ;; Per bash: trap -- EXIT clears EXIT trap
             (let ((norm (normalize-or-error (car rest))))
               (if norm
                 (begin (trap-set! norm 'default) 0)
                 1))
             (trap-set-action (car rest) (cdr rest))))))
      ;; Single argument (no action, just signal name): clear the trap
      ;; trap EXIT = clear EXIT trap, trap 0 = clear EXIT trap
      ((= (length args) 1)
       (let ((norm (normalize-or-error (car args))))
         (if norm
           (begin (trap-set! norm 'default) 0)
           1)))
      ;; Two or more args: check POSIX unsigned integer heuristic
      ;; If first arg is an unsigned decimal integer, all args are signals to reset
      ((>= (length args) 2)
       (if (unsigned-integer? (car args))
         ;; POSIX: all operands are conditions to reset
         (let ((status 0))
           (for-each
            (lambda (sig)
              (let ((norm (normalize-or-error sig)))
                (if norm
                  (trap-set! norm 'default)
                  (set! status 1))))
            args)
           status)
         ;; Normal case: first arg is action, rest are signals
         (trap-set-action (car args) (cdr args))))
      (else 0)))

;; jobs [-lnprs]
(defbuiltin "jobs"
    (job-update-status!)
    (let ((print-pids? (and (pair? args) (string=? (car args) "-p"))))
      (for-each
       (lambda (job)
         (if print-pids?
           (displayln (job-pgid job))
           (displayln (format "[~a] ~a ~a"
                             (job-id job)
                             (case (job-status job)
                               ((running) "Running")
                               ((stopped) "Stopped")
                               ((done) "Done")
                               ((killed) "Killed")
                               (else "???"))
                             (job-command-text job)))))
       (job-table-list))
      ;; Clean up completed jobs after listing
      (job-table-cleanup!)
      0))

;; fg [jobspec]
(defbuiltin "fg"
    (let ((spec (if (pair? args) (car args) "%%")))
      (let ((job (job-table-get spec)))
        (if job
          (begin
            (displayln (job-command-text job))
            (job-foreground! job))
          (begin
            (fprintf (current-error-port) "fg: ~a: no such job~n" spec)
            1)))))

;; bg [jobspec...]
(defbuiltin "bg"
    (let ((specs (if (null? args) ["%%"] args)))
      (for-each
       (lambda (spec)
         (let ((job (job-table-get spec)))
           (if job
             (job-background! job)
             (fprintf (current-error-port) "bg: ~a: no such job~n" spec))))
       specs)
      0))

;; wait [id...]
(defbuiltin "wait"
    (let ((result
           (cond
             ;; wait with no args - wait for all background jobs
             ((null? args)
              (for-each (lambda (job) (job-wait job)) (job-table-list))
              0)
             ;; wait -n - wait for next job to complete
             ((string=? (car args) "-n")
              (let* ((rest-args (cdr args))
                     ;; Validate any PID args
                     (valid? (let vloop ((a rest-args))
                               (or (null? a)
                                   (let ((arg (car a)))
                                     (if (or (string->number arg)
                                             (and (> (string-length arg) 0)
                                                  (char=? (string-ref arg 0) #\%)))
                                       (vloop (cdr a))
                                       (begin
                                         (fprintf (current-error-port) "wait: `~a': not a pid or valid job spec~n" arg)
                                         #f)))))))
                (if (not valid?)
                  127
                  (let ((jobs (if (null? rest-args)
                               (job-table-list)
                               ;; Look up specific jobs
                               (filter identity
                                 (map (lambda (a) (job-table-get a)) rest-args)))))
                    (if (null? jobs)
                      127
                      (job-wait-any jobs))))))
             ;; Wait for specific jobs
             (else
              (let loop ((args args) (status 0))
               (if (null? args)
                 status
                 (let* ((arg (car args))
                        (valid-arg? (or (string->number arg)
                                        (and (> (string-length arg) 0)
                                             (char=? (string-ref arg 0) #\%)))))
                   (if (not valid-arg?)
                     ;; Invalid argument - not a PID or job spec
                     (begin
                       (fprintf (current-error-port) "wait: `~a': not a pid or valid job spec~n" arg)
                       (loop (cdr args) 2))
                     (let ((job (job-table-get arg)))
                       (if job
                         (loop (cdr args) (job-wait job))
                         (loop (cdr args) 127)))))))))))
      ;; Clean up completed jobs
      (job-table-cleanup!)
      result))

;; kill [-signal] pid|jobspec
(defbuiltin "kill"
    (if (null? args)
      (begin
        (fprintf (current-error-port) "kill: usage: kill [-signal] pid|jobspec~n")
        1)
      (let loop ((args args) (sig SIGTERM))
        (cond
          ((null? args) 0)
          ((and (> (string-length (car args)) 1)
                (char=? (string-ref (car args) 0) #\-))
           (let* ((sig-name (substring (car args) 1 (string-length (car args))))
                  (sig-num (or (signal-name->number sig-name)
                              (string->number sig-name)
                              SIGTERM)))
             (loop (cdr args) sig-num)))
          (else
           (let ((self-pid (ffi-getpid))
                 (sent-to-self? #f))
             (for-each
              (lambda (target)
                (cond
                  ((char=? (string-ref target 0) #\%)
                   (let ((job (job-table-get target)))
                     (when job
                       (for-each
                        (lambda (proc)
                          (kill (job-process-pid proc) sig))
                        (job-processes job)))))
                  (else
                   (let ((pid (string->number target)))
                     (when pid
                       (when (= pid self-pid) (set! sent-to-self? #t))
                       (kill pid sig))))))
              args)
             ;; When we sent a signal to ourselves, give Gambit's signal
             ;; handler thread time to run so the trap can fire promptly
             (when sent-to-self?
               (thread-sleep! 0.001))
             0))))))

;; history [n]
(defbuiltin "history"
    (cond
      ;; Too many arguments
      ((and (pair? args) (pair? (cdr args)))
       (fprintf (current-error-port) "gsh: history: too many arguments~n")
       2)
      ;; Invalid flag (e.g. -5, -c, etc. — anything starting with -)
      ((and (pair? args)
            (> (string-length (car args)) 0)
            (char=? (string-ref (car args) 0) #\-))
       (fprintf (current-error-port) "gsh: history: ~a: invalid option~n" (car args))
       2)
      ;; Non-numeric argument
      ((and (pair? args) (not (string->number (car args))))
       (fprintf (current-error-port) "gsh: history: ~a: numeric argument required~n" (car args))
       2)
      (else
       (let* ((entries (history-list))
              (n (if (pair? args) (or (string->number (car args)) (length entries))
                     (length entries)))
              (to-show (if (< n (length entries))
                         (list-tail entries (- (length entries) n))
                         entries)))
         (let loop ((entries to-show) (num (- (length entries) (length to-show) -1)))
           (when (pair? entries)
             (displayln (format "  ~a  ~a" num (car entries)))
             (loop (cdr entries) (+ num 1))))
         0))))

;; local [-n] [-i] [-r] [-x] var[=value] ...
(defbuiltin "local"
    ;; Handle local -p: print local vars in declare format
    (if (and (pair? args) (string=? (car args) "-p"))
      (let ((specific-names (cdr args)))
        (if (pair? specific-names)
          ;; local -p name ...: print named local vars
          (begin
            (for-each
             (lambda (name)
               (let ((var (hash-get (shell-environment-vars env) name)))
                 (when (and var (shell-var-local? var)
                            (not (eq? (shell-var-value var) +unset-sentinel+)))
                   (display-declare-var name var))))
             specific-names)
            0)
          ;; local -p (no names): print all local vars
          (begin
            (let ((names (sort! (hash-keys (shell-environment-vars env)) string<?)))
              (for-each
               (lambda (name)
                 (let ((var (hash-get (shell-environment-vars env) name)))
                   (when (and var (shell-var-local? var)
                              (not (eq? (shell-var-value var) +unset-sentinel+)))
                     (display-declare-var name var))))
               names))
            0)))
    ;; Bare local (no args): list local vars in simple name=value format
    (if (null? args)
      (begin
        (let ((names (sort! (hash-keys (shell-environment-vars env)) string<?)))
          (for-each
           (lambda (name)
             (let ((var (hash-get (shell-environment-vars env) name)))
               (when (and var (shell-var-local? var)
                          (not (eq? (shell-var-value var) +unset-sentinel+)))
                 (displayln (format "~a=~a" name (shell-var-scalar-value var))))))
           names))
        0)
    (let loop ((args args) (nameref? #f) (integer? #f) (readonly? #f) (export? #f)
               (array? #f) (assoc? #f))
      (cond
        ((null? args) 0)
        ;; Parse flag arguments
        ((and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-)
              (char-alphabetic? (string-ref (car args) 1)))
         (let floop ((i 1) (nr? nameref?) (int? integer?) (ro? readonly?) (ex? export?)
                     (ar? array?) (as? assoc?))
           (if (>= i (string-length (car args)))
             (loop (cdr args) nr? int? ro? ex? ar? as?)
             (let ((ch (string-ref (car args) i)))
               (case ch
                 ((#\n) (floop (+ i 1) #t int? ro? ex? ar? as?))
                 ((#\i) (floop (+ i 1) nr? #t ro? ex? ar? as?))
                 ((#\r) (floop (+ i 1) nr? int? #t ex? ar? as?))
                 ((#\x) (floop (+ i 1) nr? int? ro? #t ar? as?))
                 ((#\a) (floop (+ i 1) nr? int? ro? ex? #t as?))
                 ((#\A) (floop (+ i 1) nr? int? ro? ex? ar? #t))
                 (else (floop (+ i 1) nr? int? ro? ex? ar? as?)))))))
        (else
         ;; Process name or name=value arguments
         ;; Honor allexport: auto-export when set -a is active
         (let ((effective-export? (or export? (env-option? env "allexport"))))
           (for-each
            (lambda (arg)
              (let ((eq-pos (string-find-char* arg #\=)))
                (if eq-pos
                  ;; Args already expanded by expand-declaration-args (no re-expansion)
                  (let* ((name (substring arg 0 eq-pos))
                         (value (substring arg (+ eq-pos 1) (string-length arg))))
                    ;; Check for compound array: value starts with (
                    (if (and (> (string-length value) 0)
                             (char=? (string-ref value 0) #\())
                      ;; Compound array assignment
                      (let* ((inner (if (and (> (string-length value) 1)
                                             (char=? (string-ref value (- (string-length value) 1)) #\)))
                                      (substring value 1 (- (string-length value) 1))
                                      (substring value 1 (string-length value))))
                             (tbl (make-hash-table)))
                        (if assoc?
                          ;; Associative array
                          (let ((elems (parse-array-compound-elements inner)))
                            (for-each
                             (lambda (elem)
                               (let ((bracket-start (string-find-char* elem #\[)))
                                 (if bracket-start
                                   (let* ((bracket-end (string-find-char-from elem #\] (+ bracket-start 1)))
                                          (key (substring elem (+ bracket-start 1) (or bracket-end (string-length elem))))
                                          (eq2 (string-find-char-from elem #\= (or bracket-end 0)))
                                          (val (if eq2 (substring elem (+ eq2 1) (string-length elem)) "")))
                                     (hash-put! tbl key val))
                                   (hash-put! tbl elem ""))))
                             elems)
                            (hash-put! (shell-environment-vars env) name
                                       (make-shell-var tbl effective-export? readonly? #t
                                                      integer? #f #f nameref? #f #t)))
                          ;; Indexed array
                          (let ((elems (parse-array-compound-elements inner)))
                            (let eloop ((es elems) (idx 0))
                              (when (pair? es)
                                (let ((elem (car es)))
                                  (let ((bracket-start (string-find-char* elem #\[)))
                                    (if bracket-start
                                      (let* ((bracket-end (string-find-char-from elem #\] (+ bracket-start 1)))
                                             (key (or (string->number
                                                       (substring elem (+ bracket-start 1)
                                                                  (or bracket-end (string-length elem))))
                                                      idx))
                                             (eq2 (string-find-char-from elem #\= (or bracket-end 0)))
                                             (val (if eq2 (substring elem (+ eq2 1) (string-length elem)) "")))
                                        (hash-put! tbl key val)
                                        (eloop (cdr es) (+ key 1)))
                                      (begin (hash-put! tbl idx elem)
                                             (eloop (cdr es) (+ idx 1))))))))
                            (hash-put! (shell-environment-vars env) name
                                       (make-shell-var tbl effective-export? readonly? #t
                                                      integer? #f #f nameref? #t #f)))))
                      ;; Regular scalar assignment
                      ;; Check if the variable is already readonly in current scope
                      (let ((existing (hash-get (shell-environment-vars env) name)))
                        (when (and existing (shell-var-readonly? existing))
                          (error (format "~a: readonly variable" name)))
                        (hash-put! (shell-environment-vars env) name
                                   (make-shell-var value effective-export? readonly? #t
                                                  integer? #f #f nameref? array? assoc?)))))
                  ;; No value - just declare as local
                  ;; If already exists in this scope, keep it (second 'local foo' is no-op)
                  (let ((existing (hash-get (shell-environment-vars env) arg)))
                    (if existing
                      ;; Already declared locally — apply any new flags but keep value
                      (begin
                        (when nameref? (set! (shell-var-nameref? existing) #t))
                        (when integer? (set! (shell-var-integer? existing) #t))
                        (when readonly? (set! (shell-var-readonly? existing) #t))
                        (when effective-export? (set! (shell-var-exported? existing) #t))
                        (when array? (set! (shell-var-array? existing) #t))
                        (when assoc? (set! (shell-var-assoc? existing) #t))
                        (set! (shell-var-local? existing) #t))
                      ;; New local declaration
                      (hash-put! (shell-environment-vars env) arg
                                 (make-shell-var (if (or array? assoc?) (make-hash-table) +unset-sentinel+)
                                                effective-export? readonly? #t
                                                integer? #f #f nameref? array? assoc?)))))))
            args)
           0)))))))

;; declare/typeset [-aAfFgilnrtux] [-p] [name[=value] ...]
;; Parse compound array elements from the content inside (...).
;; Handles shell quoting: single quotes, double quotes, backslash escapes.
;; Returns a list of element strings.
(def (parse-array-compound-elements inner)
  (let ((len (string-length inner))
        (elems [])
        (buf (open-output-string)))
    ;; in-elem? tracks whether we've started an element (via quotes or content)
    (let loop ((i 0) (in-single? #f) (in-double? #f) (in-elem? #f))
      (cond
        ((>= i len)
         ;; Flush remaining content
         (let ((s (get-output-string buf)))
           (reverse (if (or in-elem? (> (string-length s) 0)) (cons s elems) elems))))
        (in-single?
         (if (char=? (string-ref inner i) #\')
           (loop (+ i 1) #f in-double? #t)
           (begin (display (string-ref inner i) buf) (loop (+ i 1) #t in-double? #t))))
        (in-double?
         (cond
           ((char=? (string-ref inner i) #\")
            (loop (+ i 1) in-single? #f #t))
           ((and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
            (display (string-ref inner (+ i 1)) buf)
            (loop (+ i 2) in-single? #t #t))
           (else (display (string-ref inner i) buf) (loop (+ i 1) in-single? #t #t))))
        ((char=? (string-ref inner i) #\')
         (loop (+ i 1) #t in-double? #t))
        ((char=? (string-ref inner i) #\")
         (loop (+ i 1) in-single? #t #t))
        ((and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
         (display (string-ref inner (+ i 1)) buf)
         (loop (+ i 2) in-single? in-double? #t))
        ;; Whitespace separates elements
        ((or (char=? (string-ref inner i) #\space)
             (char=? (string-ref inner i) #\tab)
             (char=? (string-ref inner i) #\newline))
         (let ((s (get-output-string buf)))
           (if (or in-elem? (> (string-length s) 0))
             (begin
               (set! elems (cons s elems))
               (set! buf (open-output-string))
               (loop (+ i 1) #f #f #f))
             (loop (+ i 1) #f #f #f))))
        (else
         (display (string-ref inner i) buf)
         (loop (+ i 1) in-single? in-double? #t))))))

;; Split inner text of compound array value into raw token strings,
;; preserving quote characters. Splits on unquoted whitespace.
;; E.g., "'' x \"\" ''" → ("''" "x" "\"\"" "''")
(def (parse-array-compound-raw inner)
  (let ((len (string-length inner))
        (tokens []))
    (let loop ((i 0) (start #f) (in-single? #f) (in-double? #f))
      (cond
        ((>= i len)
         (reverse (if start (cons (substring inner start i) tokens) tokens)))
        (in-single?
         (if (char=? (string-ref inner i) #\')
           (loop (+ i 1) start #f in-double?)
           (loop (+ i 1) start #t in-double?)))
        (in-double?
         (cond
           ((char=? (string-ref inner i) #\")
            (loop (+ i 1) start in-single? #f))
           ((and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
            (loop (+ i 2) start in-single? #t))
           (else (loop (+ i 1) start in-single? #t))))
        ((char=? (string-ref inner i) #\')
         (loop (+ i 1) (or start i) #t in-double?))
        ((char=? (string-ref inner i) #\")
         (loop (+ i 1) (or start i) in-single? #t))
        ((and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
         (loop (+ i 2) (or start i) in-single? in-double?))
        ;; Whitespace separates tokens
        ((or (char=? (string-ref inner i) #\space)
             (char=? (string-ref inner i) #\tab)
             (char=? (string-ref inner i) #\newline))
         (if start
           (begin
             (set! tokens (cons (substring inner start i) tokens))
             (loop (+ i 1) #f #f #f))
           (loop (+ i 1) #f #f #f)))
        (else
         (loop (+ i 1) (or start i) in-single? in-double?))))))

(def (builtin-declare args env)
  (let ((flags (make-hash-table))
        (names [])
        (print? #f)
        (remove? #f)
        (func-mode #f)          ;; 'f for -f, 'F for -F
        )
    ;; Parse flags and name arguments
    (let loop ((args args))
      (when (pair? args)
        (let ((arg (car args)))
          (cond
            ;; Flag argument: -xyz or +xyz
            ((and (> (string-length arg) 1)
                  (or (char=? (string-ref arg 0) #\-)
                      (char=? (string-ref arg 0) #\+))
                  (char-alphabetic? (string-ref arg 1)))
             (let ((remove (char=? (string-ref arg 0) #\+)))
               (let floop ((i 1))
                 (when (< i (string-length arg))
                   (let ((ch (string-ref arg i)))
                     (case ch
                       ((#\p) (set! print? #t))
                       ((#\x) (hash-put! flags 'export (not remove)))
                       ((#\r) (hash-put! flags 'readonly (not remove)))
                       ((#\i) (hash-put! flags 'integer (not remove)))
                       ((#\l) (hash-put! flags 'lowercase (not remove)))
                       ((#\u) (hash-put! flags 'uppercase (not remove)))
                       ((#\n) (hash-put! flags 'nameref (not remove)))
                       ((#\a) (hash-put! flags 'array #t))
                       ((#\A) (hash-put! flags 'assoc #t))
                       ((#\g) (hash-put! flags 'global #t))
                       ;; -f: functions, -F: function names only
                       ((#\f) (set! func-mode 'f))
                       ((#\F) (set! func-mode 'F))
                       (else #!void)))
                   (floop (+ i 1))))
               (loop (cdr args))))
            ;; Name or name=value
            (else
             (set! names (cons arg names))
             (loop (cdr args)))))))
    ;; Handle -f/-F: function operations
    (when func-mode
      (if (null? names)
        ;; No names: list all functions
        (begin
          (let ((fnames (sort! (hash-keys (shell-environment-functions env)) string<?)))
            (for-each
             (lambda (fname)
               (if (eq? func-mode 'F)
                 (displayln (format "declare -f ~a" fname))
                 (displayln (format "~a () { ... }" fname))))
             fnames))
          ((return-from-declare) 0))
        ;; With names: check if each function exists
        (let ((status 0))
          (for-each
           (lambda (fname)
             (let ((func (hash-get (shell-environment-functions env) fname)))
               (if func
                 (if (eq? func-mode 'F)
                   ;; declare -F name: with extdebug, show line and file
                   (if (env-shopt? env "extdebug")
                     (let ((lineno (or (shell-function-lineno func) ""))
                           (srcfile (or (shell-function-source-file func) "")))
                       (displayln (format "~a ~a ~a" fname lineno srcfile)))
                     (displayln fname))
                   (displayln (format "~a () { ... }" fname)))
                 (begin
                   (fprintf (current-error-port) "declare: ~a: not found~n" fname)
                   (set! status 1)))))
           (reverse names))
          ((return-from-declare) status))))
    ;; If -p with no names: print all variables with given attributes
    (when (and print? (null? names))
      (let ((target (if (hash-get flags 'global) (env-root env) env)))
        (declare-print-all-vars target flags))
      ((return-from-declare) 0))
    ;; If no flags and no names: print all variables (simple name=value format)
    (when (and (= (hash-length flags) 0) (null? names) (not print?))
      (declare-print-all-vars-simple env)
      ((return-from-declare) 0))
    ;; Process each name
    (let ((status 0))
    (for-each
     (lambda (arg)
       (let* ((eq-pos (string-find-char* arg #\=))
              (name (if eq-pos (substring arg 0 eq-pos) arg))
              ;; Strip array subscript for name validation: name[idx]=val
              (base-name (let ((bracket (string-find-char* name #\[)))
                           (if bracket (substring name 0 bracket) name)))
              ;; Args already expanded by expand-declaration-args (no re-expansion)
              (value (if eq-pos
                       (substring arg (+ eq-pos 1) (string-length arg))
                       #f)))
         ;; Validate variable name (inline check: [a-zA-Z_][a-zA-Z0-9_]*)
         (if (not (and (> (string-length base-name) 0)
                       (let ((ch0 (string-ref base-name 0)))
                         (or (char-alphabetic? ch0) (char=? ch0 #\_)))
                       (let vloop ((vi 1))
                         (or (>= vi (string-length base-name))
                             (let ((ch (string-ref base-name vi)))
                               (and (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))
                                    (vloop (+ vi 1))))))))
           (begin
             (fprintf (current-error-port) "declare: `~a': not a valid identifier~n" arg)
             (set! status 1))
         (if print?
           ;; -p name: print declaration
           ;; -pg: look in global scope only
           ;; When attribute flags (-n/-r/-x) are present with -p and names,
           ;; filter to show only variables matching those attributes.
           (let* ((lookup-env (if (hash-get flags 'global) (env-root env) env))
                  (var (or (env-get-var lookup-env name)
                           (env-get-raw-var lookup-env name)))
                  ;; Check if any attribute filter flags are set
                  (has-attr-filter? (or (hash-get flags 'nameref)
                                       (hash-get flags 'readonly)
                                       (hash-get flags 'export)
                                       (hash-get flags 'integer)
                                       (hash-get flags 'array)
                                       (hash-get flags 'assoc)
                                       (hash-get flags 'uppercase)
                                       (hash-get flags 'lowercase))))
             (if var
               ;; If attribute filters present, only print if var matches
               (when (or (not has-attr-filter?)
                         (and (hash-get flags 'nameref) (shell-var-nameref? var))
                         (and (hash-get flags 'readonly) (shell-var-readonly? var))
                         (and (hash-get flags 'export) (shell-var-exported? var))
                         (and (hash-get flags 'integer) (shell-var-integer? var))
                         (and (hash-get flags 'array) (shell-var-array? var))
                         (and (hash-get flags 'assoc) (shell-var-assoc? var))
                         (and (hash-get flags 'uppercase) (shell-var-uppercase? var))
                         (and (hash-get flags 'lowercase) (shell-var-lowercase? var)))
                 (display-declare-var name var))
               (begin
                 (fprintf (current-error-port) "declare: ~a: not found~n" name)
                 (set! status 1))))
           ;; Apply attributes — use global scope for -g flag
           (let* ((target-env (if (hash-get flags 'global)
                                (env-root env) env))
                  (var (or (hash-get (shell-environment-vars target-env) name)
                           (let ((new-var (make-shell-var (or value +unset-sentinel+)
                                                         #f #f #f #f #f #f #f #f #f)))
                             (hash-put! (shell-environment-vars target-env) name new-var)
                             new-var))))
             ;; Set value if provided
             (when value
               ;; Check for compound array assignment: value starts with (
               (if (and (> (string-length value) 0)
                        (char=? (string-ref value 0) #\())
                 ;; Compound array assignment: parse (elem1 elem2 ...)
                 (let ((inner (if (and (> (string-length value) 1)
                                      (char=? (string-ref value (- (string-length value) 1)) #\)))
                               (substring value 1 (- (string-length value) 1))
                               (substring value 1 (string-length value)))))
                   (let ((tbl (make-hash-table)))
                     (if (hash-get flags 'assoc)
                       ;; Associative array: parse [key]=value pairs
                       (begin
                         (let ((elems (parse-array-compound-elements inner)))
                           (for-each
                            (lambda (elem)
                              (let ((bracket-start (string-find-char* elem #\[)))
                                (if bracket-start
                                  (let* ((bracket-end (string-find-char-from elem #\] (+ bracket-start 1)))
                                         (key (substring elem (+ bracket-start 1) (or bracket-end (string-length elem))))
                                         (eq-pos (string-find-char-from elem #\= (or bracket-end 0)))
                                         (val (if eq-pos (substring elem (+ eq-pos 1) (string-length elem)) "")))
                                    (hash-put! tbl key val))
                                  ;; Plain value — shouldn't happen for -A but handle gracefully
                                  #!void)))
                            elems))
                         (set! (shell-var-value var) tbl)
                         (set! (shell-var-assoc? var) #t)
                         (set! (shell-var-array? var) #f))
                       ;; Indexed array: elements are positional
                       (begin
                         (let ((elems (parse-array-compound-elements inner))
                               (idx 0))
                           (for-each
                            (lambda (elem)
                              ;; Check for [idx]=value syntax
                              (if (and (> (string-length elem) 0)
                                       (char=? (string-ref elem 0) #\[))
                                (let* ((bracket-end (string-find-char* elem #\]))
                                       (key-str (substring elem 1 (or bracket-end (string-length elem))))
                                       (eq-pos (string-find-char-from elem #\= (or bracket-end 0)))
                                       (val (if eq-pos (substring elem (+ eq-pos 1) (string-length elem)) ""))
                                       (key (or (string->number key-str) idx)))
                                  (hash-put! tbl key val)
                                  (set! idx (+ key 1)))
                                (begin
                                  (hash-put! tbl idx elem)
                                  (set! idx (+ idx 1)))))
                            elems))
                         (set! (shell-var-value var) tbl)
                         (set! (shell-var-array? var) #t)))))
                 ;; Regular scalar value
                 (let ((final (apply-var-attrs var value env)))
                   (set! (shell-var-value var) final)
                   (when (shell-var-exported? var)
                     (setenv name final)))))
             ;; Honor allexport: auto-export when set -a is active
             (when (and (env-option? env "allexport")
                        (not (hash-key? flags 'export)))
               (set! (shell-var-exported? var) #t)
               (setenv name (or (shell-var-scalar-value var) "")))
             ;; Apply flags
             (when (hash-get flags 'export)
               (set! (shell-var-exported? var) #t)
               (setenv name (shell-var-value var)))
             (when (hash-key? flags 'export)
               (unless (hash-get flags 'export)
                 (set! (shell-var-exported? var) #f)
                 (ffi-unsetenv name)))
             (when (hash-get flags 'readonly)
               (set! (shell-var-readonly? var) #t))
             (when (hash-key? flags 'readonly)
               (unless (hash-get flags 'readonly)
                 (set! (shell-var-readonly? var) #f)))
             (when (hash-get flags 'integer)
               (set! (shell-var-integer? var) #t)
               ;; Apply arithmetic evaluation to current value
               (set! (shell-var-value var)
                     (apply-var-attrs var (shell-var-value var) env)))
             (when (hash-key? flags 'integer)
               (unless (hash-get flags 'integer)
                 (set! (shell-var-integer? var) #f)))
             (when (hash-get flags 'uppercase)
               (set! (shell-var-uppercase? var) #t)
               (set! (shell-var-lowercase? var) #f)
               (when (string? (shell-var-value var))
                 (set! (shell-var-value var) (string-upcase (shell-var-value var)))))
             (when (hash-get flags 'lowercase)
               (set! (shell-var-lowercase? var) #t)
               (set! (shell-var-uppercase? var) #f)
               (when (string? (shell-var-value var))
                 (set! (shell-var-value var) (string-downcase (shell-var-value var)))))
             (when (hash-get flags 'nameref)
               (set! (shell-var-nameref? var) #t))
             (when (hash-get flags 'array)
               (unless (or (shell-var-array? var) (shell-var-assoc? var))
                 ;; Convert scalar to indexed array
                 (let ((old-val (shell-var-value var)))
                   (let ((tbl (make-hash-table)))
                     (when (and (string? old-val) (> (string-length old-val) 0))
                       (hash-put! tbl 0 old-val))
                     (set! (shell-var-value var) tbl)
                     (set! (shell-var-array? var) #t)))))
             (when (hash-get flags 'assoc)
               (unless (shell-var-assoc? var)
                 (set! (shell-var-value var) (make-hash-table))
                 (set! (shell-var-assoc? var) #t)
                 (set! (shell-var-array? var) #f))))))))  ;; extra close for valid-shell-name? if
     (reverse names))
    status)))

;; Helper: print a variable declaration
;; Collect all variables from the scope chain, with closer scopes taking priority
(def (collect-all-vars env)
  (let ((result (make-hash-table)))
    (let walk ((e env))
      (when e
        ;; Walk parents first so closer scopes override
        (walk (shell-environment-parent e))
        (for ([name . var] (hash->list (shell-environment-vars e)))
          (hash-put! result name var))))
    result))

;; Print all variables matching flags criteria from entire scope chain
(def (declare-print-all-vars env flags)
  (let* ((all-vars (collect-all-vars env))
         (filter-export (hash-get flags 'export))
         (filter-readonly (hash-get flags 'readonly))
         (filter-nameref (hash-get flags 'nameref))
         (filter-array (hash-get flags 'array))
         (filter-assoc (hash-get flags 'assoc))
         (names (sort! (hash-keys all-vars) string<?)))
    (for-each
     (lambda (name)
       (let ((var (hash-get all-vars name)))
         (when (and var
                    (not (eq? (shell-var-value var) +unset-sentinel+))
                    ;; Apply attribute filters
                    (or (not filter-export) (shell-var-exported? var))
                    (or (not filter-readonly) (shell-var-readonly? var))
                    (or (not filter-nameref) (shell-var-nameref? var))
                    (or (not filter-array) (shell-var-array? var))
                    (or (not filter-assoc) (shell-var-assoc? var)))
           (display-declare-var name var))))
     names)))

;; Print all variables in simple name=value format (for bare `declare`)
(def (declare-print-all-vars-simple env)
  (let* ((all-vars (collect-all-vars env))
         (names (sort! (hash-keys all-vars) string<?)))
    (for-each
     (lambda (name)
       (let ((var (hash-get all-vars name)))
         (when (and var (not (eq? (shell-var-value var) +unset-sentinel+)))
           (displayln (format "~a=~a" name (shell-var-scalar-value var))))))
     names)))

;; Build flag string: modifier flags (alphabetical) then type flags (A/a)
;; This matches the order bash uses in declare -p output: -ilnrux then -A/-a
;; e.g. declare -ra arr=() for a readonly indexed array
(def (declare-var-flags var)
  (string-append
   (if (shell-var-integer? var) "i" "")
   (if (shell-var-lowercase? var) "l" "")
   (if (shell-var-nameref? var) "n" "")
   (if (shell-var-readonly? var) "r" "")
   (if (shell-var-uppercase? var) "u" "")
   (if (shell-var-exported? var) "x" "")
   (if (shell-var-assoc? var) "A" "")
   (if (shell-var-array? var) "a" "")))

;; Check if an indexed array is dense (keys are 0, 1, 2, ..., n-1)
(def (array-dense? tbl)
  (let ((keys (hash-keys tbl)))
    (and (pair? keys)
         (let ((n (length keys)))
           (let loop ((i 0))
             (if (>= i n) #t
                 (and (hash-key? tbl i)
                      (loop (+ i 1)))))))))

;; Quote a value for declare -p array element output (bash-compatible quoting)
;; Uses shell-quote-value which produces: bare for safe chars, 'single' for specials, $'...' for control
(def (declare-quote-value val)
  (if (not val) "''"
    (let ((s (if (string? val) val (format "~a" val))))
      (shell-quote-value s))))

;; Quote a scalar value for declare -p output — always uses double-quote form like bash
;; Escapes: $ ` " \ newline within double quotes
(def (declare-quote-scalar val)
  (let* ((s (if (string? val) val (format "~a" val)))
         (len (string-length s))
         (buf (open-output-string))
         (has-control? (let loop ((i 0))
                         (if (>= i len) #f
                           (let ((ch (string-ref s i)))
                             (if (or (char<? ch #\space) (char=? ch #\x7f))
                               #t
                               (loop (+ i 1))))))))
    (if has-control?
      ;; Use $'...' for control chars (bash uses this format)
      (shell-quote-value s)
      ;; Normal double-quote wrapping
      (begin
        (display "\"" buf)
        (let loop ((i 0))
          (when (< i len)
            (let ((ch (string-ref s i)))
              (case ch
                ((#\$ #\` #\" #\\) (display "\\" buf) (display ch buf))
                ((#\newline) (display "\\n" buf))
                (else (display ch buf))))
            (loop (+ i 1))))
        (display "\"" buf)
        (get-output-string buf)))))

(def (display-declare-var name var)
  (let* ((flags (declare-var-flags var))
         (flag-str (if (string=? flags "") "--" (string-append "-" flags))))
    (cond
      ;; Unset variable (declared but no value)
      ((eq? (shell-var-value var) +unset-sentinel+)
       (displayln (format "declare ~a ~a" flag-str name)))
      ((shell-var-array? var)
       ;; Indexed array
       (let ((tbl (shell-var-value var)))
         (display (format "declare ~a ~a=(" flag-str name))
         (when (hash-table? tbl)
           (let* ((keys (sort! (hash-keys tbl) <))
                  (last-key (and (pair? keys) (last keys))))
             (if (array-dense? tbl)
               ;; Dense array: (val1 'val 2' val3) — no indices
               (for-each
                (lambda (k)
                  (display (declare-quote-value (hash-get tbl k)))
                  (unless (equal? k last-key) (display " ")))
                keys)
               ;; Sparse array: ([3]=val [5]='val 2') — with indices
               (for-each
                (lambda (k)
                  (display (format "[~a]=~a" k (declare-quote-value (hash-get tbl k))))
                  (unless (equal? k last-key) (display " ")))
                keys))))
         (displayln ")")))
      ((shell-var-assoc? var)
       ;; Assoc array: declare -A map=(['key1']=val1 ['key2']='val 2')
       (let ((tbl (shell-var-value var)))
         (display (format "declare ~a ~a=(" flag-str name))
         (when (hash-table? tbl)
           (let* ((keys (sort! (map (lambda (k) (if (string? k) k (format "~a" k)))
                                    (hash-keys tbl))
                               string<?))
                  (last-key (and (pair? keys) (last keys))))
             (for-each
              (lambda (k)
                ;; Keys always single-quoted in declare -p: ['key']=value
                (let ((qk (let ((sv (shell-quote-value k)))
                            ;; If shell-quote-value returns bare, wrap in single quotes
                            (if (and (> (string-length sv) 0)
                                     (not (char=? (string-ref sv 0) #\'))
                                     (not (and (> (string-length sv) 1)
                                               (char=? (string-ref sv 0) #\$)
                                               (char=? (string-ref sv 1) #\'))))
                              (string-append "'" sv "'")
                              sv))))
                  (display (format "[~a]=~a" qk (declare-quote-value (hash-get tbl k)))))
                (unless (string=? k last-key) (display " ")))
              keys)))
         (displayln ")")))
      (else
       (displayln (format "declare ~a ~a=~a" flag-str name
                         (declare-quote-value (shell-var-scalar-value var))))))))

;; Continuation for early return from declare
(def return-from-declare (make-parameter #f))

(defbuiltin "declare"
    (let/cc return
      (parameterize ((return-from-declare return))
        (builtin-declare args env))))

(defbuiltin "typeset"
    (let/cc return
      (parameterize ((return-from-declare return))
        (builtin-declare args env))))

;; let expr...
(defbuiltin "let"
    (let loop ((args args) (result 0))
      (if (null? args)
        (if (= result 0) 1 0)  ;; let returns 1 if last expr is 0
        (let ((val (arith-eval-wrapper (car args) env)))
          (loop (cdr args) val)))))

;; shopt [-su] [optname...]
(defbuiltin "shopt"
    (cond
      ((null? args)
       ;; List all shopts
       0)
      ((string=? (car args) "-s")
       (for-each (lambda (name) (env-shopt-set! env name #t)) (cdr args))
       0)
      ((string=? (car args) "-u")
       (for-each (lambda (name) (env-shopt-set! env name #f)) (cdr args))
       0)
      (else 0)))

;; help [pattern]
(defbuiltin "help"
    (if (null? args)
      (begin
        (displayln "gsh - Gerbil Shell")
        (displayln "Built-in commands:")
        (for-each (lambda (name) (display "  ") (displayln name))
                  (builtin-list))
        0)
      0))

;; umask [-S] [mode]
(defbuiltin "umask"
    (if (null? args)
      ;; Display current umask
      (let ((mask (ffi-umask 0)))
        (ffi-umask mask)  ;; restore
        (displayln (format "~4,'0o" mask))
        0)
      ;; Set umask
      (let ((mode (string->number (car args) 8)))
        (if mode
          (begin (ffi-umask mode) 0)
          (begin (fprintf (current-error-port) "umask: ~a: invalid octal number~n" (car args)) 1)))))

;; ulimit [-SHa] [-cdefilmnpqrstuvx] [limit]
;; Resource limit management
(def *ulimit-resources*
  ;; (flag description resource-const block-size)
  ;; Resource constants from sys/resource.h (Linux):
  ;; RLIMIT_CPU=0 DATA=2 FSIZE=1 CORE=4 NOFILE=7 STACK=3 AS=9
  ;; NPROC=6 MEMLOCK=8 LOCKS=10 SIGPENDING=11 MSGQUEUE=12 NICE=13 RTPRIO=14
  '((#\c "core file size          (blocks, -c)" 4 512)
    (#\d "data seg size           (kbytes, -d)" 2 1024)
    (#\f "file size               (blocks, -f)" 1 512)
    (#\i "pending signals                 (-i)" 11 1)
    (#\l "max locked memory       (kbytes, -l)" 8 1024)
    (#\m "max memory size         (kbytes, -m)" 9 1024)
    (#\n "open files                      (-n)" 7 1)
    (#\q "POSIX message queues     (bytes, -q)" 12 1)
    (#\r "real-time priority              (-r)" 14 1)
    (#\s "stack size              (kbytes, -s)" 3 1024)
    (#\t "cpu time               (seconds, -t)" 0 1)
    (#\u "max user processes              (-u)" 6 1)
    (#\v "virtual memory          (kbytes, -v)" 9 1024)
    (#\x "file locks                      (-x)" 10 1)
    (#\e "scheduling priority             (-e)" 13 1)
    (#\p "pipe size            (512 bytes, -p)" #f 1)))  ;; pipe size is read-only

(defbuiltin "ulimit"
    (let ((soft? #f) (hard? #f) (show-all? #f)
          (resource-flag #f) (value #f) (explicit-sh? #f))
      ;; Parse args
      (let loop ((rest args) (seen-resource #f))
        (cond
          ((null? rest)
           ;; Execute
           (let ((flag (or resource-flag #\f)))  ;; default is -f (file size)
             (if show-all?
               ;; -a: show all limits
               (begin
                 (for-each
                  (lambda (entry)
                    (let* ((ch (car entry))
                           (desc (cadr entry))
                           (res-const (caddr entry))
                           (block-size (cadddr entry)))
                      (if (not res-const)
                        (displayln (format "~a 8" desc))  ;; pipe size is always 8
                        (let* ((raw (if (and hard? (not soft?))
                                      (ffi-getrlimit-hard res-const)
                                      (ffi-getrlimit-soft res-const)))
                               (display-val (cond
                                              ((= raw -1) "unlimited")
                                              ((= raw -2) "error")
                                              (else (number->string (quotient raw block-size))))))
                          (displayln (format "~a ~a" desc display-val))))))
                  *ulimit-resources*)
                 0)
               ;; Single resource
               (let* ((entry (assoc flag *ulimit-resources*))
                      (res-const (and entry (caddr entry)))
                      (block-size (and entry (cadddr entry))))
                 (cond
                   ((not entry)
                    (fprintf (current-error-port) "ulimit: invalid option '~a'~n" flag)
                    1)
                   ((not value)
                    ;; Display current value
                    (if (not res-const)
                      (begin (displayln "8") 0)  ;; pipe size
                      (let ((raw (if (and hard? (not soft?))
                                   (ffi-getrlimit-hard res-const)
                                   (ffi-getrlimit-soft res-const))))
                        (cond
                          ((= raw -1) (displayln "unlimited") 0)
                          ((= raw -2) (fprintf (current-error-port) "ulimit: error getting limit~n") 1)
                          (else (displayln (quotient raw block-size)) 0)))))
                   ;; Set value
                   ((not res-const)
                    (fprintf (current-error-port) "ulimit: -p: cannot modify limit~n") 1)
                   (else
                    (let* ((raw-val (cond
                                      ((string=? value "unlimited") -1)
                                      ((string=? value "hard")
                                       (ffi-getrlimit-hard res-const))
                                      ((string=? value "soft")
                                       (ffi-getrlimit-soft res-const))
                                      (else
                                       (let ((n (string->number value)))
                                         (and n (* n block-size))))))
                           ;; When -S or -H was explicitly given, only set that one
                           ;; Otherwise set both (default behavior)
                           (only-soft (and explicit-sh? soft? (not hard?)))
                           (only-hard (and explicit-sh? hard? (not soft?))))
                      (if (not raw-val)
                        (begin
                          (fprintf (current-error-port)
                                   "ulimit: ~a: invalid number~n" value)
                          1)
                        (let ((rc (ffi-setrlimit res-const raw-val raw-val
                                                 only-soft only-hard)))
                          (if (= rc 0) 0
                              (begin
                                (fprintf (current-error-port)
                                         "ulimit: error setting limit~n")
                                1)))))))))))
          ;; Parse flags
          ((string=? (car rest) "-S")
           (set! soft? #t) (set! explicit-sh? #t)
           (loop (cdr rest) seen-resource))
          ((string=? (car rest) "-H")
           (set! hard? #t) (set! explicit-sh? #t)
           (loop (cdr rest) seen-resource))
          ((or (string=? (car rest) "-a") (string=? (car rest) "--all"))
           (if seen-resource
             ;; Can't combine -a with resource flag
             (begin
               (fprintf (current-error-port) "ulimit: ~a: too many arguments~n" (car rest))
               2)
             (begin
               (set! show-all? #t)
               (loop (cdr rest) #t))))  ;; mark as seen-resource to reject further flags
          ((and (> (string-length (car rest)) 1)
                (char=? (string-ref (car rest) 0) #\-))
           ;; Resource flag like -n, -f, etc.
           (if seen-resource
             ;; Multiple resource flags not allowed
             (begin
               (fprintf (current-error-port)
                        "ulimit: ~a: too many arguments~n" (car rest))
               2)
             (let ((ch (string-ref (car rest) 1)))
               (set! resource-flag ch)
               (loop (cdr rest) #t))))
          (else
           ;; Value argument
           (if show-all?
             ;; -a doesn't accept arguments
             (begin
               (fprintf (current-error-port) "ulimit: ~a: too many arguments~n" (car rest))
               2)
             (begin
               (set! value (car rest))
               (loop (cdr rest) seen-resource))))))))

;; dirs [-clpv]
(defbuiltin "dirs"
    (for-each displayln (cons (current-directory) (shell-environment-dir-stack env)))
    0)

;; pushd [dir]
(defbuiltin "pushd"
    (let ((dir (if (pair? args) (car args)
                   ;; Swap top two
                   (if (pair? (shell-environment-dir-stack env))
                     (car (shell-environment-dir-stack env))
                     (begin (fprintf (current-error-port) "pushd: no other directory~n")
                            #f)))))
      (if dir
        (let ((old (current-directory)))
          (with-catch
           (lambda (e)
             (fprintf (current-error-port) "pushd: ~a: No such file or directory~n" dir)
             1)
           (lambda ()
             (current-directory (expand-word-nosplit dir env))
             (set! (shell-environment-dir-stack env)
               (cons old (shell-environment-dir-stack env)))
             (let ((new-pwd (strip-trailing-slash (current-directory))))
               (env-set! env "PWD" new-pwd)
               (*internal-pwd* new-pwd))
             ;; Print stack
             (display (current-directory))
             (for-each (lambda (d) (display " ") (display d))
                       (shell-environment-dir-stack env))
             (newline)
             0)))
        1)))

;; popd [+N|-N]
(defbuiltin "popd"
    (let ((stack (shell-environment-dir-stack env)))
      (if (null? stack)
        (begin (fprintf (current-error-port) "popd: directory stack empty~n") 1)
        (let ((dir (car stack)))
          (set! (shell-environment-dir-stack env) (cdr stack))
          (current-directory dir)
          (env-set! env "OLDPWD" (env-get env "PWD"))
          (let ((new-pwd (strip-trailing-slash (current-directory))))
            (env-set! env "PWD" new-pwd)
            (*internal-pwd* new-pwd))
          (display (current-directory))
          (for-each (lambda (d) (display " ") (display d))
                    (shell-environment-dir-stack env))
          (newline)
          0))))

;; mapfile [-d delim] [-n count] [-O origin] [-s count] [-t] [array]
;; readarray is an alias for mapfile
(def (builtin-mapfile args env)
  (let loop ((args args) (delim "\n") (max-count 0) (origin 0) (skip 0) (trim? #f) (arr-name #f))
    (cond
      ((null? args)
       (let* ((name (or arr-name "MAPFILE"))
              (delim-ch (if (> (string-length delim) 0) (string-ref delim 0) #\newline))
              (lines []))
         ;; Read all lines from stdin
         (let rloop ((lines []) (count 0))
           (let ((line (if (char=? delim-ch #\newline)
                         (read-line)
                         ;; Custom delimiter: read until delim-ch
                         (let ((buf (open-output-string)))
                           (let cloop ()
                             (let ((ch (read-char)))
                               (cond
                                 ((eof-object? ch)
                                  (let ((s (get-output-string buf)))
                                    (if (string=? s "") ch s)))
                                 ((char=? ch delim-ch)
                                  (if trim?
                                    (get-output-string buf)
                                    (begin (display ch buf)
                                           (get-output-string buf))))
                                 (else
                                  (display ch buf)
                                  (cloop)))))))))
             (cond
               ((eof-object? line)
                ;; Done — assign to array
                (let ((kept (if (> skip 0) (list-tail* lines skip) lines)))
                  (let aloop ((lst kept) (idx origin))
                    (when (pair? lst)
                      (env-array-set! env name idx (car lst))
                      (aloop (cdr lst) (+ idx 1)))))
                0)
               ((and (> max-count 0) (>= (- count skip) max-count))
                ;; Reached max count — assign what we have
                (let ((kept (if (> skip 0) (list-tail* lines skip) lines)))
                  (let aloop ((lst kept) (idx origin))
                    (when (pair? lst)
                      (env-array-set! env name idx (car lst))
                      (aloop (cdr lst) (+ idx 1)))))
                0)
               (else
                (let ((line (if (and trim? (char=? delim-ch #\newline))
                              line  ;; read-line already strips newline
                              (if (and (not trim?) (char=? delim-ch #\newline))
                                (string-append line "\n")
                                line))))
                  (rloop (append lines [line]) (+ count 1)))))))))
      ((string=? (car args) "-d")
       (if (pair? (cdr args))
         (loop (cddr args) (cadr args) max-count origin skip trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-n")
       (if (pair? (cdr args))
         (loop (cddr args) delim (or (string->number (cadr args)) 0) origin skip trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-O")
       (if (pair? (cdr args))
         (loop (cddr args) delim max-count (or (string->number (cadr args)) 0) skip trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-s")
       (if (pair? (cdr args))
         (loop (cddr args) delim max-count origin (or (string->number (cadr args)) 0) trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-t")
       (loop (cdr args) delim max-count origin skip #t arr-name))
      (else
       (loop (cdr args) delim max-count origin skip trim? (car args))))))

;; Safe list-tail that returns [] if list is too short
(def (list-tail* lst n)
  (if (or (<= n 0) (null? lst))
    lst
    (list-tail* (cdr lst) (- n 1))))

(builtin-register! "mapfile" builtin-mapfile)
(builtin-register! "readarray" builtin-mapfile)

;;; --- test/[ implementation ---

(def (test-eval args)
  (test-eval-or args))

;; Parse: expr -o expr -o ...
(def (test-eval-or args)
  ;; Find first -o at top level (not inside parens)
  ;; Only treat -o as binary OR if there's something on both sides
  (let split ((rest args) (left []) (depth 0))
    (cond
      ((null? rest)
       (test-eval-and (reverse left)))
      ;; Track ( ) nesting
      ((string=? (car rest) "(")
       (split (cdr rest) (cons (car rest) left) (+ depth 1)))
      ((string=? (car rest) ")")
       (split (cdr rest) (cons (car rest) left) (max 0 (- depth 1))))
      ((and (= depth 0)
            (string=? (car rest) "-o")
            (pair? left)         ;; must have left operand
            (pair? (cdr rest)))  ;; must have right operand
       ;; Evaluate left side; if true, short-circuit
       (let ((lresult (test-eval-and (reverse left))))
         (if (= lresult 0) 0
           (test-eval-or (cdr rest)))))
      (else
       (split (cdr rest) (cons (car rest) left) depth)))))

;; Parse: expr -a expr -a ...
(def (test-eval-and args)
  (let split ((rest args) (left []) (depth 0))
    (cond
      ((null? rest)
       (test-eval-not (reverse left)))
      ;; Track ( ) nesting
      ((string=? (car rest) "(")
       (split (cdr rest) (cons (car rest) left) (+ depth 1)))
      ((string=? (car rest) ")")
       (split (cdr rest) (cons (car rest) left) (max 0 (- depth 1))))
      ((and (= depth 0) (string=? (car rest) "-a"))
       ;; Check if this is unary -a (file exists) or binary -a (AND)
       ;; It's binary -a if we have something on both sides
       (if (and (pair? left) (pair? (cdr rest)))
         (let ((lresult (test-eval-not (reverse left))))
           (if (= lresult 0)
             (test-eval-and (cdr rest))
             1))
         ;; Could be unary, don't split here
         (split (cdr rest) (cons (car rest) left) depth)))
      (else
       (split (cdr rest) (cons (car rest) left) depth)))))

;; Find matching ) for ( at the start of args. Returns index or #f.
(def (test-find-close-paren args)
  (let loop ((rest (cdr args)) (depth 1) (idx 1))
    (cond
      ((null? rest) #f)
      ((string=? (car rest) "(") (loop (cdr rest) (+ depth 1) (+ idx 1)))
      ((string=? (car rest) ")")
       (if (= depth 1) idx
         (loop (cdr rest) (- depth 1) (+ idx 1))))
      (else (loop (cdr rest) depth (+ idx 1))))))

;; Parse: ! expr | ( expr ) | primary
(def (test-eval-not args)
  (let ((len (length args)))
    (cond
      ((= len 0) 1)
      ;; Negation: ! expr
      ((string=? (car args) "!")
       (let ((result (test-eval-not (cdr args))))
         (if (= result 0) 1 (if (= result 1) 0 result))))
      ;; Parenthesized: ( ... ) — find matching close paren
      ((and (string=? (car args) "(") (>= len 3))
       (let ((close-idx (test-find-close-paren args)))
         (if (and close-idx (= close-idx (- len 1)))
           ;; ( expr ) — entire args is parenthesized
           (test-eval (list-head (cdr args) (- close-idx 1)))
           ;; ( not at end — fall through to normal evaluation
           (test-eval-primary args len))))
      ;; Primary expressions
      (else (test-eval-primary args len)))))

;; Handle primary test expressions by argument count
(def (test-eval-primary args len)
  (cond
    ;; 1 arg: non-empty string test
    ((= len 1)
     (if (> (string-length (car args)) 0) 0 1))
    ;; 2 args: unary operator
    ((= len 2)
     (test-unary (car args) (cadr args)))
    ;; 3 args: binary operator OR special forms
    ((= len 3)
     (let ((a (car args)) (b (cadr args)) (c (caddr args)))
       (cond
         ;; ! unary-op arg
         ((string=? a "!")
          (let ((r (test-eval-not (cdr args))))
            (if (= r 0) 1 (if (= r 1) 0 r))))
         ;; ( expr )
         ((and (string=? a "(") (string=? c ")"))
          (test-eval-not (list b)))
         ;; binary operator
         (else (test-binary a b c)))))
    ;; 4 args: ! with 3 arg expr, or ( expr op expr )
    ((= len 4)
     (let ((a (car args)) (d (list-ref args 3)))
       (cond
         ((string=? a "!")
          (let ((r (test-eval (cdr args))))
            (if (= r 0) 1 (if (= r 1) 0 r))))
         ;; ( expr op expr ) - parenthesized binary
         ((and (string=? a "(") (string=? d ")"))
          (test-eval (list (cadr args) (caddr args))))
         (else 2))))
    ;; 5+ args: should have been handled by -a/-o splitting
    ;; Try to handle remaining cases
    (else
     ;; Check for ! at the start
     (if (string=? (car args) "!")
       (let ((r (test-eval (cdr args))))
         (if (= r 0) 1 (if (= r 1) 0 r)))
       2))))

(def (test-unary flag arg)
  (case (string->symbol flag)
    ((-z) (if (= (string-length arg) 0) 0 1))
    ((-n) (if (> (string-length arg) 0) 0 1))
    ((-e -a) (if (file-exists? arg) 0 1))  ;; -a as unary is alias for -e
    ((-f) (if (file-regular? arg) 0 1))
    ((-d) (if (file-directory? arg) 0 1))
    ((-r) (if (file-readable? arg) 0 1))
    ((-w) (if (file-writable? arg) 0 1))
    ((-x) (if (= (ffi-access arg 1) 0) 0 1))  ;; X_OK = 1
    ((-s) (if (file-nonempty? arg) 0 1))
    ((-L -h) (if (file-symlink? arg) 0 1))
    ((-t) (let ((fd (string->number arg)))
             (if (not fd) 2  ;; invalid fd → exit 2
               (if (= (ffi-isatty fd) 1) 0 1))))
    ((-p) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'fifo)) 0 1))
    ((-b) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'block-special)) 0 1))
    ((-c) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'character-special)) 0 1))
    ((-v) ;; True if the named variable is set
     (if (*test-var-fn*)
       (if ((*test-var-fn*) arg) 0 1)
       1))
    ((-o) ;; Shell option test
     (if (*test-option-fn*)
       (if ((*test-option-fn*) arg) 0 1)
       1))
    ((-G) (with-catch (lambda (e) 1)
            (lambda () (if (and (file-exists? arg)
                                (= (file-info-group (file-info arg)) (ffi-getegid))) 0 1))))
    ((-O) (with-catch (lambda (e) 1)
            (lambda () (if (and (file-exists? arg)
                                (= (file-info-owner (file-info arg)) (ffi-geteuid))) 0 1))))
    ((-u) (with-catch (lambda (e) 1)  ;; setuid
            (lambda () (if (and (file-exists? arg)
                                (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o4000)))) 0 1))))
    ((-g) (with-catch (lambda (e) 1)  ;; setgid
            (lambda () (if (and (file-exists? arg)
                                (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o2000)))) 0 1))))
    ((-k) (with-catch (lambda (e) 1)  ;; sticky bit
            (lambda () (if (and (file-exists? arg)
                                (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o1000)))) 0 1))))
    ((-S) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'socket)) 0 1))
    ((-N) (with-catch (lambda (e) 1)  ;; modified since last read
            (lambda ()
              (if (file-exists? arg)
                (let ((fi (file-info arg)))
                  (if (> (time->seconds (file-info-last-modification-time fi))
                         (time->seconds (file-info-last-access-time fi))) 0 1))
                1))))
    (else
     ;; Not a recognized flag, treat as non-empty string test for both args
     ;; This handles cases like [ "str1" "str2" ] which is a syntax error → exit 2
     2)))

;; Parameter for checking variable existence (set by executor)
(def *test-var-fn* (make-parameter #f))
;; Parameter for checking shell options (set by executor)
(def *test-option-fn* (make-parameter #f))

;; Parse a shell integer for [ comparisons: handles decimal, 0x hex, 0 octal, N#val
(def (test-parse-integer str)
  (let ((s (test-string-trim str)))
    (cond
      ((string=? s "") #f)
      ;; Negative
      ((and (> (string-length s) 0) (char=? (string-ref s 0) #\-))
       (let ((n (test-parse-integer (substring s 1 (string-length s)))))
         (and n (- n))))
      ;; [/test treats all numbers as decimal — no 0x hex, no 0 octal
      ;; Plain decimal only
      (else (string->number s)))))

(def (test-string-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i len))
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                  (loop (- i 1)) i))))
    (substring s start end)))

(def (test-binary left op right)
  (case (string->symbol op)
    ((= ==) (if (string=? left right) 0 1))
    ((!=) (if (not (string=? left right)) 0 1))
    ((<) (if (string<? left right) 0 1))
    ((>) (if (string>? left right) 0 1))
    ((-eq) (test-int-cmp = left right))
    ((-ne) (test-int-cmp (lambda (a b) (not (= a b))) left right))
    ((-lt) (test-int-cmp < left right))
    ((-le) (test-int-cmp <= left right))
    ((-gt) (test-int-cmp > left right))
    ((-ge) (test-int-cmp >= left right))
    ((-nt) (if (and (file-exists? left) (file-exists? right)
                     (> (time->seconds (file-info-last-modification-time (file-info left)))
                        (time->seconds (file-info-last-modification-time (file-info right))))) 0 1))
    ((-ot) (if (and (file-exists? left) (file-exists? right)
                     (< (time->seconds (file-info-last-modification-time (file-info left)))
                        (time->seconds (file-info-last-modification-time (file-info right))))) 0 1))
    ((-ef) (if (and (file-exists? left) (file-exists? right)
                     (let ((li (file-info left)) (ri (file-info right)))
                       (and (= (file-info-device li) (file-info-device ri))
                            (= (file-info-inode li) (file-info-inode ri))))) 0 1))
    ;; Binary -a (AND) and -o (OR)
    ((-a) (let ((l (if (> (string-length left) 0) 0 1))
                (r (if (> (string-length right) 0) 0 1)))
            (if (and (= l 0) (= r 0)) 0 1)))
    ((-o) (let ((l (if (> (string-length left) 0) 0 1))
                (r (if (> (string-length right) 0) 0 1)))
            (if (or (= l 0) (= r 0)) 0 1)))
    (else 2)))  ;; Unknown operator → exit 2

;; Integer comparison for test: returns 0 (true), 1 (false), 2 (error)
(def (test-int-cmp cmp left right)
  (let ((l (test-parse-integer left))
        (r (test-parse-integer right)))
    (cond
      ((not l)
       (fprintf (current-error-port) "test: ~a: integer expression expected~n" left)
       2)
      ((not r)
       (fprintf (current-error-port) "test: ~a: integer expression expected~n" right)
       2)
      (else (if (cmp l r) 0 1)))))

;;; --- Helpers ---

;; Display a string as raw bytes (Latin-1), bypassing UTF-8 encoding.
;; For chars < 128, use normal display. For chars >= 128, write the byte
;; directly to the output port's underlying file descriptor.
(def (display-raw-bytes segments)
  ;; segments is a list of: string for normal text, fixnum for raw byte
  (for-each
   (lambda (seg)
     (if (fixnum? seg)
       (write-u8 seg)
       (display seg)))
   segments))

;; Returns (cons segments stop?) — segments is a list of string or fixnum (raw byte)
;; stop? is #t when \c was encountered
(def (echo-expand-escapes str)
  (let ((len (string-length str))
        (buf (open-output-string)))
    ;; Flush buf contents into segments list (in reverse)
    (define (flush-buf! acc)
      (let ((s (get-output-string buf)))
        (set! buf (open-output-string))  ;; reset — critical for Chez (get-output-string doesn't drain)
        (if (string=? s "") acc (cons s acc))))
    (let loop ((i 0) (acc []))
      (cond
        ((>= i len) (cons (reverse (flush-buf! acc)) #f))
        ((and (char=? (string-ref str i) #\\) (< (+ i 1) len))
         (let ((next (string-ref str (+ i 1))))
           (case next
             ((#\n) (display "\n" buf) (loop (+ i 2) acc))
             ((#\t) (display "\t" buf) (loop (+ i 2) acc))
             ((#\r) (display "\r" buf) (loop (+ i 2) acc))
             ((#\a) (display "\a" buf) (loop (+ i 2) acc))
             ((#\b) (display "\b" buf) (loop (+ i 2) acc))
             ((#\e #\E) (display (string (integer->char #x1b)) buf) (loop (+ i 2) acc))
             ((#\f) (display "\x0c;" buf) (loop (+ i 2) acc))
             ((#\v) (display "\x0b;" buf) (loop (+ i 2) acc))
             ((#\\) (display "\\" buf) (loop (+ i 2) acc))
             ((#\0) ;; \0nnn - octal (up to 3 digits after the 0, matching bash echo)
              (let oloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 3)
                         (char>=? (string-ref str j) #\0)
                         (char<=? (string-ref str j) #\7))
                  (oloop (+ j 1)
                         (+ (* val 8) (- (char->integer (string-ref str j))
                                         (char->integer #\0)))
                         (+ count 1))
                  ;; Emit raw byte for octal values
                  (let ((byte (bitwise-and val #xFF)))
                    (loop j (cons byte (flush-buf! acc)))))))
             ((#\x) ;; \xHH - hex byte (up to 2 digits)
              (let hloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 2))
                  (let ((hv (hex-digit-value (string-ref str j))))
                    (if hv
                      (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                      (if (= count 0)
                        (begin (display "\\x" buf) (loop j acc))
                        ;; Emit raw byte for hex values
                        (loop j (cons (bitwise-and val #xFF) (flush-buf! acc))))))
                  (if (= count 0)
                    (begin (display "\\x" buf) (loop j acc))
                    (loop j (cons (bitwise-and val #xFF) (flush-buf! acc)))))))
             ((#\u) ;; \uNNNN - unicode (up to 4 hex digits)
              (let hloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 4))
                  (let ((hv (hex-digit-value (string-ref str j))))
                    (if hv
                      (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                      (if (= count 0)
                        (begin (display "\\u" buf) (loop j acc))
                        (begin (display-unicode-char val buf) (loop j acc)))))
                  (if (= count 0)
                    (begin (display "\\u" buf) (loop j acc))
                    (begin (display-unicode-char val buf) (loop j acc))))))
             ((#\U) ;; \UNNNNNNNN - unicode (up to 8 hex digits)
              (let hloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 8))
                  (let ((hv (hex-digit-value (string-ref str j))))
                    (if hv
                      (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                      (if (= count 0)
                        (begin (display "\\U" buf) (loop j acc))
                        (begin (display-unicode-char val buf) (loop j acc)))))
                  (if (= count 0)
                    (begin (display "\\U" buf) (loop j acc))
                    (begin (display-unicode-char val buf) (loop j acc))))))
             ((#\c) (cons (reverse (flush-buf! acc)) #t))  ;; \c = stop all output
             (else (display "\\" buf) (display (string next) buf) (loop (+ i 2) acc)))))
        (else
         (let* ((ch (string-ref str i))
                (code (char->integer ch)))
           (if (and (>= code raw-byte-base) (<= code (+ raw-byte-base #xFF)))
             ;; PUA raw byte from $'...' — emit as fixnum for raw byte output
             (loop (+ i 1) (cons (- code raw-byte-base) (flush-buf! acc)))
             (begin
               (display ch buf)
               (loop (+ i 1) acc)))))))))

;;; --- Full printf implementation ---

;; Format a printf string with arguments
;; Supports argument recycling: if more args than format specifiers, repeat format
(def (shell-printf fmt args)
  (let ((buf (open-output-u8vector (list char-encoding: 'UTF-8))))
    (parameterize ((*printf-stop* #f))
      (if (null? args)
        ;; No args: just process format escapes once
        (begin (printf-format-once fmt [] buf)
               (get-output-u8vector buf))
        ;; With args: loop until all consumed (argument recycling)
        (let loop ((remaining args))
          (if (*printf-stop*)
            (get-output-u8vector buf)
            (let ((leftover (printf-format-once fmt remaining buf)))
              (if (or (null? leftover)
                      (equal? leftover remaining)) ;; no args consumed → stop
                (get-output-u8vector buf)
                (loop leftover)))))))))

;; Process format string once, consuming args as needed
;; Returns remaining args after one pass through format
(def (printf-format-once fmt args buf)
  (let ((flen (string-length fmt)))
    (let loop ((i 0) (args args))
      (cond
        ((or (>= i flen) (*printf-stop*)) args)
        ;; Format specifier
        ((and (char=? (string-ref fmt i) #\%) (< (+ i 1) flen))
         (let-values (((end consumed-arg) (printf-format-spec fmt (+ i 1) args buf)))
           (loop end consumed-arg)))
        ;; Backslash escape
        ((and (char=? (string-ref fmt i) #\\) (< (+ i 1) flen))
         (let ((end (printf-escape fmt (+ i 1) buf)))
           (loop end args)))
        ;; Regular character
        (else
         (display (string-ref fmt i) buf)
         (loop (+ i 1) args))))))

;; Process a printf format specifier at position i (after the %)
;; Returns (values next-position remaining-args)
(def (printf-format-spec fmt i args buf)
  (let ((flen (string-length fmt)))
    ;; Parse flags: -, +, space, 0, #
    (let flag-loop ((i i) (left-align? #f) (plus-sign? #f) (space-sign? #f)
                    (zero-pad? #f) (alt-form? #f))
      (if (>= i flen)
        (values i args)
        (let ((ch (string-ref fmt i)))
          (cond
            ((char=? ch #\-) (flag-loop (+ i 1) #t plus-sign? space-sign? zero-pad? alt-form?))
            ((char=? ch #\+) (flag-loop (+ i 1) left-align? #t space-sign? zero-pad? alt-form?))
            ((char=? ch #\space) (flag-loop (+ i 1) left-align? plus-sign? #t zero-pad? alt-form?))
            ((char=? ch #\0) (flag-loop (+ i 1) left-align? plus-sign? space-sign? #t alt-form?))
            ((char=? ch #\#) (flag-loop (+ i 1) left-align? plus-sign? space-sign? zero-pad? #t))
            ;; %(strftime)T format — check after flags
            ((char=? ch #\()
             (let ((close (string-find-char-from fmt #\) (+ i 1))))
               (if (and close (< (+ close 1) flen)
                        (char=? (string-ref fmt (+ close 1)) #\T))
                 (let* ((strfmt (substring fmt (+ i 1) close))
                        (arg (if (pair? args) (car args) ""))
                        (rest (if (pair? args) (cdr args) []))
                        (epoch (cond
                                 ((string=? arg "") (inexact->exact (floor (time->seconds (current-time)))))
                                 ((string=? arg "-1") (inexact->exact (floor (time->seconds (current-time)))))
                                 ((string=? arg "-2")
                                  (inexact->exact (floor (time->seconds (current-time)))))
                                 (else (or (string->number arg) 0))))
                        (result (ffi-strftime strfmt epoch)))
                   (display result buf)
                   (values (+ close 2) rest))
                 ;; Not a valid %(...)T format
                 (values i args))))
            (else
             ;; Parse width (may be *)
             (let-values (((width i args) (parse-printf-number fmt i args)))
               ;; Parse precision: .N or just . (=0)
               (let-values (((prec i args)
                             (if (and (< i flen) (char=? (string-ref fmt i) #\.))
                               (let-values (((p j a) (parse-printf-number fmt (+ i 1) args)))
                                 (values (or p 0) j a))  ;; . alone means precision 0
                               (values #f i args))))
                 ;; Check for %(strftime)T after width/precision
                 (if (and (< i flen) (char=? (string-ref fmt i) #\())
                   (let ((close (string-find-char-from fmt #\) (+ i 1))))
                     (if (and close (< (+ close 1) flen)
                              (char=? (string-ref fmt (+ close 1)) #\T))
                       (let* ((strfmt (substring fmt (+ i 1) close))
                              (arg (if (pair? args) (car args) ""))
                              (rest (if (pair? args) (cdr args) []))
                              (epoch (cond
                                       ((string=? arg "") (inexact->exact (floor (time->seconds (current-time)))))
                                       ((string=? arg "-1") (inexact->exact (floor (time->seconds (current-time)))))
                                       ((string=? arg "-2")
                                        (inexact->exact (floor (time->seconds (current-time)))))
                                       (else (or (string->number arg) 0))))
                              (result (ffi-strftime strfmt epoch))
                              ;; Apply precision as truncation length
                              (result (if prec
                                        (substring result 0 (min prec (string-length result)))
                                        result)))
                         (display (pad-string result (or width 0) left-align? #\space) buf)
                         (values (+ close 2) rest))
                       ;; Not a valid %(...)T
                       (values i args)))
                 ;; Conversion specifier
                 (if (>= i flen)
                   (values i args)
                   (let ((spec (string-ref fmt i))
                         (arg (if (pair? args) (car args) ""))
                         (rest (if (pair? args) (cdr args) [])))
                     (case spec
                       ;; %% - literal percent
                       ((#\%) (display "%" buf) (values (+ i 1) args))
                       ;; %s - string
                       ((#\s)
                        (let* ((s (if (string? arg) arg (if (pair? args) arg "")))
                               (s (if prec (substring s 0 (min prec (string-length s))) s)))
                          (display (pad-string s (or width 0) left-align? #\space) buf)
                          (values (+ i 1) rest)))
                       ;; %d %i - signed decimal integer
                       ((#\d #\i)
                        (let* ((n (string->integer-safe arg))
                               ;; Clamp to 64-bit signed range (matching bash)
                               (n (cond ((> n 9223372036854775807) 9223372036854775807)
                                        ((< n -9223372036854775808) -9223372036854775808)
                                        (else n)))
                               (neg? (< n 0))
                               (digits (number->string (abs n)))
                               ;; Precision: minimum number of digits (zero-pad)
                               (digits (if (and prec (> prec (string-length digits)))
                                         (string-append (make-string (- prec (string-length digits)) #\0)
                                                        digits)
                                         digits))
                               ;; Precision of 0 with value 0 produces empty string
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (s (if neg? (string-append "-" digits) digits))
                               (s (if (and plus-sign? (not neg?)) (string-append "+" s) s))
                               (s (if (and space-sign? (not neg?) (not plus-sign?))
                                    (string-append " " s) s))
                               ;; Zero-pad only when no precision specified
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %u - unsigned decimal integer
                       ((#\u)
                        (let* ((n (string->integer-safe arg))
                               (neg? (< n 0))
                               ;; Two's complement for negative numbers (64-bit)
                               (n (if neg? (+ (expt 2 64) n) n))
                               ;; Clamp: if still <= 0 after wrap (overflow) or > max, use ULLONG_MAX
                               (n (cond ((and neg? (<= n 0)) 18446744073709551615)
                                        ((> n 18446744073709551615) 18446744073709551615)
                                        (else n)))
                               (digits (number->string n))
                               (digits (if (and prec (> prec (string-length digits)))
                                         (string-append (make-string (- prec (string-length digits)) #\0)
                                                        digits)
                                         digits))
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string digits (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %o - octal
                       ((#\o)
                        (let* ((n (string->integer-safe arg))
                               ;; Two's complement for negative numbers (64-bit)
                               (n (if (< n 0) (+ (expt 2 64) n) n))
                               ;; Clamp to 64-bit unsigned range
                               (n (if (> n 18446744073709551615) 18446744073709551615 n))
                               (digits (number->string n 8))
                               (digits (if (and prec (> prec (string-length digits)))
                                         (string-append (make-string (- prec (string-length digits)) #\0)
                                                        digits)
                                         digits))
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (s (if (and alt-form? (not (string=? digits ""))
                                           (not (char=? (string-ref digits 0) #\0)))
                                    (string-append "0" digits) digits))
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %x %X - hexadecimal
                       ((#\x #\X)
                        (let* ((n (string->integer-safe arg))
                               ;; Two's complement for negative numbers (64-bit)
                               (n (if (< n 0) (+ (expt 2 64) n) n))
                               ;; Clamp to 64-bit unsigned range
                               (n (if (> n 18446744073709551615) 18446744073709551615 n))
                               (raw (string-downcase (number->string n 16)))
                               (raw (if (char=? spec #\X) (string-upcase raw) raw))
                               (digits (if (and prec (> prec (string-length raw)))
                                         (string-append (make-string (- prec (string-length raw)) #\0)
                                                        raw)
                                         raw))
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (s (if (and alt-form? (not (= n 0)))
                                    (string-append (if (char=? spec #\X) "0X" "0x") digits) digits))
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %c - first byte of first char (bash outputs first byte, not full UTF-8)
                       ((#\c)
                        (when (and (string? arg) (> (string-length arg) 0))
                          (let* ((ch (string-ref arg 0))
                                 (cp (char->integer ch)))
                            (if (< cp 128)
                              (write-u8 cp buf)
                              ;; For multi-byte chars, output only the first byte of UTF-8 encoding
                              (let ((tmp (open-output-u8vector (list char-encoding: 'UTF-8))))
                                (display ch tmp)
                                (let ((bytes (get-output-u8vector tmp)))
                                  (when (> (u8vector-length bytes) 0)
                                    (write-u8 (u8vector-ref bytes 0) buf)))))))
                        (values (+ i 1) rest))
                       ;; %b - interpret backslash escapes in argument
                       ((#\b)
                        (let ((stopped? (printf-interpret-b-escapes (if (string? arg) arg "") buf)))
                          (when stopped? (*printf-stop* #t))
                          (values (+ i 1) rest)))
                       ;; %q - shell-quoted
                       ((#\q)
                        (let ((quoted (shell-quote-string (if (string? arg) arg ""))))
                          (display (pad-string quoted (or width 0) left-align? #\space) buf))
                        (values (+ i 1) rest))
                       ;; %f %e %g and uppercase variants - floating point
                       ((#\f #\F #\e #\g #\E #\G)
                        (let* ((n (string->number-safe arg))
                               (p (or prec 6))
                               (s (format-float n spec p alt-form?)))
                          (display (pad-string s (or width 0) left-align?
                                              (if (and zero-pad? (not left-align?)) #\0 #\space)) buf)
                          (values (+ i 1) rest)))
                       ;; Unknown specifier
                       (else
                        (fprintf (current-error-port) "printf: %~a: invalid format character~n" (string spec))
                        (*printf-conversion-error* #t)
                        (values (+ i 1) args)))))))))))))))

;; Parse a number (or * for arg-supplied width/precision) in format string
(def (parse-printf-number fmt i args)
  (let ((flen (string-length fmt)))
    (if (>= i flen)
      (values #f i args)
      (if (char=? (string-ref fmt i) #\*)
        ;; Width/precision from argument
        (let ((n (if (pair? args) (string->integer-safe (car args)) 0))
              (rest (if (pair? args) (cdr args) [])))
          (values (abs n) (+ i 1) rest))
        ;; Parse digits
        (let digit-loop ((j i) (n 0) (found? #f))
          (if (and (< j flen) (char-numeric? (string-ref fmt j)))
            (digit-loop (+ j 1)
                       (+ (* n 10) (- (char->integer (string-ref fmt j)) 48))
                       #t)
            (values (if found? n #f) j args)))))))

;; Convert u8vector to string, trying UTF-8 first, falling back to Latin-1 (byte-per-char)
(def (u8vector->string-lossy vec)
  (with-catch
    (lambda (_)
      ;; Fallback: each byte becomes its code-point character (Latin-1)
      (list->string (map integer->char (u8vector->list vec))))
    (lambda ()
      (let ((p (open-input-u8vector (list init: vec char-encoding: 'UTF-8))))
        (let loop ((chars '()))
          (let ((ch (read-char p)))
            (if (eof-object? ch)
              (list->string (reverse chars))
              (loop (cons ch chars)))))))))

;; Process a printf backslash escape
;; Helper: convert hex digit char to its integer value, or #f
(def (hex-digit-value ch)
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9)) (- (char->integer ch) 48))
    ((and (char>=? ch #\a) (char<=? ch #\f)) (- (char->integer ch) 87))
    ((and (char>=? ch #\A) (char<=? ch #\F)) (- (char->integer ch) 55))
    (else #f)))

;; Helper: display a unicode code point as UTF-8
(def (display-unicode-char n buf)
  (if (and (>= n 0) (<= n #x10FFFF))
    (display (string (integer->char n)) buf)
    (display (string (integer->char #xFFFD)) buf)))  ;; replacement char for invalid

(def (printf-escape fmt i buf)
  (let ((flen (string-length fmt)))
    (if (>= i flen)
      (begin (display "\\" buf) i)
      (let ((ch (string-ref fmt i)))
        (case ch
          ((#\n) (display "\n" buf) (+ i 1))
          ((#\t) (display "\t" buf) (+ i 1))
          ((#\r) (display "\r" buf) (+ i 1))
          ((#\a) (display "\a" buf) (+ i 1))
          ((#\b) (display "\b" buf) (+ i 1))
          ((#\f) (display "\x0c;" buf) (+ i 1))
          ((#\v) (display "\x0b;" buf) (+ i 1))
          ((#\\) (display "\\" buf) (+ i 1))
          ((#\') (display "'" buf) (+ i 1))
          ((#\") (display "\"" buf) (+ i 1))
          ;; \0nnn - octal (up to 2 digits after the 0, matching bash)
          ((#\0)
           (let octal-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 2)
                      (char>=? (string-ref fmt j) #\0)
                      (char<=? (string-ref fmt j) #\7))
               (octal-loop (+ j 1)
                          (+ (* n 8) (- (char->integer (string-ref fmt j)) 48))
                          (+ count 1))
               (begin (write-u8 (modulo n 256) buf) j))))
          ;; \e / \E - escape (0x1b)
          ((#\e #\E) (display (string (integer->char #x1b)) buf) (+ i 1))
          ;; \xHH - hex byte (raw byte output)
          ((#\x)
           (let hex-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 2))
               (let ((hch (string-ref fmt j)))
                 (cond
                   ((and (char>=? hch #\0) (char<=? hch #\9))
                    (hex-loop (+ j 1) (+ (* n 16) (- (char->integer hch) 48)) (+ count 1)))
                   ((and (char>=? hch #\a) (char<=? hch #\f))
                    (hex-loop (+ j 1) (+ (* n 16) (- (char->integer hch) 87)) (+ count 1)))
                   ((and (char>=? hch #\A) (char<=? hch #\F))
                    (hex-loop (+ j 1) (+ (* n 16) (- (char->integer hch) 55)) (+ count 1)))
                   (else (write-u8 n buf) j)))
               (begin (write-u8 n buf) j))))
          ;; \uNNNN - unicode (4 hex digits)
          ((#\u)
           (let hex-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 4))
               (let* ((hch (string-ref fmt j))
                      (hv (hex-digit-value hch)))
                 (if hv
                   (hex-loop (+ j 1) (+ (* n 16) hv) (+ count 1))
                   (begin (display-unicode-char n buf) j)))
               (begin (display-unicode-char n buf) j))))
          ;; \UNNNNNNNN - unicode (8 hex digits)
          ((#\U)
           (let hex-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 8))
               (let* ((hch (string-ref fmt j))
                      (hv (hex-digit-value hch)))
                 (if hv
                   (hex-loop (+ j 1) (+ (* n 16) hv) (+ count 1))
                   (begin (display-unicode-char n buf) j)))
               (begin (display-unicode-char n buf) j))))
          ;; \nnn - octal without leading 0 (up to 3 digits, raw byte output)
          ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)
           (let octal-loop ((j i) (n 0) (count 0))
             (if (and (< j flen) (< count 3)
                      (char>=? (string-ref fmt j) #\0)
                      (char<=? (string-ref fmt j) #\7))
               (octal-loop (+ j 1)
                          (+ (* n 8) (- (char->integer (string-ref fmt j)) 48))
                          (+ count 1))
               (begin (write-u8 (bitwise-and n #xFF) buf) j))))
          ;; \c - stop output (only used in %b context, but handle here too)
          ((#\c) (+ i 1))  ;; caller should check for this
          (else (display "\\" buf) (display (string ch) buf) (+ i 1)))))))

;; Interpret backslash escapes in a string (for %b format)
;; Returns #t if \c was encountered (stop all output), #f otherwise
;; %b differences from format-string escapes:
;; - \c stops ALL remaining printf output
;; - \NNN (3-digit octal without leading 0) is supported
;; - \0NNN (4-digit with leading 0) is also supported
(def (printf-interpret-b-escapes str buf)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (>= i len)
        #f  ;; normal completion
        (let ((ch (string-ref str i)))
          (if (char=? ch #\\)
            (if (< (+ i 1) len)
              (let ((next-ch (string-ref str (+ i 1))))
                (cond
                  ;; \c - stop all output
                  ((char=? next-ch #\c) #t)
                  ;; \0NNN - 4-digit octal (up to 3 after the 0, raw byte output)
                  ((char=? next-ch #\0)
                   (let oloop ((j (+ i 2)) (val 0) (count 0))
                     (if (and (< j len) (< count 3)
                              (char>=? (string-ref str j) #\0)
                              (char<=? (string-ref str j) #\7))
                       (oloop (+ j 1)
                              (+ (* val 8) (- (char->integer (string-ref str j)) 48))
                              (+ count 1))
                       (begin (write-u8 (modulo val 256) buf)
                              (loop j)))))
                  ;; \NNN - 3-digit octal (without leading 0, 1-7 start, raw byte output)
                  ((and (char>=? next-ch #\1) (char<=? next-ch #\7))
                   (let oloop ((j (+ i 1)) (val 0) (count 0))
                     (if (and (< j len) (< count 3)
                              (char>=? (string-ref str j) #\0)
                              (char<=? (string-ref str j) #\7))
                       (oloop (+ j 1)
                              (+ (* val 8) (- (char->integer (string-ref str j)) 48))
                              (+ count 1))
                       (begin (write-u8 (modulo val 256) buf)
                              (loop j)))))
                  ;; All other escapes: delegate to printf-escape
                  (else
                   (let ((next (printf-escape str (+ i 1) buf)))
                     (loop next)))))
              (begin (display "\\" buf) (loop (+ i 1))))
            (begin (display ch buf) (loop (+ i 1)))))))))

;; Pad a string to a minimum width
(def (pad-string s width left-align? pad-ch)
  (let ((len (string-length s)))
    (if (<= width len)
      s
      (let ((padding (make-string (- width len) pad-ch)))
        (if left-align?
          (string-append s padding)
          ;; For zero-padding with sign, put sign before zeros
          (if (and (char=? pad-ch #\0)
                   (> (string-length s) 0)
                   (or (char=? (string-ref s 0) #\-)
                       (char=? (string-ref s 0) #\+)))
            (string-append (substring s 0 1)
                          padding
                          (substring s 1 (string-length s)))
            (string-append padding s)))))))

;; Convert string to integer safely (handles 0x, 0, 'c' char literal, +prefix, leading spaces)
;; Parameter to track conversion errors during printf
(def *printf-conversion-error* (make-parameter #f))
;; Parameter to signal \c (stop all output) from %b
(def *printf-stop* (make-parameter #f))

(def (string->integer-safe s)
  (if (or (not (string? s)) (string=? s ""))
    0
    ;; Strip leading whitespace
    (let* ((s (let trim ((i 0))
               (if (and (< i (string-length s))
                        (or (char=? (string-ref s i) #\space)
                            (char=? (string-ref s i) #\tab)))
                 (trim (+ i 1))
                 (substring s i (string-length s))))))
      (cond
        ((string=? s "") 0)
        ;; Character literal: 'c or "c -> ASCII value, bare ' or " -> 0
        ((or (char=? (string-ref s 0) #\')
             (char=? (string-ref s 0) #\"))
         (if (>= (string-length s) 2)
           (char->integer (string-ref s 1))
           0))
        ;; Reject base#value syntax (e.g. 64#a) — return leading integer portion
        ((let loop ((i 0)) (and (< i (string-length s))
                                (or (char=? (string-ref s i) #\#) (loop (+ i 1)))))
         (fprintf (current-error-port) "printf: ~a: invalid number~n" s)
         (*printf-conversion-error* #t)
         (parse-leading-integer s))
        (else
         ;; Handle optional sign prefix
         (let* ((neg? (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
                (pos? (and (> (string-length s) 0) (char=? (string-ref s 0) #\+)))
                (s2 (if (or neg? pos?) (substring s 1 (string-length s)) s)))
           ;; Try strict parse first; if it fails, try partial (leading digits)
           (let ((strict-val
                  (cond
                    ((string=? s2 "") #f)
                    ;; Hex: 0x...
                    ((and (>= (string-length s2) 2)
                          (char=? (string-ref s2 0) #\0)
                          (or (char=? (string-ref s2 1) #\x)
                              (char=? (string-ref s2 1) #\X)))
                     (string->number (substring s2 2 (string-length s2)) 16))
                    ;; Octal: 0...
                    ((and (> (string-length s2) 1) (char=? (string-ref s2 0) #\0))
                     (or (string->number s2 8) (string->number s2)))
                    (else (string->number s2)))))
             (if strict-val
               (if neg? (- strict-val) strict-val)
               ;; Partial parse: extract leading digits
               (let ((partial (parse-leading-integer s2)))
                 (fprintf (current-error-port) "printf: ~a: invalid number~n" s)
                 (*printf-conversion-error* #t)
                 (if neg? (- partial) partial))))))))))

;; Parse leading decimal digits from a string, returning the integer value
(def (parse-leading-integer s)
  (let loop ((i 0) (n 0))
    (if (and (< i (string-length s))
             (char>=? (string-ref s i) #\0)
             (char<=? (string-ref s i) #\9))
      (loop (+ i 1) (+ (* n 10) (- (char->integer (string-ref s i)) 48)))
      n)))

;; Convert string to float safely
(def (string->number-safe s)
  (if (string? s) (or (string->number s) 0.0) 0.0))

;; Format a floating-point number
(def (format-float n spec precision (alt-form? #f))
  (let ((n (inexact->exact (if (number? n) n 0))))
    (let ((n (exact->inexact n)))
      (case spec
        ((#\f #\F)
         (let ((s (format-fixed n precision)))
           ;; #flag on %f: always show decimal point (it already does)
           ;; but also preserve trailing dot
           (if alt-form? (ensure-decimal-point s) s)))
        ((#\e #\E) (format-scientific n precision (char=? spec #\E)))
        ((#\g #\G) ;; Use %e if exponent < -4 or >= precision, else %f
         (let* ((p (max 1 (or precision 6)))
                (e (if (= n 0.0) 0 (inexact->exact (floor (/ (log (abs n)) (log 10)))))))
           (if alt-form?
             ;; # flag: don't strip trailing zeros, but ensure decimal point
             (if (or (< e -4) (>= e p))
               (ensure-decimal-point (format-scientific n (- p 1) (char=? spec #\G)))
               (ensure-decimal-point (format-fixed n (max 0 (- p 1 (inexact->exact e))))))
             ;; Normal: strip trailing zeros
             (if (or (< e -4) (>= e p))
               (strip-trailing-zeros (format-scientific n (- p 1) (char=? spec #\G)))
               (strip-trailing-zeros (format-fixed n (max 0 (- p 1 (inexact->exact e)))))))))
        (else (number->string n))))))

;; Remove trailing zeros after decimal point for %g format
(def (string-contains-char? s ch)
  (let loop ((i 0))
    (and (< i (string-length s))
         (or (char=? (string-ref s i) ch) (loop (+ i 1))))))

;; Ensure string has a decimal point (for # flag)
(def (ensure-decimal-point s)
  (if (string-contains-char? s #\.)
    s
    (string-append s ".")))

(def (strip-trailing-zeros s)
  (if (string-contains-char? s #\.)
    (let* ((s (let loop ((i (- (string-length s) 1)))
               (if (and (>= i 0) (char=? (string-ref s i) #\0))
                 (loop (- i 1))
                 (substring s 0 (+ i 1)))))
           ;; Remove trailing dot too
           (s (if (and (> (string-length s) 0)
                       (char=? (string-ref s (- (string-length s) 1)) #\.))
                (substring s 0 (- (string-length s) 1))
                s)))
      s)
    s))

;; Simple fixed-point formatting
(def (format-fixed n precision)
  (let* ((s (number->string (exact->inexact n)))
         (dot-pos (string-find-char* s #\.)))
    (if dot-pos
      (let* ((int-part (substring s 0 dot-pos))
             (frac-part (substring s (+ dot-pos 1) (string-length s)))
             (frac-len (string-length frac-part)))
        (cond
          ((= precision 0) int-part)
          ((<= frac-len precision)
           (string-append int-part "." frac-part
                         (make-string (- precision frac-len) #\0)))
          (else
           (string-append int-part "." (substring frac-part 0 precision)))))
      ;; No decimal point
      (if (= precision 0)
        s
        (string-append s "." (make-string precision #\0))))))

;; Simple scientific notation formatting
(def (format-scientific n precision upper?)
  (if (= n 0.0)
    (string-append "0." (make-string (or precision 6) #\0)
                   (if upper? "E+00" "e+00"))
    (let* ((sign (if (< n 0) -1 1))
           (abs-n (abs n))
           (exp (floor (/ (log abs-n) (log 10))))
           (mantissa (* sign (/ abs-n (expt 10.0 exp))))
           (mstr (format-fixed (exact->inexact mantissa) (or precision 6)))
           (echar (if upper? "E" "e"))
           (esign (if (>= exp 0) "+" "-"))
           (estr (number->string (inexact->exact (abs exp)))))
      (string-append mstr echar esign
                    (if (< (string-length estr) 2)
                      (string-append "0" estr) estr)))))

;; Shell-quote a string for %q — uses quoting style like bash:
;; - No quoting for simple strings (alphanumeric, _, /, ., -, +, etc.)
;; - Single quotes for strings with spaces/shell metachars but no single quotes
;; - $'...' for strings with control chars or single quotes
;; printf %q quoting: bash uses backslash-escaping for metacharacters
;; and $'...' only for control characters
(def (shell-quote-string s)
  (if (string=? s "")
    "''"
    ;; Check if string contains only safe chars (no quoting needed)
    (let* ((has-control?
            (let loop ((i 0))
              (if (>= i (string-length s)) #f
                (let ((ch (string-ref s i)))
                  (if (or (< (char->integer ch) 32) (char=? ch #\x7f))
                    #t (loop (+ i 1)))))))
           (needs-quoting?
            (let loop ((i 0))
              (if (>= i (string-length s)) #f
                (let ((ch (string-ref s i)))
                  (if (or (char-alphabetic? ch) (char-numeric? ch)
                          (char=? ch #\_) (char=? ch #\/) (char=? ch #\.)
                          (char=? ch #\-) (char=? ch #\+) (char=? ch #\,)
                          (char=? ch #\:) (char=? ch #\@) (char=? ch #\%))
                    (loop (+ i 1))
                    #t))))))
      (cond
        ((not needs-quoting?) s)
        (has-control?
         ;; Use $'...' for strings with control chars
         (let ((buf (open-output-string)))
           (display "$'" buf)
           (let loop ((i 0))
             (when (< i (string-length s))
               (let ((ch (string-ref s i)))
                 (cond
                   ((char=? ch #\') (display "\\'" buf))
                   ((char=? ch #\\) (display "\\\\" buf))
                   ((char=? ch #\newline) (display "\\n" buf))
                   ((char=? ch #\tab) (display "\\t" buf))
                   ((char=? ch #\return) (display "\\r" buf))
                   ((char=? ch (integer->char 7)) (display "\\a" buf))
                   ((char=? ch (integer->char 8)) (display "\\b" buf))
                   ((char=? ch (integer->char 27)) (display "\\e" buf))
                   ((or (char=? ch #\x7f) (< (char->integer ch) 32))
                    ;; Use octal \NNN (matching bash $'...' output)
                    (let ((oct (number->string (char->integer ch) 8)))
                      (display (string-append "\\" (make-string (- 3 (string-length oct)) #\0) oct) buf)))
                   (else (display ch buf))))
               (loop (+ i 1))))
           (display "'" buf)
           (get-output-string buf)))
        (else
         ;; Backslash-escape shell metacharacters (bash %q style)
         (let ((buf (open-output-string)))
           (let loop ((i 0))
             (when (< i (string-length s))
               (let ((ch (string-ref s i)))
                 (if (or (char-alphabetic? ch) (char-numeric? ch)
                         (char=? ch #\_) (char=? ch #\/) (char=? ch #\.)
                         (char=? ch #\-) (char=? ch #\+) (char=? ch #\,)
                         (char=? ch #\:) (char=? ch #\@) (char=? ch #\%))
                   (display ch buf)
                   (begin (display #\\ buf) (display ch buf))))
               (loop (+ i 1))))
           (get-output-string buf)))))))


(def (apply-set-options! env flag-str enable?)
  (let ((len (string-length flag-str)))
    (let loop ((i 1))
      (when (< i len)
        (let ((ch (string-ref flag-str i)))
          (case ch
            ((#\e) (env-option-set! env "errexit" enable?))
            ((#\f) (env-option-set! env "noglob" enable?))
            ((#\h) (env-option-set! env "hashall" enable?))
            ((#\m) (env-option-set! env "monitor" enable?))
            ((#\C) (env-option-set! env "noclobber" enable?))
            ((#\u) (env-option-set! env "nounset" enable?))
            ((#\x) (env-option-set! env "xtrace" enable?))
            ((#\v) (env-option-set! env "verbose" enable?))
            ((#\n) (env-option-set! env "noexec" enable?))
            ((#\a) (env-option-set! env "allexport" enable?))
            (else #!void))
          (loop (+ i 1)))))))

(def (split-by-ifs str ifs max-fields)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (fields []) (count 1))
      (cond
        ((>= i len)
         (reverse (if (> i start)
                    (cons (substring str start i) fields)
                    fields)))
        ((and (< count max-fields) (ifs-member? (string-ref str i) ifs))
         (loop (+ i 1) (+ i 1)
               (if (> i start) (cons (substring str start i) fields) fields)
               (+ count 1)))
        (else (loop (+ i 1) start fields count))))))

(def (ifs-member? ch ifs)
  (let loop ((i 0))
    (and (< i (string-length ifs))
         (or (char=? ch (string-ref ifs i))
             (loop (+ i 1))))))

(def (string-join-sp lst)
  (if (null? lst) ""
      (call-with-output-string
        (lambda (port)
          (display (car lst) port)
          (for-each (lambda (s)
                      (display " " port)
                      (display s port))
                    (cdr lst))))))

(def (string-find-char* str ch)
  (let loop ((i 0))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))

(def (list-head lst n)
  (if (or (<= n 0) (null? lst)) []
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

(def (last-elem* lst)
  (if (null? (cdr lst)) (car lst) (last-elem* (cdr lst))))

(def (butlast lst)
  (if (null? (cdr lst)) []
      (cons (car lst) (butlast (cdr lst)))))

(def (env-exported-alist-pairs env)
  (let ((result []))
    (for ([name . var] (hash->list (shell-environment-vars env)))
      (when (shell-var-exported? var)
        (set! result (cons (cons name (or (shell-var-scalar-value var) "")) result))))
    result))

(def (arith-eval-wrapper expr env)
  (with-catch
   (lambda (e) 0)
   (lambda ()
     (arith-eval expr
                 (arith-env-getter env)
                 (arith-env-setter env)))))

;; Import arith-eval from arithmetic module
(import :gsh/arithmetic)
