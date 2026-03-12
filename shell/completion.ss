;;; completion.ss — Tab completion engine for gsh
;;; Provides default and programmable completion.

(export #t)
(import :std/sugar
        :std/format
        :std/iter
        :std/sort
        :gsh/util
        :gsh/environment
        :gsh/registry
        :gsh/builtins
        :gsh/functions
        :gsh/glob)

;;; --- Completion registry (programmable completion) ---

;; complete-spec: hash-table mapping command-name -> completion-spec
;; A completion-spec is a hash-table with keys:
;;   'function  -> function name to call
;;   'wordlist  -> list of words
;;   'actions   -> list of action symbols: 'file, 'directory, 'command,
;;                 'variable, 'alias, 'builtin, 'signal, 'user
;;   'glob      -> glob pattern
;;   'options   -> list of option symbols: 'nospace, 'filenames, 'default

(def *complete-specs* (make-hash-table))

(def (complete-register! command spec)
  (hash-put! *complete-specs* command spec))

(def (complete-unregister! command)
  (hash-remove! *complete-specs* command))

(def (complete-get-spec command)
  (hash-get *complete-specs* command))

(def (complete-list-specs)
  (hash-keys *complete-specs*))

;;; --- Public interface ---

;; Generate completions for the current line at cursor position.
;; Returns a list of completion strings.
(def (complete-word line cursor env)
  (let* ((words (split-line-for-completion line cursor))
         (current-word (completion-current-word words))
         (word-index (completion-word-index words))
         (cmd-name (if (and (pair? words) (> (length words) 0))
                     (car words)
                     #f)))
    (cond
      ;; Empty line or first word: complete commands
      ((or (= word-index 0) (not cmd-name))
       (complete-command current-word env))
      ;; Check for programmable completion
      ((and cmd-name (complete-get-spec cmd-name))
       => (lambda (spec)
            (complete-with-spec spec current-word words word-index env)))
      ;; Variable completion after $
      ((and (> (string-length current-word) 0)
            (char=? (string-ref current-word 0) #\$))
       (complete-variable (substring current-word 1 (string-length current-word)) env))
      ;; Tilde completion
      ((and (> (string-length current-word) 0)
            (char=? (string-ref current-word 0) #\~))
       (complete-tilde current-word))
      ;; Special context: after cd, complete directories only
      ((and cmd-name (string=? cmd-name "cd"))
       (complete-directory current-word))
      ;; Default: file completion
      (else
       (complete-file current-word)))))

;;; --- Word splitting for completion ---

(def (split-line-for-completion line cursor)
  ;; Split the line up to cursor into words, respecting quotes
  (let ((str (substring line 0 (min cursor (string-length line)))))
    (let loop ((i 0) (words []) (current (open-output-string)) (in-quote #f))
      (cond
        ((>= i (string-length str))
         (reverse (cons (get-output-string current) words)))
        ((and (not in-quote) (char-whitespace? (string-ref str i)))
         (let ((word (get-output-string current)))
           (if (> (string-length word) 0)
             (loop (+ i 1) (cons word words) (open-output-string) #f)
             (loop (+ i 1) words (open-output-string) #f))))
        ((and (not in-quote) (char=? (string-ref str i) #\'))
         (loop (+ i 1) words current #\'))
        ((and (not in-quote) (char=? (string-ref str i) #\"))
         (loop (+ i 1) words current #\"))
        ((and in-quote (char=? (string-ref str i) in-quote))
         (loop (+ i 1) words current #f))
        (else
         (display (string-ref str i) current)
         (loop (+ i 1) words current in-quote))))))

(def (completion-current-word words)
  (if (null? words) ""
      (car (last-pair words))))

(def (completion-word-index words)
  (max 0 (- (length words) 1)))

;;; --- Command completion ---

(def (complete-command prefix env)
  ;; Complete command names: builtins + functions + aliases + PATH executables
  (let ((results []))
    ;; Builtins
    (for-each
     (lambda (name)
       (when (string-prefix-match? prefix name)
         (set! results (cons name results))))
     (builtin-list))
    ;; Functions
    (for-each
     (lambda (name)
       (when (string-prefix-match? prefix name)
         (set! results (cons name results))))
     (function-list env))
    ;; Aliases
    (for-each
     (lambda (name)
       (when (string-prefix-match? prefix name)
         (set! results (cons name results))))
     (alias-list env))
    ;; PATH executables
    (let ((path-dirs (string-split-path (or (env-get env "PATH") ""))))
      (for-each
       (lambda (dir)
         (with-catch
          (lambda (e) #!void)
          (lambda ()
            (when (file-exists? dir)
              (for-each
               (lambda (name)
                 (when (and (string-prefix-match? prefix name)
                            (not (member name results)))
                   (let ((full-path (string-append dir "/" name)))
                     (when (executable? full-path)
                       (set! results (cons name results))))))
               (directory-files dir))))))
       path-dirs))
    (sort-strings (unique-strings results))))

;;; --- File completion ---

(def (complete-file prefix)
  ;; Complete file and directory names
  (let* ((dir-and-base (split-path-for-completion prefix))
         (dir (car dir-and-base))
         (base (cdr dir-and-base))
         (search-dir (if (string=? dir "") "." dir)))
    (with-catch
     (lambda (e) [])
     (lambda ()
       (if (file-exists? search-dir)
         (let ((entries (directory-files search-dir)))
           (let loop ((entries entries) (results []))
             (if (null? entries)
               (sort-strings results)
               (let ((name (car entries)))
                 (if (string-prefix-match? base name)
                   (let* ((full-path (if (string=? dir "")
                                       name
                                       (string-append dir name)))
                          (display-name
                           (if (directory? (if (string=? dir "")
                                             name
                                             (string-append search-dir "/" name)))
                             (string-append full-path "/")
                             full-path)))
                     (loop (cdr entries) (cons display-name results)))
                   (loop (cdr entries) results))))))
         [])))))

;;; --- Directory completion ---

(def (complete-directory prefix)
  ;; Complete only directory names
  (let* ((dir-and-base (split-path-for-completion prefix))
         (dir (car dir-and-base))
         (base (cdr dir-and-base))
         (search-dir (if (string=? dir "") "." dir)))
    (with-catch
     (lambda (e) [])
     (lambda ()
       (if (file-exists? search-dir)
         (let ((entries (directory-files search-dir)))
           (let loop ((entries entries) (results []))
             (if (null? entries)
               (sort-strings results)
               (let ((name (car entries)))
                 (if (and (string-prefix-match? base name)
                          (directory? (if (string=? dir "")
                                       name
                                       (string-append search-dir "/" name))))
                   (let ((full-path (if (string=? dir "")
                                      name
                                      (string-append dir name))))
                     (loop (cdr entries)
                           (cons (string-append full-path "/") results)))
                   (loop (cdr entries) results))))))
         [])))))

;;; --- Variable completion ---

(def (complete-variable prefix env)
  ;; Complete variable names, prefixing with $
  (let ((var-names (env-var-names env)))
    (let loop ((names var-names) (results []))
      (if (null? names)
        (sort-strings results)
        (let ((name (car names)))
          (if (string-prefix-match? prefix name)
            (loop (cdr names) (cons (string-append "$" name) results))
            (loop (cdr names) results)))))))

;;; --- Tilde completion ---

(def (complete-tilde prefix)
  ;; Complete ~username
  ;; For now, just complete ~ to home directory
  (if (string=? prefix "~")
    [(string-append (home-directory) "/")]
    []))

;;; --- Programmable completion ---

(def (complete-with-spec spec current-word words word-index env)
  (let ((results []))
    ;; Word list
    (let ((wl (hash-get spec 'wordlist)))
      (when (and wl (list? wl))
        (for-each
         (lambda (w)
           (when (string-prefix-match? current-word w)
             (set! results (cons w results))))
         wl)))
    ;; Actions
    (let ((actions (hash-get spec 'actions)))
      (when (and actions (list? actions))
        (for-each
         (lambda (action)
           (case action
             ((file)
              (set! results (append (complete-file current-word) results)))
             ((directory)
              (set! results (append (complete-directory current-word) results)))
             ((command)
              (set! results (append (complete-command current-word env) results)))
             ((variable)
              (set! results (append (complete-variable current-word env) results)))
             ((alias)
              (for-each
               (lambda (name)
                 (when (string-prefix-match? current-word name)
                   (set! results (cons name results))))
               (alias-list env)))
             ((builtin)
              (for-each
               (lambda (name)
                 (when (string-prefix-match? current-word name)
                   (set! results (cons name results))))
               (builtin-list)))
             ((signal)
              (set! results
                    (append (complete-signals current-word) results)))))
         actions)))
    ;; Glob pattern
    (let ((pat (hash-get spec 'glob)))
      (when (and pat (string? pat))
        (set! results (append (glob-expand pat) results))))
    ;; Default fallback
    (let ((opts (or (hash-get spec 'options) [])))
      (when (and (null? results) (memq 'default opts))
        (set! results (complete-file current-word))))
    (sort-strings (unique-strings results))))

;;; --- Signal completion ---

(def (complete-signals prefix)
  (let ((signals '("HUP" "INT" "QUIT" "ILL" "TRAP" "ABRT" "FPE" "KILL"
                   "SEGV" "PIPE" "ALRM" "TERM" "USR1" "USR2" "CHLD"
                   "CONT" "STOP" "TSTP" "TTIN" "TTOU" "WINCH")))
    (filter (lambda (s) (string-prefix-match? prefix s)) signals)))

;;; --- compgen implementation ---

;; Generate completions for `compgen` builtin
;; flags: list of (flag . value) pairs
(def (compgen-generate flags prefix env)
  (let ((results []))
    (for-each
     (lambda (flag-val)
       (let ((flag (car flag-val))
             (val (cdr flag-val)))
         (case flag
           ((#\W)  ;; word list
            (let ((words (string-split-chars val #\space)))
              (for-each
               (lambda (w)
                 (when (string-prefix-match? prefix w)
                   (set! results (cons w results))))
               words)))
           ((#\f)  ;; files
            (set! results (append (complete-file prefix) results)))
           ((#\d)  ;; directories
            (set! results (append (complete-directory prefix) results)))
           ((#\c)  ;; commands
            (set! results (append (complete-command prefix env) results)))
           ((#\v)  ;; variables
            (set! results (append (complete-variable prefix env) results)))
           ((#\a)  ;; aliases
            (for-each
             (lambda (name)
               (when (string-prefix-match? prefix name)
                 (set! results (cons name results))))
             (alias-list env)))
           ((#\b)  ;; builtins
            (for-each
             (lambda (name)
               (when (string-prefix-match? prefix name)
                 (set! results (cons name results))))
             (builtin-list)))
           ((#\A)  ;; -A action
            (cond
              ((string=? val "signal")
               (set! results (append (complete-signals prefix) results)))
              ((string=? val "file")
               (set! results (append (complete-file prefix) results)))
              ((string=? val "directory")
               (set! results (append (complete-directory prefix) results)))
              ((string=? val "command")
               (set! results (append (complete-command prefix env) results)))
              ((string=? val "variable")
               (set! results (append (complete-variable prefix env) results))))))))
     flags)
    (sort-strings (unique-strings results))))

;;; --- Helpers ---

(def (string-prefix-match? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(def (split-path-for-completion path)
  ;; Split a path into (directory . basename) for completion
  ;; Returns ("" . "foo") for "foo", ("dir/" . "bar") for "dir/bar"
  (let ((last-slash (string-last-index-of path #\/)))
    (if last-slash
      (cons (substring path 0 (+ last-slash 1))
            (substring path (+ last-slash 1) (string-length path)))
      (cons "" path))))


(def (string-split-path path-str)
  ;; Split PATH-style colon-separated string
  (string-split-chars path-str #\:))

(def (directory? path)
  (file-directory? path))

(def (env-var-names env)
  ;; Get all variable names from the environment
  ;; Walk the scope chain
  (let ((names []))
    (let loop ((e env))
      (when e
        (for ([k . v] (hash->list (shell-environment-vars e)))
          (set! names (cons k names)))
        (loop (shell-environment-parent e))))
    (unique-strings names)))

(def (sort-strings lst)
  (sort lst string<?))

(def (unique-strings lst)
  (let ((seen (make-hash-table)))
    (filter
     (lambda (s)
       (if (hash-get seen s)
         #f
         (begin (hash-put! seen s #t) #t)))
     lst)))


