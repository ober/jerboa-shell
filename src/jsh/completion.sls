#!chezscheme
(library (jsh completion)
  (export *complete-specs* complete-register!
   complete-unregister! complete-get-spec complete-list-specs
   complete-word split-line-for-completion
   completion-current-word completion-word-index
   complete-command complete-file complete-directory
   complete-variable complete-tilde complete-with-spec
   complete-signals compgen-generate string-prefix-match?
   split-path-for-completion string-split-path directory?
   env-var-names sort-strings unique-strings)
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
   (jsh environment) (jsh registry)
   (except (jsh builtins) list-head) (jsh functions)
   (jsh glob))
  (define *complete-specs* (make-hash-table))
  (define (complete-register! command spec)
    (hash-put! *complete-specs* command spec))
  (define (complete-unregister! command)
    (hash-remove! *complete-specs* command))
  (define (complete-get-spec command)
    (hash-get *complete-specs* command))
  (define (complete-list-specs) (hash-keys *complete-specs*))
  (define (complete-word line cursor env)
    (let* ([words (split-line-for-completion line cursor)])
      (let* ([current-word (completion-current-word words)])
        (let* ([word-index (completion-word-index words)])
          (let* ([cmd-name (if (and (pair? words)
                                    (> (length words) 0))
                               (car words)
                               #f)])
            (cond
              [(or (= word-index 0) (not cmd-name))
               (complete-command current-word env)]
              [(and cmd-name (complete-get-spec cmd-name)) =>
               (lambda (spec)
                 (complete-with-spec spec current-word words word-index
                   env))]
              [(and (> (string-length current-word) 0)
                    (char=? (string-ref current-word 0) #\$))
               (complete-variable
                 (substring current-word 1 (string-length current-word))
                 env)]
              [(and (> (string-length current-word) 0)
                    (char=? (string-ref current-word 0) #\~))
               (complete-tilde current-word)]
              [(and cmd-name (string=? cmd-name "cd"))
               (complete-directory current-word)]
              [else (complete-file current-word)]))))))
  (define (split-line-for-completion line cursor)
    (let ([str (substring
                 line
                 0
                 (min cursor (string-length line)))])
      (let loop ([i 0]
                 [words (list)]
                 [current (open-output-string)]
                 [in-quote #f])
        (cond
          [(>= i (string-length str))
           (reverse (cons (get-output-string current) words))]
          [(and (not in-quote) (char-whitespace? (string-ref str i)))
           (let ([word (get-output-string current)])
             (if (> (string-length word) 0)
                 (loop (+ i 1) (cons word words) (open-output-string) #f)
                 (loop (+ i 1) words (open-output-string) #f)))]
          [(and (not in-quote) (char=? (string-ref str i) #\'))
           (loop (+ i 1) words current #\')]
          [(and (not in-quote) (char=? (string-ref str i) #\"))
           (loop (+ i 1) words current #\")]
          [(and in-quote (char=? (string-ref str i) in-quote))
           (loop (+ i 1) words current #f)]
          [else
           (display (string-ref str i) current)
           (loop (+ i 1) words current in-quote)]))))
  (define (completion-current-word words)
    (if (null? words) "" (car (last-pair words))))
  (define (completion-word-index words)
    (max 0 (- (length words) 1)))
  (define (complete-command prefix env)
    (let ([results (list)])
      (for-each
        (lambda (name)
          (when (string-prefix-match? prefix name)
            (set! results (cons name results))))
        (builtin-list))
      (for-each
        (lambda (name)
          (when (string-prefix-match? prefix name)
            (set! results (cons name results))))
        (function-list env))
      (for-each
        (lambda (name)
          (when (string-prefix-match? prefix name)
            (set! results (cons name results))))
        (alias-list env))
      (let ([path-dirs (string-split-path
                         (or (env-get env "PATH") ""))])
        (for-each
          (lambda (dir)
            (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
              (when (file-exists? dir)
                (for-each
                  (lambda (name)
                    (when (and (string-prefix-match? prefix name)
                               (not (member name results)))
                      (let ([full-path (string-append dir "/" name)])
                        (when (executable? full-path)
                          (set! results (cons name results))))))
                  (directory-files dir)))))
          path-dirs))
      (sort-strings (unique-strings results))))
  (define (complete-file prefix)
    (let* ([dir-and-base (split-path-for-completion prefix)])
      (let* ([dir (car dir-and-base)])
        (let* ([base (cdr dir-and-base)])
          (let* ([search-dir (if (string=? dir "") "." dir)])
            (guard (__exn [#t ((lambda (e) (list)) __exn)])
              (if (file-exists? search-dir)
                  (let ([entries (directory-files search-dir)])
                    (let loop ([entries entries] [results (list)])
                      (if (null? entries)
                          (sort-strings results)
                          (let ([name (car entries)])
                            (if (string-prefix-match? base name)
                                (let* ([full-path (if (string=? dir "")
                                                      name
                                                      (string-append
                                                        dir
                                                        name))])
                                  (let* ([display-name (if (directory?
                                                             (if (string=?
                                                                   dir
                                                                   "")
                                                                 name
                                                                 (string-append
                                                                   search-dir
                                                                   "/"
                                                                   name)))
                                                           (string-append
                                                             full-path
                                                             "/")
                                                           full-path)])
                                    (loop
                                      (cdr entries)
                                      (cons display-name results))))
                                (loop (cdr entries) results))))))
                  (list))))))))
  (define (complete-directory prefix)
    (let* ([dir-and-base (split-path-for-completion prefix)])
      (let* ([dir (car dir-and-base)])
        (let* ([base (cdr dir-and-base)])
          (let* ([search-dir (if (string=? dir "") "." dir)])
            (guard (__exn [#t ((lambda (e) (list)) __exn)])
              (if (file-exists? search-dir)
                  (let ([entries (directory-files search-dir)])
                    (let loop ([entries entries] [results (list)])
                      (if (null? entries)
                          (sort-strings results)
                          (let ([name (car entries)])
                            (if (and (string-prefix-match? base name)
                                     (directory?
                                       (if (string=? dir "")
                                           name
                                           (string-append
                                             search-dir
                                             "/"
                                             name))))
                                (let ([full-path (if (string=? dir "")
                                                     name
                                                     (string-append
                                                       dir
                                                       name))])
                                  (loop
                                    (cdr entries)
                                    (cons
                                      (string-append full-path "/")
                                      results)))
                                (loop (cdr entries) results))))))
                  (list))))))))
  (define (complete-variable prefix env)
    (let ([var-names (env-var-names env)])
      (let loop ([names var-names] [results (list)])
        (if (null? names)
            (sort-strings results)
            (let ([name (car names)])
              (if (string-prefix-match? prefix name)
                  (loop
                    (cdr names)
                    (cons (string-append "$" name) results))
                  (loop (cdr names) results)))))))
  (define (complete-tilde prefix)
    (if (string=? prefix "~")
        (list (string-append (home-directory) "/"))
        (list)))
  (define (complete-with-spec spec current-word words
           word-index env)
    (let ([results (list)])
      (let ([wl (hash-get spec 'wordlist)])
        (when (and wl (list? wl))
          (for-each
            (lambda (w)
              (when (string-prefix-match? current-word w)
                (set! results (cons w results))))
            wl)))
      (let ([actions (hash-get spec 'actions)])
        (when (and actions (list? actions))
          (for-each
            (lambda (action)
              (case action
                [(file)
                 (set! results
                   (append (complete-file current-word) results))]
                [(directory)
                 (set! results
                   (append (complete-directory current-word) results))]
                [(command)
                 (set! results
                   (append (complete-command current-word env) results))]
                [(variable)
                 (set! results
                   (append (complete-variable current-word env) results))]
                [(alias)
                 (for-each
                   (lambda (name)
                     (when (string-prefix-match? current-word name)
                       (set! results (cons name results))))
                   (alias-list env))]
                [(builtin)
                 (for-each
                   (lambda (name)
                     (when (string-prefix-match? current-word name)
                       (set! results (cons name results))))
                   (builtin-list))]
                [(signal)
                 (set! results
                   (append (complete-signals current-word) results))]))
            actions)))
      (let ([pat (hash-get spec 'glob)])
        (when (and pat (string? pat))
          (set! results (append (glob-expand pat) results))))
      (let ([opts (or (hash-get spec 'options) (list))])
        (when (and (null? results) (memq 'default opts))
          (set! results (complete-file current-word))))
      (sort-strings (unique-strings results))))
  (define (complete-signals prefix)
    (let ([signals '("HUP" "INT" "QUIT" "ILL" "TRAP" "ABRT" "FPE" "KILL"
                     "SEGV" "PIPE" "ALRM" "TERM" "USR1" "USR2" "CHLD"
                     "CONT" "STOP" "TSTP" "TTIN" "TTOU" "WINCH")])
      (filter
        (lambda (s) (string-prefix-match? prefix s))
        signals)))
  (define (compgen-generate flags prefix env)
    (let ([results (list)])
      (for-each
        (lambda (flag-val)
          (let ([flag (car flag-val)] [val (cdr flag-val)])
            (case flag
              [(#\W)
               (let ([words (string-split-chars val #\space)])
                 (for-each
                   (lambda (w)
                     (when (string-prefix-match? prefix w)
                       (set! results (cons w results))))
                   words))]
              [(#\f)
               (set! results (append (complete-file prefix) results))]
              [(#\d)
               (set! results (append (complete-directory prefix) results))]
              [(#\c)
               (set! results
                 (append (complete-command prefix env) results))]
              [(#\v)
               (set! results
                 (append (complete-variable prefix env) results))]
              [(#\a)
               (for-each
                 (lambda (name)
                   (when (string-prefix-match? prefix name)
                     (set! results (cons name results))))
                 (alias-list env))]
              [(#\b)
               (for-each
                 (lambda (name)
                   (when (string-prefix-match? prefix name)
                     (set! results (cons name results))))
                 (builtin-list))]
              [(#\A)
               (cond
                 [(string=? val "signal")
                  (set! results
                    (append (complete-signals prefix) results))]
                 [(string=? val "file")
                  (set! results (append (complete-file prefix) results))]
                 [(string=? val "directory")
                  (set! results
                    (append (complete-directory prefix) results))]
                 [(string=? val "command")
                  (set! results
                    (append (complete-command prefix env) results))]
                 [(string=? val "variable")
                  (set! results
                    (append (complete-variable prefix env) results))])])))
        flags)
      (sort-strings (unique-strings results))))
  (define (string-prefix-match? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))
  (define (split-path-for-completion path)
    (let ([last-slash (string-last-index-of path #\/)])
      (if last-slash
          (cons
            (substring path 0 (+ last-slash 1))
            (substring path (+ last-slash 1) (string-length path)))
          (cons "" path))))
  (define (string-split-path path-str)
    (string-split-chars path-str #\:))
  (define (directory? path) (file-directory? path))
  (define (env-var-names env)
    (let ([names (list)])
      (let loop ([e env])
        (when e
          (for-each
            (lambda (pair)
              (let ([k (car pair)] [v (cdr pair)])
                (set! names (cons k names))))
            (hash->list (shell-environment-vars e)))
          (loop (shell-environment-parent e))))
      (unique-strings names)))
  (define (sort-strings lst) (sort lst string<?))
  (define (unique-strings lst)
    (let ([seen (make-hash-table)])
      (filter
        (lambda (s)
          (if (hash-get seen s) #f (begin (hash-put! seen s #t) #t)))
        lst))))
