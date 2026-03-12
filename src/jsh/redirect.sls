#!chezscheme
(library (jsh redirect)
  (export O_RDONLY O_WRONLY O_RDWR O_CREAT O_TRUNC O_APPEND
   O_NONBLOCK apply-redirections restore-redirections
   apply-redirections-permanent! apply-single-redirect!
   apply-named-fd-redirect! apply-single-redirect-permanent!
   apply-named-fd-redirect-permanent! default-fd-for-op
   SAVE-FD-MIN save-fd restore-single! redirect-fd-to-file!
   set-port-for-fd! make-port-for-fd set-current-port-for-fd!
   dup-gambit-port! redirections->process-settings)
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
    (std sugar) (gsh ast) (gsh ffi) (gsh environment)
    (gsh expander) (gsh util))
  (define O_RDONLY 0)
  (define O_WRONLY 1)
  (define O_RDWR 2)
  (define O_CREAT 64)
  (define O_TRUNC 512)
  (define O_APPEND 1024)
  (define O_NONBLOCK 2048)
  (define (apply-redirections redirs env)
    (let loop ([redirs redirs] [saved (list)])
      (if (null? redirs)
          (let ([result (reverse saved)])
            (*active-redirect-fds*
              (append (map car result) (*active-redirect-fds*)))
            result)
          (let* ([redir (car redirs)])
            (let* ([result (guard (__exn
                                    [#t
                                     ((lambda (e)
                                        (restore-redirections
                                          (reverse saved))
                                        (raise e))
                                       __exn)])
                             (apply-single-redirect! redir env))])
              (cond
                [(not result) (loop (cdr redirs) saved)]
                [(integer? (car result))
                 (loop (cdr redirs) (cons result saved))]
                [else
                 (loop (cdr redirs) (append (reverse result) saved))]))))))
  (define (restore-redirections saved)
    (let ([restored-fds (map car saved)])
      (*active-redirect-fds*
        (filter
          (lambda (fd) (not (member fd restored-fds)))
          (*active-redirect-fds*))))
    (for-each
      (lambda (entry)
        (let ([fd (car entry)]
              [saved-real-fd (cadr entry)]
              [saved-port (caddr entry)]
              [new-port (and (> (length entry) 3) (cadddr entry))])
          (when new-port
            (guard (__exn [#t (void __exn)])
              (flush-output-port new-port))
            (guard (__exn [#t (void __exn)]) (close-port new-port)))
          (if (>= saved-real-fd 0)
              (begin
                (ffi-dup2 saved-real-fd fd)
                (ffi-close-fd saved-real-fd))
              (when (> fd 2) (ffi-close-fd fd)))
          (when saved-port
            (case fd
              [(0) (current-input-port saved-port)]
              [(1) (current-output-port saved-port)]
              [(2) (current-error-port saved-port)]))))
      (reverse saved)))
  (define (apply-redirections-permanent! redirs env)
    (for-each
      (lambda (redir)
        (apply-single-redirect-permanent! redir env))
      redirs))
  (define (apply-single-redirect! redir env)
    (let* ([op (redir-op redir)])
      (let* ([fd-var (redir-fd-var redir)])
        (if (and fd-var
                 (not (and (memq op '(>& <&))
                           (string=? (redir-target redir) "-"))))
            (apply-named-fd-redirect! redir env)
            (let* ([fd (cond
                         [fd-var
                          (let ([val (env-get env fd-var)])
                            (or (and val (string->number val))
                                (begin
                                  (fprintf
                                    (current-error-port)
                                    "gsh: ~a: Bad file descriptor~n"
                                    fd-var)
                                  (error 'gerbil
                                    (string-append
                                      fd-var
                                      ": Bad file descriptor")))))]
                         [(redir-fd redir) => values]
                         [else (default-fd-for-op op)])])
              (let* ([target-str (cond
                                   [(memq op '(<< <<- <<< <<q <<-q))
                                    (cond
                                      [(memq op '(<<q <<-q))
                                       (redir-target redir)]
                                      [(eq? op '<<<)
                                       (expand-word-nosplit
                                         (redir-target redir)
                                         env)]
                                      [else
                                       (expand-heredoc-body
                                         (redir-target redir)
                                         env)])]
                                   [(memq op '(>& <&))
                                    (expand-word-nosplit
                                      (redir-target redir)
                                      env)]
                                   [else
                                    (let ([words (expand-word
                                                   (redir-target redir)
                                                   env)])
                                      (cond
                                        [(or (null? words)
                                             (> (length words) 1))
                                         (error 'gerbil
                                           (string-append
                                             (redir-target redir)
                                             ": ambiguous redirect"))]
                                        [else
                                         (let ([w (car words)])
                                           (when (string=? w "")
                                             (error 'gerbil
                                               ": No such file or directory"))
                                           w)]))])])
                (case op
                  [(<)
                   (let ([save (save-fd fd)])
                     (redirect-fd-to-file! fd target-str O_RDONLY 0)
                     (when (= fd 0)
                       (current-input-port (open-input-file target-str)))
                     save)]
                  [(>)
                   (let ([save (save-fd fd)])
                     (when (and (env-option? env "noclobber")
                                (file-regular? target-str))
                       (fprintf
                         (current-error-port)
                         "gsh: ~a: cannot overwrite existing file~n"
                         target-str)
                       (restore-single! save)
                       (error 'gerbil "cannot overwrite existing file"))
                     (redirect-fd-to-file!
                       fd
                       target-str
                       (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                       438)
                     (if (or (= fd 1) (= fd 2))
                         (let ([new-port (set-port-for-fd!
                                           fd
                                           target-str
                                           'truncate)])
                           (append save (list new-port)))
                         save))]
                  [(>>)
                   (let ([save (save-fd fd)])
                     (redirect-fd-to-file!
                       fd
                       target-str
                       (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                       438)
                     (if (or (= fd 1) (= fd 2))
                         (let ([new-port (set-port-for-fd!
                                           fd
                                           target-str
                                           'append)])
                           (append save (list new-port)))
                         save))]
                  [(clobber)
                   (let ([save (save-fd fd)])
                     (redirect-fd-to-file!
                       fd
                       target-str
                       (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                       438)
                     (if (or (= fd 1) (= fd 2))
                         (let ([new-port (set-port-for-fd!
                                           fd
                                           target-str
                                           'truncate)])
                           (append save (list new-port)))
                         save))]
                  [(&>)
                   (let ([save1 (save-fd 1)] [save2 (save-fd 2)])
                     (redirect-fd-to-file!
                       1
                       target-str
                       (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                       438)
                     (ffi-dup2 1 2)
                     (let ([port (open-output-file
                                   (list
                                     'path:
                                     target-str
                                     'truncate:
                                     #t))])
                       (current-output-port port)
                       (current-error-port port))
                     (list save1 save2))]
                  [(&>>)
                   (let ([save1 (save-fd 1)] [save2 (save-fd 2)])
                     (redirect-fd-to-file!
                       1
                       target-str
                       (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                       438)
                     (ffi-dup2 1 2)
                     (let ([port (open-output-file
                                   (list 'path: target-str 'append: #t))])
                       (current-output-port port)
                       (current-error-port port))
                     (list save1 save2))]
                  [(<< <<- <<q <<-q)
                   (let ([save (save-fd fd)])
                     (let-values ([(read-fd write-fd) (ffi-pipe-raw)])
                       (let ([write-port (open-output-file
                                           (string-append
                                             "/dev/fd/"
                                             (number->string write-fd)))])
                         (display target-str write-port)
                         (flush-output-port write-port)
                         (close-port write-port))
                       (ffi-close-fd write-fd)
                       (ffi-dup2 read-fd fd)
                       (unless (= read-fd fd) (ffi-close-fd read-fd)))
                     (when (= fd 0)
                       (current-input-port (open-input-string target-str)))
                     save)]
                  [(<<<)
                   (let ([save (save-fd fd)])
                     (let-values ([(read-fd write-fd) (ffi-pipe-raw)])
                       (let ([write-port (open-output-file
                                           (string-append
                                             "/dev/fd/"
                                             (number->string write-fd)))])
                         (display target-str write-port)
                         (newline write-port)
                         (flush-output-port write-port)
                         (close-port write-port))
                       (ffi-close-fd write-fd)
                       (ffi-dup2 read-fd fd)
                       (unless (= read-fd fd) (ffi-close-fd read-fd)))
                     (current-input-port
                       (open-input-string (string-append target-str "\n")))
                     save)]
                  [(>&)
                   (cond
                     [(string=? target-str "-")
                      (let ([save (save-fd fd)]) (ffi-close-fd fd) save)]
                     [else
                      (let-values ([(actual-target close-source?)
                                    (if (and (> (string-length target-str)
                                                1)
                                             (char=?
                                               (string-ref
                                                 target-str
                                                 (- (string-length
                                                      target-str)
                                                    1))
                                               #\-)
                                             (string->number
                                               (substring
                                                 target-str
                                                 0
                                                 (- (string-length
                                                      target-str)
                                                    1))))
                                        (values
                                          (substring
                                            target-str
                                            0
                                            (- (string-length target-str)
                                               1))
                                          #t)
                                        (values target-str #f))])
                        (let ([target-fd (string->number actual-target)])
                          (cond
                            [target-fd
                             (let ([save (save-fd fd)])
                               (let ([r (ffi-dup2 target-fd fd)])
                                 (when (< r 0)
                                   (restore-single! save)
                                   (error 'gerbil
                                     (string-append
                                       (number->string target-fd)
                                       ": Bad file descriptor")))
                                 (when close-source?
                                   (ffi-close-fd target-fd))
                                 (if (and (<= 0 fd 2)
                                          (not (<= 0 target-fd 2)))
                                     (let ([new-port (make-port-for-fd
                                                       fd)])
                                       (if new-port
                                           (begin
                                             (set-current-port-for-fd!
                                               fd
                                               new-port)
                                             (append save (list new-port)))
                                           save))
                                     (begin
                                       (dup-gambit-port! target-fd fd)
                                       save))))]
                            [(redir-fd redir)
                             (error 'gerbil
                               (string-append
                                 target-str
                                 ": Bad file descriptor"))]
                            [else
                             (let ([save1 (save-fd 1)] [save2 (save-fd 2)])
                               (redirect-fd-to-file!
                                 1
                                 target-str
                                 (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                                 438)
                               (ffi-dup2 1 2)
                               (let ([port (open-output-file
                                             (list
                                               'path:
                                               target-str
                                               'truncate:
                                               #t))])
                                 (current-output-port port)
                                 (current-error-port port))
                               (list save1 save2))])))])]
                  [(<&)
                   (cond
                     [(string=? target-str "-")
                      (let ([save (save-fd fd)]) (ffi-close-fd fd) save)]
                     [else
                      (let-values ([(actual-target close-source?)
                                    (if (and (> (string-length target-str)
                                                1)
                                             (char=?
                                               (string-ref
                                                 target-str
                                                 (- (string-length
                                                      target-str)
                                                    1))
                                               #\-)
                                             (string->number
                                               (substring
                                                 target-str
                                                 0
                                                 (- (string-length
                                                      target-str)
                                                    1))))
                                        (values
                                          (substring
                                            target-str
                                            0
                                            (- (string-length target-str)
                                               1))
                                          #t)
                                        (values target-str #f))])
                        (let ([target-fd (string->number actual-target)])
                          (if target-fd
                              (let ([save (save-fd fd)])
                                (let ([r (ffi-dup2 target-fd fd)])
                                  (when (< r 0)
                                    (restore-single! save)
                                    (error 'gerbil
                                      (string-append
                                        (number->string target-fd)
                                        ": Bad file descriptor")))
                                  (when close-source?
                                    (ffi-close-fd target-fd))
                                  (if (and (<= 0 fd 2)
                                           (not (<= 0 target-fd 2)))
                                      (let ([new-port (make-port-for-fd
                                                        fd)])
                                        (if new-port
                                            (begin
                                              (set-current-port-for-fd!
                                                fd
                                                new-port)
                                              (append
                                                save
                                                (list new-port)))
                                            save))
                                      (begin
                                        (dup-gambit-port! target-fd fd)
                                        save))))
                              (begin
                                (fprintf
                                  (current-error-port)
                                  "gsh: ~a: bad file descriptor~n"
                                  target-str)
                                #f))))])]
                  [(<>)
                   (let ([save (save-fd fd)])
                     (let ([raw-fd (ffi-open-raw
                                     target-str
                                     (bitwise-ior O_RDWR O_NONBLOCK)
                                     438)])
                       (when (< raw-fd 0)
                         (restore-single! save)
                         (fprintf
                           (current-error-port)
                           "gsh: ~a: No such file or directory~n"
                           target-str)
                         (error 'gerbil "cannot open file" target-str))
                       (let ([flags (ffi-fcntl-getfl raw-fd)])
                         (when (>= flags 0)
                           (ffi-fcntl-setfl
                             raw-fd
                             (bitwise-and
                               flags
                               (bitwise-not O_NONBLOCK)))))
                       (unless (= raw-fd fd)
                         (ffi-dup2 raw-fd fd)
                         (ffi-close-fd raw-fd)))
                     save)]
                  [else
                   (fprintf
                     (current-error-port)
                     "gsh: unsupported redirect operator ~a~n"
                     op)
                   #f])))))))
  (define (apply-named-fd-redirect! redir env)
    (let* ([op (redir-op redir)])
      (let* ([fd-var (redir-fd-var redir)])
        (let* ([target-str (let ([words (expand-word
                                          (redir-target redir)
                                          env)])
                             (cond
                               [(or (null? words) (> (length words) 1))
                                (error 'gerbil
                                  (string-append
                                    (redir-target redir)
                                    ": ambiguous redirect"))]
                               [else
                                (let ([w (car words)])
                                  (when (string=? w "")
                                    (error 'gerbil
                                      ": No such file or directory"))
                                  w)]))])
          (let* ([flags (case op
                          [(<) O_RDONLY]
                          [(>) (bitwise-ior O_WRONLY O_CREAT O_TRUNC)]
                          [(>>) (bitwise-ior O_WRONLY O_CREAT O_APPEND)]
                          [(clobber)
                           (bitwise-ior O_WRONLY O_CREAT O_TRUNC)]
                          [(<>) O_RDWR]
                          [else (bitwise-ior O_WRONLY O_CREAT O_TRUNC)])])
            (let* ([raw-fd (ffi-open-raw target-str flags 438)])
              (when (< raw-fd 0)
                (fprintf
                  (current-error-port)
                  "gsh: ~a: No such file or directory~n"
                  target-str)
                (error 'gerbil "cannot open file" target-str))
              (let ([high-fd (ffi-dup-above raw-fd 10)])
                (ffi-close-fd raw-fd)
                (env-set! env fd-var (number->string high-fd))
                #f)))))))
  (define (apply-single-redirect-permanent! redir env)
    (let* ([op (redir-op redir)])
      (let* ([fd-var (redir-fd-var redir)])
        (if (and fd-var
                 (not (and (memq op '(>& <&))
                           (string=? (redir-target redir) "-"))))
            (apply-named-fd-redirect-permanent! redir env)
            (let* ([fd (cond
                         [fd-var
                          (let ([val (env-get env fd-var)])
                            (or (and val (string->number val))
                                (begin
                                  (fprintf
                                    (current-error-port)
                                    "gsh: ~a: Bad file descriptor~n"
                                    fd-var)
                                  (error 'gerbil
                                    (string-append
                                      fd-var
                                      ": Bad file descriptor")))))]
                         [(redir-fd redir) => values]
                         [else (default-fd-for-op op)])])
              (let* ([target-str (cond
                                   [(memq op '(<< <<- <<< <<q <<-q))
                                    (cond
                                      [(memq op '(<<q <<-q))
                                       (redir-target redir)]
                                      [(eq? op '<<<)
                                       (expand-word-nosplit
                                         (redir-target redir)
                                         env)]
                                      [else
                                       (expand-heredoc-body
                                         (redir-target redir)
                                         env)])]
                                   [(memq op '(>& <&))
                                    (expand-word-nosplit
                                      (redir-target redir)
                                      env)]
                                   [else
                                    (let ([words (expand-word
                                                   (redir-target redir)
                                                   env)])
                                      (cond
                                        [(or (null? words)
                                             (> (length words) 1))
                                         (error 'gerbil
                                           (string-append
                                             (redir-target redir)
                                             ": ambiguous redirect"))]
                                        [else
                                         (let ([w (car words)])
                                           (when (string=? w "")
                                             (error 'gerbil
                                               ": No such file or directory"))
                                           w)]))])])
                (case op
                  [(<)
                   (redirect-fd-to-file! fd target-str O_RDONLY 0)
                   (when (= fd 0)
                     (current-input-port (open-input-file target-str)))]
                  [(>)
                   (redirect-fd-to-file!
                     fd
                     target-str
                     (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                     438)
                   (when (or (= fd 1) (= fd 2))
                     (set-port-for-fd! fd target-str 'truncate))]
                  [(>>)
                   (redirect-fd-to-file!
                     fd
                     target-str
                     (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                     438)
                   (when (or (= fd 1) (= fd 2))
                     (set-port-for-fd! fd target-str 'append))]
                  [(clobber)
                   (redirect-fd-to-file!
                     fd
                     target-str
                     (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                     438)
                   (when (or (= fd 1) (= fd 2))
                     (set-port-for-fd! fd target-str 'truncate))]
                  [(&>)
                   (redirect-fd-to-file!
                     1
                     target-str
                     (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                     438)
                   (ffi-dup2 1 2)
                   (let ([port (open-output-file
                                 (list 'path: target-str 'truncate: #t))])
                     (current-output-port port)
                     (current-error-port port))]
                  [(&>>)
                   (redirect-fd-to-file!
                     1
                     target-str
                     (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                     438)
                   (ffi-dup2 1 2)
                   (let ([port (open-output-file
                                 (list 'path: target-str 'append: #t))])
                     (current-output-port port)
                     (current-error-port port))]
                  [(>&)
                   (cond
                     [(string=? target-str "-") (ffi-close-fd fd)]
                     [else
                      (let-values ([(actual-target close-source?)
                                    (if (and (> (string-length target-str)
                                                1)
                                             (char=?
                                               (string-ref
                                                 target-str
                                                 (- (string-length
                                                      target-str)
                                                    1))
                                               #\-)
                                             (string->number
                                               (substring
                                                 target-str
                                                 0
                                                 (- (string-length
                                                      target-str)
                                                    1))))
                                        (values
                                          (substring
                                            target-str
                                            0
                                            (- (string-length target-str)
                                               1))
                                          #t)
                                        (values target-str #f))])
                        (let ([target-fd (string->number actual-target)])
                          (cond
                            [target-fd
                             (let ([r (ffi-dup2 target-fd fd)])
                               (when (< r 0)
                                 (error 'gerbil
                                   (string-append
                                     (number->string target-fd)
                                     ": Bad file descriptor")))
                               (when close-source?
                                 (ffi-close-fd target-fd))
                               (if (and (<= 0 fd 2)
                                        (not (<= 0 target-fd 2)))
                                   (let ([new-port (make-port-for-fd fd)])
                                     (when new-port
                                       (set-current-port-for-fd!
                                         fd
                                         new-port)))
                                   (dup-gambit-port! target-fd fd)))]
                            [(redir-fd redir)
                             (error 'gerbil
                               (string-append
                                 target-str
                                 ": Bad file descriptor"))]
                            [else
                             (redirect-fd-to-file!
                               1
                               target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               438)
                             (ffi-dup2 1 2)
                             (let ([port (open-output-file
                                           (list
                                             'path:
                                             target-str
                                             'truncate:
                                             #t))])
                               (current-output-port port)
                               (current-error-port port))])))])]
                  [(<&)
                   (cond
                     [(string=? target-str "-") (ffi-close-fd fd)]
                     [else
                      (let-values ([(actual-target close-source?)
                                    (if (and (> (string-length target-str)
                                                1)
                                             (char=?
                                               (string-ref
                                                 target-str
                                                 (- (string-length
                                                      target-str)
                                                    1))
                                               #\-)
                                             (string->number
                                               (substring
                                                 target-str
                                                 0
                                                 (- (string-length
                                                      target-str)
                                                    1))))
                                        (values
                                          (substring
                                            target-str
                                            0
                                            (- (string-length target-str)
                                               1))
                                          #t)
                                        (values target-str #f))])
                        (let ([target-fd (string->number actual-target)])
                          (if target-fd
                              (let ([r (ffi-dup2 target-fd fd)])
                                (when (< r 0)
                                  (error 'gerbil
                                    (string-append
                                      (number->string target-fd)
                                      ": Bad file descriptor")))
                                (when close-source?
                                  (ffi-close-fd target-fd))
                                (if (and (<= 0 fd 2)
                                         (not (<= 0 target-fd 2)))
                                    (let ([new-port (make-port-for-fd fd)])
                                      (when new-port
                                        (set-current-port-for-fd!
                                          fd
                                          new-port)))
                                    (dup-gambit-port! target-fd fd)))
                              (fprintf
                                (current-error-port)
                                "gsh: ~a: bad file descriptor~n"
                                target-str))))])]
                  [(<>)
                   (let ([raw-fd (ffi-open-raw
                                   target-str
                                   (bitwise-ior O_RDWR O_NONBLOCK)
                                   438)])
                     (when (< raw-fd 0)
                       (fprintf
                         (current-error-port)
                         "gsh: ~a: No such file or directory~n"
                         target-str)
                       (error 'gerbil "cannot open file" target-str))
                     (let ([flags (ffi-fcntl-getfl raw-fd)])
                       (when (>= flags 0)
                         (ffi-fcntl-setfl
                           raw-fd
                           (bitwise-and flags (bitwise-not O_NONBLOCK)))))
                     (unless (= raw-fd fd)
                       (ffi-dup2 raw-fd fd)
                       (ffi-close-fd raw-fd)))]
                  [(<< <<- <<q <<-q)
                   (let-values ([(read-fd write-fd) (ffi-pipe-raw)])
                     (let ([write-port (open-output-file
                                         (string-append
                                           "/dev/fd/"
                                           (number->string write-fd)))])
                       (display target-str write-port)
                       (flush-output-port write-port)
                       (close-port write-port))
                     (ffi-close-fd write-fd)
                     (ffi-dup2 read-fd fd)
                     (unless (= read-fd fd) (ffi-close-fd read-fd))
                     (when (= fd 0)
                       (current-input-port
                         (open-input-string target-str))))]
                  [(<<<)
                   (let-values ([(read-fd write-fd) (ffi-pipe-raw)])
                     (let ([write-port (open-output-file
                                         (string-append
                                           "/dev/fd/"
                                           (number->string write-fd)))])
                       (display target-str write-port)
                       (newline write-port)
                       (flush-output-port write-port)
                       (close-port write-port))
                     (ffi-close-fd write-fd)
                     (ffi-dup2 read-fd fd)
                     (unless (= read-fd fd) (ffi-close-fd read-fd))
                     (current-input-port
                       (open-input-string
                         (string-append target-str "\n"))))]
                  [else
                   (fprintf
                     (current-error-port)
                     "gsh: unsupported redirect operator ~a~n"
                     op)])))))))
  (define (apply-named-fd-redirect-permanent! redir env)
    (let* ([op (redir-op redir)])
      (let* ([fd-var (redir-fd-var redir)])
        (let* ([target-str (let ([words (expand-word
                                          (redir-target redir)
                                          env)])
                             (cond
                               [(or (null? words) (> (length words) 1))
                                (error 'gerbil
                                  (string-append
                                    (redir-target redir)
                                    ": ambiguous redirect"))]
                               [else
                                (let ([w (car words)])
                                  (when (string=? w "")
                                    (error 'gerbil
                                      ": No such file or directory"))
                                  w)]))])
          (let* ([flags (case op
                          [(<) O_RDONLY]
                          [(>) (bitwise-ior O_WRONLY O_CREAT O_TRUNC)]
                          [(>>) (bitwise-ior O_WRONLY O_CREAT O_APPEND)]
                          [(clobber)
                           (bitwise-ior O_WRONLY O_CREAT O_TRUNC)]
                          [(<>) O_RDWR]
                          [else (bitwise-ior O_WRONLY O_CREAT O_TRUNC)])])
            (let* ([raw-fd (ffi-open-raw target-str flags 438)])
              (when (< raw-fd 0)
                (fprintf
                  (current-error-port)
                  "gsh: ~a: No such file or directory~n"
                  target-str)
                (error 'gerbil "cannot open file" target-str))
              (let ([high-fd (ffi-dup-above raw-fd 10)])
                (ffi-close-fd raw-fd)
                (env-set! env fd-var (number->string high-fd)))))))))
  (define (default-fd-for-op op)
    (case op
      [(< << <<- <<q <<-q <<< <& <>) 0]
      [(> >> clobber >& &> &>>) 1]
      [else 0]))
  (define SAVE-FD-MIN 200)
  (define (save-fd fd)
    (let ([saved-real-fd (ffi-dup-above fd SAVE-FD-MIN)]
          [saved-port (case fd
                        [(0) (current-input-port)]
                        [(1) (current-output-port)]
                        [(2) (current-error-port)]
                        [else #f])])
      (list fd saved-real-fd saved-port)))
  (define (restore-single! save)
    (let ([fd (car save)]
          [saved-real-fd (cadr save)]
          [saved-port (caddr save)])
      (when (>= saved-real-fd 0)
        (ffi-dup2 saved-real-fd fd)
        (ffi-close-fd saved-real-fd))
      (when saved-port
        (case fd
          [(0) (current-input-port saved-port)]
          [(1) (current-output-port saved-port)]
          [(2) (current-error-port saved-port)]))))
  (define (redirect-fd-to-file! fd filename flags mode)
    (let ([raw-fd (ffi-open-raw filename flags mode)])
      (when (< raw-fd 0)
        (fprintf
          (current-error-port)
          "gsh: ~a: No such file or directory~n"
          filename)
        (error 'gerbil "cannot open file" filename))
      (unless (= raw-fd fd)
        (ffi-dup2 raw-fd fd)
        (ffi-close-fd raw-fd))))
  (define (set-port-for-fd! fd filename mode)
    (let ([port (case mode
                  [(truncate)
                   (open-output-file (list 'path: filename 'truncate: #t))]
                  [(append)
                   (open-output-file
                     (list 'path: filename 'append: #t))])])
      (case fd
        [(1) (current-output-port port)]
        [(2) (current-error-port port)])
      port))
  (define (make-port-for-fd fd)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (case fd
        [(0)
         (open-input-file
           (list
             'path:
             (string-append "/dev/fd/" (number->string fd))))]
        [(1 2)
         (open-output-file
           (list
             'path:
             (string-append "/dev/fd/" (number->string fd))
             'append:
             #t))])))
  (define (set-current-port-for-fd! fd port)
    (case fd
      [(0) (current-input-port port)]
      [(1) (current-output-port port)]
      [(2) (current-error-port port)]))
  (define (dup-gambit-port! source-fd dest-fd)
    (let ([source-port (case source-fd
                         [(0) (current-input-port)]
                         [(1) (current-output-port)]
                         [(2) (current-error-port)]
                         [else #f])])
      (when source-port
        (case dest-fd
          [(0) (current-input-port source-port)]
          [(1) (current-output-port source-port)]
          [(2) (current-error-port source-port)]))))
  (define (redirections->process-settings redirs env) (list)))
