;;; redirect.ss — File descriptor redirection for gsh
;;;
;;; Key insight: external commands launched via open-process with
;;; stdin/stdout/stderr-redirection: #f inherit REAL file descriptors (0,1,2),
;;; not Gambit port parameters. So we must manipulate both:
;;;   1. Real fds via ffi-dup/ffi-dup2/ffi-open-raw
;;;   2. Gambit port parameters for builtins

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/ffi
        :gsh/environment
        :gsh/expander
        :gsh/util)

;;; --- O_* flags for ffi-open-raw ---
(def O_RDONLY   0)
(def O_WRONLY   1)
(def O_RDWR     2)
(def O_CREAT   64)
(def O_TRUNC  512)
(def O_APPEND 1024)
(def O_NONBLOCK 2048)

;;; --- Saved state structure ---
;;; Each entry: (fd saved-real-fd saved-port)
;;;   fd           — which fd we redirected (0, 1, 2)
;;;   saved-real-fd — dup'd copy of the original real fd (for restore)
;;;   saved-port    — original Gambit port parameter value (for restore)

;;; --- Public interface ---

;; Apply a list of redirections, return saved state for restoration
;; Saved state is a flat list of (fd saved-real-fd saved-port) entries
(def (apply-redirections redirs env)
  (let loop ((redirs redirs) (saved []))
    (if (null? redirs)
      (let ((result (reverse saved)))
        ;; Track which fds are active redirects so ffi-fork-exec can preserve them
        (*active-redirect-fds*
         (append (map car result) (*active-redirect-fds*)))
        result)
      (let* ((redir (car redirs))
             (result (with-catch
                      (lambda (e)
                        ;; Redirect failed — restore already-applied redirections
                        (restore-redirections (reverse saved))
                        (raise e))
                      (lambda ()
                        (apply-single-redirect! redir env)))))
        (cond
          ((not result) (loop (cdr redirs) saved))
          ;; Multi-save: &> and &>> return a list of save entries
          ;; A save entry starts with an integer fd, a list-of-saves
          ;; starts with a list. Distinguish by checking (car result).
          ((integer? (car result))
           ;; Single save entry: (fd saved-real-fd saved-port)
           (loop (cdr redirs) (cons result saved)))
          (else
           ;; List of save entries: ((fd1 ...) (fd2 ...))
           (loop (cdr redirs) (append (reverse result) saved))))))))

;; Restore redirections from saved state
;; Each entry: (fd saved-real-fd saved-port) or (fd saved-real-fd saved-port new-port)
(def (restore-redirections saved)
  ;; Remove these fds from the active-redirect tracking
  (let ((restored-fds (map car saved)))
    (*active-redirect-fds*
     (filter (lambda (fd) (not (member fd restored-fds)))
             (*active-redirect-fds*))))
  (for-each
   (lambda (entry)
     (let ((fd (car entry))
           (saved-real-fd (cadr entry))
           (saved-port (caddr entry))
           (new-port (and (> (length entry) 3) (cadddr entry))))
       ;; Flush and close any new port we created (before restoring fd)
       (when new-port
         (with-catch void (lambda () (force-output new-port)))
         (with-catch void (lambda () (close-port new-port))))
       ;; Restore the real fd
       (if (>= saved-real-fd 0)
         (begin
           (ffi-dup2 saved-real-fd fd)
           (ffi-close-fd saved-real-fd))
         ;; saved-real-fd is -1: fd wasn't open before, close it to restore
         ;; Only close non-standard fds (never close 0/1/2)
         (when (> fd 2)
           (ffi-close-fd fd)))
       ;; Restore the Gambit port parameter
       (when saved-port
         (case fd
           ((0) (current-input-port saved-port))
           ((1) (current-output-port saved-port))
           ((2) (current-error-port saved-port))))))
   (reverse saved)))

;; Apply redirections permanently (for exec without command)
;; Does not save original fds — changes are permanent
(def (apply-redirections-permanent! redirs env)
  (for-each
   (lambda (redir) (apply-single-redirect-permanent! redir env))
   redirs))

;;; --- Single redirection ---

(def (apply-single-redirect! redir env)
  (let* ((op (redir-op redir))
         (fd-var (redir-fd-var redir)))
    ;; Named fd {varname}> handled separately (except {varname}>&- which is close)
    (if (and fd-var
             (not (and (memq op '(>& <&))
                       (string=? (redir-target redir) "-"))))
      (apply-named-fd-redirect! redir env)
      ;; Normal redirect (or named fd close)
      (let* ((fd (cond
                   ;; Named fd close: {varname}>&- — resolve variable
                   (fd-var
                    (let ((val (env-get env fd-var)))
                      (or (and val (string->number val))
                          (begin
                            (fprintf (current-error-port) "gsh: ~a: Bad file descriptor~n" fd-var)
                            (error (string-append fd-var ": Bad file descriptor"))))))
                   ;; Explicit numeric fd
                   ((redir-fd redir) => values)
                   ;; Default fd for the operator
                   (else (default-fd-for-op op))))
             (target-str (cond
                       ((memq op '(<< <<- <<< <<q <<-q))
                        ;; Heredocs/herestrings: expand but no word splitting validation
                        (cond
                          ((memq op '(<<q <<-q)) (redir-target redir))
                          ((eq? op '<<<)
                           (expand-word-nosplit (redir-target redir) env))
                          (else (expand-heredoc-body (redir-target redir) env))))
                       ;; Fd dup targets (&> / <&): don't validate as file
                       ((memq op '(>& <&))
                        (expand-word-nosplit (redir-target redir) env))
                       (else
                        ;; File redirect: validate target
                        (let ((words (expand-word (redir-target redir) env)))
                          (cond
                            ((or (null? words) (> (length words) 1))
                             (error (string-append (redir-target redir) ": ambiguous redirect")))
                            (else
                             (let ((w (car words)))
                               (when (string=? w "")
                                 (error ": No such file or directory"))
                               w))))))))
    (case op
      ;; < file — input from file
      ((<)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str O_RDONLY 0)
         ;; Create Gambit port for builtins
         (when (= fd 0)
           (current-input-port (open-input-file target-str)))
         save))
      ;; > file — output to file (truncate)
      ((>)
       (let ((save (save-fd fd)))
         ;; Check noclobber — only block regular files (not /dev/null etc.)
         (when (and (env-option? env "noclobber")
                    (file-regular? target-str))
           (fprintf (current-error-port) "gsh: ~a: cannot overwrite existing file~n" target-str)
           (restore-single! save)
           (error "cannot overwrite existing file"))
         (redirect-fd-to-file! fd target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               #o666)
         (if (or (= fd 1) (= fd 2))
           (let ((new-port (set-port-for-fd! fd target-str 'truncate)))
             (append save [new-port]))
           save)))
      ;; >> file — output to file (append)
      ((>>)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str
                               (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                               #o666)
         (if (or (= fd 1) (= fd 2))
           (let ((new-port (set-port-for-fd! fd target-str 'append)))
             (append save [new-port]))
           save)))
      ;; clobber (>|) — output to file ignoring noclobber
      ((clobber)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               #o666)
         (if (or (= fd 1) (= fd 2))
           (let ((new-port (set-port-for-fd! fd target-str 'truncate)))
             (append save [new-port]))
           save)))
      ;; &> file — stdout+stderr to file
      ((&>)
       (let ((save1 (save-fd 1))
             (save2 (save-fd 2)))
         (redirect-fd-to-file! 1 target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               #o666)
         ;; stderr → same as stdout
         (ffi-dup2 1 2)
         (let ((port (open-output-file [path: target-str truncate: #t])))
           (current-output-port port)
           (current-error-port port))
         ;; Return both saves
         (list save1 save2)))
      ;; &>> file — stdout+stderr append
      ((&>>)
       (let ((save1 (save-fd 1))
             (save2 (save-fd 2)))
         (redirect-fd-to-file! 1 target-str
                               (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                               #o666)
         (ffi-dup2 1 2)
         (let ((port (open-output-file [path: target-str append: #t])))
           (current-output-port port)
           (current-error-port port))
         (list save1 save2)))
      ;; << heredoc — here-document
      ((<< <<- <<q <<-q)
       (let ((save (save-fd fd)))
         ;; For heredoc, we use a pipe: write content, read from pipe
         (let-values (((read-fd write-fd) (ffi-pipe-raw)))
           ;; Write the heredoc content to the write end via /dev/fd
           (let ((write-port (open-output-file
                              (string-append "/dev/fd/" (number->string write-fd)))))
             (display target-str write-port)
             (force-output write-port)
             (close-port write-port))
           (ffi-close-fd write-fd)
           ;; Redirect to target fd (defaults to 0/stdin)
           (ffi-dup2 read-fd fd)
           ;; Only close read-fd if it differs from target fd
           ;; (pipe may return the target fd itself when it's free)
           (unless (= read-fd fd)
             (ffi-close-fd read-fd)))
         (when (= fd 0)
           (current-input-port (open-input-string target-str)))
         save))
      ;; <<< word — here-string
      ((<<<)
       (let ((save (save-fd fd)))
         (let-values (((read-fd write-fd) (ffi-pipe-raw)))
           (let ((write-port (open-output-file
                              (string-append "/dev/fd/" (number->string write-fd)))))
             (display target-str write-port)
             (newline write-port)
             (force-output write-port)
             (close-port write-port))
           (ffi-close-fd write-fd)
           (ffi-dup2 read-fd fd)
           (unless (= read-fd fd)
             (ffi-close-fd read-fd)))
         (current-input-port (open-input-string (string-append target-str "\n")))
         save))
      ;; >& n — dup fd (or close with >& -)
      ;; >& word — when word is not a number or -, redirect both stdout+stderr to file
      ((>&)
       (cond
         ((string=? target-str "-")
          ;; Close fd
          (let ((save (save-fd fd)))
            (ffi-close-fd fd)
            save))
         (else
          ;; Check for fd move: N- means dup N then close N
          (let-values (((actual-target close-source?)
                        (if (and (> (string-length target-str) 1)
                                 (char=? (string-ref target-str (- (string-length target-str) 1)) #\-)
                                 (string->number (substring target-str 0 (- (string-length target-str) 1))))
                          (values (substring target-str 0 (- (string-length target-str) 1)) #t)
                          (values target-str #f))))
            (let ((target-fd (string->number actual-target)))
              (cond
                (target-fd
                 (let ((save (save-fd fd)))
                   (let ((r (ffi-dup2 target-fd fd)))
                     (when (< r 0)
                       (restore-single! save)
                       (error (string-append (number->string target-fd) ": Bad file descriptor")))
                     ;; Close source fd if this is a move operation (N-)
                     (when close-source?
                       (ffi-close-fd target-fd))
                     ;; Update Gambit port: if the source fd is not a standard
                     ;; Gambit port (0/1/2), create a fresh port via /dev/fd/N
                     ;; to avoid sendto-on-non-socket errors when Gambit cached
                     ;; the fd type at startup. Track the new port in save for cleanup.
                     (if (and (<= 0 fd 2) (not (<= 0 target-fd 2)))
                       ;; High fd → standard fd: try to create fresh Gambit port
                       ;; If /dev/fd/N fails (e.g., socket), keep existing port
                       (let ((new-port (make-port-for-fd fd)))
                         (if new-port
                           (begin
                             (set-current-port-for-fd! fd new-port)
                             (append save (list new-port)))
                           save))
                       ;; Standard fd → standard fd: copy existing Gambit port
                       (begin (dup-gambit-port! target-fd fd)
                              save)))))
                ;; Only treat as &> if fd was NOT explicitly specified
                ;; 1>&file → error (bad file descriptor)
                ;; >&file → equivalent to &>file
                ((redir-fd redir)
                 (error (string-append target-str ": Bad file descriptor")))
                (else
                 ;; Not a number, no explicit fd → treat as &> (redirect stdout+stderr to file)
                 (let ((save1 (save-fd 1))
                       (save2 (save-fd 2)))
                   (redirect-fd-to-file! 1 target-str
                                         (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                                         #o666)
                   (ffi-dup2 1 2)
                   (let ((port (open-output-file [path: target-str truncate: #t])))
                     (current-output-port port)
                     (current-error-port port))
                   (list save1 save2)))))))))
      ;; <& n — dup fd for input
      ((<&)
       (cond
         ((string=? target-str "-")
          (let ((save (save-fd fd)))
            (ffi-close-fd fd)
            save))
         (else
          ;; Check for fd move: N- means dup N then close N
          (let-values (((actual-target close-source?)
                        (if (and (> (string-length target-str) 1)
                                 (char=? (string-ref target-str (- (string-length target-str) 1)) #\-)
                                 (string->number (substring target-str 0 (- (string-length target-str) 1))))
                          (values (substring target-str 0 (- (string-length target-str) 1)) #t)
                          (values target-str #f))))
            (let ((target-fd (string->number actual-target)))
              (if target-fd
                (let ((save (save-fd fd)))
                  (let ((r (ffi-dup2 target-fd fd)))
                    (when (< r 0)
                      (restore-single! save)
                      (error (string-append (number->string target-fd) ": Bad file descriptor")))
                    (when close-source?
                      (ffi-close-fd target-fd))
                    (if (and (<= 0 fd 2) (not (<= 0 target-fd 2)))
                      ;; Try to create fresh port; keep existing if it fails
                      (let ((new-port (make-port-for-fd fd)))
                        (if new-port
                          (begin
                            (set-current-port-for-fd! fd new-port)
                            (append save (list new-port)))
                          save))
                      (begin (dup-gambit-port! target-fd fd)
                             save))))
                (begin
                  (fprintf (current-error-port) "gsh: ~a: bad file descriptor~n" target-str)
                  #f)))))))
      ;; <> file — open read-write on fd
      ;; For FIFOs, use O_NONBLOCK to avoid blocking, then clear it
      ((<>)
       (let ((save (save-fd fd)))
         ;; Open with O_NONBLOCK to prevent blocking on FIFO
         (let ((raw-fd (ffi-open-raw target-str (bitwise-ior O_RDWR O_NONBLOCK) #o666)))
           (when (< raw-fd 0)
             (restore-single! save)
             (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" target-str)
             (error "cannot open file" target-str))
           ;; Clear O_NONBLOCK flag after successful open
           (let ((flags (ffi-fcntl-getfl raw-fd)))
             (when (>= flags 0)
               (ffi-fcntl-setfl raw-fd (bitwise-and flags (bitwise-not O_NONBLOCK)))))
           ;; dup2 onto target fd and close original
           (unless (= raw-fd fd)
             (ffi-dup2 raw-fd fd)
             (ffi-close-fd raw-fd)))
         save))
      (else
       (fprintf (current-error-port) "gsh: unsupported redirect operator ~a~n" op)
       #f))))))

;; Apply named fd redirect: {varname}>file — auto-allocate fd >= 10
(def (apply-named-fd-redirect! redir env)
  (let* ((op (redir-op redir))
         (fd-var (redir-fd-var redir))
         (target-str (let ((words (expand-word (redir-target redir) env)))
                       (cond
                         ((or (null? words) (> (length words) 1))
                          (error (string-append (redir-target redir) ": ambiguous redirect")))
                         (else
                          (let ((w (car words)))
                            (when (string=? w "")
                              (error ": No such file or directory"))
                            w)))))
         ;; Determine open flags based on operator
         (flags (case op
                  ((<) O_RDONLY)
                  ((>) (bitwise-ior O_WRONLY O_CREAT O_TRUNC))
                  ((>>) (bitwise-ior O_WRONLY O_CREAT O_APPEND))
                  ((clobber) (bitwise-ior O_WRONLY O_CREAT O_TRUNC))
                  ((<>) O_RDWR)
                  (else (bitwise-ior O_WRONLY O_CREAT O_TRUNC))))
         ;; Open the file to get a raw fd
         (raw-fd (ffi-open-raw target-str flags #o666)))
    (when (< raw-fd 0)
      (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" target-str)
      (error "cannot open file" target-str))
    ;; Allocate fd >= 10, skipping any Gambit-internal fds
    ;; ffi-dup-above calls fcntl(F_DUPFD, min) which skips occupied fds
    (let ((high-fd (ffi-dup-above raw-fd 10)))
      (ffi-close-fd raw-fd)
      ;; Store the fd number in the variable
      (env-set! env fd-var (number->string high-fd))
      ;; Named fds stay open after the command (bash behavior).
      ;; They are only closed explicitly with {varname}>&-
      ;; Return #f so apply-redirections doesn't track this for restore.
      #f)))

;; Apply a single redirect permanently (for exec)
;; Same logic but no save/restore
(def (apply-single-redirect-permanent! redir env)
  (let* ((op (redir-op redir))
         (fd-var (redir-fd-var redir)))
    ;; Named fd {varname}> handled separately (except close)
    (if (and fd-var
             (not (and (memq op '(>& <&))
                       (string=? (redir-target redir) "-"))))
      (apply-named-fd-redirect-permanent! redir env)
      (let* ((fd (cond
                   (fd-var
                    (let ((val (env-get env fd-var)))
                      (or (and val (string->number val))
                          (begin
                            (fprintf (current-error-port) "gsh: ~a: Bad file descriptor~n" fd-var)
                            (error (string-append fd-var ": Bad file descriptor"))))))
                   ((redir-fd redir) => values)
                   (else (default-fd-for-op op))))
             (target-str (cond
                       ((memq op '(<< <<- <<< <<q <<-q))
                        (cond
                          ((memq op '(<<q <<-q)) (redir-target redir))
                          ((eq? op '<<<)
                           (expand-word-nosplit (redir-target redir) env))
                          (else (expand-heredoc-body (redir-target redir) env))))
                       ((memq op '(>& <&))
                        (expand-word-nosplit (redir-target redir) env))
                       (else
                        (let ((words (expand-word (redir-target redir) env)))
                          (cond
                            ((or (null? words) (> (length words) 1))
                             (error (string-append (redir-target redir) ": ambiguous redirect")))
                            (else
                             (let ((w (car words)))
                               (when (string=? w "")
                                 (error ": No such file or directory"))
                               w))))))))
    (case op
      ((<) (redirect-fd-to-file! fd target-str O_RDONLY 0)
           (when (= fd 0)
             (current-input-port (open-input-file target-str))))
      ((>) (redirect-fd-to-file! fd target-str
                                 (bitwise-ior O_WRONLY O_CREAT O_TRUNC) #o666)
           (when (or (= fd 1) (= fd 2))
             (set-port-for-fd! fd target-str 'truncate)))
      ((>>) (redirect-fd-to-file! fd target-str
                                  (bitwise-ior O_WRONLY O_CREAT O_APPEND) #o666)
            (when (or (= fd 1) (= fd 2))
              (set-port-for-fd! fd target-str 'append)))
      ((clobber) (redirect-fd-to-file! fd target-str
                                       (bitwise-ior O_WRONLY O_CREAT O_TRUNC) #o666)
                 (when (or (= fd 1) (= fd 2))
                   (set-port-for-fd! fd target-str 'truncate)))
      ((&>) (redirect-fd-to-file! 1 target-str
                                  (bitwise-ior O_WRONLY O_CREAT O_TRUNC) #o666)
            (ffi-dup2 1 2)
            (let ((port (open-output-file [path: target-str truncate: #t])))
              (current-output-port port)
              (current-error-port port)))
      ((&>>) (redirect-fd-to-file! 1 target-str
                                   (bitwise-ior O_WRONLY O_CREAT O_APPEND) #o666)
             (ffi-dup2 1 2)
             (let ((port (open-output-file [path: target-str append: #t])))
               (current-output-port port)
               (current-error-port port)))
      ((>&)
       (cond
         ((string=? target-str "-")
          (ffi-close-fd fd))
         (else
          (let-values (((actual-target close-source?)
                        (if (and (> (string-length target-str) 1)
                                 (char=? (string-ref target-str (- (string-length target-str) 1)) #\-)
                                 (string->number (substring target-str 0 (- (string-length target-str) 1))))
                          (values (substring target-str 0 (- (string-length target-str) 1)) #t)
                          (values target-str #f))))
            (let ((target-fd (string->number actual-target)))
              (cond
                (target-fd
                 (let ((r (ffi-dup2 target-fd fd)))
                   (when (< r 0)
                     (error (string-append (number->string target-fd) ": Bad file descriptor")))
                   (when close-source?
                     (ffi-close-fd target-fd))
                   (if (and (<= 0 fd 2) (not (<= 0 target-fd 2)))
                     ;; Try to create fresh port; keep existing if it fails
                     (let ((new-port (make-port-for-fd fd)))
                       (when new-port
                         (set-current-port-for-fd! fd new-port)))
                     (dup-gambit-port! target-fd fd))))
                ((redir-fd redir)
                 (error (string-append target-str ": Bad file descriptor")))
                (else
                 ;; >&word → redirect both stdout+stderr to file
                 (redirect-fd-to-file! 1 target-str
                                       (bitwise-ior O_WRONLY O_CREAT O_TRUNC) #o666)
                 (ffi-dup2 1 2)
                 (let ((port (open-output-file [path: target-str truncate: #t])))
                   (current-output-port port)
                   (current-error-port port)))))))))
      ((<&)
       (cond
         ((string=? target-str "-")
          (ffi-close-fd fd))
         (else
          (let-values (((actual-target close-source?)
                        (if (and (> (string-length target-str) 1)
                                 (char=? (string-ref target-str (- (string-length target-str) 1)) #\-)
                                 (string->number (substring target-str 0 (- (string-length target-str) 1))))
                          (values (substring target-str 0 (- (string-length target-str) 1)) #t)
                          (values target-str #f))))
            (let ((target-fd (string->number actual-target)))
              (if target-fd
                (let ((r (ffi-dup2 target-fd fd)))
                  (when (< r 0)
                    (error (string-append (number->string target-fd) ": Bad file descriptor")))
                  (when close-source?
                    (ffi-close-fd target-fd))
                  (if (and (<= 0 fd 2) (not (<= 0 target-fd 2)))
                    ;; Try to create fresh port; keep existing if it fails
                    (let ((new-port (make-port-for-fd fd)))
                      (when new-port
                        (set-current-port-for-fd! fd new-port)))
                    (dup-gambit-port! target-fd fd)))
                (fprintf (current-error-port) "gsh: ~a: bad file descriptor~n" target-str)))))))
      ;; <> file — open read-write (permanent)
      ;; For FIFOs, use O_NONBLOCK to avoid blocking, then clear it
      ((<>)
       ;; Open with O_NONBLOCK to prevent blocking on FIFO
       (let ((raw-fd (ffi-open-raw target-str (bitwise-ior O_RDWR O_NONBLOCK) #o666)))
         (when (< raw-fd 0)
           (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" target-str)
           (error "cannot open file" target-str))
         ;; Clear O_NONBLOCK flag after successful open
         (let ((flags (ffi-fcntl-getfl raw-fd)))
           (when (>= flags 0)
             (ffi-fcntl-setfl raw-fd (bitwise-and flags (bitwise-not O_NONBLOCK)))))
         ;; dup2 onto target fd and close original
         (unless (= raw-fd fd)
           (ffi-dup2 raw-fd fd)
           (ffi-close-fd raw-fd))))
      ;; << heredoc (permanent)
      ((<< <<- <<q <<-q)
       (let-values (((read-fd write-fd) (ffi-pipe-raw)))
         (let ((write-port (open-output-file
                            (string-append "/dev/fd/" (number->string write-fd)))))
           (display target-str write-port)
           (force-output write-port)
           (close-port write-port))
         (ffi-close-fd write-fd)
         (ffi-dup2 read-fd fd)
         (unless (= read-fd fd)
           (ffi-close-fd read-fd))
         (when (= fd 0)
           (current-input-port (open-input-string target-str)))))
      ;; <<< here-string (permanent)
      ((<<<)
       (let-values (((read-fd write-fd) (ffi-pipe-raw)))
         (let ((write-port (open-output-file
                            (string-append "/dev/fd/" (number->string write-fd)))))
           (display target-str write-port)
           (newline write-port)
           (force-output write-port)
           (close-port write-port))
         (ffi-close-fd write-fd)
         (ffi-dup2 read-fd fd)
         (unless (= read-fd fd)
           (ffi-close-fd read-fd))
         (current-input-port (open-input-string (string-append target-str "\n")))))
      (else (fprintf (current-error-port) "gsh: unsupported redirect operator ~a~n" op)))))))

;; Apply named fd redirect permanently: {varname}>file
(def (apply-named-fd-redirect-permanent! redir env)
  (let* ((op (redir-op redir))
         (fd-var (redir-fd-var redir))
         (target-str (let ((words (expand-word (redir-target redir) env)))
                       (cond
                         ((or (null? words) (> (length words) 1))
                          (error (string-append (redir-target redir) ": ambiguous redirect")))
                         (else
                          (let ((w (car words)))
                            (when (string=? w "")
                              (error ": No such file or directory"))
                            w)))))
         (flags (case op
                  ((<) O_RDONLY)
                  ((>) (bitwise-ior O_WRONLY O_CREAT O_TRUNC))
                  ((>>) (bitwise-ior O_WRONLY O_CREAT O_APPEND))
                  ((clobber) (bitwise-ior O_WRONLY O_CREAT O_TRUNC))
                  ((<>) O_RDWR)
                  (else (bitwise-ior O_WRONLY O_CREAT O_TRUNC))))
         (raw-fd (ffi-open-raw target-str flags #o666)))
    (when (< raw-fd 0)
      (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" target-str)
      (error "cannot open file" target-str))
    (let ((high-fd (ffi-dup-above raw-fd 10)))
      (ffi-close-fd raw-fd)
      (env-set! env fd-var (number->string high-fd)))))

;;; --- Helpers ---

(def (default-fd-for-op op)
  (case op
    ((< << <<- <<q <<-q <<< <& <>) 0)
    ((> >> clobber >& &> &>>) 1)
    (else 0)))

;; Minimum fd for save-fd — must be above any user-visible fd range
;; Named fds start at 10, bash tests use up to ~100, Gambit internal ~255
(def SAVE-FD-MIN 200)

;; Save the real fd + Gambit port for later restoration
;; Returns (fd saved-real-fd saved-port)
;; Uses ffi-dup-above to save to fd >= SAVE-FD-MIN
(def (save-fd fd)
  (let ((saved-real-fd (ffi-dup-above fd SAVE-FD-MIN))
        (saved-port (case fd
                      ((0) (current-input-port))
                      ((1) (current-output-port))
                      ((2) (current-error-port))
                      (else #f))))
    (list fd saved-real-fd saved-port)))

;; Restore a single save entry
(def (restore-single! save)
  (let ((fd (car save))
        (saved-real-fd (cadr save))
        (saved-port (caddr save)))
    (when (>= saved-real-fd 0)
      (ffi-dup2 saved-real-fd fd)
      (ffi-close-fd saved-real-fd))
    (when saved-port
      (case fd
        ((0) (current-input-port saved-port))
        ((1) (current-output-port saved-port))
        ((2) (current-error-port saved-port))))))

;; Open a file and dup2 onto target fd
(def (redirect-fd-to-file! fd filename flags mode)
  (let ((raw-fd (ffi-open-raw filename flags mode)))
    (when (< raw-fd 0)
      (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" filename)
      (error "cannot open file" filename))
    (unless (= raw-fd fd)
      (ffi-dup2 raw-fd fd)
      (ffi-close-fd raw-fd))))

;; Set Gambit port parameter for an output fd. Returns the new port.
;; Opens the file directly to create a character port for builtins.
;; Note: this creates an independent file description with its own offset.
;; External commands writing to fd 1 may have a different offset — the caller
;; must sync offsets before forking (see execute-external's ffi-lseek-end).
(def (set-port-for-fd! fd filename mode)
  (let ((port (case mode
                ((truncate) (open-output-file [path: filename truncate: #t]))
                ((append) (open-output-file [path: filename append: #t])))))
    (case fd
      ((1) (current-output-port port))
      ((2) (current-error-port port)))
    port))

;; Create a fresh Gambit port for a standard fd (0/1/2) after dup2
;; Uses /dev/fd/N so Gambit creates a proper port with write() not sendto()
;; Returns #f if /dev/fd/N cannot be opened (e.g., fd points to a socket)
(def (make-port-for-fd fd)
  (with-catch
   (lambda (e) #f)  ;; Return #f if open fails (e.g., socket, ENXIO)
   (lambda ()
     (case fd
       ((0) (open-input-file [path: (string-append "/dev/fd/" (number->string fd))]))
       ((1 2) (open-output-file [path: (string-append "/dev/fd/" (number->string fd))
                                 append: #t]))))))

;; Set current-input/output/error-port for a standard fd
(def (set-current-port-for-fd! fd port)
  (case fd
    ((0) (current-input-port port))
    ((1) (current-output-port port))
    ((2) (current-error-port port))))

;; Duplicate Gambit port from source-fd to dest-fd
(def (dup-gambit-port! source-fd dest-fd)
  (let ((source-port (case source-fd
                       ((0) (current-input-port))
                       ((1) (current-output-port))
                       ((2) (current-error-port))
                       (else #f))))
    (when source-port
      (case dest-fd
        ((0) (current-input-port source-port))
        ((1) (current-output-port source-port))
        ((2) (current-error-port source-port))))))

;; Convert redirections to open-process settings (kept for compatibility)
(def (redirections->process-settings redirs env)
  ;; With real fd redirection, we always use #f for stdin/stdout/stderr-redirection
  ;; to let the child inherit real fds. This function is kept for cases
  ;; where we need to know if redirections exist.
  [])
