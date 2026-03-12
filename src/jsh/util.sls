#!chezscheme
(library (jsh util)
  (export string->c-safe status->exit-code
   open-process-with-sigpipe *soh* pack-with-soh
   pack-fds-with-soh string-trim-whitespace string-split-chars
   string-index string-contains? string-join
   string-last-index-of which which-cached which-cache-invalidate!
   find-file-in-path executable?
   home-directory expand-tilde read-file-lines
   exception-message try-or-false safe-substring
   make-string-repeated valid-name? string->integer
   string-upcase string-downcase string-has-control-chars?
   hex-char? utf8-string-length utf8-count-codepoints
   utf8-byte-count file-regular? file-directory? file-symlink?
   file-nonempty? file-readable? file-writable? raw-byte-base
   byte->raw-char raw-byte-char? shell-display
   shell-display-raw)
  (import
    (except (chezscheme) box box? unbox set-box! andmap ormap
     iota last-pair find \x31;+ \x31;- fx/ fx1+ fx1- error? raise
     with-exception-handler identifier? hash-table?
     make-hash-table sort sort! path-extension printf fprintf
     file-directory? file-exists? getenv close-port void
     open-output-file open-input-file file-regular?
     string-downcase string-upcase)
    (except (jerboa runtime) bind-method! call-method ~ void
      cons* make-list)
    (runtime mop)
    (except (runtime util) string-join string-index last-pair
      iota \x31;- \x31;+ displayln make-keyword)
    (except (compat gambit) number->string make-mutex
      with-output-to-string file-directory? string->bytes
      bytes->string thread?)
    (except (std error) with-exception-handler error-trace
      error-irritants error-message)
    (except (std misc string) string-join string-index
      string-join string-split string-index string-empty?)
    (except (std misc list) take drop filter-map)
    (except (std misc alist) pget pgetv pgetq aget agetv agetq)
    (except
      (std os path)
      path-expand
      path-normalize
      path-absolute?)
    (except (std format) format) (std sort) (std pregexp)
    (except (jsh pregexp-compat) pregexp-quote pregexp-replace*
      pregexp-replace pregexp-split pregexp-match
      pregexp-match-positions pregexp)
    (std sugar) (jsh ffi))
  (define (string->c-safe s)
    (let ([len (string-length s)])
      (let check ([i 0])
        (cond
          [(>= i len) s]
          [(< (char->integer (string-ref s i)) 128) (check (+ i 1))]
          [else
           (let* ([u8 (string->bytes s)])
             (let* ([u8len (u8vector-length u8)])
               (let* ([out (make-string u8len)])
                 (let loop ([j 0])
                   (if (>= j u8len)
                       out
                       (begin
                         (string-set!
                           out
                           j
                           (integer->char (u8vector-ref u8 j)))
                         (loop (+ j 1))))))))]))))
  (define (status->exit-code raw-status)
    (cond
      [(WIFEXITED raw-status) (WEXITSTATUS raw-status)]
      [(WIFSIGNALED raw-status) (+ 128 (WTERMSIG raw-status))]
      [(WIFSTOPPED raw-status) (+ 128 (WSTOPSIG raw-status))]
      [else 255]))
  (define (open-process-with-sigpipe settings)
    (ffi-sigpipe-unblock)
    (let ([proc (open-process settings)])
      (ffi-sigpipe-block)
      proc))
  (define *soh* (make-string 1 (integer->char 1)))
  (define (pack-with-soh strs)
    (if (null? strs)
        ""
        (call-with-output-string
          (lambda (port)
            (display (car strs) port)
            (for-each
              (lambda (s) (display *soh* port) (display s port))
              (cdr strs))))))
  (define (pack-fds-with-soh fds)
    (pack-with-soh (map number->string fds)))
  (define (string-trim-whitespace str)
    (let* ([len (string-length str)])
      (let* ([start (let loop ([i 0])
                      (if (and (< i len)
                               (char-whitespace? (string-ref str i)))
                          (loop (+ i 1))
                          i))])
        (let* ([end (let loop ([i (- len 1)])
                      (if (and (>= i start)
                               (char-whitespace? (string-ref str i)))
                          (loop (- i 1))
                          (+ i 1)))])
          (substring str start end)))))
  (define (string-split-chars str charset)
    (let loop ([i 0] [start 0] [result (list)])
      (cond
        [(>= i (string-length str))
         (reverse
           (if (> i start)
               (cons (substring str start i) result)
               result))]
        [(string-index charset (string-ref str i))
         (loop
           (+ i 1)
           (+ i 1)
           (if (> i start)
               (cons (substring str start i) result)
               result))]
        [else (loop (+ i 1) start result)])))
  (define (string-index str ch)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (string-contains? haystack needle)
    (let ([nlen (string-length needle)]
          [hlen (string-length haystack)])
      (if (> nlen hlen)
          #f
          (let loop ([i 0])
            (cond
              [(> (+ i nlen) hlen) #f]
              [(string=? (substring haystack i (+ i nlen)) needle) #t]
              [else (loop (+ i 1))])))))
  (define (string-join lst sep)
    (if (null? lst)
        ""
        (call-with-output-string
          (lambda (port)
            (display (car lst) port)
            (for-each
              (lambda (s) (display sep port) (display s port))
              (cdr lst))))))
  (define (string-last-index-of str ch)
    (let loop ([i (- (string-length str) 1)])
      (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))])))
  (define (which name)
    (if (string-contains? name "/")
        (let ([resolved (gambit-path-expand name)])
          (and (file-exists? resolved) (executable? resolved) name))
        (let ([path-dirs (string-split-chars
                           (or (getenv "PATH" #f) "/usr/bin:/bin")
                           ":")])
          (let loop ([dirs path-dirs])
            (if (null? dirs)
                #f
                (let ([full (string-append (car dirs) "/" name)])
                  (if (and (file-exists? full) (executable? full))
                      full
                      (loop (cdr dirs)))))))))
  ;; PATH lookup cache — equivalent to bash's command_hash
  (define *which-cache* (make-hashtable string-hash string=?))
  (define *which-cache-path* #f)
  (define (which-cache-invalidate!)
    (hashtable-clear! *which-cache*)
    (set! *which-cache-path* #f))
  (define (which-cached name)
    (if (string-contains? name "/")
        (which name)
        (let ([current-path (or (getenv "PATH" #f) "/usr/bin:/bin")])
          (unless (equal? current-path *which-cache-path*)
            (hashtable-clear! *which-cache*)
            (set! *which-cache-path* current-path))
          (let ([cached (hashtable-ref *which-cache* name #f)])
            (or cached
                (let ([found (which name)])
                  (when found (hashtable-set! *which-cache* name found))
                  found))))))
  (define find-file-in-path
    (case-lambda
      [(name)
       (let* ([path-str #f])
         (let ([path-dirs (string-split-chars
                            (or path-str
                                (getenv "PATH" #f)
                                "/usr/bin:/bin")
                            ":")])
           (let loop ([dirs path-dirs])
             (if (null? dirs)
                 #f
                 (let ([full (string-append (car dirs) "/" name)])
                   (if (and (file-exists? full)
                            (not (file-directory? full)))
                       full
                       (loop (cdr dirs))))))))]
      [(name path-str)
       (let ([path-dirs (string-split-chars
                          (or path-str (getenv "PATH" #f) "/usr/bin:/bin")
                          ":")])
         (let loop ([dirs path-dirs])
           (if (null? dirs)
               #f
               (let ([full (string-append (car dirs) "/" name)])
                 (if (and (file-exists? full) (not (file-directory? full)))
                     full
                     (loop (cdr dirs)))))))]))
  (define (executable? path)
    (and (not (file-directory? path))
         (guard (__exn [#t ((lambda (e) #f) __exn)])
           (= (ffi-access path 1) 0))))
  (define (home-directory)
    (or (getenv "HOME" #f)
        (guard (__exn [#t ((lambda (e) "/") __exn)])
          (user-info-home (user-info (user-name))))))
  (define (expand-tilde path)
    (cond
      [(string=? path "~") (home-directory)]
      [(and (> (string-length path) 1)
            (char=? (string-ref path 0) #\~)
            (char=? (string-ref path 1) #\/))
       (string-append
         (home-directory)
         (substring path 1 (string-length path)))]
      [(and (> (string-length path) 1)
            (char=? (string-ref path 0) #\~))
       (let* ([slash-pos (string-index path #\/)])
         (let* ([username (if slash-pos
                              (substring path 1 slash-pos)
                              (substring path 1 (string-length path)))])
           (guard (__exn [#t ((lambda (e) path) __exn)])
             (let ([home (user-info-home (user-info username))])
               (if slash-pos
                   (string-append
                     home
                     (substring path slash-pos (string-length path)))
                   home)))))]
      [else path]))
  (define (read-file-lines path)
    (if (file-exists? path)
        (call-with-input-file
          path
          (lambda (port)
            (let loop ([lines (list)])
              (let ([line (get-line port)])
                (if (eof-object? line)
                    (reverse lines)
                    (loop (cons line lines)))))))
        (list)))
  (define (exception-message e)
    (define (format-condition e)
      (let ([msg (call-with-string-output-port
                   (lambda (p) (display-condition e p)))])
        (if (and (> (string-length msg) 11)
                 (string=? (substring msg 0 11) "Exception: "))
            (substring msg 11 (string-length msg))
            (if (and (> (string-length msg) 13)
                     (string=? (substring msg 0 13) "Exception in "))
                (let loop ([i 13])
                  (cond
                    [(>= (+ i 1) (string-length msg)) msg]
                    [(and (char=? (string-ref msg i) #\:)
                          (char=? (string-ref msg (+ i 1)) #\space))
                     (substring msg (+ i 2) (string-length msg))]
                    [else (loop (+ i 1))]))
                msg))))
    (cond
      [(string? e) e]
      [(condition? e) (format-condition e)]
      [else (call-with-string-output-port
              (lambda (p) (display e p)))]))
  (define (try-or-false thunk)
    (guard (__exn [#t ((lambda (e) #f) __exn)]) (thunk)))
  (define (safe-substring str start end)
    (let ([len (string-length str)])
      (substring str (min start len) (min end len))))
  (define (make-string-repeated ch n) (make-string n ch))
  (define (valid-name? str)
    (and (> (string-length str) 0)
         (let ([ch (string-ref str 0)])
           (or (char-alphabetic? ch) (char=? ch #\_)))
         (let loop ([i 1])
           (or (>= i (string-length str))
               (let ([ch (string-ref str i)])
                 (and (or (char-alphabetic? ch)
                          (char-numeric? ch)
                          (char=? ch #\_))
                      (loop (+ i 1))))))))
  (define (string->integer str)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (let ([n (string->number str)])
        (and (integer? n) (exact? n) n))))
  (define (string-upcase str)
    (list->string (map char-upcase (string->list str))))
  (define (string-downcase str)
    (list->string (map char-downcase (string->list str))))
  (define (string-has-control-chars? s)
    (let ([len (string-length s)])
      (let loop ([i 0])
        (if (>= i len)
            #f
            (let ([c (char->integer (string-ref s i))])
              (if (or (< c 32) (= c 127)) #t (loop (+ i 1))))))))
  (define (hex-char? ch)
    (or (and (char>=? ch #\0) (char<=? ch #\9))
        (and (char>=? ch #\a) (char<=? ch #\f))
        (and (char>=? ch #\A) (char<=? ch #\F))))
  (define (utf8-string-length str)
    (let ([len (string-length str)])
      (let check ([i 0])
        (if (>= i len)
            len
            (let ([c (char->integer (string-ref str i))])
              (if (> c 127)
                  (utf8-count-codepoints str len)
                  (check (+ i 1))))))))
  (define (utf8-count-codepoints str len)
    (let loop ([i 0] [count 0])
      (if (>= i len)
          count
          (let ([b (char->integer (string-ref str i))])
            (cond
              [(< b 128) (loop (+ i 1) (+ count 1))]
              [(< b 192) (loop (+ i 1) count)]
              [(< b 224) (loop (+ i 1) (+ count 1))]
              [(< b 240) (loop (+ i 1) (+ count 1))]
              [else (loop (+ i 1) (+ count 1))])))))
  (define (utf8-byte-count str)
    (let ([len (string-length str)])
      (let loop ([i 0] [bytes 0])
        (if (>= i len)
            bytes
            (let ([cp (char->integer (string-ref str i))])
              (loop
                (+ i 1)
                (+ bytes
                   (cond
                     [(< cp 128) 1]
                     [(< cp 2048) 2]
                     [(< cp 65536) 3]
                     [else 4]))))))))
  (define (file-regular? path)
    (and (file-exists? path)
         (guard (__exn [#t ((lambda (e) #f) __exn)])
           (eq? (gambit-file-info-type (gambit-file-info path))
                'regular))))
  (define (file-directory? path)
    (and (file-exists? path)
         (guard (__exn [#t ((lambda (e) #f) __exn)])
           (eq? (gambit-file-info-type (gambit-file-info path))
                'directory))))
  (define (file-symlink? path)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (eq? (gambit-file-info-type (gambit-file-info path #f))
           'symbolic-link)))
  (define (file-nonempty? path)
    (and (file-exists? path)
         (guard (__exn [#t ((lambda (e) #f) __exn)])
           (> (file-info-size (gambit-file-info path)) 0))))
  (define (file-readable? path)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (= (ffi-access path 4) 0)))
  (define (file-writable? path)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (= (ffi-access path 2) 0)))
  (define raw-byte-base 57344)
  (define (byte->raw-char val)
    (if (>= val 128)
        (integer->char (+ raw-byte-base val))
        (integer->char val)))
  (define (raw-byte-char? ch)
    (let ([code (char->integer ch)])
      (and (>= code raw-byte-base)
           (<= code (+ raw-byte-base 255)))))
  (define (shell-display str)
    (let ([len (string-length str)])
      (let scan ([i 0])
        (cond
          [(>= i len) (display str)]
          [(raw-byte-char? (string-ref str i))
           (shell-display-raw str)]
          [else (scan (+ i 1))]))))
  (define (shell-display-raw str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (when (< i len)
          (let* ([ch (string-ref str i)])
            (let* ([code (char->integer ch)])
              (if (and (>= code raw-byte-base)
                       (<= code (+ raw-byte-base 255)))
                  (begin
                    (flush-output-port (current-output-port))
                    (write-u8 (- code raw-byte-base))
                    (flush-output-port (current-output-port)))
                  (display ch))))
          (loop (+ i 1)))))))
