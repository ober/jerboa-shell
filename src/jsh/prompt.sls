#!chezscheme
(library (jsh prompt)
  (export expand-prompt prompt-width current-time-values
    time-string-24h time-string-12h time-string-ampm
    time-string-hhmm date-string seconds->date-string
    hostname-short path-basename string-prefix?
    string-index-from)
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
    (except (std misc string) string-prefix? string-join
      string-split string-index string-empty?)
    (except (std misc list) take drop filter-map)
    (except (std misc alist) pget pgetv pgetq aget agetv agetq)
    (except
      (std os path)
      path-expand
      path-normalize
      path-absolute?)
    (except (std format) format) (std sort) (std pregexp)
    (std sugar) (jsh ffi)
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase)
    (only (jsh expander) find-matching-paren))
  (define expand-prompt
    (case-lambda
      [(ps-string env-get)
       (let* ([job-count 0]
              [cmd-number 0]
              [history-number 0]
              [cmd-exec-fn #f])
         (let ([len (string-length ps-string)]
               [out (open-output-string)])
           (let loop ([i 0])
             (cond
               [(>= i len) (get-output-string out)]
               [(and cmd-exec-fn
                     (char=? (string-ref ps-string i) #\$)
                     (< (+ i 1) len)
                     (char=? (string-ref ps-string (+ i 1)) #\())
                (let ([close (find-matching-paren ps-string (+ i 2))])
                  (if close
                      (let* ([cmd-str (substring ps-string (+ i 2) close)])
                        (let* ([output (guard (__exn
                                                [#t
                                                 ((lambda (e) "") __exn)])
                                         (cmd-exec-fn cmd-str))])
                          (display output out)
                          (loop (+ close 1))))
                      (begin (display "$(" out) (loop (+ i 2)))))]
               [(and (char=? (string-ref ps-string i) #\\) (< (+ i 1) len))
                (let ([ch (string-ref ps-string (+ i 1))])
                  (case ch
                    [(#\u)
                     (display (or (env-get "USER") (user-name)) out)
                     (loop (+ i 2))]
                    [(#\h)
                     (let* ([host (or (env-get "HOSTNAME")
                                      (guard (__exn
                                               [#t
                                                ((lambda (e) "localhost")
                                                  __exn)])
                                        (hostname-short)))])
                       (let* ([dot (string-index host #\.)])
                         (display (if dot (substring host 0 dot) host) out)
                         (loop (+ i 2))))]
                    [(#\H)
                     (display
                       (or (env-get "HOSTNAME")
                           (guard (__exn
                                    [#t ((lambda (e) "localhost") __exn)])
                             (hostname-short)))
                       out)
                     (loop (+ i 2))]
                    [(#\w)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (let* ([display-pwd (if (and (> (string-length
                                                           home)
                                                         0)
                                                      (let ([pfx home]
                                                            [str pwd])
                                                        (let ([plen (string-length
                                                                      pfx)])
                                                          (and (<= plen
                                                                   (string-length
                                                                     str))
                                                               (string=?
                                                                 pfx
                                                                 (substring
                                                                   str
                                                                   0
                                                                   plen))))))
                                                 (string-append
                                                   "~"
                                                   (substring
                                                     pwd
                                                     (string-length home)
                                                     (string-length pwd)))
                                                 pwd)])
                           (display display-pwd out)
                           (loop (+ i 2)))))]
                    [(#\W)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (if (string=? pwd home)
                             (display "~" out)
                             (display (path-basename pwd) out))
                         (loop (+ i 2))))]
                    [(#\d) (display (date-string) out) (loop (+ i 2))]
                    [(#\t) (display (time-string-24h) out) (loop (+ i 2))]
                    [(#\T) (display (time-string-12h) out) (loop (+ i 2))]
                    [(#\@) (display (time-string-ampm) out) (loop (+ i 2))]
                    [(#\A) (display (time-string-hhmm) out) (loop (+ i 2))]
                    [(#\n) (display "\n" out) (loop (+ i 2))]
                    [(#\r) (display "\r" out) (loop (+ i 2))]
                    [(#\s) (display "jsh" out) (loop (+ i 2))]
                    [(#\v) (display "0.1" out) (loop (+ i 2))]
                    [(#\V) (display "0.1.0" out) (loop (+ i 2))]
                    [(#\j)
                     (display (number->string job-count) out)
                     (loop (+ i 2))]
                    [(#\l) (display "tty" out) (loop (+ i 2))]
                    [(#\#)
                     (display (number->string cmd-number) out)
                     (loop (+ i 2))]
                    [(#\!)
                     (display (number->string history-number) out)
                     (loop (+ i 2))]
                    [(#\$)
                     (display (if (= (ffi-geteuid) 0) "#" "$") out)
                     (loop (+ i 2))]
                    [(#\a) (display "\x0;07" out) (loop (+ i 2))]
                    [(#\\) (display "\\" out) (loop (+ i 2))]
                    [(#\D)
                     (if (and (< (+ i 2) len)
                              (char=? (string-ref ps-string (+ i 2)) #\{))
                         (let ([close (string-index-from
                                        ps-string
                                        #\}
                                        (+ i 3))])
                           (if close
                               (begin
                                 (display (date-string) out)
                                 (loop (+ close 1)))
                               (begin (display "\\D" out) (loop (+ i 2)))))
                         (begin (display "\\D" out) (loop (+ i 2))))]
                    [(#\[) (loop (+ i 2))]
                    [(#\]) (loop (+ i 2))]
                    [else
                     (display "\\" out)
                     (display (string ch) out)
                     (loop (+ i 2))]))]
               [else
                (display (string (string-ref ps-string i)) out)
                (loop (+ i 1))]))))]
      [(ps-string env-get job-count)
       (let* ([cmd-number 0] [history-number 0] [cmd-exec-fn #f])
         (let ([len (string-length ps-string)]
               [out (open-output-string)])
           (let loop ([i 0])
             (cond
               [(>= i len) (get-output-string out)]
               [(and cmd-exec-fn
                     (char=? (string-ref ps-string i) #\$)
                     (< (+ i 1) len)
                     (char=? (string-ref ps-string (+ i 1)) #\())
                (let ([close (find-matching-paren ps-string (+ i 2))])
                  (if close
                      (let* ([cmd-str (substring ps-string (+ i 2) close)])
                        (let* ([output (guard (__exn
                                                [#t
                                                 ((lambda (e) "") __exn)])
                                         (cmd-exec-fn cmd-str))])
                          (display output out)
                          (loop (+ close 1))))
                      (begin (display "$(" out) (loop (+ i 2)))))]
               [(and (char=? (string-ref ps-string i) #\\) (< (+ i 1) len))
                (let ([ch (string-ref ps-string (+ i 1))])
                  (case ch
                    [(#\u)
                     (display (or (env-get "USER") (user-name)) out)
                     (loop (+ i 2))]
                    [(#\h)
                     (let* ([host (or (env-get "HOSTNAME")
                                      (guard (__exn
                                               [#t
                                                ((lambda (e) "localhost")
                                                  __exn)])
                                        (hostname-short)))])
                       (let* ([dot (string-index host #\.)])
                         (display (if dot (substring host 0 dot) host) out)
                         (loop (+ i 2))))]
                    [(#\H)
                     (display
                       (or (env-get "HOSTNAME")
                           (guard (__exn
                                    [#t ((lambda (e) "localhost") __exn)])
                             (hostname-short)))
                       out)
                     (loop (+ i 2))]
                    [(#\w)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (let* ([display-pwd (if (and (> (string-length
                                                           home)
                                                         0)
                                                      (let ([pfx home]
                                                            [str pwd])
                                                        (let ([plen (string-length
                                                                      pfx)])
                                                          (and (<= plen
                                                                   (string-length
                                                                     str))
                                                               (string=?
                                                                 pfx
                                                                 (substring
                                                                   str
                                                                   0
                                                                   plen))))))
                                                 (string-append
                                                   "~"
                                                   (substring
                                                     pwd
                                                     (string-length home)
                                                     (string-length pwd)))
                                                 pwd)])
                           (display display-pwd out)
                           (loop (+ i 2)))))]
                    [(#\W)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (if (string=? pwd home)
                             (display "~" out)
                             (display (path-basename pwd) out))
                         (loop (+ i 2))))]
                    [(#\d) (display (date-string) out) (loop (+ i 2))]
                    [(#\t) (display (time-string-24h) out) (loop (+ i 2))]
                    [(#\T) (display (time-string-12h) out) (loop (+ i 2))]
                    [(#\@) (display (time-string-ampm) out) (loop (+ i 2))]
                    [(#\A) (display (time-string-hhmm) out) (loop (+ i 2))]
                    [(#\n) (display "\n" out) (loop (+ i 2))]
                    [(#\r) (display "\r" out) (loop (+ i 2))]
                    [(#\s) (display "jsh" out) (loop (+ i 2))]
                    [(#\v) (display "0.1" out) (loop (+ i 2))]
                    [(#\V) (display "0.1.0" out) (loop (+ i 2))]
                    [(#\j)
                     (display (number->string job-count) out)
                     (loop (+ i 2))]
                    [(#\l) (display "tty" out) (loop (+ i 2))]
                    [(#\#)
                     (display (number->string cmd-number) out)
                     (loop (+ i 2))]
                    [(#\!)
                     (display (number->string history-number) out)
                     (loop (+ i 2))]
                    [(#\$)
                     (display (if (= (ffi-geteuid) 0) "#" "$") out)
                     (loop (+ i 2))]
                    [(#\a) (display "\x0;07" out) (loop (+ i 2))]
                    [(#\\) (display "\\" out) (loop (+ i 2))]
                    [(#\D)
                     (if (and (< (+ i 2) len)
                              (char=? (string-ref ps-string (+ i 2)) #\{))
                         (let ([close (string-index-from
                                        ps-string
                                        #\}
                                        (+ i 3))])
                           (if close
                               (begin
                                 (display (date-string) out)
                                 (loop (+ close 1)))
                               (begin (display "\\D" out) (loop (+ i 2)))))
                         (begin (display "\\D" out) (loop (+ i 2))))]
                    [(#\[) (loop (+ i 2))]
                    [(#\]) (loop (+ i 2))]
                    [else
                     (display "\\" out)
                     (display (string ch) out)
                     (loop (+ i 2))]))]
               [else
                (display (string (string-ref ps-string i)) out)
                (loop (+ i 1))]))))]
      [(ps-string env-get job-count cmd-number)
       (let* ([history-number 0] [cmd-exec-fn #f])
         (let ([len (string-length ps-string)]
               [out (open-output-string)])
           (let loop ([i 0])
             (cond
               [(>= i len) (get-output-string out)]
               [(and cmd-exec-fn
                     (char=? (string-ref ps-string i) #\$)
                     (< (+ i 1) len)
                     (char=? (string-ref ps-string (+ i 1)) #\())
                (let ([close (find-matching-paren ps-string (+ i 2))])
                  (if close
                      (let* ([cmd-str (substring ps-string (+ i 2) close)])
                        (let* ([output (guard (__exn
                                                [#t
                                                 ((lambda (e) "") __exn)])
                                         (cmd-exec-fn cmd-str))])
                          (display output out)
                          (loop (+ close 1))))
                      (begin (display "$(" out) (loop (+ i 2)))))]
               [(and (char=? (string-ref ps-string i) #\\) (< (+ i 1) len))
                (let ([ch (string-ref ps-string (+ i 1))])
                  (case ch
                    [(#\u)
                     (display (or (env-get "USER") (user-name)) out)
                     (loop (+ i 2))]
                    [(#\h)
                     (let* ([host (or (env-get "HOSTNAME")
                                      (guard (__exn
                                               [#t
                                                ((lambda (e) "localhost")
                                                  __exn)])
                                        (hostname-short)))])
                       (let* ([dot (string-index host #\.)])
                         (display (if dot (substring host 0 dot) host) out)
                         (loop (+ i 2))))]
                    [(#\H)
                     (display
                       (or (env-get "HOSTNAME")
                           (guard (__exn
                                    [#t ((lambda (e) "localhost") __exn)])
                             (hostname-short)))
                       out)
                     (loop (+ i 2))]
                    [(#\w)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (let* ([display-pwd (if (and (> (string-length
                                                           home)
                                                         0)
                                                      (let ([pfx home]
                                                            [str pwd])
                                                        (let ([plen (string-length
                                                                      pfx)])
                                                          (and (<= plen
                                                                   (string-length
                                                                     str))
                                                               (string=?
                                                                 pfx
                                                                 (substring
                                                                   str
                                                                   0
                                                                   plen))))))
                                                 (string-append
                                                   "~"
                                                   (substring
                                                     pwd
                                                     (string-length home)
                                                     (string-length pwd)))
                                                 pwd)])
                           (display display-pwd out)
                           (loop (+ i 2)))))]
                    [(#\W)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (if (string=? pwd home)
                             (display "~" out)
                             (display (path-basename pwd) out))
                         (loop (+ i 2))))]
                    [(#\d) (display (date-string) out) (loop (+ i 2))]
                    [(#\t) (display (time-string-24h) out) (loop (+ i 2))]
                    [(#\T) (display (time-string-12h) out) (loop (+ i 2))]
                    [(#\@) (display (time-string-ampm) out) (loop (+ i 2))]
                    [(#\A) (display (time-string-hhmm) out) (loop (+ i 2))]
                    [(#\n) (display "\n" out) (loop (+ i 2))]
                    [(#\r) (display "\r" out) (loop (+ i 2))]
                    [(#\s) (display "jsh" out) (loop (+ i 2))]
                    [(#\v) (display "0.1" out) (loop (+ i 2))]
                    [(#\V) (display "0.1.0" out) (loop (+ i 2))]
                    [(#\j)
                     (display (number->string job-count) out)
                     (loop (+ i 2))]
                    [(#\l) (display "tty" out) (loop (+ i 2))]
                    [(#\#)
                     (display (number->string cmd-number) out)
                     (loop (+ i 2))]
                    [(#\!)
                     (display (number->string history-number) out)
                     (loop (+ i 2))]
                    [(#\$)
                     (display (if (= (ffi-geteuid) 0) "#" "$") out)
                     (loop (+ i 2))]
                    [(#\a) (display "\x0;07" out) (loop (+ i 2))]
                    [(#\\) (display "\\" out) (loop (+ i 2))]
                    [(#\D)
                     (if (and (< (+ i 2) len)
                              (char=? (string-ref ps-string (+ i 2)) #\{))
                         (let ([close (string-index-from
                                        ps-string
                                        #\}
                                        (+ i 3))])
                           (if close
                               (begin
                                 (display (date-string) out)
                                 (loop (+ close 1)))
                               (begin (display "\\D" out) (loop (+ i 2)))))
                         (begin (display "\\D" out) (loop (+ i 2))))]
                    [(#\[) (loop (+ i 2))]
                    [(#\]) (loop (+ i 2))]
                    [else
                     (display "\\" out)
                     (display (string ch) out)
                     (loop (+ i 2))]))]
               [else
                (display (string (string-ref ps-string i)) out)
                (loop (+ i 1))]))))]
      [(ps-string env-get job-count cmd-number history-number)
       (let* ([cmd-exec-fn #f])
         (let ([len (string-length ps-string)]
               [out (open-output-string)])
           (let loop ([i 0])
             (cond
               [(>= i len) (get-output-string out)]
               [(and cmd-exec-fn
                     (char=? (string-ref ps-string i) #\$)
                     (< (+ i 1) len)
                     (char=? (string-ref ps-string (+ i 1)) #\())
                (let ([close (find-matching-paren ps-string (+ i 2))])
                  (if close
                      (let* ([cmd-str (substring ps-string (+ i 2) close)])
                        (let* ([output (guard (__exn
                                                [#t
                                                 ((lambda (e) "") __exn)])
                                         (cmd-exec-fn cmd-str))])
                          (display output out)
                          (loop (+ close 1))))
                      (begin (display "$(" out) (loop (+ i 2)))))]
               [(and (char=? (string-ref ps-string i) #\\) (< (+ i 1) len))
                (let ([ch (string-ref ps-string (+ i 1))])
                  (case ch
                    [(#\u)
                     (display (or (env-get "USER") (user-name)) out)
                     (loop (+ i 2))]
                    [(#\h)
                     (let* ([host (or (env-get "HOSTNAME")
                                      (guard (__exn
                                               [#t
                                                ((lambda (e) "localhost")
                                                  __exn)])
                                        (hostname-short)))])
                       (let* ([dot (string-index host #\.)])
                         (display (if dot (substring host 0 dot) host) out)
                         (loop (+ i 2))))]
                    [(#\H)
                     (display
                       (or (env-get "HOSTNAME")
                           (guard (__exn
                                    [#t ((lambda (e) "localhost") __exn)])
                             (hostname-short)))
                       out)
                     (loop (+ i 2))]
                    [(#\w)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (let* ([display-pwd (if (and (> (string-length
                                                           home)
                                                         0)
                                                      (let ([pfx home]
                                                            [str pwd])
                                                        (let ([plen (string-length
                                                                      pfx)])
                                                          (and (<= plen
                                                                   (string-length
                                                                     str))
                                                               (string=?
                                                                 pfx
                                                                 (substring
                                                                   str
                                                                   0
                                                                   plen))))))
                                                 (string-append
                                                   "~"
                                                   (substring
                                                     pwd
                                                     (string-length home)
                                                     (string-length pwd)))
                                                 pwd)])
                           (display display-pwd out)
                           (loop (+ i 2)))))]
                    [(#\W)
                     (let* ([pwd (or (env-get "PWD") (current-directory))])
                       (let* ([home (or (env-get "HOME") "")])
                         (if (string=? pwd home)
                             (display "~" out)
                             (display (path-basename pwd) out))
                         (loop (+ i 2))))]
                    [(#\d) (display (date-string) out) (loop (+ i 2))]
                    [(#\t) (display (time-string-24h) out) (loop (+ i 2))]
                    [(#\T) (display (time-string-12h) out) (loop (+ i 2))]
                    [(#\@) (display (time-string-ampm) out) (loop (+ i 2))]
                    [(#\A) (display (time-string-hhmm) out) (loop (+ i 2))]
                    [(#\n) (display "\n" out) (loop (+ i 2))]
                    [(#\r) (display "\r" out) (loop (+ i 2))]
                    [(#\s) (display "jsh" out) (loop (+ i 2))]
                    [(#\v) (display "0.1" out) (loop (+ i 2))]
                    [(#\V) (display "0.1.0" out) (loop (+ i 2))]
                    [(#\j)
                     (display (number->string job-count) out)
                     (loop (+ i 2))]
                    [(#\l) (display "tty" out) (loop (+ i 2))]
                    [(#\#)
                     (display (number->string cmd-number) out)
                     (loop (+ i 2))]
                    [(#\!)
                     (display (number->string history-number) out)
                     (loop (+ i 2))]
                    [(#\$)
                     (display (if (= (ffi-geteuid) 0) "#" "$") out)
                     (loop (+ i 2))]
                    [(#\a) (display "\x0;07" out) (loop (+ i 2))]
                    [(#\\) (display "\\" out) (loop (+ i 2))]
                    [(#\D)
                     (if (and (< (+ i 2) len)
                              (char=? (string-ref ps-string (+ i 2)) #\{))
                         (let ([close (string-index-from
                                        ps-string
                                        #\}
                                        (+ i 3))])
                           (if close
                               (begin
                                 (display (date-string) out)
                                 (loop (+ close 1)))
                               (begin (display "\\D" out) (loop (+ i 2)))))
                         (begin (display "\\D" out) (loop (+ i 2))))]
                    [(#\[) (loop (+ i 2))]
                    [(#\]) (loop (+ i 2))]
                    [else
                     (display "\\" out)
                     (display (string ch) out)
                     (loop (+ i 2))]))]
               [else
                (display (string (string-ref ps-string i)) out)
                (loop (+ i 1))]))))]
      [(ps-string env-get job-count cmd-number history-number
        cmd-exec-fn)
       (let ([len (string-length ps-string)]
             [out (open-output-string)])
         (let loop ([i 0])
           (cond
             [(>= i len) (get-output-string out)]
             [(and cmd-exec-fn
                   (char=? (string-ref ps-string i) #\$)
                   (< (+ i 1) len)
                   (char=? (string-ref ps-string (+ i 1)) #\())
              (let ([close (find-matching-paren ps-string (+ i 2))])
                (if close
                    (let* ([cmd-str (substring ps-string (+ i 2) close)])
                      (let* ([output (guard (__exn
                                              [#t ((lambda (e) "") __exn)])
                                       (cmd-exec-fn cmd-str))])
                        (display output out)
                        (loop (+ close 1))))
                    (begin (display "$(" out) (loop (+ i 2)))))]
             [(and (char=? (string-ref ps-string i) #\\) (< (+ i 1) len))
              (let ([ch (string-ref ps-string (+ i 1))])
                (case ch
                  [(#\u)
                   (display (or (env-get "USER") (user-name)) out)
                   (loop (+ i 2))]
                  [(#\h)
                   (let* ([host (or (env-get "HOSTNAME")
                                    (guard (__exn
                                             [#t
                                              ((lambda (e) "localhost")
                                                __exn)])
                                      (hostname-short)))])
                     (let* ([dot (string-index host #\.)])
                       (display (if dot (substring host 0 dot) host) out)
                       (loop (+ i 2))))]
                  [(#\H)
                   (display
                     (or (env-get "HOSTNAME")
                         (guard (__exn
                                  [#t ((lambda (e) "localhost") __exn)])
                           (hostname-short)))
                     out)
                   (loop (+ i 2))]
                  [(#\w)
                   (let* ([pwd (or (env-get "PWD") (current-directory))])
                     (let* ([home (or (env-get "HOME") "")])
                       (let* ([display-pwd (if (and (> (string-length home)
                                                       0)
                                                    (let ([pfx home]
                                                          [str pwd])
                                                      (let ([plen (string-length
                                                                    pfx)])
                                                        (and (<= plen
                                                                 (string-length
                                                                   str))
                                                             (string=?
                                                               pfx
                                                               (substring
                                                                 str
                                                                 0
                                                                 plen))))))
                                               (string-append
                                                 "~"
                                                 (substring
                                                   pwd
                                                   (string-length home)
                                                   (string-length pwd)))
                                               pwd)])
                         (display display-pwd out)
                         (loop (+ i 2)))))]
                  [(#\W)
                   (let* ([pwd (or (env-get "PWD") (current-directory))])
                     (let* ([home (or (env-get "HOME") "")])
                       (if (string=? pwd home)
                           (display "~" out)
                           (display (path-basename pwd) out))
                       (loop (+ i 2))))]
                  [(#\d) (display (date-string) out) (loop (+ i 2))]
                  [(#\t) (display (time-string-24h) out) (loop (+ i 2))]
                  [(#\T) (display (time-string-12h) out) (loop (+ i 2))]
                  [(#\@) (display (time-string-ampm) out) (loop (+ i 2))]
                  [(#\A) (display (time-string-hhmm) out) (loop (+ i 2))]
                  [(#\n) (display "\n" out) (loop (+ i 2))]
                  [(#\r) (display "\r" out) (loop (+ i 2))]
                  [(#\s) (display "jsh" out) (loop (+ i 2))]
                  [(#\v) (display "0.1" out) (loop (+ i 2))]
                  [(#\V) (display "0.1.0" out) (loop (+ i 2))]
                  [(#\j)
                   (display (number->string job-count) out)
                   (loop (+ i 2))]
                  [(#\l) (display "tty" out) (loop (+ i 2))]
                  [(#\#)
                   (display (number->string cmd-number) out)
                   (loop (+ i 2))]
                  [(#\!)
                   (display (number->string history-number) out)
                   (loop (+ i 2))]
                  [(#\$)
                   (display (if (= (ffi-geteuid) 0) "#" "$") out)
                   (loop (+ i 2))]
                  [(#\a) (display "\x0;07" out) (loop (+ i 2))]
                  [(#\\) (display "\\" out) (loop (+ i 2))]
                  [(#\D)
                   (if (and (< (+ i 2) len)
                            (char=? (string-ref ps-string (+ i 2)) #\{))
                       (let ([close (string-index-from
                                      ps-string
                                      #\}
                                      (+ i 3))])
                         (if close
                             (begin
                               (display (date-string) out)
                               (loop (+ close 1)))
                             (begin (display "\\D" out) (loop (+ i 2)))))
                       (begin (display "\\D" out) (loop (+ i 2))))]
                  [(#\[) (loop (+ i 2))]
                  [(#\]) (loop (+ i 2))]
                  [else
                   (display "\\" out)
                   (display (string ch) out)
                   (loop (+ i 2))]))]
             [else
              (display (string (string-ref ps-string i)) out)
              (loop (+ i 1))])))]))
  (define (prompt-width prompt-string)
    (let ([len (string-length prompt-string)])
      (let loop ([i 0] [width 0] [in-escape? #f])
        (cond
          [(>= i len) width]
          [(and (not in-escape?)
                (char=? (string-ref prompt-string i) #\esc))
           (loop (+ i 1) width #t)]
          [in-escape?
           (if (char-alphabetic? (string-ref prompt-string i))
               (loop (+ i 1) width #f)
               (loop (+ i 1) width #t))]
          [else (loop (+ i 1) (+ width 1) #f)]))))
  (define (current-time-values)
    (let* ([t (let ([t (current-time)])
                (if (time? t)
                    (+ (time-second t)
                       (/ (time-nanosecond t) 1000000000.0))
                    t))])
      (let* ([secs (inexact->exact (floor t))])
        (values (modulo secs 60) (modulo (quotient secs 60) 60)
          (modulo (quotient secs 3600) 24) 0 0 0 0))))
  (define (time-string-24h)
    (let-values ([(s m h d mo y wd) (current-time-values)])
      (format "~2,'0d:~2,'0d:~2,'0d" h m s)))
  (define (time-string-12h)
    (let-values ([(s m h d mo y wd) (current-time-values)])
      (let ([h12 (cond
                   [(= h 0) 12]
                   [(> h 12) (- h 12)]
                   [else h])])
        (format "~2,'0d:~2,'0d:~2,'0d" h12 m s))))
  (define (time-string-ampm)
    (let-values ([(s m h d mo y wd) (current-time-values)])
      (let ([h12 (cond [(= h 0) 12] [(> h 12) (- h 12)] [else h])]
            [ampm (if (>= h 12) "PM" "AM")])
        (format "~2,'0d:~2,'0d ~a" h12 m ampm))))
  (define (time-string-hhmm)
    (let-values ([(s m h d mo y wd) (current-time-values)])
      (format "~2,'0d:~2,'0d" h m)))
  (define (date-string)
    (let-values ([(s m h d mo y wd) (current-time-values)])
      (format
        "~a"
        (seconds->date-string
          (let ([t (current-time)])
            (if (time? t)
                (+ (time-second t) (/ (time-nanosecond t) 1000000000.0))
                t))))))
  (define (seconds->date-string secs)
    (guard (__exn [#t ((lambda (e) "???") __exn)])
      (let ([port (open-input-process
                    (list
                      'path:
                      "/bin/date"
                      'arguments:
                      (list "+%a %b %d")))])
        (let ([result (get-line port)])
          (close-port port)
          (if (string? result) result "???")))))
  (define (hostname-short)
    (guard (__exn [#t ((lambda (e) "localhost") __exn)])
      (let ([port (open-input-process
                    (list 'path: "/bin/hostname"))])
        (let ([result (get-line port)])
          (close-port port)
          (if (string? result) result "localhost")))))
  (define (path-basename path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) path]
        [(char=? (string-ref path i) #\/)
         (substring path (+ i 1) (string-length path))]
        [else (loop (- i 1))])))
  (define (string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))
  (define (string-index-from str ch start)
    (let loop ([i start])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))]))))
