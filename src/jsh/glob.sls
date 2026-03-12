#!chezscheme
(library (jsh glob)
  (export glob-pattern? glob-match? glob-remove-escapes
   glob-expand glob-pattern->pregexp pregexp-quote-char
   find-posix-class-end posix-class->regex-chars
   find-extglob-close extglob-split-pipes
   glob-sub-pattern->pregexp glob-join-alternatives
   glob-expand-path split-glob-path glob-expand-parts
   glob-match-dir directory-exists? glob-ignore-filter
   string-split-colon path-basename glob-any? needs-globbing?)
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
    (except (jsh pregexp-compat) pregexp-quote pregexp-replace*
      pregexp-replace pregexp-split pregexp-match
      pregexp-match-positions pregexp)
    (std sugar)
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase))
  (define glob-pattern?
    (case-lambda
      [(str)
       (let* ([extglob? #f])
         (let ([len (string-length str)])
           (let loop ([i 0] [escaped? #f])
             (if (>= i len)
                 #f
                 (let ([ch (string-ref str i)])
                   (cond
                     [escaped? (loop (+ i 1) #f)]
                     [(char=? ch #\\) (loop (+ i 1) #t)]
                     [(or (char=? ch #\*) (char=? ch #\?) (char=? ch #\[))
                      #t]
                     [(and extglob?
                           (< (+ i 1) len)
                           (char=? (string-ref str (+ i 1)) #\()
                           (or (char=? ch #\?)
                               (char=? ch #\*)
                               (char=? ch #\+)
                               (char=? ch #\@)
                               (char=? ch #\!)))
                      #t]
                     [else (loop (+ i 1) #f)]))))))]
      [(str extglob?)
       (let ([len (string-length str)])
         (let loop ([i 0] [escaped? #f])
           (if (>= i len)
               #f
               (let ([ch (string-ref str i)])
                 (cond
                   [escaped? (loop (+ i 1) #f)]
                   [(char=? ch #\\) (loop (+ i 1) #t)]
                   [(or (char=? ch #\*) (char=? ch #\?) (char=? ch #\[))
                    #t]
                   [(and extglob?
                         (< (+ i 1) len)
                         (char=? (string-ref str (+ i 1)) #\()
                         (or (char=? ch #\?)
                             (char=? ch #\*)
                             (char=? ch #\+)
                             (char=? ch #\@)
                             (char=? ch #\!)))
                    #t]
                   [else (loop (+ i 1) #f)])))))]))
  (define glob-match?
    (case-lambda
      [(pattern string)
       (let* ([path-mode? #t] [extglob? #f])
         (let ([rx (glob-pattern->pregexp
                     pattern
                     path-mode?
                     extglob?)])
           (and (pregexp-match rx string) #t)))]
      [(pattern string path-mode?)
       (let* ([extglob? #f])
         (let ([rx (glob-pattern->pregexp
                     pattern
                     path-mode?
                     extglob?)])
           (and (pregexp-match rx string) #t)))]
      [(pattern string path-mode? extglob?)
       (let ([rx (glob-pattern->pregexp
                   pattern
                   path-mode?
                   extglob?)])
         (and (pregexp-match rx string) #t))]))
  (define (glob-remove-escapes str)
    (let ([len (string-length str)])
      (if (not (string-index str #\\))
          str
          (let ([buf (open-output-string)])
            (let loop ([i 0])
              (if (>= i len)
                  (get-output-string buf)
                  (let ([ch (string-ref str i)])
                    (if (and (char=? ch #\\) (< (+ i 1) len))
                        (begin
                          (display (string-ref str (+ i 1)) buf)
                          (loop (+ i 2)))
                        (begin (display ch buf) (loop (+ i 1)))))))))))
  (define glob-expand
    (case-lambda
      [(pattern)
       (let* ([dotglob? #f]
              [nullglob? #f]
              [failglob? #f]
              [nocase? #f]
              [extglob? #f]
              [globskipdots? #t])
         (let ([matches (glob-expand-path pattern dotglob? nocase?
                          extglob? globskipdots?)])
           (cond
             [(pair? matches) (sort matches string<?)]
             [failglob? (raise (cons 'failglob pattern))]
             [nullglob? (list)]
             [else (list pattern)])))]
      [(pattern dotglob?)
       (let* ([nullglob? #f]
              [failglob? #f]
              [nocase? #f]
              [extglob? #f]
              [globskipdots? #t])
         (let ([matches (glob-expand-path pattern dotglob? nocase?
                          extglob? globskipdots?)])
           (cond
             [(pair? matches) (sort matches string<?)]
             [failglob? (raise (cons 'failglob pattern))]
             [nullglob? (list)]
             [else (list pattern)])))]
      [(pattern dotglob? nullglob?)
       (let* ([failglob? #f]
              [nocase? #f]
              [extglob? #f]
              [globskipdots? #t])
         (let ([matches (glob-expand-path pattern dotglob? nocase?
                          extglob? globskipdots?)])
           (cond
             [(pair? matches) (sort matches string<?)]
             [failglob? (raise (cons 'failglob pattern))]
             [nullglob? (list)]
             [else (list pattern)])))]
      [(pattern dotglob? nullglob? failglob?)
       (let* ([nocase? #f] [extglob? #f] [globskipdots? #t])
         (let ([matches (glob-expand-path pattern dotglob? nocase?
                          extglob? globskipdots?)])
           (cond
             [(pair? matches) (sort matches string<?)]
             [failglob? (raise (cons 'failglob pattern))]
             [nullglob? (list)]
             [else (list pattern)])))]
      [(pattern dotglob? nullglob? failglob? nocase?)
       (let* ([extglob? #f] [globskipdots? #t])
         (let ([matches (glob-expand-path pattern dotglob? nocase?
                          extglob? globskipdots?)])
           (cond
             [(pair? matches) (sort matches string<?)]
             [failglob? (raise (cons 'failglob pattern))]
             [nullglob? (list)]
             [else (list pattern)])))]
      [(pattern dotglob? nullglob? failglob? nocase? extglob?)
       (let* ([globskipdots? #t])
         (let ([matches (glob-expand-path pattern dotglob? nocase?
                          extglob? globskipdots?)])
           (cond
             [(pair? matches) (sort matches string<?)]
             [failglob? (raise (cons 'failglob pattern))]
             [nullglob? (list)]
             [else (list pattern)])))]
      [(pattern dotglob? nullglob? failglob? nocase? extglob?
        globskipdots?)
       (let ([matches (glob-expand-path pattern dotglob? nocase?
                        extglob? globskipdots?)])
         (cond
           [(pair? matches) (sort matches string<?)]
           [failglob? (raise (cons 'failglob pattern))]
           [nullglob? (list)]
           [else (list pattern)]))]))
  (define glob-pattern->pregexp
    (case-lambda
      [(pattern)
       (let* ([path-mode? #t] [extglob? #f])
         (let ([rx (open-output-string)]
               [len (string-length pattern)])
           (display "^" rx)
           (let loop ([i 0] [in-bracket? #f])
             (if (>= i (string-length pattern))
                 (begin (display "$" rx) (get-output-string rx))
                 (let ([ch (string-ref pattern i)])
                   (cond
                     [in-bracket?
                      (cond
                        [(and (char=? ch #\]) (eq? in-bracket? 'start))
                         (display "\\]" rx)
                         (loop (+ i 1) #t)]
                        [(char=? ch #\])
                         (display "]" rx)
                         (loop (+ i 1) #f)]
                        [(and (memq ch '(#\! #\^))
                              (eq? in-bracket? 'start))
                         (display "^" rx)
                         (if (and (< (+ i 1) (string-length pattern))
                                  (char=?
                                    (string-ref pattern (+ i 1))
                                    #\]))
                             (begin (display "\\]" rx) (loop (+ i 2) #t))
                             (loop (+ i 1) #t))]
                        [(and (char=? ch #\[)
                              (< (+ i 1) (string-length pattern))
                              (char=? (string-ref pattern (+ i 1)) #\:))
                         (let ([class-end (find-posix-class-end
                                            pattern
                                            (+ i 2))])
                           (if class-end
                               (let* ([class-name (substring
                                                    pattern
                                                    (+ i 2)
                                                    class-end)])
                                 (let* ([class-chars (posix-class->regex-chars
                                                       class-name)])
                                   (if class-chars
                                       (begin
                                         (display class-chars rx)
                                         (loop (+ class-end 2) #t))
                                       (begin
                                         (display "\\[" rx)
                                         (loop (+ i 1) #t)))))
                               (begin
                                 (display "\\[" rx)
                                 (loop (+ i 1) #t))))]
                        [(char=? ch #\[)
                         (display "\\[" rx)
                         (loop (+ i 1) #t)]
                        [(and (char=? ch #\\)
                              (< (+ i 1) (string-length pattern)))
                         (let ([esc-ch (string-ref pattern (+ i 1))])
                           (if (char=? esc-ch #\-)
                               (display "\\-" rx)
                               (display (pregexp-quote-char esc-ch) rx)))
                         (loop (+ i 2) #t)]
                        [else
                         (display (pregexp-quote-char ch) rx)
                         (loop (+ i 1) #t)])]
                     [(char=? ch #\\)
                      (if (< (+ i 1) (string-length pattern))
                          (begin
                            (display
                              (pregexp-quote-char
                                (string-ref pattern (+ i 1)))
                              rx)
                            (loop (+ i 2) #f))
                          (begin (display "\\\\" rx) (loop (+ i 1) #f)))]
                     [(and extglob?
                           (< (+ i 1) len)
                           (char=? (string-ref pattern (+ i 1)) #\()
                           (or (char=? ch #\?)
                               (char=? ch #\*)
                               (char=? ch #\+)
                               (char=? ch #\@)
                               (char=? ch #\!)))
                      (let ([close (find-extglob-close pattern (+ i 2))])
                        (if close
                            (let* ([body (substring
                                           pattern
                                           (+ i 2)
                                           close)])
                              (let* ([alternatives (extglob-split-pipes
                                                     body)])
                                (let* ([rx-alts (glob-join-alternatives
                                                  (map (lambda (alt)
                                                         (glob-sub-pattern->pregexp
                                                           alt
                                                           path-mode?
                                                           extglob?))
                                                       alternatives)
                                                  "|")])
                                  (cond
                                    [(char=? ch #\?)
                                     (display
                                       (string-append "(?:" rx-alts ")?")
                                       rx)]
                                    [(char=? ch #\*)
                                     (display
                                       (string-append "(?:" rx-alts ")*")
                                       rx)]
                                    [(char=? ch #\+)
                                     (display
                                       (string-append "(?:" rx-alts ")+")
                                       rx)]
                                    [(char=? ch #\@)
                                     (display
                                       (string-append "(?:" rx-alts ")")
                                       rx)]
                                    [(char=? ch #\!)
                                     (let ([rest-rx (glob-sub-pattern->pregexp
                                                      (substring
                                                        pattern
                                                        (+ close 1)
                                                        len)
                                                      path-mode?
                                                      extglob?)])
                                       (display
                                         (string-append "(?!(?:" rx-alts ")" rest-rx
                                           "$)"
                                           (if path-mode? "[^/]*" ".*"))
                                         rx))])
                                  (loop (+ close 1) #f))))
                            (begin
                              (display (pregexp-quote-char ch) rx)
                              (loop (+ i 1) #f))))]
                     [(char=? ch #\*)
                      (if (and (< (+ i 1) (string-length pattern))
                               (char=? (string-ref pattern (+ i 1)) #\*))
                          (begin (display ".*" rx) (loop (+ i 2) #f))
                          (if path-mode?
                              (begin
                                (display "[^/]*" rx)
                                (loop (+ i 1) #f))
                              (begin
                                (display "[\\s\\S]*" rx)
                                (loop (+ i 1) #f))))]
                     [(char=? ch #\?)
                      (display (if path-mode? "[^/]" "[\\s\\S]") rx)
                      (loop (+ i 1) #f)]
                     [(char=? ch #\[)
                      (let* ([start (+ i 1)])
                        (let* ([skip (cond
                                       [(>= start (string-length pattern))
                                        0]
                                       [(char=?
                                          (string-ref pattern start)
                                          #\!)
                                        (+ 1
                                           (if (and (< (+ start 1)
                                                       (string-length
                                                         pattern))
                                                    (char=?
                                                      (string-ref
                                                        pattern
                                                        (+ start 1))
                                                      #\]))
                                               1
                                               0))]
                                       [(char=?
                                          (string-ref pattern start)
                                          #\^)
                                        (+ 1
                                           (if (and (< (+ start 1)
                                                       (string-length
                                                         pattern))
                                                    (char=?
                                                      (string-ref
                                                        pattern
                                                        (+ start 1))
                                                      #\]))
                                               1
                                               0))]
                                       [(char=?
                                          (string-ref pattern start)
                                          #\])
                                        1]
                                       [else 0])])
                          (let* ([has-close? (let find ([j (+ start skip)])
                                               (cond
                                                 [(>= j
                                                      (string-length
                                                        pattern))
                                                  #f]
                                                 [(char=?
                                                    (string-ref pattern j)
                                                    #\])
                                                  #t]
                                                 [else (find (+ j 1))]))])
                            (if has-close?
                                (begin
                                  (display "[" rx)
                                  (loop (+ i 1) 'start))
                                (begin
                                  (display "\\[" rx)
                                  (loop (+ i 1) #f))))))]
                     [else
                      (display (pregexp-quote-char ch) rx)
                      (loop (+ i 1) #f)]))))))]
      [(pattern path-mode?)
       (let* ([extglob? #f])
         (let ([rx (open-output-string)]
               [len (string-length pattern)])
           (display "^" rx)
           (let loop ([i 0] [in-bracket? #f])
             (if (>= i (string-length pattern))
                 (begin (display "$" rx) (get-output-string rx))
                 (let ([ch (string-ref pattern i)])
                   (cond
                     [in-bracket?
                      (cond
                        [(and (char=? ch #\]) (eq? in-bracket? 'start))
                         (display "\\]" rx)
                         (loop (+ i 1) #t)]
                        [(char=? ch #\])
                         (display "]" rx)
                         (loop (+ i 1) #f)]
                        [(and (memq ch '(#\! #\^))
                              (eq? in-bracket? 'start))
                         (display "^" rx)
                         (if (and (< (+ i 1) (string-length pattern))
                                  (char=?
                                    (string-ref pattern (+ i 1))
                                    #\]))
                             (begin (display "\\]" rx) (loop (+ i 2) #t))
                             (loop (+ i 1) #t))]
                        [(and (char=? ch #\[)
                              (< (+ i 1) (string-length pattern))
                              (char=? (string-ref pattern (+ i 1)) #\:))
                         (let ([class-end (find-posix-class-end
                                            pattern
                                            (+ i 2))])
                           (if class-end
                               (let* ([class-name (substring
                                                    pattern
                                                    (+ i 2)
                                                    class-end)])
                                 (let* ([class-chars (posix-class->regex-chars
                                                       class-name)])
                                   (if class-chars
                                       (begin
                                         (display class-chars rx)
                                         (loop (+ class-end 2) #t))
                                       (begin
                                         (display "\\[" rx)
                                         (loop (+ i 1) #t)))))
                               (begin
                                 (display "\\[" rx)
                                 (loop (+ i 1) #t))))]
                        [(char=? ch #\[)
                         (display "\\[" rx)
                         (loop (+ i 1) #t)]
                        [(and (char=? ch #\\)
                              (< (+ i 1) (string-length pattern)))
                         (let ([esc-ch (string-ref pattern (+ i 1))])
                           (if (char=? esc-ch #\-)
                               (display "\\-" rx)
                               (display (pregexp-quote-char esc-ch) rx)))
                         (loop (+ i 2) #t)]
                        [else
                         (display (pregexp-quote-char ch) rx)
                         (loop (+ i 1) #t)])]
                     [(char=? ch #\\)
                      (if (< (+ i 1) (string-length pattern))
                          (begin
                            (display
                              (pregexp-quote-char
                                (string-ref pattern (+ i 1)))
                              rx)
                            (loop (+ i 2) #f))
                          (begin (display "\\\\" rx) (loop (+ i 1) #f)))]
                     [(and extglob?
                           (< (+ i 1) len)
                           (char=? (string-ref pattern (+ i 1)) #\()
                           (or (char=? ch #\?)
                               (char=? ch #\*)
                               (char=? ch #\+)
                               (char=? ch #\@)
                               (char=? ch #\!)))
                      (let ([close (find-extglob-close pattern (+ i 2))])
                        (if close
                            (let* ([body (substring
                                           pattern
                                           (+ i 2)
                                           close)])
                              (let* ([alternatives (extglob-split-pipes
                                                     body)])
                                (let* ([rx-alts (glob-join-alternatives
                                                  (map (lambda (alt)
                                                         (glob-sub-pattern->pregexp
                                                           alt
                                                           path-mode?
                                                           extglob?))
                                                       alternatives)
                                                  "|")])
                                  (cond
                                    [(char=? ch #\?)
                                     (display
                                       (string-append "(?:" rx-alts ")?")
                                       rx)]
                                    [(char=? ch #\*)
                                     (display
                                       (string-append "(?:" rx-alts ")*")
                                       rx)]
                                    [(char=? ch #\+)
                                     (display
                                       (string-append "(?:" rx-alts ")+")
                                       rx)]
                                    [(char=? ch #\@)
                                     (display
                                       (string-append "(?:" rx-alts ")")
                                       rx)]
                                    [(char=? ch #\!)
                                     (let ([rest-rx (glob-sub-pattern->pregexp
                                                      (substring
                                                        pattern
                                                        (+ close 1)
                                                        len)
                                                      path-mode?
                                                      extglob?)])
                                       (display
                                         (string-append "(?!(?:" rx-alts ")" rest-rx
                                           "$)"
                                           (if path-mode? "[^/]*" ".*"))
                                         rx))])
                                  (loop (+ close 1) #f))))
                            (begin
                              (display (pregexp-quote-char ch) rx)
                              (loop (+ i 1) #f))))]
                     [(char=? ch #\*)
                      (if (and (< (+ i 1) (string-length pattern))
                               (char=? (string-ref pattern (+ i 1)) #\*))
                          (begin (display ".*" rx) (loop (+ i 2) #f))
                          (if path-mode?
                              (begin
                                (display "[^/]*" rx)
                                (loop (+ i 1) #f))
                              (begin
                                (display "[\\s\\S]*" rx)
                                (loop (+ i 1) #f))))]
                     [(char=? ch #\?)
                      (display (if path-mode? "[^/]" "[\\s\\S]") rx)
                      (loop (+ i 1) #f)]
                     [(char=? ch #\[)
                      (let* ([start (+ i 1)])
                        (let* ([skip (cond
                                       [(>= start (string-length pattern))
                                        0]
                                       [(char=?
                                          (string-ref pattern start)
                                          #\!)
                                        (+ 1
                                           (if (and (< (+ start 1)
                                                       (string-length
                                                         pattern))
                                                    (char=?
                                                      (string-ref
                                                        pattern
                                                        (+ start 1))
                                                      #\]))
                                               1
                                               0))]
                                       [(char=?
                                          (string-ref pattern start)
                                          #\^)
                                        (+ 1
                                           (if (and (< (+ start 1)
                                                       (string-length
                                                         pattern))
                                                    (char=?
                                                      (string-ref
                                                        pattern
                                                        (+ start 1))
                                                      #\]))
                                               1
                                               0))]
                                       [(char=?
                                          (string-ref pattern start)
                                          #\])
                                        1]
                                       [else 0])])
                          (let* ([has-close? (let find ([j (+ start skip)])
                                               (cond
                                                 [(>= j
                                                      (string-length
                                                        pattern))
                                                  #f]
                                                 [(char=?
                                                    (string-ref pattern j)
                                                    #\])
                                                  #t]
                                                 [else (find (+ j 1))]))])
                            (if has-close?
                                (begin
                                  (display "[" rx)
                                  (loop (+ i 1) 'start))
                                (begin
                                  (display "\\[" rx)
                                  (loop (+ i 1) #f))))))]
                     [else
                      (display (pregexp-quote-char ch) rx)
                      (loop (+ i 1) #f)]))))))]
      [(pattern path-mode? extglob?)
       (let ([rx (open-output-string)]
             [len (string-length pattern)])
         (display "^" rx)
         (let loop ([i 0] [in-bracket? #f])
           (if (>= i (string-length pattern))
               (begin (display "$" rx) (get-output-string rx))
               (let ([ch (string-ref pattern i)])
                 (cond
                   [in-bracket?
                    (cond
                      [(and (char=? ch #\]) (eq? in-bracket? 'start))
                       (display "\\]" rx)
                       (loop (+ i 1) #t)]
                      [(char=? ch #\]) (display "]" rx) (loop (+ i 1) #f)]
                      [(and (memq ch '(#\! #\^)) (eq? in-bracket? 'start))
                       (display "^" rx)
                       (if (and (< (+ i 1) (string-length pattern))
                                (char=? (string-ref pattern (+ i 1)) #\]))
                           (begin (display "\\]" rx) (loop (+ i 2) #t))
                           (loop (+ i 1) #t))]
                      [(and (char=? ch #\[)
                            (< (+ i 1) (string-length pattern))
                            (char=? (string-ref pattern (+ i 1)) #\:))
                       (let ([class-end (find-posix-class-end
                                          pattern
                                          (+ i 2))])
                         (if class-end
                             (let* ([class-name (substring
                                                  pattern
                                                  (+ i 2)
                                                  class-end)])
                               (let* ([class-chars (posix-class->regex-chars
                                                     class-name)])
                                 (if class-chars
                                     (begin
                                       (display class-chars rx)
                                       (loop (+ class-end 2) #t))
                                     (begin
                                       (display "\\[" rx)
                                       (loop (+ i 1) #t)))))
                             (begin
                               (display "\\[" rx)
                               (loop (+ i 1) #t))))]
                      [(char=? ch #\[)
                       (display "\\[" rx)
                       (loop (+ i 1) #t)]
                      [(and (char=? ch #\\)
                            (< (+ i 1) (string-length pattern)))
                       (let ([esc-ch (string-ref pattern (+ i 1))])
                         (if (char=? esc-ch #\-)
                             (display "\\-" rx)
                             (display (pregexp-quote-char esc-ch) rx)))
                       (loop (+ i 2) #t)]
                      [else
                       (display (pregexp-quote-char ch) rx)
                       (loop (+ i 1) #t)])]
                   [(char=? ch #\\)
                    (if (< (+ i 1) (string-length pattern))
                        (begin
                          (display
                            (pregexp-quote-char
                              (string-ref pattern (+ i 1)))
                            rx)
                          (loop (+ i 2) #f))
                        (begin (display "\\\\" rx) (loop (+ i 1) #f)))]
                   [(and extglob?
                         (< (+ i 1) len)
                         (char=? (string-ref pattern (+ i 1)) #\()
                         (or (char=? ch #\?)
                             (char=? ch #\*)
                             (char=? ch #\+)
                             (char=? ch #\@)
                             (char=? ch #\!)))
                    (let ([close (find-extglob-close pattern (+ i 2))])
                      (if close
                          (let* ([body (substring pattern (+ i 2) close)])
                            (let* ([alternatives (extglob-split-pipes
                                                   body)])
                              (let* ([rx-alts (glob-join-alternatives
                                                (map (lambda (alt)
                                                       (glob-sub-pattern->pregexp
                                                         alt
                                                         path-mode?
                                                         extglob?))
                                                     alternatives)
                                                "|")])
                                (cond
                                  [(char=? ch #\?)
                                   (display
                                     (string-append "(?:" rx-alts ")?")
                                     rx)]
                                  [(char=? ch #\*)
                                   (display
                                     (string-append "(?:" rx-alts ")*")
                                     rx)]
                                  [(char=? ch #\+)
                                   (display
                                     (string-append "(?:" rx-alts ")+")
                                     rx)]
                                  [(char=? ch #\@)
                                   (display
                                     (string-append "(?:" rx-alts ")")
                                     rx)]
                                  [(char=? ch #\!)
                                   (let ([rest-rx (glob-sub-pattern->pregexp
                                                    (substring
                                                      pattern
                                                      (+ close 1)
                                                      len)
                                                    path-mode?
                                                    extglob?)])
                                     (display
                                       (string-append "(?!(?:" rx-alts ")" rest-rx "$)"
                                         (if path-mode? "[^/]*" ".*"))
                                       rx))])
                                (loop (+ close 1) #f))))
                          (begin
                            (display (pregexp-quote-char ch) rx)
                            (loop (+ i 1) #f))))]
                   [(char=? ch #\*)
                    (if (and (< (+ i 1) (string-length pattern))
                             (char=? (string-ref pattern (+ i 1)) #\*))
                        (begin (display ".*" rx) (loop (+ i 2) #f))
                        (if path-mode?
                            (begin (display "[^/]*" rx) (loop (+ i 1) #f))
                            (begin
                              (display "[\\s\\S]*" rx)
                              (loop (+ i 1) #f))))]
                   [(char=? ch #\?)
                    (display (if path-mode? "[^/]" "[\\s\\S]") rx)
                    (loop (+ i 1) #f)]
                   [(char=? ch #\[)
                    (let* ([start (+ i 1)])
                      (let* ([skip (cond
                                     [(>= start (string-length pattern)) 0]
                                     [(char=?
                                        (string-ref pattern start)
                                        #\!)
                                      (+ 1
                                         (if (and (< (+ start 1)
                                                     (string-length
                                                       pattern))
                                                  (char=?
                                                    (string-ref
                                                      pattern
                                                      (+ start 1))
                                                    #\]))
                                             1
                                             0))]
                                     [(char=?
                                        (string-ref pattern start)
                                        #\^)
                                      (+ 1
                                         (if (and (< (+ start 1)
                                                     (string-length
                                                       pattern))
                                                  (char=?
                                                    (string-ref
                                                      pattern
                                                      (+ start 1))
                                                    #\]))
                                             1
                                             0))]
                                     [(char=?
                                        (string-ref pattern start)
                                        #\])
                                      1]
                                     [else 0])])
                        (let* ([has-close? (let find ([j (+ start skip)])
                                             (cond
                                               [(>= j
                                                    (string-length
                                                      pattern))
                                                #f]
                                               [(char=?
                                                  (string-ref pattern j)
                                                  #\])
                                                #t]
                                               [else (find (+ j 1))]))])
                          (if has-close?
                              (begin
                                (display "[" rx)
                                (loop (+ i 1) 'start))
                              (begin
                                (display "\\[" rx)
                                (loop (+ i 1) #f))))))]
                   [else
                    (display (pregexp-quote-char ch) rx)
                    (loop (+ i 1) #f)])))))]))
  (define (pregexp-quote-char ch)
    (let ([s (string ch)])
      (if (pregexp-match
            "[\\\\\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}]"
            s)
          (string-append "\\" s)
          s)))
  (define (find-posix-class-end pattern start)
    (let ([len (string-length pattern)])
      (let loop ([i start])
        (cond
          [(>= (+ i 1) len) #f]
          [(and (char=? (string-ref pattern i) #\:)
                (char=? (string-ref pattern (+ i 1)) #\]))
           i]
          [(char=? (string-ref pattern i) #\]) #f]
          [else (loop (+ i 1))]))))
  (define (posix-class->regex-chars name)
    (cond
      [(string=? name "alpha") "a-zA-Z"]
      [(string=? name "digit") "0-9"]
      [(string=? name "alnum") "a-zA-Z0-9"]
      [(string=? name "upper") "A-Z"]
      [(string=? name "lower") "a-z"]
      [(string=? name "xdigit") "0-9a-fA-F"]
      [(string=? name "space") " \t\n\r\v\f"]
      [(string=? name "blank") " \t"]
      [(string=? name "print") " -~"]
      [(string=? name "graph") "!-~"]
      [(string=? name "cntrl") "\x0;-\x1F;\x7F;"]
      [(string=? name "punct") "!-/:-@\\[-`{-~"]
      [(string=? name "ascii") "\x0;-\x7F;"]
      [else #f]))
  (define (find-extglob-close pattern start)
    (let ([len (string-length pattern)])
      (let loop ([i start] [depth 1])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref pattern i) #\\) (loop (+ i 2) depth)]
          [(char=? (string-ref pattern i) #\()
           (loop (+ i 1) (+ depth 1))]
          [(char=? (string-ref pattern i) #\))
           (if (= depth 1) i (loop (+ i 1) (- depth 1)))]
          [else (loop (+ i 1) depth)]))))
  (define (extglob-split-pipes body)
    (let ([len (string-length body)])
      (let loop ([i 0] [start 0] [depth 0] [parts (list)])
        (cond
          [(>= i len) (reverse (cons (substring body start i) parts))]
          [(char=? (string-ref body i) #\\)
           (loop (+ i 2) start depth parts)]
          [(char=? (string-ref body i) #\()
           (loop (+ i 1) start (+ depth 1) parts)]
          [(char=? (string-ref body i) #\))
           (loop (+ i 1) start (- depth 1) parts)]
          [(and (char=? (string-ref body i) #\|) (= depth 0))
           (loop
             (+ i 1)
             (+ i 1)
             depth
             (cons (substring body start i) parts))]
          [else (loop (+ i 1) start depth parts)]))))
  (define (glob-sub-pattern->pregexp pattern path-mode?
           extglob?)
    (let ([rx (open-output-string)]
          [len (string-length pattern)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string rx)
            (let ([ch (string-ref pattern i)])
              (cond
                [(char=? ch #\\)
                 (if (< (+ i 1) len)
                     (begin
                       (display
                         (pregexp-quote-char (string-ref pattern (+ i 1)))
                         rx)
                       (loop (+ i 2)))
                     (begin (display "\\\\" rx) (loop (+ i 1))))]
                [(and extglob?
                      (< (+ i 1) len)
                      (char=? (string-ref pattern (+ i 1)) #\()
                      (or (char=? ch #\?)
                          (char=? ch #\*)
                          (char=? ch #\+)
                          (char=? ch #\@)
                          (char=? ch #\!)))
                 (let ([close (find-extglob-close pattern (+ i 2))])
                   (if close
                       (let* ([body (substring pattern (+ i 2) close)])
                         (let* ([alternatives (extglob-split-pipes body)])
                           (let* ([rx-alts (glob-join-alternatives
                                             (map (lambda (alt)
                                                    (glob-sub-pattern->pregexp
                                                      alt
                                                      path-mode?
                                                      extglob?))
                                                  alternatives)
                                             "|")])
                             (cond
                               [(char=? ch #\?)
                                (display
                                  (string-append "(?:" rx-alts ")?")
                                  rx)]
                               [(char=? ch #\*)
                                (display
                                  (string-append "(?:" rx-alts ")*")
                                  rx)]
                               [(char=? ch #\+)
                                (display
                                  (string-append "(?:" rx-alts ")+")
                                  rx)]
                               [(char=? ch #\@)
                                (display
                                  (string-append "(?:" rx-alts ")")
                                  rx)]
                               [(char=? ch #\!)
                                (let ([rest-rx (glob-sub-pattern->pregexp
                                                 (substring
                                                   pattern
                                                   (+ close 1)
                                                   len)
                                                 path-mode?
                                                 extglob?)])
                                  (display
                                    (string-append "(?!(?:" rx-alts ")" rest-rx "$)"
                                      (if path-mode? "[^/]*" ".*"))
                                    rx))])
                             (loop (+ close 1)))))
                       (begin
                         (display (pregexp-quote-char ch) rx)
                         (loop (+ i 1)))))]
                [(char=? ch #\*)
                 (display (if path-mode? "[^/]*" ".*") rx)
                 (loop (+ i 1))]
                [(char=? ch #\?)
                 (display (if path-mode? "[^/]" ".") rx)
                 (loop (+ i 1))]
                [(char=? ch #\[)
                 (display "[" rx)
                 (let bloop ([j (+ i 1)])
                   (cond
                     [(>= j len) (loop j)]
                     [(char=? (string-ref pattern j) #\])
                      (display "]" rx)
                      (loop (+ j 1))]
                     [(and (char=? (string-ref pattern j) #\\)
                           (< (+ j 1) len))
                      (display "\\" rx)
                      (display (string-ref pattern (+ j 1)) rx)
                      (bloop (+ j 2))]
                     [else
                      (display (string-ref pattern j) rx)
                      (bloop (+ j 1))]))]
                [else
                 (display (pregexp-quote-char ch) rx)
                 (loop (+ i 1))]))))))
  (define (glob-join-alternatives strs sep)
    (if (null? strs)
        ""
        (call-with-output-string
          (lambda (port)
            (display (car strs) port)
            (for-each
              (lambda (s) (display sep port) (display s port))
              (cdr strs))))))
  (define glob-expand-path
    (case-lambda
      [(pattern)
       (let* ([dotglob? #f]
              [nocase? #f]
              [extglob? #f]
              [globskipdots? #t])
         (let ([parts (split-glob-path pattern)])
           (if (null? parts)
               (list)
               (let ([start (if (and (> (string-length pattern) 0)
                                     (char=? (string-ref pattern 0) #\/))
                                "/"
                                ".")])
                 (glob-expand-parts parts start (char=? (string-ref pattern 0) #\/) dotglob?
                   nocase? extglob? globskipdots?)))))]
      [(pattern dotglob?)
       (let* ([nocase? #f] [extglob? #f] [globskipdots? #t])
         (let ([parts (split-glob-path pattern)])
           (if (null? parts)
               (list)
               (let ([start (if (and (> (string-length pattern) 0)
                                     (char=? (string-ref pattern 0) #\/))
                                "/"
                                ".")])
                 (glob-expand-parts parts start (char=? (string-ref pattern 0) #\/) dotglob?
                   nocase? extglob? globskipdots?)))))]
      [(pattern dotglob? nocase?)
       (let* ([extglob? #f] [globskipdots? #t])
         (let ([parts (split-glob-path pattern)])
           (if (null? parts)
               (list)
               (let ([start (if (and (> (string-length pattern) 0)
                                     (char=? (string-ref pattern 0) #\/))
                                "/"
                                ".")])
                 (glob-expand-parts parts start (char=? (string-ref pattern 0) #\/) dotglob?
                   nocase? extglob? globskipdots?)))))]
      [(pattern dotglob? nocase? extglob?)
       (let* ([globskipdots? #t])
         (let ([parts (split-glob-path pattern)])
           (if (null? parts)
               (list)
               (let ([start (if (and (> (string-length pattern) 0)
                                     (char=? (string-ref pattern 0) #\/))
                                "/"
                                ".")])
                 (glob-expand-parts parts start (char=? (string-ref pattern 0) #\/) dotglob?
                   nocase? extglob? globskipdots?)))))]
      [(pattern dotglob? nocase? extglob? globskipdots?)
       (let ([parts (split-glob-path pattern)])
         (if (null? parts)
             (list)
             (let ([start (if (and (> (string-length pattern) 0)
                                   (char=? (string-ref pattern 0) #\/))
                              "/"
                              ".")])
               (glob-expand-parts parts start
                 (char=? (string-ref pattern 0) #\/) dotglob? nocase?
                 extglob? globskipdots?))))]))
  (define (split-glob-path path)
    (let loop ([i 0] [start 0] [parts (list)])
      (cond
        [(>= i (string-length path))
         (reverse
           (if (> i start)
               (cons (substring path start i) parts)
               parts))]
        [(char=? (string-ref path i) #\/)
         (loop
           (+ i 1)
           (+ i 1)
           (if (> i start)
               (cons (substring path start i) parts)
               parts))]
        [else (loop (+ i 1) start parts)])))
  (define glob-expand-parts
    (case-lambda
      [(parts base absolute?)
       (let* ([dotglob? #f]
              [nocase? #f]
              [extglob? #f]
              [globskipdots? #t])
         (if (null? parts)
             (list (if absolute? base (if (string=? base ".") "" base)))
             (let* ([pattern (car parts)])
               (let* ([rest (cdr parts)])
                 (let* ([entries (glob-match-dir base pattern dotglob?
                                   nocase? extglob? globskipdots?)])
                   (let loop ([entries entries] [results (list)])
                     (if (null? entries)
                         results
                         (let* ([entry (car entries)])
                           (let* ([full (if (string=? base ".")
                                            entry
                                            (if (string=? base "/")
                                                (string-append "/" entry)
                                                (string-append
                                                  base
                                                  "/"
                                                  entry)))])
                             (if (null? rest)
                                 (loop (cdr entries) (cons full results))
                                 (if (directory-exists? full)
                                     (loop
                                       (cdr entries)
                                       (append
                                         (glob-expand-parts rest full absolute? dotglob?
                                           nocase? extglob? globskipdots?)
                                         results))
                                     (loop
                                       (cdr entries)
                                       results))))))))))))]
      [(parts base absolute? dotglob?)
       (let* ([nocase? #f] [extglob? #f] [globskipdots? #t])
         (if (null? parts)
             (list (if absolute? base (if (string=? base ".") "" base)))
             (let* ([pattern (car parts)])
               (let* ([rest (cdr parts)])
                 (let* ([entries (glob-match-dir base pattern dotglob?
                                   nocase? extglob? globskipdots?)])
                   (let loop ([entries entries] [results (list)])
                     (if (null? entries)
                         results
                         (let* ([entry (car entries)])
                           (let* ([full (if (string=? base ".")
                                            entry
                                            (if (string=? base "/")
                                                (string-append "/" entry)
                                                (string-append
                                                  base
                                                  "/"
                                                  entry)))])
                             (if (null? rest)
                                 (loop (cdr entries) (cons full results))
                                 (if (directory-exists? full)
                                     (loop
                                       (cdr entries)
                                       (append
                                         (glob-expand-parts rest full absolute? dotglob?
                                           nocase? extglob? globskipdots?)
                                         results))
                                     (loop
                                       (cdr entries)
                                       results))))))))))))]
      [(parts base absolute? dotglob? nocase?)
       (let* ([extglob? #f] [globskipdots? #t])
         (if (null? parts)
             (list (if absolute? base (if (string=? base ".") "" base)))
             (let* ([pattern (car parts)])
               (let* ([rest (cdr parts)])
                 (let* ([entries (glob-match-dir base pattern dotglob?
                                   nocase? extglob? globskipdots?)])
                   (let loop ([entries entries] [results (list)])
                     (if (null? entries)
                         results
                         (let* ([entry (car entries)])
                           (let* ([full (if (string=? base ".")
                                            entry
                                            (if (string=? base "/")
                                                (string-append "/" entry)
                                                (string-append
                                                  base
                                                  "/"
                                                  entry)))])
                             (if (null? rest)
                                 (loop (cdr entries) (cons full results))
                                 (if (directory-exists? full)
                                     (loop
                                       (cdr entries)
                                       (append
                                         (glob-expand-parts rest full absolute? dotglob?
                                           nocase? extglob? globskipdots?)
                                         results))
                                     (loop
                                       (cdr entries)
                                       results))))))))))))]
      [(parts base absolute? dotglob? nocase? extglob?)
       (let* ([globskipdots? #t])
         (if (null? parts)
             (list (if absolute? base (if (string=? base ".") "" base)))
             (let* ([pattern (car parts)])
               (let* ([rest (cdr parts)])
                 (let* ([entries (glob-match-dir base pattern dotglob?
                                   nocase? extglob? globskipdots?)])
                   (let loop ([entries entries] [results (list)])
                     (if (null? entries)
                         results
                         (let* ([entry (car entries)])
                           (let* ([full (if (string=? base ".")
                                            entry
                                            (if (string=? base "/")
                                                (string-append "/" entry)
                                                (string-append
                                                  base
                                                  "/"
                                                  entry)))])
                             (if (null? rest)
                                 (loop (cdr entries) (cons full results))
                                 (if (directory-exists? full)
                                     (loop
                                       (cdr entries)
                                       (append
                                         (glob-expand-parts rest full absolute? dotglob?
                                           nocase? extglob? globskipdots?)
                                         results))
                                     (loop
                                       (cdr entries)
                                       results))))))))))))]
      [(parts base absolute? dotglob? nocase? extglob?
        globskipdots?)
       (if (null? parts)
           (list (if absolute? base (if (string=? base ".") "" base)))
           (let* ([pattern (car parts)])
             (let* ([rest (cdr parts)])
               (let* ([entries (glob-match-dir base pattern dotglob?
                                 nocase? extglob? globskipdots?)])
                 (let loop ([entries entries] [results (list)])
                   (if (null? entries)
                       results
                       (let* ([entry (car entries)])
                         (let* ([full (if (string=? base ".")
                                          entry
                                          (if (string=? base "/")
                                              (string-append "/" entry)
                                              (string-append
                                                base
                                                "/"
                                                entry)))])
                           (if (null? rest)
                               (loop (cdr entries) (cons full results))
                               (if (directory-exists? full)
                                   (loop
                                     (cdr entries)
                                     (append
                                       (glob-expand-parts rest full absolute? dotglob?
                                         nocase? extglob? globskipdots?)
                                       results))
                                   (loop
                                     (cdr entries)
                                     results)))))))))))]))
  (define glob-match-dir
    (case-lambda
      [(dir pattern)
       (let* ([dotglob? #f]
              [nocase? #f]
              [extglob? #f]
              [globskipdots? #t])
         (guard (__exn [#t ((lambda (e) (list)) __exn)])
           (let* ([rx (glob-pattern->pregexp
                        (if nocase? (string-downcase pattern) pattern)
                        #t
                        extglob?)])
             (let* ([show-dots? (or dotglob?
                                    (and (> (string-length pattern) 0)
                                         (char=?
                                           (string-ref pattern 0)
                                           #\.)))])
               (let* ([entries (if show-dots?
                                   (directory-files
                                     (list 'path: dir 'ignore-hidden: #f))
                                   (directory-files dir))])
                 (filter
                   (lambda (entry)
                     (and (or (not globskipdots?)
                              (and (not (string=? entry "."))
                                   (not (string=? entry ".."))))
                          (or show-dots?
                              (not (and (> (string-length entry) 0)
                                        (char=?
                                          (string-ref entry 0)
                                          #\.))))
                          (pregexp-match
                            rx
                            (if nocase? (string-downcase entry) entry))))
                   entries))))))]
      [(dir pattern dotglob?)
       (let* ([nocase? #f] [extglob? #f] [globskipdots? #t])
         (guard (__exn [#t ((lambda (e) (list)) __exn)])
           (let* ([rx (glob-pattern->pregexp
                        (if nocase? (string-downcase pattern) pattern)
                        #t
                        extglob?)])
             (let* ([show-dots? (or dotglob?
                                    (and (> (string-length pattern) 0)
                                         (char=?
                                           (string-ref pattern 0)
                                           #\.)))])
               (let* ([entries (if show-dots?
                                   (directory-files
                                     (list 'path: dir 'ignore-hidden: #f))
                                   (directory-files dir))])
                 (filter
                   (lambda (entry)
                     (and (or (not globskipdots?)
                              (and (not (string=? entry "."))
                                   (not (string=? entry ".."))))
                          (or show-dots?
                              (not (and (> (string-length entry) 0)
                                        (char=?
                                          (string-ref entry 0)
                                          #\.))))
                          (pregexp-match
                            rx
                            (if nocase? (string-downcase entry) entry))))
                   entries))))))]
      [(dir pattern dotglob? nocase?)
       (let* ([extglob? #f] [globskipdots? #t])
         (guard (__exn [#t ((lambda (e) (list)) __exn)])
           (let* ([rx (glob-pattern->pregexp
                        (if nocase? (string-downcase pattern) pattern)
                        #t
                        extglob?)])
             (let* ([show-dots? (or dotglob?
                                    (and (> (string-length pattern) 0)
                                         (char=?
                                           (string-ref pattern 0)
                                           #\.)))])
               (let* ([entries (if show-dots?
                                   (directory-files
                                     (list 'path: dir 'ignore-hidden: #f))
                                   (directory-files dir))])
                 (filter
                   (lambda (entry)
                     (and (or (not globskipdots?)
                              (and (not (string=? entry "."))
                                   (not (string=? entry ".."))))
                          (or show-dots?
                              (not (and (> (string-length entry) 0)
                                        (char=?
                                          (string-ref entry 0)
                                          #\.))))
                          (pregexp-match
                            rx
                            (if nocase? (string-downcase entry) entry))))
                   entries))))))]
      [(dir pattern dotglob? nocase? extglob?)
       (let* ([globskipdots? #t])
         (guard (__exn [#t ((lambda (e) (list)) __exn)])
           (let* ([rx (glob-pattern->pregexp
                        (if nocase? (string-downcase pattern) pattern)
                        #t
                        extglob?)])
             (let* ([show-dots? (or dotglob?
                                    (and (> (string-length pattern) 0)
                                         (char=?
                                           (string-ref pattern 0)
                                           #\.)))])
               (let* ([entries (if show-dots?
                                   (directory-files
                                     (list 'path: dir 'ignore-hidden: #f))
                                   (directory-files dir))])
                 (filter
                   (lambda (entry)
                     (and (or (not globskipdots?)
                              (and (not (string=? entry "."))
                                   (not (string=? entry ".."))))
                          (or show-dots?
                              (not (and (> (string-length entry) 0)
                                        (char=?
                                          (string-ref entry 0)
                                          #\.))))
                          (pregexp-match
                            rx
                            (if nocase? (string-downcase entry) entry))))
                   entries))))))]
      [(dir pattern dotglob? nocase? extglob? globskipdots?)
       (guard (__exn [#t ((lambda (e) (list)) __exn)])
         (let* ([rx (glob-pattern->pregexp
                      (if nocase? (string-downcase pattern) pattern)
                      #t
                      extglob?)])
           (let* ([show-dots? (or dotglob?
                                  (and (> (string-length pattern) 0)
                                       (char=?
                                         (string-ref pattern 0)
                                         #\.)))])
             (let* ([entries (if show-dots?
                                 (directory-files
                                   (list 'path: dir 'ignore-hidden: #f))
                                 (directory-files dir))])
               (filter
                 (lambda (entry)
                   (and (or (not globskipdots?)
                            (and (not (string=? entry "."))
                                 (not (string=? entry ".."))))
                        (or show-dots?
                            (not (and (> (string-length entry) 0)
                                      (char=? (string-ref entry 0) #\.))))
                        (pregexp-match
                          rx
                          (if nocase? (string-downcase entry) entry))))
                 entries)))))]))
  (define (directory-exists? path) (file-directory? path))
  (define (glob-ignore-filter paths globignore-str)
    (let ([patterns (string-split-colon globignore-str)])
      (if (null? patterns)
          paths
          (filter
            (lambda (path)
              (let ([base (path-basename path)])
                (not (glob-any?
                       (lambda (pat) (glob-match? pat base #f))
                       patterns))))
            paths))))
  (define (string-split-colon str)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [parts (list)])
        (cond
          [(>= i len)
           (reverse
             (if (> i start)
                 (cons (substring str start i) parts)
                 parts))]
          [(char=? (string-ref str i) #\:)
           (loop
             (+ i 1)
             (+ i 1)
             (if (> i start)
                 (cons (substring str start i) parts)
                 parts))]
          [else (loop (+ i 1) start parts)]))))
  (define (path-basename path)
    (let ([pos (let loop ([i (- (string-length path) 1)])
                 (cond
                   [(< i 0) #f]
                   [(char=? (string-ref path i) #\/) i]
                   [else (loop (- i 1))]))])
      (if pos
          (substring path (+ pos 1) (string-length path))
          path)))
  (define (glob-any? pred lst)
    (and (pair? lst)
         (or (pred (car lst)) (glob-any? pred (cdr lst)))))
  (define (needs-globbing? word) (glob-pattern? word)))
