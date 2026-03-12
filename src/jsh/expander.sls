#!chezscheme
(library (jsh expander)
  (export latin1->utf8 nounset-error! *in-dquote-context*
   *command-sub-ran* make-modifier-segments modifier-segments?
   modifier-segments-list segments->string
   segments-literals-splittable *procsub-cleanups*
   *procsub-counter* make-procsub-fifo! run-procsub-cleanups!
   expand-process-sub expand-word seg-splittable?
   seg-globbable? glob-escape-quoted expand-string-segments
   split-expanded-segments expand-word-nosplit
   expand-word-as-pattern expand-assignment-value
   split-assignment-on-colon expand-heredoc-body expand-words
   expand-string glob-quote expand-pattern expand-tilde-in
   expand-dollar expand-dollar-ansi-c expand-simple-var
   expand-parameter-braced expand-parameter-content
   find-array-subscript-start expand-array-parameter
   special-param-char? find-patsub-separator
   parse-modifier-from-rest parse-parameter-modifier
   pattern-substitute-first pattern-substitute-all take-sublist
   shell-quote-for-at-q get-var-attributes
   get-var-declare-prefix expand-ansi-c-escapes
   slice-arith-eval valid-indirect-ref? unescape-brace
   tilde-expand-default apply-parameter-modifier
   expand-command-sub expand-backtick command-substitute
   expand-arith-sub expand-arith-expr word-split ifs-char?
   ifs-whitespace? word-has-quotes? word-has-quoted-at?
   split-word-at-quoted-at expand-word-parts-split
   expand-word-with-at read-single-quote expand-double-quote
   find-matching-brace find-matching-paren find-arith-close
   case-convert-matching remove-suffix remove-prefix
   string-trim-trailing-newlines read-all-string append-map
   string-find-char-from string-trim-whitespace-str
   brace-expand brace-expand-once find-brace-close
   brace-has-comma? brace-split-commas brace-sequence?
   brace-expand-sequence string-split-dot-dot brace-parse-int
   brace-range pad-number)
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
   (except (jsh pregexp-compat) pregexp-quote pregexp-replace*
     pregexp-replace pregexp-split pregexp-match
     pregexp-match-positions pregexp)
   (jsh ast)
   (except (jsh util) string-index string-join file-directory?
     string-join string-index string-downcase file-regular?
     string-upcase)
   (jsh environment) (jsh ffi) (jsh functions) (jsh glob)
   (jsh arithmetic))
  (define (latin1->utf8 s)
    (let ([len (string-length s)])
      (let check ([i 0])
        (if (>= i len)
            s
            (if (< (char->integer (string-ref s i)) 128)
                (check (+ i 1))
                (guard (__exn [#t ((lambda (_) s) __exn)])
                  (let* ([bytes (list->u8vector
                                  (map char->integer (string->list s)))])
                    (let* ([p (open-input-u8vector
                                (list
                                  'init:
                                  bytes
                                  'char-encoding:
                                  'UTF-8))])
                      (let loop ([chars '()])
                        (let ([ch (read-char p)])
                          (if (eof-object? ch)
                              (list->string (reverse chars))
                              (loop (cons ch chars)))))))))))))
  (define (nounset-error! name env)
    (fprintf
      (current-error-port)
      "jsh: ~a: unbound variable~n"
      name)
    (raise (make-nounset-exception 1)))
  (define *in-dquote-context* (make-parameter #f))
  (define *command-sub-ran* (make-parameter #f))
  (define (make-modifier-segments segs)
    (vector 'modifier-segments segs))
  (define (modifier-segments? x)
    (and (vector? x)
         (>= (vector-length x) 2)
         (eq? (vector-ref x 0) 'modifier-segments)))
  (define (modifier-segments-list x) (vector-ref x 1))
  (define (segments->string segs)
    (apply string-append (map car segs)))
  (define (segments-literals-splittable segs)
    (map (lambda (seg)
           (if (eq? (cdr seg) 'literal)
               (cons (car seg) 'expanded)
               seg))
         segs))
  (define *procsub-cleanups* (make-parameter (list)))
  (define *procsub-counter*-cell (vector 0))
  (define-syntax *procsub-counter*
    (identifier-syntax
      [id (vector-ref *procsub-counter*-cell 0)]
      [(set! id v) (vector-set! *procsub-counter*-cell 0 v)]))
  (define (make-procsub-fifo!)
    (set! *procsub-counter* (+ *procsub-counter* 1))
    (let ([path (string-append
                  "/tmp/jsh-procsub-"
                  (number->string (ffi-getpid))
                  "-"
                  (number->string *procsub-counter*))])
      (let ([rc (ffi-mkfifo path 384)])
        (when (< rc 0) (error 'gerbil "mkfifo failed" path))
        path)))
  (define (run-procsub-cleanups!)
    (for-each (lambda (thunk) (thunk)) (*procsub-cleanups*))
    (*procsub-cleanups* (list)))
  (define (expand-process-sub psub env)
    (let* ([dir (word-process-sub-direction psub)])
      (let* ([cmd-text (word-process-sub-command psub)])
        (let* ([path (make-procsub-fifo!)])
          (let* ([redir (if (eq? dir 'in)
                            (string-append "exec >" path "; ")
                            (string-append "exec <" path "; "))])
            (let* ([p (open-process-with-sigpipe
                        (list 'path: "/bin/sh" 'arguments:
                          (list
                            "-c"
                            (string->c-safe
                              (string-append redir cmd-text)))
                          'environment:
                          (map string->c-safe (env-exported-alist env))
                          'directory: (current-directory)
                          'stdin-redirection: #f 'stdout-redirection: #f
                          'stderr-redirection: #f))])
              (*procsub-cleanups*
                (cons
                  (lambda ()
                    (guard (__exn [#t (void __exn)]) (process-status p))
                    (guard (__exn [#t (void __exn)]) (ffi-unlink path)))
                  (*procsub-cleanups*)))
              path))))))
  (define (expand-word word env)
    (cond
      [(string? word)
       (let ([brace-words (if (env-option? env "braceexpand")
                              (brace-expand word)
                              (list word))])
         (let ([result (append-map
                         (lambda (w)
                           (if (word-has-quoted-at? w)
                               (expand-word-with-at w env)
                               (let* ([segments (expand-string-segments
                                                  w
                                                  env)])
                                 (let* ([all-quoted? (every
                                                       (lambda (seg)
                                                         (eq? (cdr seg)
                                                              'quoted))
                                                       segments)])
                                   (let* ([any-quoted? (any (lambda (seg)
                                                              (eq? (cdr seg)
                                                                   'quoted))
                                                            segments)])
                                     (let* ([split (split-expanded-segments
                                                     segments
                                                     env)])
                                       (let* ([glob-text-for (lambda (display-text)
                                                               (if any-quoted?
                                                                   (let ([gt (apply
                                                                               string-append
                                                                               (map (lambda (seg)
                                                                                      (if (eq? (cdr seg)
                                                                                               'quoted)
                                                                                          (glob-escape-quoted
                                                                                            (car seg))
                                                                                          (car seg)))
                                                                                    segments))])
                                                                     (if (= (length
                                                                              split)
                                                                            1)
                                                                         gt
                                                                         display-text))
                                                                   display-text))])
                                         (let* ([globbed (let ([raw (guard (__exn
                                                                             [#t
                                                                              ((lambda (e)
                                                                                 (if (and (pair?
                                                                                            e)
                                                                                          (eq? (car e)
                                                                                               'failglob))
                                                                                     (begin
                                                                                       (fprintf
                                                                                         (current-error-port)
                                                                                         "jsh: ~a: no match~n"
                                                                                         (cdr e))
                                                                                       (raise
                                                                                         e))
                                                                                     (raise
                                                                                       e)))
                                                                                __exn)])
                                                                      (append-map
                                                                        (lambda (item)
                                                                          (let* ([s (car item)])
                                                                            (let* ([can-glob? (cdr item)])
                                                                              (let* ([gpat (if can-glob?
                                                                                               (glob-text-for
                                                                                                 s)
                                                                                               s)])
                                                                                (if (and can-glob?
                                                                                         (glob-pattern?
                                                                                           gpat
                                                                                           (env-shopt?
                                                                                             env
                                                                                             "extglob"))
                                                                                         (not (env-option?
                                                                                                env
                                                                                                "noglob")))
                                                                                    (let ([matches (glob-expand
                                                                                                     gpat
                                                                                                     (env-shopt?
                                                                                                       env
                                                                                                       "dotglob")
                                                                                                     (env-shopt?
                                                                                                       env
                                                                                                       "nullglob")
                                                                                                     (env-shopt?
                                                                                                       env
                                                                                                       "failglob")
                                                                                                     (env-shopt?
                                                                                                       env
                                                                                                       "nocaseglob")
                                                                                                     (env-shopt?
                                                                                                       env
                                                                                                       "extglob")
                                                                                                     (env-shopt?
                                                                                                       env
                                                                                                       "globskipdots"))])
                                                                                      (if (and (= (length
                                                                                                    matches)
                                                                                                  1)
                                                                                               (string=?
                                                                                                 (car matches)
                                                                                                 gpat)
                                                                                               (not (string=?
                                                                                                      gpat
                                                                                                      s)))
                                                                                          (list
                                                                                            s)
                                                                                          matches))
                                                                                    (list
                                                                                      s))))))
                                                                        split))])
                                                           (let ([gi (env-get
                                                                       env
                                                                       "GLOBIGNORE")])
                                                             (if (and gi
                                                                      (not (string=?
                                                                             gi
                                                                             "")))
                                                                 (glob-ignore-filter
                                                                   raw
                                                                   gi)
                                                                 raw)))])
                                           (cond
                                             [(null? globbed)
                                              (if any-quoted?
                                                  (list "")
                                                  (list))]
                                             [(and (not any-quoted?)
                                                   (= (length globbed) 1)
                                                   (string=?
                                                     (car globbed)
                                                     "")
                                                   (every
                                                     (lambda (seg)
                                                       (string=?
                                                         (car seg)
                                                         ""))
                                                     segments))
                                              (list)]
                                             [else globbed])))))))))
                         brace-words)])
           result))]
      [(word-process-sub? word)
       (list (expand-process-sub word env))]
      [else (list word)]))
  (define (seg-splittable? type) (eq? type 'expanded))
  (define (seg-globbable? type) (not (eq? type 'quoted)))
  (define (glob-escape-quoted text)
    (let ([len (string-length text)] [buf (open-output-string)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string buf)
            (let ([ch (string-ref text i)])
              (when (memq ch '(#\* #\? #\[ #\] #\-)) (display #\\ buf))
              (display ch buf)
              (loop (+ i 1)))))))
  (define (expand-string-segments word env)
    (let ([len (string-length word)])
      (let loop ([i 0] [segments (list)])
        (if (>= i len)
            (reverse segments)
            (let ([ch (string-ref word i)])
              (cond
                [(and (= i 0) (char=? ch #\~))
                 (let-values ([(expanded end)
                               (expand-tilde-in word i env)])
                   (loop end (cons (cons expanded 'literal) segments)))]
                [(char=? ch #\$)
                 (if (and (< (+ i 1) len)
                          (let ([next (string-ref word (+ i 1))])
                            (or (char=? next #\@) (char=? next #\*))))
                     (let ([params (env-positional-list env)])
                       (if (null? params)
                           (loop (+ i 2) segments)
                           (let param-loop ([rest params]
                                            [segs segments]
                                            [first? #t])
                             (if (null? rest)
                                 (loop (+ i 2) segs)
                                 (let ([new-segs (if first?
                                                     (cons
                                                       (cons
                                                         (car rest)
                                                         'expanded)
                                                       segs)
                                                     (cons
                                                       (cons
                                                         (car rest)
                                                         'expanded)
                                                       (cons
                                                         (cons
                                                           ""
                                                           'word-break)
                                                         segs)))])
                                   (param-loop (cdr rest) new-segs #f))))))
                     (let-values ([(expanded end)
                                   (expand-dollar word i env)])
                       (if (modifier-segments? expanded)
                           (loop
                             end
                             (append
                               (reverse (modifier-segments-list expanded))
                               segments))
                           (loop
                             end
                             (cons (cons expanded 'expanded) segments)))))]
                [(char=? ch #\`)
                 (let-values ([(expanded end)
                               (expand-backtick word i env)])
                   (loop end (cons (cons expanded 'expanded) segments)))]
                [(char=? ch #\\)
                 (if (< (+ i 1) len)
                     (loop
                       (+ i 2)
                       (cons
                         (cons (string (string-ref word (+ i 1))) 'quoted)
                         segments))
                     (loop (+ i 1) (cons (cons "\\" 'literal) segments)))]
                [(char=? ch #\')
                 (let-values ([(content end)
                               (read-single-quote word (+ i 1))])
                   (loop end (cons (cons content 'quoted) segments)))]
                [(char=? ch #\")
                 (let-values ([(content end)
                               (expand-double-quote word (+ i 1) env)])
                   (loop end (cons (cons content 'quoted) segments)))]
                [else
                 (let lit-loop ([j (+ i 1)])
                   (if (and (< j len)
                            (let ([c (string-ref word j)])
                              (not (or (char=? c #\$)
                                       (char=? c #\`)
                                       (char=? c #\\)
                                       (char=? c #\')
                                       (char=? c #\")
                                       (char=? c #\~)))))
                       (lit-loop (+ j 1))
                       (loop
                         j
                         (cons
                           (cons (substring word i j) 'literal)
                           segments))))]))))))
  (define (split-expanded-segments segments env)
    (if (null? segments)
        (list (cons "" #f))
        (let ([ifs (or (env-get env "IFS") " \t\n")])
          (if (string=? ifs "")
              (let ([has-breaks? (any (lambda (seg)
                                        (eq? (cdr seg) 'word-break))
                                      segments)])
                (if (not has-breaks?)
                    (let ([text (apply string-append (map car segments))]
                          [can-glob? (any (lambda (seg)
                                            (seg-globbable? (cdr seg)))
                                          segments)])
                      (list (cons text can-glob?)))
                    (let group ([segs segments]
                                [cur-parts (list)]
                                [result (list)])
                      (cond
                        [(null? segs)
                         (if (null? cur-parts)
                             (reverse result)
                             (let* ([parts (reverse cur-parts)])
                               (let* ([can-glob? (any (lambda (seg)
                                                        (seg-globbable?
                                                          (cdr seg)))
                                                      parts)])
                                 (let* ([text (apply
                                                string-append
                                                (map car parts))])
                                   (if (string=? text "")
                                       (reverse result)
                                       (reverse
                                         (cons
                                           (cons text can-glob?)
                                           result)))))))]
                        [(eq? (cdar segs) 'word-break)
                         (if (null? cur-parts)
                             (group (cdr segs) (list) result)
                             (let* ([parts (reverse cur-parts)])
                               (let* ([can-glob? (any (lambda (seg)
                                                        (seg-globbable?
                                                          (cdr seg)))
                                                      parts)])
                                 (let* ([text (apply
                                                string-append
                                                (map car parts))])
                                   (if (string=? text "")
                                       (group (cdr segs) (list) result)
                                       (group
                                         (cdr segs)
                                         (list)
                                         (cons
                                           (cons text can-glob?)
                                           result)))))))]
                        [else
                         (group
                           (cdr segs)
                           (cons (car segs) cur-parts)
                           result)]))))
              (let ([words (list)]
                    [current (open-output-string)]
                    [cur-can-glob? #f])
                (let seg-loop ([segs segments]
                               [words (list)]
                               [current (open-output-string)]
                               [cur-can-glob? #f]
                               [word-started? #f])
                  (if (null? segs)
                      (let ([final (get-output-string current)])
                        (if (or word-started? (> (string-length final) 0))
                            (reverse
                              (cons (cons final cur-can-glob?) words))
                            (reverse words)))
                      (let* ([seg (car segs)])
                        (let* ([text (car seg)])
                          (let* ([type (cdr seg)])
                            (cond
                              [(eq? type 'word-break)
                               (let ([w (get-output-string current)])
                                 (if (> (string-length w) 0)
                                     (seg-loop (cdr segs)
                                       (cons (cons w cur-can-glob?) words)
                                       (open-output-string) #f #f)
                                     (seg-loop (cdr segs) words
                                       (open-output-string) #f #f)))]
                              [(not (seg-splittable? type))
                               (display text current)
                               (seg-loop (cdr segs) words current
                                 (or cur-can-glob? (seg-globbable? type))
                                 (or word-started? #t))]
                              [else
                               (let* ([ifs (or (env-get env "IFS")
                                               " \t\n")])
                                 (let* ([trailing-ifs-delim? (and (> (string-length
                                                                       text)
                                                                     0)
                                                                  (pair?
                                                                    (cdr segs))
                                                                  (not (string=?
                                                                         ifs
                                                                         ""))
                                                                  (ifs-char?
                                                                    (string-ref
                                                                      text
                                                                      (- (string-length
                                                                           text)
                                                                         1))
                                                                    ifs))])
                                   (let* ([leading-ifs-delim? (and (> (string-length
                                                                        text)
                                                                      0)
                                                                   word-started?
                                                                   (not (string=?
                                                                          ifs
                                                                          ""))
                                                                   (ifs-char?
                                                                     (string-ref
                                                                       text
                                                                       0)
                                                                     ifs))])
                                     (let* ([raw-split (word-split
                                                         text
                                                         env)])
                                       (let* ([split-words (if (and trailing-ifs-delim?
                                                                    (pair?
                                                                      raw-split))
                                                               (append
                                                                 raw-split
                                                                 (list ""))
                                                               raw-split)])
                                         (let* ([split-words (if (and leading-ifs-delim?
                                                                      (pair?
                                                                        split-words)
                                                                      (not (string=?
                                                                             (car split-words)
                                                                             "")))
                                                                 (cons
                                                                   ""
                                                                   split-words)
                                                                 split-words)])
                                           (let* ([n (length split-words)])
                                             (cond
                                               [(= n 0)
                                                (if (and word-started?
                                                         (pair? (cdr segs))
                                                         (> (string-length
                                                              text)
                                                            0))
                                                    (let ([w (get-output-string
                                                               current)])
                                                      (seg-loop (cdr segs)
                                                        (cons
                                                          (cons w #t)
                                                          words)
                                                        (open-output-string)
                                                        #f #f))
                                                    (seg-loop (cdr segs) words
                                                      current
                                                      (or cur-can-glob? #t)
                                                      word-started?))]
                                               [(= n 1)
                                                (display
                                                  (car split-words)
                                                  current)
                                                (seg-loop (cdr segs) words current
                                                  (or cur-can-glob? #t)
                                                  (or word-started?
                                                      (> (string-length
                                                           (car split-words))
                                                         0)
                                                      (> (string-length
                                                           text)
                                                         0)))]
                                               [else
                                                (display
                                                  (car split-words)
                                                  current)
                                                (let ([w (get-output-string
                                                           current)])
                                                  (let inner-loop ([rest (cdr split-words)]
                                                                   [words (cons
                                                                            (cons
                                                                              w
                                                                              #t)
                                                                            words)])
                                                    (if (null? (cdr rest))
                                                        (let ([new-current (open-output-string)])
                                                          (display
                                                            (car rest)
                                                            new-current)
                                                          (seg-loop (cdr segs)
                                                            words
                                                            new-current #t
                                                            #t))
                                                        (inner-loop
                                                          (cdr rest)
                                                          (cons
                                                            (cons
                                                              (car rest)
                                                              #t)
                                                            words)))))]))))))))])))))))))))
  (define (expand-word-nosplit word env)
    (if (string? word) (expand-string word env) word))
  (define (expand-word-as-pattern word env)
    (if (string? word) (expand-pattern word env) word))
  (define (expand-assignment-value word env)
    (if (string? word)
        (let ([segments (split-assignment-on-colon word)])
          (let ([strs (map (lambda (seg) (expand-string seg env))
                           segments)]
                [sep ":"])
            (if (null? strs)
                ""
                (let lp ([result (car strs)] [rest (cdr strs)])
                  (if (null? rest)
                      result
                      (lp (string-append result sep (car rest))
                          (cdr rest)))))))
        word))
  (define (split-assignment-on-colon str)
    (let ([len (string-length str)]
          [out (open-output-string)]
          [result (list)])
      (let loop ([i 0] [in-sq? #f] [in-dq? #f] [brace-depth 0])
        (if (>= i len)
            (reverse (cons (get-output-string out) result))
            (let ([ch (string-ref str i)])
              (cond
                [(and (char=? ch #\') (not in-dq?))
                 (display ch out)
                 (if in-sq?
                     (loop (+ i 1) #f in-dq? brace-depth)
                     (loop (+ i 1) #t in-dq? brace-depth))]
                [in-sq?
                 (display ch out)
                 (loop (+ i 1) in-sq? in-dq? brace-depth)]
                [(char=? ch #\")
                 (display ch out)
                 (loop (+ i 1) in-sq? (not in-dq?) brace-depth)]
                [(and (char=? ch #\\) (< (+ i 1) len))
                 (display ch out)
                 (display (string-ref str (+ i 1)) out)
                 (loop (+ i 2) in-sq? in-dq? brace-depth)]
                [(and (char=? ch #\$)
                      (< (+ i 1) len)
                      (char=? (string-ref str (+ i 1)) #\{))
                 (display ch out)
                 (display (string-ref str (+ i 1)) out)
                 (loop (+ i 2) in-sq? in-dq? (+ brace-depth 1))]
                [(and (char=? ch #\}) (> brace-depth 0))
                 (display ch out)
                 (loop (+ i 1) in-sq? in-dq? (- brace-depth 1))]
                [(and (char=? ch #\:) (not in-dq?) (= brace-depth 0))
                 (set! result (cons (get-output-string out) result))
                 (set! out (open-output-string))
                 (loop (+ i 1) in-sq? in-dq? brace-depth)]
                [else
                 (display ch out)
                 (loop (+ i 1) in-sq? in-dq? brace-depth)]))))))
  (define (expand-heredoc-body body env)
    (let ([len (string-length body)] [out (open-output-string)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let ([ch (string-ref body i)])
              (cond
                [(char=? ch #\$)
                 (let-values ([(expanded end) (expand-dollar body i env)])
                   (display
                     (if (modifier-segments? expanded)
                         (segments->string
                           (modifier-segments-list expanded))
                         expanded)
                     out)
                   (loop end))]
                [(char=? ch #\`)
                 (let-values ([(expanded end)
                               (expand-backtick body i env)])
                   (display expanded out)
                   (loop end))]
                [(char=? ch #\\)
                 (if (< (+ i 1) len)
                     (let ([next (string-ref body (+ i 1))])
                       (if (memv next '(#\$ #\` #\\ #\newline))
                           (begin (display next out) (loop (+ i 2)))
                           (begin
                             (display #\\ out)
                             (display next out)
                             (loop (+ i 2)))))
                     (begin (display #\\ out) (loop (+ i 1))))]
                [else (display ch out) (loop (+ i 1))]))))))
  (define (expand-words words env)
    (append-map (lambda (w) (expand-word w env)) words))
  (define (expand-string str env)
    (let ([len (string-length str)] [out (open-output-string)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let ([ch (string-ref str i)])
              (cond
                [(and (= i 0) (char=? ch #\~) (not (*in-dquote-context*)))
                 (let-values ([(expanded end) (expand-tilde-in str i env)])
                   (display expanded out)
                   (loop end))]
                [(char=? ch #\$)
                 (let-values ([(expanded end) (expand-dollar str i env)])
                   (display
                     (if (modifier-segments? expanded)
                         (segments->string
                           (modifier-segments-list expanded))
                         expanded)
                     out)
                   (loop end))]
                [(char=? ch #\`)
                 (let-values ([(expanded end) (expand-backtick str i env)])
                   (display expanded out)
                   (loop end))]
                [(char=? ch #\\)
                 (if (< (+ i 1) len)
                     (let ([next (string-ref str (+ i 1))])
                       (if (and (*in-dquote-context*)
                                (not (memq
                                       next
                                       '(#\$ #\` #\" #\\ #\newline))))
                           (begin
                             (display "\\" out)
                             (display next out)
                             (loop (+ i 2)))
                           (begin (display next out) (loop (+ i 2)))))
                     (begin (display "\\" out) (loop (+ i 1))))]
                [(char=? ch #\')
                 (if (*in-dquote-context*)
                     (begin (display ch out) (loop (+ i 1)))
                     (let-values ([(content end)
                                   (read-single-quote str (+ i 1))])
                       (display content out)
                       (loop end)))]
                [(char=? ch #\")
                 (let-values ([(content end)
                               (expand-double-quote str (+ i 1) env)])
                   (display content out)
                   (loop end))]
                [else (display ch out) (loop (+ i 1))]))))))
  (define (glob-quote str)
    (let ([len (string-length str)] [out (open-output-string)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let ([ch (string-ref str i)])
              (when (memq ch '(#\* #\? #\[ #\])) (display "\\" out))
              (display ch out)
              (loop (+ i 1)))))))
  (define (expand-pattern str env)
    (let ([len (string-length str)] [out (open-output-string)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let ([ch (string-ref str i)])
              (cond
                [(and (= i 0) (char=? ch #\~))
                 (let-values ([(expanded end) (expand-tilde-in str i env)])
                   (display (glob-quote expanded) out)
                   (loop end))]
                [(char=? ch #\$)
                 (let-values ([(expanded end) (expand-dollar str i env)])
                   (display
                     (if (modifier-segments? expanded)
                         (segments->string
                           (modifier-segments-list expanded))
                         expanded)
                     out)
                   (loop end))]
                [(char=? ch #\`)
                 (let-values ([(expanded end) (expand-backtick str i env)])
                   (display (glob-quote expanded) out)
                   (loop end))]
                [(char=? ch #\\)
                 (if (< (+ i 1) len)
                     (let ([next (string-ref str (+ i 1))])
                       (display "\\" out)
                       (display next out)
                       (loop (+ i 2)))
                     (begin (display "\\" out) (loop (+ i 1))))]
                [(char=? ch #\')
                 (let-values ([(content end)
                               (read-single-quote str (+ i 1))])
                   (display (glob-quote content) out)
                   (loop end))]
                [(char=? ch #\")
                 (let-values ([(content end)
                               (expand-double-quote str (+ i 1) env)])
                   (display (glob-quote content) out)
                   (loop end))]
                [else (display ch out) (loop (+ i 1))]))))))
  (define (expand-tilde-in str i env)
    (let* ([len (string-length str)])
      (let* ([end (let loop ([j (+ i 1)])
                    (if (or (>= j len) (char=? (string-ref str j) #\/))
                        j
                        (loop (+ j 1))))])
        (let* ([prefix (substring str (+ i 1) end)])
          (cond
            [(= (string-length prefix) 0)
             (values (or (env-get env "HOME") "~") end)]
            [(string=? prefix "+")
             (values (or (env-get env "PWD") "+") end)]
            [(string=? prefix "-")
             (values (or (env-get env "OLDPWD") "-") end)]
            [else
             (guard (__exn
                      [#t
                       ((lambda (e) (values (substring str i end) end))
                         __exn)])
               (values (user-info-home (user-info prefix)) end))])))))
  (define (expand-dollar str i env)
    (let ([len (string-length str)])
      (if (>= (+ i 1) len)
          (values "$" (+ i 1))
          (let ([next (string-ref str (+ i 1))])
            (cond
              [(and (char=? next #\()
                    (< (+ i 2) len)
                    (char=? (string-ref str (+ i 2)) #\())
               (expand-arith-sub str i env)]
              [(char=? next #\() (expand-command-sub str i env)]
              [(char=? next #\{) (expand-parameter-braced str i env)]
              [(or (char-alphabetic? next) (char=? next #\_))
               (expand-simple-var str i env)]
              [(memq
                 next
                 '(#\? #\$ #\! #\# #\* #\@ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6
                       #\7 #\8 #\9))
               (let ([val (env-get env (string next))])
                 (values (or val "") (+ i 2)))]
              [(char=? next #\') (expand-dollar-ansi-c str (+ i 1) env)]
              [else (values "$" (+ i 1))])))))
  (define (expand-dollar-ansi-c str i env)
    (let ([len (string-length str)] [buf (open-output-string)])
      (let loop ([j (+ i 1)])
        (if (>= j len)
            (values "$" i)
            (let ([ch (string-ref str j)])
              (cond
                [(char=? ch #\') (values (get-output-string buf) (+ j 1))]
                [(char=? ch #\\)
                 (if (>= (+ j 1) len)
                     (begin (display ch buf) (loop (+ j 1)))
                     (let ([esc (string-ref str (+ j 1))])
                       (case esc
                         [(#\n) (display #\newline buf) (loop (+ j 2))]
                         [(#\t) (display #\tab buf) (loop (+ j 2))]
                         [(#\r) (display #\return buf) (loop (+ j 2))]
                         [(#\a)
                          (display (integer->char 7) buf)
                          (loop (+ j 2))]
                         [(#\b)
                          (display (integer->char 8) buf)
                          (loop (+ j 2))]
                         [(#\e #\E)
                          (display (integer->char 27) buf)
                          (loop (+ j 2))]
                         [(#\f)
                          (display (integer->char 12) buf)
                          (loop (+ j 2))]
                         [(#\v)
                          (display (integer->char 11) buf)
                          (loop (+ j 2))]
                         [(#\\) (display #\\ buf) (loop (+ j 2))]
                         [(#\') (display #\' buf) (loop (+ j 2))]
                         [(#\") (display #\" buf) (loop (+ j 2))]
                         [(#\0)
                          (let oloop ([k (+ j 2)] [val 0] [count 0])
                            (if (and (< k len)
                                     (< count 3)
                                     (let ([c (string-ref str k)])
                                       (and (char>=? c #\0)
                                            (char<=? c #\7))))
                                (oloop
                                  (+ k 1)
                                  (+ (* val 8)
                                     (- (char->integer (string-ref str k))
                                        48))
                                  (+ count 1))
                                (begin
                                  (display (integer->char val) buf)
                                  (loop k))))]
                         [(#\x)
                          (let hloop ([k (+ j 2)] [val 0] [count 0])
                            (if (and (< k len)
                                     (< count 2)
                                     (let ([c (string-ref str k)])
                                       (or (and (char>=? c #\0)
                                                (char<=? c #\9))
                                           (and (char>=? c #\a)
                                                (char<=? c #\f))
                                           (and (char>=? c #\A)
                                                (char<=? c #\F)))))
                                (let ([c (string-ref str k)])
                                  (hloop
                                    (+ k 1)
                                    (+ (* val 16)
                                       (cond
                                         [(char>=? c #\a)
                                          (+ 10 (- (char->integer c) 97))]
                                         [(char>=? c #\A)
                                          (+ 10 (- (char->integer c) 65))]
                                         [else (- (char->integer c) 48)]))
                                    (+ count 1)))
                                (begin
                                  (when (> count 0)
                                    (display (integer->char val) buf))
                                  (loop k))))]
                         [else (display esc buf) (loop (+ j 2))])))]
                [else (display ch buf) (loop (+ j 1))]))))))
  (define (expand-simple-var str i env)
    (let* ([len (string-length str)])
      (let* ([end (let loop ([j (+ i 1)])
                    (if (and (< j len)
                             (let ([ch (string-ref str j)])
                               (or (char-alphabetic? ch)
                                   (char-numeric? ch)
                                   (char=? ch #\_))))
                        (loop (+ j 1))
                        j))])
        (let* ([name (substring str (+ i 1) end)])
          (let* ([val (env-get env name)])
            (when (and (not val) (env-option? env "nounset"))
              (nounset-error! name env))
            (values (or val "") end))))))
  (define (expand-parameter-braced str i env)
    (let* ([len (string-length str)])
      (let* ([close (find-matching-brace str (+ i 2))])
        (let* ([content (if close
                            (substring str (+ i 2) close)
                            (error 'gerbil
                              "bad substitution: unclosed ${"))])
          (let ([result (expand-parameter-content content env)])
            (values result (+ close 1)))))))
  (define (expand-parameter-content content env)
    (let ([len (string-length content)])
      (cond
        [(and (> len 1) (char=? (string-ref content 0) #\#))
         (let* ([rest (substring content 1 len)])
           (let* ([bracket-pos (string-find-char-from rest #\[ 0)])
             (let* ([close-pos (and bracket-pos
                                    (string-find-char-from
                                      rest
                                      #\]
                                      (+ bracket-pos 1)))])
               (when (and close-pos
                          (< (+ close-pos 1) (string-length rest)))
                 (error 'gerbil (format "bad substitution")))
               (if (and bracket-pos
                        close-pos
                        (let ([subscript (substring
                                           rest
                                           (+ bracket-pos 1)
                                           close-pos)])
                          (or (string=? subscript "@")
                              (string=? subscript "*"))))
                   (let ([name (substring rest 0 bracket-pos)])
                     (number->string (env-array-length env name)))
                   (if bracket-pos
                       (let* ([name (substring rest 0 bracket-pos)])
                         (let* ([idx (substring
                                       rest
                                       (+ bracket-pos 1)
                                       (or close-pos
                                           (string-length rest)))])
                           (let* ([val (env-array-get
                                         env
                                         name
                                         (expand-word-nosplit idx env))])
                             (number->string (string-length val)))))
                       (begin
                         (when (let check ([j 0])
                                 (and (< j (string-length rest))
                                      (let ([ch (string-ref rest j)])
                                        (if (or (char-alphabetic? ch)
                                                (char-numeric? ch)
                                                (char=? ch #\_))
                                            (check (+ j 1))
                                            #t))))
                           (error 'gerbil
                             (format "bad substitution: ${#~a}" rest)))
                         (let* ([val (env-get env rest)])
                           (let* ([val (or val
                                           (begin
                                             (when (env-option?
                                                     env
                                                     "nounset")
                                               (nounset-error! rest env))
                                             ""))])
                             (let* ([lc-all (env-get env "LC_ALL")])
                               (let* ([byte-locale? (and lc-all
                                                         (or (string=?
                                                               lc-all
                                                               "C")
                                                             (string=?
                                                               lc-all
                                                               "POSIX")))])
                                 (number->string
                                   (if byte-locale?
                                       (utf8-byte-count val)
                                       (utf8-string-length
                                         val)))))))))))))]
        [(and (> len 1)
              (or (char=? (string-ref content 0) #\!)
                  (and (> len 2)
                       (char=? (string-ref content 0) #\\)
                       (char=? (string-ref content 1) #\!))))
         (let* ([name-start (if (char=? (string-ref content 0) #\!)
                                1
                                2)])
           (let* ([rest (substring content name-start len)])
             (let* ([bracket-pos (string-find-char-from rest #\[ 0)])
               (if (and bracket-pos
                        (let ([close (string-find-char-from
                                       rest
                                       #\]
                                       (+ bracket-pos 1))])
                          (and close
                               (let ([subscript (substring
                                                  rest
                                                  (+ bracket-pos 1)
                                                  close)])
                                 (or (string=? subscript "@")
                                     (string=? subscript "*"))))))
                   (let* ([close (string-find-char-from
                                   rest
                                   #\]
                                   (+ bracket-pos 1))])
                     (let* ([name (substring rest 0 bracket-pos)])
                       (let* ([after-bracket (substring
                                               rest
                                               (+ close 1)
                                               (string-length rest))])
                         (let* ([has-at-op? (and (> (string-length
                                                      after-bracket)
                                                    0)
                                                 (char=?
                                                   (string-ref
                                                     after-bracket
                                                     0)
                                                   #\@))])
                           (let* ([at-op-char (and has-at-op?
                                                   (> (string-length
                                                        after-bracket)
                                                      1)
                                                   (string-ref
                                                     after-bracket
                                                     1))])
                             (let* ([keys (env-array-keys env name)])
                               (let ([transformed-keys (if (and has-at-op?
                                                                at-op-char
                                                                (char=?
                                                                  at-op-char
                                                                  #\a))
                                                           (map (lambda (k)
                                                                  (let ([k-str (if (string?
                                                                                     k)
                                                                                   k
                                                                                   (number->string
                                                                                     k))])
                                                                    (get-var-attributes
                                                                      k-str
                                                                      env)))
                                                                keys)
                                                           keys)])
                                 (if (null? transformed-keys)
                                     ""
                                     (make-modifier-segments
                                       (let eloop ([rest transformed-keys]
                                                   [segs (list)]
                                                   [first? #t])
                                         (if (null? rest)
                                             (reverse segs)
                                             (let ([new-segs (if first?
                                                                 (cons
                                                                   (cons
                                                                     (car rest)
                                                                     'expanded)
                                                                   segs)
                                                                 (cons
                                                                   (cons
                                                                     (car rest)
                                                                     'expanded)
                                                                   (cons
                                                                     (cons
                                                                       ""
                                                                       'word-break)
                                                                     segs)))])
                                               (eloop
                                                 (cdr rest)
                                                 new-segs
                                                 #f)))))))))))))
                   (let* ([rlen (string-length rest)])
                     (let* ([last-ch (and (> rlen 0)
                                          (string-ref rest (- rlen 1)))])
                       (if (and last-ch
                                (or (char=? last-ch #\@)
                                    (char=? last-ch #\*)))
                           (let* ([prefix (substring rest 0 (- rlen 1))])
                             (let* ([names (env-matching-names
                                             env
                                             prefix)])
                               (if (null? names)
                                   ""
                                   (if (char=? last-ch #\@)
                                       (make-modifier-segments
                                         (let eloop ([rest names]
                                                     [segs (list)]
                                                     [first? #t])
                                           (if (null? rest)
                                               (reverse segs)
                                               (let ([new-segs (if first?
                                                                   (cons
                                                                     (cons
                                                                       (car rest)
                                                                       'expanded)
                                                                     segs)
                                                                   (cons
                                                                     (cons
                                                                       (car rest)
                                                                       'expanded)
                                                                     (cons
                                                                       (cons
                                                                         ""
                                                                         'word-break)
                                                                       segs)))])
                                                 (eloop
                                                   (cdr rest)
                                                   new-segs
                                                   #f)))))
                                       (let* ([ifs (or (env-get env "IFS")
                                                       " \t\n")])
                                         (let* ([sep (if (> (string-length
                                                              ifs)
                                                            0)
                                                         (string
                                                           (string-ref
                                                             ifs
                                                             0))
                                                         "")])
                                           (string-join-with
                                             sep
                                             names)))))))
                           (let-values ([(iname modifier arg)
                                         (parse-parameter-modifier rest)])
                             (let* ([ref-name (env-get env iname)])
                               (let* ([_ (when (not ref-name)
                                           (fprintf
                                             (current-error-port)
                                             "jsh: ~a: invalid indirect expansion~n"
                                             iname)
                                           (raise
                                             (make-nounset-exception 1)))])
                                 (let* ([_ (when (and ref-name
                                                      (not (valid-indirect-ref?
                                                             ref-name)))
                                             (error 'gerbil
                                               (format
                                                 "bad substitution: ${!~a}"
                                                 iname)))])
                                   (let* ([bracket-pos (and ref-name
                                                            (find-array-subscript-start
                                                              ref-name))])
                                     (let* ([val (cond
                                                   [(not ref-name) #f]
                                                   [(and bracket-pos
                                                         (eq? modifier
                                                              'at-op)
                                                         (string=?
                                                           arg
                                                           "a"))
                                                    (let ([base-name (substring
                                                                       ref-name
                                                                       0
                                                                       bracket-pos)])
                                                      (get-var-attributes
                                                        base-name
                                                        env))]
                                                   [bracket-pos
                                                    (let* ([indirect-content (if modifier
                                                                                 (string-append
                                                                                   ref-name
                                                                                   (case modifier
                                                                                     [(-)
                                                                                      (string-append
                                                                                        "-"
                                                                                        arg)]
                                                                                     [(:-)
                                                                                      (string-append
                                                                                        ":-"
                                                                                        arg)]
                                                                                     [(+)
                                                                                      (string-append
                                                                                        "+"
                                                                                        arg)]
                                                                                     [(:+)
                                                                                      (string-append
                                                                                        ":+"
                                                                                        arg)]
                                                                                     [(=)
                                                                                      (string-append
                                                                                        "="
                                                                                        arg)]
                                                                                     [(:=)
                                                                                      (string-append
                                                                                        ":="
                                                                                        arg)]
                                                                                     [(?)
                                                                                      (string-append
                                                                                        "?"
                                                                                        arg)]
                                                                                     [(:?)
                                                                                      (string-append
                                                                                        ":?"
                                                                                        arg)]
                                                                                     [else
                                                                                      ""]))
                                                                                 ref-name)])
                                                      (let* ([bp (find-array-subscript-start
                                                                   indirect-content)])
                                                        (expand-array-parameter
                                                          indirect-content
                                                          bp
                                                          env)))]
                                                   [else
                                                    (env-get
                                                      env
                                                      ref-name)])])
                                       (cond
                                         [(and bracket-pos
                                               (eq? modifier 'at-op)
                                               (string=? arg "a"))
                                          val]
                                         [bracket-pos val]
                                         [modifier
                                          (apply-parameter-modifier val (or ref-name iname)
                                            modifier arg env)]
                                         [else (or val "")]))))))))))))))]
        [else
         (let ([bracket-pos (find-array-subscript-start content)])
           (if bracket-pos
               (expand-array-parameter content bracket-pos env)
               (let-values ([(name modifier arg)
                             (parse-parameter-modifier content)])
                 (let ([val (env-get env name)])
                   (when (and (not val)
                              (env-option? env "nounset")
                              (not (memq modifier '(:- - := = :+ + :? ?))))
                     (nounset-error! name env))
                   (if (and (or (string=? name "@") (string=? name "*"))
                            modifier
                            (memq
                              modifier
                              '(% %% prefix-short prefix-long ^^ ^ lc-all
                                  lc-first / // at-op)))
                       (let* ([params (env-positional-list env)])
                         (let* ([transformed (map (lambda (p)
                                                    (apply-parameter-modifier p name modifier arg
                                                      env))
                                                  params)])
                           (string-join-with " " transformed)))
                       (apply-parameter-modifier val name modifier arg
                         env))))))])))
  (define (find-array-subscript-start content)
    (let ([len (string-length content)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref content i) #\[)
           (if (and (> i 0)
                    (let loop2 ([j 0])
                      (or (= j i)
                          (and (let ([ch (string-ref content j)])
                                 (or (char-alphabetic? ch)
                                     (char-numeric? ch)
                                     (char=? ch #\_)))
                               (loop2 (+ j 1))))))
               i
               #f)]
          [else (loop (+ i 1))]))))
  (define (expand-array-parameter content bracket-pos env)
    (let* ([name (substring content 0 bracket-pos)])
      (let* ([len (string-length content)])
        (let* ([close (string-find-char-from
                        content
                        #\]
                        (+ bracket-pos 1))])
          (if (not close)
              (let-values ([(pname modifier arg)
                            (parse-parameter-modifier content)])
                (let ([val (env-get env pname)])
                  (apply-parameter-modifier val pname modifier arg env)))
              (let* ([subscript (substring
                                  content
                                  (+ bracket-pos 1)
                                  close)])
                (let* ([after-bracket (substring content (+ close 1) len)])
                  (cond
                    [(and (string=? subscript "@")
                          (string=? after-bracket ""))
                     (let ([vals (env-array-values env name)])
                       (if (null? vals)
                           ""
                           (make-modifier-segments
                             (let eloop ([rest vals]
                                         [segs (list)]
                                         [first? #t])
                               (if (null? rest)
                                   (reverse segs)
                                   (let ([new-segs (if first?
                                                       (cons
                                                         (cons
                                                           (car rest)
                                                           'expanded)
                                                         segs)
                                                       (cons
                                                         (cons
                                                           (car rest)
                                                           'expanded)
                                                         (cons
                                                           (cons
                                                             ""
                                                             'word-break)
                                                           segs)))])
                                     (eloop (cdr rest) new-segs #f)))))))]
                    [(and (string=? subscript "*")
                          (string=? after-bracket ""))
                     (let* ([vals (env-array-values env name)])
                       (let* ([ifs (or (env-get env "IFS") " \t\n")])
                         (let* ([sep (if (> (string-length ifs) 0)
                                         (string (string-ref ifs 0))
                                         "")])
                           (if (and (string=? sep "") (not (null? vals)))
                               (make-modifier-segments
                                 (let eloop ([rest vals]
                                             [segs (list)]
                                             [first? #t])
                                   (if (null? rest)
                                       (reverse segs)
                                       (let ([new-segs (if first?
                                                           (cons
                                                             (cons
                                                               (car rest)
                                                               'expanded)
                                                             segs)
                                                           (cons
                                                             (cons
                                                               (car rest)
                                                               'expanded)
                                                             (cons
                                                               (cons
                                                                 ""
                                                                 'word-break)
                                                               segs)))])
                                         (eloop (cdr rest) new-segs #f)))))
                               (string-join-with sep vals)))))]
                    [(and (or (string=? subscript "@")
                              (string=? subscript "*"))
                          (> (string-length after-bracket) 0))
                     (let* ([vals (env-array-values env name)])
                       (let-values ([(mname modifier arg)
                                     (parse-parameter-modifier
                                       (string-append "x" after-bracket))])
                         (if (string=? subscript "@")
                             (if (memq modifier '(- :- + :+ = := ? :?))
                                 (let ([val (if (null? vals)
                                                #f
                                                (string-join-with
                                                  " "
                                                  vals))])
                                   (apply-parameter-modifier
                                     (if (and (memq
                                                modifier
                                                '(:- :+ := :?))
                                              (null? vals))
                                         #f
                                         val)
                                     name modifier arg env))
                                 (let ([modified-vals (map (lambda (v)
                                                             (apply-parameter-modifier v name
                                                               modifier arg
                                                               env))
                                                           vals)])
                                   (if (null? modified-vals)
                                       ""
                                       (make-modifier-segments
                                         (let eloop ([rest modified-vals]
                                                     [segs (list)]
                                                     [first? #t])
                                           (if (null? rest)
                                               (reverse segs)
                                               (let ([new-segs (if first?
                                                                   (cons
                                                                     (cons
                                                                       (car rest)
                                                                       'expanded)
                                                                     segs)
                                                                   (cons
                                                                     (cons
                                                                       (car rest)
                                                                       'expanded)
                                                                     (cons
                                                                       (cons
                                                                         ""
                                                                         'word-break)
                                                                       segs)))])
                                                 (eloop
                                                   (cdr rest)
                                                   new-segs
                                                   #f))))))))
                             (if (eq? modifier 'at-op)
                                 (let* ([modified-vals (map (lambda (v)
                                                              (apply-parameter-modifier v name
                                                                modifier
                                                                arg env))
                                                            vals)])
                                   (let* ([ifs (or (env-get env "IFS")
                                                   " \t\n")])
                                     (let* ([sep (if (> (string-length ifs)
                                                        0)
                                                     (string
                                                       (string-ref ifs 0))
                                                     "")])
                                       (string-join-with
                                         sep
                                         modified-vals))))
                                 (let* ([ifs (or (env-get env "IFS")
                                                 " \t\n")])
                                   (let* ([sep (if (> (string-length ifs)
                                                      0)
                                                   (string
                                                     (string-ref ifs 0))
                                                   "")])
                                     (let* ([joined (string-join-with
                                                      sep
                                                      vals)])
                                       (let ([effective-modifier (if (and (not (*in-dquote-context*))
                                                                          (>= (length
                                                                                vals)
                                                                              2))
                                                                     (case modifier
                                                                       [(:-)
                                                                        '-]
                                                                       [(:+)
                                                                        '+]
                                                                       [(:=)
                                                                        '=]
                                                                       [(:?)
                                                                        '?]
                                                                       [else
                                                                        modifier])
                                                                     modifier)])
                                         (apply-parameter-modifier (if (null? vals) #f joined) name
                                           effective-modifier arg
                                           env)))))))))]
                    [else
                     (let ([expanded-idx (let ([var (env-get-var
                                                      env
                                                      name)])
                                           (if (and var
                                                    (shell-var-assoc? var))
                                               (expand-word-nosplit
                                                 subscript
                                                 env)
                                               (number->string
                                                 (slice-arith-eval
                                                   subscript
                                                   env))))])
                       (if (string=? after-bracket "")
                           (env-array-get env name expanded-idx)
                           (let* ([raw-val (env-array-get
                                             env
                                             name
                                             expanded-idx)])
                             (let* ([val (if (env-array-element-set?
                                               env
                                               name
                                               expanded-idx)
                                             raw-val
                                             #f)])
                               (let-values ([(mname modifier arg)
                                             (parse-parameter-modifier
                                               (string-append
                                                 "x"
                                                 after-bracket))])
                                 (if (and (memq modifier '(= :=))
                                          (or (not val)
                                              (and (eq? modifier ':=)
                                                   val
                                                   (string=? val ""))))
                                     (let ([default (expand-string
                                                      arg
                                                      env)])
                                       (env-array-set!
                                         env
                                         name
                                         expanded-idx
                                         default)
                                       default)
                                     (apply-parameter-modifier val name
                                       modifier arg env)))))))]))))))))
  (define (special-param-char? ch)
    (or (char=? ch #\@)
        (char=? ch #\*)
        (char=? ch #\?)
        (char=? ch #\!)
        (char=? ch #\$)
        (char=? ch #\-)))
  (define (find-patsub-separator str pattern-start)
    (let ([len (string-length str)])
      (let ([past-first (cond
                          [(>= pattern-start len) pattern-start]
                          [(char=? (string-ref str pattern-start) #\')
                           (let inner ([j (+ pattern-start 1)])
                             (cond
                               [(>= j len) j]
                               [(char=? (string-ref str j) #\') (+ j 1)]
                               [else (inner (+ j 1))]))]
                          [(char=? (string-ref str pattern-start) #\")
                           (let inner ([j (+ pattern-start 1)])
                             (cond
                               [(>= j len) j]
                               [(char=? (string-ref str j) #\") (+ j 1)]
                               [(char=? (string-ref str j) #\\)
                                (inner (+ j 2))]
                               [else (inner (+ j 1))]))]
                          [(char=? (string-ref str pattern-start) #\\)
                           (+ pattern-start 2)]
                          [else (+ pattern-start 1)])])
        (let loop ([i past-first])
          (cond
            [(>= i len) #f]
            [(char=? (string-ref str i) #\')
             (let inner ([j (+ i 1)])
               (cond
                 [(>= j len) #f]
                 [(char=? (string-ref str j) #\') (loop (+ j 1))]
                 [else (inner (+ j 1))]))]
            [(char=? (string-ref str i) #\")
             (let inner ([j (+ i 1)])
               (cond
                 [(>= j len) #f]
                 [(char=? (string-ref str j) #\") (loop (+ j 1))]
                 [(char=? (string-ref str j) #\\) (inner (+ j 2))]
                 [else (inner (+ j 1))]))]
            [(char=? (string-ref str i) #\\) (loop (+ i 2))]
            [(char=? (string-ref str i) #\/) i]
            [else (loop (+ i 1))])))))
  (define (parse-modifier-from-rest rest)
    (let ([rlen (string-length rest)])
      (if (= rlen 0)
          (values #f "")
          (let ([mod-ch (string-ref rest 0)])
            (cond
              [(char=? mod-ch #\:)
               (if (> rlen 1)
                   (let ([mod2 (string-ref rest 1)])
                     (case mod2
                       [(#\- #\+ #\= #\?)
                        (values
                          (string->symbol (string #\: mod2))
                          (substring rest 2 rlen))]
                       [else (values ': (substring rest 1 rlen))]))
                   (error 'gerbil "bad substitution"))]
              [(memq mod-ch '(#\- #\+ #\= #\?))
               (values
                 (string->symbol (string mod-ch))
                 (substring rest 1 rlen))]
              [(char=? mod-ch #\%)
               (if (and (> rlen 1) (char=? (string-ref rest 1) #\%))
                   (values '%% (substring rest 2 rlen))
                   (values '% (substring rest 1 rlen)))]
              [(char=? mod-ch #\#)
               (if (and (> rlen 1) (char=? (string-ref rest 1) #\#))
                   (values 'prefix-long (substring rest 2 rlen))
                   (values 'prefix-short (substring rest 1 rlen)))]
              [(char=? mod-ch #\/)
               (if (and (> rlen 1) (char=? (string-ref rest 1) #\/))
                   (let ([sep (find-patsub-separator rest 2)])
                     (if sep
                         (values
                           '//
                           (cons
                             (substring rest 2 sep)
                             (substring rest (+ sep 1) rlen)))
                         (values '// (cons (substring rest 2 rlen) ""))))
                   (let ([sep (find-patsub-separator rest 1)])
                     (if sep
                         (values
                           '/
                           (cons
                             (substring rest 1 sep)
                             (substring rest (+ sep 1) rlen)))
                         (values '/ (cons (substring rest 1 rlen) "")))))]
              [(char=? mod-ch #\^)
               (if (and (> rlen 1) (char=? (string-ref rest 1) #\^))
                   (values '^^ (substring rest 2 rlen))
                   (values '^ (substring rest 1 rlen)))]
              [(char=? mod-ch #\,)
               (if (and (> rlen 1) (char=? (string-ref rest 1) #\,))
                   (values 'lc-all (substring rest 2 rlen))
                   (values 'lc-first (substring rest 1 rlen)))]
              [(char=? mod-ch #\@)
               (if (> rlen 1)
                   (values 'at-op (substring rest 1 rlen))
                   (values #f ""))]
              [else (values #f "")])))))
  (define (parse-parameter-modifier content)
    (let ([len (string-length content)])
      (if (and (> len 0)
               (let ([ch (string-ref content 0)])
                 (or (special-param-char? ch)
                     (and (char-numeric? ch)
                          (or (= len 1)
                              (not (char-numeric?
                                     (string-ref content 1))))))))
          (let* ([name (string (string-ref content 0))])
            (let* ([rest (substring content 1 len)])
              (let-values ([(modifier arg)
                            (parse-modifier-from-rest rest)])
                (if modifier
                    (values name modifier arg)
                    (values name #f "")))))
          (let loop ([i 0])
            (cond
              [(>= i len) (values content #f "")]
              [(memq
                 (string-ref content i)
                 '(#\: #\% #\# #\/ #\^ #\, #\@))
               (let ([ch (string-ref content i)])
                 (case ch
                   [(#\:)
                    (if (< (+ i 1) len)
                        (let ([mod-ch (string-ref content (+ i 1))])
                          (case mod-ch
                            [(#\- #\+ #\= #\?)
                             (values
                               (substring content 0 i)
                               (string->symbol (string #\: mod-ch))
                               (substring content (+ i 2) len))]
                            [else
                             (values
                               (substring content 0 i)
                               ':
                               (substring content (+ i 1) len))]))
                        (error 'gerbil
                          (format "bad substitution: ${~a}" content)))]
                   [(#\%)
                    (if (and (< (+ i 1) len)
                             (char=? (string-ref content (+ i 1)) #\%))
                        (values
                          (substring content 0 i)
                          '%%
                          (substring content (+ i 2) len))
                        (values
                          (substring content 0 i)
                          '%
                          (substring content (+ i 1) len)))]
                   [(#\#)
                    (if (and (< (+ i 1) len)
                             (char=? (string-ref content (+ i 1)) #\#))
                        (values
                          (substring content 0 i)
                          'prefix-long
                          (substring content (+ i 2) len))
                        (values
                          (substring content 0 i)
                          'prefix-short
                          (substring content (+ i 1) len)))]
                   [(#\/)
                    (if (and (< (+ i 1) len)
                             (char=? (string-ref content (+ i 1)) #\/))
                        (let ([sep (find-patsub-separator
                                     content
                                     (+ i 2))])
                          (if sep
                              (values
                                (substring content 0 i)
                                '//
                                (cons
                                  (substring content (+ i 2) sep)
                                  (substring content (+ sep 1) len)))
                              (values
                                (substring content 0 i)
                                '//
                                (cons
                                  (substring content (+ i 2) len)
                                  ""))))
                        (let ([sep (find-patsub-separator
                                     content
                                     (+ i 1))])
                          (if sep
                              (values
                                (substring content 0 i)
                                '/
                                (cons
                                  (substring content (+ i 1) sep)
                                  (substring content (+ sep 1) len)))
                              (values
                                (substring content 0 i)
                                '/
                                (cons
                                  (substring content (+ i 1) len)
                                  "")))))]
                   [(#\^)
                    (if (and (< (+ i 1) len)
                             (char=? (string-ref content (+ i 1)) #\^))
                        (values
                          (substring content 0 i)
                          '^^
                          (substring content (+ i 2) len))
                        (values
                          (substring content 0 i)
                          '^
                          (substring content (+ i 1) len)))]
                   [(#\,)
                    (if (and (< (+ i 1) len)
                             (char=? (string-ref content (+ i 1)) #\,))
                        (values
                          (substring content 0 i)
                          'lc-all
                          (substring content (+ i 2) len))
                        (values
                          (substring content 0 i)
                          'lc-first
                          (substring content (+ i 1) len)))]
                   [(#\@)
                    (values
                      (substring content 0 i)
                      'at-op
                      (substring content (+ i 1) len))]
                   [else (loop (+ i 1))]))]
              [(and (memq (string-ref content i) '(#\- #\+ #\= #\?))
                    (let loop2 ([j 0])
                      (or (= j i)
                          (and (let ([ch (string-ref content j)])
                                 (or (char-alphabetic? ch)
                                     (char-numeric? ch)
                                     (char=? ch #\_)))
                               (loop2 (+ j 1))))))
               (values
                 (substring content 0 i)
                 (string->symbol (string (string-ref content i)))
                 (substring content (+ i 1) len))]
              [(not (let ([ch (string-ref content i)])
                      (or (char-alphabetic? ch)
                          (char-numeric? ch)
                          (char=? ch #\_)
                          (char=? ch #\[)
                          (char=? ch #\])
                          (char=? ch #\!)
                          (char=? ch #\*))))
               (error 'gerbil
                 (format
                   "~a: bad substitution"
                   (string-append "${" content "}")))]
              [else (loop (+ i 1))])))))
  (define pattern-substitute-first
    (case-lambda
      [(val pattern replacement)
       (let* ([extglob? #f])
         (let ([vlen (string-length val)])
           (cond
             [(and (> (string-length pattern) 0)
                   (char=? (string-ref pattern 0) #\#))
              (let ([pat (substring pattern 1 (string-length pattern))])
                (let loop ([i vlen])
                  (if (< i 0)
                      val
                      (if (glob-match? pat (substring val 0 i) #f extglob?)
                          (string-append
                            replacement
                            (substring val i vlen))
                          (loop (- i 1))))))]
             [(and (> (string-length pattern) 0)
                   (char=? (string-ref pattern 0) #\%))
              (let ([pat (substring pattern 1 (string-length pattern))])
                (let loop ([i 0])
                  (if (> i vlen)
                      val
                      (if (glob-match?
                            pat
                            (substring val i vlen)
                            #f
                            extglob?)
                          (string-append (substring val 0 i) replacement)
                          (loop (+ i 1))))))]
             [else
              (let loop ([start 0])
                (if (> start vlen)
                    val
                    (let inner ([end vlen])
                      (cond
                        [(< end start) (loop (+ start 1))]
                        [(glob-match?
                           pattern
                           (substring val start end)
                           #f
                           extglob?)
                         (string-append
                           (substring val 0 start)
                           replacement
                           (substring val end vlen))]
                        [else (inner (- end 1))]))))])))]
      [(val pattern replacement extglob?)
       (let ([vlen (string-length val)])
         (cond
           [(and (> (string-length pattern) 0)
                 (char=? (string-ref pattern 0) #\#))
            (let ([pat (substring pattern 1 (string-length pattern))])
              (let loop ([i vlen])
                (if (< i 0)
                    val
                    (if (glob-match? pat (substring val 0 i) #f extglob?)
                        (string-append replacement (substring val i vlen))
                        (loop (- i 1))))))]
           [(and (> (string-length pattern) 0)
                 (char=? (string-ref pattern 0) #\%))
            (let ([pat (substring pattern 1 (string-length pattern))])
              (let loop ([i 0])
                (if (> i vlen)
                    val
                    (if (glob-match?
                          pat
                          (substring val i vlen)
                          #f
                          extglob?)
                        (string-append (substring val 0 i) replacement)
                        (loop (+ i 1))))))]
           [else
            (let loop ([start 0])
              (if (> start vlen)
                  val
                  (let inner ([end vlen])
                    (cond
                      [(< end start) (loop (+ start 1))]
                      [(glob-match?
                         pattern
                         (substring val start end)
                         #f
                         extglob?)
                       (string-append
                         (substring val 0 start)
                         replacement
                         (substring val end vlen))]
                      [else (inner (- end 1))]))))]))]))
  (define pattern-substitute-all
    (case-lambda
      [(val pattern replacement)
       (let* ([extglob? #f])
         (let ([vlen (string-length val)])
           (if (string=? pattern "")
               val
               (let ([buf (open-output-string)])
                 (let loop ([start 0])
                   (if (> start vlen)
                       (get-output-string buf)
                       (let inner ([end vlen])
                         (cond
                           [(< end (+ start 1))
                            (if (< start vlen)
                                (begin
                                  (display (string-ref val start) buf)
                                  (loop (+ start 1)))
                                (get-output-string buf))]
                           [(glob-match?
                              pattern
                              (substring val start end)
                              #f
                              extglob?)
                            (display replacement buf)
                            (if (= end start)
                                (begin
                                  (when (< start vlen)
                                    (display (string-ref val start) buf))
                                  (loop (+ start 1)))
                                (loop end))]
                           [else (inner (- end 1))]))))))))]
      [(val pattern replacement extglob?)
       (let ([vlen (string-length val)])
         (if (string=? pattern "")
             val
             (let ([buf (open-output-string)])
               (let loop ([start 0])
                 (if (> start vlen)
                     (get-output-string buf)
                     (let inner ([end vlen])
                       (cond
                         [(< end (+ start 1))
                          (if (< start vlen)
                              (begin
                                (display (string-ref val start) buf)
                                (loop (+ start 1)))
                              (get-output-string buf))]
                         [(glob-match?
                            pattern
                            (substring val start end)
                            #f
                            extglob?)
                          (display replacement buf)
                          (if (= end start)
                              (begin
                                (when (< start vlen)
                                  (display (string-ref val start) buf))
                                (loop (+ start 1)))
                              (loop end))]
                         [else (inner (- end 1))])))))))]))
  (define (take-sublist lst start end)
    (let loop ([l lst] [i 0] [result (list)])
      (cond
        [(or (null? l) (>= i end)) (reverse result)]
        [(< i start) (loop (cdr l) (+ i 1) result)]
        [else (loop (cdr l) (+ i 1) (cons (car l) result))])))
  (define (shell-quote-for-at-q s)
    (if (string=? s "")
        "''"
        (let ([len (string-length s)])
          (if (string-has-control-chars? s)
              (let ([buf (open-output-string)])
                (display "$'" buf)
                (let loop ([i 0])
                  (when (< i len)
                    (let* ([ch (string-ref s i)])
                      (let* ([code (char->integer ch)])
                        (cond
                          [(char=? ch #\newline) (display "\\n" buf)]
                          [(char=? ch #\tab) (display "\\t" buf)]
                          [(char=? ch #\return) (display "\\r" buf)]
                          [(char=? ch #\\) (display "\\\\" buf)]
                          [(char=? ch #\') (display "\\'" buf)]
                          [(or (< code 32) (= code 127))
                           (let ([oct (number->string code 8)])
                             (display
                               (string-append
                                 "\\"
                                 (make-string
                                   (- 3 (string-length oct))
                                   #\0)
                                 oct)
                               buf))]
                          [else (display ch buf)])))
                    (loop (+ i 1))))
                (display "'" buf)
                (get-output-string buf))
              (let ([buf (open-output-string)])
                (display "'" buf)
                (let loop ([i 0])
                  (when (< i len)
                    (let ([ch (string-ref s i)])
                      (if (char=? ch #\')
                          (display "'\\''" buf)
                          (display ch buf)))
                    (loop (+ i 1))))
                (display "'" buf)
                (get-output-string buf))))))
  (define (get-var-attributes name env)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (if (not var)
          ""
          (string-append (if (shell-var-assoc? var) "A" "")
            (if (shell-var-array? var) "a" "")
            (if (shell-var-integer? var) "i" "")
            (if (shell-var-lowercase? var) "l" "")
            (if (shell-var-nameref? var) "n" "")
            (if (shell-var-readonly? var) "r" "")
            (if (shell-var-uppercase? var) "u" "")
            (if (shell-var-exported? var) "x" "")))))
  (define (get-var-declare-prefix name env)
    (let ([attrs (get-var-attributes name env)])
      (if (string=? attrs "")
          "declare -- "
          (string-append "declare -" attrs " "))))
  (define (expand-ansi-c-escapes s)
    (let ([buf (open-output-string)] [len (string-length s)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string buf)
            (let ([ch (string-ref s i)])
              (if (and (char=? ch #\\) (< (+ i 1) len))
                  (let ([esc (string-ref s (+ i 1))])
                    (case esc
                      [(#\n) (display #\newline buf) (loop (+ i 2))]
                      [(#\t) (display #\tab buf) (loop (+ i 2))]
                      [(#\r) (display #\return buf) (loop (+ i 2))]
                      [(#\a)
                       (display (integer->char 7) buf)
                       (loop (+ i 2))]
                      [(#\b)
                       (display (integer->char 8) buf)
                       (loop (+ i 2))]
                      [(#\e #\E)
                       (display (integer->char 27) buf)
                       (loop (+ i 2))]
                      [(#\f)
                       (display (integer->char 12) buf)
                       (loop (+ i 2))]
                      [(#\v)
                       (display (integer->char 11) buf)
                       (loop (+ i 2))]
                      [(#\\) (display #\\ buf) (loop (+ i 2))]
                      [(#\') (display #\' buf) (loop (+ i 2))]
                      [(#\") (display #\" buf) (loop (+ i 2))]
                      [else
                       (display ch buf)
                       (display esc buf)
                       (loop (+ i 2))]))
                  (begin (display ch buf) (loop (+ i 1)))))))))
  (define (slice-arith-eval str env)
    (let ([trimmed (string-trim-whitespace-str str)])
      (if (string=? trimmed "")
          0
          (or (string->number trimmed)
              (arith-eval
                (parameterize ([*in-dquote-context* #t])
                  (expand-arith-expr trimmed env))
                (arith-env-getter env)
                (arith-env-setter env)
                (and (env-option? env "nounset")
                     (lambda (name) (nounset-error! name env))))))))
  (define (valid-indirect-ref? s)
    (let ([len (string-length s)])
      (and (> len 0)
           (let ([ch0 (string-ref s 0)])
             (cond
               [(and (= len 1)
                     (or (char=? ch0 #\?)
                         (char=? ch0 #\#)
                         (char=? ch0 #\@)
                         (char=? ch0 #\*)
                         (char=? ch0 #\$)
                         (char=? ch0 #\!)
                         (char=? ch0 #\-)
                         (char=? ch0 #\_)))
                #t]
               [(char-numeric? ch0)
                (let loop ([i 1])
                  (or (>= i len)
                      (and (char-numeric? (string-ref s i))
                           (loop (+ i 1)))))]
               [(or (char-alphabetic? ch0) (char=? ch0 #\_))
                (let loop ([i 1])
                  (if (>= i len)
                      #t
                      (let ([ch (string-ref s i)])
                        (cond
                          [(or (char-alphabetic? ch)
                               (char-numeric? ch)
                               (char=? ch #\_))
                           (loop (+ i 1))]
                          [(char=? ch #\[) #t]
                          [else #f]))))]
               [else #f])))))
  (define (unescape-brace arg)
    (if (not (string? arg))
        arg
        (let ([len (string-length arg)])
          (if (not (string-index arg #\\))
              arg
              (let ([buf (open-output-string)])
                (let loop ([i 0])
                  (if (>= i len)
                      (get-output-string buf)
                      (let ([ch (string-ref arg i)])
                        (cond
                          [(and (char=? ch #\\)
                                (< (+ i 1) len)
                                (char=? (string-ref arg (+ i 1)) #\}))
                           (display #\} buf)
                           (loop (+ i 2))]
                          [else (display ch buf) (loop (+ i 1))])))))))))
  (define (tilde-expand-default arg env)
    (let ([segments (split-assignment-on-colon arg)])
      (let ([strs (map (lambda (seg)
                         (if (and (> (string-length seg) 0)
                                  (char=? (string-ref seg 0) #\~))
                             (let-values ([(expanded end)
                                           (expand-tilde-in seg 0 env)])
                               (string-append
                                 expanded
                                 (substring seg end (string-length seg))))
                             seg))
                       segments)]
            [sep ":"])
        (if (null? strs)
            ""
            (let lp ([result (car strs)] [rest (cdr strs)])
              (if (null? rest)
                  result
                  (lp (string-append result sep (car rest))
                      (cdr rest))))))))
  (define (apply-parameter-modifier val name modifier arg0
           env)
    (let ([arg (unescape-brace arg0)]
          [colon-null? (lambda (v)
                         (or (not v)
                             (and (string? v)
                                  (string=? v "")
                                  (not (and (not (*in-dquote-context*))
                                            (or (string=? name "*")
                                                (string=? name "@"))
                                            (let ([params (env-positional-list
                                                            env)])
                                              (and (pair? params)
                                                   (pair?
                                                     (cdr params)))))))))])
      (case modifier
        [(:-)
         (if (colon-null? val)
             (let ([earg (if (*in-dquote-context*)
                             arg
                             (tilde-expand-default arg env))])
               (if (*in-dquote-context*)
                   (expand-string earg env)
                   (make-modifier-segments
                     (segments-literals-splittable
                       (expand-string-segments earg env)))))
             val)]
        [(-)
         (if (not val)
             (let ([earg (if (*in-dquote-context*)
                             arg
                             (tilde-expand-default arg env))])
               (if (*in-dquote-context*)
                   (expand-string earg env)
                   (make-modifier-segments
                     (segments-literals-splittable
                       (expand-string-segments earg env)))))
             val)]
        [(:=)
         (if (colon-null? val)
             (let* ([earg (if (*in-dquote-context*)
                              arg
                              (tilde-expand-default arg env))])
               (let* ([default (expand-string earg env)])
                 (env-set! env name default)
                 (if (*in-dquote-context*)
                     default
                     (make-modifier-segments
                       (segments-literals-splittable
                         (expand-string-segments earg env))))))
             val)]
        [(=)
         (if (not val)
             (let* ([earg (if (*in-dquote-context*)
                              arg
                              (tilde-expand-default arg env))])
               (let* ([default (expand-string earg env)])
                 (env-set! env name default)
                 (if (*in-dquote-context*)
                     default
                     (make-modifier-segments
                       (segments-literals-splittable
                         (expand-string-segments earg env))))))
             val)]
        [(:?)
         (if (colon-null? val)
             (let ([msg (if (string=? arg "")
                            "parameter null or not set"
                            (expand-string arg env))])
               (fprintf (current-error-port) "jsh: ~a: ~a~n" name msg)
               (raise (make-nounset-exception 1)))
             val)]
        [(?)
         (if (not val)
             (let ([msg (if (string=? arg "")
                            "parameter not set"
                            (expand-string arg env))])
               (fprintf (current-error-port) "jsh: ~a: ~a~n" name msg)
               (raise (make-nounset-exception 1)))
             val)]
        [(:+)
         (if (not (colon-null? val))
             (let ([earg (if (*in-dquote-context*)
                             arg
                             (tilde-expand-default arg env))])
               (if (*in-dquote-context*)
                   (expand-string earg env)
                   (make-modifier-segments
                     (segments-literals-splittable
                       (expand-string-segments earg env)))))
             "")]
        [(+)
         (if val
             (let ([earg (if (*in-dquote-context*)
                             arg
                             (tilde-expand-default arg env))])
               (if (*in-dquote-context*)
                   (expand-string earg env)
                   (make-modifier-segments
                     (segments-literals-splittable
                       (expand-string-segments earg env)))))
             "")]
        [(%)
         (if val
             (remove-suffix
               val
               (expand-pattern arg env)
               #f
               (env-shopt? env "extglob"))
             "")]
        [(%%)
         (if val
             (remove-suffix
               val
               (expand-pattern arg env)
               #t
               (env-shopt? env "extglob"))
             "")]
        [(prefix-short)
         (if val
             (remove-prefix
               val
               (expand-pattern arg env)
               #f
               (env-shopt? env "extglob"))
             "")]
        [(prefix-long)
         (if val
             (remove-prefix
               val
               (expand-pattern arg env)
               #t
               (env-shopt? env "extglob"))
             "")]
        [(^^)
         (if val
             (if (string=? arg "")
                 (string-upcase val)
                 (let ([pat (expand-pattern arg env)])
                   (case-convert-matching val pat char-upcase #t
                     (env-shopt? env "extglob"))))
             "")]
        [(^)
         (if (and val (> (string-length val) 0))
             (if (string=? arg "")
                 (string-append
                   (string (char-upcase (string-ref val 0)))
                   (substring val 1 (string-length val)))
                 (let ([pat (expand-pattern arg env)])
                   (case-convert-matching val pat char-upcase #f
                     (env-shopt? env "extglob"))))
             (or val ""))]
        [(lc-all)
         (if val
             (if (string=? arg "")
                 (string-downcase val)
                 (let ([pat (expand-pattern arg env)])
                   (case-convert-matching val pat char-downcase #t
                     (env-shopt? env "extglob"))))
             "")]
        [(lc-first)
         (if (and val (> (string-length val) 0))
             (if (string=? arg "")
                 (string-append
                   (string (char-downcase (string-ref val 0)))
                   (substring val 1 (string-length val)))
                 (let ([pat (expand-pattern arg env)])
                   (case-convert-matching val pat char-downcase #f
                     (env-shopt? env "extglob"))))
             (or val ""))]
        [(:)
         (cond
           [(and val (or (string=? name "@") (string=? name "*")))
            (let* ([colon-pos (string-find-char-from arg #\: 0)])
              (let* ([offset-str (if colon-pos
                                     (substring arg 0 colon-pos)
                                     (string-trim-whitespace-str arg))])
                (let* ([length-str (if colon-pos
                                       (string-trim-whitespace-str
                                         (substring
                                           arg
                                           (+ colon-pos 1)
                                           (string-length arg)))
                                       #f)])
                  (let* ([offset (slice-arith-eval offset-str env)])
                    (let* ([params (env-positional-list env)])
                      (let* ([full-list (if (<= offset 0)
                                            (cons
                                              (shell-environment-shell-name
                                                env)
                                              params)
                                            params)])
                        (let* ([slen (length full-list)])
                          (let* ([start (cond
                                          [(= offset 0) 0]
                                          [(< offset 0)
                                           (max 0 (+ slen offset))]
                                          [else (- offset 1)])])
                            (let* ([start (max 0 (min start slen))])
                              (let* ([sliced (if length-str
                                                 (let ([ln (slice-arith-eval
                                                             length-str
                                                             env)])
                                                   (if (< ln 0)
                                                       (let ([end (max start
                                                                       (+ slen
                                                                          ln))])
                                                         (take-sublist
                                                           full-list
                                                           start
                                                           end))
                                                       (take-sublist
                                                         full-list
                                                         start
                                                         (min slen
                                                              (+ start
                                                                 ln)))))
                                                 (take-sublist
                                                   full-list
                                                   start
                                                   slen))])
                                (string-join-with " " sliced)))))))))))]
           [val
            (let* ([str val])
              (let* ([slen (string-length str)])
                (let* ([colon-pos (string-find-char-from arg #\: 0)])
                  (let* ([offset-str (if colon-pos
                                         (substring arg 0 colon-pos)
                                         (string-trim-whitespace-str
                                           arg))])
                    (let* ([length-str (if colon-pos
                                           (string-trim-whitespace-str
                                             (substring
                                               arg
                                               (+ colon-pos 1)
                                               (string-length arg)))
                                           #f)])
                      (let* ([offset (slice-arith-eval offset-str env)])
                        (let* ([start (if (< offset 0)
                                          (max 0 (+ slen offset))
                                          (min offset slen))])
                          (if length-str
                              (let ([length (slice-arith-eval
                                              length-str
                                              env)])
                                (if (< length 0)
                                    (let ([end (max start
                                                    (+ slen length))])
                                      (substring str start end))
                                    (substring
                                      str
                                      start
                                      (min slen (+ start length)))))
                              (substring str start slen)))))))))]
           [else ""])]
        [(/)
         (if val
             (let ([pattern (expand-pattern (car arg) env)]
                   [replacement (parameterize ([*in-dquote-context* #f])
                                  (expand-string (cdr arg) env))])
               (pattern-substitute-first
                 val
                 pattern
                 replacement
                 (env-shopt? env "extglob")))
             "")]
        [(//)
         (if val
             (let ([pattern (expand-pattern (car arg) env)]
                   [replacement (parameterize ([*in-dquote-context* #f])
                                  (expand-string (cdr arg) env))])
               (pattern-substitute-all
                 val
                 pattern
                 replacement
                 (env-shopt? env "extglob")))
             "")]
        [(at-op)
         (let ([op (if (> (string-length arg) 0)
                       (string-ref arg 0)
                       #f)])
           (case op
             [(#\Q) (if val (shell-quote-for-at-q val) "")]
             [(#\a) (get-var-attributes name env)]
             [(#\U) (if val (string-upcase val) "")]
             [(#\u)
              (if (and val (> (string-length val) 0))
                  (string-append
                    (string (char-upcase (string-ref val 0)))
                    (substring val 1 (string-length val)))
                  (or val ""))]
             [(#\L) (if val (string-downcase val) "")]
             [(#\E) (if val (expand-ansi-c-escapes val) "")]
             [(#\P) (or val "")]
             [(#\K #\k) (if val (shell-quote-for-at-q val) "")]
             [(#\A)
              (if val
                  (let ([attrs (get-var-attributes name env)])
                    (if (string=? attrs "")
                        (string-append name "=" (shell-quote-for-at-q val))
                        (string-append "declare -" attrs " " name "="
                          (shell-quote-for-at-q val))))
                  "")]
             [else (or val "")]))]
        [(#f) (or val "")]
        [else (or val "")])))
  (define (expand-command-sub str i env)
    (let* ([close (find-matching-paren str (+ i 2))])
      (let* ([cmd (substring str (+ i 2) close)])
        (let* ([output (command-substitute cmd env)])
          (values output (+ close 1))))))
  (define expand-backtick
    (case-lambda
      [(str i env)
       (let* ([in-dquote? #f])
         (let* ([len (string-length str)])
           (let* ([end (let loop ([j (+ i 1)])
                         (cond
                           [(>= j len) j]
                           [(char=? (string-ref str j) #\\) (loop (+ j 2))]
                           [(char=? (string-ref str j) #\`) j]
                           [else (loop (+ j 1))]))])
             (let* ([raw (substring str (+ i 1) end)])
               (let* ([cmd (let ([rlen (string-length raw)]
                                 [buf (open-output-string)]
                                 [special (if in-dquote?
                                              '(#\$ #\` #\\ #\" #\newline)
                                              '(#\$ #\` #\\ #\newline))])
                             (let loop ([k 0])
                               (cond
                                 [(>= k rlen) (get-output-string buf)]
                                 [(char=? (string-ref raw k) #\\)
                                  (if (< (+ k 1) rlen)
                                      (let ([next (string-ref
                                                    raw
                                                    (+ k 1))])
                                        (if (memq next special)
                                            (begin
                                              (display next buf)
                                              (loop (+ k 2)))
                                            (begin
                                              (display #\\ buf)
                                              (display next buf)
                                              (loop (+ k 2)))))
                                      (begin
                                        (display #\\ buf)
                                        (loop (+ k 1))))]
                                 [else
                                  (display (string-ref raw k) buf)
                                  (loop (+ k 1))])))])
                 (let* ([output (command-substitute cmd env)])
                   (values output (+ end 1))))))))]
      [(str i env in-dquote?)
       (let* ([len (string-length str)])
         (let* ([end (let loop ([j (+ i 1)])
                       (cond
                         [(>= j len) j]
                         [(char=? (string-ref str j) #\\) (loop (+ j 2))]
                         [(char=? (string-ref str j) #\`) j]
                         [else (loop (+ j 1))]))])
           (let* ([raw (substring str (+ i 1) end)])
             (let* ([cmd (let ([rlen (string-length raw)]
                               [buf (open-output-string)]
                               [special (if in-dquote?
                                            '(#\$ #\` #\\ #\" #\newline)
                                            '(#\$ #\` #\\ #\newline))])
                           (let loop ([k 0])
                             (cond
                               [(>= k rlen) (get-output-string buf)]
                               [(char=? (string-ref raw k) #\\)
                                (if (< (+ k 1) rlen)
                                    (let ([next (string-ref raw (+ k 1))])
                                      (if (memq next special)
                                          (begin
                                            (display next buf)
                                            (loop (+ k 2)))
                                          (begin
                                            (display #\\ buf)
                                            (display next buf)
                                            (loop (+ k 2)))))
                                    (begin
                                      (display #\\ buf)
                                      (loop (+ k 1))))]
                               [else
                                (display (string-ref raw k) buf)
                                (loop (+ k 1))])))])
               (let* ([output (command-substitute cmd env)])
                 (values output (+ end 1)))))))]))
  (define (command-substitute cmd env)
    (*command-sub-ran* #t)
    (let ([exec-fn (*execute-input*)])
      (if exec-fn
          (guard (__exn [#t ((lambda (e) "") __exn)])
            (let-values ([(read-fd write-fd) (ffi-pipe-raw)])
              (let ([saved-fd (ffi-dup 1)]
                    [saved-port (current-output-port)])
                (ffi-dup2 write-fd 1)
                (ffi-close-fd write-fd)
                (let ([pipe-port (open-output-file
                                   (string-append
                                     "/dev/fd/"
                                     (number->string 1)))])
                  (current-output-port pipe-port)
                  (let ([sub-env (env-clone env)])
                    (guard (__exn
                             [#t
                              ((lambda (e)
                                 (when (subshell-exit-exception? e)
                                   (env-set-last-status!
                                     env
                                     (subshell-exit-exception-status e))))
                                __exn)])
                      (parameterize ([*in-subshell* #t]
                                     [*pipeline-stdin-fd* #f]
                                     [*pipeline-stdout-fd* #f])
                        (exec-fn cmd sub-env))
                      (env-set-last-status!
                        env
                        (shell-environment-last-status sub-env))))
                  (flush-output-port pipe-port)
                  (close-port pipe-port))
                (ffi-dup2 saved-fd 1)
                (ffi-close-fd saved-fd)
                (current-output-port saved-port)
                (let ([output (latin1->utf8
                                (ffi-read-all-from-fd read-fd))])
                  (ffi-close-fd read-fd)
                  (string-trim-trailing-newlines output)))))
          (guard (__exn [#t ((lambda (e) "") __exn)])
            (let* ([port (open-input-process
                           (list 'path: "/bin/sh" 'arguments:
                             (list "-c" cmd) 'environment:
                             (env-exported-alist env)))])
              (let* ([output (read-all-string port)])
                (close-port port)
                (string-trim-trailing-newlines output)))))))
  (define (expand-arith-sub str i env)
    (let* ([close (find-arith-close str (+ i 3))])
      (let* ([raw-expr (substring str (+ i 3) close)])
        (let* ([expr (parameterize ([*in-dquote-context* #t])
                       (expand-arith-expr raw-expr env))])
          (let* ([result (arith-eval
                           expr
                           (arith-env-getter env)
                           (arith-env-setter env)
                           (and (env-option? env "nounset")
                                (lambda (name)
                                  (nounset-error! name env))))])
            (values (number->string result) (+ close 2)))))))
  (define (expand-arith-expr expr env)
    (let ([len (string-length expr)] [buf (open-output-string)])
      (let loop ([i 0])
        (cond
          [(>= i len) (get-output-string buf)]
          [(and (char=? (string-ref expr i) #\$) (< (+ i 1) len))
           (let ([next (string-ref expr (+ i 1))])
             (cond
               [(and (char=? next #\()
                     (< (+ i 2) len)
                     (char=? (string-ref expr (+ i 2)) #\())
                (let-values ([(val end) (expand-arith-sub expr i env)])
                  (display val buf)
                  (loop end))]
               [(char=? next #\()
                (let-values ([(val end) (expand-command-sub expr i env)])
                  (display val buf)
                  (loop end))]
               [(char=? next #\{)
                (let-values ([(val end)
                              (expand-parameter-braced expr i env)])
                  (display val buf)
                  (loop end))]
               [(or (char-alphabetic? next) (char=? next #\_))
                (let-values ([(val end) (expand-simple-var expr i env)])
                  (display val buf)
                  (loop end))]
               [(char-numeric? next)
                (let-values ([(val end) (expand-simple-var expr i env)])
                  (display val buf)
                  (loop end))]
               [(memv next '(#\? #\! #\# #\- #\$ #\0))
                (let-values ([(val end) (expand-simple-var expr i env)])
                  (display val buf)
                  (loop end))]
               [(char=? next #\')
                (let qloop ([j (+ i 2)])
                  (cond
                    [(>= j len)
                     (display (substring expr (+ i 2) len) buf)
                     (loop len)]
                    [(char=? (string-ref expr j) #\')
                     (display (substring expr (+ i 2) j) buf)
                     (loop (+ j 1))]
                    [(char=? (string-ref expr j) #\\)
                     (qloop (min (+ j 2) len))]
                    [else (qloop (+ j 1))]))]
               [(char=? next #\")
                (let qloop ([j (+ i 2)])
                  (cond
                    [(>= j len)
                     (display (substring expr (+ i 2) len) buf)
                     (loop len)]
                    [(char=? (string-ref expr j) #\")
                     (let ([inner (substring expr (+ i 2) j)])
                       (display (expand-arith-expr inner env) buf)
                       (loop (+ j 1)))]
                    [(char=? (string-ref expr j) #\\)
                     (qloop (min (+ j 2) len))]
                    [else (qloop (+ j 1))]))]
               [else (display #\$ buf) (loop (+ i 1))]))]
          [(char=? (string-ref expr i) #\`)
           (let-values ([(val end) (expand-backtick expr i env)])
             (display val buf)
             (loop end))]
          [(char=? (string-ref expr i) #\')
           (let qloop ([j (+ i 1)])
             (cond
               [(>= j len) (display (substring expr i len) buf) (loop len)]
               [(char=? (string-ref expr j) #\')
                (display (substring expr (+ i 1) j) buf)
                (loop (+ j 1))]
               [else (qloop (+ j 1))]))]
          [(char=? (string-ref expr i) #\")
           (let qloop ([j (+ i 1)])
             (cond
               [(>= j len) (display (substring expr i len) buf) (loop len)]
               [(char=? (string-ref expr j) #\")
                (let ([inner (substring expr (+ i 1) j)])
                  (display (expand-arith-expr inner env) buf)
                  (loop (+ j 1)))]
               [(char=? (string-ref expr j) #\\) (qloop (min (+ j 2) len))]
               [else (qloop (+ j 1))]))]
          [else (display (string-ref expr i) buf) (loop (+ i 1))]))))
  (define (word-split str env)
    (let ([ifs (or (env-get env "IFS") " \t\n")])
      (if (string=? ifs "")
          (list str)
          (let ([len (string-length str)])
            (if (= len 0)
                (list str)
                (let ([ifs-ws? (lambda (ch)
                                 (and (ifs-char? ch ifs)
                                      (or (char=? ch #\space)
                                          (char=? ch #\tab)
                                          (char=? ch #\newline))))]
                      [ifs-nws? (lambda (ch)
                                  (and (ifs-char? ch ifs)
                                       (not (or (char=? ch #\space)
                                                (char=? ch #\tab)
                                                (char=? ch #\newline)))))])
                  (let loop ([i 0] [start 0] [words (list)])
                    (cond
                      [(>= i len)
                       (let ([words (if (> i start)
                                        (cons
                                          (substring str start i)
                                          words)
                                        words)])
                         (reverse words))]
                      [(ifs-nws? (string-ref str i))
                       (let ([words (cons (substring str start i) words)])
                         (let skip-ws ([j (+ i 1)])
                           (if (and (< j len) (ifs-ws? (string-ref str j)))
                               (skip-ws (+ j 1))
                               (loop j j words))))]
                      [(ifs-ws? (string-ref str i))
                       (let skip-ws ([j (+ i 1)])
                         (cond
                           [(and (< j len) (ifs-nws? (string-ref str j)))
                            (let ([words (if (> i start)
                                             (cons
                                               (substring str start i)
                                               words)
                                             words)])
                              (let skip-ws2 ([k (+ j 1)])
                                (if (and (< k len)
                                         (ifs-ws? (string-ref str k)))
                                    (skip-ws2 (+ k 1))
                                    (loop k k words))))]
                           [(and (< j len) (ifs-ws? (string-ref str j)))
                            (skip-ws (+ j 1))]
                           [else
                            (let ([words (if (> i start)
                                             (cons
                                               (substring str start i)
                                               words)
                                             words)])
                              (loop j j words))]))]
                      [else (loop (+ i 1) start words)]))))))))
  (define (ifs-char? ch ifs)
    (let loop ([i 0])
      (and (< i (string-length ifs))
           (or (char=? ch (string-ref ifs i)) (loop (+ i 1))))))
  (define (ifs-whitespace? ch ifs)
    (and (ifs-char? ch ifs)
         (or (char=? ch #\space)
             (char=? ch #\tab)
             (char=? ch #\newline))))
  (define (word-has-quotes? word)
    (let ([len (string-length word)])
      (let loop ([i 0] [depth 0])
        (if (>= i len)
            #f
            (let ([ch (string-ref word i)])
              (cond
                [(and (= depth 0) (or (char=? ch #\') (char=? ch #\"))) #t]
                [(and (char=? ch #\$)
                      (< (+ i 1) len)
                      (char=? (string-ref word (+ i 1)) #\())
                 (loop (+ i 2) (+ depth 1))]
                [(and (char=? ch #\$)
                      (< (+ i 2) len)
                      (char=? (string-ref word (+ i 1)) #\()
                      (char=? (string-ref word (+ i 2)) #\())
                 (loop (+ i 3) (+ depth 1))]
                [(and (> depth 0) (char=? ch #\())
                 (loop (+ i 1) (+ depth 1))]
                [(and (> depth 0) (char=? ch #\)))
                 (loop (+ i 1) (- depth 1))]
                [(and (= depth 0) (char=? ch #\`))
                 (let skip-bt ([j (+ i 1)])
                   (cond
                     [(>= j len) #f]
                     [(char=? (string-ref word j) #\`) (loop (+ j 1) 0)]
                     [(char=? (string-ref word j) #\\) (skip-bt (+ j 2))]
                     [else (skip-bt (+ j 1))]))]
                [(and (= depth 0) (char=? ch #\\)) (loop (+ i 2) 0)]
                [else (loop (+ i 1) depth)]))))))
  (define (word-has-quoted-at? word)
    (let ([len (string-length word)])
      (let loop ([i 0] [in-dq #f] [depth 0] [brace-depth 0])
        (if (>= i len)
            #f
            (let ([ch (string-ref word i)])
              (cond
                [(char=? ch #\\)
                 (loop (min (+ i 2) len) in-dq depth brace-depth)]
                [(and (char=? ch #\") (= depth 0) (= brace-depth 0))
                 (loop (+ i 1) (not in-dq) depth brace-depth)]
                [(and (= depth 0)
                      (= brace-depth 0)
                      (not in-dq)
                      (char=? ch #\'))
                 (let skip ([j (+ i 1)])
                   (if (or (>= j len) (char=? (string-ref word j) #\'))
                       (loop (+ j 1) in-dq depth brace-depth)
                       (skip (+ j 1))))]
                [(and in-dq
                      (= depth 0)
                      (= brace-depth 0)
                      (char=? ch #\$)
                      (< (+ i 1) len)
                      (char=? (string-ref word (+ i 1)) #\@))
                 #t]
                [(and (char=? ch #\$)
                      (< (+ i 1) len)
                      (char=? (string-ref word (+ i 1)) #\{))
                 (cond
                   [(and in-dq
                         (= depth 0)
                         (= brace-depth 0)
                         (< (+ i 2) len)
                         (char=? (string-ref word (+ i 2)) #\@))
                    #t]
                   [(and in-dq (= depth 0) (= brace-depth 0))
                    (let* ([start (+ i 2)])
                      (let* ([has-bang? (or (and (< start len)
                                                 (char=?
                                                   (string-ref word start)
                                                   #\!))
                                            (and (< (+ start 1) len)
                                                 (char=?
                                                   (string-ref word start)
                                                   #\\)
                                                 (char=?
                                                   (string-ref
                                                     word
                                                     (+ start 1))
                                                   #\!)))])
                        (let* ([name-start (cond
                                             [(not has-bang?) start]
                                             [(char=?
                                                (string-ref word start)
                                                #\!)
                                              (+ start 1)]
                                             [else (+ start 2)])])
                          (let scan ([j name-start])
                            (cond
                              [(>= j len)
                               (loop
                                 (+ i 2)
                                 in-dq
                                 depth
                                 (+ brace-depth 1))]
                              [(and (not has-bang?)
                                    (char=? (string-ref word j) #\[)
                                    (< (+ j 2) len)
                                    (char=? (string-ref word (+ j 1)) #\@)
                                    (char=? (string-ref word (+ j 2)) #\]))
                               (let find-close ([k (+ j 3)] [bd 1])
                                 (cond
                                   [(>= k len)
                                    (loop
                                      (+ i 2)
                                      in-dq
                                      depth
                                      (+ brace-depth 1))]
                                   [(char=? (string-ref word k) #\}) #t]
                                   [else (find-close (+ k 1) bd)]))]
                              [(and has-bang?
                                    (char=? (string-ref word j) #\@)
                                    (< (+ j 1) len)
                                    (char=? (string-ref word (+ j 1)) #\}))
                               #t]
                              [(or (char-alphabetic? (string-ref word j))
                                   (char-numeric? (string-ref word j))
                                   (char=? (string-ref word j) #\_))
                               (scan (+ j 1))]
                              [else
                               (loop
                                 (+ i 2)
                                 in-dq
                                 depth
                                 (+ brace-depth 1))])))))]
                   [else (loop (+ i 2) in-dq depth (+ brace-depth 1))])]
                [(and (> brace-depth 0) (char=? ch #\}))
                 (loop (+ i 1) in-dq depth (- brace-depth 1))]
                [(and (char=? ch #\$)
                      (< (+ i 1) len)
                      (char=? (string-ref word (+ i 1)) #\())
                 (loop (+ i 2) in-dq (+ depth 1) brace-depth)]
                [(and (> depth 0) (char=? ch #\())
                 (loop (+ i 1) in-dq (+ depth 1) brace-depth)]
                [(and (> depth 0) (char=? ch #\)))
                 (loop (+ i 1) in-dq (- depth 1) brace-depth)]
                [(and (= depth 0) (= brace-depth 0) (char=? ch #\`))
                 (let skip ([j (+ i 1)])
                   (cond
                     [(>= j len) (loop j in-dq depth brace-depth)]
                     [(char=? (string-ref word j) #\\) (skip (+ j 2))]
                     [(char=? (string-ref word j) #\`)
                      (loop (+ j 1) in-dq depth brace-depth)]
                     [else (skip (+ j 1))]))]
                [else (loop (+ i 1) in-dq depth brace-depth)]))))))
  (define (split-word-at-quoted-at word)
    (let ([len (string-length word)])
      (let loop ([i 0] [in-dq #f] [depth 0] [brace-depth 0])
        (cond
          [(>= i len) (values word "" #f #f)]
          [(char=? (string-ref word i) #\\)
           (loop (min (+ i 2) len) in-dq depth brace-depth)]
          [(and (char=? (string-ref word i) #\")
                (= depth 0)
                (= brace-depth 0))
           (loop (+ i 1) (not in-dq) depth brace-depth)]
          [(and (= depth 0)
                (= brace-depth 0)
                (not in-dq)
                (char=? (string-ref word i) #\'))
           (let skip ([j (+ i 1)])
             (if (or (>= j len) (char=? (string-ref word j) #\'))
                 (loop (+ j 1) in-dq depth brace-depth)
                 (skip (+ j 1))))]
          [(and in-dq
                (= depth 0)
                (= brace-depth 0)
                (char=? (string-ref word i) #\$)
                (< (+ i 1) len)
                (char=? (string-ref word (+ i 1)) #\@))
           (values
             (string-append (substring word 0 i) "\"")
             (string-append "\"" (substring word (+ i 2) len))
             #f
             #f)]
          [(and (char=? (string-ref word i) #\$)
                (< (+ i 1) len)
                (char=? (string-ref word (+ i 1)) #\{))
           (cond
             [(and in-dq
                   (= depth 0)
                   (= brace-depth 0)
                   (< (+ i 2) len)
                   (char=? (string-ref word (+ i 2)) #\@))
              (if (and (< (+ i 3) len)
                       (char=? (string-ref word (+ i 3)) #\}))
                  (values
                    (string-append (substring word 0 i) "\"")
                    (string-append "\"" (substring word (+ i 4) len))
                    #f
                    #f)
                  (let find-close ([j (+ i 3)] [bd 1])
                    (cond
                      [(>= j len)
                       (loop (+ i 2) in-dq depth (+ brace-depth 1))]
                      [(char=? (string-ref word j) #\{)
                       (find-close (+ j 1) (+ bd 1))]
                      [(char=? (string-ref word j) #\})
                       (if (= bd 1)
                           (let ([at-body (substring word (+ i 3) j)]
                                 [end-pos (+ j 1)])
                             (values
                               (string-append (substring word 0 i) "\"")
                               (string-append
                                 "\""
                                 (substring word end-pos len))
                               #f
                               at-body))
                           (find-close (+ j 1) (- bd 1)))]
                      [else (find-close (+ j 1) bd)])))]
             [(and in-dq (= depth 0) (= brace-depth 0))
              (let* ([start (+ i 2)])
                (let* ([has-bang? (or (and (< start len)
                                           (char=?
                                             (string-ref word start)
                                             #\!))
                                      (and (< (+ start 1) len)
                                           (char=?
                                             (string-ref word start)
                                             #\\)
                                           (char=?
                                             (string-ref word (+ start 1))
                                             #\!)))])
                  (let* ([name-start (cond
                                       [(not has-bang?) start]
                                       [(char=?
                                          (string-ref word start)
                                          #\!)
                                        (+ start 1)]
                                       [else (+ start 2)])])
                    (let scan ([j name-start])
                      (cond
                        [(>= j len)
                         (loop (+ i 2) in-dq depth (+ brace-depth 1))]
                        [(and (not has-bang?)
                              (char=? (string-ref word j) #\[)
                              (< (+ j 2) len)
                              (char=? (string-ref word (+ j 1)) #\@)
                              (char=? (string-ref word (+ j 2)) #\]))
                         (let ([arr-name (substring word name-start j)]
                               [after-bracket (+ j 3)])
                           (if (and (< after-bracket len)
                                    (char=?
                                      (string-ref word after-bracket)
                                      #\}))
                               (values
                                 (string-append (substring word 0 i) "\"")
                                 (string-append
                                   "\""
                                   (substring
                                     word
                                     (+ after-bracket 1)
                                     len))
                                 arr-name
                                 #f)
                               (let find-close ([k after-bracket] [bd 1])
                                 (cond
                                   [(>= k len)
                                    (loop
                                      (+ i 2)
                                      in-dq
                                      depth
                                      (+ brace-depth 1))]
                                   [(char=? (string-ref word k) #\})
                                    (let ([at-body (substring
                                                     word
                                                     after-bracket
                                                     k)]
                                          [end-pos (+ k 1)])
                                      (values
                                        (string-append
                                          (substring word 0 i)
                                          "\"")
                                        (string-append
                                          "\""
                                          (substring word end-pos len))
                                        arr-name
                                        at-body))]
                                   [else (find-close (+ k 1) bd)]))))]
                        [(and has-bang?
                              (char=? (string-ref word j) #\@)
                              (< (+ j 1) len)
                              (char=? (string-ref word (+ j 1)) #\}))
                         (let ([prefix-name (string-append
                                              "!"
                                              (substring
                                                word
                                                name-start
                                                j))]
                               [end-pos (+ j 2)])
                           (values
                             (string-append (substring word 0 i) "\"")
                             (string-append
                               "\""
                               (substring word end-pos len))
                             prefix-name
                             #f))]
                        [(or (char-alphabetic? (string-ref word j))
                             (char-numeric? (string-ref word j))
                             (char=? (string-ref word j) #\_))
                         (scan (+ j 1))]
                        [else
                         (loop
                           (+ i 2)
                           in-dq
                           depth
                           (+ brace-depth 1))])))))]
             [else (loop (+ i 2) in-dq depth (+ brace-depth 1))])]
          [(and (> brace-depth 0) (char=? (string-ref word i) #\}))
           (loop (+ i 1) in-dq depth (- brace-depth 1))]
          [(and (char=? (string-ref word i) #\$)
                (< (+ i 1) len)
                (char=? (string-ref word (+ i 1)) #\())
           (loop (+ i 2) in-dq (+ depth 1) brace-depth)]
          [(and (> depth 0) (char=? (string-ref word i) #\())
           (loop (+ i 1) in-dq (+ depth 1) brace-depth)]
          [(and (> depth 0) (char=? (string-ref word i) #\)))
           (loop (+ i 1) in-dq (- depth 1) brace-depth)]
          [(and (= depth 0)
                (= brace-depth 0)
                (char=? (string-ref word i) #\`))
           (let skip ([j (+ i 1)])
             (cond
               [(>= j len) (loop j in-dq depth brace-depth)]
               [(char=? (string-ref word j) #\\) (skip (+ j 2))]
               [(char=? (string-ref word j) #\`)
                (loop (+ j 1) in-dq depth brace-depth)]
               [else (skip (+ j 1))]))]
          [else (loop (+ i 1) in-dq depth brace-depth)]))))
  (define (expand-word-parts-split word-str env)
    (let* ([segments (expand-string-segments word-str env)])
      (let* ([split (split-expanded-segments segments env)])
        (map car split))))
  (define (expand-word-with-at word env)
    (let-values ([(prefix-raw suffix-raw array-name at-body)
                  (split-word-at-quoted-at word)])
      (let* ([all-elements (cond
                             [(and array-name
                                   (> (string-length array-name) 0)
                                   (char=? (string-ref array-name 0) #\!))
                              (env-matching-names
                                env
                                (substring
                                  array-name
                                  1
                                  (string-length array-name)))]
                             [array-name (env-array-values env array-name)]
                             [else (env-positional-list env)])])
        (let* ([elements (cond
                           [(not at-body) all-elements]
                           [(and (char=? (string-ref at-body 0) #\:)
                                 (or (<= (string-length at-body) 1)
                                     (not (memv
                                            (string-ref at-body 1)
                                            '(#\- #\+ #\= #\?)))))
                            (let* ([spec (substring
                                           at-body
                                           1
                                           (string-length at-body))])
                              (let* ([colon-pos (string-find-char-from
                                                  spec
                                                  #\:
                                                  0)])
                                (let* ([offset-str (if colon-pos
                                                       (substring
                                                         spec
                                                         0
                                                         colon-pos)
                                                       spec)])
                                  (let* ([length-str (if colon-pos
                                                         (substring
                                                           spec
                                                           (+ colon-pos 1)
                                                           (string-length
                                                             spec))
                                                         #f)])
                                    (let* ([offset (slice-arith-eval
                                                     offset-str
                                                     env)])
                                      (let* ([full-list (if (<= offset 0)
                                                            (cons
                                                              (shell-environment-shell-name
                                                                env)
                                                              all-elements)
                                                            all-elements)])
                                        (let* ([slen (length full-list)])
                                          (let* ([start (cond
                                                          [(= offset 0) 0]
                                                          [(< offset 0)
                                                           (max 0
                                                                (+ slen
                                                                   offset))]
                                                          [else
                                                           (- offset 1)])])
                                            (let* ([start (max 0
                                                               (min start
                                                                    slen))])
                                              (if length-str
                                                  (let ([ln (slice-arith-eval
                                                              length-str
                                                              env)])
                                                    (if (< ln 0)
                                                        (let ([end (max start
                                                                        (+ slen
                                                                           ln))])
                                                          (take-sublist
                                                            full-list
                                                            start
                                                            end))
                                                        (take-sublist
                                                          full-list
                                                          start
                                                          (min slen
                                                               (+ start
                                                                  ln)))))
                                                  (take-sublist
                                                    full-list
                                                    start
                                                    slen)))))))))))]
                           [else
                            (let-values ([(mname modifier arg)
                                          (parse-parameter-modifier
                                            (string-append "@" at-body))])
                              (if modifier
                                  (if (memq
                                        modifier
                                        '(- :- + :+ = := ? :?))
                                      (let* ([is-empty (null?
                                                         all-elements)])
                                        (let* ([is-null (or is-empty
                                                            (and (memq
                                                                   modifier
                                                                   '(:- :+
                                                                        :=
                                                                        :?))
                                                                 (= (length
                                                                      all-elements)
                                                                    1)
                                                                 (string=?
                                                                   (car all-elements)
                                                                   "")))])
                                          (case modifier
                                            [(-)
                                             (if is-empty
                                                 (list
                                                   (parameterize ([*in-dquote-context*
                                                                   #t])
                                                     (expand-string
                                                       arg
                                                       env)))
                                                 all-elements)]
                                            [(:-)
                                             (if is-null
                                                 (list
                                                   (parameterize ([*in-dquote-context*
                                                                   #t])
                                                     (expand-string
                                                       arg
                                                       env)))
                                                 all-elements)]
                                            [(+)
                                             (if is-empty
                                                 '()
                                                 (list
                                                   (parameterize ([*in-dquote-context*
                                                                   #t])
                                                     (expand-string
                                                       arg
                                                       env))))]
                                            [(:+)
                                             (if is-null
                                                 '()
                                                 (list
                                                   (parameterize ([*in-dquote-context*
                                                                   #t])
                                                     (expand-string
                                                       arg
                                                       env))))]
                                            [else
                                             (let* ([val (if is-empty
                                                             #f
                                                             (string-join-with
                                                               " "
                                                               all-elements))])
                                               (let* ([result (parameterize ([*in-dquote-context*
                                                                              #t])
                                                                (apply-parameter-modifier val
                                                                  (or array-name
                                                                      "@")
                                                                  modifier
                                                                  arg
                                                                  env))])
                                                 (if result
                                                     (list result)
                                                     '())))])))
                                      (let ([attr-name (or array-name
                                                           "@")])
                                        (map (lambda (p)
                                               (apply-parameter-modifier p attr-name modifier arg
                                                 env))
                                             all-elements)))
                                  all-elements))])])
          (let* ([prefix-words (expand-word-parts-split
                                 prefix-raw
                                 env)])
            (let* ([suffix-words (if (word-has-quoted-at? suffix-raw)
                                     (expand-word-with-at suffix-raw env)
                                     (expand-word-parts-split
                                       suffix-raw
                                       env))])
              (let ([butlast (lambda (lst)
                               (let loop ([l lst] [acc (list)])
                                 (if (null? (cdr l))
                                     (reverse acc)
                                     (loop (cdr l) (cons (car l) acc)))))])
                (cond
                  [(null? elements)
                   (cond
                     [(and (null? prefix-words) (null? suffix-words))
                      (list)]
                     [(null? suffix-words)
                      (let ([combined (apply string-append prefix-words)])
                        (if (string=? combined "") (list) prefix-words))]
                     [(null? prefix-words)
                      (let ([combined (apply string-append suffix-words)])
                        (if (string=? combined "") (list) suffix-words))]
                     [else
                      (let* ([pfx-init (butlast prefix-words)])
                        (let* ([pfx-last (last prefix-words)])
                          (let* ([sfx-first (car suffix-words)])
                            (let* ([sfx-rest (cdr suffix-words)])
                              (let* ([joined (string-append
                                               pfx-last
                                               sfx-first)])
                                (let* ([result (append
                                                 pfx-init
                                                 (list joined)
                                                 sfx-rest)])
                                  (let* ([combined (apply
                                                     string-append
                                                     result)])
                                    (if (string=? combined "")
                                        (list)
                                        result))))))))])]
                  [else
                   (let* ([pfx-last (if (null? prefix-words)
                                        ""
                                        (last prefix-words))])
                     (let* ([pfx-rest (if (or (null? prefix-words)
                                              (null? (cdr prefix-words)))
                                          (list)
                                          (butlast prefix-words))])
                       (let* ([sfx-first (if (null? suffix-words)
                                             ""
                                             (car suffix-words))])
                         (let* ([sfx-rest (if (or (null? suffix-words)
                                                  (null?
                                                    (cdr suffix-words)))
                                              (list)
                                              (cdr suffix-words))])
                           (let* ([n (length elements)])
                             (if (= n 1)
                                 (append
                                   pfx-rest
                                   (list
                                     (string-append
                                       pfx-last
                                       (car elements)
                                       sfx-first))
                                   sfx-rest)
                                 (let* ([first-elem (string-append
                                                      pfx-last
                                                      (car elements))])
                                   (let* ([last-elem (string-append
                                                       (last elements)
                                                       sfx-first)])
                                     (let* ([middle (let mid-loop ([ps (cdr elements)]
                                                                   [acc (list)])
                                                      (if (null? (cdr ps))
                                                          (reverse acc)
                                                          (mid-loop
                                                            (cdr ps)
                                                            (cons
                                                              (car ps)
                                                              acc))))])
                                       (append pfx-rest (list first-elem)
                                         middle (list last-elem)
                                         sfx-rest))))))))))]))))))))
  (define (read-single-quote str i)
    (let ([len (string-length str)])
      (let loop ([j i] [buf (open-output-string)])
        (cond
          [(>= j len) (values (get-output-string buf) j)]
          [(char=? (string-ref str j) #\')
           (values (get-output-string buf) (+ j 1))]
          [else
           (display (string-ref str j) buf)
           (loop (+ j 1) buf)]))))
  (define (expand-double-quote str i env)
    (let ([len (string-length str)] [buf (open-output-string)])
      (let loop ([j i])
        (cond
          [(>= j len) (values (get-output-string buf) j)]
          [(char=? (string-ref str j) #\")
           (values (get-output-string buf) (+ j 1))]
          [(char=? (string-ref str j) #\\)
           (if (< (+ j 1) len)
               (let ([next (string-ref str (+ j 1))])
                 (if (memq next '(#\$ #\` #\" #\\ #\newline))
                     (begin (display next buf) (loop (+ j 2)))
                     (begin
                       (display "\\" buf)
                       (display next buf)
                       (loop (+ j 2)))))
               (begin (display "\\" buf) (loop (+ j 1))))]
          [(char=? (string-ref str j) #\$)
           (let-values ([(expanded end)
                         (parameterize ([*in-dquote-context* #t])
                           (expand-dollar str j env))])
             (display
               (if (modifier-segments? expanded)
                   (segments->string (modifier-segments-list expanded))
                   expanded)
               buf)
             (loop end))]
          [(char=? (string-ref str j) #\`)
           (let-values ([(expanded end)
                         (expand-backtick str j env #t)])
             (display expanded buf)
             (loop end))]
          [else (display (string-ref str j) buf) (loop (+ j 1))]))))
  (define (find-matching-brace str start)
    (let ([len (string-length str)])
      (let loop ([i start] [depth 1])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) #\{) (loop (+ i 1) (+ depth 1))]
          [(char=? (string-ref str i) #\})
           (if (= depth 1) i (loop (+ i 1) (- depth 1)))]
          [(char=? (string-ref str i) #\\) (loop (+ i 2) depth)]
          [(char=? (string-ref str i) #\')
           (let sq ([j (+ i 1)])
             (cond
               [(>= j len) (loop j depth)]
               [(char=? (string-ref str j) #\') (loop (+ j 1) depth)]
               [else (sq (+ j 1))]))]
          [(char=? (string-ref str i) #\")
           (let dq ([j (+ i 1)])
             (cond
               [(>= j len) (loop j depth)]
               [(char=? (string-ref str j) #\\) (dq (+ j 2))]
               [(char=? (string-ref str j) #\") (loop (+ j 1) depth)]
               [else (dq (+ j 1))]))]
          [else (loop (+ i 1) depth)]))))
  (define (find-matching-paren str start)
    (let ([len (string-length str)])
      (define (update-case-depth w cd)
        (cond
          [(string=? w "case") (+ cd 1)]
          [(string=? w "esac") (max 0 (- cd 1))]
          [else cd]))
      (let loop ([i start] [depth 1] [case-depth 0] [word ""])
        (cond
          [(>= i len) (- len 1)]
          [(char=? (string-ref str i) #\()
           (let ([cd (update-case-depth word case-depth)])
             (loop (+ i 1) (+ depth 1) cd ""))]
          [(char=? (string-ref str i) #\))
           (let ([cd (update-case-depth word case-depth)])
             (if (and (= depth 1) (> cd 0))
                 (loop (+ i 1) depth cd "")
                 (if (= depth 1) i (loop (+ i 1) (- depth 1) cd ""))))]
          [(char=? (string-ref str i) #\\)
           (let ([cd (update-case-depth word case-depth)])
             (loop (+ i 2) depth cd ""))]
          [(char=? (string-ref str i) #\')
           (let ([cd (update-case-depth word case-depth)])
             (let sq ([j (+ i 1)])
               (cond
                 [(>= j len) (loop j depth cd "")]
                 [(char=? (string-ref str j) #\')
                  (loop (+ j 1) depth cd "")]
                 [else (sq (+ j 1))])))]
          [(char=? (string-ref str i) #\")
           (let ([cd (update-case-depth word case-depth)])
             (let dq ([j (+ i 1)])
               (cond
                 [(>= j len) (loop j depth cd "")]
                 [(char=? (string-ref str j) #\\) (dq (+ j 2))]
                 [(char=? (string-ref str j) #\")
                  (loop (+ j 1) depth cd "")]
                 [else (dq (+ j 1))])))]
          [(or (char-alphabetic? (string-ref str i))
               (char=? (string-ref str i) #\_))
           (loop
             (+ i 1)
             depth
             case-depth
             (string-append word (string (string-ref str i))))]
          [else
           (let ([cd (update-case-depth word case-depth)])
             (loop (+ i 1) depth cd ""))]))))
  (define (find-arith-close str start)
    (let ([len (string-length str)])
      (let loop ([i start] [paren-depth 0])
        (cond
          [(>= i len) (- len 2)]
          [(and (= paren-depth 0)
                (char=? (string-ref str i) #\))
                (< (+ i 1) len)
                (char=? (string-ref str (+ i 1)) #\)))
           i]
          [(and (< (+ i 2) len)
                (char=? (string-ref str i) #\$)
                (char=? (string-ref str (+ i 1)) #\()
                (char=? (string-ref str (+ i 2)) #\())
           (let ([inner-close (find-arith-close str (+ i 3))])
             (loop (+ inner-close 2) paren-depth))]
          [(and (< (+ i 1) len)
                (char=? (string-ref str i) #\$)
                (char=? (string-ref str (+ i 1)) #\())
           (let ([close (find-matching-paren str (+ i 2))])
             (loop (+ close 1) paren-depth))]
          [(char=? (string-ref str i) #\()
           (loop (+ i 1) (+ paren-depth 1))]
          [(and (char=? (string-ref str i) #\)) (> paren-depth 0))
           (loop (+ i 1) (- paren-depth 1))]
          [else (loop (+ i 1) paren-depth)]))))
  (define case-convert-matching
    (case-lambda
      [(val pattern convert-fn all?)
       (let* ([extglob? #f])
         (let* ([len (string-length val)])
           (let* ([buf (open-output-string)])
             (let loop ([i 0] [converted? #f])
               (if (>= i len)
                   (get-output-string buf)
                   (let* ([ch (string-ref val i)])
                     (let* ([ch-str (string ch)])
                       (let* ([matches? (glob-match?
                                          pattern
                                          ch-str
                                          #f
                                          extglob?)])
                         (if (and matches? (or all? (not converted?)))
                             (begin
                               (display (convert-fn ch) buf)
                               (loop (+ i 1) #t))
                             (begin
                               (display ch buf)
                               (loop (+ i 1) converted?)))))))))))]
      [(val pattern convert-fn all? extglob?)
       (let* ([len (string-length val)])
         (let* ([buf (open-output-string)])
           (let loop ([i 0] [converted? #f])
             (if (>= i len)
                 (get-output-string buf)
                 (let* ([ch (string-ref val i)])
                   (let* ([ch-str (string ch)])
                     (let* ([matches? (glob-match?
                                        pattern
                                        ch-str
                                        #f
                                        extglob?)])
                       (if (and matches? (or all? (not converted?)))
                           (begin
                             (display (convert-fn ch) buf)
                             (loop (+ i 1) #t))
                           (begin
                             (display ch buf)
                             (loop (+ i 1) converted?))))))))))]))
  (define remove-suffix
    (case-lambda
      [(val pattern longest?)
       (let* ([extglob? #f])
         (if longest?
             (let loop ([i 0])
               (if (> i (string-length val))
                   val
                   (if (glob-match?
                         pattern
                         (substring val i (string-length val))
                         #f
                         extglob?)
                       (substring val 0 i)
                       (loop (+ i 1)))))
             (let loop ([i (string-length val)])
               (if (< i 0)
                   val
                   (if (glob-match?
                         pattern
                         (substring val i (string-length val))
                         #f
                         extglob?)
                       (substring val 0 i)
                       (loop (- i 1)))))))]
      [(val pattern longest? extglob?)
       (if longest?
           (let loop ([i 0])
             (if (> i (string-length val))
                 val
                 (if (glob-match?
                       pattern
                       (substring val i (string-length val))
                       #f
                       extglob?)
                     (substring val 0 i)
                     (loop (+ i 1)))))
           (let loop ([i (string-length val)])
             (if (< i 0)
                 val
                 (if (glob-match?
                       pattern
                       (substring val i (string-length val))
                       #f
                       extglob?)
                     (substring val 0 i)
                     (loop (- i 1))))))]))
  (define remove-prefix
    (case-lambda
      [(val pattern longest?)
       (let* ([extglob? #f])
         (if longest?
             (let loop ([i (string-length val)])
               (if (< i 0)
                   val
                   (if (glob-match?
                         pattern
                         (substring val 0 i)
                         #f
                         extglob?)
                       (substring val i (string-length val))
                       (loop (- i 1)))))
             (let loop ([i 0])
               (if (> i (string-length val))
                   val
                   (if (glob-match?
                         pattern
                         (substring val 0 i)
                         #f
                         extglob?)
                       (substring val i (string-length val))
                       (loop (+ i 1)))))))]
      [(val pattern longest? extglob?)
       (if longest?
           (let loop ([i (string-length val)])
             (if (< i 0)
                 val
                 (if (glob-match? pattern (substring val 0 i) #f extglob?)
                     (substring val i (string-length val))
                     (loop (- i 1)))))
           (let loop ([i 0])
             (if (> i (string-length val))
                 val
                 (if (glob-match? pattern (substring val 0 i) #f extglob?)
                     (substring val i (string-length val))
                     (loop (+ i 1))))))]))
  (define (string-trim-trailing-newlines str)
    (let loop ([i (- (string-length str) 1)])
      (if (and (>= i 0) (char=? (string-ref str i) #\newline))
          (loop (- i 1))
          (substring str 0 (+ i 1)))))
  (define (read-all-string port)
    (let ([buf (open-output-string)])
      (let loop ()
        (let ([ch (read-char port)])
          (if (eof-object? ch)
              (get-output-string buf)
              (begin (display ch buf) (loop)))))))
  (define (append-map f lst) (apply append (map f lst)))
  (define (string-find-char-from str ch start)
    (let loop ([i start])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (string-trim-whitespace-str str)
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
  (define (brace-expand word)
    (let ([result (brace-expand-once word)])
      (if (and (= (length result) 1) (string=? (car result) word))
          result
          (append-map brace-expand result))))
  (define (brace-expand-once word)
    (let ([len (string-length word)])
      (let find-open ([i 0])
        (cond
          [(>= i len) (list word)]
          [(char=? (string-ref word i) #\')
           (let skip ([j (+ i 1)])
             (cond
               [(>= j len) (list word)]
               [(char=? (string-ref word j) #\') (find-open (+ j 1))]
               [else (skip (+ j 1))]))]
          [(char=? (string-ref word i) #\")
           (let skip ([j (+ i 1)])
             (cond
               [(>= j len) (list word)]
               [(char=? (string-ref word j) #\") (find-open (+ j 1))]
               [(char=? (string-ref word j) #\\) (skip (+ j 2))]
               [else (skip (+ j 1))]))]
          [(and (char=? (string-ref word i) #\$)
                (< (+ i 1) len)
                (char=? (string-ref word (+ i 1)) #\{))
           (let skip ([j (+ i 2)] [depth 1])
             (cond
               [(>= j len) (list word)]
               [(char=? (string-ref word j) #\})
                (if (= depth 1)
                    (find-open (+ j 1))
                    (skip (+ j 1) (- depth 1)))]
               [(char=? (string-ref word j) #\{)
                (skip (+ j 1) (+ depth 1))]
               [(char=? (string-ref word j) #\\) (skip (+ j 2) depth)]
               [else (skip (+ j 1) depth)]))]
          [(char=? (string-ref word i) #\\) (find-open (+ i 2))]
          [(char=? (string-ref word i) #\{)
           (let ([close (find-brace-close word (+ i 1))])
             (if close
                 (let ([content (substring word (+ i 1) close)]
                       [preamble (substring word 0 i)]
                       [postscript (substring word (+ close 1) len)])
                   (cond
                     [(brace-has-comma? content)
                      (let ([parts (brace-split-commas content)])
                        (map (lambda (part)
                               (string-append preamble part postscript))
                             parts))]
                     [(brace-sequence? content)
                      (let ([seq (brace-expand-sequence content)])
                        (if seq
                            (map (lambda (item)
                                   (string-append
                                     preamble
                                     item
                                     postscript))
                                 seq)
                            (find-open (+ close 1))))]
                     [else (find-open (+ close 1))]))
                 (list word)))]
          [else (find-open (+ i 1))]))))
  (define (find-brace-close str start)
    (let ([len (string-length str)])
      (let loop ([i start] [depth 1])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) #\\) (loop (+ i 2) depth)]
          [(char=? (string-ref str i) #\')
           (let skip ([j (+ i 1)])
             (cond
               [(>= j len) #f]
               [(char=? (string-ref str j) #\') (loop (+ j 1) depth)]
               [else (skip (+ j 1))]))]
          [(char=? (string-ref str i) #\")
           (let skip ([j (+ i 1)])
             (cond
               [(>= j len) #f]
               [(char=? (string-ref str j) #\") (loop (+ j 1) depth)]
               [(char=? (string-ref str j) #\\) (skip (+ j 2))]
               [else (skip (+ j 1))]))]
          [(char=? (string-ref str i) #\{) (loop (+ i 1) (+ depth 1))]
          [(char=? (string-ref str i) #\})
           (if (= depth 1) i (loop (+ i 1) (- depth 1)))]
          [else (loop (+ i 1) depth)]))))
  (define (brace-has-comma? content)
    (let ([len (string-length content)])
      (let loop ([i 0] [depth 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref content i) #\\) (loop (+ i 2) depth)]
          [(char=? (string-ref content i) #\{)
           (loop (+ i 1) (+ depth 1))]
          [(char=? (string-ref content i) #\})
           (loop (+ i 1) (- depth 1))]
          [(and (char=? (string-ref content i) #\,) (= depth 0)) #t]
          [else (loop (+ i 1) depth)]))))
  (define (brace-split-commas content)
    (let ([len (string-length content)])
      (let loop ([i 0] [start 0] [depth 0] [parts (list)])
        (cond
          [(>= i len)
           (reverse (cons (substring content start i) parts))]
          [(char=? (string-ref content i) #\\)
           (loop (+ i 2) start depth parts)]
          [(char=? (string-ref content i) #\{)
           (loop (+ i 1) start (+ depth 1) parts)]
          [(char=? (string-ref content i) #\})
           (loop (+ i 1) start (- depth 1) parts)]
          [(and (char=? (string-ref content i) #\,) (= depth 0))
           (loop
             (+ i 1)
             (+ i 1)
             depth
             (cons (substring content start i) parts))]
          [else (loop (+ i 1) start depth parts)]))))
  (define (brace-sequence? content)
    (and (string-contains? content "..")
         (not (brace-has-comma? content))))
  (define (brace-expand-sequence content)
    (let* ([parts (string-split-dot-dot content)])
      (let* ([nparts (length parts)])
        (cond
          [(and (= nparts 2)
                (> (string-length (car parts)) 0)
                (> (string-length (cadr parts)) 0))
           (brace-range (car parts) (cadr parts) #f)]
          [(and (= nparts 3)
                (> (string-length (car parts)) 0)
                (> (string-length (cadr parts)) 0)
                (> (string-length (caddr parts)) 0))
           (brace-range (car parts) (cadr parts) (caddr parts))]
          [else #f]))))
  (define (string-split-dot-dot str)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [parts (list)])
        (cond
          [(>= i len) (reverse (cons (substring str start i) parts))]
          [(and (< (+ i 1) len)
                (char=? (string-ref str i) #\.)
                (char=? (string-ref str (+ i 1)) #\.))
           (loop (+ i 2) (+ i 2) (cons (substring str start i) parts))]
          [else (loop (+ i 1) start parts)]))))
  (define (brace-parse-int str)
    (let ([len (string-length str)])
      (and (> len 0)
           (let* ([start (if (and (> len 1)
                                  (char=? (string-ref str 0) #\-))
                             1
                             0)])
             (let* ([has-digit? #f])
               (let loop ([i start])
                 (if (>= i len)
                     (and has-digit? (string->number str))
                     (if (char-numeric? (string-ref str i))
                         (begin (set! has-digit? #t) (loop (+ i 1)))
                         #f))))))))
  (define (brace-range start-str end-str step-str)
    (let ([start-num (brace-parse-int start-str)]
          [end-num (brace-parse-int end-str)]
          [step-num (and step-str (brace-parse-int step-str))])
      (cond
        [(and start-num end-num)
         (let* ([raw-step (or (and step-num
                                   (if (= step-num 0)
                                       (if (<= start-num end-num) 1 -1)
                                       step-num))
                              (if (< end-num start-num) -1 1))])
           (let* ([step (if (<= start-num end-num)
                            (abs raw-step)
                            (- (abs raw-step)))])
             (let* ([pad-width (max (string-length start-str)
                                    (string-length end-str))])
               (let* ([needs-pad? (or (and (> (string-length start-str) 1)
                                           (char=?
                                             (string-ref start-str 0)
                                             #\0))
                                      (and (> (string-length end-str) 1)
                                           (char=?
                                             (string-ref end-str 0)
                                             #\0)))])
                 (cond
                   [(= step 0) #f]
                   [else
                    (let loop ([i start-num] [result (list)])
                      (if (if (> step 0) (> i end-num) (< i end-num))
                          (reverse result)
                          (loop
                            (+ i step)
                            (cons
                              (if needs-pad?
                                  (pad-number i pad-width)
                                  (number->string i))
                              result))))])))))]
        [(and (= (string-length start-str) 1)
              (= (string-length end-str) 1)
              (char-alphabetic? (string-ref start-str 0))
              (char-alphabetic? (string-ref end-str 0))
              (eq? (char-upper-case? (string-ref start-str 0))
                   (char-upper-case? (string-ref end-str 0))))
         (let* ([start-ch (char->integer (string-ref start-str 0))])
           (let* ([end-ch (char->integer (string-ref end-str 0))])
             (let* ([raw-step (or (and step-num
                                       (let ([s (inexact->exact step-num)])
                                         (if (= s 0)
                                             (if (<= start-ch end-ch) 1 -1)
                                             s)))
                                  (if (<= start-ch end-ch) 1 -1))])
               (let* ([step (if (<= start-ch end-ch)
                                (abs raw-step)
                                (- (abs raw-step)))])
                 (cond
                   [(= step 0) #f]
                   [else
                    (let loop ([i start-ch] [result (list)])
                      (if (if (> step 0) (> i end-ch) (< i end-ch))
                          (reverse result)
                          (loop
                            (+ i step)
                            (cons
                              (string (integer->char i))
                              result))))])))))]
        [else #f])))
  (define (pad-number n width)
    (let* ([s (number->string (abs n))])
      (let* ([padding (max 0
                           (- width (string-length s) (if (< n 0) 1 0)))])
        (string-append
          (if (< n 0) "-" "")
          (make-string padding #\0)
          s)))))
