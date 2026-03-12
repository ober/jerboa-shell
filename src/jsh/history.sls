#!chezscheme
(library (jsh history)
  (export make-history-entry history-entry-timestamp
   history-entry-cwd history-entry-command history-state::t
   make-history-state history-state? history-state-entries
   history-state-count history-state-max-size
   history-state-file history-state-file-max
   history-state-control history-state-ignore-pats
   history-state-entries-set! history-state-count-set!
   history-state-max-size-set! history-state-file-set!
   history-state-file-max-set! history-state-control-set!
   history-state-ignore-pats-set! &history-state-entries
   &history-state-count &history-state-max-size
   &history-state-file &history-state-file-max
   &history-state-control &history-state-ignore-pats
   &history-state-entries-set! &history-state-count-set!
   &history-state-max-size-set! &history-state-file-set!
   &history-state-file-max-set! &history-state-control-set!
   &history-state-ignore-pats-set! *history* *last-subst*
   history-init! history-add! history-get-entry history-get
   history-get-entry-relative history-get-relative
   history-search history-search-reverse history-entries
   history-list history-count history-clear!
   history-split-words select-words apply-history-modifier
   parse-event find-event-end parse-word-designator
   parse-modifier parse-substitution parse-history-ref
   history-expand expand-quick-subst history-save!
   history-load! history-add-raw! history-set-control!
   history-should-ignore? history-unique-commands
   find-number-end find-word-end string-index-of
   string-replace-first string-replace-all string-search
   string-search-from string-trim)
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
    (except (std misc string) string-trim string-join
      string-split string-index string-empty?)
    (except (std misc list) take drop filter-map)
    (except (std misc alist) pget pgetv pgetq aget agetv agetq)
    (except
      (std os path)
      path-expand
      path-normalize
      path-absolute?)
    (except (std format) format) (std sort) (std pregexp)
    (std sugar)
    ;; Jerboa transducers for efficient history search pipelines
    (std transducer)
    (except (jsh pregexp-compat) pregexp-quote pregexp-replace*
      pregexp-replace pregexp-split pregexp-match
      pregexp-match-positions pregexp)
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase))
  (define (make-history-entry timestamp cwd command)
    (vector timestamp cwd command))
  (define (history-entry-timestamp entry)
    (vector-ref entry 0))
  (define (history-entry-cwd entry) (vector-ref entry 1))
  (define (history-entry-command entry) (vector-ref entry 2))
  (begin
    (define history-state::t
      (make-class-type 'gerbil\x23;history-state::t 'history-state
        (list object::t)
        '(entries count max-size file file-max control ignore-pats)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-history-state . args)
      (let* ([type history-state::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (history-state? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;history-state::t))
    (define (history-state-entries obj)
      (unchecked-slot-ref obj 'entries))
    (define (history-state-count obj)
      (unchecked-slot-ref obj 'count))
    (define (history-state-max-size obj)
      (unchecked-slot-ref obj 'max-size))
    (define (history-state-file obj)
      (unchecked-slot-ref obj 'file))
    (define (history-state-file-max obj)
      (unchecked-slot-ref obj 'file-max))
    (define (history-state-control obj)
      (unchecked-slot-ref obj 'control))
    (define (history-state-ignore-pats obj)
      (unchecked-slot-ref obj 'ignore-pats))
    (define (history-state-entries-set! obj val)
      (unchecked-slot-set! obj 'entries val))
    (define (history-state-count-set! obj val)
      (unchecked-slot-set! obj 'count val))
    (define (history-state-max-size-set! obj val)
      (unchecked-slot-set! obj 'max-size val))
    (define (history-state-file-set! obj val)
      (unchecked-slot-set! obj 'file val))
    (define (history-state-file-max-set! obj val)
      (unchecked-slot-set! obj 'file-max val))
    (define (history-state-control-set! obj val)
      (unchecked-slot-set! obj 'control val))
    (define (history-state-ignore-pats-set! obj val)
      (unchecked-slot-set! obj 'ignore-pats val))
    (define (&history-state-entries obj)
      (unchecked-slot-ref obj 'entries))
    (define (&history-state-count obj)
      (unchecked-slot-ref obj 'count))
    (define (&history-state-max-size obj)
      (unchecked-slot-ref obj 'max-size))
    (define (&history-state-file obj)
      (unchecked-slot-ref obj 'file))
    (define (&history-state-file-max obj)
      (unchecked-slot-ref obj 'file-max))
    (define (&history-state-control obj)
      (unchecked-slot-ref obj 'control))
    (define (&history-state-ignore-pats obj)
      (unchecked-slot-ref obj 'ignore-pats))
    (define (&history-state-entries-set! obj val)
      (unchecked-slot-set! obj 'entries val))
    (define (&history-state-count-set! obj val)
      (unchecked-slot-set! obj 'count val))
    (define (&history-state-max-size-set! obj val)
      (unchecked-slot-set! obj 'max-size val))
    (define (&history-state-file-set! obj val)
      (unchecked-slot-set! obj 'file val))
    (define (&history-state-file-max-set! obj val)
      (unchecked-slot-set! obj 'file-max val))
    (define (&history-state-control-set! obj val)
      (unchecked-slot-set! obj 'control val))
    (define (&history-state-ignore-pats-set! obj val)
      (unchecked-slot-set! obj 'ignore-pats val)))
  (define *history*-cell (vector #f))
  (define-syntax *history*
    (identifier-syntax
      [id (vector-ref *history*-cell 0)]
      [(set! id v) (vector-set! *history*-cell 0 v)]))
  (define *last-subst*-cell (vector #f))
  (define-syntax *last-subst*
    (identifier-syntax
      [id (vector-ref *last-subst*-cell 0)]
      [(set! id v) (vector-set! *last-subst*-cell 0 v)]))
  (define history-init!
    (case-lambda
      [(histfile histsize)
       (let* ([filesize #f])
         (set! *history*
           (make-history-state (make-vector (or histsize 1000) #f) 0 (or histsize 1000)
             (expand-tilde (or histfile "~/.jsh_history"))
             (or filesize (* 2 (or histsize 1000))) (list) (list)))
         (history-load!))]
      [(histfile histsize filesize)
       (set! *history*
         (make-history-state (make-vector (or histsize 1000) #f) 0 (or histsize 1000)
           (expand-tilde (or histfile "~/.jsh_history"))
           (or filesize (* 2 (or histsize 1000))) (list) (list)))
       (history-load!)]))
  (define (history-add! line)
    (when (and *history*
               (> (string-length (string-trim line)) 0))
      (let ([trimmed (string-trim line)])
        (unless (history-should-ignore? trimmed)
          (let* ([h *history*])
            (let* ([idx (modulo
                          (history-state-count h)
                          (history-state-max-size h))])
              (let* ([entry (make-history-entry
                              (inexact->exact (floor (current-second)))
                              (guard (__exn [#t ((lambda (e) "") __exn)])
                                (current-directory))
                              trimmed)])
                (vector-set! (history-state-entries h) idx entry)
                (history-state-count-set!
                  h
                  (+ 1 (history-state-count h))))))))))
  (define (history-get-entry n)
    (when *history*
      (let* ([h *history*])
        (let* ([total (history-state-count h)])
          (let* ([max-sz (history-state-max-size h)])
            (if (and (> n 0) (<= n total))
                (let ([raw (vector-ref
                             (history-state-entries h)
                             (modulo (- n 1) max-sz))])
                  (and raw
                       (if (vector? raw)
                           raw
                           (make-history-entry 0 "" raw))))
                #f))))))
  (define (history-get n)
    (let ([entry (history-get-entry n)])
      (and entry (history-entry-command entry))))
  (define (history-get-entry-relative offset)
    (when *history*
      (let* ([h *history*])
        (let* ([total (history-state-count h)])
          (history-get-entry (- total offset))))))
  (define (history-get-relative offset)
    (let ([entry (history-get-entry-relative offset)])
      (and entry (history-entry-command entry))))
  ;; history-search: transducer-based prefix search with deduplication.
  ;; (deduplicate) removes consecutive dups only; use a seen hashtable for
  ;; full dedup across the entire history list. Composes as a transducer pipeline:
  ;;   filter by prefix → seen-set dedup → take up to 200
  (define (history-search prefix)
    (if (not *history*)
        (list)
        (let ([plen (string-length prefix)]
              [seen (make-hashtable string-hash string=?)])
          (sequence
            (compose-transducers
              (filtering (lambda (cmd)
                (and (>= (string-length cmd) plen)
                     (string=? (substring cmd 0 plen) prefix))))
              (filtering (lambda (cmd)
                (if (hashtable-ref seen cmd #f)
                  #f
                  (begin (hashtable-set! seen cmd #t) #t))))
              (taking 200))
            (history-list)))))
  (define (history-search-reverse substr)
    (if (not *history*)
        #f
        (let* ([h *history*])
          (let* ([total (history-state-count h)])
            (let* ([max-sz (history-state-max-size h)])
              (let* ([start (min total max-sz)])
                (let loop ([i (- start 1)])
                  (if (< i 0)
                      #f
                      (let* ([idx (modulo
                                    (- total 1 (- (- start 1) i))
                                    max-sz)])
                        (let* ([raw (vector-ref
                                      (history-state-entries h)
                                      idx)])
                          (let* ([cmd (cond
                                        [(not raw) #f]
                                        [(vector? raw)
                                         (history-entry-command raw)]
                                        [else raw])])
                            (if (and cmd (string-contains? cmd substr))
                                cmd
                                (loop (- i 1))))))))))))))
  (define (history-entries)
    (if (not *history*)
        (list)
        (let* ([h *history*])
          (let* ([total (history-state-count h)])
            (let* ([max-sz (history-state-max-size h)])
              (let* ([count (min total max-sz)])
                (let loop ([i 0] [result (list)])
                  (if (>= i count)
                      (reverse result)
                      (let* ([abs-num (+ (max 1 (- total max-sz -1)) i)])
                        (let* ([idx (modulo (- abs-num 1) max-sz)])
                          (let* ([raw (vector-ref
                                        (history-state-entries h)
                                        idx)])
                            (loop
                              (+ i 1)
                              (cond
                                [(not raw) result]
                                [(vector? raw) (cons raw result)]
                                [else
                                 (cons
                                   (make-history-entry 0 "" raw)
                                   result)])))))))))))))
  (define (history-list)
    (map history-entry-command (history-entries)))
  (define (history-count)
    (if *history*
        (min (history-state-count *history*)
             (history-state-max-size *history*))
        0))
  (define (history-clear!)
    (when *history*
      (let ([h *history*])
        (history-state-entries-set!
          h
          (make-vector (history-state-max-size h) #f))
        (history-state-count-set! h 0))))
  (define (history-split-words str)
    (let ([len (string-length str)])
      (let loop ([i 0]
                 [words (list)]
                 [buf (open-output-string)]
                 [in-word? #f])
        (cond
          [(>= i len)
           (if in-word?
               (reverse (cons (get-output-string buf) words))
               (reverse words))]
          [(char-whitespace? (string-ref str i))
           (if in-word?
               (loop
                 (+ i 1)
                 (cons (get-output-string buf) words)
                 (open-output-string)
                 #f)
               (loop (+ i 1) words buf #f))]
          [(char=? (string-ref str i) #\')
           (display #\' buf)
           (let qloop ([j (+ i 1)])
             (cond
               [(>= j len) (loop j words buf #t)]
               [(char=? (string-ref str j) #\')
                (display #\' buf)
                (loop (+ j 1) words buf #t)]
               [else (display (string-ref str j) buf) (qloop (+ j 1))]))]
          [(char=? (string-ref str i) #\")
           (display #\" buf)
           (let qloop ([j (+ i 1)])
             (cond
               [(>= j len) (loop j words buf #t)]
               [(char=? (string-ref str j) #\")
                (display #\" buf)
                (loop (+ j 1) words buf #t)]
               [(and (char=? (string-ref str j) #\\) (< (+ j 1) len))
                (display (string-ref str j) buf)
                (display (string-ref str (+ j 1)) buf)
                (qloop (+ j 2))]
               [else (display (string-ref str j) buf) (qloop (+ j 1))]))]
          [else
           (display (string-ref str i) buf)
           (loop (+ i 1) words buf #t)]))))
  (define (select-words words designator)
    (let ([n (length words)])
      (cond
        [(eq? designator 'caret) (if (> n 1) (list-ref words 1) "")]
        [(eq? designator 'dollar)
         (if (> n 0) (list-ref words (- n 1)) "")]
        [(eq? designator 'star)
         (if (> n 1)
             (let ([strs (list-tail words 1)] [sep " "])
               (if (null? strs)
                   ""
                   (let lp ([result (car strs)] [rest (cdr strs)])
                     (if (null? rest)
                         result
                         (lp (string-append result sep (car rest))
                             (cdr rest))))))
             "")]
        [(integer? designator)
         (if (and (>= designator 0) (< designator n))
             (list-ref words designator)
             "")]
        [(and (pair? designator) (eq? (car designator) 'range))
         (let* ([start (cadr designator)])
           (let* ([end-spec (cddr designator)])
             (let* ([end (cond
                           [(eq? end-spec 'dollar) (- n 1)]
                           [(eq? end-spec 'minus-one) (max 0 (- n 2))]
                           [else (min end-spec (- n 1))])])
               (if (and (>= start 0) (< start n) (>= end start))
                   (let ([strs (let loop ([i start] [acc (list)])
                                 (if (> i end)
                                     (reverse acc)
                                     (loop
                                       (+ i 1)
                                       (cons (list-ref words i) acc))))]
                         [sep " "])
                     (if (null? strs)
                         ""
                         (let lp ([result (car strs)] [rest (cdr strs)])
                           (if (null? rest)
                               result
                               (lp (string-append result sep (car rest))
                                   (cdr rest))))))
                   ""))))]
        [else
         (let ([strs words] [sep " "])
           (if (null? strs)
               ""
               (let lp ([result (car strs)] [rest (cdr strs)])
                 (if (null? rest)
                     result
                     (lp (string-append result sep (car rest))
                         (cdr rest))))))])))
  (define (apply-history-modifier text modifier)
    (cond
      [(eq? modifier 'h)
       (let ([pos (string-last-index-of text #\/)])
         (cond
           [(not pos) text]
           [(= pos 0) "/"]
           [else (substring text 0 pos)]))]
      [(eq? modifier 't)
       (let ([pos (string-last-index-of text #\/)])
         (if pos
             (substring text (+ pos 1) (string-length text))
             text))]
      [(eq? modifier 'r)
       (let ([pos (string-last-index-of text #\.)])
         (if (and pos
                  (> pos 0)
                  (let ([before (string-ref text (- pos 1))])
                    (not (char=? before #\/))))
             (substring text 0 pos)
             text))]
      [(eq? modifier 'e)
       (let ([pos (string-last-index-of text #\.)])
         (if pos (substring text pos (string-length text)) ""))]
      [(eq? modifier 'q) (string-append "'" text "'")]
      [(eq? modifier 'x)
       (let ([strs (map (lambda (w) (string-append "'" w "'"))
                        (history-split-words text))]
             [sep " "])
         (if (null? strs)
             ""
             (let lp ([result (car strs)] [rest (cdr strs)])
               (if (null? rest)
                   result
                   (lp (string-append result sep (car rest))
                       (cdr rest))))))]
      [(eq? modifier 'p) text]
      [(and (pair? modifier) (eq? (car modifier) 'subst))
       (let ([old (cadr modifier)]
             [new (caddr modifier)]
             [global? (cadddr modifier)])
         (set! *last-subst* (cons old new))
         (if global?
             (string-replace-all text old new)
             (string-replace-first text old new)))]
      [(eq? modifier 'repeat-subst)
       (if *last-subst*
           (string-replace-first
             text
             (car *last-subst*)
             (cdr *last-subst*))
           text)]
      [else text]))
  (define (parse-event line pos)
    (let ([len (string-length line)])
      (cond
        [(and (< pos len) (char=? (string-ref line pos) #\!))
         (let ([entry (history-get-relative 0)])
           (if entry
               (values entry (+ pos 1))
               (error 'gerbil "!!: event not found")))]
        [(and (< pos len) (char=? (string-ref line pos) #\#))
         (values (substring line 0 (- pos 1)) (+ pos 1))]
        [(and (< pos len) (char=? (string-ref line pos) #\-))
         (let* ([num-end (find-number-end line (+ pos 1))])
           (let* ([num-str (substring line (+ pos 1) num-end)])
             (let* ([n (string->number num-str)])
               (if n
                   (let ([entry (history-get-relative (- n 1))])
                     (if entry
                         (values entry num-end)
                         (error 'gerbil
                           (format "!-~a: event not found" n))))
                   (error 'gerbil "!-: bad event specification")))))]
        [(and (< pos len) (char-numeric? (string-ref line pos)))
         (let* ([num-end (find-number-end line pos)])
           (let* ([num-str (substring line pos num-end)])
             (let* ([n (string->number num-str)])
               (if n
                   (let ([entry (history-get n)])
                     (if entry
                         (values entry num-end)
                         (error 'gerbil
                           (format "!~a: event not found" n))))
                   (error 'gerbil "bad event specification")))))]
        [(and (< pos len) (char=? (string-ref line pos) #\?))
         (let* ([close (string-index-of line #\? (+ pos 1))])
           (let* ([end (or close len)])
             (let* ([substr (substring line (+ pos 1) end)])
               (let* ([entry (history-search-reverse substr)])
                 (if entry
                     (values entry (if close (+ close 1) end))
                     (error 'gerbil
                       (format "!?~a?: event not found" substr)))))))]
        [(< pos len)
         (let* ([word-end (find-event-end line pos)])
           (let* ([prefix (substring line pos word-end)])
             (let* ([matches (history-search prefix)])
               (if (pair? matches)
                   (values (last matches) word-end)
                   (error 'gerbil
                     (format "!~a: event not found" prefix))))))]
        [else (error 'gerbil "bad ! event specification")])))
  (define (find-event-end str start)
    (let loop ([i start])
      (if (and (< i (string-length str))
               (not (char-whitespace? (string-ref str i)))
               (not (char=? (string-ref str i) #\:)))
          (loop (+ i 1))
          i)))
  (define (parse-word-designator line pos)
    (let ([len (string-length line)])
      (if (>= pos len)
          (values #f pos)
          (let ([ch (string-ref line pos)])
            (cond
              [(char=? ch #\^) (values 'caret (+ pos 1))]
              [(char=? ch #\$) (values 'dollar (+ pos 1))]
              [(char=? ch #\*) (values 'star (+ pos 1))]
              [(char-numeric? ch)
               (let* ([num-end (find-number-end line pos)])
                 (let* ([n (string->number (substring line pos num-end))])
                   (cond
                     [(and (< num-end len)
                           (char=? (string-ref line num-end) #\*))
                      (values
                        (cons 'range (cons n 'dollar))
                        (+ num-end 1))]
                     [(and (< num-end len)
                           (char=? (string-ref line num-end) #\-))
                      (let ([after-dash (+ num-end 1)])
                        (if (and (< after-dash len)
                                 (char-numeric?
                                   (string-ref line after-dash)))
                            (let* ([m-end (find-number-end
                                            line
                                            after-dash)])
                              (let* ([m (string->number
                                          (substring
                                            line
                                            after-dash
                                            m-end))])
                                (values (cons 'range (cons n m)) m-end)))
                            (values
                              (cons 'range (cons n 'minus-one))
                              after-dash)))]
                     [else (values n num-end)])))]
              [else (values #f pos)])))))
  (define (parse-modifier line pos)
    (let ([len (string-length line)])
      (if (>= pos len)
          (values #f pos)
          (let ([ch (string-ref line pos)])
            (cond
              [(char=? ch #\h) (values 'h (+ pos 1))]
              [(char=? ch #\t) (values 't (+ pos 1))]
              [(char=? ch #\r) (values 'r (+ pos 1))]
              [(char=? ch #\e) (values 'e (+ pos 1))]
              [(char=? ch #\p) (values 'p (+ pos 1))]
              [(char=? ch #\q) (values 'q (+ pos 1))]
              [(char=? ch #\x) (values 'x (+ pos 1))]
              [(char=? ch #\&) (values 'repeat-subst (+ pos 1))]
              [(or (char=? ch #\g) (char=? ch #\a))
               (if (and (< (+ pos 1) len)
                        (char=? (string-ref line (+ pos 1)) #\s))
                   (parse-substitution line (+ pos 2) #t)
                   (values #f (+ pos 1)))]
              [(char=? ch #\s) (parse-substitution line (+ pos 1) #f)]
              [else (values #f pos)])))))
  (define (parse-substitution line pos global?)
    (let ([len (string-length line)])
      (if (>= pos len)
          (values #f pos)
          (let ([delim (string-ref line pos)])
            (let ([old-end (string-index-of line delim (+ pos 1))])
              (if (not old-end)
                  (values #f pos)
                  (let ([old (substring line (+ pos 1) old-end)])
                    (let ([new-end (string-index-of
                                     line
                                     delim
                                     (+ old-end 1))])
                      (if new-end
                          (values
                            (list
                              'subst
                              old
                              (substring line (+ old-end 1) new-end)
                              global?)
                            (+ new-end 1))
                          (values
                            (list
                              'subst
                              old
                              (substring line (+ old-end 1) len)
                              global?)
                            len))))))))))
  (define (parse-history-ref line pos)
    (let ([len (string-length line)])
      (cond
        [(and (< (+ pos 1) len)
              (char=? (string-ref line (+ pos 1)) #\$))
         (let* ([prev (or (history-get-relative 0) "")])
           (let* ([words (history-split-words prev)])
             (let* ([text (select-words words 'dollar)])
               (values text (+ pos 2) #f))))]
        [(and (< (+ pos 1) len)
              (char=? (string-ref line (+ pos 1)) #\^))
         (let* ([prev (or (history-get-relative 0) "")])
           (let* ([words (history-split-words prev)])
             (let* ([text (select-words words 'caret)])
               (values text (+ pos 2) #f))))]
        [(and (< (+ pos 1) len)
              (char=? (string-ref line (+ pos 1)) #\*))
         (let* ([prev (or (history-get-relative 0) "")])
           (let* ([words (history-split-words prev)])
             (let* ([text (select-words words 'star)])
               (values text (+ pos 2) #f))))]
        [else
         (let-values ([(event end) (parse-event line (+ pos 1))])
           (let ([text event] [i end] [print-only? #f])
             (let process ([text text] [i i] [print-only? print-only?])
               (if (and (< i len) (char=? (string-ref line i) #\:))
                   (let-values ([(wd wd-end)
                                 (parse-word-designator line (+ i 1))])
                     (if wd
                         (let* ([words (history-split-words text)])
                           (let* ([selected (select-words words wd)])
                             (process selected wd-end print-only?)))
                         (let-values ([(mod mod-end)
                                       (parse-modifier line (+ i 1))])
                           (if mod
                               (let ([new-text (apply-history-modifier
                                                 text
                                                 mod)]
                                     [new-p (or print-only? (eq? mod 'p))])
                                 (process new-text mod-end new-p))
                               (values text i print-only?)))))
                   (values text i print-only?)))))])))
  (define (history-expand line)
    (if (not *history*)
        (cons line #t)
        (let ([len (string-length line)])
          (if (and (> len 0) (char=? (string-ref line 0) #\^))
              (let ([result (expand-quick-subst line)]) (cons result #t))
              (let loop ([i 0]
                         [result (open-output-string)]
                         [changed? #f]
                         [print-only? #f])
                (cond
                  [(>= i len)
                   (if changed?
                       (cons (get-output-string result) (not print-only?))
                       (cons line #t))]
                  [(and (< (+ i 1) len)
                        (char=? (string-ref line i) #\\)
                        (char=? (string-ref line (+ i 1)) #\!))
                   (display "!" result)
                   (loop (+ i 2) result #t print-only?)]
                  [(char=? (string-ref line i) #\')
                   (display "'" result)
                   (let qloop ([j (+ i 1)])
                     (cond
                       [(>= j len) (loop j result changed? print-only?)]
                       [(char=? (string-ref line j) #\')
                        (display "'" result)
                        (loop (+ j 1) result changed? print-only?)]
                       [else
                        (display (string-ref line j) result)
                        (qloop (+ j 1))]))]
                  [(char=? (string-ref line i) #\!)
                   (if (or (= (+ i 1) len)
                           (char-whitespace? (string-ref line (+ i 1)))
                           (char=? (string-ref line (+ i 1)) #\=)
                           (char=? (string-ref line (+ i 1)) #\())
                       (begin
                         (display "!" result)
                         (loop (+ i 1) result changed? print-only?))
                       (let-values ([(text end ponly?)
                                     (parse-history-ref line i)])
                         (display text result)
                         (loop end result #t (or print-only? ponly?))))]
                  [else
                   (display (string-ref line i) result)
                   (loop (+ i 1) result changed? print-only?)]))))))
  (define (expand-quick-subst line)
    (let* ([len (string-length line)])
      (let* ([second (string-index-of line #\^ 1)])
        (let* ([third (if second
                          (string-index-of line #\^ (+ second 1))
                          #f)])
          (let* ([old (and second (substring line 1 second))])
            (let* ([new (and second
                             (if third
                                 (substring line (+ second 1) third)
                                 (substring line (+ second 1) len)))])
              (let* ([prev (history-get-relative 0)])
                (if (and old new prev)
                    (let ([expanded (string-replace-first prev old new)])
                      (if third
                          (string-append
                            expanded
                            (substring line (+ third 1) len))
                          expanded))
                    line))))))))
  (define (history-save!)
    (when *history*
      (let ([entries (history-entries)]
            [file (history-state-file *history*)])
        (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
          (call-with-output-file
            file
            (lambda (port)
              (for-each
                (lambda (entry)
                  (fprintf port "~a\t~a\t~a~n" (history-entry-timestamp entry)
                    (history-entry-cwd entry)
                    (history-entry-command entry)))
                entries)))))))
  (define (history-load!)
    (when *history*
      (let ([file (history-state-file *history*)])
        (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
          (when (file-exists? file)
            (let ([lines (read-file-lines file)])
              (for-each
                (lambda (line)
                  (when (> (string-length line) 0)
                    (let ([tab1 (string-index-of line #\tab 0)])
                      (if tab1
                          (let ([tab2 (string-index-of
                                        line
                                        #\tab
                                        (+ tab1 1))])
                            (if tab2
                                (let* ([ts-str (substring line 0 tab1)])
                                  (let* ([cwd (substring
                                                line
                                                (+ tab1 1)
                                                tab2)])
                                    (let* ([cmd (substring
                                                  line
                                                  (+ tab2 1)
                                                  (string-length line))])
                                      (let* ([ts (or (string->number
                                                       ts-str)
                                                     0)])
                                        (history-add-raw! ts cwd cmd)))))
                                (history-add-raw! 0 "" line)))
                          (history-add-raw! 0 "" line)))))
                lines)))))))
  (define (history-add-raw! timestamp cwd command)
    (when (and *history* (> (string-length command) 0))
      (unless (history-should-ignore? command)
        (let* ([h *history*])
          (let* ([idx (modulo
                        (history-state-count h)
                        (history-state-max-size h))])
            (let* ([entry (make-history-entry timestamp cwd command)])
              (vector-set! (history-state-entries h) idx entry)
              (history-state-count-set!
                h
                (+ 1 (history-state-count h)))))))))
  (define (history-set-control! controls)
    (when *history*
      (history-state-control-set! *history* controls)))
  (define (history-should-ignore? line)
    (let ([controls (if *history*
                        (history-state-control *history*)
                        (list))])
      (or (and (memq 'ignorespace controls)
               (> (string-length line) 0)
               (char=? (string-ref line 0) #\space))
          (and (memq 'ignoredups controls)
               (let ([prev (history-get-relative 0)])
                 (and prev (string=? prev line)))))))
  (define (history-unique-commands)
    (let ([entries (history-entries)] [seen (make-hash-table)])
      (let loop ([es (reverse entries)] [result (list)])
        (if (null? es)
            result
            (let ([cmd (history-entry-command (car es))])
              (if (hash-get seen cmd)
                  (loop (cdr es) result)
                  (begin
                    (hash-put! seen cmd #t)
                    (loop (cdr es) (cons cmd result)))))))))
  (define (find-number-end str start)
    (let loop ([i start])
      (if (and (< i (string-length str))
               (char-numeric? (string-ref str i)))
          (loop (+ i 1))
          i)))
  (define (find-word-end str start)
    (let loop ([i start])
      (if (and (< i (string-length str))
               (not (char-whitespace? (string-ref str i))))
          (loop (+ i 1))
          i)))
  (define (string-index-of str ch start)
    (let loop ([i start])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (string-replace-first str old new)
    (let ([pos (string-search str old)])
      (if pos
          (string-append
            (substring str 0 pos)
            new
            (substring
              str
              (+ pos (string-length old))
              (string-length str)))
          str)))
  (define (string-replace-all str old new)
    (let ([olen (string-length old)])
      (if (= olen 0)
          str
          (let loop ([start 0] [result (open-output-string)])
            (let ([pos (string-search-from str old start)])
              (if pos
                  (begin
                    (display (substring str start pos) result)
                    (display new result)
                    (loop (+ pos olen) result))
                  (begin
                    (display
                      (substring str start (string-length str))
                      result)
                    (get-output-string result))))))))
  (define (string-search haystack needle)
    (string-search-from haystack needle 0))
  (define (string-search-from haystack needle start)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      (let loop ([i start])
        (cond
          [(> (+ i nlen) hlen) #f]
          [(string=? (substring haystack i (+ i nlen)) needle) i]
          [else (loop (+ i 1))]))))
  (define (string-trim str)
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
          (substring str start end))))))
