#!chezscheme
(library (jsh fzf)
  (export fzf-select fzf-history-search fzf-find-files
    fzf-find-directories fzf-init! collect-files
    collect-files-git collect-directories)
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
    (std sugar) (gsh util) (gsh fuzzy) (gsh lineedit) (gsh ffi))
  (begin
    (define fzf-state::t
      (make-class-type 'gerbil\x23;fzf-state::t 'fzf-state (list object::t)
        '(candidates filtered query cursor selected scroll-offset total
           match-count height columns prompt multi? marked done?
           result)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-fzf-state . args)
      (let* ([type fzf-state::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (fzf-state? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;fzf-state::t))
    (define (fzf-state-candidates obj)
      (unchecked-slot-ref obj 'candidates))
    (define (fzf-state-filtered obj)
      (unchecked-slot-ref obj 'filtered))
    (define (fzf-state-query obj)
      (unchecked-slot-ref obj 'query))
    (define (fzf-state-cursor obj)
      (unchecked-slot-ref obj 'cursor))
    (define (fzf-state-selected obj)
      (unchecked-slot-ref obj 'selected))
    (define (fzf-state-scroll-offset obj)
      (unchecked-slot-ref obj 'scroll-offset))
    (define (fzf-state-total obj)
      (unchecked-slot-ref obj 'total))
    (define (fzf-state-match-count obj)
      (unchecked-slot-ref obj 'match-count))
    (define (fzf-state-height obj)
      (unchecked-slot-ref obj 'height))
    (define (fzf-state-columns obj)
      (unchecked-slot-ref obj 'columns))
    (define (fzf-state-prompt obj)
      (unchecked-slot-ref obj 'prompt))
    (define (fzf-state-multi? obj)
      (unchecked-slot-ref obj 'multi?))
    (define (fzf-state-marked obj)
      (unchecked-slot-ref obj 'marked))
    (define (fzf-state-done? obj)
      (unchecked-slot-ref obj 'done?))
    (define (fzf-state-result obj)
      (unchecked-slot-ref obj 'result))
    (define (fzf-state-candidates-set! obj val)
      (unchecked-slot-set! obj 'candidates val))
    (define (fzf-state-filtered-set! obj val)
      (unchecked-slot-set! obj 'filtered val))
    (define (fzf-state-query-set! obj val)
      (unchecked-slot-set! obj 'query val))
    (define (fzf-state-cursor-set! obj val)
      (unchecked-slot-set! obj 'cursor val))
    (define (fzf-state-selected-set! obj val)
      (unchecked-slot-set! obj 'selected val))
    (define (fzf-state-scroll-offset-set! obj val)
      (unchecked-slot-set! obj 'scroll-offset val))
    (define (fzf-state-total-set! obj val)
      (unchecked-slot-set! obj 'total val))
    (define (fzf-state-match-count-set! obj val)
      (unchecked-slot-set! obj 'match-count val))
    (define (fzf-state-height-set! obj val)
      (unchecked-slot-set! obj 'height val))
    (define (fzf-state-columns-set! obj val)
      (unchecked-slot-set! obj 'columns val))
    (define (fzf-state-prompt-set! obj val)
      (unchecked-slot-set! obj 'prompt val))
    (define (fzf-state-multi?-set! obj val)
      (unchecked-slot-set! obj 'multi? val))
    (define (fzf-state-marked-set! obj val)
      (unchecked-slot-set! obj 'marked val))
    (define (fzf-state-done?-set! obj val)
      (unchecked-slot-set! obj 'done? val))
    (define (fzf-state-result-set! obj val)
      (unchecked-slot-set! obj 'result val))
    (define (&fzf-state-candidates obj)
      (unchecked-slot-ref obj 'candidates))
    (define (&fzf-state-filtered obj)
      (unchecked-slot-ref obj 'filtered))
    (define (&fzf-state-query obj)
      (unchecked-slot-ref obj 'query))
    (define (&fzf-state-cursor obj)
      (unchecked-slot-ref obj 'cursor))
    (define (&fzf-state-selected obj)
      (unchecked-slot-ref obj 'selected))
    (define (&fzf-state-scroll-offset obj)
      (unchecked-slot-ref obj 'scroll-offset))
    (define (&fzf-state-total obj)
      (unchecked-slot-ref obj 'total))
    (define (&fzf-state-match-count obj)
      (unchecked-slot-ref obj 'match-count))
    (define (&fzf-state-height obj)
      (unchecked-slot-ref obj 'height))
    (define (&fzf-state-columns obj)
      (unchecked-slot-ref obj 'columns))
    (define (&fzf-state-prompt obj)
      (unchecked-slot-ref obj 'prompt))
    (define (&fzf-state-multi? obj)
      (unchecked-slot-ref obj 'multi?))
    (define (&fzf-state-marked obj)
      (unchecked-slot-ref obj 'marked))
    (define (&fzf-state-done? obj)
      (unchecked-slot-ref obj 'done?))
    (define (&fzf-state-result obj)
      (unchecked-slot-ref obj 'result))
    (define (&fzf-state-candidates-set! obj val)
      (unchecked-slot-set! obj 'candidates val))
    (define (&fzf-state-filtered-set! obj val)
      (unchecked-slot-set! obj 'filtered val))
    (define (&fzf-state-query-set! obj val)
      (unchecked-slot-set! obj 'query val))
    (define (&fzf-state-cursor-set! obj val)
      (unchecked-slot-set! obj 'cursor val))
    (define (&fzf-state-selected-set! obj val)
      (unchecked-slot-set! obj 'selected val))
    (define (&fzf-state-scroll-offset-set! obj val)
      (unchecked-slot-set! obj 'scroll-offset val))
    (define (&fzf-state-total-set! obj val)
      (unchecked-slot-set! obj 'total val))
    (define (&fzf-state-match-count-set! obj val)
      (unchecked-slot-set! obj 'match-count val))
    (define (&fzf-state-height-set! obj val)
      (unchecked-slot-set! obj 'height val))
    (define (&fzf-state-columns-set! obj val)
      (unchecked-slot-set! obj 'columns val))
    (define (&fzf-state-prompt-set! obj val)
      (unchecked-slot-set! obj 'prompt val))
    (define (&fzf-state-multi?-set! obj val)
      (unchecked-slot-set! obj 'multi? val))
    (define (&fzf-state-marked-set! obj val)
      (unchecked-slot-set! obj 'marked val))
    (define (&fzf-state-done?-set! obj val)
      (unchecked-slot-set! obj 'done? val))
    (define (&fzf-state-result-set! obj val)
      (unchecked-slot-set! obj 'result val)))
  (define ESC-STR (string #\esc))
  (define (ansi-clear-screen port)
    (fprintf port "~a[2J~a[H" ESC-STR ESC-STR))
  (define (ansi-move-to row col port)
    (fprintf port "~a[~a;~aH" ESC-STR row col))
  (define (ansi-clear-line port)
    (fprintf port "~a[2K" ESC-STR))
  (define (ansi-bold port) (fprintf port "~a[1m" ESC-STR))
  (define (ansi-underline port)
    (fprintf port "~a[4m" ESC-STR))
  (define (ansi-fg-green port)
    (fprintf port "~a[32m" ESC-STR))
  (define (ansi-fg-yellow port)
    (fprintf port "~a[33m" ESC-STR))
  (define (ansi-fg-cyan port) (fprintf port "~a[36m" ESC-STR))
  (define (ansi-reset port) (fprintf port "~a[0m" ESC-STR))
  (define (ansi-hide-cursor port)
    (fprintf port "~a[?25l" ESC-STR))
  (define (ansi-show-cursor port)
    (fprintf port "~a[?25h" ESC-STR))
  (define (ansi-save-cursor port)
    (fprintf port "~a7" ESC-STR))
  (define (ansi-restore-cursor port)
    (fprintf port "~a8" ESC-STR))
  (define (ansi-alt-screen-on port)
    (fprintf port "~a[?1049h" ESC-STR))
  (define (ansi-alt-screen-off port)
    (fprintf port "~a[?1049l" ESC-STR))
  (define (get-terminal-size)
    "Returns (values rows cols)."
    (let ([cols (ffi-terminal-columns 0)])
      (let ([rows (ffi-terminal-rows 0)])
        (values
          (if (or (not rows) (<= rows 0)) 24 rows)
          (if (or (not cols) (<= cols 0)) 80 cols)))))
  (define (render-fzf state port)
    "Render the full fzf interface."
    (let* ([height (fzf-state-height state)])
      (let* ([cols (fzf-state-columns state)])
        (let* ([filtered (fzf-state-filtered state)])
          (let* ([query (fzf-state-query state)])
            (let* ([selected (fzf-state-selected state)])
              (let* ([scroll (fzf-state-scroll-offset state)])
                (let* ([match-count (fzf-state-match-count state)])
                  (let* ([total (fzf-state-total state)])
                    (let* ([prompt-str (fzf-state-prompt state)])
                      (let* ([marked (fzf-state-marked state)])
                        (let* ([avail-lines (- height 2)])
                          (let* ([visible (fzf-take
                                            (fzf-drop filtered scroll)
                                            avail-lines)])
                            (ansi-move-to 1 1 port)
                            (ansi-clear-line port)
                            (ansi-fg-cyan port)
                            (fprintf port "  ~a/~a" match-count total)
                            (when (pair? marked)
                              (fprintf
                                port
                                " [~a selected]"
                                (length marked)))
                            (ansi-reset port)
                            (display "\r\n" port)
                            (let loop ([items visible] [idx 0])
                              (when (< idx avail-lines)
                                (ansi-clear-line port)
                                (if (pair? items)
                                    (let* ([item (car items)])
                                      (let* ([score (car item)])
                                        (let* ([text (cdr item)])
                                          (let* ([is-selected? (= idx
                                                                  selected)])
                                            (let* ([is-marked? (member
                                                                 text
                                                                 marked)])
                                              (if is-selected?
                                                  (begin
                                                    (ansi-fg-green port)
                                                    (display "> " port)
                                                    (ansi-reset port))
                                                  (display "  " port))
                                              (when is-marked?
                                                (ansi-fg-yellow port)
                                                (display "* " port)
                                                (ansi-reset port))
                                              (render-highlighted-candidate text query is-selected?
                                                cols port)
                                              (display "\r\n" port))))))
                                    (display "\r\n" port))
                                (loop
                                  (if (pair? items) (cdr items) (list))
                                  (+ idx 1))))
                            (ansi-clear-line port)
                            (ansi-fg-green port)
                            (display prompt-str port)
                            (ansi-reset port)
                            (display query port)
                            (flush-output-port port))))))))))))))
  (define (render-highlighted-candidate text query
           is-selected? cols port)
    "Render a candidate with fuzzy-matched characters highlighted."
    (let* ([positions (if (> (string-length query) 0)
                          (fuzzy-match-positions query text)
                          (list))])
      (let* ([pos-set (list->hash-set positions)])
        (let* ([max-width (- cols 4)])
          (let* ([display-len (min (string-length text) max-width)])
            (when is-selected? (ansi-bold port))
            (let loop ([i 0])
              (when (< i display-len)
                (let ([ch (string-ref text i)])
                  (if (hash-get pos-set i)
                      (begin
                        (ansi-fg-green port)
                        (ansi-underline port)
                        (display ch port)
                        (ansi-reset port)
                        (when is-selected? (ansi-bold port)))
                      (display ch port)))
                (loop (+ i 1))))
            (ansi-reset port))))))
  (define (list->hash-set lst)
    "Convert a list to a hash set for O(1) membership testing."
    (let ([ht (make-hash-table)])
      (for-each (lambda (x) (hash-put! ht x #t)) lst)
      ht))
  (define (update-filter! state)
    "Re-filter candidates based on current query."
    (let* ([query (fzf-state-query state)])
      (let* ([candidates (fzf-state-candidates state)])
        (let* ([len (vector-length candidates)])
          (let* ([filtered (if (string=? query "")
                               (let loop ([i 0] [acc (list)])
                                 (if (>= i len)
                                     (reverse acc)
                                     (loop
                                       (+ i 1)
                                       (cons
                                         (cons 1 (vector-ref candidates i))
                                         acc))))
                               (let loop ([i 0] [acc (list)])
                                 (if (>= i len)
                                     (begin
                                       (sort!
                                         acc
                                         (lambda (a b)
                                           (> (car a) (car b))))
                                       acc)
                                     (let* ([cand (vector-ref
                                                    candidates
                                                    i)])
                                       (let* ([score (fzf-score
                                                       query
                                                       cand)])
                                         (loop
                                           (+ i 1)
                                           (if (> score 0)
                                               (cons (cons score cand) acc)
                                               acc)))))))])
            (fzf-state-filtered-set! state filtered)
            (fzf-state-match-count-set! state (length filtered))
            (fzf-state-selected-set! state 0)
            (fzf-state-scroll-offset-set! state 0))))))
  (define (handle-fzf-key state key)
    "Handle a keypress in fzf mode. Returns #t if state changed."
    (let ([filtered (fzf-state-filtered state)]
          [selected (fzf-state-selected state)]
          [avail (- (fzf-state-height state) 2)])
      (cond
        [(or (and (char? key)
                  (or (= (char->integer key) 13)
                      (= (char->integer key) 10)))
             (and (char? key) (= (char->integer key) 10)))
         (if (pair? filtered)
             (let ([item (fzf-list-ref filtered selected)])
               (if item
                   (begin
                     (fzf-state-result-set!
                       state
                       (if (fzf-state-multi? state)
                           (let ([marked (fzf-state-marked state)])
                             (if (null? marked) (list (cdr item)) marked))
                           (cdr item)))
                     (fzf-state-done?-set! state #t))
                   (fzf-state-done?-set! state #t)))
             (begin
               (fzf-state-result-set! state #f)
               (fzf-state-done?-set! state #t)))
         #t]
        [(or (and (char? key) (= (char->integer key) 3))
             (eq? key 'escape))
         (fzf-state-result-set! state #f)
         (fzf-state-done?-set! state #t)
         #t]
        [(or (eq? key 'up)
             (and (char? key) (= (char->integer key) 16)))
         (when (> selected 0)
           (fzf-state-selected-set! state (- selected 1))
           (when (< (fzf-state-selected state)
                    (fzf-state-scroll-offset state))
             (fzf-state-scroll-offset-set!
               state
               (fzf-state-selected state))))
         #t]
        [(or (eq? key 'down)
             (and (char? key) (= (char->integer key) 14)))
         (when (< selected (- (length filtered) 1))
           (fzf-state-selected-set! state (+ selected 1))
           (when (>= (- (fzf-state-selected state)
                        (fzf-state-scroll-offset state))
                     avail)
             (fzf-state-scroll-offset-set!
               state
               (- (fzf-state-selected state) avail -1))))
         #t]
        [(and (char? key)
              (= (char->integer key) 9)
              (fzf-state-multi? state))
         (when (pair? filtered)
           (let* ([item (fzf-list-ref filtered selected)])
             (let* ([text (and item (cdr item))])
               (let* ([marked (fzf-state-marked state)])
                 (when text
                   (if (member text marked)
                       (fzf-state-marked-set!
                         state
                         (filter
                           (lambda (m) (not (string=? m text)))
                           marked))
                       (fzf-state-marked-set!
                         state
                         (cons text marked))))))))
         (when (< selected (- (length filtered) 1))
           (fzf-state-selected-set! state (+ selected 1)))
         #t]
        [(and (char? key) (= (char->integer key) 21))
         (fzf-state-query-set! state "")
         (fzf-state-cursor-set! state 0)
         (update-filter! state)
         #t]
        [(and (char? key) (= (char->integer key) 23))
         (let* ([query (fzf-state-query state)])
           (let* ([pos (fzf-state-cursor state)])
             (when (> pos 0)
               (let skip-sp ([i (- pos 1)])
                 (let ([start (if (and (>= i 0)
                                       (char=?
                                         (string-ref query i)
                                         #\space))
                                  (skip-sp (- i 1))
                                  (+ i 1))])
                   (let skip-word ([i (- start 1)])
                     (let ([word-start (if (and (>= i 0)
                                                (not (char=?
                                                       (string-ref query i)
                                                       #\space)))
                                           (skip-word (- i 1))
                                           (+ i 1))])
                       (fzf-state-query-set!
                         state
                         (string-append
                           (substring query 0 word-start)
                           (substring query pos (string-length query))))
                       (fzf-state-cursor-set! state word-start))))))))
         (update-filter! state)
         #t]
        [(and (char? key)
              (or (= (char->integer key) 127) (= (char->integer key) 8)))
         (let ([query (fzf-state-query state)]
               [pos (fzf-state-cursor state)])
           (when (> pos 0)
             (fzf-state-query-set!
               state
               (string-append
                 (substring query 0 (- pos 1))
                 (substring query pos (string-length query))))
             (fzf-state-cursor-set! state (- pos 1))
             (update-filter! state)))
         #t]
        [(and (char? key) (= (char->integer key) 1))
         (fzf-state-cursor-set! state 0)
         #t]
        [(and (char? key) (= (char->integer key) 5))
         (fzf-state-cursor-set!
           state
           (string-length (fzf-state-query state)))
         #t]
        [(eq? key 'page-up)
         (fzf-state-selected-set! state (max 0 (- selected avail)))
         (fzf-state-scroll-offset-set!
           state
           (max 0 (- (fzf-state-scroll-offset state) avail)))
         #t]
        [(eq? key 'page-down)
         (let ([max-sel (- (length filtered) 1)])
           (fzf-state-selected-set!
             state
             (min max-sel (+ selected avail)))
           (fzf-state-scroll-offset-set!
             state
             (max 0 (- (fzf-state-selected state) avail -1))))
         #t]
        [(and (char? key)
              (>= (char->integer key) 32)
              (not (= (char->integer key) 127)))
         (let ([query (fzf-state-query state)]
               [pos (fzf-state-cursor state)])
           (fzf-state-query-set!
             state
             (string-append
               (substring query 0 pos)
               (string key)
               (substring query pos (string-length query))))
           (fzf-state-cursor-set! state (+ pos 1))
           (update-filter! state))
         #t]
        [else #f])))
  (define (fzf-take lst n)
    (if (or (null? lst) (<= n 0))
        (list)
        (cons (car lst) (fzf-take (cdr lst) (- n 1)))))
  (define (fzf-drop lst n)
    (if (or (null? lst) (<= n 0))
        lst
        (fzf-drop (cdr lst) (- n 1))))
  (define (fzf-list-ref lst idx)
    (if (or (null? lst) (< idx 0))
        #f
        (if (= idx 0)
            (car lst)
            (fzf-list-ref (cdr lst) (- idx 1)))))
  (define fzf-select
    (case-lambda
      [(candidates)
       (let* ([prompt "> "] [multi? #f] [initial-query ""])
         "Run interactive fuzzy finder on a list of strings.\n   Returns: selected string (single mode), list of strings (multi mode), or #f on cancel."
         (let ([in-port (current-input-port)]
               [out-port (current-error-port)])
           (let-values ([(rows cols) (get-terminal-size)])
             (let* ([cand-vec (list->vector candidates)])
               (let* ([state (make-fzf-state cand-vec (list) initial-query
                               (string-length initial-query) 0 0
                               (vector-length cand-vec) 0 rows cols prompt
                               multi? (list) #f #f)])
                 (update-filter! state)
                 (dynamic-wind
                   (lambda ()
                     (term-raw! in-port)
                     (ansi-alt-screen-on out-port)
                     (ansi-hide-cursor out-port)
                     (flush-output-port out-port))
                   (lambda ()
                     (render-fzf state out-port)
                     (let loop ()
                       (let ([key (read-key in-port)])
                         (cond
                           [(eq? key 'eof)
                            (fzf-state-result-set! state #f)
                            (fzf-state-done?-set! state #t)]
                           [else (handle-fzf-key state key)])
                         (unless (fzf-state-done? state)
                           (render-fzf state out-port)
                           (loop))))
                     (fzf-state-result state))
                   (lambda ()
                     (ansi-show-cursor out-port)
                     (ansi-alt-screen-off out-port)
                     (flush-output-port out-port)
                     (term-cooked! in-port))))))))]
      [(candidates prompt)
       (let* ([multi? #f] [initial-query ""])
         "Run interactive fuzzy finder on a list of strings.\n   Returns: selected string (single mode), list of strings (multi mode), or #f on cancel."
         (let ([in-port (current-input-port)]
               [out-port (current-error-port)])
           (let-values ([(rows cols) (get-terminal-size)])
             (let* ([cand-vec (list->vector candidates)])
               (let* ([state (make-fzf-state cand-vec (list) initial-query
                               (string-length initial-query) 0 0
                               (vector-length cand-vec) 0 rows cols prompt
                               multi? (list) #f #f)])
                 (update-filter! state)
                 (dynamic-wind
                   (lambda ()
                     (term-raw! in-port)
                     (ansi-alt-screen-on out-port)
                     (ansi-hide-cursor out-port)
                     (flush-output-port out-port))
                   (lambda ()
                     (render-fzf state out-port)
                     (let loop ()
                       (let ([key (read-key in-port)])
                         (cond
                           [(eq? key 'eof)
                            (fzf-state-result-set! state #f)
                            (fzf-state-done?-set! state #t)]
                           [else (handle-fzf-key state key)])
                         (unless (fzf-state-done? state)
                           (render-fzf state out-port)
                           (loop))))
                     (fzf-state-result state))
                   (lambda ()
                     (ansi-show-cursor out-port)
                     (ansi-alt-screen-off out-port)
                     (flush-output-port out-port)
                     (term-cooked! in-port))))))))]
      [(candidates prompt multi?)
       (let* ([initial-query ""])
         "Run interactive fuzzy finder on a list of strings.\n   Returns: selected string (single mode), list of strings (multi mode), or #f on cancel."
         (let ([in-port (current-input-port)]
               [out-port (current-error-port)])
           (let-values ([(rows cols) (get-terminal-size)])
             (let* ([cand-vec (list->vector candidates)])
               (let* ([state (make-fzf-state cand-vec (list) initial-query
                               (string-length initial-query) 0 0
                               (vector-length cand-vec) 0 rows cols prompt
                               multi? (list) #f #f)])
                 (update-filter! state)
                 (dynamic-wind
                   (lambda ()
                     (term-raw! in-port)
                     (ansi-alt-screen-on out-port)
                     (ansi-hide-cursor out-port)
                     (flush-output-port out-port))
                   (lambda ()
                     (render-fzf state out-port)
                     (let loop ()
                       (let ([key (read-key in-port)])
                         (cond
                           [(eq? key 'eof)
                            (fzf-state-result-set! state #f)
                            (fzf-state-done?-set! state #t)]
                           [else (handle-fzf-key state key)])
                         (unless (fzf-state-done? state)
                           (render-fzf state out-port)
                           (loop))))
                     (fzf-state-result state))
                   (lambda ()
                     (ansi-show-cursor out-port)
                     (ansi-alt-screen-off out-port)
                     (flush-output-port out-port)
                     (term-cooked! in-port))))))))]
      [(candidates prompt multi? initial-query)
       "Run interactive fuzzy finder on a list of strings.\n   Returns: selected string (single mode), list of strings (multi mode), or #f on cancel."
       (let ([in-port (current-input-port)]
             [out-port (current-error-port)])
         (let-values ([(rows cols) (get-terminal-size)])
           (let* ([cand-vec (list->vector candidates)])
             (let* ([state (make-fzf-state cand-vec (list) initial-query
                             (string-length initial-query) 0 0
                             (vector-length cand-vec) 0 rows cols prompt
                             multi? (list) #f #f)])
               (update-filter! state)
               (dynamic-wind
                 (lambda ()
                   (term-raw! in-port)
                   (ansi-alt-screen-on out-port)
                   (ansi-hide-cursor out-port)
                   (flush-output-port out-port))
                 (lambda ()
                   (render-fzf state out-port)
                   (let loop ()
                     (let ([key (read-key in-port)])
                       (cond
                         [(eq? key 'eof)
                          (fzf-state-result-set! state #f)
                          (fzf-state-done?-set! state #t)]
                         [else (handle-fzf-key state key)])
                       (unless (fzf-state-done? state)
                         (render-fzf state out-port)
                         (loop))))
                   (fzf-state-result state))
                 (lambda ()
                   (ansi-show-cursor out-port)
                   (ansi-alt-screen-off out-port)
                   (flush-output-port out-port)
                   (term-cooked! in-port)))))))]))
  (define (fzf-history-search history-commands)
    "Run fzf on history commands (newest first). Returns selected command or #f."
    (fzf-select history-commands "(history)> "))
  (define fzf-find-files
    (case-lambda
      [()
       (let* ([dir "."] [max-files 50000])
         "Recursively list files under dir for fzf selection.\n   Uses git ls-files in git repos, falls back to filesystem walk.\n   Returns selected file path or #f."
         (let ([files (collect-files-git dir max-files)])
           (fzf-select files 'prompt: "(files)> ")))]
      [(dir)
       (let* ([max-files 50000])
         "Recursively list files under dir for fzf selection.\n   Uses git ls-files in git repos, falls back to filesystem walk.\n   Returns selected file path or #f."
         (let ([files (collect-files-git dir max-files)])
           (fzf-select files 'prompt: "(files)> ")))]
      [(dir max-files)
       "Recursively list files under dir for fzf selection.\n   Uses git ls-files in git repos, falls back to filesystem walk.\n   Returns selected file path or #f."
       (let ([files (collect-files-git dir max-files)])
         (fzf-select files 'prompt: "(files)> "))]))
  (define (collect-files-git dir max-files)
    "Collect files using git ls-files if in a git repo, else fall back to collect-files."
    (guard (__exn
             [#t ((lambda (e) (collect-files dir max-files)) __exn)])
      (let* ([proc (open-process
                     (list 'path: "git" 'arguments:
                       (list
                         "ls-files"
                         "--cached"
                         "--others"
                         "--exclude-standard")
                       'directory: dir 'stdin-redirection: #f
                       'stderr-redirection: #f))])
        (let* ([files (let loop ([acc (list)])
                        (let ([line (get-line proc)])
                          (if (or (eof-object? line)
                                  (>= (length acc) max-files))
                              (reverse acc)
                              (loop (cons line acc)))))])
          (let* ([status (process-status proc)])
            (if (= status 0) files (collect-files dir max-files)))))))
  (define *skip-dirs*
    '(".git" "node_modules" "__pycache__" ".gerbil" ".cache"
       "venv" ".venv" "target" "dist" ".hg" ".svn"))
  (define (collect-files dir max-files)
    "Recursively collect file paths under dir, up to max-files."
    (let ([result (list)] [count 0])
      (let walk ([d dir] [prefix ""])
        (when (< count max-files)
          (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
            (let ([entries (directory-files d)])
              (for-each
                (lambda (name)
                  (when (and (< count max-files)
                             (not (string=? name "."))
                             (not (string=? name ".."))
                             (not (and (> (string-length name) 0)
                                       (char=? (string-ref name 0) #\.)))
                             (not (member name *skip-dirs*)))
                    (let* ([rel (if (string=? prefix "")
                                    name
                                    (string-append prefix "/" name))])
                      (let* ([full (string-append d "/" name)])
                        (if (file-directory? full)
                            (walk full rel)
                            (begin
                              (set! result (cons rel result))
                              (set! count (+ count 1))))))))
                entries)))))
      (reverse result)))
  (define fzf-find-directories
    (case-lambda
      [()
       (let* ([dir "."] [max-dirs 10000])
         "Recursively list directories under dir for fzf selection.\n   Returns selected directory path or #f."
         (let ([dirs (collect-directories dir max-dirs)])
           (fzf-select dirs 'prompt: "(dirs)> ")))]
      [(dir)
       (let* ([max-dirs 10000])
         "Recursively list directories under dir for fzf selection.\n   Returns selected directory path or #f."
         (let ([dirs (collect-directories dir max-dirs)])
           (fzf-select dirs 'prompt: "(dirs)> ")))]
      [(dir max-dirs)
       "Recursively list directories under dir for fzf selection.\n   Returns selected directory path or #f."
       (let ([dirs (collect-directories dir max-dirs)])
         (fzf-select dirs 'prompt: "(dirs)> "))]))
  (define (collect-directories dir max-dirs)
    "Recursively collect directory paths under dir."
    (let ([result (list)] [count 0])
      (let walk ([d dir] [prefix ""])
        (when (< count max-dirs)
          (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
            (let ([entries (directory-files d)])
              (for-each
                (lambda (name)
                  (when (and (< count max-dirs)
                             (not (string=? name "."))
                             (not (string=? name ".."))
                             (not (and (> (string-length name) 0)
                                       (char=? (string-ref name 0) #\.)))
                             (not (member name *skip-dirs*)))
                    (let* ([rel (if (string=? prefix "")
                                    name
                                    (string-append prefix "/" name))])
                      (let* ([full (string-append d "/" name)])
                        (when (file-directory? full)
                          (set! result (cons rel result))
                          (set! count (+ count 1))
                          (walk full rel))))))
                entries)))))
      (reverse result)))
  (define (fzf-init!)
    "Register fzf handlers as lineedit callbacks."
    (*fzf-history-fn* fzf-history-search)
    (*fzf-files-fn* fzf-find-files)
    (*fzf-dirs-fn* fzf-find-directories)))
