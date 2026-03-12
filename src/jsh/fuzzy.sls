#!chezscheme
(library (jsh fuzzy)
  (export fuzzy-match-score fuzzy-match? fuzzy-match-positions
    fzf-score fzf-match? fzf-filter search-term::t
    make-search-term search-term? search-term-type
    search-term-text search-term-negate? search-term-type-set!
    search-term-text-set! search-term-negate?-set!
    parse-search-pattern)
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
    (std sugar) (gsh util))
  (define SCORE-MATCH 1)
  (define SCORE-GAP-START -3)
  (define SCORE-GAP-EXTEND -1)
  (define BONUS-BOUNDARY 8)
  (define BONUS-CAMEL 7)
  (define BONUS-CONSECUTIVE 4)
  (define BONUS-FIRST-CHAR 2)
  (define (char-boundary? ch)
    "Check if character is a word boundary marker."
    (or (char=? ch #\/)
        (char=? ch #\-)
        (char=? ch #\_)
        (char=? ch #\.)
        (char=? ch #\space)
        (char=? ch #\tab)))
  (define (boundary-bonus? target i)
    "Check if position i in target is after a word boundary."
    (or (= i 0)
        (and (> i 0) (char-boundary? (string-ref target (- i 1))))))
  (define (camel-bonus? target i)
    "Check if position i is a camelCase boundary (lower→upper transition)."
    (and (> i 0)
         (char-upper-case? (string-ref target i))
         (char-lower-case? (string-ref target (- i 1)))))
  (define (fuzzy-match-score pattern target)
    "Score a fuzzy match of pattern against target.\n   Returns score > 0 if matches, 0 if no match.\n   Higher score = better match."
    (let* ([plen (string-length pattern)])
      (let* ([tlen (string-length target)])
        (let* ([case-sensitive? (let loop ([i 0])
                                  (if (>= i plen)
                                      #f
                                      (if (char-upper-case?
                                            (string-ref pattern i))
                                          #t
                                          (loop (+ i 1)))))])
          (if (= plen 0)
              SCORE-MATCH
              (let ([char= (if case-sensitive?
                               char=?
                               (lambda (a b)
                                 (char=?
                                   (char-downcase a)
                                   (char-downcase b))))])
                (let loop ([pi 0]
                           [ti 0]
                           [score 0]
                           [consecutive 0]
                           [in-gap? #f])
                  (cond
                    [(>= pi plen) (if (> score 0) score SCORE-MATCH)]
                    [(>= ti tlen) 0]
                    [(char= (string-ref pattern pi) (string-ref target ti))
                     (let* ([bonus (+ SCORE-MATCH
                                      (if (boundary-bonus? target ti)
                                          BONUS-BOUNDARY
                                          0)
                                      (if (camel-bonus? target ti)
                                          BONUS-CAMEL
                                          0)
                                      (if (> consecutive 0)
                                          BONUS-CONSECUTIVE
                                          0)
                                      (if (= pi 0) BONUS-FIRST-CHAR 0))])
                       (loop (+ pi 1) (+ ti 1) (+ score bonus)
                         (+ consecutive 1) #f))]
                    [else
                     (let ([penalty (if in-gap?
                                        SCORE-GAP-EXTEND
                                        SCORE-GAP-START)])
                       (loop pi (+ ti 1) (+ score penalty) 0 #t))]))))))))
  (define (fuzzy-match? pattern target)
    "Check if pattern fuzzy-matches target (returns #t/#f)."
    (> (fuzzy-match-score pattern target) 0))
  (define (fuzzy-match-positions pattern target)
    "Find character positions in target where pattern characters match.\n   Returns list of indices, or [] if no match."
    (let* ([plen (string-length pattern)])
      (let* ([tlen (string-length target)])
        (let* ([case-sensitive? (let loop ([i 0])
                                  (if (>= i plen)
                                      #f
                                      (if (char-upper-case?
                                            (string-ref pattern i))
                                          #t
                                          (loop (+ i 1)))))])
          (if (= plen 0)
              (list)
              (let ([char= (if case-sensitive?
                               char=?
                               (lambda (a b)
                                 (char=?
                                   (char-downcase a)
                                   (char-downcase b))))])
                (let loop ([pi 0] [ti 0] [positions (list)])
                  (cond
                    [(>= pi plen) (reverse positions)]
                    [(>= ti tlen) (list)]
                    [(char= (string-ref pattern pi) (string-ref target ti))
                     (loop (+ pi 1) (+ ti 1) (cons ti positions))]
                    [else (loop pi (+ ti 1) positions)]))))))))
  (define (exact-match-score pattern target)
    "Score an exact substring match. Returns score > 0 if found, 0 otherwise."
    (let* ([case-sensitive? (let loop ([i 0])
                              (if (>= i (string-length pattern))
                                  #f
                                  (if (char-upper-case?
                                        (string-ref pattern i))
                                      #t
                                      (loop (+ i 1)))))])
      (let* ([hay (if case-sensitive?
                      target
                      (string-downcase target))])
        (let* ([needle (if case-sensitive?
                           pattern
                           (string-downcase pattern))])
          (if (string-contains? hay needle)
              (+ SCORE-MATCH
                 (* BONUS-CONSECUTIVE (string-length pattern))
                 (let ([pos (string-search-pos hay needle)])
                   (if (and pos (boundary-bonus? target pos))
                       BONUS-BOUNDARY
                       0)))
              0)))))
  (define (prefix-match-score pattern target)
    "Score a prefix match. Returns score > 0 if target starts with pattern, 0 otherwise."
    (let* ([plen (string-length pattern)])
      (let* ([tlen (string-length target)])
        (let* ([case-sensitive? (let loop ([i 0])
                                  (if (>= i plen)
                                      #f
                                      (if (char-upper-case?
                                            (string-ref pattern i))
                                          #t
                                          (loop (+ i 1)))))])
          (if (< tlen plen)
              0
              (let ([hay (if case-sensitive?
                             target
                             (string-downcase target))]
                    [needle (if case-sensitive?
                                pattern
                                (string-downcase pattern))])
                (if (and (>= (string-length hay) plen)
                         (string=? (substring hay 0 plen) needle))
                    (+ SCORE-MATCH
                       (* BONUS-CONSECUTIVE plen)
                       BONUS-BOUNDARY
                       BONUS-FIRST-CHAR)
                    0)))))))
  (define (suffix-match-score pattern target)
    "Score a suffix match. Returns score > 0 if target ends with pattern, 0 otherwise."
    (let* ([plen (string-length pattern)])
      (let* ([tlen (string-length target)])
        (let* ([case-sensitive? (let loop ([i 0])
                                  (if (>= i plen)
                                      #f
                                      (if (char-upper-case?
                                            (string-ref pattern i))
                                          #t
                                          (loop (+ i 1)))))])
          (if (< tlen plen)
              0
              (let ([hay (if case-sensitive?
                             target
                             (string-downcase target))]
                    [needle (if case-sensitive?
                                pattern
                                (string-downcase pattern))])
                (if (string=?
                      (substring
                        hay
                        (- (string-length hay) plen)
                        (string-length hay))
                      needle)
                    (+ SCORE-MATCH (* BONUS-CONSECUTIVE plen))
                    0)))))))
  (define (string-search-pos haystack needle)
    "Find position of needle in haystack, or #f."
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      (let loop ([i 0])
        (cond
          [(> (+ i nlen) hlen) #f]
          [(string=? (substring haystack i (+ i nlen)) needle) i]
          [else (loop (+ i 1))]))))
  (begin
    (define search-term::t
      (make-class-type 'gerbil\x23;search-term::t 'search-term (list object::t)
        '(type text negate?) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-search-term . args)
      (let* ([type search-term::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (search-term? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;search-term::t))
    (define (search-term-type obj)
      (unchecked-slot-ref obj 'type))
    (define (search-term-text obj)
      (unchecked-slot-ref obj 'text))
    (define (search-term-negate? obj)
      (unchecked-slot-ref obj 'negate?))
    (define (search-term-type-set! obj val)
      (unchecked-slot-set! obj 'type val))
    (define (search-term-text-set! obj val)
      (unchecked-slot-set! obj 'text val))
    (define (search-term-negate?-set! obj val)
      (unchecked-slot-set! obj 'negate? val))
    (define (&search-term-type obj)
      (unchecked-slot-ref obj 'type))
    (define (&search-term-text obj)
      (unchecked-slot-ref obj 'text))
    (define (&search-term-negate? obj)
      (unchecked-slot-ref obj 'negate?))
    (define (&search-term-type-set! obj val)
      (unchecked-slot-set! obj 'type val))
    (define (&search-term-text-set! obj val)
      (unchecked-slot-set! obj 'text val))
    (define (&search-term-negate?-set! obj val)
      (unchecked-slot-set! obj 'negate? val)))
  (define (parse-search-pattern pattern)
    "Parse an fzf extended search pattern into a list of OR-groups.\n   Each OR-group is a list of search-terms that must ALL match (AND).\n   The groups are OR'd together: if any group matches, the candidate matches.\n   Returns: list of (list of search-term)"
    (let* ([groups (split-or-groups pattern)])
      (map parse-and-group groups)))
  (define (split-or-groups str)
    "Split pattern on | (pipe), respecting spaces around |."
    (let loop ([i 0] [start 0] [groups (list)])
      (cond
        [(>= i (string-length str))
         (reverse (cons (substring str start i) groups))]
        [(and (char=? (string-ref str i) #\|)
              (or (= i 0) (char=? (string-ref str (- i 1)) #\space))
              (or (= (+ i 1) (string-length str))
                  (char=? (string-ref str (+ i 1)) #\space)))
         (loop
           (+ i 1)
           (+ i 1)
           (cons (substring str start i) groups))]
        [else (loop (+ i 1) start groups)])))
  (define (parse-and-group str)
    "Parse space-separated terms into a list of search-term structs."
    (let ([words (filter
                   (lambda (w) (> (string-length w) 0))
                   (string-split-chars (fuzzy-trim str) " "))])
      (map parse-single-term words)))
  (define (parse-single-term word)
    "Parse a single search term with optional modifiers."
    (let* ([negate? (and (> (string-length word) 1)
                         (char=? (string-ref word 0) #\!))])
      (let* ([text (if negate?
                       (substring word 1 (string-length word))
                       word)])
        (cond
          [(and (> (string-length text) 1)
                (char=? (string-ref text 0) #\'))
           (make-search-term
             'exact
             (substring text 1 (string-length text))
             negate?)]
          [(and (> (string-length text) 1)
                (char=? (string-ref text 0) #\^))
           (make-search-term
             'prefix
             (substring text 1 (string-length text))
             negate?)]
          [(and (> (string-length text) 1)
                (char=? (string-ref text (- (string-length text) 1)) #\$))
           (make-search-term
             'suffix
             (substring text 0 (- (string-length text) 1))
             negate?)]
          [else (make-search-term 'fuzzy text negate?)]))))
  (define (fuzzy-trim str)
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
  (define (score-term term target)
    "Score a single search-term against a target string.\n   Returns score > 0 for match, 0 for no match."
    (let ([raw-score (case (search-term-type term)
                       [(fuzzy)
                        (fuzzy-match-score (search-term-text term) target)]
                       [(exact)
                        (exact-match-score (search-term-text term) target)]
                       [(prefix)
                        (prefix-match-score
                          (search-term-text term)
                          target)]
                       [(suffix)
                        (suffix-match-score
                          (search-term-text term)
                          target)]
                       [else 0])])
      (if (search-term-negate? term)
          (if (> raw-score 0) 0 SCORE-MATCH)
          raw-score)))
  (define (score-and-group group target)
    "Score an AND group: all terms must match. Returns total score or 0."
    (let loop ([terms group] [total 0])
      (cond
        [(null? terms) (if (> total 0) total SCORE-MATCH)]
        [else
         (let ([s (score-term (car terms) target)])
           (if (= s 0) 0 (loop (cdr terms) (+ total s))))])))
  (define (fzf-score pattern target)
    "Score target against an fzf extended search pattern string.\n   Returns score > 0 for match, 0 for no match."
    (if (= (string-length (fuzzy-trim pattern)) 0)
        SCORE-MATCH
        (let ([groups (parse-search-pattern pattern)])
          (let loop ([gs groups] [best 0])
            (if (null? gs)
                best
                (let ([s (score-and-group (car gs) target)])
                  (loop (cdr gs) (max best s))))))))
  (define (fzf-match? pattern target)
    "Check if target matches the fzf extended search pattern."
    (> (fzf-score pattern target) 0))
  (define (fzf-filter pattern candidates)
    "Filter and sort candidates by fzf score.\n   Returns list of (score . candidate) pairs, sorted best-first."
    (let ([scored (filter-map
                    (lambda (cand)
                      (let ([s (fzf-score pattern cand)])
                        (and (> s 0) (cons s cand))))
                    candidates)])
      (sort! scored (lambda (a b) (> (car a) (car b))))
      scored)))
