;;; fuzzy.ss — Fuzzy matching engine for gsh
;;;
;;; Scoring algorithm inspired by fzf:
;;; - Boundary bonus (+8): match after / - _ . space
;;; - CamelCase bonus (+7): lowercase→uppercase transition
;;; - Consecutive bonus (+4): adjacent matching characters
;;; - First char bonus (+2): match at position 0
;;; - Gap penalty: -3 first gap, -1 subsequent gaps
;;;
;;; Extended search syntax (fzf-compatible):
;;; - foo     — fuzzy match
;;; - 'exact  — exact substring match
;;; - ^prefix — prefix match
;;; - suffix$ — suffix match
;;; - !term   — exclude matches
;;; - a | b   — OR (any term matches)
;;; - a b     — AND (all space-separated terms must match, except | groups)

(export fuzzy-match-score
        fuzzy-match?
        fuzzy-match-positions
        fzf-score
        fzf-match?
        fzf-filter
        (struct-out search-term)
        parse-search-pattern)
(import :std/sugar
        :std/sort
        :std/srfi/1
        :gsh/util)

;;;============================================================================
;;; Scoring constants
;;;============================================================================

(def SCORE-MATCH 1)
(def SCORE-GAP-START -3)
(def SCORE-GAP-EXTEND -1)
(def BONUS-BOUNDARY 8)
(def BONUS-CAMEL 7)
(def BONUS-CONSECUTIVE 4)
(def BONUS-FIRST-CHAR 2)

;;;============================================================================
;;; Core fuzzy matching
;;;============================================================================

(def (char-boundary? ch)
  "Check if character is a word boundary marker."
  (or (char=? ch #\/) (char=? ch #\-)
      (char=? ch #\_) (char=? ch #\.)
      (char=? ch #\space) (char=? ch #\tab)))

(def (boundary-bonus? target i)
  "Check if position i in target is after a word boundary."
  (or (= i 0)
      (and (> i 0) (char-boundary? (string-ref target (- i 1))))))

(def (camel-bonus? target i)
  "Check if position i is a camelCase boundary (lower→upper transition)."
  (and (> i 0)
       (char-upper-case? (string-ref target i))
       (char-lower-case? (string-ref target (- i 1)))))

(def (fuzzy-match-score pattern target)
  "Score a fuzzy match of pattern against target.
   Returns score > 0 if matches, 0 if no match.
   Higher score = better match."
  (let* ((plen (string-length pattern))
         (tlen (string-length target))
         ;; Smart case: case-insensitive unless pattern has uppercase
         (case-sensitive? (let loop ((i 0))
                           (if (>= i plen) #f
                               (if (char-upper-case? (string-ref pattern i)) #t
                                   (loop (+ i 1)))))))
    (if (= plen 0) SCORE-MATCH
        (let ((char=
                (if case-sensitive?
                  char=?
                  (lambda (a b) (char=? (char-downcase a) (char-downcase b))))))
          ;; Greedy forward match (fzf V1 algorithm)
          (let loop ((pi 0) (ti 0) (score 0) (consecutive 0) (in-gap? #f))
            (cond
              ;; Matched all pattern chars
              ((>= pi plen)
               (if (> score 0) score SCORE-MATCH))
              ;; Ran out of target chars before matching all pattern chars
              ((>= ti tlen) 0)
              ;; Current chars match
              ((char= (string-ref pattern pi) (string-ref target ti))
               (let* ((bonus (+ SCORE-MATCH
                               (if (boundary-bonus? target ti) BONUS-BOUNDARY 0)
                               (if (camel-bonus? target ti) BONUS-CAMEL 0)
                               (if (> consecutive 0) BONUS-CONSECUTIVE 0)
                               (if (= pi 0) BONUS-FIRST-CHAR 0))))
                 (loop (+ pi 1) (+ ti 1) (+ score bonus) (+ consecutive 1) #f)))
              ;; Gap
              (else
               (let ((penalty (if in-gap? SCORE-GAP-EXTEND SCORE-GAP-START)))
                 (loop pi (+ ti 1) (+ score penalty) 0 #t)))))))))

(def (fuzzy-match? pattern target)
  "Check if pattern fuzzy-matches target (returns #t/#f)."
  (> (fuzzy-match-score pattern target) 0))

(def (fuzzy-match-positions pattern target)
  "Find character positions in target where pattern characters match.
   Returns list of indices, or [] if no match."
  (let* ((plen (string-length pattern))
         (tlen (string-length target))
         (case-sensitive? (let loop ((i 0))
                           (if (>= i plen) #f
                               (if (char-upper-case? (string-ref pattern i)) #t
                                   (loop (+ i 1)))))))
    (if (= plen 0) []
        (let ((char=
                (if case-sensitive?
                  char=?
                  (lambda (a b) (char=? (char-downcase a) (char-downcase b))))))
          (let loop ((pi 0) (ti 0) (positions []))
            (cond
              ((>= pi plen) (reverse positions))
              ((>= ti tlen) [])  ;; no match
              ((char= (string-ref pattern pi) (string-ref target ti))
               (loop (+ pi 1) (+ ti 1) (cons ti positions)))
              (else
               (loop pi (+ ti 1) positions))))))))

;;;============================================================================
;;; Exact matching helpers
;;;============================================================================

(def (exact-match-score pattern target)
  "Score an exact substring match. Returns score > 0 if found, 0 otherwise."
  (let* ((case-sensitive? (let loop ((i 0))
                           (if (>= i (string-length pattern)) #f
                               (if (char-upper-case? (string-ref pattern i)) #t
                                   (loop (+ i 1))))))
         (hay (if case-sensitive? target (string-downcase target)))
         (needle (if case-sensitive? pattern (string-downcase pattern))))
    (if (string-contains? hay needle)
      (+ SCORE-MATCH (* BONUS-CONSECUTIVE (string-length pattern))
         (let ((pos (string-search-pos hay needle)))
           (if (and pos (boundary-bonus? target pos))
             BONUS-BOUNDARY 0)))
      0)))

(def (prefix-match-score pattern target)
  "Score a prefix match. Returns score > 0 if target starts with pattern, 0 otherwise."
  (let* ((plen (string-length pattern))
         (tlen (string-length target))
         (case-sensitive? (let loop ((i 0))
                           (if (>= i plen) #f
                               (if (char-upper-case? (string-ref pattern i)) #t
                                   (loop (+ i 1)))))))
    (if (< tlen plen) 0
        (let ((hay (if case-sensitive? target (string-downcase target)))
              (needle (if case-sensitive? pattern (string-downcase pattern))))
          (if (and (>= (string-length hay) plen)
                   (string=? (substring hay 0 plen) needle))
            (+ SCORE-MATCH (* BONUS-CONSECUTIVE plen) BONUS-BOUNDARY BONUS-FIRST-CHAR)
            0)))))

(def (suffix-match-score pattern target)
  "Score a suffix match. Returns score > 0 if target ends with pattern, 0 otherwise."
  (let* ((plen (string-length pattern))
         (tlen (string-length target))
         (case-sensitive? (let loop ((i 0))
                           (if (>= i plen) #f
                               (if (char-upper-case? (string-ref pattern i)) #t
                                   (loop (+ i 1)))))))
    (if (< tlen plen) 0
        (let ((hay (if case-sensitive? target (string-downcase target)))
              (needle (if case-sensitive? pattern (string-downcase pattern))))
          (if (string=? (substring hay (- (string-length hay) plen) (string-length hay))
                        needle)
            (+ SCORE-MATCH (* BONUS-CONSECUTIVE plen))
            0)))))

(def (string-search-pos haystack needle)
  "Find position of needle in haystack, or #f."
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))

;;;============================================================================
;;; Extended search syntax parser
;;;============================================================================

;; A search-term is: (type text negate?)
;; type: 'fuzzy, 'exact, 'prefix, 'suffix
(defstruct search-term
  (type text negate?)
  transparent: #t)

(def (parse-search-pattern pattern)
  "Parse an fzf extended search pattern into a list of OR-groups.
   Each OR-group is a list of search-terms that must ALL match (AND).
   The groups are OR'd together: if any group matches, the candidate matches.
   Returns: list of (list of search-term)"
  (let* ((groups (split-or-groups pattern)))
    (map parse-and-group groups)))

(def (split-or-groups str)
  "Split pattern on | (pipe), respecting spaces around |."
  (let loop ((i 0) (start 0) (groups []))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) groups)))
      ;; Look for " | " (space-pipe-space) or standalone |
      ((and (char=? (string-ref str i) #\|)
            ;; Ensure it's not part of a word
            (or (= i 0) (char=? (string-ref str (- i 1)) #\space))
            (or (= (+ i 1) (string-length str))
                (char=? (string-ref str (+ i 1)) #\space)))
       (loop (+ i 1) (+ i 1) (cons (substring str start i) groups)))
      (else (loop (+ i 1) start groups)))))

(def (parse-and-group str)
  "Parse space-separated terms into a list of search-term structs."
  (let ((words (filter (lambda (w) (> (string-length w) 0))
                       (string-split-chars (fuzzy-trim str) " "))))
    (map parse-single-term words)))

(def (parse-single-term word)
  "Parse a single search term with optional modifiers."
  (let* ((negate? (and (> (string-length word) 1)
                       (char=? (string-ref word 0) #\!)))
         (text (if negate? (substring word 1 (string-length word)) word)))
    (cond
      ;; 'exact — exact substring match
      ((and (> (string-length text) 1)
            (char=? (string-ref text 0) #\'))
       (make-search-term 'exact (substring text 1 (string-length text)) negate?))
      ;; ^prefix
      ((and (> (string-length text) 1)
            (char=? (string-ref text 0) #\^))
       (make-search-term 'prefix (substring text 1 (string-length text)) negate?))
      ;; suffix$
      ((and (> (string-length text) 1)
            (char=? (string-ref text (- (string-length text) 1)) #\$))
       (make-search-term 'suffix (substring text 0 (- (string-length text) 1)) negate?))
      ;; default: fuzzy
      (else
       (make-search-term 'fuzzy text negate?)))))

(def (fuzzy-trim str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))

;;;============================================================================
;;; Extended search scoring
;;;============================================================================

(def (score-term term target)
  "Score a single search-term against a target string.
   Returns score > 0 for match, 0 for no match."
  (let ((raw-score
          (case (search-term-type term)
            ((fuzzy) (fuzzy-match-score (search-term-text term) target))
            ((exact) (exact-match-score (search-term-text term) target))
            ((prefix) (prefix-match-score (search-term-text term) target))
            ((suffix) (suffix-match-score (search-term-text term) target))
            (else 0))))
    (if (search-term-negate? term)
      (if (> raw-score 0) 0 SCORE-MATCH)  ;; invert
      raw-score)))

(def (score-and-group group target)
  "Score an AND group: all terms must match. Returns total score or 0."
  (let loop ((terms group) (total 0))
    (cond
      ((null? terms) (if (> total 0) total SCORE-MATCH))
      (else
       (let ((s (score-term (car terms) target)))
         (if (= s 0) 0  ;; AND fails: one term didn't match
             (loop (cdr terms) (+ total s))))))))

(def (fzf-score pattern target)
  "Score target against an fzf extended search pattern string.
   Returns score > 0 for match, 0 for no match."
  (if (= (string-length (fuzzy-trim pattern)) 0)
    SCORE-MATCH  ;; empty pattern matches everything
    (let ((groups (parse-search-pattern pattern)))
      ;; OR: take the best scoring group
      (let loop ((gs groups) (best 0))
        (if (null? gs)
          best
          (let ((s (score-and-group (car gs) target)))
            (loop (cdr gs) (max best s))))))))

(def (fzf-match? pattern target)
  "Check if target matches the fzf extended search pattern."
  (> (fzf-score pattern target) 0))

;;;============================================================================
;;; Batch filtering and sorting
;;;============================================================================

(def (fzf-filter pattern candidates)
  "Filter and sort candidates by fzf score.
   Returns list of (score . candidate) pairs, sorted best-first."
  (let ((scored (filter-map
                  (lambda (cand)
                    (let ((s (fzf-score pattern cand)))
                      (and (> s 0) (cons s cand))))
                  candidates)))
    (sort! scored (lambda (a b) (> (car a) (car b))))
    scored))
