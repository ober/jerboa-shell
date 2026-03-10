;;; expander.ss — Word expansion for gsh
;;; Order: brace -> tilde -> parameter -> command-sub -> arithmetic
;;;        -> process-sub -> word-split -> pathname -> quote-removal

(export #t)
(import :std/sugar
        :std/format
        ./pregexp-compat
        :std/srfi/1
        :gsh/ast
        :gsh/util
        :gsh/environment
        :gsh/ffi
        :gsh/functions
        :gsh/glob
        :gsh/arithmetic)

;;; --- Latin-1 to UTF-8 re-decode ---
;;; ffi-read-all-from-fd uses char-string which decodes bytes as Latin-1.
;;; Re-interpret as UTF-8 when the bytes form valid UTF-8, so that multi-byte
;;; characters (e.g. from printf \xE2\x98\xA0) are correctly decoded.
(def (latin1->utf8 s)
  ;; For ASCII-only strings, Latin-1 and UTF-8 are identical — fast path
  (let ((len (string-length s)))
    (let check ((i 0))
      (if (>= i len) s  ;; all ASCII, return as-is
          (if (< (char->integer (string-ref s i)) 128)
            (check (+ i 1))
            ;; Has non-ASCII: try UTF-8 re-decode
            (with-catch
              (lambda (_) s)  ;; If UTF-8 decode fails, keep Latin-1
              (lambda ()
                (let* ((bytes (list->u8vector (map char->integer (string->list s))))
                       (p (open-input-u8vector (list init: bytes char-encoding: 'UTF-8))))
                  (let loop ((chars '()))
                    (let ((ch (read-char p)))
                      (if (eof-object? ch)
                        (list->string (reverse chars))
                        (loop (cons ch chars)))))))))))))

;;; --- Nounset (set -u) error ---
(def (nounset-error! name env)
  (fprintf (current-error-port) "gsh: ~a: unbound variable~n" name)
  (raise (make-nounset-exception 1)))

;;; --- Double-quote context tracking ---
;; When #t, backslash inside expand-string uses double-quote rules:
;; only $, `, ", \, newline are special; others preserve the backslash.
(def *in-dquote-context* (make-parameter #f))

;; Set to #t when a command substitution runs during expansion.
;; Used by execute-simple-command to decide whether bare assignments
;; should preserve $? from command subs or reset to 0.
(def *command-sub-ran* (make-parameter #f))

;;; --- Modifier segments (preserves quoting info from parameter defaults) ---
;; When a parameter modifier like ${X:-'a b c'} expands a default value,
;; we need to preserve the quoting information so word splitting respects it.
;; A modifier-segments value wraps a list of (text . type) segments.
(def (make-modifier-segments segs) (vector 'modifier-segments segs))
(def (modifier-segments? x) (and (vector? x) (>= (vector-length x) 2)
                                  (eq? (vector-ref x 0) 'modifier-segments)))
(def (modifier-segments-list x) (vector-ref x 1))
(def (segments->string segs)
  (apply string-append (map car segs)))
;; Re-tag literal segments as expanded so they undergo IFS word splitting.
;; Used for modifier default/alternate values where unquoted literal text
;; (e.g. ${Unset:-a b c}) should be split just like a variable expansion.
(def (segments-literals-splittable segs)
  (map (lambda (seg)
         (if (eq? (cdr seg) 'literal)
           (cons (car seg) 'expanded)
           seg))
       segs))

;;; --- Process substitution state ---
;; Accumulates cleanup thunks for process substitutions created during expansion.
;; Caller should bind this and call the cleanups after the command finishes.
(def *procsub-cleanups* (make-parameter []))
(def *procsub-counter* 0)

;; Create a temporary FIFO for process substitution
(def (make-procsub-fifo!)
  (set! *procsub-counter* (+ *procsub-counter* 1))
  (let ((path (string-append "/tmp/gsh-procsub-"
                             (number->string (ffi-getpid)) "-"
                             (number->string *procsub-counter*))))
    (let ((rc (ffi-mkfifo path #o600)))
      (when (< rc 0)
        (error "mkfifo failed" path))
      path)))

;; Run process substitution cleanup: reap processes and remove FIFOs
(def (run-procsub-cleanups!)
  (for-each (lambda (thunk) (thunk)) (*procsub-cleanups*))
  (*procsub-cleanups* []))

;; Expand a process substitution word-process-sub into a FIFO path.
;; Spawns the child process and registers cleanup.
(def (expand-process-sub psub env)
  (let* ((dir (word-process-sub-direction psub))
         (cmd-text (word-process-sub-command psub))
         (path (make-procsub-fifo!))
         ;; For <(cmd): cmd stdout goes to FIFO
         ;; For >(cmd): cmd stdin comes from FIFO
         (redir (if (eq? dir 'in)
                  (string-append "exec >" path "; ")
                  (string-append "exec <" path "; ")))
         ;; Don't redirect stdin/stdout/stderr — child inherits current fds.
         ;; The exec redirect inside the shell command handles the FIFO connection.
         (p (open-process-with-sigpipe
             [path: "/bin/sh"
              arguments: ["-c" (string->c-safe (string-append redir cmd-text))]
              environment: (map string->c-safe (env-exported-alist env))
              directory: (current-directory)
              stdin-redirection: #f
              stdout-redirection: #f
              stderr-redirection: #f])))
    ;; Register cleanup
    (*procsub-cleanups*
     (cons (lambda ()
             (with-catch void (lambda () (process-status p)))
             (with-catch void (lambda () (ffi-unlink path))))
           (*procsub-cleanups*)))
    path))

;;; --- Public interface ---

;; Expand a word (string with shell syntax) to a list of strings
;; Performs full expansion pipeline including word splitting and globbing
(def (expand-word word env)
  (cond
    ((string? word)
     ;; Step 1: Brace expansion (produces multiple words)
     (let ((brace-words (if (env-option? env "braceexpand")
                          (brace-expand word)
                          [word])))
       ;; Step 2: For each brace-expanded word, do remaining expansions
       (let ((result
              (append-map
               (lambda (w)
                 ;; Special handling for "$@" — expands to multiple words
                 (if (word-has-quoted-at? w)
                   (expand-word-with-at w env)
                   ;; Use segment-aware expansion for proper mixed quote handling
                   (let* ((segments (expand-string-segments w env))
                          (all-quoted? (every (lambda (seg) (eq? (cdr seg) 'quoted)) segments))
                          (any-quoted? (any (lambda (seg) (eq? (cdr seg) 'quoted)) segments))
                          ;; Split segments: unquoted parts undergo IFS splitting,
                          ;; quoted parts are preserved
                          (split (split-expanded-segments segments env))
                          ;; Build glob-safe text for single-word case where quoted
                          ;; segments contain glob metacharacters that should be literal.
                          ;; For multi-word (IFS split) results, use text as-is.
                          (glob-text-for
                            (lambda (display-text)
                              (if any-quoted?
                                ;; Rebuild from segments with quoted glob chars escaped
                                (let ((gt (apply string-append
                                            (map (lambda (seg)
                                                   (if (eq? (cdr seg) 'quoted)
                                                     (glob-escape-quoted (car seg))
                                                     (car seg)))
                                                 segments))))
                                  ;; Only use glob-text if it could match the display-text
                                  ;; (they differ only in glob-metachar escaping)
                                  (if (= (length split) 1) gt display-text))
                                display-text)))
                          ;; Glob expansion: only on words from unquoted context
                          ;; split returns (text . has-unquoted?) pairs
                          (globbed (let ((raw (with-catch
                                               (lambda (e)
                                                 (if (and (pair? e) (eq? (car e) 'failglob))
                                                   (begin
                                                     (fprintf (current-error-port) "gsh: ~a: no match~n" (cdr e))
                                                     (raise e))
                                                   (raise e)))
                                               (lambda ()
                                                 (append-map
                                                  (lambda (item)
                                                    (let* ((s (car item))
                                                           (can-glob? (cdr item))
                                                           (gpat (if can-glob? (glob-text-for s) s)))
                                                      (if (and can-glob?
                                                               (glob-pattern? gpat (env-shopt? env "extglob"))
                                                               (not (env-option? env "noglob")))
                                                        (let ((matches (glob-expand gpat
                                                                         (env-shopt? env "dotglob")
                                                                         (env-shopt? env "nullglob")
                                                                         (env-shopt? env "failglob")
                                                                         (env-shopt? env "nocaseglob")
                                                                         (env-shopt? env "extglob")
                                                                         (env-shopt? env "globskipdots"))))
                                                          ;; If no matches and glob text differs from display text,
                                                          ;; return display text (not the escaped version)
                                                          (if (and (= (length matches) 1)
                                                                   (string=? (car matches) gpat)
                                                                   (not (string=? gpat s)))
                                                            [s]  ;; no match — use display text
                                                            matches))
                                                        [s])))
                                                  split)))))
                                    ;; Apply GLOBIGNORE filtering
                                    (let ((gi (env-get env "GLOBIGNORE")))
                                      (if (and gi (not (string=? gi "")))
                                        (glob-ignore-filter raw gi)
                                        raw)))))
                     ;; For quoted words, preserve empty string [""]
                     ;; For unquoted words that expand to empty/nothing, remove them
                     (cond
                       ((null? globbed)
                        (if any-quoted? [""] []))
                       ;; Unquoted word that expanded to single empty string → remove
                       ;; But only if ALL segments expanded to empty (not if IFS split
                       ;; of a non-empty value produced an empty field)
                       ((and (not any-quoted?)
                             (= (length globbed) 1)
                             (string=? (car globbed) "")
                             (every (lambda (seg) (string=? (car seg) "")) segments))
                        [])
                       (else globbed)))))
               brace-words)))
         result)))
    ;; Process substitution: expand to FIFO path
    ((word-process-sub? word)
     [(expand-process-sub word env)])
    (else [word])))

;;; --- Segment-aware expansion ---
;;; Segments are (text . type) where type is:
;;;   'quoted   — from "..." or '...' — no IFS splitting, no globbing
;;;   'literal  — literal unquoted text, tilde, backslash-escape — no IFS splitting, yes globbing
;;;   'expanded — from unquoted $var, $(cmd), $((arith)) — yes IFS splitting, yes globbing

;; Check if a segment type allows IFS splitting
(def (seg-splittable? type) (eq? type 'expanded))
;; Check if a segment type allows glob expansion
(def (seg-globbable? type) (not (eq? type 'quoted)))
;; Escape glob metacharacters in a string so the glob engine treats them as literal.
;; Used for quoted segments mixed into a word that also has globbable parts.
(def (glob-escape-quoted text)
  (let ((len (string-length text))
        (buf (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string buf)
        (let ((ch (string-ref text i)))
          (when (memq ch '(#\* #\? #\[ #\] #\-))
            (display #\\ buf))
          (display ch buf)
          (loop (+ i 1)))))))
(def (expand-string-segments word env)
  (let ((len (string-length word)))
    (let loop ((i 0) (segments []))
      (if (>= i len)
        (reverse segments)
        (let ((ch (string-ref word i)))
          (cond
            ;; Tilde at start — literal (no split, yes glob)
            ((and (= i 0) (char=? ch #\~))
             (let-values (((expanded end) (expand-tilde-in word i env)))
               (loop end (cons (cons expanded 'literal) segments))))
            ;; Dollar expansion — unquoted, subject to splitting
            ((char=? ch #\$)
             ;; Check for unquoted $@ or $* — each positional param is a separate field
             (if (and (< (+ i 1) len)
                      (let ((next (string-ref word (+ i 1))))
                        (or (char=? next #\@) (char=? next #\*))))
               ;; Unquoted $@ or $* — produce word-break separated segments
               (let ((params (env-positional-list env)))
                 (if (null? params)
                   (loop (+ i 2) segments)
                   ;; Build: param1.expanded, word-break, param2.expanded, ...
                   (let param-loop ((rest params) (segs segments) (first? #t))
                     (if (null? rest)
                       (loop (+ i 2) segs)
                       (let ((new-segs (if first?
                                         (cons (cons (car rest) 'expanded) segs)
                                         (cons (cons (car rest) 'expanded)
                                               (cons (cons "" 'word-break) segs)))))
                         (param-loop (cdr rest) new-segs #f))))))
               ;; Normal dollar expansion
               (let-values (((expanded end) (expand-dollar word i env)))
                 (if (modifier-segments? expanded)
                   ;; Modifier returned segments — splice them to preserve quoting
                   (loop end (append (reverse (modifier-segments-list expanded)) segments))
                   (loop end (cons (cons expanded 'expanded) segments))))))
            ;; Backtick command substitution — unquoted
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick word i env)))
               (loop end (cons (cons expanded 'expanded) segments))))
            ;; Backslash escape — quoted (no IFS split, no glob)
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (loop (+ i 2) (cons (cons (string (string-ref word (+ i 1))) 'quoted) segments))
               (loop (+ i 1) (cons (cons "\\" 'literal) segments))))
            ;; Single quote — literal (quoted)
            ((char=? ch #\')
             (let-values (((content end) (read-single-quote word (+ i 1))))
               (loop end (cons (cons content 'quoted) segments))))
            ;; Double quote — contents are quoted (no splitting)
            ((char=? ch #\")
             (let-values (((content end) (expand-double-quote word (+ i 1) env)))
               (loop end (cons (cons content 'quoted) segments))))
            ;; Regular character — literal (quoted, not split)
            (else
             ;; Accumulate consecutive literal chars
             (let lit-loop ((j (+ i 1)))
               (if (and (< j len)
                        (let ((c (string-ref word j)))
                          (not (or (char=? c #\$) (char=? c #\`) (char=? c #\\)
                                   (char=? c #\') (char=? c #\") (char=? c #\~)))))
                 (lit-loop (+ j 1))
                 (loop j (cons (cons (substring word i j) 'literal) segments)))))))))))

;; Split expanded segments according to IFS rules.
;; Input: list of (text . quoted?) pairs
;; Output: list of (text . has-unquoted?) pairs (after IFS splitting)
;; Unquoted segments are split on IFS; quoted segments are not.
;; Adjacent segments join into the same word (no split boundary between them).
(def (split-expanded-segments segments env)
  (if (null? segments)
    [(cons "" #f)]
    (let ((ifs (or (env-get env "IFS") " \t\n")))
      (if (string=? ifs "")
        ;; Empty IFS: no splitting, but respect word-break boundaries from $@/$*
        (let ((has-breaks? (any (lambda (seg) (eq? (cdr seg) 'word-break)) segments)))
          (if (not has-breaks?)
            ;; No word-breaks: original behavior — just concatenate
            (let ((text (apply string-append (map car segments)))
                  (can-glob? (any (lambda (seg) (seg-globbable? (cdr seg))) segments)))
              [(cons text can-glob?)])
            ;; Has word-breaks: group segments between breaks into separate words
            (let group ((segs segments) (cur-parts []) (result []))
              (cond
                ((null? segs)
                 (if (null? cur-parts) (reverse result)
                   (let* ((parts (reverse cur-parts))
                          (can-glob? (any (lambda (seg) (seg-globbable? (cdr seg))) parts))
                          (text (apply string-append (map car parts))))
                     (if (string=? text "") (reverse result)
                       (reverse (cons (cons text can-glob?) result))))))
                ((eq? (cdar segs) 'word-break)
                 (if (null? cur-parts) (group (cdr segs) [] result)
                   (let* ((parts (reverse cur-parts))
                          (can-glob? (any (lambda (seg) (seg-globbable? (cdr seg))) parts))
                          (text (apply string-append (map car parts))))
                     (if (string=? text "") (group (cdr segs) [] result)
                       (group (cdr segs) [] (cons (cons text can-glob?) result))))))
                (else (group (cdr segs) (cons (car segs) cur-parts) result))))))
        ;; Normal IFS splitting
        (let ((words [])      ;; completed words (reversed)
              (current (open-output-string))  ;; current word being built
              (cur-can-glob? #f))  ;; does current word have unquoted content?
          ;; Process each segment
          (let seg-loop ((segs segments)
                         (words [])
                         (current (open-output-string))
                         (cur-can-glob? #f)
                         (word-started? #f))
            (if (null? segs)
              ;; Done: emit final word if anything was accumulated
              (let ((final (get-output-string current)))
                (if (or word-started? (> (string-length final) 0))
                  (reverse (cons (cons final cur-can-glob?) words))
                  (reverse words)))
              (let* ((seg (car segs))
                     (text (car seg))
                     (type (cdr seg)))
                (cond
                  ((eq? type 'word-break)
                   ;; Force word boundary — emit current word if non-empty
                   (let ((w (get-output-string current)))
                     (if (> (string-length w) 0)
                       (seg-loop (cdr segs) (cons (cons w cur-can-glob?) words)
                                 (open-output-string) #f #f)
                       (seg-loop (cdr segs) words (open-output-string) #f #f))))
                  ((not (seg-splittable? type))
                   ;; Quoted segment: append directly to current word, never split
                   (display text current)
                   (seg-loop (cdr segs) words current (or cur-can-glob? (seg-globbable? type))
                             (or word-started? #t)))
                  (else
                  ;; Unquoted segment: apply IFS splitting
                  ;; If text ends with non-ws IFS delimiter and there are more segments,
                  ;; add trailing empty field to force word boundary
                  (let* ((ifs (or (env-get env "IFS") " \t\n"))
                         ;; Check if text ends with ANY IFS delimiter and more segments follow
                         ;; Non-ws IFS: creates empty trailing field (POSIX)
                         ;; Ws IFS: creates word boundary with adjacent content
                         (trailing-ifs-delim?
                           (and (> (string-length text) 0)
                                (pair? (cdr segs))
                                (not (string=? ifs ""))
                                (ifs-char? (string-ref text (- (string-length text) 1)) ifs)))
                         ;; Check if text starts with ANY IFS delimiter and word already started
                         (leading-ifs-delim?
                           (and (> (string-length text) 0)
                                word-started?
                                (not (string=? ifs ""))
                                (ifs-char? (string-ref text 0) ifs)))
                         (raw-split (word-split text env))
                         ;; Only add boundary markers when split produced actual words
                         (split-words (if (and trailing-ifs-delim? (pair? raw-split))
                                       (append raw-split [""])
                                       raw-split))
                         (split-words (if (and leading-ifs-delim?
                                              (pair? split-words)
                                              (not (string=? (car split-words) "")))
                                       (cons "" split-words)
                                       split-words))
                         (n (length split-words)))
                    (cond
                      ;; Empty expansion (all IFS whitespace): create word boundary if we
                      ;; have content accumulated AND there are more segments after this one
                      ((= n 0)
                       (if (and word-started? (pair? (cdr segs))
                                (> (string-length text) 0))
                         ;; IFS whitespace between content — emit current word, start new
                         (let ((w (get-output-string current)))
                           (seg-loop (cdr segs)
                                     (cons (cons w #t) words)
                                     (open-output-string) #f #f))
                         (seg-loop (cdr segs) words current (or cur-can-glob? #t) word-started?)))
                      ;; Single word: append to current (joining with adjacent)
                      ;; Mark word-started if the original text was non-empty (IFS produced an empty field)
                      ((= n 1)
                       (display (car split-words) current)
                       (seg-loop (cdr segs) words current (or cur-can-glob? #t)
                                 (or word-started? (> (string-length (car split-words)) 0)
                                     (> (string-length text) 0))))
                      ;; Multiple words: first joins with current, rest are separate
                      (else
                       (display (car split-words) current)
                       ;; Emit current word
                       (let ((w (get-output-string current)))
                         (let inner-loop ((rest (cdr split-words))
                                          (words (cons (cons w #t) words)))
                           (if (null? (cdr rest))
                             ;; Last split word: start a new current
                             (let ((new-current (open-output-string)))
                               (display (car rest) new-current)
                               (seg-loop (cdr segs) words new-current #t #t))
                             ;; Middle split words: complete words
                             (inner-loop (cdr rest)
                                         (cons (cons (car rest) #t) words))))))))))))))))))

;; Expand a word without word splitting or globbing
;; Used for assignments, here-docs, etc.
(def (expand-word-nosplit word env)
  (if (string? word)
    (expand-string word env)
    word))

;; Expand a word as a glob pattern — quoted glob chars become literal
(def (expand-word-as-pattern word env)
  (if (string? word)
    (expand-pattern word env)
    word))

;; Expand assignment value: like expand-word-nosplit but also expands ~ after :
;; In assignment context, tilde expansion occurs at start AND after each unquoted :
;; But we must not expand tilde inside quotes.
;; Strategy: split on unquoted : first, expand tilde at start of each segment,
;; then expand the rest normally, and rejoin with :
(def (expand-assignment-value word env)
  (if (string? word)
    ;; Split on unquoted colons, expand each segment, rejoin
    (let ((segments (split-assignment-on-colon word)))
      (string-join
       (map (lambda (seg) (expand-string seg env)) segments)
       ":"))
    word))

;; Split an assignment value on unquoted : characters
;; Returns list of segments. Respects single/double quoting and ${...} nesting.
(def (split-assignment-on-colon str)
  (let ((len (string-length str))
        (out (open-output-string))
        (result []))
    (let loop ((i 0) (in-sq? #f) (in-dq? #f) (brace-depth 0))
      (if (>= i len)
        (reverse (cons (get-output-string out) result))
        (let ((ch (string-ref str i)))
          (cond
            ;; Single quote toggle (not in dq, not in ${})
            ((and (char=? ch #\') (not in-dq?))
             (display ch out)
             (if in-sq?
               (loop (+ i 1) #f in-dq? brace-depth)
               (loop (+ i 1) #t in-dq? brace-depth)))
            ;; In single quotes, everything is literal
            (in-sq?
             (display ch out)
             (loop (+ i 1) in-sq? in-dq? brace-depth))
            ;; Double quote toggle
            ((char=? ch #\")
             (display ch out)
             (loop (+ i 1) in-sq? (not in-dq?) brace-depth))
            ;; Backslash escape
            ((and (char=? ch #\\) (< (+ i 1) len))
             (display ch out)
             (display (string-ref str (+ i 1)) out)
             (loop (+ i 2) in-sq? in-dq? brace-depth))
            ;; ${ opens brace depth
            ((and (char=? ch #\$) (< (+ i 1) len) (char=? (string-ref str (+ i 1)) #\{))
             (display ch out)
             (display (string-ref str (+ i 1)) out)
             (loop (+ i 2) in-sq? in-dq? (+ brace-depth 1)))
            ;; } closes brace depth
            ((and (char=? ch #\}) (> brace-depth 0))
             (display ch out)
             (loop (+ i 1) in-sq? in-dq? (- brace-depth 1)))
            ;; Unquoted colon — split only at brace-depth 0
            ((and (char=? ch #\:) (not in-dq?) (= brace-depth 0))
             (set! result (cons (get-output-string out) result))
             (set! out (open-output-string))
             (loop (+ i 1) in-sq? in-dq? brace-depth))
            ;; Everything else
            (else
             (display ch out)
             (loop (+ i 1) in-sq? in-dq? brace-depth))))))))

;; Expand heredoc body: $, `, \ escaping, but NO quote processing (" and ' are literal)
(def (expand-heredoc-body body env)
  (let ((len (string-length body))
        (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref body i)))
          (cond
            ;; Dollar expansion
            ((char=? ch #\$)
             (let-values (((expanded end) (expand-dollar body i env)))
               (display (if (modifier-segments? expanded)
                          (segments->string (modifier-segments-list expanded))
                          expanded)
                        out)
               (loop end)))
            ;; Backtick command substitution
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick body i env)))
               (display expanded out)
               (loop end)))
            ;; Backslash: only escape $, `, \, and newline in heredocs
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (let ((next (string-ref body (+ i 1))))
                 (if (memv next '(#\$ #\` #\\ #\newline))
                   (begin
                     (display next out)
                     (loop (+ i 2)))
                   (begin
                     (display #\\ out)
                     (display next out)
                     (loop (+ i 2)))))
               (begin
                 (display #\\ out)
                 (loop (+ i 1)))))
            ;; Everything else is literal (including " and ')
            (else
             (display ch out)
             (loop (+ i 1)))))))))

;; Expand a list of words, returning a flat list of strings
(def (expand-words words env)
  (append-map (lambda (w) (expand-word w env)) words))

;;; --- String expansion ---

;; Main expansion: processes $, `, ~, and quoting in a string
(def (expand-string str env)
  (let ((len (string-length str))
        (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref str i)))
          (cond
            ;; Tilde at start (not in double-quote context)
            ((and (= i 0) (char=? ch #\~) (not (*in-dquote-context*)))
             (let-values (((expanded end) (expand-tilde-in str i env)))
               (display expanded out)
               (loop end)))
            ;; Dollar expansion
            ((char=? ch #\$)
             (let-values (((expanded end) (expand-dollar str i env)))
               (display (if (modifier-segments? expanded)
                          (segments->string (modifier-segments-list expanded))
                          expanded)
                        out)
               (loop end)))
            ;; Backtick command substitution
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick str i env)))
               (display expanded out)
               (loop end)))
            ;; Backslash escape
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (let ((next (string-ref str (+ i 1))))
                 (if (and (*in-dquote-context*)
                          (not (memq next '(#\$ #\` #\" #\\ #\newline))))
                   ;; In double-quote context, preserve backslash for non-special chars
                   (begin (display "\\" out) (display next out) (loop (+ i 2)))
                   ;; Outside double quotes or special char: consume backslash
                   (begin (display next out) (loop (+ i 2)))))
               (begin
                 (display "\\" out)
                 (loop (+ i 1)))))
            ;; Single quote — literal (but in dquote context, treated as literal char)
            ((char=? ch #\')
             (if (*in-dquote-context*)
               ;; Inside double quotes, single quotes are literal characters
               (begin (display ch out)
                      (loop (+ i 1)))
               (let-values (((content end) (read-single-quote str (+ i 1))))
                 (display content out)
                 (loop end))))
            ;; Double quote — partial expansion
            ((char=? ch #\")
             (let-values (((content end) (expand-double-quote str (+ i 1) env)))
               (display content out)
               (loop end)))
            ;; Regular character
            (else
             (display ch out)
             (loop (+ i 1)))))))))

;; Escape glob-special characters in a string by prefixing with backslash
(def (glob-quote str)
  (let ((len (string-length str))
        (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref str i)))
          (when (memq ch '(#\* #\? #\[ #\]))
            (display "\\" out))
          (display ch out)
          (loop (+ i 1)))))))

;; Expand a string as a pattern — like expand-string but preserves
;; glob-literal distinction: quoted glob chars are backslash-escaped
;; so glob-pattern->pregexp treats them as literals.
(def (expand-pattern str env)
  (let ((len (string-length str))
        (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref str i)))
          (cond
            ;; Tilde at start — expanded text is literal (quote glob chars)
            ((and (= i 0) (char=? ch #\~))
             (let-values (((expanded end) (expand-tilde-in str i env)))
               (display (glob-quote expanded) out)
               (loop end)))
            ;; Dollar expansion — when unquoted in pattern context, preserve
            ;; glob chars so $pat where pat='[ab]*' works as a glob pattern
            ((char=? ch #\$)
             (let-values (((expanded end) (expand-dollar str i env)))
               (display (if (modifier-segments? expanded)
                          (segments->string (modifier-segments-list expanded))
                          expanded)
                        out)
               (loop end)))
            ;; Backtick command substitution — expanded text is literal
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick str i env)))
               (display (glob-quote expanded) out)
               (loop end)))
            ;; Backslash escape — escaped char is literal
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (let ((next (string-ref str (+ i 1))))
                 ;; Emit backslash-escaped char so glob treats it as literal
                 (display "\\" out) (display next out)
                 (loop (+ i 2)))
               (begin
                 (display "\\" out)
                 (loop (+ i 1)))))
            ;; Single quote — content is literal, escape glob chars
            ((char=? ch #\')
             (let-values (((content end) (read-single-quote str (+ i 1))))
               (display (glob-quote content) out)
               (loop end)))
            ;; Double quote — expand $, but escape glob chars in result
            ((char=? ch #\")
             (let-values (((content end) (expand-double-quote str (+ i 1) env)))
               (display (glob-quote content) out)
               (loop end)))
            ;; Regular unquoted character — keep as-is (glob chars stay as glob)
            (else
             (display ch out)
             (loop (+ i 1)))))))))

;;; --- Tilde expansion ---

(def (expand-tilde-in str i env)
  ;; Find end of tilde prefix (up to first / or end)
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (if (or (>= j len) (char=? (string-ref str j) #\/))
                  j
                  (loop (+ j 1)))))
         (prefix (substring str (+ i 1) end)))
    (cond
      ;; ~ alone or ~/...
      ((= (string-length prefix) 0)
       (values (or (env-get env "HOME") "~") end))
      ;; ~+ = $PWD
      ((string=? prefix "+")
       (values (or (env-get env "PWD") "+") end))
      ;; ~- = $OLDPWD
      ((string=? prefix "-")
       (values (or (env-get env "OLDPWD") "-") end))
      ;; ~user
      (else
       (with-catch
        (lambda (e) (values (substring str i end) end))
        (lambda ()
          (values (user-info-home (user-info prefix)) end)))))))

;;; --- Dollar expansion ---

(def (expand-dollar str i env)
  (let ((len (string-length str)))
    (if (>= (+ i 1) len)
      (values "$" (+ i 1))
      (let ((next (string-ref str (+ i 1))))
        (cond
          ;; $(( ... )) — arithmetic
          ((and (char=? next #\()
                (< (+ i 2) len)
                (char=? (string-ref str (+ i 2)) #\())
           (expand-arith-sub str i env))
          ;; $( ... ) — command substitution
          ((char=? next #\()
           (expand-command-sub str i env))
          ;; ${ ... } — parameter expansion
          ((char=? next #\{)
           (expand-parameter-braced str i env))
          ;; $name — simple variable
          ((or (char-alphabetic? next) (char=? next #\_))
           (expand-simple-var str i env))
          ;; $? $$ $! $# $* $@ $- $0-$9
          ((memq next '(#\? #\$ #\! #\# #\* #\@ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
           (let ((val (env-get env (string next))))
             (values (or val "") (+ i 2))))
          ;; $'...' — ANSI-C quoting inside patterns/words
          ((char=? next #\')
           (expand-dollar-ansi-c str (+ i 1) env))
          (else
           (values "$" (+ i 1))))))))

;; ANSI-C quoting $'...' inside patterns and expansions
;; i points to the opening quote character
(def (expand-dollar-ansi-c str i env)
  (let ((len (string-length str))
        (buf (open-output-string)))
    (let loop ((j (+ i 1))) ;; skip opening quote
      (if (>= j len)
        ;; Unterminated — treat $ as literal
        (values "$" i)
        (let ((ch (string-ref str j)))
          (cond
            ((char=? ch #\')
             ;; Closing quote
             (values (get-output-string buf) (+ j 1)))
            ((char=? ch #\\)
             (if (>= (+ j 1) len)
               (begin (display ch buf) (loop (+ j 1)))
               (let ((esc (string-ref str (+ j 1))))
                 (case esc
                   ((#\n) (display #\newline buf) (loop (+ j 2)))
                   ((#\t) (display #\tab buf) (loop (+ j 2)))
                   ((#\r) (display #\return buf) (loop (+ j 2)))
                   ((#\a) (display (integer->char 7) buf) (loop (+ j 2)))
                   ((#\b) (display (integer->char 8) buf) (loop (+ j 2)))
                   ((#\e #\E) (display (integer->char 27) buf) (loop (+ j 2)))
                   ((#\f) (display (integer->char 12) buf) (loop (+ j 2)))
                   ((#\v) (display (integer->char 11) buf) (loop (+ j 2)))
                   ((#\\) (display #\\ buf) (loop (+ j 2)))
                   ((#\') (display #\' buf) (loop (+ j 2)))
                   ((#\") (display #\" buf) (loop (+ j 2)))
                   ((#\0)
                    ;; Octal \0NNN
                    (let oloop ((k (+ j 2)) (val 0) (count 0))
                      (if (and (< k len) (< count 3)
                               (let ((c (string-ref str k)))
                                 (and (char>=? c #\0) (char<=? c #\7))))
                        (oloop (+ k 1)
                               (+ (* val 8) (- (char->integer (string-ref str k)) 48))
                               (+ count 1))
                        (begin (display (integer->char val) buf) (loop k)))))
                   ((#\x)
                    ;; Hex \xHH
                    (let hloop ((k (+ j 2)) (val 0) (count 0))
                      (if (and (< k len) (< count 2)
                               (let ((c (string-ref str k)))
                                 (or (and (char>=? c #\0) (char<=? c #\9))
                                     (and (char>=? c #\a) (char<=? c #\f))
                                     (and (char>=? c #\A) (char<=? c #\F)))))
                        (let ((c (string-ref str k)))
                          (hloop (+ k 1)
                                 (+ (* val 16)
                                    (cond ((char>=? c #\a) (+ 10 (- (char->integer c) 97)))
                                          ((char>=? c #\A) (+ 10 (- (char->integer c) 65)))
                                          (else (- (char->integer c) 48))))
                                 (+ count 1)))
                        (begin (when (> count 0) (display (integer->char val) buf))
                               (loop k)))))
                   (else (display esc buf) (loop (+ j 2)))))))
            (else
             (display ch buf)
             (loop (+ j 1)))))))))

;; Simple variable: $NAME
(def (expand-simple-var str i env)
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (if (and (< j len)
                         (let ((ch (string-ref str j)))
                           (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))))
                  (loop (+ j 1))
                  j)))
         (name (substring str (+ i 1) end))
         (val (env-get env name)))
    (when (and (not val) (env-option? env "nounset"))
      (nounset-error! name env))
    (values (or val "") end)))

;; Braced parameter: ${name} ${name:-default} ${#name} etc.
(def (expand-parameter-braced str i env)
  (let* ((len (string-length str))
         (close (find-matching-brace str (+ i 2)))
         (content (if close
                    (substring str (+ i 2) close)
                    (error "bad substitution: unclosed ${"))))
    (let ((result (expand-parameter-content content env)))
      (values result (+ close 1)))))

;; Expand parameter content (inside ${...})
(def (expand-parameter-content content env)
  (let ((len (string-length content)))
    (cond
      ;; ${#name[@]} or ${#name[*]} — array length
      ((and (> len 1) (char=? (string-ref content 0) #\#))
       (let* ((rest (substring content 1 len))
              (bracket-pos (string-find-char-from rest #\[ 0))
              (close-pos (and bracket-pos
                              (string-find-char-from rest #\] (+ bracket-pos 1)))))
         ;; If there's content after ], it's a bad substitution like ${#A[@]@P}
         (when (and close-pos (< (+ close-pos 1) (string-length rest)))
           (error (format "bad substitution")))
         (if (and bracket-pos close-pos
                  (let ((subscript (substring rest (+ bracket-pos 1) close-pos)))
                    (or (string=? subscript "@") (string=? subscript "*"))))
           ;; ${#name[@]} — number of elements
           (let ((name (substring rest 0 bracket-pos)))
             (number->string (env-array-length env name)))
           ;; ${#name} — string length (or ${#name[idx]} — element length)
           (if bracket-pos
             ;; ${#name[idx]} — length of specific element
             (let* ((name (substring rest 0 bracket-pos))
                    (idx (substring rest (+ bracket-pos 1)
                                   (or close-pos (string-length rest))))
                    (val (env-array-get env name (expand-word-nosplit idx env))))
               (number->string (string-length val)))
             ;; ${#name} — string length (code points, or bytes under LC_ALL=C)
             ;; Reject ${#name<modifier>} combinations like ${#x-default}
             (begin
               (when (let check ((j 0))
                       (and (< j (string-length rest))
                            (let ((ch (string-ref rest j)))
                              (if (or (char-alphabetic? ch) (char-numeric? ch)
                                      (char=? ch #\_))
                                (check (+ j 1))
                                #t))))  ;; found non-identifier char
                 (error (format "bad substitution: ${#~a}" rest)))
               (let* ((val (env-get env rest))
                      (val (or val (begin
                                    (when (env-option? env "nounset")
                                      (nounset-error! rest env))
                                    "")))
                      ;; Under C/POSIX locale, return byte count
                      (lc-all (env-get env "LC_ALL"))
                      (byte-locale? (and lc-all
                                         (or (string=? lc-all "C")
                                             (string=? lc-all "POSIX")))))
                 (number->string (if byte-locale?
                                   (utf8-byte-count val)
                                   (utf8-string-length val)))))))))
      ;; ${!name[@]} or ${!name[*]} — array keys/indices
      ;; Also handle ${!name} — indirect expansion
      ((and (> len 1)
            (or (char=? (string-ref content 0) #\!)
                (and (> len 2) (char=? (string-ref content 0) #\\)
                     (char=? (string-ref content 1) #\!))))
       (let* ((name-start (if (char=? (string-ref content 0) #\!) 1 2))
              (rest (substring content name-start len))
              (bracket-pos (string-find-char-from rest #\[ 0)))
         (if (and bracket-pos
                  (let ((close (string-find-char-from rest #\] (+ bracket-pos 1))))
                    (and close
                         (let ((subscript (substring rest (+ bracket-pos 1) close)))
                           (or (string=? subscript "@") (string=? subscript "*"))))))
           ;; ${!name[@]} or ${!name[*]} — array keys, possibly with @op suffix
           (let* ((close (string-find-char-from rest #\] (+ bracket-pos 1)))
                  (name (substring rest 0 bracket-pos))
                  ;; Check for transform suffix after ], e.g. @a in ${!A[@]@a}
                  (after-bracket (substring rest (+ close 1) (string-length rest)))
                  (has-at-op? (and (> (string-length after-bracket) 0)
                                   (char=? (string-ref after-bracket 0) #\@)))
                  (at-op-char (and has-at-op? (> (string-length after-bracket) 1)
                                   (string-ref after-bracket 1)))
                  (keys (env-array-keys env name)))
             ;; Apply transform to keys if @a suffix present
             (let ((transformed-keys
                    (if (and has-at-op? at-op-char (char=? at-op-char #\a))
                      ;; @a: get attributes of each key-as-variable-name
                      (map (lambda (k)
                             (let ((k-str (if (string? k) k (number->string k))))
                               (get-var-attributes k-str env)))
                           keys)
                      keys)))
               (if (null? transformed-keys)
                 ""
                 (make-modifier-segments
                  (let eloop ((rest transformed-keys) (segs []) (first? #t))
                    (if (null? rest)
                      (reverse segs)
                      (let ((new-segs (if first?
                                       (cons (cons (car rest) 'expanded) segs)
                                       (cons (cons (car rest) 'expanded)
                                             (cons (cons "" 'word-break) segs)))))
                        (eloop (cdr rest) new-segs #f))))))))
           ;; Check for ${!prefix@} or ${!prefix*} — variable name matching
           (let* ((rlen (string-length rest))
                  (last-ch (and (> rlen 0) (string-ref rest (- rlen 1)))))
             (if (and last-ch (or (char=? last-ch #\@) (char=? last-ch #\*)))
               ;; ${!prefix@} or ${!prefix*} — list variable names with matching prefix
               (let* ((prefix (substring rest 0 (- rlen 1)))
                      (names (env-matching-names env prefix)))
                 ;; Return as separate words when @ suffix, or IFS-empty for *
                 (if (null? names)
                   ""
                   (if (char=? last-ch #\@)
                     (make-modifier-segments
                      (let eloop ((rest names) (segs []) (first? #t))
                        (if (null? rest)
                          (reverse segs)
                          (let ((new-segs (if first?
                                           (cons (cons (car rest) 'expanded) segs)
                                           (cons (cons (car rest) 'expanded)
                                                 (cons (cons "" 'word-break) segs)))))
                            (eloop (cdr rest) new-segs #f)))))
                     ;; ${!prefix*} — join with first char of IFS
                     (let* ((ifs (or (env-get env "IFS") " \t\n"))
                            (sep (if (> (string-length ifs) 0)
                                   (string (string-ref ifs 0))
                                   "")))
                       (string-join-with sep names)))))
               ;; ${!name} — indirect expansion, possibly with modifiers
               (let-values (((iname modifier arg) (parse-parameter-modifier rest)))
                 (let* ((ref-name (env-get env iname))
                        ;; Bash error: indirect expansion of unset variable
                        (_ (when (not ref-name)
                             (fprintf (current-error-port) "gsh: ~a: invalid indirect expansion~n" iname)
                             (raise (make-nounset-exception 1))))
                        ;; Validate ref-name is a valid variable name (or array ref)
                        ;; Bad names like "bad var name" or "/" must raise error
                        (_ (when (and ref-name
                                      (not (valid-indirect-ref? ref-name)))
                             (error (format "bad substitution: ${!~a}" iname))))
                        ;; If ref-name contains [, it's an array subscript like a[0] or a[@]
                        (bracket-pos (and ref-name (find-array-subscript-start ref-name)))
                        (val (cond
                               ((not ref-name) #f)
                               ;; For @a with indirect array ref like ${!r@a} where r='a[0]',
                               ;; return attributes of the base variable, not the element value
                               ((and bracket-pos (eq? modifier 'at-op)
                                     (string=? arg "a"))
                                (let ((base-name (substring ref-name 0 bracket-pos)))
                                  (get-var-attributes base-name env)))
                               (bracket-pos
                                ;; Resolve indirect to array element/slice
                                ;; Build the full content with any modifier appended
                                (let* ((indirect-content
                                        (if modifier
                                          (string-append ref-name
                                            (case modifier
                                              ((-) (string-append "-" arg))
                                              ((:-) (string-append ":-" arg))
                                              ((+) (string-append "+" arg))
                                              ((:+) (string-append ":+" arg))
                                              ((=) (string-append "=" arg))
                                              ((:=) (string-append ":=" arg))
                                              ((?) (string-append "?" arg))
                                              ((:?) (string-append ":?" arg))
                                              (else "")))
                                          ref-name))
                                       (bp (find-array-subscript-start indirect-content)))
                                  (expand-array-parameter indirect-content bp env)))
                               (else (env-get env ref-name)))))
                   (cond
                     ;; @a with indirect array ref already computed above
                     ((and bracket-pos (eq? modifier 'at-op) (string=? arg "a")) val)
                     ;; Array path already handled modifiers
                     (bracket-pos val)
                     (modifier
                      (apply-parameter-modifier val (or ref-name iname) modifier arg env))
                     (else (or val ""))))))))))
      (else
       ;; Check for array subscript: name[idx] or name[@] or name[*]
       (let ((bracket-pos (find-array-subscript-start content)))
         (if bracket-pos
           (expand-array-parameter content bracket-pos env)
           ;; Regular parameter — find modifier
           (let-values (((name modifier arg) (parse-parameter-modifier content)))
             (let ((val (env-get env name)))
               ;; nounset check: only for bare ${name} or modifiers that don't provide defaults
               (when (and (not val) (env-option? env "nounset")
                          (not (memq modifier '(:- - := = :+ + :? ?))))
                 (nounset-error! name env))
               ;; For @ and * with transform modifiers, apply per-element
               (if (and (or (string=? name "@") (string=? name "*"))
                        modifier
                        (memq modifier '(% %% prefix-short prefix-long
                                         ^^ ^ lc-all lc-first / // at-op)))
                 (let* ((params (env-positional-list env))
                        (transformed (map (lambda (p)
                                           (apply-parameter-modifier p name modifier arg env))
                                         params)))
                   (string-join-with " " transformed))
                 (apply-parameter-modifier val name modifier arg env))))))))))

;; Find the start of an array subscript [idx] in parameter content.
;; Returns position of [ or #f. Must be preceded by a valid name.
(def (find-array-subscript-start content)
  (let ((len (string-length content)))
    (let loop ((i 0))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref content i) #\[)
         ;; Check that the part before [ is a valid name
         (if (and (> i 0)
                  (let loop2 ((j 0))
                    (or (= j i)
                        (and (let ((ch (string-ref content j)))
                               (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))
                             (loop2 (+ j 1))))))
           i
           #f))
        (else (loop (+ i 1)))))))

;; Expand an array parameter like name[idx], name[@], name[*]
;; bracket-pos is the position of [ in content.
;; Handles modifiers after the ] too (e.g. ${arr[@]:-default}).
(def (expand-array-parameter content bracket-pos env)
  (let* ((name (substring content 0 bracket-pos))
         (len (string-length content))
         (close (string-find-char-from content #\] (+ bracket-pos 1))))
    (if (not close)
      ;; No closing ] — treat as regular parameter
      (let-values (((pname modifier arg) (parse-parameter-modifier content)))
        (let ((val (env-get env pname)))
          (apply-parameter-modifier val pname modifier arg env)))
      ;; Extract subscript and any trailing modifier
      (let* ((subscript (substring content (+ bracket-pos 1) close))
             (after-bracket (substring content (+ close 1) len)))
        (cond
          ;; ${name[@]} — all elements as separate words
          ((and (string=? subscript "@") (string=? after-bracket ""))
           (let ((vals (env-array-values env name)))
             (if (null? vals)
               ""
               ;; Return modifier-segments with word-break between elements
               (make-modifier-segments
                (let eloop ((rest vals) (segs []) (first? #t))
                  (if (null? rest)
                    (reverse segs)
                    (let ((new-segs (if first?
                                     (cons (cons (car rest) 'expanded) segs)
                                     (cons (cons (car rest) 'expanded)
                                           (cons (cons "" 'word-break) segs)))))
                      (eloop (cdr rest) new-segs #f))))))))
          ;; ${name[*]} — all elements joined by first char of IFS
          ((and (string=? subscript "*") (string=? after-bracket ""))
           (let* ((vals (env-array-values env name))
                  (ifs (or (env-get env "IFS") " \t\n"))
                  (sep (if (> (string-length ifs) 0)
                         (string (string-ref ifs 0))
                         "")))
             ;; When IFS is empty, bash preserves element boundaries
             (if (and (string=? sep "") (not (null? vals)))
               (make-modifier-segments
                (let eloop ((rest vals) (segs []) (first? #t))
                  (if (null? rest)
                    (reverse segs)
                    (let ((new-segs (if first?
                                     (cons (cons (car rest) 'expanded) segs)
                                     (cons (cons (car rest) 'expanded)
                                           (cons (cons "" 'word-break) segs)))))
                      (eloop (cdr rest) new-segs #f)))))
               (string-join-with sep vals))))
          ;; ${name[@]modifier} or ${name[*]modifier} — with modifier
          ((and (or (string=? subscript "@") (string=? subscript "*"))
                (> (string-length after-bracket) 0))
           ;; Parse the modifier from after-bracket
           (let* ((vals (env-array-values env name)))
             (let-values (((mname modifier arg)
                           (parse-parameter-modifier
                            (string-append "x" after-bracket))))
               ;; For @, conditionals apply to whole array, transforms per-element
               (if (string=? subscript "@")
                 (if (memq modifier '(- :- + :+ = := ? :?))
                   ;; Conditional modifier: treat array as whole
                   (let ((val (if (null? vals) #f (string-join-with " " vals))))
                     ;; For :- and :+, empty array also triggers default/alternate
                     (apply-parameter-modifier
                       (if (and (memq modifier '(:- :+ := :?)) (null? vals)) #f val)
                       name modifier arg env))
                   ;; Transform modifier: apply per-element, return as separate words
                   (let ((modified-vals
                          (map (lambda (v)
                                 (apply-parameter-modifier v name modifier arg env))
                               vals)))
                     (if (null? modified-vals)
                       ""
                       (make-modifier-segments
                        (let eloop ((rest modified-vals) (segs []) (first? #t))
                          (if (null? rest)
                            (reverse segs)
                            (let ((new-segs (if first?
                                             (cons (cons (car rest) 'expanded) segs)
                                             (cons (cons (car rest) 'expanded)
                                                   (cons (cons "" 'word-break) segs)))))
                              (eloop (cdr rest) new-segs #f))))))))
                 ;; For *, transform per-element for at-op, otherwise join first then apply
                 (if (eq? modifier 'at-op)
                   ;; at-op (@Q, @a, @P etc): transform each element, then join
                   (let* ((modified-vals
                           (map (lambda (v)
                                  (apply-parameter-modifier v name modifier arg env))
                                vals))
                          (ifs (or (env-get env "IFS") " \t\n"))
                          (sep (if (> (string-length ifs) 0) (string (string-ref ifs 0)) "")))
                     (string-join-with sep modified-vals))
                   (let* ((ifs (or (env-get env "IFS") " \t\n"))
                          (sep (if (> (string-length ifs) 0) (string (string-ref ifs 0)) ""))
                          (joined (string-join-with sep vals)))
                     ;; For colon variants with 2+ elements in UNQUOTED context:
                     ;; array is non-null even if joined result is "" (analogous to
                     ;; $* with 2+ params being non-null). Convert colon to non-colon
                     ;; so only truly unset triggers default.
                     ;; In QUOTED context, check the joined string value as-is.
                     (let ((effective-modifier
                            (if (and (not (*in-dquote-context*))
                                     (>= (length vals) 2))
                              (case modifier
                                ((:-)  '-) ((:+)  '+) ((:=)  '=) ((:?)  '?)
                                (else modifier))
                              modifier)))
                       (apply-parameter-modifier (if (null? vals) #f joined)
                                                name effective-modifier arg env))))))))
          ;; ${name[idx]} — single element access
          (else
           (let ((expanded-idx
                  ;; Evaluate subscript as arithmetic (supports side effects like b=2)
                  ;; For associative arrays, use string expansion (key is a string)
                  (let ((var (env-get-var env name)))
                    (if (and var (shell-var-assoc? var))
                      (expand-word-nosplit subscript env)
                      (number->string (slice-arith-eval subscript env))))))
             (if (string=? after-bracket "")
               ;; Simple element access
               (env-array-get env name expanded-idx)
               ;; Element access with modifier: ${arr[0]:-default}
               (let* ((raw-val (env-array-get env name expanded-idx))
                      ;; Distinguish unset (-> #f) from empty string (-> "")
                      (val (if (env-array-element-set? env name expanded-idx)
                             raw-val
                             #f)))
                 (let-values (((mname modifier arg)
                               (parse-parameter-modifier
                                (string-append "x" after-bracket))))
                   ;; For = and := modifiers on array elements, use
                   ;; env-array-set! instead of env-set! (which sets scalar)
                   (if (and (memq modifier '(= :=))
                            (or (not val)
                                (and (eq? modifier ':=) val (string=? val ""))))
                     (let ((default (expand-string arg env)))
                       (env-array-set! env name expanded-idx default)
                       default)
                     (apply-parameter-modifier val name modifier arg env))))))))))))

;; Parse NAME and modifier from parameter content
(def (special-param-char? ch)
  (or (char=? ch #\@) (char=? ch #\*)
      (char=? ch #\?) (char=? ch #\!)
      (char=? ch #\$) (char=? ch #\-)))

;; Find the separator slash in str for patsub.
;; Scans from pattern-start with quote awareness, skipping the first
;; "unit" (character or quoted group) to ensure the pattern gets at least
;; one char. Returns the index of the separator slash, or #f if not found.
(def (find-patsub-separator str pattern-start)
  (let ((len (string-length str)))
    ;; First, skip one unit (char or quoted group) to consume the minimal pattern
    (let ((past-first
           (cond
             ((>= pattern-start len) pattern-start)
             ((char=? (string-ref str pattern-start) #\')
              ;; Skip single-quoted group
              (let inner ((j (+ pattern-start 1)))
                (cond ((>= j len) j)
                      ((char=? (string-ref str j) #\') (+ j 1))
                      (else (inner (+ j 1))))))
             ((char=? (string-ref str pattern-start) #\")
              ;; Skip double-quoted group
              (let inner ((j (+ pattern-start 1)))
                (cond ((>= j len) j)
                      ((char=? (string-ref str j) #\") (+ j 1))
                      ((char=? (string-ref str j) #\\) (inner (+ j 2)))
                      (else (inner (+ j 1))))))
             ((char=? (string-ref str pattern-start) #\\)
              ;; Skip escaped char
              (+ pattern-start 2))
             (else (+ pattern-start 1)))))
      ;; Now search for unquoted slash from past-first onwards
      (let loop ((i past-first))
        (cond
          ((>= i len) #f)
          ((char=? (string-ref str i) #\')
           (let inner ((j (+ i 1)))
             (cond ((>= j len) #f)
                   ((char=? (string-ref str j) #\') (loop (+ j 1)))
                   (else (inner (+ j 1))))))
          ((char=? (string-ref str i) #\")
           (let inner ((j (+ i 1)))
             (cond ((>= j len) #f)
                   ((char=? (string-ref str j) #\") (loop (+ j 1)))
                   ((char=? (string-ref str j) #\\) (inner (+ j 2)))
                   (else (inner (+ j 1))))))
          ((char=? (string-ref str i) #\\)
           (loop (+ i 2)))
          ((char=? (string-ref str i) #\/) i)
          (else (loop (+ i 1))))))))

;; Parse modifier from a rest string starting after the name.
;; Returns (values modifier arg) or #f if no modifier found.
(def (parse-modifier-from-rest rest)
  (let ((rlen (string-length rest)))
    (if (= rlen 0) (values #f "")
      (let ((mod-ch (string-ref rest 0)))
        (cond
          ((char=? mod-ch #\:)
           (if (> rlen 1)
             (let ((mod2 (string-ref rest 1)))
               (case mod2
                 ((#\- #\+ #\= #\?)
                  (values (string->symbol (string #\: mod2))
                          (substring rest 2 rlen)))
                 (else
                  (values ': (substring rest 1 rlen)))))
             ;; ${name:} — colon with nothing after is bad substitution
             (error "bad substitution")))
          ((memq mod-ch '(#\- #\+ #\= #\?))
           (values (string->symbol (string mod-ch))
                   (substring rest 1 rlen)))
          ((char=? mod-ch #\%)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\%))
             (values '%% (substring rest 2 rlen))
             (values '% (substring rest 1 rlen))))
          ((char=? mod-ch #\#)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\#))
             (values 'prefix-long (substring rest 2 rlen))
             (values 'prefix-short (substring rest 1 rlen))))
          ((char=? mod-ch #\/)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\/))
             ;; ${name//pattern/replacement} — global replace
             ;; Pattern starts at pos 2; find separator after first pattern unit
             (let ((sep (find-patsub-separator rest 2)))
               (if sep
                 (values '// (cons (substring rest 2 sep)
                                   (substring rest (+ sep 1) rlen)))
                 (values '// (cons (substring rest 2 rlen) ""))))
             ;; ${name/pattern/replacement} — single replace
             ;; Pattern starts at pos 1; find separator after first pattern unit
             (let ((sep (find-patsub-separator rest 1)))
               (if sep
                 (values '/ (cons (substring rest 1 sep)
                                  (substring rest (+ sep 1) rlen)))
                 (values '/ (cons (substring rest 1 rlen) ""))))))
          ((char=? mod-ch #\^)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\^))
             (values '^^ (substring rest 2 rlen))
             (values '^ (substring rest 1 rlen))))
          ((char=? mod-ch #\,)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\,))
             (values 'lc-all (substring rest 2 rlen))
             (values 'lc-first (substring rest 1 rlen))))
          ((char=? mod-ch #\@)
           ;; ${var@operator} — Q, a, P, u, U, L, K, E etc.
           (if (> rlen 1)
             (values 'at-op (substring rest 1 rlen))
             (values #f "")))
          (else (values #f "")))))))

(def (parse-parameter-modifier content)
  (let ((len (string-length content)))
    ;; Handle special parameters: @, *, ?, !, $, -, 0-9
    ;; These are single-char names — modifier starts at position 1
    (if (and (> len 0)
             (let ((ch (string-ref content 0)))
               (or (special-param-char? ch)
                   (and (char-numeric? ch)
                        (or (= len 1)
                            (not (char-numeric? (string-ref content 1))))))))
      (let* ((name (string (string-ref content 0)))
             (rest (substring content 1 len)))
        (let-values (((modifier arg) (parse-modifier-from-rest rest)))
          (if modifier
            (values name modifier arg)
            (values name #f ""))))
    ;; Find the modifier operator
    (let loop ((i 0))
      (cond
        ((>= i len) (values content #f ""))
        ((memq (string-ref content i) '(#\: #\% #\# #\/ #\^ #\, #\@))
         (let ((ch (string-ref content i)))
           (case ch
             ((#\:)
              (if (< (+ i 1) len)
                (let ((mod-ch (string-ref content (+ i 1))))
                  (case mod-ch
                    ((#\- #\+ #\= #\?)
                     (values (substring content 0 i)
                             (string->symbol (string #\: mod-ch))
                             (substring content (+ i 2) len)))
                    (else
                     ;; ${name:offset} or ${name:offset:length}
                     (values (substring content 0 i) ':
                             (substring content (+ i 1) len)))))
                ;; ${name:} — colon at end with nothing after is bad substitution
                (error (format "bad substitution: ${~a}" content))))
             ((#\%)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\%))
                (values (substring content 0 i) '%%
                        (substring content (+ i 2) len))
                (values (substring content 0 i) '%
                        (substring content (+ i 1) len))))
             ((#\#)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\#))
                (values (substring content 0 i) 'prefix-long
                        (substring content (+ i 2) len))
                (values (substring content 0 i) 'prefix-short
                        (substring content (+ i 1) len))))
             ((#\/)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\/))
                ;; ${name//pattern/replacement} — global replace
                ;; Pattern starts at i+2; find separator after first pattern unit
                (let ((sep (find-patsub-separator content (+ i 2))))
                  (if sep
                    (values (substring content 0 i) '//
                            (cons (substring content (+ i 2) sep)
                                  (substring content (+ sep 1) len)))
                    (values (substring content 0 i) '//
                            (cons (substring content (+ i 2) len) ""))))
                ;; ${name/pattern/replacement} — single replace
                ;; Pattern starts at i+1; find separator after first pattern unit
                (let ((sep (find-patsub-separator content (+ i 1))))
                  (if sep
                    (values (substring content 0 i) '/
                            (cons (substring content (+ i 1) sep)
                                  (substring content (+ sep 1) len)))
                    (values (substring content 0 i) '/
                            (cons (substring content (+ i 1) len) ""))))))
             ((#\^)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\^))
                (values (substring content 0 i) '^^
                        (substring content (+ i 2) len))
                (values (substring content 0 i) '^
                        (substring content (+ i 1) len))))
             ((#\,)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\,))
                (values (substring content 0 i) 'lc-all
                        (substring content (+ i 2) len))
                (values (substring content 0 i) 'lc-first
                        (substring content (+ i 1) len))))
             ((#\@)
              (values (substring content 0 i) 'at-op
                      (substring content (+ i 1) len)))
             (else (loop (+ i 1))))))
        ;; Check for uncolon modifiers: - + = ?
        ((and (memq (string-ref content i) '(#\- #\+ #\= #\?))
              ;; Only if this is the first non-name char
              (let loop2 ((j 0))
                (or (= j i)
                    (and (let ((ch (string-ref content j)))
                           (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))
                         (loop2 (+ j 1))))))
         (values (substring content 0 i)
                 (string->symbol (string (string-ref content i)))
                 (substring content (+ i 1) len)))
        ;; If character is not a valid name char and not a known operator,
        ;; this is a bad substitution (e.g. ${a&})
        ((not (let ((ch (string-ref content i)))
                (or (char-alphabetic? ch) (char-numeric? ch)
                    (char=? ch #\_) (char=? ch #\[) (char=? ch #\])
                    (char=? ch #\!) (char=? ch #\*))))
         (error (format "~a: bad substitution" (string-append "${" content "}"))))
        (else (loop (+ i 1))))))))

;; Pattern substitution helpers for ${var/pattern/repl} and ${var//pattern/repl}
;; Returns string with first match of glob pattern replaced
(def (pattern-substitute-first val pattern replacement (extglob? #f))
  (let ((vlen (string-length val)))
    (cond
      ;; ${var/#pattern/repl} — anchor to start (prefix match)
      ((and (> (string-length pattern) 0) (char=? (string-ref pattern 0) #\#))
       (let ((pat (substring pattern 1 (string-length pattern))))
         ;; Try longest match from start
         (let loop ((i vlen))
           (if (< i 0)
             val
             (if (glob-match? pat (substring val 0 i) #f extglob?)
               (string-append replacement (substring val i vlen))
               (loop (- i 1)))))))
      ;; ${var/%pattern/repl} — anchor to end (suffix match)
      ((and (> (string-length pattern) 0) (char=? (string-ref pattern 0) #\%))
       (let ((pat (substring pattern 1 (string-length pattern))))
         ;; Try longest match from end
         (let loop ((i 0))
           (if (> i vlen)
             val
             (if (glob-match? pat (substring val i vlen) #f extglob?)
               (string-append (substring val 0 i) replacement)
               (loop (+ i 1)))))))
      ;; Unanchored — find first match anywhere
      (else
       (let loop ((start 0))
         (if (> start vlen)
           val
           ;; Try all end positions from longest to shortest
           (let inner ((end vlen))
             (cond
               ((< end start)
                ;; No match at this start position, try next
                (loop (+ start 1)))
               ((glob-match? pattern (substring val start end) #f extglob?)
                ;; Found a match
                (string-append (substring val 0 start)
                               replacement
                               (substring val end vlen)))
               (else (inner (- end 1)))))))))))

;; Returns string with all matches of glob pattern replaced
(def (pattern-substitute-all val pattern replacement (extglob? #f))
  (let ((vlen (string-length val)))
    (if (string=? pattern "")
      val  ;; Empty pattern — no replacement
      (let ((buf (open-output-string)))
        (let loop ((start 0))
          (if (> start vlen)
            (get-output-string buf)
            ;; Try all end positions from longest to shortest
            (let inner ((end vlen))
              (cond
                ((< end (+ start 1))
                 ;; No match at this start, emit char and move on
                 (if (< start vlen)
                   (begin
                     (display (string-ref val start) buf)
                     (loop (+ start 1)))
                   (get-output-string buf)))
                ((glob-match? pattern (substring val start end) #f extglob?)
                 ;; Found match — emit replacement and skip past match
                 (display replacement buf)
                 (if (= end start)
                   ;; Zero-length match — advance by one to avoid infinite loop
                   (begin
                     (when (< start vlen)
                       (display (string-ref val start) buf))
                     (loop (+ start 1)))
                   (loop end)))
                (else (inner (- end 1)))))))))))

;; Take elements from index start to end of a list
(def (take-sublist lst start end)
  (let loop ((l lst) (i 0) (result []))
    (cond
      ((or (null? l) (>= i end)) (reverse result))
      ((< i start) (loop (cdr l) (+ i 1) result))
      (else (loop (cdr l) (+ i 1) (cons (car l) result))))))

;; Shell-quote a value for ${var@Q} — bash-compatible quoting
;; Bash ALWAYS quotes: empty → '', control chars → $'...' notation,
;; otherwise single-quote style: 'text' with ' escaped as '\''.
(def (shell-quote-for-at-q s)
  (if (string=? s "")
    "''"
    (let ((len (string-length s)))
      (if (string-has-control-chars? s)
        ;; Use $'...' notation for strings with control chars
        (let ((buf (open-output-string)))
          (display "$'" buf)
          (let loop ((i 0))
            (when (< i len)
              (let* ((ch (string-ref s i))
                     (code (char->integer ch)))
                (cond
                  ((char=? ch #\newline) (display "\\n" buf))
                  ((char=? ch #\tab) (display "\\t" buf))
                  ((char=? ch #\return) (display "\\r" buf))
                  ((char=? ch #\\) (display "\\\\" buf))
                  ((char=? ch #\') (display "\\'" buf))
                  ((or (< code #x20) (= code #x7f))
                   ;; Use octal \NNN (matching bash $'...' output)
                   (let ((oct (number->string code 8)))
                     (display (string-append "\\" (make-string (- 3 (string-length oct)) #\0) oct) buf)))
                  (else (display ch buf))))
              (loop (+ i 1))))
          (display "'" buf)
          (get-output-string buf))
        ;; Use single-quote style: 'text' with ' escaped as '\''
        (let ((buf (open-output-string)))
          (display "'" buf)
          (let loop ((i 0))
            (when (< i len)
              (let ((ch (string-ref s i)))
                (if (char=? ch #\')
                  (display "'\\''" buf)
                  (display ch buf)))
              (loop (+ i 1))))
          (display "'" buf)
          (get-output-string buf))))))


;; Get variable attribute flags for ${var@a}
;; Alphabetical order matching bash: A a i l n r u x
(def (get-var-attributes name env)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (if (not var) ""
      (string-append
        (if (shell-var-assoc? var) "A" "")
        (if (shell-var-array? var) "a" "")
        (if (shell-var-integer? var) "i" "")
        (if (shell-var-lowercase? var) "l" "")
        (if (shell-var-nameref? var) "n" "")
        (if (shell-var-readonly? var) "r" "")
        (if (shell-var-uppercase? var) "u" "")
        (if (shell-var-exported? var) "x" "")))))

;; Get declare prefix for ${var@A} — returns "declare " with flags, or just ""
(def (get-var-declare-prefix name env)
  (let ((attrs (get-var-attributes name env)))
    (if (string=? attrs "")
      "declare -- "
      (string-append "declare -" attrs " "))))

;; Expand ANSI-C escape sequences for ${var@E} — subset of $'...' escapes
(def (expand-ansi-c-escapes s)
  (let ((buf (open-output-string))
        (len (string-length s)))
    (let loop ((i 0))
      (if (>= i len) (get-output-string buf)
        (let ((ch (string-ref s i)))
          (if (and (char=? ch #\\) (< (+ i 1) len))
            (let ((esc (string-ref s (+ i 1))))
              (case esc
                ((#\n) (display #\newline buf) (loop (+ i 2)))
                ((#\t) (display #\tab buf) (loop (+ i 2)))
                ((#\r) (display #\return buf) (loop (+ i 2)))
                ((#\a) (display (integer->char 7) buf) (loop (+ i 2)))
                ((#\b) (display (integer->char 8) buf) (loop (+ i 2)))
                ((#\e #\E) (display (integer->char 27) buf) (loop (+ i 2)))
                ((#\f) (display (integer->char 12) buf) (loop (+ i 2)))
                ((#\v) (display (integer->char 11) buf) (loop (+ i 2)))
                ((#\\) (display #\\ buf) (loop (+ i 2)))
                ((#\') (display #\' buf) (loop (+ i 2)))
                ((#\") (display #\" buf) (loop (+ i 2)))
                (else (display ch buf) (display esc buf) (loop (+ i 2)))))
            (begin (display ch buf) (loop (+ i 1)))))))))

;; Apply parameter modifier
;; Evaluate a string as an arithmetic expression for slice offset/length.
;; Bash evaluates ${var:expr:expr} offset and length as arithmetic.
(def (slice-arith-eval str env)
  (let ((trimmed (string-trim-whitespace-str str)))
    (if (string=? trimmed "")
      0
      ;; Try simple number first for speed
      (or (string->number trimmed)
          (arith-eval (parameterize ((*in-dquote-context* #t))
                        (expand-arith-expr trimmed env))
                      (arith-env-getter env)
                      (arith-env-setter env)
                      (and (env-option? env "nounset")
                           (lambda (name)
                             (nounset-error! name env))))))))


;; Validate an indirect expansion target is a valid variable name, special
;; parameter, or array reference like "name[idx]".
;; Returns #t if valid, #f if invalid (e.g. "bad var name", "/").
(def (valid-indirect-ref? s)
  (let ((len (string-length s)))
    (and (> len 0)
         (let ((ch0 (string-ref s 0)))
           (cond
             ;; Special single-char parameters: ?, #, @, *, $, !, -, _
             ((and (= len 1)
                   (or (char=? ch0 #\?) (char=? ch0 #\#) (char=? ch0 #\@)
                       (char=? ch0 #\*) (char=? ch0 #\$) (char=? ch0 #\!)
                       (char=? ch0 #\-) (char=? ch0 #\_)))
              #t)
             ;; Positional parameters: 0-9, or multi-digit
             ((char-numeric? ch0)
              (let loop ((i 1))
                (or (>= i len)
                    (and (char-numeric? (string-ref s i))
                         (loop (+ i 1))))))
             ;; Regular variable name: starts with letter or underscore
             ;; Optionally followed by [subscript]
             ((or (char-alphabetic? ch0) (char=? ch0 #\_))
              (let loop ((i 1))
                (if (>= i len) #t
                  (let ((ch (string-ref s i)))
                    (cond
                      ((or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))
                       (loop (+ i 1)))
                      ;; [ starts array subscript — rest is valid
                      ((char=? ch #\[) #t)
                      (else #f))))))
             (else #f))))))

;; Unescape \} → } in parameter expansion word.
;; The lexer stores \} inside ${...} to prevent premature brace close,
;; but by the time we expand the word, it should be a literal }.
(def (unescape-brace arg)
  ;; arg can be a string, cons pair (for // and / ops), or other types
  (if (not (string? arg))
    arg  ;; non-string args (cons pairs, etc.) pass through
    (let ((len (string-length arg)))
      (if (not (string-index arg #\\))
        arg  ;; fast path: no backslashes
        (let ((buf (open-output-string)))
          (let loop ((i 0))
            (if (>= i len)
              (get-output-string buf)
              (let ((ch (string-ref arg i)))
                (cond
                  ((and (char=? ch #\\) (< (+ i 1) len)
                        (char=? (string-ref arg (+ i 1)) #\}))
                   (display #\} buf)
                   (loop (+ i 2)))
                  (else
                   (display ch buf)
                   (loop (+ i 1))))))))))))

;; Pre-expand tildes in default/assign values (like assignment context: ~ at start and after :)
(def (tilde-expand-default arg env)
  (let ((segments (split-assignment-on-colon arg)))
    (string-join
     (map (lambda (seg)
            (if (and (> (string-length seg) 0)
                     (char=? (string-ref seg 0) #\~))
              (let-values (((expanded end) (expand-tilde-in seg 0 env)))
                (string-append expanded (substring seg end (string-length seg))))
              seg))
          segments)
     ":")))

(def (apply-parameter-modifier val name modifier arg0 env)
  ;; Inside ${...}, the lexer preserved \} to prevent premature brace close.
  ;; Now that we have the complete arg, unescape \} → } since } is not a
  ;; real backslash-escape target in double-quote context.
  (let ((arg (unescape-brace arg0))
        ;; For $* and $@, "null" check depends on context:
        ;; - Unquoted with 2+ positional params: always non-null (even if expansion is empty)
        ;; - Quoted or 0-1 params: check actual expanded value
        ;; This matches bash behavior where unquoted $* with IFS= and set -- "" ""
        ;; is non-null, but "$*" in the same scenario IS null.
        (colon-null? (lambda (v)
                       (or (not v)
                           (and (string? v) (string=? v "")
                                ;; $* and $@ unquoted with 2+ params: non-null
                                (not (and (not (*in-dquote-context*))
                                          (or (string=? name "*") (string=? name "@"))
                                          (let ((params (env-positional-list env)))
                                            (and (pair? params) (pair? (cdr params)))))))))))
  (case modifier
    ;; ${name:-word} — default if unset or null
    ((:-) (if (colon-null? val)
            (let ((earg (if (*in-dquote-context*) arg (tilde-expand-default arg env))))
              (if (*in-dquote-context*)
                (expand-string earg env)
                (make-modifier-segments (segments-literals-splittable (expand-string-segments earg env)))))
            val))
    ;; ${name-word} — default if unset
    ((-) (if (not val)
           (let ((earg (if (*in-dquote-context*) arg (tilde-expand-default arg env))))
             (if (*in-dquote-context*)
               (expand-string earg env)
               (make-modifier-segments (segments-literals-splittable (expand-string-segments earg env)))))
           val))
    ;; ${name:=word} — assign default if unset or null
    ((:=) (if (colon-null? val)
            (let* ((earg (if (*in-dquote-context*) arg (tilde-expand-default arg env)))
                   (default (expand-string earg env)))
              (env-set! env name default)
              (if (*in-dquote-context*)
                default
                (make-modifier-segments (segments-literals-splittable (expand-string-segments earg env)))))
            val))
    ;; ${name=word} — assign default if unset
    ((=) (if (not val)
           (let* ((earg (if (*in-dquote-context*) arg (tilde-expand-default arg env)))
                  (default (expand-string earg env)))
             (env-set! env name default)
             (if (*in-dquote-context*)
               default
               (make-modifier-segments (segments-literals-splittable (expand-string-segments earg env)))))
           val))
    ;; ${name:?word} — error if unset or null
    ((:?) (if (colon-null? val)
            (let ((msg (if (string=? arg "") "parameter null or not set"
                         (expand-string arg env))))
              (fprintf (current-error-port) "gsh: ~a: ~a~n" name msg)
              (raise (make-nounset-exception 1)))
            val))
    ;; ${name?word} — error if unset
    ((?) (if (not val)
           (let ((msg (if (string=? arg "") "parameter not set"
                        (expand-string arg env))))
             (fprintf (current-error-port) "gsh: ~a: ~a~n" name msg)
             (raise (make-nounset-exception 1)))
           val))
    ;; ${name:+word} — alternate if set and non-null
    ((:+) (if (not (colon-null? val))
            (let ((earg (if (*in-dquote-context*) arg (tilde-expand-default arg env))))
              (if (*in-dquote-context*)
                (expand-string earg env)
                (make-modifier-segments (segments-literals-splittable (expand-string-segments earg env)))))
            ""))
    ;; ${name+word} — alternate if set
    ((+) (if val
           (let ((earg (if (*in-dquote-context*) arg (tilde-expand-default arg env))))
             (if (*in-dquote-context*)
               (expand-string earg env)
               (make-modifier-segments (segments-literals-splittable (expand-string-segments earg env)))))
           ""))
    ;; ${name%pattern} — remove shortest suffix
    ((%) (if val (remove-suffix val (expand-pattern arg env) #f (env-shopt? env "extglob")) ""))
    ;; ${name%%pattern} — remove longest suffix
    ((%%) (if val (remove-suffix val (expand-pattern arg env) #t (env-shopt? env "extglob")) ""))
    ;; ${name#pattern} — remove shortest prefix
    ((prefix-short) (if val (remove-prefix val (expand-pattern arg env) #f (env-shopt? env "extglob")) ""))
    ;; ${name##pattern} — remove longest prefix
    ((prefix-long) (if val (remove-prefix val (expand-pattern arg env) #t (env-shopt? env "extglob")) ""))
    ;; ${name^^pattern} — uppercase all (matching pattern, or all if no pattern)
    ((^^) (if val
            (if (string=? arg "")
              (string-upcase val)
              (let ((pat (expand-pattern arg env)))
                (case-convert-matching val pat char-upcase #t (env-shopt? env "extglob"))))
            ""))
    ;; ${name^pattern} — uppercase first (matching pattern, or first if no pattern)
    ((^) (if (and val (> (string-length val) 0))
           (if (string=? arg "")
             (string-append (string (char-upcase (string-ref val 0)))
                            (substring val 1 (string-length val)))
             (let ((pat (expand-pattern arg env)))
               (case-convert-matching val pat char-upcase #f (env-shopt? env "extglob"))))
           (or val "")))
    ;; ${name,,pattern} — lowercase all (matching pattern, or all if no pattern)
    ((lc-all) (if val
                (if (string=? arg "")
                  (string-downcase val)
                  (let ((pat (expand-pattern arg env)))
                    (case-convert-matching val pat char-downcase #t (env-shopt? env "extglob"))))
                ""))
    ;; ${name,pattern} — lowercase first (matching pattern, or first if no pattern)
    ((lc-first) (if (and val (> (string-length val) 0))
                  (if (string=? arg "")
                    (string-append (string (char-downcase (string-ref val 0)))
                                  (substring val 1 (string-length val)))
                    (let ((pat (expand-pattern arg env)))
                      (case-convert-matching val pat char-downcase #f (env-shopt? env "extglob"))))
                  (or val "")))
    ;; ${name:offset} or ${name:offset:length} — substring expansion
    ((:) (cond
           ;; Special: ${@:offset} / ${*:offset} — positional param list slicing
           ((and val (or (string=? name "@") (string=? name "*")))
            (let* ((colon-pos (string-find-char-from arg #\: 0))
                   (offset-str (if colon-pos (substring arg 0 colon-pos)
                                   (string-trim-whitespace-str arg)))
                   (length-str (if colon-pos
                                 (string-trim-whitespace-str
                                  (substring arg (+ colon-pos 1) (string-length arg)))
                                 #f))
                   (offset (slice-arith-eval offset-str env))
                   ;; Build the full list: for offset 0, prepend $0
                   (params (env-positional-list env))
                   (full-list (if (<= offset 0)
                                (cons (shell-environment-shell-name env) params)
                                params))
                   (slen (length full-list))
                   ;; For offset 0: start at 0 in full-list (which has $0 prepended)
                   ;; For negative: from end of full-list
                   ;; For offset > 0: index into params (subtract 1 since $1 is first)
                   (start (cond
                            ((= offset 0) 0)
                            ((< offset 0) (max 0 (+ slen offset)))
                            (else (- offset 1))))
                   (start (max 0 (min start slen))))
              (let* ((sliced (if length-str
                               (let ((ln (slice-arith-eval length-str env)))
                                 (if (< ln 0)
                                   (let ((end (max start (+ slen ln))))
                                     (take-sublist full-list start end))
                                   (take-sublist full-list start (min slen (+ start ln)))))
                               (take-sublist full-list start slen))))
                (string-join-with " " sliced))))
           ;; Normal string substring
           (val
            (let* ((str val)
                   (slen (string-length str))
                   (colon-pos (string-find-char-from arg #\: 0))
                   (offset-str (if colon-pos (substring arg 0 colon-pos)
                                   (string-trim-whitespace-str arg)))
                   (length-str (if colon-pos
                                 (string-trim-whitespace-str
                                  (substring arg (+ colon-pos 1) (string-length arg)))
                                 #f))
                   (offset (slice-arith-eval offset-str env))
                   (start (if (< offset 0) (max 0 (+ slen offset)) (min offset slen))))
              (if length-str
                (let ((length (slice-arith-eval length-str env)))
                  (if (< length 0)
                    (let ((end (max start (+ slen length))))
                      (substring str start end))
                    (substring str start (min slen (+ start length)))))
                (substring str start slen))))
           (else "")))
    ;; ${name/pattern/replacement} — replace first match
    ((/) (if val
           (let ((pattern (expand-pattern (car arg) env))
                 (replacement (parameterize ((*in-dquote-context* #f))
                                (expand-string (cdr arg) env))))
             (pattern-substitute-first val pattern replacement
                                       (env-shopt? env "extglob")))
           ""))
    ;; ${name//pattern/replacement} — replace all matches
    ((//) (if val
            (let ((pattern (expand-pattern (car arg) env))
                  (replacement (parameterize ((*in-dquote-context* #f))
                                 (expand-string (cdr arg) env))))
              (pattern-substitute-all val pattern replacement
                                      (env-shopt? env "extglob")))
            ""))
    ;; ${name@operator} — value transformation
    ((at-op)
     (let ((op (if (> (string-length arg) 0) (string-ref arg 0) #f)))
       (case op
         ;; ${var@Q} — shell-quoted value (undefined → empty, empty → '')
         ((#\Q) (if val (shell-quote-for-at-q val) ""))
         ;; ${var@a} — attribute flags
         ((#\a) (get-var-attributes name env))
         ;; ${var@U} — uppercase all
         ((#\U) (if val (string-upcase val) ""))
         ;; ${var@u} — uppercase first char
         ((#\u) (if (and val (> (string-length val) 0))
                  (string-append (string (char-upcase (string-ref val 0)))
                                 (substring val 1 (string-length val)))
                  (or val "")))
         ;; ${var@L} — lowercase all
         ((#\L) (if val (string-downcase val) ""))
         ;; ${var@E} — expand backslash escapes (like $'...')
         ((#\E) (if val (expand-ansi-c-escapes val) ""))
         ;; ${var@P} — prompt expansion (stub: just return value)
         ((#\P) (or val ""))
         ;; ${var@K} and ${var@k} — quoted value (for scalars; key-value for arrays)
         ((#\K #\k) (if val (shell-quote-for-at-q val) ""))
         ;; ${var@A} — assignment form (declare only when attributes present)
         ((#\A) (if val
                  (let ((attrs (get-var-attributes name env)))
                    (if (string=? attrs "")
                      (string-append name "=" (shell-quote-for-at-q val))
                      (string-append "declare -" attrs " " name "=" (shell-quote-for-at-q val))))
                  ""))
         (else (or val "")))))
    ;; No modifier
    ((#f) (or val ""))
    (else (or val "")))))

;;; --- Command substitution ---

(def (expand-command-sub str i env)
  ;; $( ... ) — find matching paren
  (let* ((close (find-matching-paren str (+ i 2)))
         (cmd (substring str (+ i 2) close))
         (output (command-substitute cmd env)))
    (values output (+ close 1))))

(def (expand-backtick str i env (in-dquote? #f))
  ;; ` ... ` — find matching backtick
  ;; Process backtick-specific escaping:
  ;; \$ → $, \` → `, \\ → \  (always, per POSIX)
  ;; \" → "  (only when backtick is inside double quotes)
  ;; \x for other x → \x  (backslash preserved)
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (cond
                  ((>= j len) j)
                  ((char=? (string-ref str j) #\\)
                   (loop (+ j 2)))
                  ((char=? (string-ref str j) #\`)
                   j)
                  (else (loop (+ j 1))))))
         (raw (substring str (+ i 1) end))
         (cmd (let ((rlen (string-length raw))
                    (buf (open-output-string))
                    (special (if in-dquote?
                               '(#\$ #\` #\\ #\" #\newline)
                               '(#\$ #\` #\\ #\newline))))
                (let loop ((k 0))
                  (cond
                    ((>= k rlen) (get-output-string buf))
                    ((char=? (string-ref raw k) #\\)
                     (if (< (+ k 1) rlen)
                       (let ((next (string-ref raw (+ k 1))))
                         (if (memq next special)
                           (begin (display next buf) (loop (+ k 2)))
                           (begin (display #\\ buf) (display next buf) (loop (+ k 2)))))
                       (begin (display #\\ buf) (loop (+ k 1)))))
                    (else (display (string-ref raw k) buf) (loop (+ k 1)))))))
         (output (command-substitute cmd env)))
    (values output (+ end 1))))

;; Execute a command and capture its stdout
;; Strips trailing newlines (per POSIX)
(def (command-substitute cmd env)
  ;; Signal that a command substitution ran (for bare assignment $? tracking)
  (*command-sub-ran* #t)
  (let ((exec-fn (*execute-input*)))
    (if exec-fn
      ;; Use gsh's own executor: redirect stdout to a pipe, run command, read output
      (with-catch
       (lambda (e) "")
       (lambda ()
         (let-values (((read-fd write-fd) (ffi-pipe-raw)))
           ;; Save real stdout fd and Gambit port
           (let ((saved-fd (ffi-dup 1))
                 (saved-port (current-output-port)))
             ;; Redirect real fd 1 to pipe write end
             (ffi-dup2 write-fd 1)
             (ffi-close-fd write-fd)
             ;; Create a Gambit character port via /dev/fd for builtins
             (let ((pipe-port (open-output-file
                               (string-append "/dev/fd/" (number->string 1)))))
               (current-output-port pipe-port)
               ;; Execute in subshell context so exit doesn't terminate parent.
               ;; Clone env so changes (aliases, variables) don't leak back.
               ;; Clear pipeline fd params so execute-external won't override
               ;; fd 0/1 with pipeline pipe fds — we already redirected fd 1
               ;; to the capture pipe above.
               (let ((sub-env (env-clone env)))
                 (with-catch
                  (lambda (e)
                    (when (subshell-exit-exception? e)
                      (env-set-last-status! env (subshell-exit-exception-status e))))
                  (lambda ()
                    (parameterize ((*in-subshell* #t)
                                   (*pipeline-stdin-fd* #f)
                                   (*pipeline-stdout-fd* #f))
                      (exec-fn cmd sub-env))
                    ;; Normal completion: propagate $? from subshell
                    (env-set-last-status! env (shell-environment-last-status sub-env)))))
               ;; Flush and close the pipe port
               (force-output pipe-port)
               (close-port pipe-port))
             ;; Restore real fd 1 and Gambit port
             (ffi-dup2 saved-fd 1)
             (ffi-close-fd saved-fd)
             (current-output-port saved-port)
             ;; Read the output from the pipe read end using raw read()
             ;; (open-input-file "/dev/fd/N" blocks on empty pipes in Gambit)
             ;; Note: ffi-read-all-from-fd returns Latin-1 decoded string;
             ;; re-decode as UTF-8 for correct multi-byte character handling
             (let ((output (latin1->utf8 (ffi-read-all-from-fd read-fd))))
               (ffi-close-fd read-fd)
               (string-trim-trailing-newlines output))))))
      ;; Fallback: use /bin/sh if executor not initialized
      (with-catch
       (lambda (e) "")
       (lambda ()
         (let* ((port (open-input-process
                       [path: "/bin/sh"
                        arguments: ["-c" cmd]
                        environment: (env-exported-alist env)]))
                (output (read-all-string port)))
           (close-port port)
           (string-trim-trailing-newlines output)))))))

;;; --- Arithmetic substitution ---

(def (expand-arith-sub str i env)
  ;; $(( ... )) — find matching ))
  ;; Pre-expand $var, ${var}, $(cmd) inside the expression before arith-eval
  (let* ((close (find-arith-close str (+ i 3)))
         (raw-expr (substring str (+ i 3) close))
         ;; Expand in dquote context — arithmetic doesn't do word splitting,
         ;; so ${var:-default} must return strings, not modifier-segments
         (expr (parameterize ((*in-dquote-context* #t))
                 (expand-arith-expr raw-expr env)))
         (result (arith-eval expr
                            (arith-env-getter env)
                            (arith-env-setter env)
                            (and (env-option? env "nounset")
                                 (lambda (name)
                                   (nounset-error! name env))))))
    (values (number->string result) (+ close 2))))

;; Expand $var, ${var}, $(cmd) inside an arithmetic expression
;; but leave bare variable names alone (they're resolved by arith-eval)
(def (expand-arith-expr expr env)
  (let ((len (string-length expr))
        (buf (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string buf))
        ((and (char=? (string-ref expr i) #\$)
              (< (+ i 1) len))
         (let ((next (string-ref expr (+ i 1))))
           (cond
             ;; $(( ... )) — nested arithmetic substitution
             ((and (char=? next #\()
                   (< (+ i 2) len)
                   (char=? (string-ref expr (+ i 2)) #\())
              (let-values (((val end) (expand-arith-sub expr i env)))
                (display val buf)
                (loop end)))
             ;; $( ... ) — command substitution
             ((char=? next #\()
              (let-values (((val end) (expand-command-sub expr i env)))
                (display val buf)
                (loop end)))
             ;; ${ ... } — braced parameter expansion
             ((char=? next #\{)
              (let-values (((val end) (expand-parameter-braced expr i env)))
                (display val buf)
                (loop end)))
             ;; $name — simple variable
             ((or (char-alphabetic? next) (char=? next #\_))
              (let-values (((val end) (expand-simple-var expr i env)))
                (display val buf)
                (loop end)))
             ;; $N — positional parameter
             ((char-numeric? next)
              (let-values (((val end) (expand-simple-var expr i env)))
                (display val buf)
                (loop end)))
             ;; $? $! $# etc.
             ((memv next '(#\? #\! #\# #\- #\$ #\0))
              (let-values (((val end) (expand-simple-var expr i env)))
                (display val buf)
                (loop end)))
             ;; $'...' — ANSI-C quoting: extract content (just strip quotes for arith)
             ((char=? next #\')
              (let qloop ((j (+ i 2)))
                (cond
                  ((>= j len) (display (substring expr (+ i 2) len) buf) (loop len))
                  ((char=? (string-ref expr j) #\')
                   ;; For arithmetic, the content is usually just a digit string
                   (display (substring expr (+ i 2) j) buf)
                   (loop (+ j 1)))
                  ((char=? (string-ref expr j) #\\)
                   (qloop (min (+ j 2) len)))
                  (else (qloop (+ j 1))))))
             ;; $"..." — locale quoting: strip $ and process as double-quoted
             ((char=? next #\")
              (let qloop ((j (+ i 2)))
                (cond
                  ((>= j len) (display (substring expr (+ i 2) len) buf) (loop len))
                  ((char=? (string-ref expr j) #\")
                   (let ((inner (substring expr (+ i 2) j)))
                     (display (expand-arith-expr inner env) buf)
                     (loop (+ j 1))))
                  ((char=? (string-ref expr j) #\\)
                   (qloop (min (+ j 2) len)))
                  (else (qloop (+ j 1))))))
             (else
              (display #\$ buf)
              (loop (+ i 1))))))
        ;; Backquote substitution
        ((char=? (string-ref expr i) #\`)
         (let-values (((val end) (expand-backtick expr i env)))
           (display val buf)
           (loop end)))
        ;; Quoted strings: pass through removing quotes for arith purposes
        ((char=? (string-ref expr i) #\')
         ;; Single-quoted string in arithmetic — the content is a literal
         (let qloop ((j (+ i 1)))
           (cond
             ((>= j len) (display (substring expr i len) buf) (loop len))
             ((char=? (string-ref expr j) #\')
              (display (substring expr (+ i 1) j) buf)
              (loop (+ j 1)))
             (else (qloop (+ j 1))))))
        ((char=? (string-ref expr i) #\")
         ;; Double-quoted string in arithmetic — expand contents
         (let qloop ((j (+ i 1)))
           (cond
             ((>= j len) (display (substring expr i len) buf) (loop len))
             ((char=? (string-ref expr j) #\")
              (let ((inner (substring expr (+ i 1) j)))
                (display (expand-arith-expr inner env) buf)
                (loop (+ j 1))))
             ((char=? (string-ref expr j) #\\)
              (qloop (min (+ j 2) len)))
             (else (qloop (+ j 1))))))
        (else
         (display (string-ref expr i) buf)
         (loop (+ i 1)))))))

;;; --- Word splitting ---

;; POSIX IFS field splitting algorithm:
;; - IFS whitespace chars (space, tab, newline in IFS): collapse consecutive, trim leading/trailing
;; - IFS non-whitespace chars: each is a delimiter, adjacent ones produce empty fields
;; - Mixed: IFS whitespace around non-whitespace delimiter is trimmed (doesn't create extra fields)
;; - Leading non-whitespace IFS char produces leading empty field
;; - Trailing non-whitespace IFS char does NOT produce trailing empty field
(def (word-split str env)
  (let ((ifs (or (env-get env "IFS") " \t\n")))
    (if (string=? ifs "")
      [str]  ;; empty IFS = no splitting
      (let ((len (string-length str)))
        (if (= len 0) [str]
          ;; Classify IFS characters
          (let ((ifs-ws? (lambda (ch) (and (ifs-char? ch ifs)
                                           (or (char=? ch #\space)
                                               (char=? ch #\tab)
                                               (char=? ch #\newline)))))
                (ifs-nws? (lambda (ch) (and (ifs-char? ch ifs)
                                            (not (or (char=? ch #\space)
                                                     (char=? ch #\tab)
                                                     (char=? ch #\newline)))))))
            ;; Algorithm: scan left to right.
            ;; State: accumulating non-IFS chars into a field, or at a delimiter boundary.
            (let loop ((i 0) (start 0) (words []))
              (cond
                ((>= i len)
                 ;; End of string — trailing IFS delimiters do NOT produce empty fields
                 (let ((words (if (> i start)
                                (cons (substring str start i) words)
                                words)))
                   (reverse words)))
                ;; Non-whitespace IFS delimiter
                ((ifs-nws? (string-ref str i))
                 ;; Add the field before this delimiter (may be empty for leading/adjacent)
                 (let ((words (cons (substring str start i) words)))
                   ;; Skip any IFS whitespace after the delimiter
                   (let skip-ws ((j (+ i 1)))
                     (if (and (< j len) (ifs-ws? (string-ref str j)))
                       (skip-ws (+ j 1))
                       (loop j j words)))))
                ;; IFS whitespace
                ((ifs-ws? (string-ref str i))
                 ;; Skip all consecutive IFS whitespace
                 (let skip-ws ((j (+ i 1)))
                   (cond
                     ;; Whitespace followed by non-whitespace IFS delimiter:
                     ;; the nws delimiter is the actual field separator
                     ((and (< j len) (ifs-nws? (string-ref str j)))
                      ;; Add field before whitespace if there's content
                      (let ((words (if (> i start)
                                     (cons (substring str start i) words)
                                     words)))
                        ;; Skip past the nws delimiter and any trailing ws
                        (let skip-ws2 ((k (+ j 1)))
                          (if (and (< k len) (ifs-ws? (string-ref str k)))
                            (skip-ws2 (+ k 1))
                            (loop k k words)))))
                     ;; Plain whitespace delimiter (no adjacent nws)
                     ((and (< j len) (ifs-ws? (string-ref str j)))
                      (skip-ws (+ j 1)))
                     ;; End of whitespace run - it's a plain ws delimiter
                     (else
                      (let ((words (if (> i start)
                                     (cons (substring str start i) words)
                                     words)))
                        (loop j j words))))))
                ;; Regular character
                (else
                 (loop (+ i 1) start words))))))))))

(def (ifs-char? ch ifs)
  (let loop ((i 0))
    (and (< i (string-length ifs))
         (or (char=? ch (string-ref ifs i))
             (loop (+ i 1))))))

(def (ifs-whitespace? ch ifs)
  (and (ifs-char? ch ifs)
       (or (char=? ch #\space)
           (char=? ch #\tab)
           (char=? ch #\newline))))

;;; --- Quoting helpers ---

;; Check if a raw word contains quote characters (single or double quotes)
;; Used to determine if word splitting should be inhibited
;; Check if a word has top-level quotes (not inside $() or ``)
;; Quotes inside command substitutions don't suppress word splitting
(def (word-has-quotes? word)
  (let ((len (string-length word)))
    (let loop ((i 0) (depth 0))
      (if (>= i len) #f
          (let ((ch (string-ref word i)))
            (cond
              ;; Top-level ' or " → word is quoted
              ((and (= depth 0) (or (char=? ch #\') (char=? ch #\")))
               #t)
              ;; $( opens a nesting level
              ((and (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\())
               (loop (+ i 2) (+ depth 1)))
              ;; $((  opens a nesting level (arithmetic)
              ((and (char=? ch #\$) (< (+ i 2) len)
                    (char=? (string-ref word (+ i 1)) #\()
                    (char=? (string-ref word (+ i 2)) #\())
               (loop (+ i 3) (+ depth 1)))
              ;; ( inside $() increments depth
              ((and (> depth 0) (char=? ch #\())
               (loop (+ i 1) (+ depth 1)))
              ;; ) decrements depth
              ((and (> depth 0) (char=? ch #\)))
               (loop (+ i 1) (- depth 1)))
              ;; Backtick at top level counts as nesting
              ((and (= depth 0) (char=? ch #\`))
               (let skip-bt ((j (+ i 1)))
                 (cond
                   ((>= j len) #f)
                   ((char=? (string-ref word j) #\`)
                    (loop (+ j 1) 0))
                   ((char=? (string-ref word j) #\\)
                    (skip-bt (+ j 2)))
                   (else (skip-bt (+ j 1))))))
              ;; Backslash at top level: skip next char
              ((and (= depth 0) (char=? ch #\\))
               (loop (+ i 2) 0))
              (else (loop (+ i 1) depth))))))))

;; Check if a word contains $@ or ${@} or ${name[@]} inside double quotes
;; (not inside $(), ``, or nested ${...})
;; Used to detect the need for "$@" / "${arr[@]}" multi-word expansion
(def (word-has-quoted-at? word)
  (let ((len (string-length word)))
    (let loop ((i 0) (in-dq #f) (depth 0) (brace-depth 0))
      (if (>= i len) #f
          (let ((ch (string-ref word i)))
            (cond
              ;; Backslash: skip next char
              ((char=? ch #\\) (loop (min (+ i 2) len) in-dq depth brace-depth))
              ;; Double quote: only toggle at depth 0 and brace-depth 0
              ((and (char=? ch #\") (= depth 0) (= brace-depth 0))
               (loop (+ i 1) (not in-dq) depth brace-depth))
              ;; Single quote outside double-quotes at depth 0, brace-depth 0: skip
              ((and (= depth 0) (= brace-depth 0) (not in-dq) (char=? ch #\'))
               (let skip ((j (+ i 1)))
                 (if (or (>= j len) (char=? (string-ref word j) #\'))
                   (loop (+ j 1) in-dq depth brace-depth)
                   (skip (+ j 1)))))
              ;; $@ at depth 0 inside double quotes (any brace-depth) — MATCH
              ((and in-dq (= depth 0) (= brace-depth 0) (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\@))
               #t)
              ;; ${ at depth 0 — check for ${@}, ${name[@]}, or generic ${...}
              ((and (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\{))
               (cond
                 ;; ${@...} inside double quotes — any form (${@}, ${@:...}, ${@//...}, etc.)
                 ((and in-dq (= depth 0) (= brace-depth 0)
                       (< (+ i 2) len)
                       (char=? (string-ref word (+ i 2)) #\@))
                  #t)
                 ;; ${name[@]} or ${!prefix@} or ${\!prefix@} inside double quotes — MATCH
                 ((and in-dq (= depth 0) (= brace-depth 0))
                  (let* ((start (+ i 2))
                         ;; Check for ! or \! (lexer escapes ! as \!)
                         (has-bang? (or (and (< start len) (char=? (string-ref word start) #\!))
                                       (and (< (+ start 1) len)
                                            (char=? (string-ref word start) #\\)
                                            (char=? (string-ref word (+ start 1)) #\!))))
                         (name-start (cond
                                       ((not has-bang?) start)
                                       ((char=? (string-ref word start) #\!) (+ start 1))
                                       (else (+ start 2)))))  ;; skip \!
                    ;; Scan for name[@]} or prefix@} pattern
                    (let scan ((j name-start))
                      (cond
                        ((>= j len)
                         ;; No match — still enter the brace
                         (loop (+ i 2) in-dq depth (+ brace-depth 1)))
                        ;; Found [@] — match for array! (also match [@]body} for modifiers)
                        ((and (not has-bang?) (char=? (string-ref word j) #\[)
                              (< (+ j 2) len)
                              (char=? (string-ref word (+ j 1)) #\@)
                              (char=? (string-ref word (+ j 2)) #\]))
                         ;; Check for } or modifier body after ]
                         (let find-close ((k (+ j 3)) (bd 1))
                           (cond
                             ((>= k len) (loop (+ i 2) in-dq depth (+ brace-depth 1)))
                             ((char=? (string-ref word k) #\}) #t)
                             (else (find-close (+ k 1) bd)))))
                        ;; Found @} — match for ${!prefix@}!
                        ((and has-bang? (char=? (string-ref word j) #\@)
                              (< (+ j 1) len)
                              (char=? (string-ref word (+ j 1)) #\}))
                         #t)
                        ;; Valid identifier chars — keep scanning
                        ((or (char-alphabetic? (string-ref word j))
                             (char-numeric? (string-ref word j))
                             (char=? (string-ref word j) #\_))
                         (scan (+ j 1)))
                        ;; Not a simple name — enter brace
                        (else (loop (+ i 2) in-dq depth (+ brace-depth 1)))))))
                 ;; Not in matching context — just enter the brace
                 (else (loop (+ i 2) in-dq depth (+ brace-depth 1)))))
              ;; } at brace-depth > 0 — decrease brace depth
              ((and (> brace-depth 0) (char=? ch #\}))
               (loop (+ i 1) in-dq depth (- brace-depth 1)))
              ;; $( — increase depth
              ((and (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\())
               (loop (+ i 2) in-dq (+ depth 1) brace-depth))
              ;; ( inside command sub — increase depth
              ((and (> depth 0) (char=? ch #\())
               (loop (+ i 1) in-dq (+ depth 1) brace-depth))
              ;; ) — decrease depth
              ((and (> depth 0) (char=? ch #\)))
               (loop (+ i 1) in-dq (- depth 1) brace-depth))
              ;; Backtick at depth 0: skip to matching backtick
              ((and (= depth 0) (= brace-depth 0) (char=? ch #\`))
               (let skip ((j (+ i 1)))
                 (cond
                   ((>= j len) (loop j in-dq depth brace-depth))
                   ((char=? (string-ref word j) #\\) (skip (+ j 2)))
                   ((char=? (string-ref word j) #\`) (loop (+ j 1) in-dq depth brace-depth))
                   (else (skip (+ j 1))))))
              (else (loop (+ i 1) in-dq depth brace-depth))))))))

;; Find $@ or ${@} or ${name[@]} inside double quotes and split into prefix/suffix
;; Returns (values prefix-str suffix-str array-name-or-false) where quotes are balanced
;; array-name is #f for $@/${@}, or the array name string for ${name[@]}
(def (split-word-at-quoted-at word)
  (let ((len (string-length word)))
    (let loop ((i 0) (in-dq #f) (depth 0) (brace-depth 0))
      (cond
        ((>= i len)
         (values word "" #f #f))
        ((char=? (string-ref word i) #\\)
         (loop (min (+ i 2) len) in-dq depth brace-depth))
        ((and (char=? (string-ref word i) #\") (= depth 0) (= brace-depth 0))
         (loop (+ i 1) (not in-dq) depth brace-depth))
        ((and (= depth 0) (= brace-depth 0) (not in-dq) (char=? (string-ref word i) #\'))
         (let skip ((j (+ i 1)))
           (if (or (>= j len) (char=? (string-ref word j) #\'))
             (loop (+ j 1) in-dq depth brace-depth)
             (skip (+ j 1)))))
        ;; $@ inside double quotes at depth 0, brace-depth 0
        ((and in-dq (= depth 0) (= brace-depth 0) (char=? (string-ref word i) #\$) (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\@))
         (values (string-append (substring word 0 i) "\"")
                 (string-append "\"" (substring word (+ i 2) len))
                 #f #f))
        ;; ${ — check for ${@}, ${name[@]}, or generic ${...}
        ((and (char=? (string-ref word i) #\$) (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\{))
         (cond
           ;; ${@...} inside double quotes — any positional param form
           ;; (${@}, ${@:slice}, ${@//pat/rep}, ${@%pat}, etc.)
           ((and in-dq (= depth 0) (= brace-depth 0)
                 (< (+ i 2) len)
                 (char=? (string-ref word (+ i 2)) #\@))
            ;; Check for simple ${@}
            (if (and (< (+ i 3) len)
                     (char=? (string-ref word (+ i 3)) #\}))
              (values (string-append (substring word 0 i) "\"")
                      (string-append "\"" (substring word (+ i 4) len))
                      #f #f)
              ;; ${@modifier...} — find the closing } tracking nested braces
              (let find-close ((j (+ i 3)) (bd 1))
                (cond
                  ((>= j len) (loop (+ i 2) in-dq depth (+ brace-depth 1)))
                  ((char=? (string-ref word j) #\{) (find-close (+ j 1) (+ bd 1)))
                  ((char=? (string-ref word j) #\})
                   (if (= bd 1)
                     ;; Found the closing }, extract body after @ up to }
                     (let ((at-body (substring word (+ i 3) j))
                           (end-pos (+ j 1)))
                       (values (string-append (substring word 0 i) "\"")
                               (string-append "\"" (substring word end-pos len))
                               #f at-body))
                     (find-close (+ j 1) (- bd 1))))
                  (else (find-close (+ j 1) bd))))))
           ;; ${name[@]} or ${!prefix@} or ${\!prefix@} inside double quotes
           ((and in-dq (= depth 0) (= brace-depth 0))
            (let* ((start (+ i 2))
                   ;; Check for ! or \! (lexer escapes ! as \!)
                   (has-bang? (or (and (< start len) (char=? (string-ref word start) #\!))
                                 (and (< (+ start 1) len)
                                      (char=? (string-ref word start) #\\)
                                      (char=? (string-ref word (+ start 1)) #\!))))
                   (name-start (cond
                                 ((not has-bang?) start)
                                 ((char=? (string-ref word start) #\!) (+ start 1))
                                 (else (+ start 2)))))  ;; skip \!
              (let scan ((j name-start))
                (cond
                  ((>= j len) (loop (+ i 2) in-dq depth (+ brace-depth 1)))
                  ;; Found [@] for array
                  ((and (not has-bang?) (char=? (string-ref word j) #\[)
                        (< (+ j 2) len)
                        (char=? (string-ref word (+ j 1)) #\@)
                        (char=? (string-ref word (+ j 2)) #\]))
                   (let ((arr-name (substring word name-start j))
                         (after-bracket (+ j 3)))
                     (if (and (< after-bracket len)
                              (char=? (string-ref word after-bracket) #\}))
                       ;; Simple ${name[@]}
                       (values (string-append (substring word 0 i) "\"")
                               (string-append "\"" (substring word (+ after-bracket 1) len))
                               arr-name #f)
                       ;; ${name[@]body} — find closing } for modifier
                       (let find-close ((k after-bracket) (bd 1))
                         (cond
                           ((>= k len) (loop (+ i 2) in-dq depth (+ brace-depth 1)))
                           ((char=? (string-ref word k) #\})
                            (let ((at-body (substring word after-bracket k))
                                  (end-pos (+ k 1)))
                              (values (string-append (substring word 0 i) "\"")
                                      (string-append "\"" (substring word end-pos len))
                                      arr-name at-body)))
                           (else (find-close (+ k 1) bd)))))))
                  ;; Found @} for ${!prefix@}
                  ((and has-bang? (char=? (string-ref word j) #\@)
                        (< (+ j 1) len)
                        (char=? (string-ref word (+ j 1)) #\}))
                   (let ((prefix-name (string-append "!" (substring word name-start j)))
                         (end-pos (+ j 2)))
                     (values (string-append (substring word 0 i) "\"")
                             (string-append "\"" (substring word end-pos len))
                             prefix-name #f)))
                  ((or (char-alphabetic? (string-ref word j))
                       (char-numeric? (string-ref word j))
                       (char=? (string-ref word j) #\_))
                   (scan (+ j 1)))
                  (else (loop (+ i 2) in-dq depth (+ brace-depth 1)))))))
           ;; Not in matching context — enter brace
           (else (loop (+ i 2) in-dq depth (+ brace-depth 1)))))
        ;; } at brace-depth > 0
        ((and (> brace-depth 0) (char=? (string-ref word i) #\}))
         (loop (+ i 1) in-dq depth (- brace-depth 1)))
        ((and (char=? (string-ref word i) #\$) (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\())
         (loop (+ i 2) in-dq (+ depth 1) brace-depth))
        ((and (> depth 0) (char=? (string-ref word i) #\())
         (loop (+ i 1) in-dq (+ depth 1) brace-depth))
        ((and (> depth 0) (char=? (string-ref word i) #\)))
         (loop (+ i 1) in-dq (- depth 1) brace-depth))
        ((and (= depth 0) (= brace-depth 0) (char=? (string-ref word i) #\`))
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) (loop j in-dq depth brace-depth))
             ((char=? (string-ref word j) #\\) (skip (+ j 2)))
             ((char=? (string-ref word j) #\`) (loop (+ j 1) in-dq depth brace-depth))
             (else (skip (+ j 1))))))
        (else
         (loop (+ i 1) in-dq depth brace-depth))))))

;; Expand a word string to a list of IFS-split strings (no globbing)
;; Used for prefix/suffix parts in @-expansion word joining
(def (expand-word-parts-split word-str env)
  (let* ((segments (expand-string-segments word-str env))
         (split (split-expanded-segments segments env)))
    (map car split)))

(def (expand-word-with-at word env)
  (let-values (((prefix-raw suffix-raw array-name at-body) (split-word-at-quoted-at word)))
    (let* ((all-elements (cond
                           ;; ${!prefix@} — matching variable names
                           ((and array-name (> (string-length array-name) 0)
                                 (char=? (string-ref array-name 0) #\!))
                            (env-matching-names env (substring array-name 1 (string-length array-name))))
                           ;; ${name[@]} — array values
                           (array-name (env-array-values env array-name))
                           ;; $@ — positional params
                           (else (env-positional-list env))))
           ;; Apply at-body modifier/slice if present
           (elements
            (cond
              ((not at-body) all-elements)
              ;; Slice: starts with : but NOT :- :+ := :? (those are modifiers)
              ((and (char=? (string-ref at-body 0) #\:)
                    (or (<= (string-length at-body) 1)
                        (not (memv (string-ref at-body 1) '(#\- #\+ #\= #\?)))))
               (let* ((spec (substring at-body 1 (string-length at-body)))
                      (colon-pos (string-find-char-from spec #\: 0))
                      (offset-str (if colon-pos
                                    (substring spec 0 colon-pos)
                                    spec))
                      (length-str (if colon-pos
                                    (substring spec (+ colon-pos 1)
                                               (string-length spec))
                                    #f))
                      (offset (slice-arith-eval offset-str env))
                      (full-list (if (<= offset 0)
                                   (cons (shell-environment-shell-name env) all-elements)
                                   all-elements))
                      (slen (length full-list))
                      (start (cond
                               ((= offset 0) 0)
                               ((< offset 0) (max 0 (+ slen offset)))
                               (else (- offset 1))))
                      (start (max 0 (min start slen))))
                 (if length-str
                   (let ((ln (slice-arith-eval length-str env)))
                     (if (< ln 0)
                       (let ((end (max start (+ slen ln))))
                         (take-sublist full-list start end))
                       (take-sublist full-list start (min slen (+ start ln)))))
                   (take-sublist full-list start slen))))
              ;; Other modifier: //pat/rep, %pat, %%pat, #pat, ##pat, ^, ^^, ,, etc.
              (else
               (let-values (((mname modifier arg) (parse-parameter-modifier
                                                    (string-append "@" at-body))))
                 (if modifier
                   ;; Conditional modifiers on arrays: handle empty array case
                   (if (memq modifier '(- :- + :+ = := ? :?))
                     ;; For [@] inside "...", return array elements or modifier result
                     (let* ((is-empty (null? all-elements))
                            ;; For colon variants: null if 0 elements, or exactly 1 empty element.
                            ;; 2+ elements always produce word breaks, so result is non-null.
                            (is-null (or is-empty
                                        (and (memq modifier '(:- :+ := :?))
                                             (= (length all-elements) 1)
                                             (string=? (car all-elements) "")))))
                       (case modifier
                         ;; ${arr[@]-word}: use default only if array has no elements
                         ((-) (if is-empty
                               (list (parameterize ((*in-dquote-context* #t))
                                       (expand-string arg env)))
                               all-elements))
                         ;; ${arr[@]:-word}: use default if no elements OR all empty
                         ((:-) (if is-null
                                 (list (parameterize ((*in-dquote-context* #t))
                                         (expand-string arg env)))
                                 all-elements))
                         ;; ${arr[@]+word}: use alternate if array has elements
                         ((+) (if is-empty '()
                               (list (parameterize ((*in-dquote-context* #t))
                                       (expand-string arg env)))))
                         ;; ${arr[@]:+word}: use alternate if array has non-empty elements
                         ((:+) (if is-null '()
                                 (list (parameterize ((*in-dquote-context* #t))
                                         (expand-string arg env)))))
                         ;; Other conditionals: delegate
                         (else
                          (let* ((val (if is-empty #f (string-join-with " " all-elements)))
                                 (result (parameterize ((*in-dquote-context* #t))
                                           (apply-parameter-modifier val (or array-name "@") modifier arg env))))
                            (if result (list result) '())))))
                     ;; Transform modifiers: apply per-element
                     (let ((attr-name (or array-name "@")))
                       (map (lambda (p) (apply-parameter-modifier p attr-name modifier arg env))
                            all-elements)))
                   all-elements)))))
           ;; Expand prefix with IFS splitting (returns list of strings)
           (prefix-words (expand-word-parts-split prefix-raw env))
           ;; Expand suffix — recursively handle more @-expansions
           (suffix-words (if (word-has-quoted-at? suffix-raw)
                           (expand-word-with-at suffix-raw env)
                           (expand-word-parts-split suffix-raw env))))
      ;; Join prefix-words + elements + suffix-words
      ;; Last of prefix joins with first element; last element joins with first of suffix
      (let ((butlast (lambda (lst)
                       (let loop ((l lst) (acc []))
                         (if (null? (cdr l)) (reverse acc)
                           (loop (cdr l) (cons (car l) acc)))))))
        (cond
          ((null? elements)
           ;; No elements: $@ or ${arr[@]} is empty
           ;; Join prefix and suffix directly, but elide if result is all empty
           (cond
             ((and (null? prefix-words) (null? suffix-words)) [])
             ((null? suffix-words)
              (let ((combined (apply string-append prefix-words)))
                (if (string=? combined "") [] prefix-words)))
             ((null? prefix-words)
              (let ((combined (apply string-append suffix-words)))
                (if (string=? combined "") [] suffix-words)))
             (else
              ;; Join last of prefix with first of suffix
              (let* ((pfx-init (butlast prefix-words))
                     (pfx-last (last prefix-words))
                     (sfx-first (car suffix-words))
                     (sfx-rest (cdr suffix-words))
                     (joined (string-append pfx-last sfx-first))
                     (result (append pfx-init [joined] sfx-rest))
                     (combined (apply string-append result)))
                (if (string=? combined "") [] result)))))
          (else
           ;; Has elements — do proper word joining
           (let* ((pfx-last (if (null? prefix-words) "" (last prefix-words)))
                  (pfx-rest (if (or (null? prefix-words) (null? (cdr prefix-words)))
                              [] (butlast prefix-words)))
                  (sfx-first (if (null? suffix-words) "" (car suffix-words)))
                  (sfx-rest (if (or (null? suffix-words) (null? (cdr suffix-words)))
                              [] (cdr suffix-words)))
                  (n (length elements)))
             (if (= n 1)
               (append pfx-rest
                       [(string-append pfx-last (car elements) sfx-first)]
                       sfx-rest)
               (let* ((first-elem (string-append pfx-last (car elements)))
                      (last-elem (string-append (last elements) sfx-first))
                      (middle (let mid-loop ((ps (cdr elements)) (acc []))
                                (if (null? (cdr ps))
                                  (reverse acc)
                                  (mid-loop (cdr ps) (cons (car ps) acc))))))
                 (append pfx-rest [first-elem] middle [last-elem] sfx-rest))))))))))

(def (read-single-quote str i)
  (let ((len (string-length str)))
    (let loop ((j i) (buf (open-output-string)))
      (cond
        ((>= j len) (values (get-output-string buf) j))
        ((char=? (string-ref str j) #\')
         (values (get-output-string buf) (+ j 1)))
        (else
         (display (string-ref str j) buf)
         (loop (+ j 1) buf))))))

(def (expand-double-quote str i env)
  (let ((len (string-length str))
        (buf (open-output-string)))
    (let loop ((j i))
      (cond
        ((>= j len) (values (get-output-string buf) j))
        ((char=? (string-ref str j) #\")
         (values (get-output-string buf) (+ j 1)))
        ((char=? (string-ref str j) #\\)
         (if (< (+ j 1) len)
           (let ((next (string-ref str (+ j 1))))
             (if (memq next '(#\$ #\` #\" #\\ #\newline))
               (begin (display next buf) (loop (+ j 2)))
               (begin (display "\\" buf) (display next buf) (loop (+ j 2)))))
           (begin (display "\\" buf) (loop (+ j 1)))))
        ((char=? (string-ref str j) #\$)
         (let-values (((expanded end)
                       (parameterize ((*in-dquote-context* #t))
                         (expand-dollar str j env))))
           (display (if (modifier-segments? expanded)
                      (segments->string (modifier-segments-list expanded))
                      expanded)
                    buf)
           (loop end)))
        ((char=? (string-ref str j) #\`)
         (let-values (((expanded end) (expand-backtick str j env #t)))
           (display expanded buf)
           (loop end)))
        (else
         (display (string-ref str j) buf)
         (loop (+ j 1)))))))

;;; --- Bracket/brace matching ---

(def (find-matching-brace str start)
  (let ((len (string-length str)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref str i) #\{) (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref str i) #\})
         (if (= depth 1) i (loop (+ i 1) (- depth 1))))
        ((char=? (string-ref str i) #\\) (loop (+ i 2) depth))
        ;; Skip single-quoted regions
        ((char=? (string-ref str i) #\')
         (let sq ((j (+ i 1)))
           (cond ((>= j len) (loop j depth))
                 ((char=? (string-ref str j) #\') (loop (+ j 1) depth))
                 (else (sq (+ j 1))))))
        ;; Skip double-quoted regions
        ((char=? (string-ref str i) #\")
         (let dq ((j (+ i 1)))
           (cond ((>= j len) (loop j depth))
                 ((char=? (string-ref str j) #\\) (dq (+ j 2)))
                 ((char=? (string-ref str j) #\") (loop (+ j 1) depth))
                 (else (dq (+ j 1))))))
        (else (loop (+ i 1) depth))))))

(def (find-matching-paren str start)
  ;; Tracks case/esac nesting so ) in case patterns doesn't close early
  (let ((len (string-length str)))
    (define (update-case-depth w cd)
      (cond ((string=? w "case") (+ cd 1))
            ((string=? w "esac") (max 0 (- cd 1)))
            (else cd)))
    (let loop ((i start) (depth 1) (case-depth 0) (word ""))
      (cond
        ((>= i len) (- len 1))
        ((char=? (string-ref str i) #\()
         (let ((cd (update-case-depth word case-depth)))
           (loop (+ i 1) (+ depth 1) cd "")))
        ((char=? (string-ref str i) #\))
         (let ((cd (update-case-depth word case-depth)))
           (if (and (= depth 1) (> cd 0))
             ;; Inside case: ) is pattern delimiter, not closing
             (loop (+ i 1) depth cd "")
             ;; Normal close
             (if (= depth 1) i (loop (+ i 1) (- depth 1) cd "")))))
        ((char=? (string-ref str i) #\\)
         (let ((cd (update-case-depth word case-depth)))
           (loop (+ i 2) depth cd "")))
        ((char=? (string-ref str i) #\') ;; skip single-quoted
         (let ((cd (update-case-depth word case-depth)))
           (let sq ((j (+ i 1)))
             (cond ((>= j len) (loop j depth cd ""))
                   ((char=? (string-ref str j) #\') (loop (+ j 1) depth cd ""))
                   (else (sq (+ j 1)))))))
        ((char=? (string-ref str i) #\") ;; skip double-quoted
         (let ((cd (update-case-depth word case-depth)))
           (let dq ((j (+ i 1)))
             (cond ((>= j len) (loop j depth cd ""))
                   ((char=? (string-ref str j) #\\) (dq (+ j 2)))
                   ((char=? (string-ref str j) #\") (loop (+ j 1) depth cd ""))
                   (else (dq (+ j 1)))))))
        ;; Word characters: accumulate for keyword detection
        ((or (char-alphabetic? (string-ref str i))
             (char=? (string-ref str i) #\_))
         (loop (+ i 1) depth case-depth
               (string-append word (string (string-ref str i)))))
        ;; Non-word: flush keyword
        (else
         (let ((cd (update-case-depth word case-depth)))
           (loop (+ i 1) depth cd "")))))))

(def (find-arith-close str start)
  ;; Find matching )) for $(( ... ))
  ;; Must track nested (( )) pairs AND single ( ) for expressions like ~(1|2)
  (let ((len (string-length str)))
    (let loop ((i start) (paren-depth 0))
      (cond
        ((>= i len) (- len 2))
        ;; Check for )) — only close if no open single parens
        ((and (= paren-depth 0)
              (char=? (string-ref str i) #\))
              (< (+ i 1) len)
              (char=? (string-ref str (+ i 1)) #\)))
         i)
        ;; Nested $(( — skip
        ((and (< (+ i 2) len)
              (char=? (string-ref str i) #\$)
              (char=? (string-ref str (+ i 1)) #\()
              (char=? (string-ref str (+ i 2)) #\())
         ;; Find matching )) for nested arith, then continue
         (let ((inner-close (find-arith-close str (+ i 3))))
           (loop (+ inner-close 2) paren-depth)))
        ;; $( — command substitution, skip to matching )
        ((and (< (+ i 1) len)
              (char=? (string-ref str i) #\$)
              (char=? (string-ref str (+ i 1)) #\())
         (let ((close (find-matching-paren str (+ i 2))))
           (loop (+ close 1) paren-depth)))
        ;; Single ( increases depth
        ((char=? (string-ref str i) #\()
         (loop (+ i 1) (+ paren-depth 1)))
        ;; Single ) decreases depth (must be inside parens)
        ((and (char=? (string-ref str i) #\))
              (> paren-depth 0))
         (loop (+ i 1) (- paren-depth 1)))
        (else (loop (+ i 1) paren-depth))))))

;;; --- Pattern matching helpers ---

;; Convert characters matching a glob pattern. If all? is #t, convert all
;; matching chars; if #f, only the first.
(def (case-convert-matching val pattern convert-fn all? (extglob? #f))
  (let* ((len (string-length val))
         (buf (open-output-string)))
    (let loop ((i 0) (converted? #f))
      (if (>= i len)
        (get-output-string buf)
        (let* ((ch (string-ref val i))
               (ch-str (string ch))
               (matches? (glob-match? pattern ch-str #f extglob?)))
          (if (and matches? (or all? (not converted?)))
            (begin (display (convert-fn ch) buf)
                   (loop (+ i 1) #t))
            (begin (display ch buf)
                   (loop (+ i 1) converted?))))))))

(def (remove-suffix val pattern longest? (extglob? #f))
  ;; Remove suffix matching pattern
  (if longest?
    ;; Longest: try from start
    (let loop ((i 0))
      (if (> i (string-length val))
        val
        (if (glob-match? pattern (substring val i (string-length val)) #f extglob?)
          (substring val 0 i)
          (loop (+ i 1)))))
    ;; Shortest: try from end
    (let loop ((i (string-length val)))
      (if (< i 0)
        val
        (if (glob-match? pattern (substring val i (string-length val)) #f extglob?)
          (substring val 0 i)
          (loop (- i 1)))))))

(def (remove-prefix val pattern longest? (extglob? #f))
  (if longest?
    (let loop ((i (string-length val)))
      (if (< i 0)
        val
        (if (glob-match? pattern (substring val 0 i) #f extglob?)
          (substring val i (string-length val))
          (loop (- i 1)))))
    (let loop ((i 0))
      (if (> i (string-length val))
        val
        (if (glob-match? pattern (substring val 0 i) #f extglob?)
          (substring val i (string-length val))
          (loop (+ i 1)))))))

;;; --- String helpers ---

(def (string-trim-trailing-newlines str)
  (let loop ((i (- (string-length str) 1)))
    (if (and (>= i 0) (char=? (string-ref str i) #\newline))
      (loop (- i 1))
      (substring str 0 (+ i 1)))))

(def (read-all-string port)
  (let ((buf (open-output-string)))
    (let loop ()
      (let ((ch (read-char port)))
        (if (eof-object? ch)
          (get-output-string buf)
          (begin (display ch buf) (loop)))))))

(def (append-map f lst)
  (apply append (map f lst)))

(def (string-find-char-from str ch start)
  (let loop ((i start))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

(def (string-trim-whitespace-str str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))

;;; --- Brace expansion ---

;; Expand brace expressions in a word. Returns a list of words.
;; {a,b,c} → (a b c), {1..5} → (1 2 3 4 5)
;; Preamble/postscript: pre{a,b}post → (preapost prebpost)
;; Nested: {a,b{1,2}} → (a b1 b2)
;; Does NOT expand inside quotes or ${...}
(def (brace-expand word)
  (let ((result (brace-expand-once word)))
    (if (and (= (length result) 1) (string=? (car result) word))
      result  ;; no expansion happened
      ;; Recursively expand each result (for nested braces)
      (append-map brace-expand result))))

;; Find and expand the first (leftmost) valid brace expression in word.
;; Returns a list of expanded words, or [word] if no valid brace found.
(def (brace-expand-once word)
  (let ((len (string-length word)))
    ;; Find the first valid { that isn't quoted or part of ${
    (let find-open ((i 0))
      (cond
        ((>= i len) [word])  ;; no brace found
        ;; Skip single-quoted regions
        ((char=? (string-ref word i) #\')
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) [word])
             ((char=? (string-ref word j) #\') (find-open (+ j 1)))
             (else (skip (+ j 1))))))
        ;; Skip double-quoted regions
        ((char=? (string-ref word i) #\")
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) [word])
             ((char=? (string-ref word j) #\") (find-open (+ j 1)))
             ((char=? (string-ref word j) #\\) (skip (+ j 2)))
             (else (skip (+ j 1))))))
        ;; Skip ${ parameter expansion
        ((and (char=? (string-ref word i) #\$)
              (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\{))
         (let skip ((j (+ i 2)) (depth 1))
           (cond
             ((>= j len) [word])
             ((char=? (string-ref word j) #\})
              (if (= depth 1) (find-open (+ j 1)) (skip (+ j 1) (- depth 1))))
             ((char=? (string-ref word j) #\{) (skip (+ j 1) (+ depth 1)))
             ((char=? (string-ref word j) #\\) (skip (+ j 2) depth))
             (else (skip (+ j 1) depth)))))
        ;; Skip backslash-escaped chars
        ((char=? (string-ref word i) #\\)
         (find-open (+ i 2)))
        ;; Found { — try to find matching }
        ((char=? (string-ref word i) #\{)
         (let ((close (find-brace-close word (+ i 1))))
           (if close
             (let ((content (substring word (+ i 1) close))
                   (preamble (substring word 0 i))
                   (postscript (substring word (+ close 1) len)))
               ;; Check if this is a valid brace expression
               (cond
                 ;; Comma-separated: {a,b,c}
                 ((brace-has-comma? content)
                  (let ((parts (brace-split-commas content)))
                    (map (lambda (part)
                           (string-append preamble part postscript))
                         parts)))
                 ;; Sequence: {start..end} or {start..end..step}
                 ((brace-sequence? content)
                  (let ((seq (brace-expand-sequence content)))
                    (if seq
                      (map (lambda (item)
                             (string-append preamble item postscript))
                           seq)
                      ;; Invalid sequence — skip this brace, continue searching
                      (find-open (+ close 1)))))
                 ;; Not valid — skip this brace, continue searching
                 (else (find-open (+ close 1)))))
             ;; No matching } — literal
             [word])))
        (else (find-open (+ i 1)))))))

;; Find matching } for brace expansion, respecting nesting and quoting
(def (find-brace-close str start)
  (let ((len (string-length str)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref str i) #\\) (loop (+ i 2) depth))
        ((char=? (string-ref str i) #\')
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) #f)
             ((char=? (string-ref str j) #\') (loop (+ j 1) depth))
             (else (skip (+ j 1))))))
        ((char=? (string-ref str i) #\")
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) #f)
             ((char=? (string-ref str j) #\") (loop (+ j 1) depth))
             ((char=? (string-ref str j) #\\) (skip (+ j 2)))
             (else (skip (+ j 1))))))
        ((char=? (string-ref str i) #\{) (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref str i) #\})
         (if (= depth 1) i (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth))))))

;; Check if brace content has an unquoted, unnested comma
(def (brace-has-comma? content)
  (let ((len (string-length content)))
    (let loop ((i 0) (depth 0))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref content i) #\\) (loop (+ i 2) depth))
        ((char=? (string-ref content i) #\{) (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref content i) #\}) (loop (+ i 1) (- depth 1)))
        ((and (char=? (string-ref content i) #\,) (= depth 0)) #t)
        (else (loop (+ i 1) depth))))))

;; Split brace content by top-level commas, respecting nesting
(def (brace-split-commas content)
  (let ((len (string-length content)))
    (let loop ((i 0) (start 0) (depth 0) (parts []))
      (cond
        ((>= i len)
         (reverse (cons (substring content start i) parts)))
        ((char=? (string-ref content i) #\\) (loop (+ i 2) start depth parts))
        ((char=? (string-ref content i) #\{) (loop (+ i 1) start (+ depth 1) parts))
        ((char=? (string-ref content i) #\}) (loop (+ i 1) start (- depth 1) parts))
        ((and (char=? (string-ref content i) #\,) (= depth 0))
         (loop (+ i 1) (+ i 1) depth
               (cons (substring content start i) parts)))
        (else (loop (+ i 1) start depth parts))))))

;; Check if content looks like a sequence: start..end or start..end..step
(def (brace-sequence? content)
  (and (string-contains? content "..")
       (not (brace-has-comma? content))))

;; Expand a brace sequence: "1..5" → ("1" "2" "3" "4" "5")
;; Supports numeric and alphabetic ranges, with optional step
(def (brace-expand-sequence content)
  (let* ((parts (string-split-dot-dot content))
         (nparts (length parts)))
    (cond
      ;; start..end — reject if either part is empty
      ((and (= nparts 2)
            (> (string-length (car parts)) 0)
            (> (string-length (cadr parts)) 0))
       (brace-range (car parts) (cadr parts) #f))
      ;; start..end..step — reject if any part is empty
      ((and (= nparts 3)
            (> (string-length (car parts)) 0)
            (> (string-length (cadr parts)) 0)
            (> (string-length (caddr parts)) 0))
       (brace-range (car parts) (cadr parts) (caddr parts)))
      (else #f))))

;; Split by ".." — returns list of 2 or 3 parts
(def (string-split-dot-dot str)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (parts []))
      (cond
        ((>= i len)
         (reverse (cons (substring str start i) parts)))
        ((and (< (+ i 1) len)
              (char=? (string-ref str i) #\.)
              (char=? (string-ref str (+ i 1)) #\.))
         (loop (+ i 2) (+ i 2)
               (cons (substring str start i) parts)))
        (else (loop (+ i 1) start parts))))))

;; Parse an integer string (with optional leading minus), rejecting floats
(def (brace-parse-int str)
  (let ((len (string-length str)))
    (and (> len 0)
         (let* ((start (if (and (> len 1) (char=? (string-ref str 0) #\-)) 1 0))
                (has-digit? #f))
           (let loop ((i start))
             (if (>= i len)
               (and has-digit? (string->number str))
               (if (char-numeric? (string-ref str i))
                 (begin (set! has-digit? #t) (loop (+ i 1)))
                 #f)))))))

;; Generate a range from start to end with optional step
(def (brace-range start-str end-str step-str)
  (let ((start-num (brace-parse-int start-str))
        (end-num (brace-parse-int end-str))
        (step-num (and step-str (brace-parse-int step-str))))
    (cond
      ;; Numeric range
      ((and start-num end-num)
       (let* (;; Bash uses absolute value of step, direction from start/end
              ;; Step 0 is treated as default (step 1 in natural direction)
              (raw-step (or (and step-num (if (= step-num 0)
                                            (if (<= start-num end-num) 1 -1)
                                            step-num))
                            (if (< end-num start-num) -1 1)))
              (step (if (<= start-num end-num) (abs raw-step) (- (abs raw-step))))
              ;; Zero-padding: if either has leading zeros, pad to max width
              (pad-width (max (string-length start-str) (string-length end-str)))
              (needs-pad? (or (and (> (string-length start-str) 1)
                                   (char=? (string-ref start-str 0) #\0))
                              (and (> (string-length end-str) 1)
                                   (char=? (string-ref end-str 0) #\0)))))
         (cond
           ((= step 0) #f) ;; shouldn't happen now, but safety guard
           (else
            (let loop ((i start-num) (result []))
              (if (if (> step 0) (> i end-num) (< i end-num))
                (reverse result)
                (loop (+ i step)
                      (cons (if needs-pad?
                              (pad-number i pad-width)
                              (number->string i))
                            result))))))))
      ;; Single-char alphabetic range
      ;; Mixed case (z..A) is treated as invalid — bash has a bug with backtick in result
      ((and (= (string-length start-str) 1) (= (string-length end-str) 1)
            (char-alphabetic? (string-ref start-str 0))
            (char-alphabetic? (string-ref end-str 0))
            (eq? (char-upper-case? (string-ref start-str 0))
                 (char-upper-case? (string-ref end-str 0))))
       (let* ((start-ch (char->integer (string-ref start-str 0)))
              (end-ch (char->integer (string-ref end-str 0)))
              ;; Bash uses absolute value of step, direction from start/end
              ;; Step 0 is treated as default (step 1 in natural direction)
              (raw-step (or (and step-num (let ((s (inexact->exact step-num)))
                                           (if (= s 0)
                                             (if (<= start-ch end-ch) 1 -1)
                                             s)))
                            (if (<= start-ch end-ch) 1 -1)))
              (step (if (<= start-ch end-ch) (abs raw-step) (- (abs raw-step)))))
         (cond
           ((= step 0) #f) ;; safety guard
           (else
            (let loop ((i start-ch) (result []))
              (if (if (> step 0) (> i end-ch) (< i end-ch))
                (reverse result)
                (loop (+ i step)
                      (cons (string (integer->char i)) result))))))))
      (else #f))))

;; Pad an integer to a given width with leading zeros
(def (pad-number n width)
  (let* ((s (number->string (abs n)))
         (padding (max 0 (- width (string-length s) (if (< n 0) 1 0)))))
    (string-append (if (< n 0) "-" "")
                   (make-string padding #\0)
                   s)))
