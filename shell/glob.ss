;;; glob.ss — Pathname expansion (globbing) for gsh

(export #t)
(import ./pregexp-compat
        :std/sort
        :std/sugar
        :std/iter
        :gsh/util)

;;; --- Public interface ---

;; Check if a string contains unquoted glob metacharacters
;; When extglob? is true, also detects ?(, *(, +(, @(, !( patterns
(def (glob-pattern? str (extglob? #f))
  (let ((len (string-length str)))
    (let loop ((i 0) (escaped? #f))
      (if (>= i len)
        #f
        (let ((ch (string-ref str i)))
          (cond
            (escaped? (loop (+ i 1) #f))
            ((char=? ch #\\) (loop (+ i 1) #t))
            ((or (char=? ch #\*) (char=? ch #\?) (char=? ch #\[))
             #t)
            ;; Extended glob: ?( *( +( @( !(
            ((and extglob?
                  (< (+ i 1) len)
                  (char=? (string-ref str (+ i 1)) #\()
                  (or (char=? ch #\?) (char=? ch #\*) (char=? ch #\+)
                      (char=? ch #\@) (char=? ch #\!)))
             #t)
            (else (loop (+ i 1) #f))))))))

;; Match a glob pattern against a string
;; Returns #t if the string matches the pattern
;; When path-mode? is #t (default), * and ? don't match /
;; When path-mode? is #f, * and ? match any character (for [[ ]] pattern matching)
(def (glob-match? pattern string (path-mode? #t) (extglob? #f))
  (let ((rx (glob-pattern->pregexp pattern path-mode? extglob?)))
    (and (pregexp-match rx string) #t)))

;; Remove glob escape backslashes from a pattern (for no-match fallback)
;; \[ → [, \* → *, \? → ?, \\ → \, etc.
(def (glob-remove-escapes str)
  (let ((len (string-length str)))
    (if (not (string-index str #\\))
      str  ;; fast path
      (let ((buf (open-output-string)))
        (let loop ((i 0))
          (if (>= i len)
            (get-output-string buf)
            (let ((ch (string-ref str i)))
              (if (and (char=? ch #\\) (< (+ i 1) len))
                (begin (display (string-ref str (+ i 1)) buf)
                       (loop (+ i 2)))
                (begin (display ch buf)
                       (loop (+ i 1)))))))))))
;; Expand a glob pattern to a sorted list of matching file paths
;; Returns the original pattern (as a list) if no matches (unless nullglob/failglob)
;; Options: dotglob? — include dotfiles, nullglob? — return empty on no match,
;;          failglob? — error on no match, nocase? — case-insensitive
(def (glob-expand pattern
                   dotglob?: (dotglob? #f)
                   nullglob?: (nullglob? #f)
                   failglob?: (failglob? #f)
                   nocase?: (nocase? #f)
                   extglob?: (extglob? #f)
                   globskipdots?: (globskipdots? #t))
  (let ((matches (glob-expand-path pattern dotglob? nocase? extglob? globskipdots?)))
    (cond
      ((pair? matches) (sort matches string<?))
      (failglob?
       (raise (cons 'failglob pattern)))
      (nullglob? [])
      (else [pattern]))))

;;; --- Glob-to-regex conversion ---

;; Convert a glob pattern to a pregexp string
;; When extglob? is true, recognizes ?(pat|pat) *(pat|pat) +(pat|pat) @(pat|pat) !(pat|pat)
(def (glob-pattern->pregexp pattern (path-mode? #t) (extglob? #f))
  (let ((rx (open-output-string))
        (len (string-length pattern)))
    (display "^" rx)
    ;; in-bracket? is #f (outside), 'start (first char after [/[!/[^), or #t (inside)
    (let loop ((i 0) (in-bracket? #f))
      (if (>= i (string-length pattern))
        (begin (display "$" rx)
               ;; Return STRING not compiled regex - let pregexp-match cache it!
               (get-output-string rx))
        (let ((ch (string-ref pattern i)))
          (cond
            ;; Inside bracket expression
            (in-bracket?
             (cond
               ;; ] as first char after [ or [!/[^ is literal
               ((and (char=? ch #\]) (eq? in-bracket? 'start))
                (display "\\]" rx)
                (loop (+ i 1) #t))
               ((char=? ch #\])
                (display "]" rx)
                (loop (+ i 1) #f))
               ;; ! or ^ at start of bracket = negation
               ((and (memq ch '(#\! #\^)) (eq? in-bracket? 'start))
                (display "^" rx)
                ;; Check if ] follows immediately after negation
                (if (and (< (+ i 1) (string-length pattern))
                         (char=? (string-ref pattern (+ i 1)) #\]))
                  (begin (display "\\]" rx)
                         (loop (+ i 2) #t))
                  (loop (+ i 1) #t)))
               ;; POSIX character class [:name:]
               ((and (char=? ch #\[)
                     (< (+ i 1) (string-length pattern))
                     (char=? (string-ref pattern (+ i 1)) #\:))
                (let ((class-end (find-posix-class-end pattern (+ i 2))))
                  (if class-end
                    (let* ((class-name (substring pattern (+ i 2) class-end))
                           (class-chars (posix-class->regex-chars class-name)))
                      (if class-chars
                        (begin (display class-chars rx)
                               (loop (+ class-end 2) #t)) ;; skip past :]
                        ;; Unknown class, treat [ as literal
                        (begin (display "\\[" rx)
                               (loop (+ i 1) #t))))
                    ;; No closing :], treat [ as literal
                    (begin (display "\\[" rx)
                           (loop (+ i 1) #t)))))
               ;; [ inside bracket that's not a POSIX class — escape it
               ((char=? ch #\[)
                (display "\\[" rx)
                (loop (+ i 1) #t))
               ;; Backslash escape inside brackets: \] stays in bracket, etc.
               ((and (char=? ch #\\) (< (+ i 1) (string-length pattern)))
                (let ((esc-ch (string-ref pattern (+ i 1))))
                  ;; Inside bracket, dash must be escaped or it forms a range
                  (if (char=? esc-ch #\-)
                    (display "\\-" rx)
                    (display (pregexp-quote-char esc-ch) rx)))
                (loop (+ i 2) #t))
               (else
                (display (pregexp-quote-char ch) rx)
                (loop (+ i 1) #t))))
            ;; Backslash escape
            ((char=? ch #\\)
             (if (< (+ i 1) (string-length pattern))
               (begin
                 (display (pregexp-quote-char (string-ref pattern (+ i 1))) rx)
                 (loop (+ i 2) #f))
               (begin
                 (display "\\\\" rx)
                 (loop (+ i 1) #f))))
            ;; Extended glob: ?(pat|...) *(pat|...) +(pat|...) @(pat|...) !(pat|...)
            ((and extglob?
                  (< (+ i 1) len)
                  (char=? (string-ref pattern (+ i 1)) #\()
                  (or (char=? ch #\?) (char=? ch #\*) (char=? ch #\+)
                      (char=? ch #\@) (char=? ch #\!)))
             (let ((close (find-extglob-close pattern (+ i 2))))
               (if close
                 (let* ((body (substring pattern (+ i 2) close))
                        (alternatives (extglob-split-pipes body))
                        (rx-alts (glob-join-alternatives
                                  (map (lambda (alt)
                                         (glob-sub-pattern->pregexp alt path-mode? extglob?))
                                       alternatives)
                                  "|")))
                   (cond
                     ((char=? ch #\?) (display (string-append "(?:" rx-alts ")?") rx))
                     ((char=? ch #\*) (display (string-append "(?:" rx-alts ")*") rx))
                     ((char=? ch #\+) (display (string-append "(?:" rx-alts ")+") rx))
                     ((char=? ch #\@) (display (string-append "(?:" rx-alts ")") rx))
                     ((char=? ch #\!)
                      ;; For !(pat), include rest of pattern in negative lookahead
                      ;; so !(foo|bar).txt generates (?!(?:foo|bar)\.txt$)[^/]*
                      (let ((rest-rx (glob-sub-pattern->pregexp
                                      (substring pattern (+ close 1) len)
                                      path-mode? extglob?)))
                        (display (string-append "(?!(?:" rx-alts ")" rest-rx "$)"
                                                (if path-mode? "[^/]*" ".*"))
                                 rx))))
                   (loop (+ close 1) #f))
                 ;; No matching close paren — treat as literal
                 (begin
                   (display (pregexp-quote-char ch) rx)
                   (loop (+ i 1) #f)))))
            ;; Glob metacharacters
            ((char=? ch #\*)
             ;; Check for **
             (if (and (< (+ i 1) (string-length pattern))
                      (char=? (string-ref pattern (+ i 1)) #\*))
               (begin
                 (display ".*" rx)  ;; ** matches across /
                 (loop (+ i 2) #f))
               (if path-mode?
                 (begin
                   (display "[^/]*" rx)  ;; * matches anything except /
                   (loop (+ i 1) #f))
                 (begin
                   (display "[\\s\\S]*" rx)  ;; * matches any char including newline
                   (loop (+ i 1) #f)))))
            ((char=? ch #\?)
             ;; In path mode, ? matches any char except /
             ;; In non-path mode (strip, [[ ]]), ? matches any char including newline
             (display (if path-mode? "[^/]" "[\\s\\S]") rx)
             (loop (+ i 1) #f))
            ((char=? ch #\[)
             ;; Check if there's a closing ] for this bracket expression
             ;; In glob syntax, ] right after [ (or [! / [^) is literal
             (let* ((start (+ i 1))
                    (skip (cond ((>= start (string-length pattern)) 0)
                                ((char=? (string-ref pattern start) #\!) (+ 1 (if (and (< (+ start 1) (string-length pattern))
                                                                                        (char=? (string-ref pattern (+ start 1)) #\]))
                                                                                    1 0)))
                                ((char=? (string-ref pattern start) #\^) (+ 1 (if (and (< (+ start 1) (string-length pattern))
                                                                                        (char=? (string-ref pattern (+ start 1)) #\]))
                                                                                    1 0)))
                                ((char=? (string-ref pattern start) #\]) 1)
                                (else 0)))
                    (has-close? (let find ((j (+ start skip)))
                                  (cond ((>= j (string-length pattern)) #f)
                                        ((char=? (string-ref pattern j) #\]) #t)
                                        (else (find (+ j 1)))))))
               (if has-close?
                 (begin (display "[" rx) (loop (+ i 1) 'start))
                 ;; No closing ] — treat [ as literal
                 (begin (display "\\[" rx) (loop (+ i 1) #f)))))
            ;; Regex special chars that need escaping
            (else
             (display (pregexp-quote-char ch) rx)
             (loop (+ i 1) #f))))))))

;; Quote a single char for pregexp
(def (pregexp-quote-char ch)
  (let ((s (string ch)))
    (if (pregexp-match "[\\\\\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}]" s)
      (string-append "\\" s)
      s)))

;;; --- POSIX character class helpers ---

;; Find the position of ':' in ':]' after start, within a bracket expression
;; Returns the index of ':', or #f if not found
(def (find-posix-class-end pattern start)
  (let ((len (string-length pattern)))
    (let loop ((i start))
      (cond
        ((>= (+ i 1) len) #f)
        ((and (char=? (string-ref pattern i) #\:)
              (char=? (string-ref pattern (+ i 1)) #\]))
         i)
        ;; Hit ] before :] — not a valid class
        ((char=? (string-ref pattern i) #\]) #f)
        (else (loop (+ i 1)))))))

;; Convert a POSIX class name to regex character set (for inside [...])
(def (posix-class->regex-chars name)
  (cond
    ((string=? name "alpha")  "a-zA-Z")
    ((string=? name "digit")  "0-9")
    ((string=? name "alnum")  "a-zA-Z0-9")
    ((string=? name "upper")  "A-Z")
    ((string=? name "lower")  "a-z")
    ((string=? name "xdigit") "0-9a-fA-F")
    ((string=? name "space")  " \t\n\r\x0b;\x0c;")
    ((string=? name "blank")  " \t")
    ((string=? name "print")  " -~")
    ((string=? name "graph")  "!-~")
    ((string=? name "cntrl")  "\x00;-\x1f;\x7f;")
    ((string=? name "punct")  "!-/:-@\\[-`{-~")
    ((string=? name "ascii")  "\x00;-\x7f;")
    (else #f)))

;;; --- Extended glob helpers ---

;; Find the matching close paren for an extglob pattern, starting after the '('
;; Handles nested parens and escapes
(def (find-extglob-close pattern start)
  (let ((len (string-length pattern)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref pattern i) #\\)
         (loop (+ i 2) depth))  ;; skip escaped char
        ((char=? (string-ref pattern i) #\()
         (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref pattern i) #\))
         (if (= depth 1)
           i
           (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth))))))

;; Split extglob body by | at the top level (not inside nested parens)
(def (extglob-split-pipes body)
  (let ((len (string-length body)))
    (let loop ((i 0) (start 0) (depth 0) (parts []))
      (cond
        ((>= i len)
         (reverse (cons (substring body start i) parts)))
        ((char=? (string-ref body i) #\\)
         (loop (+ i 2) start depth parts))
        ((char=? (string-ref body i) #\()
         (loop (+ i 1) start (+ depth 1) parts))
        ((char=? (string-ref body i) #\))
         (loop (+ i 1) start (- depth 1) parts))
        ((and (char=? (string-ref body i) #\|) (= depth 0))
         (loop (+ i 1) (+ i 1) depth
               (cons (substring body start i) parts)))
        (else (loop (+ i 1) start depth parts))))))

;; Convert a glob sub-pattern (inside extglob) to a pregexp fragment (no ^ or $)
(def (glob-sub-pattern->pregexp pattern path-mode? extglob?)
  (let ((rx (open-output-string))
        (len (string-length pattern)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string rx)
        (let ((ch (string-ref pattern i)))
          (cond
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (begin
                 (display (pregexp-quote-char (string-ref pattern (+ i 1))) rx)
                 (loop (+ i 2)))
               (begin
                 (display "\\\\" rx)
                 (loop (+ i 1)))))
            ;; Nested extglob
            ((and extglob?
                  (< (+ i 1) len)
                  (char=? (string-ref pattern (+ i 1)) #\()
                  (or (char=? ch #\?) (char=? ch #\*) (char=? ch #\+)
                      (char=? ch #\@) (char=? ch #\!)))
             (let ((close (find-extglob-close pattern (+ i 2))))
               (if close
                 (let* ((body (substring pattern (+ i 2) close))
                        (alternatives (extglob-split-pipes body))
                        (rx-alts (glob-join-alternatives
                                  (map (lambda (alt)
                                         (glob-sub-pattern->pregexp alt path-mode? extglob?))
                                       alternatives)
                                  "|")))
                   (cond
                     ((char=? ch #\?) (display (string-append "(?:" rx-alts ")?") rx))
                     ((char=? ch #\*) (display (string-append "(?:" rx-alts ")*") rx))
                     ((char=? ch #\+) (display (string-append "(?:" rx-alts ")+") rx))
                     ((char=? ch #\@) (display (string-append "(?:" rx-alts ")") rx))
                     ((char=? ch #\!)
                      (let ((rest-rx (glob-sub-pattern->pregexp
                                      (substring pattern (+ close 1) len)
                                      path-mode? extglob?)))
                        (display (string-append "(?!(?:" rx-alts ")" rest-rx "$)"
                                                (if path-mode? "[^/]*" ".*"))
                                 rx))))
                   (loop (+ close 1)))
                 (begin
                   (display (pregexp-quote-char ch) rx)
                   (loop (+ i 1))))))
            ((char=? ch #\*)
             (display (if path-mode? "[^/]*" ".*") rx)
             (loop (+ i 1)))
            ((char=? ch #\?)
             (display (if path-mode? "[^/]" ".") rx)
             (loop (+ i 1)))
            ((char=? ch #\[)
             ;; Pass through bracket expression, handling backslash escapes
             (display "[" rx)
             (let bloop ((j (+ i 1)))
               (cond
                 ((>= j len) (loop j))
                 ((char=? (string-ref pattern j) #\])
                  (display "]" rx)
                  (loop (+ j 1)))
                 ;; Backslash escape inside brackets — pass through to regex
                 ((and (char=? (string-ref pattern j) #\\)
                       (< (+ j 1) len))
                  (display "\\" rx)
                  (display (string-ref pattern (+ j 1)) rx)
                  (bloop (+ j 2)))
                 (else
                  (display (string-ref pattern j) rx)
                  (bloop (+ j 1))))))
            (else
             (display (pregexp-quote-char ch) rx)
             (loop (+ i 1)))))))))

;; Join strings with a separator (O(n) via output port)
(def (glob-join-alternatives strs sep)
  (if (null? strs)
    ""
    (call-with-output-string
      (lambda (port)
        (display (car strs) port)
        (for-each (lambda (s)
                    (display sep port)
                    (display s port))
                  (cdr strs))))))

;;; --- Path expansion ---

;; Expand a glob pattern against the filesystem
;; Handles path components separately: dir/pattern
(def (glob-expand-path pattern (dotglob? #f) (nocase? #f) (extglob? #f) (globskipdots? #t))
  (let ((parts (split-glob-path pattern)))
    (if (null? parts)
      []
      (let ((start (if (and (> (string-length pattern) 0)
                            (char=? (string-ref pattern 0) #\/))
                     "/"
                     ".")))
        (glob-expand-parts parts start (char=? (string-ref pattern 0) #\/)
                           dotglob? nocase? extglob? globskipdots?)))))

;; Split a glob path into components
(def (split-glob-path path)
  (let loop ((i 0) (start 0) (parts []))
    (cond
      ((>= i (string-length path))
       (reverse (if (> i start)
                  (cons (substring path start i) parts)
                  parts)))
      ((char=? (string-ref path i) #\/)
       (loop (+ i 1) (+ i 1)
             (if (> i start)
               (cons (substring path start i) parts)
               parts)))
      (else
       (loop (+ i 1) start parts)))))

;; Expand a list of path components against the filesystem
(def (glob-expand-parts parts base absolute? (dotglob? #f) (nocase? #f) (extglob? #f) (globskipdots? #t))
  (if (null? parts)
    [(if absolute? base
         (if (string=? base ".") "" base))]
    (let* ((pattern (car parts))
           (rest (cdr parts))
           (entries (glob-match-dir base pattern dotglob? nocase? extglob? globskipdots?)))
      (let loop ((entries entries) (results []))
        (if (null? entries)
          results
          (let* ((entry (car entries))
                 (full (if (string=? base ".")
                         entry
                         (if (string=? base "/")
                           (string-append "/" entry)
                           (string-append base "/" entry)))))
            (if (null? rest)
              (loop (cdr entries) (cons full results))
              ;; More components: recurse only if this is a directory
              (if (directory-exists? full)
                (loop (cdr entries)
                      (append (glob-expand-parts rest full absolute?
                                                 dotglob? nocase? extglob? globskipdots?) results))
                (loop (cdr entries) results)))))))))

;; Match entries in a directory against a glob pattern
(def (glob-match-dir dir pattern (dotglob? #f) (nocase? #f) (extglob? #f) (globskipdots? #t))
  (with-catch
   (lambda (e) [])
   (lambda ()
     ;; For case-insensitive matching, lowercase both pattern and entries
     (let* ((rx (glob-pattern->pregexp (if nocase? (string-downcase pattern) pattern) #t extglob?))
            (show-dots? (or dotglob?
                           (and (> (string-length pattern) 0)
                                (char=? (string-ref pattern 0) #\.))))
            ;; Use ignore-hidden: #f to get dotfiles when needed
            (entries (if show-dots?
                      (directory-files [path: dir ignore-hidden: #f])
                      (directory-files dir))))
       (filter
        (lambda (entry)
          (and ;; Exclude . and .. when globskipdots is on (default)
               (or (not globskipdots?)
                   (and (not (string=? entry "."))
                        (not (string=? entry ".."))))
               ;; Dotfile filter (when not showing dots)
               (or show-dots?
                   (not (and (> (string-length entry) 0)
                             (char=? (string-ref entry 0) #\.))))
               (pregexp-match rx (if nocase? (string-downcase entry) entry))))
        entries)))))

;; Check if path is a directory
(def (directory-exists? path)
  (file-directory? path))

;;; --- GLOBIGNORE filtering ---

;; Filter glob results by GLOBIGNORE (colon-separated patterns)
;; Removes entries whose basename matches any GLOBIGNORE pattern
(def (glob-ignore-filter paths globignore-str)
  (let ((patterns (string-split-colon globignore-str)))
    (if (null? patterns)
      paths
      (filter
       (lambda (path)
         (let ((base (path-basename path)))
           (not (glob-any? (lambda (pat) (glob-match? pat base #f))
                        patterns))))
       paths))))

;; Split a string by colons
(def (string-split-colon str)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (parts []))
      (cond
        ((>= i len)
         (reverse (if (> i start)
                    (cons (substring str start i) parts)
                    parts)))
        ((char=? (string-ref str i) #\:)
         (loop (+ i 1) (+ i 1)
               (if (> i start)
                 (cons (substring str start i) parts)
                 parts)))
        (else (loop (+ i 1) start parts))))))

;; Get the basename of a path
(def (path-basename path)
  (let ((pos (let loop ((i (- (string-length path) 1)))
               (cond ((< i 0) #f)
                     ((char=? (string-ref path i) #\/) i)
                     (else (loop (- i 1)))))))
    (if pos
      (substring path (+ pos 1) (string-length path))
      path)))

;; Check if any element satisfies predicate (glob-local helper)
(def (glob-any? pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (glob-any? pred (cdr lst)))))

;;; --- Extended glob patterns (extglob) ---
;; ?(pat) *(pat) +(pat) @(pat) !(pat)

;;; --- Utility ---

;; Check if a string has any glob metacharacters (unescaped)
(def (needs-globbing? word)
  (glob-pattern? word))
