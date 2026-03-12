;;; history.ss — Command history for jsh
;;;
;;; Entries stored as 3-element vectors: #(timestamp cwd command)
;;; File format: EPOCH\tCWD\tCOMMAND  (tab-separated, one per line)
;;; Backward-compatible: plain text lines loaded as #(0 "" command)

(export #t)
(import :std/sugar
        :std/format
        ./pregexp-compat
        :jsh/util)

;;; --- History entry accessors ---
;;; Entries are 3-element vectors: #(timestamp cwd command)

(def (make-history-entry timestamp cwd command)
  (vector timestamp cwd command))

(def (history-entry-timestamp entry)
  (vector-ref entry 0))

(def (history-entry-cwd entry)
  (vector-ref entry 1))

(def (history-entry-command entry)
  (vector-ref entry 2))

;;; --- History state ---

(defstruct history-state
  (entries       ;; vector of history-entry (ring buffer)
   count         ;; number of entries added
   max-size      ;; $HISTSIZE
   file          ;; $HISTFILE path
   file-max      ;; $HISTFILESIZE
   control       ;; list of symbols: ignorespace ignoredups erasedups
   ignore-pats   ;; list of glob patterns (HISTIGNORE)
   )
  transparent: #t)

(def *history* #f)
(def *last-subst* #f)  ;; last :s/old/new/ for :& repeat

;;; --- Public interface ---

(def (history-init! histfile histsize (filesize #f))
  (set! *history*
    (make-history-state
      (make-vector (or histsize 1000) #f)
      0
      (or histsize 1000)
      (expand-tilde (or histfile "~/.gsh_history"))
      (or filesize (* 2 (or histsize 1000)))
      []    ;; control
      []))  ;; ignore patterns
  (history-load!))

;; Add a line to history (with timestamp and CWD)
(def (history-add! line)
  (when (and *history* (> (string-length (string-trim line)) 0))
    (let ((trimmed (string-trim line)))
      ;; Check HISTCONTROL
      (unless (history-should-ignore? trimmed)
        (let* ((h *history*)
               (idx (modulo (history-state-count h)
                            (history-state-max-size h)))
               (entry (make-history-entry
                        (inexact->exact (floor (current-second)))
                        (with-catch (lambda (e) "") current-directory)
                        trimmed)))
          (vector-set! (history-state-entries h) idx entry)
          (set! (history-state-count h) (+ 1 (history-state-count h))))))))

;; Get raw entry by absolute number (1-based) — returns history-entry vector or #f
(def (history-get-entry n)
  (when *history*
    (let* ((h *history*)
           (total (history-state-count h))
           (max-sz (history-state-max-size h)))
      (if (and (> n 0) (<= n total))
        (let ((raw (vector-ref (history-state-entries h) (modulo (- n 1) max-sz))))
          (and raw (if (vector? raw) raw (make-history-entry 0 "" raw))))
        #f))))

;; Get command string by absolute number (1-based) — backward compatible
(def (history-get n)
  (let ((entry (history-get-entry n)))
    (and entry (history-entry-command entry))))

;; Get raw entry by offset from end (0 = most recent)
(def (history-get-entry-relative offset)
  (when *history*
    (let* ((h *history*)
           (total (history-state-count h)))
      (history-get-entry (- total offset)))))

;; Get command string by offset from end (0 = most recent) — backward compatible
(def (history-get-relative offset)
  (let ((entry (history-get-entry-relative offset)))
    (and entry (history-entry-command entry))))

;; Search history for entries starting with prefix (returns list of command strings)
(def (history-search prefix)
  (if (not *history*) []
      (let* ((h *history*)
             (entries (history-list)))
        (filter (lambda (cmd)
                  (and (>= (string-length cmd) (string-length prefix))
                       (string=? (substring cmd 0 (string-length prefix)) prefix)))
                entries))))

;; Reverse search for entry containing substring (returns command string or #f)
(def (history-search-reverse substr)
  (if (not *history*) #f
      (let* ((h *history*)
             (total (history-state-count h))
             (max-sz (history-state-max-size h))
             (start (min total max-sz)))
        (let loop ((i (- start 1)))
          (if (< i 0) #f
              (let* ((idx (modulo (- total 1 (- (- start 1) i)) max-sz))
                     (raw (vector-ref (history-state-entries h) idx))
                     (cmd (cond ((not raw) #f)
                                ((vector? raw) (history-entry-command raw))
                                (else raw))))
                (if (and cmd (string-contains? cmd substr))
                  cmd
                  (loop (- i 1)))))))))

;; Get all history entries as entry vectors (oldest first)
(def (history-entries)
  (if (not *history*) []
      (let* ((h *history*)
             (total (history-state-count h))
             (max-sz (history-state-max-size h))
             (count (min total max-sz)))
        (let loop ((i 0) (result []))
          (if (>= i count)
            (reverse result)
            (let* ((abs-num (+ (max 1 (- total max-sz -1)) i))
                   (idx (modulo (- abs-num 1) max-sz))
                   (raw (vector-ref (history-state-entries h) idx)))
              (loop (+ i 1)
                    (cond
                      ((not raw) result)
                      ((vector? raw) (cons raw result))
                      (else (cons (make-history-entry 0 "" raw) result))))))))))

;; Get all history command strings as a list (oldest first) — backward compatible
(def (history-list)
  (map history-entry-command (history-entries)))

;; Total number of history entries
(def (history-count)
  (if *history* (min (history-state-count *history*)
                      (history-state-max-size *history*))
      0))

;; Clear all history
(def (history-clear!)
  (when *history*
    (let ((h *history*))
      (set! (history-state-entries h)
            (make-vector (history-state-max-size h) #f))
      (set! (history-state-count h) 0))))

;;; --- Word splitting for history designators ---
;; Splits a command into words, respecting single/double quotes

(def (history-split-words str)
  (let ((len (string-length str)))
    (let loop ((i 0) (words []) (buf (open-output-string)) (in-word? #f))
      (cond
        ((>= i len)
         (if in-word?
           (reverse (cons (get-output-string buf) words))
           (reverse words)))
        ;; Whitespace outside quotes
        ((char-whitespace? (string-ref str i))
         (if in-word?
           (loop (+ i 1) (cons (get-output-string buf) words)
                 (open-output-string) #f)
           (loop (+ i 1) words buf #f)))
        ;; Single-quoted string
        ((char=? (string-ref str i) #\')
         (display #\' buf)
         (let qloop ((j (+ i 1)))
           (cond
             ((>= j len) (loop j words buf #t))
             ((char=? (string-ref str j) #\')
              (display #\' buf)
              (loop (+ j 1) words buf #t))
             (else (display (string-ref str j) buf)
                   (qloop (+ j 1))))))
        ;; Double-quoted string
        ((char=? (string-ref str i) #\")
         (display #\" buf)
         (let qloop ((j (+ i 1)))
           (cond
             ((>= j len) (loop j words buf #t))
             ((char=? (string-ref str j) #\")
              (display #\" buf)
              (loop (+ j 1) words buf #t))
             ((and (char=? (string-ref str j) #\\) (< (+ j 1) len))
              (display (string-ref str j) buf)
              (display (string-ref str (+ j 1)) buf)
              (qloop (+ j 2)))
             (else (display (string-ref str j) buf)
                   (qloop (+ j 1))))))
        ;; Regular character
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1) words buf #t))))))

;; Select words from a word list using a designator
;; designator: integer N, 'caret (^), 'dollar ($), 'star (*),
;;             (range start end) where end is integer or 'dollar or 'minus-one
(def (select-words words designator)
  (let ((n (length words)))
    (cond
      ((eq? designator 'caret)
       (if (> n 1) (list-ref words 1) ""))
      ((eq? designator 'dollar)
       (if (> n 0) (list-ref words (- n 1)) ""))
      ((eq? designator 'star)
       (if (> n 1)
         (string-join (list-tail words 1) " ")
         ""))
      ((integer? designator)
       (if (and (>= designator 0) (< designator n))
         (list-ref words designator)
         ""))
      ((and (pair? designator) (eq? (car designator) 'range))
       (let* ((start (cadr designator))
              (end-spec (cddr designator))
              (end (cond ((eq? end-spec 'dollar) (- n 1))
                         ((eq? end-spec 'minus-one) (max 0 (- n 2)))
                         (else (min end-spec (- n 1))))))
         (if (and (>= start 0) (< start n) (>= end start))
           (string-join
             (let loop ((i start) (acc []))
               (if (> i end) (reverse acc)
                   (loop (+ i 1) (cons (list-ref words i) acc))))
             " ")
           "")))
      (else (string-join words " ")))))

;;; --- Modifiers ---

(def (apply-history-modifier text modifier)
  (cond
    ((eq? modifier 'h) ;; head (dirname)
     (let ((pos (string-last-index-of text #\/)))
       (cond ((not pos) text)
             ((= pos 0) "/")
             (else (substring text 0 pos)))))
    ((eq? modifier 't) ;; tail (basename)
     (let ((pos (string-last-index-of text #\/)))
       (if pos (substring text (+ pos 1) (string-length text)) text)))
    ((eq? modifier 'r) ;; remove trailing suffix
     (let ((pos (string-last-index-of text #\.)))
       (if (and pos (> pos 0)
                (let ((before (string-ref text (- pos 1))))
                  (not (char=? before #\/))))
         (substring text 0 pos)
         text)))
    ((eq? modifier 'e) ;; extension only
     (let ((pos (string-last-index-of text #\.)))
       (if pos (substring text pos (string-length text)) "")))
    ((eq? modifier 'q) ;; single-quote
     (string-append "'" text "'"))
    ((eq? modifier 'x) ;; quote and break into words
     (string-join (map (lambda (w) (string-append "'" w "'"))
                       (history-split-words text)) " "))
    ((eq? modifier 'p) text) ;; print-only, handled at call site
    ((and (pair? modifier) (eq? (car modifier) 'subst))
     (let ((old (cadr modifier))
           (new (caddr modifier))
           (global? (cadddr modifier)))
       (set! *last-subst* (cons old new))
       (if global?
         (string-replace-all text old new)
         (string-replace-first text old new))))
    ((eq? modifier 'repeat-subst) ;; :& repeat last substitution
     (if *last-subst*
       (string-replace-first text (car *last-subst*) (cdr *last-subst*))
       text))
    (else text)))

;;; --- History expansion ---

;; Parse event designator starting after !
;; Returns (values event-string end-pos) or raises error
(def (parse-event line pos)
  (let ((len (string-length line)))
    (cond
      ;; !! = last command
      ((and (< pos len) (char=? (string-ref line pos) #\!))
       (let ((entry (history-get-relative 0)))
         (if entry (values entry (+ pos 1))
             (error "!!: event not found"))))
      ;; !# = current line so far (literal input text before !#)
      ((and (< pos len) (char=? (string-ref line pos) #\#))
       (values (substring line 0 (- pos 1)) (+ pos 1)))
      ;; !-N = relative
      ((and (< pos len) (char=? (string-ref line pos) #\-))
       (let* ((num-end (find-number-end line (+ pos 1)))
              (num-str (substring line (+ pos 1) num-end))
              (n (string->number num-str)))
         (if n
           (let ((entry (history-get-relative (- n 1))))
             (if entry (values entry num-end)
                 (error (format "!-~a: event not found" n))))
           (error "!-: bad event specification"))))
      ;; !N = absolute command number
      ((and (< pos len) (char-numeric? (string-ref line pos)))
       (let* ((num-end (find-number-end line pos))
              (num-str (substring line pos num-end))
              (n (string->number num-str)))
         (if n
           (let ((entry (history-get n)))
             (if entry (values entry num-end)
                 (error (format "!~a: event not found" n))))
           (error "bad event specification"))))
      ;; !?string? = search containing
      ((and (< pos len) (char=? (string-ref line pos) #\?))
       (let* ((close (string-index-of line #\? (+ pos 1)))
              ;; trailing ? is optional at end of line
              (end (or close len))
              (substr (substring line (+ pos 1) end))
              (entry (history-search-reverse substr)))
         (if entry
           (values entry (if close (+ close 1) end))
           (error (format "!?~a?: event not found" substr)))))
      ;; !string = prefix search
      ((< pos len)
       (let* ((word-end (find-event-end line pos))
              (prefix (substring line pos word-end))
              (matches (history-search prefix)))
         (if (pair? matches)
           (values (last matches) word-end)
           (error (format "!~a: event not found" prefix)))))
      (else
       (error "bad ! event specification")))))

;; Find end of event string (stops at whitespace, :, or end)
(def (find-event-end str start)
  (let loop ((i start))
    (if (and (< i (string-length str))
             (not (char-whitespace? (string-ref str i)))
             (not (char=? (string-ref str i) #\:)))
      (loop (+ i 1))
      i)))

;; Parse word designator at pos (pos should be right after :)
;; Returns (values designator end-pos) or #f if no valid designator
(def (parse-word-designator line pos)
  (let ((len (string-length line)))
    (if (>= pos len) (values #f pos)
        (let ((ch (string-ref line pos)))
          (cond
            ;; ^ = first argument
            ((char=? ch #\^) (values 'caret (+ pos 1)))
            ;; $ = last argument
            ((char=? ch #\$) (values 'dollar (+ pos 1)))
            ;; * = all arguments (1-$)
            ((char=? ch #\*) (values 'star (+ pos 1)))
            ;; N, N-M, N-, N*
            ((char-numeric? ch)
             (let* ((num-end (find-number-end line pos))
                    (n (string->number (substring line pos num-end))))
               (cond
                 ;; N* = N through $
                 ((and (< num-end len) (char=? (string-ref line num-end) #\*))
                  (values (cons 'range (cons n 'dollar)) (+ num-end 1)))
                 ;; N-M or N-
                 ((and (< num-end len) (char=? (string-ref line num-end) #\-))
                  (let ((after-dash (+ num-end 1)))
                    (if (and (< after-dash len) (char-numeric? (string-ref line after-dash)))
                      ;; N-M
                      (let* ((m-end (find-number-end line after-dash))
                             (m (string->number (substring line after-dash m-end))))
                        (values (cons 'range (cons n m)) m-end))
                      ;; N- (through $-1)
                      (values (cons 'range (cons n 'minus-one)) after-dash))))
                 ;; Just N
                 (else (values n num-end)))))
            ;; Not a word designator
            (else (values #f pos)))))))

;; Parse modifier at pos (pos should be right after :)
;; Returns (values modifier end-pos) or (values #f pos)
(def (parse-modifier line pos)
  (let ((len (string-length line)))
    (if (>= pos len) (values #f pos)
        (let ((ch (string-ref line pos)))
          (cond
            ((char=? ch #\h) (values 'h (+ pos 1)))
            ((char=? ch #\t) (values 't (+ pos 1)))
            ((char=? ch #\r) (values 'r (+ pos 1)))
            ((char=? ch #\e) (values 'e (+ pos 1)))
            ((char=? ch #\p) (values 'p (+ pos 1)))
            ((char=? ch #\q) (values 'q (+ pos 1)))
            ((char=? ch #\x) (values 'x (+ pos 1)))
            ((char=? ch #\&) (values 'repeat-subst (+ pos 1)))
            ;; g or a prefix for :gs/old/new/ or :s/old/new/
            ((or (char=? ch #\g) (char=? ch #\a))
             (if (and (< (+ pos 1) len) (char=? (string-ref line (+ pos 1)) #\s))
               (parse-substitution line (+ pos 2) #t)
               ;; bare :g is invalid, skip
               (values #f (+ pos 1))))
            ((char=? ch #\s)
             (parse-substitution line (+ pos 1) #f))
            (else (values #f pos)))))))

;; Parse :s/old/new/ substitution
;; pos is right after s, delim is the next char
(def (parse-substitution line pos global?)
  (let ((len (string-length line)))
    (if (>= pos len)
      (values #f pos)
      (let ((delim (string-ref line pos)))
        ;; Find old
        (let ((old-end (string-index-of line delim (+ pos 1))))
          (if (not old-end)
            (values #f pos)
            (let ((old (substring line (+ pos 1) old-end)))
              ;; Find new (terminated by delim or end of line)
              (let ((new-end (string-index-of line delim (+ old-end 1))))
                (if new-end
                  (values (list 'subst old (substring line (+ old-end 1) new-end) global?)
                          (+ new-end 1))
                  ;; No trailing delimiter — new goes to end
                  (values (list 'subst old (substring line (+ old-end 1) len) global?)
                          len))))))))))

;; Parse a complete history reference starting at !
;; Returns (values expanded-text end-pos print-only?)
(def (parse-history-ref line pos)
  (let ((len (string-length line)))
    ;; Quick shortcuts: !$ !^ !*
    (cond
      ((and (< (+ pos 1) len)
            (char=? (string-ref line (+ pos 1)) #\$))
       ;; !$ = last word of previous command
       (let* ((prev (or (history-get-relative 0) ""))
              (words (history-split-words prev))
              (text (select-words words 'dollar)))
         (values text (+ pos 2) #f)))
      ((and (< (+ pos 1) len)
            (char=? (string-ref line (+ pos 1)) #\^))
       ;; !^ = first arg of previous command
       (let* ((prev (or (history-get-relative 0) ""))
              (words (history-split-words prev))
              (text (select-words words 'caret)))
         (values text (+ pos 2) #f)))
      ((and (< (+ pos 1) len)
            (char=? (string-ref line (+ pos 1)) #\*))
       ;; !* = all args of previous command
       (let* ((prev (or (history-get-relative 0) ""))
              (words (history-split-words prev))
              (text (select-words words 'star)))
         (values text (+ pos 2) #f)))
      (else
       ;; Full form: !event[:word][:modifier...]
       (let-values (((event end) (parse-event line (+ pos 1))))
         (let ((text event) (i end) (print-only? #f))
           ;; Check for : and word designator or modifier
           (let process ((text text) (i i) (print-only? print-only?))
             (if (and (< i len) (char=? (string-ref line i) #\:))
               ;; Try word designator first
               (let-values (((wd wd-end) (parse-word-designator line (+ i 1))))
                 (if wd
                   ;; Apply word designator
                   (let* ((words (history-split-words text))
                          (selected (select-words words wd)))
                     ;; Check for more colons (modifiers)
                     (process selected wd-end print-only?))
                   ;; Try modifier
                   (let-values (((mod mod-end) (parse-modifier line (+ i 1))))
                     (if mod
                       (let ((new-text (apply-history-modifier text mod))
                             (new-p (or print-only? (eq? mod 'p))))
                         (process new-text mod-end new-p))
                       ;; Not a modifier either, stop
                       (values text i print-only?)))))
               ;; No more colons
               (values text i print-only?)))))))))

;; Main expansion function
;; Returns (cons expanded-string execute?)
;; execute? is #t normally, #f when :p modifier was used
(def (history-expand line)
  (if (not *history*)
    (cons line #t)
    (let ((len (string-length line)))
      ;; Handle ^old^new^ quick substitution at start of line
      (if (and (> len 0) (char=? (string-ref line 0) #\^))
        (let ((result (expand-quick-subst line)))
          (cons result #t))
        (let loop ((i 0) (result (open-output-string)) (changed? #f) (print-only? #f))
          (cond
            ((>= i len)
             (if changed?
               (cons (get-output-string result) (not print-only?))
               (cons line #t)))
            ;; Escaped !
            ((and (< (+ i 1) len)
                  (char=? (string-ref line i) #\\)
                  (char=? (string-ref line (+ i 1)) #\!))
             (display "!" result)
             (loop (+ i 2) result #t print-only?))
            ;; Inside single quotes - no expansion
            ((char=? (string-ref line i) #\')
             (display "'" result)
             (let qloop ((j (+ i 1)))
               (cond
                 ((>= j len) (loop j result changed? print-only?))
                 ((char=? (string-ref line j) #\')
                  (display "'" result)
                  (loop (+ j 1) result changed? print-only?))
                 (else
                  (display (string-ref line j) result)
                  (qloop (+ j 1))))))
            ;; ! history expansion
            ((char=? (string-ref line i) #\!)
             (if (or (= (+ i 1) len)
                     (char-whitespace? (string-ref line (+ i 1)))
                     (char=? (string-ref line (+ i 1)) #\=)
                     (char=? (string-ref line (+ i 1)) #\())
               ;; Bare ! before whitespace/=/( — not expansion
               (begin (display "!" result)
                      (loop (+ i 1) result changed? print-only?))
               ;; Parse full history reference
               (let-values (((text end ponly?) (parse-history-ref line i)))
                 (display text result)
                 (loop end result #t (or print-only? ponly?)))))
            ;; Regular character
            (else
             (display (string-ref line i) result)
             (loop (+ i 1) result changed? print-only?))))))))

;; Quick substitution: ^old^new^[rest]
(def (expand-quick-subst line)
  (let* ((len (string-length line))
         (second (string-index-of line #\^ 1))
         (third (if second (string-index-of line #\^ (+ second 1)) #f))
         (old (and second (substring line 1 second)))
         (new (and second
                   (if third
                     (substring line (+ second 1) third)
                     (substring line (+ second 1) len))))
         (prev (history-get-relative 0)))
    (if (and old new prev)
      (let ((expanded (string-replace-first prev old new)))
        (if third
          (string-append expanded (substring line (+ third 1) len))
          expanded))
      line)))

;;; --- File I/O ---
;;; Format: EPOCH\tCWD\tCOMMAND  (tab-separated)
;;; Backward-compatible: plain text lines (no tabs) loaded as timestamp=0, cwd=""

(def (history-save!)
  (when *history*
    (let ((entries (history-entries))
          (file (history-state-file *history*)))
      (with-catch
       (lambda (e) #!void)  ;; silently fail
       (lambda ()
         (call-with-output-file file
           (lambda (port)
             (for-each
               (lambda (entry)
                 (fprintf port "~a\t~a\t~a~n"
                          (history-entry-timestamp entry)
                          (history-entry-cwd entry)
                          (history-entry-command entry)))
               entries))))))))

(def (history-load!)
  (when *history*
    (let ((file (history-state-file *history*)))
      (with-catch
       (lambda (e) #!void)
       (lambda ()
         (when (file-exists? file)
           (let ((lines (read-file-lines file)))
             (for-each
               (lambda (line)
                 (when (> (string-length line) 0)
                   ;; Parse tab-separated format or fall back to plain text
                   (let ((tab1 (string-index-of line #\tab 0)))
                     (if tab1
                       (let ((tab2 (string-index-of line #\tab (+ tab1 1))))
                         (if tab2
                           ;; Full format: EPOCH\tCWD\tCOMMAND
                           (let* ((ts-str (substring line 0 tab1))
                                  (cwd (substring line (+ tab1 1) tab2))
                                  (cmd (substring line (+ tab2 1) (string-length line)))
                                  (ts (or (string->number ts-str) 0)))
                             (history-add-raw! ts cwd cmd))
                           ;; One tab: treat as plain text
                           (history-add-raw! 0 "" line)))
                       ;; No tabs: plain text (old format)
                       (history-add-raw! 0 "" line)))))
               lines))))))))

;; Internal: add a pre-parsed entry without timestamping
(def (history-add-raw! timestamp cwd command)
  (when (and *history* (> (string-length command) 0))
    (unless (history-should-ignore? command)
      (let* ((h *history*)
             (idx (modulo (history-state-count h)
                          (history-state-max-size h)))
             (entry (make-history-entry timestamp cwd command)))
        (vector-set! (history-state-entries h) idx entry)
        (set! (history-state-count h) (+ 1 (history-state-count h)))))))

;;; --- HISTCONTROL ---

(def (history-set-control! controls)
  (when *history*
    (set! (history-state-control *history*) controls)))

(def (history-should-ignore? line)
  (let ((controls (if *history* (history-state-control *history*) [])))
    (or
     ;; ignorespace: lines starting with space
     (and (memq 'ignorespace controls)
          (> (string-length line) 0)
          (char=? (string-ref line 0) #\space))
     ;; ignoredups: same as previous command
     (and (memq 'ignoredups controls)
          (let ((prev (history-get-relative 0)))
            (and prev (string=? prev line)))))))

;;; --- Deduplication for history display ---

;; Return history command strings deduplicated (most recent wins), newest first.
;; Used for fzf history search to avoid showing the same command many times.
(def (history-unique-commands)
  (let ((entries (history-entries))
        (seen (make-hash-table)))
    (let loop ((es (reverse entries)) (result []))
      (if (null? es)
        result
        (let ((cmd (history-entry-command (car es))))
          (if (hash-get seen cmd)
            (loop (cdr es) result)
            (begin
              (hash-put! seen cmd #t)
              (loop (cdr es) (cons cmd result)))))))))

;;; --- Helpers ---

(def (find-number-end str start)
  (let loop ((i start))
    (if (and (< i (string-length str))
             (char-numeric? (string-ref str i)))
      (loop (+ i 1))
      i)))

(def (find-word-end str start)
  (let loop ((i start))
    (if (and (< i (string-length str))
             (not (char-whitespace? (string-ref str i))))
      (loop (+ i 1))
      i)))

(def (string-index-of str ch start)
  (let loop ((i start))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

(def (string-replace-first str old new)
  (let ((pos (string-search str old)))
    (if pos
      (string-append (substring str 0 pos) new
                     (substring str (+ pos (string-length old)) (string-length str)))
      str)))

(def (string-replace-all str old new)
  (let ((olen (string-length old)))
    (if (= olen 0) str
        (let loop ((start 0) (result (open-output-string)))
          (let ((pos (string-search-from str old start)))
            (if pos
              (begin
                (display (substring str start pos) result)
                (display new result)
                (loop (+ pos olen) result))
              (begin
                (display (substring str start (string-length str)) result)
                (get-output-string result))))))))

(def (string-search haystack needle)
  (string-search-from haystack needle 0))

(def (string-search-from haystack needle start)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i start))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))

(def (string-trim str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))

