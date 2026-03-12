;;; fzf.ss — Interactive fuzzy finder TUI for gsh
;;;
;;; Full-screen terminal UI with live filtering, match highlighting,
;;; and scrollable candidate list. Bottom-up layout like fzf.
;;;
;;; Usage:
;;;   (fzf-select candidates prompt: "> " multi?: #f)
;;;   Returns selected string, or #f on cancel.

(export fzf-select
        fzf-history-search
        fzf-find-files
        fzf-find-directories
        fzf-init!
        collect-files
        collect-files-git
        collect-directories)
(import :std/sugar
        :std/format
        :std/sort
        :std/srfi/1
        :gsh/util
        :gsh/fuzzy
        :gsh/lineedit  ;; for read-key, term-raw!, term-cooked!, *fzf-*-fn* callbacks
        :gsh/ffi)

;;;============================================================================
;;; State
;;;============================================================================

(defstruct fzf-state
  (candidates      ;; vector of all candidate strings
   filtered        ;; list of (score . string) — current filtered results
   query           ;; string: current search query
   cursor          ;; integer: cursor position in query
   selected        ;; integer: selected index in filtered list (0 = top visible)
   scroll-offset   ;; integer: how many items scrolled past
   total           ;; integer: total number of candidates
   match-count     ;; integer: number of matching candidates
   height          ;; integer: terminal rows available for candidates
   columns         ;; integer: terminal width
   prompt          ;; string: prompt string
   multi?          ;; boolean: multi-select mode
   marked          ;; list of strings: marked candidates (multi-select)
   done?           ;; boolean: selection complete
   result)         ;; string, list, or #f: final result
  transparent: #t)

;;;============================================================================
;;; ANSI escape helpers
;;;============================================================================

(def ESC-STR (string #\escape))

(def (ansi-clear-screen port)
  (fprintf port "~a[2J~a[H" ESC-STR ESC-STR))

(def (ansi-move-to row col port)
  (fprintf port "~a[~a;~aH" ESC-STR row col))

(def (ansi-clear-line port)
  (fprintf port "~a[2K" ESC-STR))

(def (ansi-bold port)
  (fprintf port "~a[1m" ESC-STR))

(def (ansi-underline port)
  (fprintf port "~a[4m" ESC-STR))

(def (ansi-fg-green port)
  (fprintf port "~a[32m" ESC-STR))

(def (ansi-fg-yellow port)
  (fprintf port "~a[33m" ESC-STR))

(def (ansi-fg-cyan port)
  (fprintf port "~a[36m" ESC-STR))

(def (ansi-reset port)
  (fprintf port "~a[0m" ESC-STR))

(def (ansi-hide-cursor port)
  (fprintf port "~a[?25l" ESC-STR))

(def (ansi-show-cursor port)
  (fprintf port "~a[?25h" ESC-STR))

(def (ansi-save-cursor port)
  (fprintf port "~a7" ESC-STR))

(def (ansi-restore-cursor port)
  (fprintf port "~a8" ESC-STR))

;; Enable alternate screen buffer (preserves main screen)
(def (ansi-alt-screen-on port)
  (fprintf port "~a[?1049h" ESC-STR))

(def (ansi-alt-screen-off port)
  (fprintf port "~a[?1049l" ESC-STR))

;;;============================================================================
;;; Terminal size
;;;============================================================================

(def (get-terminal-size)
  "Returns (values rows cols)."
  (let ((cols (ffi-terminal-columns 0)))
    (let ((rows (ffi-terminal-rows 0)))
      (values (if (or (not rows) (<= rows 0)) 24 rows)
              (if (or (not cols) (<= cols 0)) 80 cols)))))

;;;============================================================================
;;; Rendering
;;;============================================================================

(def (render-fzf state port)
  "Render the full fzf interface."
  (let* ((height (fzf-state-height state))
         (cols (fzf-state-columns state))
         (filtered (fzf-state-filtered state))
         (query (fzf-state-query state))
         (selected (fzf-state-selected state))
         (scroll (fzf-state-scroll-offset state))
         (match-count (fzf-state-match-count state))
         (total (fzf-state-total state))
         (prompt-str (fzf-state-prompt state))
         (marked (fzf-state-marked state))
         ;; Reserve: 1 line for prompt, 1 line for status
         (avail-lines (- height 2))
         ;; Candidate window
         (visible (fzf-take (fzf-drop filtered scroll) avail-lines)))

    ;; Clear screen and draw from top
    (ansi-move-to 1 1 port)

    ;; Status line at top
    (ansi-clear-line port)
    (ansi-fg-cyan port)
    (fprintf port "  ~a/~a" match-count total)
    (when (pair? marked)
      (fprintf port " [~a selected]" (length marked)))
    (ansi-reset port)
    (display "\r\n" port)

    ;; Candidate lines (top-down: best match at top)
    (let loop ((items visible) (idx 0))
      (when (< idx avail-lines)
        (ansi-clear-line port)
        (if (pair? items)
          (let* ((item (car items))
                 (score (car item))
                 (text (cdr item))
                 (is-selected? (= idx selected))
                 (is-marked? (member text marked)))
            ;; Indicator
            (if is-selected?
              (begin (ansi-fg-green port) (display "> " port) (ansi-reset port))
              (display "  " port))
            ;; Mark indicator
            (when is-marked?
              (ansi-fg-yellow port)
              (display "* " port)
              (ansi-reset port))
            ;; Candidate text with match highlighting
            (render-highlighted-candidate text query is-selected? cols port)
            (display "\r\n" port))
          ;; Empty line
          (display "\r\n" port))
        (loop (if (pair? items) (cdr items) []) (+ idx 1))))

    ;; Prompt line at bottom
    (ansi-clear-line port)
    (ansi-fg-green port)
    (display prompt-str port)
    (ansi-reset port)
    (display query port)
    (force-output port)))

(def (render-highlighted-candidate text query is-selected? cols port)
  "Render a candidate with fuzzy-matched characters highlighted."
  (let* ((positions (if (> (string-length query) 0)
                      (fuzzy-match-positions query text)
                      []))
         (pos-set (list->hash-set positions))
         (max-width (- cols 4))  ;; account for prefix "  " or "> "
         (display-len (min (string-length text) max-width)))
    (when is-selected?
      (ansi-bold port))
    (let loop ((i 0))
      (when (< i display-len)
        (let ((ch (string-ref text i)))
          (if (hash-get pos-set i)
            (begin
              (ansi-fg-green port)
              (ansi-underline port)
              (display ch port)
              (ansi-reset port)
              (when is-selected? (ansi-bold port)))
            (display ch port)))
        (loop (+ i 1))))
    (ansi-reset port)))

(def (list->hash-set lst)
  "Convert a list to a hash set for O(1) membership testing."
  (let ((ht (make-hash-table)))
    (for-each (lambda (x) (hash-put! ht x #t)) lst)
    ht))

;;;============================================================================
;;; Filtering
;;;============================================================================

(def (update-filter! state)
  "Re-filter candidates based on current query."
  (let* ((query (fzf-state-query state))
         (candidates (fzf-state-candidates state))
         (len (vector-length candidates))
         (filtered (if (string=? query "")
                     ;; No query: show all candidates in order
                     (let loop ((i 0) (acc []))
                       (if (>= i len) (reverse acc)
                           (loop (+ i 1) (cons (cons 1 (vector-ref candidates i)) acc))))
                     ;; Score and filter
                     (let loop ((i 0) (acc []))
                       (if (>= i len)
                         (begin (sort! acc (lambda (a b) (> (car a) (car b)))) acc)
                         (let* ((cand (vector-ref candidates i))
                                (score (fzf-score query cand)))
                           (loop (+ i 1)
                                 (if (> score 0)
                                   (cons (cons score cand) acc)
                                   acc))))))))
    (set! (fzf-state-filtered state) filtered)
    (set! (fzf-state-match-count state) (length filtered))
    ;; Always reset selection to top when query changes
    (set! (fzf-state-selected state) 0)
    (set! (fzf-state-scroll-offset state) 0)))

;;;============================================================================
;;; Input handling
;;;============================================================================

(def (handle-fzf-key state key)
  "Handle a keypress in fzf mode. Returns #t if state changed."
  (let ((filtered (fzf-state-filtered state))
        (selected (fzf-state-selected state))
        (avail (- (fzf-state-height state) 2)))
    (cond
      ;; Enter / Ctrl-J: accept selection
      ((or (and (char? key) (or (= (char->integer key) 13)
                                (= (char->integer key) 10)))
           (and (char? key) (= (char->integer key) 10)))  ;; Ctrl-J
       (if (pair? filtered)
         (let ((item (fzf-list-ref filtered selected)))
           (if item
             (begin
               (set! (fzf-state-result state)
                     (if (fzf-state-multi? state)
                       (let ((marked (fzf-state-marked state)))
                         (if (null? marked)
                           (list (cdr item))
                           marked))
                       (cdr item)))
               (set! (fzf-state-done? state) #t))
             (set! (fzf-state-done? state) #t)))
         (begin
           (set! (fzf-state-result state) #f)
           (set! (fzf-state-done? state) #t)))
       #t)

      ;; Ctrl-C / Escape: cancel
      ((or (and (char? key) (= (char->integer key) 3))  ;; Ctrl-C
           (eq? key 'escape))
       (set! (fzf-state-result state) #f)
       (set! (fzf-state-done? state) #t)
       #t)

      ;; Up / Ctrl-P: move selection up (toward top of list)
      ((or (eq? key 'up)
           (and (char? key) (= (char->integer key) 16)))  ;; Ctrl-P
       (when (> selected 0)
         (set! (fzf-state-selected state) (- selected 1))
         ;; Scroll if needed
         (when (< (fzf-state-selected state) (fzf-state-scroll-offset state))
           (set! (fzf-state-scroll-offset state) (fzf-state-selected state))))
       #t)

      ;; Down / Ctrl-N: move selection down
      ((or (eq? key 'down)
           (and (char? key) (= (char->integer key) 14)))  ;; Ctrl-N
       (when (< selected (- (length filtered) 1))
         (set! (fzf-state-selected state) (+ selected 1))
         ;; Scroll if needed
         (when (>= (- (fzf-state-selected state) (fzf-state-scroll-offset state)) avail)
           (set! (fzf-state-scroll-offset state)
                 (- (fzf-state-selected state) avail -1))))
       #t)

      ;; Tab: toggle mark (multi-select mode)
      ((and (char? key) (= (char->integer key) 9) (fzf-state-multi? state))
       (when (pair? filtered)
         (let* ((item (fzf-list-ref filtered selected))
                (text (and item (cdr item)))
                (marked (fzf-state-marked state)))
           (when text
             (if (member text marked)
               (set! (fzf-state-marked state)
                     (filter (lambda (m) (not (string=? m text))) marked))
               (set! (fzf-state-marked state)
                     (cons text marked))))))
       ;; Move down after marking
       (when (< selected (- (length filtered) 1))
         (set! (fzf-state-selected state) (+ selected 1)))
       #t)

      ;; Ctrl-U: clear query
      ((and (char? key) (= (char->integer key) 21))
       (set! (fzf-state-query state) "")
       (set! (fzf-state-cursor state) 0)
       (update-filter! state)
       #t)

      ;; Ctrl-W: delete word backward
      ((and (char? key) (= (char->integer key) 23))
       (let* ((query (fzf-state-query state))
              (pos (fzf-state-cursor state)))
         (when (> pos 0)
           ;; Skip trailing spaces
           (let skip-sp ((i (- pos 1)))
             (let ((start (if (and (>= i 0) (char=? (string-ref query i) #\space))
                            (skip-sp (- i 1))
                            (+ i 1))))
               ;; Skip word chars
               (let skip-word ((i (- start 1)))
                 (let ((word-start (if (and (>= i 0)
                                            (not (char=? (string-ref query i) #\space)))
                                     (skip-word (- i 1))
                                     (+ i 1))))
                   (set! (fzf-state-query state)
                         (string-append (substring query 0 word-start)
                                        (substring query pos (string-length query))))
                   (set! (fzf-state-cursor state) word-start)))))))
       (update-filter! state)
       #t)

      ;; Backspace: delete char before cursor
      ((and (char? key) (or (= (char->integer key) 127) (= (char->integer key) 8)))
       (let ((query (fzf-state-query state))
             (pos (fzf-state-cursor state)))
         (when (> pos 0)
           (set! (fzf-state-query state)
                 (string-append (substring query 0 (- pos 1))
                                (substring query pos (string-length query))))
           (set! (fzf-state-cursor state) (- pos 1))
           (update-filter! state)))
       #t)

      ;; Ctrl-A: beginning of query
      ((and (char? key) (= (char->integer key) 1))
       (set! (fzf-state-cursor state) 0)
       #t)

      ;; Ctrl-E: end of query
      ((and (char? key) (= (char->integer key) 5))
       (set! (fzf-state-cursor state) (string-length (fzf-state-query state)))
       #t)

      ;; Page-Up
      ((eq? key 'page-up)
       (set! (fzf-state-selected state) (max 0 (- selected avail)))
       (set! (fzf-state-scroll-offset state)
             (max 0 (- (fzf-state-scroll-offset state) avail)))
       #t)

      ;; Page-Down
      ((eq? key 'page-down)
       (let ((max-sel (- (length filtered) 1)))
         (set! (fzf-state-selected state) (min max-sel (+ selected avail)))
         (set! (fzf-state-scroll-offset state)
               (max 0 (- (fzf-state-selected state) avail -1))))
       #t)

      ;; Printable character: insert into query
      ((and (char? key) (>= (char->integer key) 32) (not (= (char->integer key) 127)))
       (let ((query (fzf-state-query state))
             (pos (fzf-state-cursor state)))
         (set! (fzf-state-query state)
               (string-append (substring query 0 pos)
                              (string key)
                              (substring query pos (string-length query))))
         (set! (fzf-state-cursor state) (+ pos 1))
         (update-filter! state))
       #t)

      (else #f))))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (fzf-take lst n)
  (if (or (null? lst) (<= n 0)) []
      (cons (car lst) (fzf-take (cdr lst) (- n 1)))))

(def (fzf-drop lst n)
  (if (or (null? lst) (<= n 0)) lst
      (fzf-drop (cdr lst) (- n 1))))

(def (fzf-list-ref lst idx)
  (if (or (null? lst) (< idx 0)) #f
      (if (= idx 0) (car lst)
          (fzf-list-ref (cdr lst) (- idx 1)))))

;;;============================================================================
;;; Public interface
;;;============================================================================

(def (fzf-select candidates
                  prompt: (prompt "> ")
                  multi?: (multi? #f)
                  initial-query: (initial-query ""))
  "Run interactive fuzzy finder on a list of strings.
   Returns: selected string (single mode), list of strings (multi mode), or #f on cancel."
  (let ((in-port (current-input-port))
        (out-port (current-error-port)))
    (let-values (((rows cols) (get-terminal-size)))
      (let* ((cand-vec (list->vector candidates))
             (state (make-fzf-state
                      cand-vec       ;; candidates
                      []             ;; filtered
                      initial-query  ;; query
                      (string-length initial-query)  ;; cursor
                      0              ;; selected
                      0              ;; scroll-offset
                      (vector-length cand-vec)  ;; total
                      0              ;; match-count
                      rows           ;; height
                      cols           ;; columns
                      prompt         ;; prompt
                      multi?         ;; multi?
                      []             ;; marked
                      #f             ;; done?
                      #f)))          ;; result
        ;; Initial filter
        (update-filter! state)
        ;; Run TUI
        (dynamic-wind
          (lambda ()
            (term-raw! in-port)
            (ansi-alt-screen-on out-port)
            (ansi-hide-cursor out-port)
            (force-output out-port))
          (lambda ()
            (render-fzf state out-port)
            (let loop ()
              (let ((key (read-key in-port)))
                (cond
                  ((eq? key 'eof)
                   (set! (fzf-state-result state) #f)
                   (set! (fzf-state-done? state) #t))
                  (else
                   (handle-fzf-key state key)))
                (unless (fzf-state-done? state)
                  (render-fzf state out-port)
                  (loop))))
            (fzf-state-result state))
          (lambda ()
            (ansi-show-cursor out-port)
            (ansi-alt-screen-off out-port)
            (force-output out-port)
            (term-cooked! in-port)))))))

;;;============================================================================
;;; Convenience: history search via fzf
;;;============================================================================

(def (fzf-history-search history-commands)
  "Run fzf on history commands (newest first). Returns selected command or #f."
  (fzf-select history-commands prompt: "(history)> "))

;;;============================================================================
;;; Convenience: file finder via fzf
;;;============================================================================

(def (fzf-find-files (dir ".") (max-files 50000))
  "Recursively list files under dir for fzf selection.
   Uses git ls-files in git repos, falls back to filesystem walk.
   Returns selected file path or #f."
  (let ((files (collect-files-git dir max-files)))
    (fzf-select files prompt: "(files)> ")))

(def (collect-files-git dir max-files)
  "Collect files using git ls-files if in a git repo, else fall back to collect-files."
  (with-catch
   (lambda (e) (collect-files dir max-files))
   (lambda ()
     (let* ((proc (open-process (list path: "git"
                                      arguments: (list "ls-files" "--cached" "--others"
                                                       "--exclude-standard")
                                      directory: dir
                                      stdin-redirection: #f
                                      stderr-redirection: #f)))
            (files (let loop ((acc []))
                     (let ((line (read-line proc)))
                       (if (or (eof-object? line) (>= (length acc) max-files))
                         (reverse acc)
                         (loop (cons line acc))))))
            (status (process-status proc)))
       (if (= status 0)
         files
         (collect-files dir max-files))))))

(def *skip-dirs* '(".git" "node_modules" "__pycache__" ".gerbil" ".cache"
                   "venv" ".venv" "target" "dist" ".hg" ".svn"))

(def (collect-files dir max-files)
  "Recursively collect file paths under dir, up to max-files."
  (let ((result [])
        (count 0))
    (let walk ((d dir) (prefix ""))
      (when (< count max-files)
        (with-catch
         (lambda (e) #!void)
         (lambda ()
           (let ((entries (directory-files d)))
             (for-each
               (lambda (name)
                 (when (and (< count max-files)
                            (not (string=? name "."))
                            (not (string=? name ".."))
                            ;; Skip hidden files/dirs at all levels
                            (not (and (> (string-length name) 0)
                                      (char=? (string-ref name 0) #\.)))
                            ;; Skip known large directories
                            (not (member name *skip-dirs*)))
                   (let* ((rel (if (string=? prefix "")
                                 name
                                 (string-append prefix "/" name)))
                          (full (string-append d "/" name)))
                     (if (file-directory? full)
                       (walk full rel)
                       (begin
                         (set! result (cons rel result))
                         (set! count (+ count 1)))))))
               entries))))))
    (reverse result)))

;;;============================================================================
;;; Convenience: directory jump via fzf
;;;============================================================================

(def (fzf-find-directories (dir ".") (max-dirs 10000))
  "Recursively list directories under dir for fzf selection.
   Returns selected directory path or #f."
  (let ((dirs (collect-directories dir max-dirs)))
    (fzf-select dirs prompt: "(dirs)> ")))

(def (collect-directories dir max-dirs)
  "Recursively collect directory paths under dir."
  (let ((result [])
        (count 0))
    (let walk ((d dir) (prefix ""))
      (when (< count max-dirs)
        (with-catch
         (lambda (e) #!void)
         (lambda ()
           (let ((entries (directory-files d)))
             (for-each
               (lambda (name)
                 (when (and (< count max-dirs)
                            (not (string=? name "."))
                            (not (string=? name ".."))
                            ;; Skip hidden dirs at all levels
                            (not (and (> (string-length name) 0)
                                      (char=? (string-ref name 0) #\.)))
                            ;; Skip known large directories
                            (not (member name *skip-dirs*)))
                   (let* ((rel (if (string=? prefix "")
                                 name
                                 (string-append prefix "/" name)))
                          (full (string-append d "/" name)))
                     (when (file-directory? full)
                       (set! result (cons rel result))
                       (set! count (+ count 1))
                       (walk full rel)))))
               entries))))))
    (reverse result)))

;;;============================================================================
;;; Register fzf callbacks in lineedit
;;;============================================================================

(def (fzf-init!)
  "Register fzf handlers as lineedit callbacks."
  (*fzf-history-fn* fzf-history-search)
  (*fzf-files-fn* fzf-find-files)
  (*fzf-dirs-fn* fzf-find-directories))
