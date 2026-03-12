;;; lineedit.ss — Terminal line editor for gsh
;;; Custom line editor with emacs/vi modes, history, search, kill ring.

(export #t)
(import :std/sugar
        :std/format
        :gsh/util
        :gsh/history
        :gsh/ffi)

;;; --- fzf callback hooks (set by fzf.ss at startup) ---
;;; These are set to actual functions after fzf.ss loads.
;;; Initialized to make-parameter so the compiler sees them as procedures.
(def *fzf-history-fn* (make-parameter #f))   ;; parameter: (lambda (commands) -> string or #f) or #f
(def *fzf-files-fn* (make-parameter #f))     ;; parameter: (lambda () -> string or #f) or #f
(def *fzf-dirs-fn* (make-parameter #f))      ;; parameter: (lambda () -> string or #f) or #f

;;; --- Constants ---

(def ESC #\escape)
(def ESC-BYTE 27)
(def CTRL-A 1)
(def CTRL-B 2)
(def CTRL-C 3)
(def CTRL-D 4)
(def CTRL-E 5)
(def CTRL-F 6)
(def CTRL-G 7)
(def CTRL-H 8)
(def CTRL-K 11)
(def CTRL-L 12)
(def CTRL-N 14)
(def CTRL-P 16)
(def CTRL-R 18)
(def CTRL-S 19)
(def CTRL-T 20)
(def CTRL-U 21)
(def CTRL-W 23)
(def CTRL-Y 25)
(def CTRL-Z 26)
(def CTRL-UNDERSCORE 31)
(def TAB-KEY 9)
(def ENTER-KEY 13)
(def NEWLINE-KEY 10)

(def (enter-key? key)
  (and (char? key)
       (let ((code (char->integer key)))
         (or (= code ENTER-KEY) (= code NEWLINE-KEY)))))
(def BACKSPACE 127)

;;; --- Editor state ---

(defstruct line-state
  (buffer         ;; string: current line content
   cursor         ;; integer: cursor position in buffer
   prompt         ;; string: display prompt
   prompt-width   ;; integer: visible width of prompt
   history-index  ;; integer or #f: current history position
   saved-line     ;; string: saved current line before history nav
   kill-ring      ;; list of strings
   undo-stack     ;; list of (buffer . cursor) pairs
   search-mode    ;; #f, 'reverse, or 'forward
   search-string  ;; string: current search query
   search-result  ;; string or #f: matched history entry
   columns        ;; integer: terminal width
   multiline?     ;; boolean: whether to support multiline
   mode           ;; 'emacs or 'vi
   vi-mode        ;; 'insert or 'command (only for vi mode)
   complete-fn    ;; completion callback or #f
   done?          ;; boolean: editing complete
   result         ;; string or 'eof: final result
   )
  transparent: #t)

;;; --- Terminal I/O ---

(def (term-raw! port)
  ;; Set terminal to raw mode: char-at-a-time, no echo, no signal processing
  ;; Args: port input-allow-special input-echo input-raw output-raw speed
  ;; Must use Gambit's tty-mode-set! exclusively — calling tcsetattr via FFI
  ;; desynchronizes Gambit's cached terminal state and breaks read-char.
  (tty-mode-set! port #f #f #t #t 0))

(def (term-cooked! port)
  ;; Restore terminal to cooked mode: canonical, echo
  (tty-mode-set! port #t #t #f #f 0))

;; Write ANSI escape sequences
(def ESC-STR (string #\escape))

(def (term-move-cursor-col col port)
  (fprintf port "\r~a[~aC" ESC-STR (+ col 1)))

(def (term-clear-to-eol port)
  (display (string-append ESC-STR "[K") port))

(def (term-clear-line port)
  (display (string-append "\r" ESC-STR "[K") port))

(def (term-clear-screen port)
  (display (string-append ESC-STR "[H" ESC-STR "[2J") port))

(def (term-move-up n port)
  (when (> n 0)
    (fprintf port "~a[~aA" ESC-STR n)))

(def (term-move-down n port)
  (when (> n 0)
    (fprintf port "~a[~aB" ESC-STR n)))

(def (term-cursor-forward n port)
  (when (> n 0)
    (fprintf port "~a[~aC" ESC-STR n)))

(def (term-cursor-back n port)
  (when (> n 0)
    (fprintf port "~a[~aD" ESC-STR n)))

;;; --- Terminal size ---

(def *terminal-columns* 80)

(def (get-terminal-columns)
  *terminal-columns*)

(def (set-terminal-columns! cols)
  (set! *terminal-columns* cols))

(def (detect-terminal-columns)
  (let ((cols (ffi-terminal-columns 0)))
    ;; Fallback to 80 if detection fails
    (if (or (not cols) (= cols 0))
      80
      cols)))

;;; --- Read input bytes ---

(def (read-key port)
  ;; Read a key from the terminal. Returns:
  ;; - A character for regular keys
  ;; - A symbol for special keys: 'up, 'down, 'left, 'right, 'home, 'end,
  ;;   'delete, 'page-up, 'page-down
  ;; - A list '(alt char) for Alt-X keys
  ;; - 'eof for end of input
  (let ((ch (read-char port)))
    (cond
      ((eof-object? ch) 'eof)
      ;; Escape sequence
      ((char=? ch ESC)
       (if (char-ready? port)
         (let ((ch2 (read-char port)))
           (cond
             ((eof-object? ch2) 'escape)
             ;; CSI sequence: ESC [
             ((char=? ch2 #\[)
              (let ((ch3 (read-char port)))
                (cond
                  ((eof-object? ch3) 'escape)
                  ((char=? ch3 #\A) 'up)
                  ((char=? ch3 #\B) 'down)
                  ((char=? ch3 #\C) 'right)
                  ((char=? ch3 #\D) 'left)
                  ((char=? ch3 #\H) 'home)
                  ((char=? ch3 #\F) 'end)
                  ;; Extended sequences: ESC [ N ~
                  ((and (char>=? ch3 #\0) (char<=? ch3 #\9))
                   (let ((num (- (char->integer ch3) (char->integer #\0))))
                     (let ((ch4 (read-char port)))
                       (cond
                         ((eof-object? ch4) 'escape)
                         ((char=? ch4 #\~)
                          (case num
                            ((1) 'home)
                            ((3) 'delete)
                            ((4) 'end)
                            ((5) 'page-up)
                            ((6) 'page-down)
                            ((7) 'home)
                            ((8) 'end)
                            (else 'unknown)))
                         ;; Two-digit sequences
                         ((and (char>=? ch4 #\0) (char<=? ch4 #\9))
                          ;; Read until ~
                          (let loop ()
                            (let ((c (read-char port)))
                              (if (or (eof-object? c) (char=? c #\~))
                                'unknown
                                (loop))))
                          'unknown)
                         (else 'unknown)))))
                  (else 'unknown))))
             ;; SS3 sequence: ESC O
             ((char=? ch2 #\O)
              (let ((ch3 (read-char port)))
                (cond
                  ((eof-object? ch3) 'escape)
                  ((char=? ch3 #\A) 'up)
                  ((char=? ch3 #\B) 'down)
                  ((char=? ch3 #\C) 'right)
                  ((char=? ch3 #\D) 'left)
                  ((char=? ch3 #\H) 'home)
                  ((char=? ch3 #\F) 'end)
                  (else 'unknown))))
             ;; Alt-key: ESC followed by printable char
             (else (list 'alt ch2))))
         ;; Bare escape (no following chars ready)
         'escape))
      ;; Regular character (possibly ctrl)
      (else ch))))

;;; --- Rendering ---

(def (refresh-line state port)
  (let* ((prompt (line-state-prompt state))
         (buf (line-state-buffer state))
         (cursor (line-state-cursor state))
         (pwidth (line-state-prompt-width state))
         (cols (line-state-columns state))
         ;; Count newlines in prompt to handle multi-line prompts
         (prompt-lines (count-newlines prompt))
         ;; Calculate the width of the LAST line of the prompt
         ;; This is what we need for cursor positioning on multi-line prompts
         (last-line-width (visible-width-last-line prompt)))
    ;; Move to start of prompt (handle multi-line prompts)
    ;; Move up by number of newlines in prompt, then carriage return
    (when (> prompt-lines 0)
      (term-move-up prompt-lines port))
    (display "\r" port)
    ;; Display prompt with explicit \r after each \n
    ;; This ensures cursor returns to column 0 on each new line
    (let ((len (string-length prompt)))
      (let loop ((i 0))
        (when (< i len)
          (let ((ch (string-ref prompt i)))
            (display (string ch) port)
            (when (char=? ch #\newline)
              (display "\r" port)))
          (loop (+ i 1)))))
    (display buf port)
    (term-clear-to-eol port)
    ;; Position cursor - use last-line-width instead of pwidth for multi-line prompts
    (let ((cursor-col (+ last-line-width cursor)))
      ;; Move cursor back from end of line to cursor position
      (let ((end-col (+ last-line-width (string-length buf))))
        (when (> end-col cursor-col)
          (term-cursor-back (- end-col cursor-col) port))))
    (force-output port)))

(def (refresh-search state port)
  (let* ((search-str (line-state-search-string state))
         (result (or (line-state-search-result state) ""))
         (mode-str (if (eq? (line-state-search-mode state) 'reverse)
                     "(reverse-i-search)" "(i-search)")))
    (display "\r" port)
    (term-clear-to-eol port)
    (fprintf port "~a`~a': ~a" mode-str search-str result)
    (force-output port)))

;;; --- Line buffer operations ---

(def (buf-insert state ch)
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (new-buf (string-append
                   (substring buf 0 pos)
                   (string ch)
                   (substring buf pos (string-length buf)))))
    (save-undo! state)
    (set! (line-state-buffer state) new-buf)
    (set! (line-state-cursor state) (+ pos 1))))

(def (buf-insert-string state str)
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (new-buf (string-append
                   (substring buf 0 pos)
                   str
                   (substring buf pos (string-length buf)))))
    (save-undo! state)
    (set! (line-state-buffer state) new-buf)
    (set! (line-state-cursor state) (+ pos (string-length str)))))

(def (buf-delete-char state)
  ;; Delete character at cursor
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (len (string-length buf)))
    (when (< pos len)
      (save-undo! state)
      (set! (line-state-buffer state)
            (string-append (substring buf 0 pos)
                           (substring buf (+ pos 1) len))))))

(def (buf-backspace state)
  ;; Delete character before cursor
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state)))
    (when (> pos 0)
      (save-undo! state)
      (set! (line-state-buffer state)
            (string-append (substring buf 0 (- pos 1))
                           (substring buf pos (string-length buf))))
      (set! (line-state-cursor state) (- pos 1)))))

(def (buf-kill-to-end state)
  ;; Kill from cursor to end of line
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (killed (substring buf pos (string-length buf))))
    (when (> (string-length killed) 0)
      (save-undo! state)
      (kill-ring-push! state killed)
      (set! (line-state-buffer state) (substring buf 0 pos)))))

(def (buf-kill-to-start state)
  ;; Kill from start to cursor
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (killed (substring buf 0 pos)))
    (when (> (string-length killed) 0)
      (save-undo! state)
      (kill-ring-push! state killed)
      (set! (line-state-buffer state) (substring buf pos (string-length buf)))
      (set! (line-state-cursor state) 0))))

(def (buf-kill-word-back state)
  ;; Kill word backward (Ctrl-W)
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         ;; Skip trailing whitespace
         (start (let loop ((i (- pos 1)))
                  (if (and (>= i 0) (char-whitespace? (string-ref buf i)))
                    (loop (- i 1))
                    (+ i 1))))
         ;; Find word start
         (word-start (let loop ((i (- start 1)))
                       (if (and (>= i 0) (not (char-whitespace? (string-ref buf i))))
                         (loop (- i 1))
                         (+ i 1))))
         (killed (substring buf word-start pos)))
    (when (> (string-length killed) 0)
      (save-undo! state)
      (kill-ring-push! state killed)
      (set! (line-state-buffer state)
            (string-append (substring buf 0 word-start)
                           (substring buf pos (string-length buf))))
      (set! (line-state-cursor state) word-start))))

(def (buf-kill-word-forward state)
  ;; Kill word forward (Alt-D)
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (len (string-length buf))
         ;; Skip whitespace
         (start (let loop ((i pos))
                  (if (and (< i len) (char-whitespace? (string-ref buf i)))
                    (loop (+ i 1))
                    i)))
         ;; Find word end
         (word-end (let loop ((i start))
                     (if (and (< i len) (not (char-whitespace? (string-ref buf i))))
                       (loop (+ i 1))
                       i)))
         (killed (substring buf pos word-end)))
    (when (> (string-length killed) 0)
      (save-undo! state)
      (kill-ring-push! state killed)
      (set! (line-state-buffer state)
            (string-append (substring buf 0 pos)
                           (substring buf word-end len))))))

(def (buf-transpose-chars state)
  ;; Transpose the two characters before cursor
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (len (string-length buf)))
    (when (and (> pos 0) (> len 1))
      (save-undo! state)
      (let* ((p (if (= pos len) (- pos 1) pos))
             (c1 (string-ref buf (- p 1)))
             (c2 (string-ref buf p))
             (new-buf (string-copy buf)))
        (string-set! new-buf (- p 1) c2)
        (string-set! new-buf p c1)
        (set! (line-state-buffer state) new-buf)
        (set! (line-state-cursor state)
              (min (+ p 1) len))))))

;;; --- Word movement ---

(def (word-forward-pos buf pos)
  ;; Find position after next word
  (let ((len (string-length buf)))
    ;; Skip non-word chars
    (let loop ((i pos))
      (if (and (< i len) (not (char-alphanumeric? (string-ref buf i))))
        (loop (+ i 1))
        ;; Skip word chars
        (let loop2 ((i i))
          (if (and (< i len) (char-alphanumeric? (string-ref buf i)))
            (loop2 (+ i 1))
            i))))))

(def (word-backward-pos buf pos)
  ;; Find position before previous word
  (if (<= pos 0) 0
      ;; Skip non-word chars
      (let loop ((i (- pos 1)))
        (if (and (> i 0) (not (char-alphanumeric? (string-ref buf i))))
          (loop (- i 1))
          ;; Skip word chars
          (let loop2 ((i i))
            (if (and (> i 0) (char-alphanumeric? (string-ref buf (- i 1))))
              (loop2 (- i 1))
              i))))))

(def (char-alphanumeric? ch)
  (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))

;;; --- Kill ring ---

(def (kill-ring-push! state text)
  (set! (line-state-kill-ring state)
        (cons text (take-at-most (line-state-kill-ring state) 31))))

(def (kill-ring-top state)
  (let ((kr (line-state-kill-ring state)))
    (if (null? kr) #f (car kr))))

(def (take-at-most lst n)
  (if (or (null? lst) (<= n 0)) []
      (cons (car lst) (take-at-most (cdr lst) (- n 1)))))

;;; --- Undo ---

(def (save-undo! state)
  (set! (line-state-undo-stack state)
        (cons (cons (string-copy (line-state-buffer state))
                    (line-state-cursor state))
              (take-at-most (line-state-undo-stack state) 99))))

(def (undo! state)
  (let ((stack (line-state-undo-stack state)))
    (when (pair? stack)
      (let ((entry (car stack)))
        (set! (line-state-buffer state) (car entry))
        (set! (line-state-cursor state) (cdr entry))
        (set! (line-state-undo-stack state) (cdr stack))))))

;;; --- History navigation ---

(def (history-prev state)
  (let ((idx (or (line-state-history-index state) 0)))
    (let ((entry (history-get-relative idx)))
      (when entry
        ;; Save current line if first navigation
        (unless (line-state-history-index state)
          (set! (line-state-saved-line state)
                (string-copy (line-state-buffer state))))
        (set! (line-state-history-index state) (+ idx 1))
        (set! (line-state-buffer state) (string-copy entry))
        (set! (line-state-cursor state) (string-length entry))))))

(def (history-next state)
  (let ((idx (line-state-history-index state)))
    (when (and idx (> idx 0))
      (let ((new-idx (- idx 1)))
        (if (= new-idx 0)
          ;; Back to current line
          (begin
            (set! (line-state-history-index state) #f)
            (set! (line-state-buffer state)
                  (string-copy (or (line-state-saved-line state) "")))
            (set! (line-state-cursor state)
                  (string-length (line-state-buffer state))))
          ;; Navigate to history entry
          (let ((entry (history-get-relative (- new-idx 1))))
            (when entry
              (set! (line-state-history-index state) new-idx)
              (set! (line-state-buffer state) (string-copy entry))
              (set! (line-state-cursor state) (string-length entry)))))))))

;;; --- Incremental search ---

(def (search-start state mode)
  (set! (line-state-search-mode state) mode)
  (set! (line-state-search-string state) "")
  (set! (line-state-search-result state) #f))

(def (search-update state)
  ;; Update search results based on current search string
  (let ((query (line-state-search-string state)))
    (when (> (string-length query) 0)
      (let ((result (history-search-reverse query)))
        (set! (line-state-search-result state) result)))))

(def (search-accept state)
  ;; Accept search result and exit search mode
  (let ((result (line-state-search-result state)))
    (set! (line-state-search-mode state) #f)
    (when result
      (set! (line-state-buffer state) (string-copy result))
      (set! (line-state-cursor state) (string-length result)))))

(def (search-cancel state)
  ;; Cancel search and restore previous line
  (set! (line-state-search-mode state) #f)
  (set! (line-state-search-string state) "")
  (set! (line-state-search-result state) #f))

;;; --- Emacs mode key handling ---

(def (handle-emacs-key state key out-port)
  (cond
    ;; Search mode handling
    ((line-state-search-mode state)
     (handle-search-key state key out-port))
    ;; Character input (exclude DEL/backspace = 127)
    ((and (char? key) (>= (char->integer key) 32) (not (= (char->integer key) 127)))
     (buf-insert state key)
     (refresh-line state out-port))
    ;; Ctrl keys
    ((enter-key? key)
     (display "\r\n" out-port)
     (force-output out-port)
     (set! (line-state-done? state) #t)
     (set! (line-state-result state) (line-state-buffer state)))
    ((and (char? key) (= (char->integer key) CTRL-A))
     (set! (line-state-cursor state) 0)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-E))
     (set! (line-state-cursor state) (string-length (line-state-buffer state)))
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-B))
     (when (> (line-state-cursor state) 0)
       (set! (line-state-cursor state) (- (line-state-cursor state) 1))
       (refresh-line state out-port)))
    ((and (char? key) (= (char->integer key) CTRL-F))
     (when (< (line-state-cursor state) (string-length (line-state-buffer state)))
       (set! (line-state-cursor state) (+ (line-state-cursor state) 1))
       (refresh-line state out-port)))
    ((and (char? key) (= (char->integer key) CTRL-D))
     (if (= (string-length (line-state-buffer state)) 0)
       ;; EOF on empty line
       (begin
         (display "\r\n" out-port)
         (force-output out-port)
         (set! (line-state-done? state) #t)
         (set! (line-state-result state) 'eof))
       ;; Delete char at cursor
       (begin
         (buf-delete-char state)
         (refresh-line state out-port))))
    ((and (char? key) (= (char->integer key) BACKSPACE))
     (buf-backspace state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-H))
     (buf-backspace state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-K))
     (buf-kill-to-end state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-U))
     (buf-kill-to-start state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-W))
     (buf-kill-word-back state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-Y))
     ;; Yank from kill ring
     (let ((text (kill-ring-top state)))
       (when text
         (buf-insert-string state text)
         (refresh-line state out-port))))
    ((and (char? key) (= (char->integer key) CTRL-T))
     ;; Fuzzy file finder via fzf (if available), else transpose chars
     (let ((fn (*fzf-files-fn*)))
       (if fn
         (begin
           (term-cooked! (current-input-port))
           (let ((result (with-catch (lambda (e) #f) (lambda () (fn)))))
             (term-raw! (current-input-port))
             (when result
               (buf-insert-string state result))))
         (buf-transpose-chars state)))
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-L))
     ;; Clear screen
     (term-clear-screen out-port)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-P))
     (history-prev state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-N))
     (history-next state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-R))
     ;; Fuzzy history search via fzf (if available), else fallback to substring search
     (let ((fn (*fzf-history-fn*)))
       (if fn
         (let ((commands (history-unique-commands)))
           (when (pair? commands)
             (term-cooked! (current-input-port))
             (let ((result (with-catch (lambda (e) #f) (lambda () (fn commands)))))
               (term-raw! (current-input-port))
               (when result
                 (save-undo! state)
                 (set! (line-state-buffer state) (string-copy result))
                 (set! (line-state-cursor state) (string-length result))))))
         ;; Fallback: legacy substring search
         (search-start state 'reverse)))
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-S))
     (search-start state 'forward)
     (refresh-search state out-port))
    ((and (char? key) (= (char->integer key) CTRL-G))
     ;; Cancel
     (set! (line-state-buffer state) "")
     (set! (line-state-cursor state) 0)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-UNDERSCORE))
     ;; Undo
     (undo! state)
     (refresh-line state out-port))
    ((and (char? key) (= (char->integer key) CTRL-C))
     ;; Abort line
     (display "^C\r\n" out-port)
     (force-output out-port)
     (set! (line-state-buffer state) "")
     (set! (line-state-cursor state) 0)
     (set! (line-state-done? state) #t)
     (set! (line-state-result state) ""))
    ((and (char? key) (= (char->integer key) TAB-KEY))
     ;; Tab completion
     (handle-tab-completion state out-port))
    ;; Arrow keys
    ((eq? key 'up)
     (history-prev state)
     (refresh-line state out-port))
    ((eq? key 'down)
     (history-next state)
     (refresh-line state out-port))
    ((eq? key 'left)
     (when (> (line-state-cursor state) 0)
       (set! (line-state-cursor state) (- (line-state-cursor state) 1))
       (refresh-line state out-port)))
    ((eq? key 'right)
     (when (< (line-state-cursor state) (string-length (line-state-buffer state)))
       (set! (line-state-cursor state) (+ (line-state-cursor state) 1))
       (refresh-line state out-port)))
    ((eq? key 'home)
     (set! (line-state-cursor state) 0)
     (refresh-line state out-port))
    ((eq? key 'end)
     (set! (line-state-cursor state) (string-length (line-state-buffer state)))
     (refresh-line state out-port))
    ((eq? key 'delete)
     (buf-delete-char state)
     (refresh-line state out-port))
    ;; Alt-key combinations
    ((and (list? key) (eq? (car key) 'alt))
     (handle-alt-key state (cadr key) out-port))
    (else #!void)))

;;; --- Alt-key handling ---

(def (handle-alt-key state ch out-port)
  (cond
    ;; Alt-B: backward word
    ((or (char=? ch #\b) (char=? ch #\B))
     (set! (line-state-cursor state)
           (word-backward-pos (line-state-buffer state) (line-state-cursor state)))
     (refresh-line state out-port))
    ;; Alt-F: forward word
    ((or (char=? ch #\f) (char=? ch #\F))
     (set! (line-state-cursor state)
           (word-forward-pos (line-state-buffer state) (line-state-cursor state)))
     (refresh-line state out-port))
    ;; Alt-D: kill word forward
    ((or (char=? ch #\d) (char=? ch #\D))
     (buf-kill-word-forward state)
     (refresh-line state out-port))
    ;; Alt-U: uppercase word
    ((or (char=? ch #\u) (char=? ch #\U))
     (word-transform! state char-upcase)
     (refresh-line state out-port))
    ;; Alt-L: lowercase word
    ((or (char=? ch #\l) (char=? ch #\L))
     (word-transform! state char-downcase)
     (refresh-line state out-port))
    ;; Alt-C: fuzzy directory jump via fzf (if available), else capitalize word
    ((or (char=? ch #\c) (char=? ch #\C))
     (let ((fn (*fzf-dirs-fn*)))
       (if fn
         (begin
           (term-cooked! (current-input-port))
           (let ((result (with-catch (lambda (e) #f) (lambda () (fn)))))
             (term-raw! (current-input-port))
             (when result
               (save-undo! state)
               (set! (line-state-buffer state) (string-append "cd " result))
               (set! (line-state-cursor state) (+ 3 (string-length result))))))
         (word-capitalize! state)))
     (refresh-line state out-port))
    ;; Alt-<: beginning of history
    ((char=? ch #\<)
     ;; Navigate to oldest history entry
     (let ((count (history-count)))
       (when (> count 0)
         (unless (line-state-history-index state)
           (set! (line-state-saved-line state)
                 (string-copy (line-state-buffer state))))
         (let ((entry (history-get 1)))
           (when entry
             (set! (line-state-history-index state) count)
             (set! (line-state-buffer state) (string-copy entry))
             (set! (line-state-cursor state) (string-length entry))
             (refresh-line state out-port))))))
    ;; Alt->: end of history (current line)
    ((char=? ch #\>)
     (when (line-state-history-index state)
       (set! (line-state-history-index state) #f)
       (set! (line-state-buffer state)
             (string-copy (or (line-state-saved-line state) "")))
       (set! (line-state-cursor state)
             (string-length (line-state-buffer state)))
       (refresh-line state out-port)))
    ;; Alt-.: insert last argument of previous command
    ((char=? ch #\.)
     (let ((prev (history-get-relative 0)))
       (when prev
         (let ((words (simple-split prev)))
           (when (pair? words)
             (let ((last-word (car (last-pair words))))
               (buf-insert-string state last-word)
               (refresh-line state out-port)))))))
    (else #!void)))

;;; --- Word transformations ---

(def (word-transform! state transform-fn)
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (end (word-forward-pos buf pos))
         (new-buf (string-copy buf)))
    (save-undo! state)
    (let loop ((i pos))
      (when (< i end)
        (string-set! new-buf i (transform-fn (string-ref new-buf i)))
        (loop (+ i 1))))
    (set! (line-state-buffer state) new-buf)
    (set! (line-state-cursor state) end)))

(def (word-capitalize! state)
  (let* ((buf (line-state-buffer state))
         (pos (line-state-cursor state))
         (end (word-forward-pos buf pos))
         (new-buf (string-copy buf)))
    (save-undo! state)
    ;; Skip non-alpha
    (let loop ((i pos) (first? #t))
      (when (< i end)
        (let ((ch (string-ref new-buf i)))
          (cond
            ((char-alphabetic? ch)
             (string-set! new-buf i
                         (if first? (char-upcase ch) (char-downcase ch)))
             (loop (+ i 1) #f))
            (else
             (loop (+ i 1) first?))))))
    (set! (line-state-buffer state) new-buf)
    (set! (line-state-cursor state) end)))

;;; --- Search mode key handling ---

(def (handle-search-key state key out-port)
  (cond
    ;; Ctrl-R / Ctrl-S: keep searching
    ((and (char? key) (= (char->integer key) CTRL-R))
     ;; TODO: search backwards more
     (refresh-search state out-port))
    ((and (char? key) (= (char->integer key) CTRL-S))
     ;; TODO: search forwards
     (refresh-search state out-port))
    ;; Ctrl-G: cancel search
    ((and (char? key) (= (char->integer key) CTRL-G))
     (search-cancel state)
     (refresh-line state out-port))
    ;; Enter: accept search
    ((enter-key? key)
     (search-accept state)
     (display "\r\n" out-port)
     (force-output out-port)
     (set! (line-state-done? state) #t)
     (set! (line-state-result state) (line-state-buffer state)))
    ;; Backspace: remove last search char
    ((and (char? key)
          (or (= (char->integer key) BACKSPACE) (= (char->integer key) CTRL-H)))
     (let ((s (line-state-search-string state)))
       (when (> (string-length s) 0)
         (set! (line-state-search-string state)
               (substring s 0 (- (string-length s) 1)))
         (search-update state)))
     (refresh-search state out-port))
    ;; Escape or any arrow/special: accept and handle as normal
    ((or (eq? key 'escape) (symbol? key))
     (search-accept state)
     (refresh-line state out-port))
    ;; Printable char: add to search string (exclude DEL = 127)
    ((and (char? key) (>= (char->integer key) 32) (not (= (char->integer key) 127)))
     (set! (line-state-search-string state)
           (string-append (line-state-search-string state) (string key)))
     (search-update state)
     (refresh-search state out-port))
    ;; Anything else: accept search and re-handle key
    (else
     (search-accept state)
     (handle-emacs-key state key out-port))))

;;; --- Tab completion ---

(def (handle-tab-completion state out-port)
  (let ((fn (line-state-complete-fn state)))
    (when fn
      (let ((completions (fn (line-state-buffer state)
                             (line-state-cursor state))))
        (cond
          ((null? completions) #!void)  ;; No completions — beep?
          ((= (length completions) 1)
           ;; Single completion: insert it
           (let ((completion (car completions))
                 (word-start (find-word-start-for-completion
                              (line-state-buffer state)
                              (line-state-cursor state))))
             (save-undo! state)
             (set! (line-state-buffer state)
                   (string-append
                    (substring (line-state-buffer state) 0 word-start)
                    completion
                    " "
                    (substring (line-state-buffer state)
                               (line-state-cursor state)
                               (string-length (line-state-buffer state)))))
             (set! (line-state-cursor state)
                   (+ word-start (string-length completion) 1))
             (refresh-line state out-port)))
          (else
           ;; Multiple completions: insert common prefix, display list
           (let* ((prefix (common-prefix completions))
                  (word-start (find-word-start-for-completion
                               (line-state-buffer state)
                               (line-state-cursor state))))
             (when (> (string-length prefix) (- (line-state-cursor state) word-start))
               (save-undo! state)
               (set! (line-state-buffer state)
                     (string-append
                      (substring (line-state-buffer state) 0 word-start)
                      prefix
                      (substring (line-state-buffer state)
                                 (line-state-cursor state)
                                 (string-length (line-state-buffer state)))))
               (set! (line-state-cursor state) (+ word-start (string-length prefix))))
             ;; Display completions
             (display "\r\n" out-port)
             (display-completions completions out-port (line-state-columns state))
             (refresh-line state out-port))))))))

(def (find-word-start-for-completion buf cursor)
  ;; Find start of the word being completed
  (let loop ((i (- cursor 1)))
    (if (or (< i 0)
            (char-whitespace? (string-ref buf i))
            (char=? (string-ref buf i) #\/)
            (char=? (string-ref buf i) #\=))
      (+ i 1)
      (loop (- i 1)))))

(def (common-prefix strings)
  ;; Find longest common prefix of a list of strings
  (if (null? strings) ""
      (let ((first (car strings)))
        (let loop ((len (string-length first)))
          (if (<= len 0) ""
              (let ((prefix (substring first 0 len)))
                (if (every? (lambda (s)
                              (and (>= (string-length s) len)
                                   (string=? (substring s 0 len) prefix)))
                            strings)
                  prefix
                  (loop (- len 1)))))))))

(def (every? pred lst)
  (cond
    ((null? lst) #t)
    ((pred (car lst)) (every? pred (cdr lst)))
    (else #f)))

(def (display-completions completions port columns)
  ;; Display completions in columns
  ;; Use \r\n because terminal is in raw output mode (no LF→CRLF conversion)
  (let* ((max-len (apply max (map string-length completions)))
         (col-width (+ max-len 2))
         (ncols (max 1 (quotient columns col-width))))
    (let loop ((items completions) (col 0))
      (unless (null? items)
        (let ((item (car items)))
          (display item port)
          (if (= (+ col 1) ncols)
            (begin (display "\r\n" port) (loop (cdr items) 0))
            (begin
              (display (make-string (- col-width (string-length item)) #\space) port)
              (loop (cdr items) (+ col 1)))))))
    (when (and (pair? completions)
               (not (= 0 (modulo (length completions) ncols))))
      (display "\r\n" port))
    (force-output port)))

;;; --- Vi mode (basic) ---

(def (handle-vi-key state key out-port)
  (if (eq? (line-state-vi-mode state) 'insert)
    (handle-vi-insert state key out-port)
    (handle-vi-command state key out-port)))

(def (handle-vi-insert state key out-port)
  (cond
    ;; Escape -> command mode
    ((eq? key 'escape)
     (set! (line-state-vi-mode state) 'command)
     (when (> (line-state-cursor state) 0)
       (set! (line-state-cursor state) (- (line-state-cursor state) 1)))
     (refresh-line state out-port))
    ;; Enter
    ((enter-key? key)
     (display "\r\n" out-port)
     (force-output out-port)
     (set! (line-state-done? state) #t)
     (set! (line-state-result state) (line-state-buffer state)))
    ;; Backspace
    ((and (char? key) (or (= (char->integer key) BACKSPACE)
                          (= (char->integer key) CTRL-H)))
     (buf-backspace state)
     (refresh-line state out-port))
    ;; Ctrl-C
    ((and (char? key) (= (char->integer key) CTRL-C))
     (display "^C\r\n" out-port)
     (force-output out-port)
     (set! (line-state-buffer state) "")
     (set! (line-state-cursor state) 0)
     (set! (line-state-done? state) #t)
     (set! (line-state-result state) ""))
    ;; Tab
    ((and (char? key) (= (char->integer key) TAB-KEY))
     (handle-tab-completion state out-port))
    ;; Ctrl-D
    ((and (char? key) (= (char->integer key) CTRL-D))
     (if (= (string-length (line-state-buffer state)) 0)
       (begin
         (display "\r\n" out-port)
         (force-output out-port)
         (set! (line-state-done? state) #t)
         (set! (line-state-result state) 'eof))
       (begin
         (buf-delete-char state)
         (refresh-line state out-port))))
    ;; Regular printable character (exclude DEL = 127)
    ((and (char? key) (>= (char->integer key) 32) (not (= (char->integer key) 127)))
     (buf-insert state key)
     (refresh-line state out-port))
    (else #!void)))

(def (handle-vi-command state key out-port)
  (cond
    ;; Movement
    ((and (char? key) (char=? key #\h))
     (when (> (line-state-cursor state) 0)
       (set! (line-state-cursor state) (- (line-state-cursor state) 1))
       (refresh-line state out-port)))
    ((and (char? key) (char=? key #\l))
     (when (< (line-state-cursor state)
              (- (string-length (line-state-buffer state)) 1))
       (set! (line-state-cursor state) (+ (line-state-cursor state) 1))
       (refresh-line state out-port)))
    ((and (char? key) (char=? key #\w))
     (set! (line-state-cursor state)
           (word-forward-pos (line-state-buffer state) (line-state-cursor state)))
     (refresh-line state out-port))
    ((and (char? key) (char=? key #\b))
     (set! (line-state-cursor state)
           (word-backward-pos (line-state-buffer state) (line-state-cursor state)))
     (refresh-line state out-port))
    ((and (char? key) (char=? key #\0))
     (set! (line-state-cursor state) 0)
     (refresh-line state out-port))
    ((and (char? key) (char=? key #\$))
     (let ((len (string-length (line-state-buffer state))))
       (set! (line-state-cursor state) (max 0 (- len 1)))
       (refresh-line state out-port)))
    ;; Insert modes
    ((and (char? key) (char=? key #\i))
     (set! (line-state-vi-mode state) 'insert))
    ((and (char? key) (char=? key #\a))
     (when (< (line-state-cursor state) (string-length (line-state-buffer state)))
       (set! (line-state-cursor state) (+ (line-state-cursor state) 1)))
     (set! (line-state-vi-mode state) 'insert))
    ((and (char? key) (char=? key #\A))
     (set! (line-state-cursor state) (string-length (line-state-buffer state)))
     (set! (line-state-vi-mode state) 'insert))
    ((and (char? key) (char=? key #\I))
     (set! (line-state-cursor state) 0)
     (set! (line-state-vi-mode state) 'insert))
    ;; Delete char
    ((and (char? key) (char=? key #\x))
     (buf-delete-char state)
     (refresh-line state out-port))
    ;; Delete to end
    ((and (char? key) (char=? key #\D))
     (buf-kill-to-end state)
     (refresh-line state out-port))
    ;; Change to end
    ((and (char? key) (char=? key #\C))
     (buf-kill-to-end state)
     (set! (line-state-vi-mode state) 'insert)
     (refresh-line state out-port))
    ;; History
    ((and (char? key) (char=? key #\k))
     (history-prev state)
     (refresh-line state out-port))
    ((and (char? key) (char=? key #\j))
     (history-next state)
     (refresh-line state out-port))
    ((eq? key 'up)
     (history-prev state)
     (refresh-line state out-port))
    ((eq? key 'down)
     (history-next state)
     (refresh-line state out-port))
    ;; Enter
    ((enter-key? key)
     (display "\r\n" out-port)
     (force-output out-port)
     (set! (line-state-done? state) #t)
     (set! (line-state-result state) (line-state-buffer state)))
    (else #!void)))

;;; --- Helpers ---

(def (simple-split str)
  ;; Split string on whitespace
  (string-split-chars str #\space))

(def (user-name)
  (or (getenv "USER" #f) "user"))

;;; --- Public interface ---

;; Read a line of input with editing.
;; prompt: string to display
;; complete-fn: (lambda (line cursor) -> list-of-strings) or #f
;; mode: 'emacs or 'vi
;; Returns: string (the line), or 'eof
(def (line-edit prompt
                (complete-fn #f)
                (mode 'emacs))
  (let ((in-port (current-input-port))
        (out-port (current-error-port)))
    ;; If not a tty, just read a line
    (if (not (with-catch (lambda (e) #f)
               (lambda () (tty? in-port))))
      ;; Non-interactive: just read a line
      (begin
        (display prompt out-port)
        (force-output out-port)
        (let ((line (read-line in-port)))
          (if (eof-object? line) 'eof line)))
      ;; Interactive: use line editor
      (let* ((cols (detect-terminal-columns))
             (pw (visible-width-last-line prompt)))
        (set-terminal-columns! cols)
        (let ((state (make-line-state
                      ""          ;; buffer
                      0           ;; cursor
                      prompt      ;; prompt
                      pw          ;; prompt-width
                      #f          ;; history-index
                      ""          ;; saved-line
                      []          ;; kill-ring
                      []          ;; undo-stack
                      #f          ;; search-mode
                      ""          ;; search-string
                      #f          ;; search-result
                      cols        ;; columns
                      #f          ;; multiline?
                      mode        ;; mode
                      'insert     ;; vi-mode
                      complete-fn ;; complete-fn
                      #f          ;; done?
                      #f)))       ;; result
          (dynamic-wind
            (lambda () (term-raw! in-port))
            (lambda ()
              ;; Ensure we're on a fresh line before showing prompt
              ;; If previous command didn't end with newline, this prevents bleed-over
              (display "\n" out-port)
              (force-output out-port)
              ;; Show initial prompt
              (refresh-line state out-port)
              ;; Main editing loop
              (let loop ()
                (let ((key (read-key in-port)))
                  (if (eq? key 'eof)
                    (begin
                      (set! (line-state-done? state) #t)
                      (set! (line-state-result state) 'eof)
                      'eof)
                    (begin
                      (if (eq? mode 'vi)
                        (handle-vi-key state key out-port)
                        (handle-emacs-key state key out-port))
                      (if (line-state-done? state)
                        (line-state-result state)
                        (loop)))))))
            (lambda () (term-cooked! in-port))))))))

;; Check if a port is a terminal
(def (tty? port)
  (with-catch (lambda (e) #f)
    (lambda () (tty-mode-set! port #t #t #f #f 0) #t)))

;; Calculate visible width (ignoring ANSI escapes)
(def (visible-width str)
  (let ((len (string-length str)))
    (let loop ((i 0) (width 0) (in-escape? #f))
      (cond
        ((>= i len) width)
        ((and (not in-escape?)
              (char=? (string-ref str i) #\escape))
         (loop (+ i 1) width #t))
        (in-escape?
         (if (char-alphabetic? (string-ref str i))
           (loop (+ i 1) width #f)
           (loop (+ i 1) width #t)))
        (else
         (loop (+ i 1) (+ width 1) #f))))))

(def (visible-width-last-line str)
  ;; For multi-line prompts, return width of only the last line
  ;; Find the last newline and measure from there
  (let* ((len (string-length str))
         (last-newline (let loop ((i (- len 1)))
                         (cond
                           ((< i 0) -1)
                           ((char=? (string-ref str i) #\newline) i)
                           (else (loop (- i 1))))))
         (start (+ last-newline 1)))
    ;; Calculate visible width from start to end
    (let loop ((i start) (width 0) (in-escape? #f))
      (cond
        ((>= i len) width)
        ((and (not in-escape?)
              (char=? (string-ref str i) #\escape))
         (loop (+ i 1) width #t))
        (in-escape?
         (if (char-alphabetic? (string-ref str i))
           (loop (+ i 1) width #f)
           (loop (+ i 1) width #t)))
        (else
         (loop (+ i 1) (+ width 1) #f))))))

(def (count-newlines str)
  ;; Count number of newline characters in string
  (let ((len (string-length str)))
    (let loop ((i 0) (count 0))
      (cond
        ((>= i len) count)
        ((char=? (string-ref str i) #\newline)
         (loop (+ i 1) (+ count 1)))
        (else
         (loop (+ i 1) count))))))
