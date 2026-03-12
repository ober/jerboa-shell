#!chezscheme
(library (jsh lineedit)
  (export *fzf-history-fn* *fzf-files-fn* *fzf-dirs-fn* ESC
   ESC-BYTE CTRL-A CTRL-B CTRL-C CTRL-D CTRL-E CTRL-F CTRL-G
   CTRL-H CTRL-K CTRL-L CTRL-N CTRL-P CTRL-R CTRL-S CTRL-T
   CTRL-U CTRL-W CTRL-Y CTRL-Z CTRL-UNDERSCORE TAB-KEY
   ENTER-KEY NEWLINE-KEY enter-key? BACKSPACE line-state::t
   make-line-state line-state? line-state-buffer
   line-state-cursor line-state-prompt line-state-prompt-width
   line-state-history-index line-state-saved-line
   line-state-kill-ring line-state-undo-stack
   line-state-search-mode line-state-search-string
   line-state-search-result line-state-columns
   line-state-multiline? line-state-mode line-state-vi-mode
   line-state-complete-fn line-state-done? line-state-result
   line-state-buffer-set! line-state-cursor-set!
   line-state-prompt-set! line-state-prompt-width-set!
   line-state-history-index-set! line-state-saved-line-set!
   line-state-kill-ring-set! line-state-undo-stack-set!
   line-state-search-mode-set! line-state-search-string-set!
   line-state-search-result-set! line-state-columns-set!
   line-state-multiline?-set! line-state-mode-set!
   line-state-vi-mode-set! line-state-complete-fn-set!
   line-state-done?-set! line-state-result-set!
   &line-state-buffer &line-state-cursor &line-state-prompt
   &line-state-prompt-width &line-state-history-index
   &line-state-saved-line &line-state-kill-ring
   &line-state-undo-stack &line-state-search-mode
   &line-state-search-string &line-state-search-result
   &line-state-columns &line-state-multiline? &line-state-mode
   &line-state-vi-mode &line-state-complete-fn
   &line-state-done? &line-state-result &line-state-buffer-set!
   &line-state-cursor-set! &line-state-prompt-set!
   &line-state-prompt-width-set! &line-state-history-index-set!
   &line-state-saved-line-set! &line-state-kill-ring-set!
   &line-state-undo-stack-set! &line-state-search-mode-set!
   &line-state-search-string-set!
   &line-state-search-result-set! &line-state-columns-set!
   &line-state-multiline?-set! &line-state-mode-set!
   &line-state-vi-mode-set! &line-state-complete-fn-set!
   &line-state-done?-set! &line-state-result-set! term-raw!
   term-cooked! ESC-STR term-move-cursor-col term-clear-to-eol
   term-clear-line term-clear-screen term-move-up
   term-move-down term-cursor-forward term-cursor-back
   *terminal-columns* get-terminal-columns
   set-terminal-columns! detect-terminal-columns read-key
   refresh-line refresh-search buf-insert buf-insert-string
   buf-delete-char buf-backspace buf-kill-to-end
   buf-kill-to-start buf-kill-word-back buf-kill-word-forward
   buf-transpose-chars word-forward-pos word-backward-pos
   char-alphanumeric? kill-ring-push! kill-ring-top
   take-at-most save-undo! undo! history-prev history-next
   search-start search-update search-accept search-cancel
   handle-emacs-key handle-alt-key word-transform!
   word-capitalize! handle-search-key handle-tab-completion
   find-word-start-for-completion common-prefix every?
   display-completions handle-vi-key handle-vi-insert
   handle-vi-command simple-split user-name line-edit tty?
   visible-width visible-width-last-line count-newlines)
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
      with-output-to-string user-name string->bytes bytes->string
      thread?)
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
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase)
    (except (jsh history) string-trim) (jsh ffi))
  (define *fzf-history-fn* (make-parameter #f))
  (define *fzf-files-fn* (make-parameter #f))
  (define *fzf-dirs-fn* (make-parameter #f))
  (define ESC #\esc)
  (define ESC-BYTE 27)
  (define CTRL-A 1)
  (define CTRL-B 2)
  (define CTRL-C 3)
  (define CTRL-D 4)
  (define CTRL-E 5)
  (define CTRL-F 6)
  (define CTRL-G 7)
  (define CTRL-H 8)
  (define CTRL-K 11)
  (define CTRL-L 12)
  (define CTRL-N 14)
  (define CTRL-P 16)
  (define CTRL-R 18)
  (define CTRL-S 19)
  (define CTRL-T 20)
  (define CTRL-U 21)
  (define CTRL-W 23)
  (define CTRL-Y 25)
  (define CTRL-Z 26)
  (define CTRL-UNDERSCORE 31)
  (define TAB-KEY 9)
  (define ENTER-KEY 13)
  (define NEWLINE-KEY 10)
  (define (enter-key? key)
    (and (char? key)
         (let ([code (char->integer key)])
           (or (= code ENTER-KEY) (= code NEWLINE-KEY)))))
  (define BACKSPACE 127)
  (begin
    (define line-state::t
      (make-class-type 'gerbil\x23;line-state::t 'line-state (list object::t)
        '(buffer cursor prompt prompt-width history-index saved-line
           kill-ring undo-stack search-mode search-string search-result
           columns multiline? mode vi-mode complete-fn done? result)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-line-state . args)
      (let* ([type line-state::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (line-state? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;line-state::t))
    (define (line-state-buffer obj)
      (unchecked-slot-ref obj 'buffer))
    (define (line-state-cursor obj)
      (unchecked-slot-ref obj 'cursor))
    (define (line-state-prompt obj)
      (unchecked-slot-ref obj 'prompt))
    (define (line-state-prompt-width obj)
      (unchecked-slot-ref obj 'prompt-width))
    (define (line-state-history-index obj)
      (unchecked-slot-ref obj 'history-index))
    (define (line-state-saved-line obj)
      (unchecked-slot-ref obj 'saved-line))
    (define (line-state-kill-ring obj)
      (unchecked-slot-ref obj 'kill-ring))
    (define (line-state-undo-stack obj)
      (unchecked-slot-ref obj 'undo-stack))
    (define (line-state-search-mode obj)
      (unchecked-slot-ref obj 'search-mode))
    (define (line-state-search-string obj)
      (unchecked-slot-ref obj 'search-string))
    (define (line-state-search-result obj)
      (unchecked-slot-ref obj 'search-result))
    (define (line-state-columns obj)
      (unchecked-slot-ref obj 'columns))
    (define (line-state-multiline? obj)
      (unchecked-slot-ref obj 'multiline?))
    (define (line-state-mode obj)
      (unchecked-slot-ref obj 'mode))
    (define (line-state-vi-mode obj)
      (unchecked-slot-ref obj 'vi-mode))
    (define (line-state-complete-fn obj)
      (unchecked-slot-ref obj 'complete-fn))
    (define (line-state-done? obj)
      (unchecked-slot-ref obj 'done?))
    (define (line-state-result obj)
      (unchecked-slot-ref obj 'result))
    (define (line-state-buffer-set! obj val)
      (unchecked-slot-set! obj 'buffer val))
    (define (line-state-cursor-set! obj val)
      (unchecked-slot-set! obj 'cursor val))
    (define (line-state-prompt-set! obj val)
      (unchecked-slot-set! obj 'prompt val))
    (define (line-state-prompt-width-set! obj val)
      (unchecked-slot-set! obj 'prompt-width val))
    (define (line-state-history-index-set! obj val)
      (unchecked-slot-set! obj 'history-index val))
    (define (line-state-saved-line-set! obj val)
      (unchecked-slot-set! obj 'saved-line val))
    (define (line-state-kill-ring-set! obj val)
      (unchecked-slot-set! obj 'kill-ring val))
    (define (line-state-undo-stack-set! obj val)
      (unchecked-slot-set! obj 'undo-stack val))
    (define (line-state-search-mode-set! obj val)
      (unchecked-slot-set! obj 'search-mode val))
    (define (line-state-search-string-set! obj val)
      (unchecked-slot-set! obj 'search-string val))
    (define (line-state-search-result-set! obj val)
      (unchecked-slot-set! obj 'search-result val))
    (define (line-state-columns-set! obj val)
      (unchecked-slot-set! obj 'columns val))
    (define (line-state-multiline?-set! obj val)
      (unchecked-slot-set! obj 'multiline? val))
    (define (line-state-mode-set! obj val)
      (unchecked-slot-set! obj 'mode val))
    (define (line-state-vi-mode-set! obj val)
      (unchecked-slot-set! obj 'vi-mode val))
    (define (line-state-complete-fn-set! obj val)
      (unchecked-slot-set! obj 'complete-fn val))
    (define (line-state-done?-set! obj val)
      (unchecked-slot-set! obj 'done? val))
    (define (line-state-result-set! obj val)
      (unchecked-slot-set! obj 'result val))
    (define (&line-state-buffer obj)
      (unchecked-slot-ref obj 'buffer))
    (define (&line-state-cursor obj)
      (unchecked-slot-ref obj 'cursor))
    (define (&line-state-prompt obj)
      (unchecked-slot-ref obj 'prompt))
    (define (&line-state-prompt-width obj)
      (unchecked-slot-ref obj 'prompt-width))
    (define (&line-state-history-index obj)
      (unchecked-slot-ref obj 'history-index))
    (define (&line-state-saved-line obj)
      (unchecked-slot-ref obj 'saved-line))
    (define (&line-state-kill-ring obj)
      (unchecked-slot-ref obj 'kill-ring))
    (define (&line-state-undo-stack obj)
      (unchecked-slot-ref obj 'undo-stack))
    (define (&line-state-search-mode obj)
      (unchecked-slot-ref obj 'search-mode))
    (define (&line-state-search-string obj)
      (unchecked-slot-ref obj 'search-string))
    (define (&line-state-search-result obj)
      (unchecked-slot-ref obj 'search-result))
    (define (&line-state-columns obj)
      (unchecked-slot-ref obj 'columns))
    (define (&line-state-multiline? obj)
      (unchecked-slot-ref obj 'multiline?))
    (define (&line-state-mode obj)
      (unchecked-slot-ref obj 'mode))
    (define (&line-state-vi-mode obj)
      (unchecked-slot-ref obj 'vi-mode))
    (define (&line-state-complete-fn obj)
      (unchecked-slot-ref obj 'complete-fn))
    (define (&line-state-done? obj)
      (unchecked-slot-ref obj 'done?))
    (define (&line-state-result obj)
      (unchecked-slot-ref obj 'result))
    (define (&line-state-buffer-set! obj val)
      (unchecked-slot-set! obj 'buffer val))
    (define (&line-state-cursor-set! obj val)
      (unchecked-slot-set! obj 'cursor val))
    (define (&line-state-prompt-set! obj val)
      (unchecked-slot-set! obj 'prompt val))
    (define (&line-state-prompt-width-set! obj val)
      (unchecked-slot-set! obj 'prompt-width val))
    (define (&line-state-history-index-set! obj val)
      (unchecked-slot-set! obj 'history-index val))
    (define (&line-state-saved-line-set! obj val)
      (unchecked-slot-set! obj 'saved-line val))
    (define (&line-state-kill-ring-set! obj val)
      (unchecked-slot-set! obj 'kill-ring val))
    (define (&line-state-undo-stack-set! obj val)
      (unchecked-slot-set! obj 'undo-stack val))
    (define (&line-state-search-mode-set! obj val)
      (unchecked-slot-set! obj 'search-mode val))
    (define (&line-state-search-string-set! obj val)
      (unchecked-slot-set! obj 'search-string val))
    (define (&line-state-search-result-set! obj val)
      (unchecked-slot-set! obj 'search-result val))
    (define (&line-state-columns-set! obj val)
      (unchecked-slot-set! obj 'columns val))
    (define (&line-state-multiline?-set! obj val)
      (unchecked-slot-set! obj 'multiline? val))
    (define (&line-state-mode-set! obj val)
      (unchecked-slot-set! obj 'mode val))
    (define (&line-state-vi-mode-set! obj val)
      (unchecked-slot-set! obj 'vi-mode val))
    (define (&line-state-complete-fn-set! obj val)
      (unchecked-slot-set! obj 'complete-fn val))
    (define (&line-state-done?-set! obj val)
      (unchecked-slot-set! obj 'done? val))
    (define (&line-state-result-set! obj val)
      (unchecked-slot-set! obj 'result val)))
  (define (term-raw! port) (tty-mode-set! port #f #f #t #t 0))
  (define (term-cooked! port)
    (tty-mode-set! port #t #t #f #f 0))
  (define ESC-STR (string #\esc))
  (define (term-move-cursor-col col port)
    (fprintf port "\r~a[~aC" ESC-STR (+ col 1)))
  (define (term-clear-to-eol port)
    (display (string-append ESC-STR "[K") port))
  (define (term-clear-line port)
    (display (string-append "\r" ESC-STR "[K") port))
  (define (term-clear-screen port)
    (display (string-append ESC-STR "[H" ESC-STR "[2J") port))
  (define (term-move-up n port)
    (when (> n 0) (fprintf port "~a[~aA" ESC-STR n)))
  (define (term-move-down n port)
    (when (> n 0) (fprintf port "~a[~aB" ESC-STR n)))
  (define (term-cursor-forward n port)
    (when (> n 0) (fprintf port "~a[~aC" ESC-STR n)))
  (define (term-cursor-back n port)
    (when (> n 0) (fprintf port "~a[~aD" ESC-STR n)))
  (define *terminal-columns*-cell (vector 80))
  (define-syntax *terminal-columns*
    (identifier-syntax
      [id (vector-ref *terminal-columns*-cell 0)]
      [(set! id v) (vector-set! *terminal-columns*-cell 0 v)]))
  (define (get-terminal-columns) *terminal-columns*)
  (define (set-terminal-columns! cols)
    (set! *terminal-columns* cols))
  (define (detect-terminal-columns)
    (let ([cols (ffi-terminal-columns 0)])
      (if (or (not cols) (= cols 0)) 80 cols)))
  (define (read-key port)
    (let ([ch (read-char port)])
      (cond
        [(eof-object? ch) 'eof]
        [(char=? ch ESC)
         (if (char-ready? port)
             (let ([ch2 (read-char port)])
               (cond
                 [(eof-object? ch2) 'escape]
                 [(char=? ch2 #\[)
                  (let ([ch3 (read-char port)])
                    (cond
                      [(eof-object? ch3) 'escape]
                      [(char=? ch3 #\A) 'up]
                      [(char=? ch3 #\B) 'down]
                      [(char=? ch3 #\C) 'right]
                      [(char=? ch3 #\D) 'left]
                      [(char=? ch3 #\H) 'home]
                      [(char=? ch3 #\F) 'end]
                      [(and (char>=? ch3 #\0) (char<=? ch3 #\9))
                       (let ([num (- (char->integer ch3)
                                     (char->integer #\0))])
                         (let ([ch4 (read-char port)])
                           (cond
                             [(eof-object? ch4) 'escape]
                             [(char=? ch4 #\~)
                              (case num
                                [(1) 'home]
                                [(3) 'delete]
                                [(4) 'end]
                                [(5) 'page-up]
                                [(6) 'page-down]
                                [(7) 'home]
                                [(8) 'end]
                                [else 'unknown])]
                             [(and (char>=? ch4 #\0) (char<=? ch4 #\9))
                              (let loop ()
                                (let ([c (read-char port)])
                                  (if (or (eof-object? c) (char=? c #\~))
                                      'unknown
                                      (loop))))
                              'unknown]
                             [else 'unknown])))]
                      [else 'unknown]))]
                 [(char=? ch2 #\O)
                  (let ([ch3 (read-char port)])
                    (cond
                      [(eof-object? ch3) 'escape]
                      [(char=? ch3 #\A) 'up]
                      [(char=? ch3 #\B) 'down]
                      [(char=? ch3 #\C) 'right]
                      [(char=? ch3 #\D) 'left]
                      [(char=? ch3 #\H) 'home]
                      [(char=? ch3 #\F) 'end]
                      [else 'unknown]))]
                 [else (list 'alt ch2)]))
             'escape)]
        [else ch])))
  (define (refresh-line state port)
    (let* ([prompt (line-state-prompt state)])
      (let* ([buf (line-state-buffer state)])
        (let* ([cursor (line-state-cursor state)])
          (let* ([pwidth (line-state-prompt-width state)])
            (let* ([cols (line-state-columns state)])
              (let* ([prompt-lines (count-newlines prompt)])
                (let* ([last-line-width (visible-width-last-line prompt)])
                  (when (> prompt-lines 0)
                    (term-move-up prompt-lines port))
                  (display "\r" port)
                  (let ([len (string-length prompt)])
                    (let loop ([i 0])
                      (when (< i len)
                        (let ([ch (string-ref prompt i)])
                          (display (string ch) port)
                          (when (char=? ch #\newline) (display "\r" port)))
                        (loop (+ i 1)))))
                  (display buf port)
                  (term-clear-to-eol port)
                  (let ([cursor-col (+ last-line-width cursor)])
                    (let ([end-col (+ last-line-width
                                      (string-length buf))])
                      (when (> end-col cursor-col)
                        (term-cursor-back (- end-col cursor-col) port))))
                  (flush-output-port port)))))))))
  (define (refresh-search state port)
    (let* ([search-str (line-state-search-string state)])
      (let* ([result (or (line-state-search-result state) "")])
        (let* ([mode-str (if (eq? (line-state-search-mode state)
                                  'reverse)
                             "(reverse-i-search)"
                             "(i-search)")])
          (display "\r" port)
          (term-clear-to-eol port)
          (fprintf port "~a`~a': ~a" mode-str search-str result)
          (flush-output-port port)))))
  (define (buf-insert state ch)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([new-buf (string-append
                          (substring buf 0 pos)
                          (string ch)
                          (substring buf pos (string-length buf)))])
          (save-undo! state)
          (line-state-buffer-set! state new-buf)
          (line-state-cursor-set! state (+ pos 1))))))
  (define (buf-insert-string state str)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([new-buf (string-append
                          (substring buf 0 pos)
                          str
                          (substring buf pos (string-length buf)))])
          (save-undo! state)
          (line-state-buffer-set! state new-buf)
          (line-state-cursor-set!
            state
            (+ pos (string-length str)))))))
  (define (buf-delete-char state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([len (string-length buf)])
          (when (< pos len)
            (save-undo! state)
            (line-state-buffer-set!
              state
              (string-append
                (substring buf 0 pos)
                (substring buf (+ pos 1) len))))))))
  (define (buf-backspace state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (when (> pos 0)
          (save-undo! state)
          (line-state-buffer-set!
            state
            (string-append
              (substring buf 0 (- pos 1))
              (substring buf pos (string-length buf))))
          (line-state-cursor-set! state (- pos 1))))))
  (define (buf-kill-to-end state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([killed (substring buf pos (string-length buf))])
          (when (> (string-length killed) 0)
            (save-undo! state)
            (kill-ring-push! state killed)
            (line-state-buffer-set! state (substring buf 0 pos)))))))
  (define (buf-kill-to-start state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([killed (substring buf 0 pos)])
          (when (> (string-length killed) 0)
            (save-undo! state)
            (kill-ring-push! state killed)
            (line-state-buffer-set!
              state
              (substring buf pos (string-length buf)))
            (line-state-cursor-set! state 0))))))
  (define (buf-kill-word-back state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([start (let loop ([i (- pos 1)])
                        (if (and (>= i 0)
                                 (char-whitespace? (string-ref buf i)))
                            (loop (- i 1))
                            (+ i 1)))])
          (let* ([word-start (let loop ([i (- start 1)])
                               (if (and (>= i 0)
                                        (not (char-whitespace?
                                               (string-ref buf i))))
                                   (loop (- i 1))
                                   (+ i 1)))])
            (let* ([killed (substring buf word-start pos)])
              (when (> (string-length killed) 0)
                (save-undo! state)
                (kill-ring-push! state killed)
                (line-state-buffer-set!
                  state
                  (string-append
                    (substring buf 0 word-start)
                    (substring buf pos (string-length buf))))
                (line-state-cursor-set! state word-start))))))))
  (define (buf-kill-word-forward state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([len (string-length buf)])
          (let* ([start (let loop ([i pos])
                          (if (and (< i len)
                                   (char-whitespace? (string-ref buf i)))
                              (loop (+ i 1))
                              i))])
            (let* ([word-end (let loop ([i start])
                               (if (and (< i len)
                                        (not (char-whitespace?
                                               (string-ref buf i))))
                                   (loop (+ i 1))
                                   i))])
              (let* ([killed (substring buf pos word-end)])
                (when (> (string-length killed) 0)
                  (save-undo! state)
                  (kill-ring-push! state killed)
                  (line-state-buffer-set!
                    state
                    (string-append
                      (substring buf 0 pos)
                      (substring buf word-end len)))))))))))
  (define (buf-transpose-chars state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([len (string-length buf)])
          (when (and (> pos 0) (> len 1))
            (save-undo! state)
            (let* ([p (if (= pos len) (- pos 1) pos)])
              (let* ([c1 (string-ref buf (- p 1))])
                (let* ([c2 (string-ref buf p)])
                  (let* ([new-buf (string-copy buf)])
                    (string-set! new-buf (- p 1) c2)
                    (string-set! new-buf p c1)
                    (line-state-buffer-set! state new-buf)
                    (line-state-cursor-set!
                      state
                      (min (+ p 1) len)))))))))))
  (define (word-forward-pos buf pos)
    (let ([len (string-length buf)])
      (let loop ([i pos])
        (if (and (< i len)
                 (not (char-alphanumeric? (string-ref buf i))))
            (loop (+ i 1))
            (let loop2 ([i i])
              (if (and (< i len) (char-alphanumeric? (string-ref buf i)))
                  (loop2 (+ i 1))
                  i))))))
  (define (word-backward-pos buf pos)
    (if (<= pos 0)
        0
        (let loop ([i (- pos 1)])
          (if (and (> i 0)
                   (not (char-alphanumeric? (string-ref buf i))))
              (loop (- i 1))
              (let loop2 ([i i])
                (if (and (> i 0)
                         (char-alphanumeric? (string-ref buf (- i 1))))
                    (loop2 (- i 1))
                    i))))))
  (define (char-alphanumeric? ch)
    (or (char-alphabetic? ch)
        (char-numeric? ch)
        (char=? ch #\_)))
  (define (kill-ring-push! state text)
    (line-state-kill-ring-set!
      state
      (cons text (take-at-most (line-state-kill-ring state) 31))))
  (define (kill-ring-top state)
    (let ([kr (line-state-kill-ring state)])
      (if (null? kr) #f (car kr))))
  (define (take-at-most lst n)
    (if (or (null? lst) (<= n 0))
        (list)
        (cons (car lst) (take-at-most (cdr lst) (- n 1)))))
  (define (save-undo! state)
    (line-state-undo-stack-set!
      state
      (cons
        (cons
          (string-copy (line-state-buffer state))
          (line-state-cursor state))
        (take-at-most (line-state-undo-stack state) 99))))
  (define (undo! state)
    (let ([stack (line-state-undo-stack state)])
      (when (pair? stack)
        (let ([entry (car stack)])
          (line-state-buffer-set! state (car entry))
          (line-state-cursor-set! state (cdr entry))
          (line-state-undo-stack-set! state (cdr stack))))))
  (define (history-prev state)
    (let ([idx (or (line-state-history-index state) 0)])
      (let ([entry (history-get-relative idx)])
        (when entry
          (unless (line-state-history-index state)
            (line-state-saved-line-set!
              state
              (string-copy (line-state-buffer state))))
          (line-state-history-index-set! state (+ idx 1))
          (line-state-buffer-set! state (string-copy entry))
          (line-state-cursor-set! state (string-length entry))))))
  (define (history-next state)
    (let ([idx (line-state-history-index state)])
      (when (and idx (> idx 0))
        (let ([new-idx (- idx 1)])
          (if (= new-idx 0)
              (begin
                (line-state-history-index-set! state #f)
                (line-state-buffer-set!
                  state
                  (string-copy (or (line-state-saved-line state) "")))
                (line-state-cursor-set!
                  state
                  (string-length (line-state-buffer state))))
              (let ([entry (history-get-relative (- new-idx 1))])
                (when entry
                  (line-state-history-index-set! state new-idx)
                  (line-state-buffer-set! state (string-copy entry))
                  (line-state-cursor-set!
                    state
                    (string-length entry)))))))))
  (define (search-start state mode)
    (line-state-search-mode-set! state mode)
    (line-state-search-string-set! state "")
    (line-state-search-result-set! state #f))
  (define (search-update state)
    (let ([query (line-state-search-string state)])
      (when (> (string-length query) 0)
        (let ([result (history-search-reverse query)])
          (line-state-search-result-set! state result)))))
  (define (search-accept state)
    (let ([result (line-state-search-result state)])
      (line-state-search-mode-set! state #f)
      (when result
        (line-state-buffer-set! state (string-copy result))
        (line-state-cursor-set! state (string-length result)))))
  (define (search-cancel state)
    (line-state-search-mode-set! state #f)
    (line-state-search-string-set! state "")
    (line-state-search-result-set! state #f))
  (define (handle-emacs-key state key out-port)
    (cond
      [(line-state-search-mode state)
       (handle-search-key state key out-port)]
      [(and (char? key)
            (>= (char->integer key) 32)
            (not (= (char->integer key) 127)))
       (buf-insert state key)
       (refresh-line state out-port)]
      [(enter-key? key)
       (display "\r\n" out-port)
       (flush-output-port out-port)
       (line-state-done?-set! state #t)
       (line-state-result-set! state (line-state-buffer state))]
      [(and (char? key) (= (char->integer key) CTRL-A))
       (line-state-cursor-set! state 0)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-E))
       (line-state-cursor-set!
         state
         (string-length (line-state-buffer state)))
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-B))
       (when (> (line-state-cursor state) 0)
         (line-state-cursor-set!
           state
           (- (line-state-cursor state) 1))
         (refresh-line state out-port))]
      [(and (char? key) (= (char->integer key) CTRL-F))
       (when (< (line-state-cursor state)
                (string-length (line-state-buffer state)))
         (line-state-cursor-set!
           state
           (+ (line-state-cursor state) 1))
         (refresh-line state out-port))]
      [(and (char? key) (= (char->integer key) CTRL-D))
       (if (= (string-length (line-state-buffer state)) 0)
           (begin
             (display "\r\n" out-port)
             (flush-output-port out-port)
             (line-state-done?-set! state #t)
             (line-state-result-set! state 'eof))
           (begin
             (buf-delete-char state)
             (refresh-line state out-port)))]
      [(and (char? key) (= (char->integer key) BACKSPACE))
       (buf-backspace state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-H))
       (buf-backspace state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-K))
       (buf-kill-to-end state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-U))
       (buf-kill-to-start state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-W))
       (buf-kill-word-back state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-Y))
       (let ([text (kill-ring-top state)])
         (when text
           (buf-insert-string state text)
           (refresh-line state out-port)))]
      [(and (char? key) (= (char->integer key) CTRL-T))
       (let ([fn (*fzf-files-fn*)])
         (if fn
             (begin
               (term-cooked! (current-input-port))
               (let ([result (guard (__exn [#t ((lambda (e) #f) __exn)])
                               (fn))])
                 (term-raw! (current-input-port))
                 (when result (buf-insert-string state result))))
             (buf-transpose-chars state)))
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-L))
       (term-clear-screen out-port)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-P))
       (history-prev state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-N))
       (history-next state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-R))
       (let ([fn (*fzf-history-fn*)])
         (if fn
             (let ([commands (history-unique-commands)])
               (when (pair? commands)
                 (term-cooked! (current-input-port))
                 (let ([result (guard (__exn [#t ((lambda (e) #f) __exn)])
                                 (fn commands))])
                   (term-raw! (current-input-port))
                   (when result
                     (save-undo! state)
                     (line-state-buffer-set! state (string-copy result))
                     (line-state-cursor-set!
                       state
                       (string-length result))))))
             (search-start state 'reverse)))
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-S))
       (search-start state 'forward)
       (refresh-search state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-G))
       (line-state-buffer-set! state "")
       (line-state-cursor-set! state 0)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-UNDERSCORE))
       (undo! state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-C))
       (display "^C\r\n" out-port)
       (flush-output-port out-port)
       (line-state-buffer-set! state "")
       (line-state-cursor-set! state 0)
       (line-state-done?-set! state #t)
       (line-state-result-set! state "")]
      [(and (char? key) (= (char->integer key) TAB-KEY))
       (handle-tab-completion state out-port)]
      [(eq? key 'up)
       (history-prev state)
       (refresh-line state out-port)]
      [(eq? key 'down)
       (history-next state)
       (refresh-line state out-port)]
      [(eq? key 'left)
       (when (> (line-state-cursor state) 0)
         (line-state-cursor-set!
           state
           (- (line-state-cursor state) 1))
         (refresh-line state out-port))]
      [(eq? key 'right)
       (when (< (line-state-cursor state)
                (string-length (line-state-buffer state)))
         (line-state-cursor-set!
           state
           (+ (line-state-cursor state) 1))
         (refresh-line state out-port))]
      [(eq? key 'home)
       (line-state-cursor-set! state 0)
       (refresh-line state out-port)]
      [(eq? key 'end)
       (line-state-cursor-set!
         state
         (string-length (line-state-buffer state)))
       (refresh-line state out-port)]
      [(eq? key 'delete)
       (buf-delete-char state)
       (refresh-line state out-port)]
      [(and (list? key) (eq? (car key) 'alt))
       (handle-alt-key state (cadr key) out-port)]
      [else (%%void)]))
  (define (handle-alt-key state ch out-port)
    (cond
      [(or (char=? ch #\b) (char=? ch #\B))
       (line-state-cursor-set!
         state
         (word-backward-pos
           (line-state-buffer state)
           (line-state-cursor state)))
       (refresh-line state out-port)]
      [(or (char=? ch #\f) (char=? ch #\F))
       (line-state-cursor-set!
         state
         (word-forward-pos
           (line-state-buffer state)
           (line-state-cursor state)))
       (refresh-line state out-port)]
      [(or (char=? ch #\d) (char=? ch #\D))
       (buf-kill-word-forward state)
       (refresh-line state out-port)]
      [(or (char=? ch #\u) (char=? ch #\U))
       (word-transform! state char-upcase)
       (refresh-line state out-port)]
      [(or (char=? ch #\l) (char=? ch #\L))
       (word-transform! state char-downcase)
       (refresh-line state out-port)]
      [(or (char=? ch #\c) (char=? ch #\C))
       (let ([fn (*fzf-dirs-fn*)])
         (if fn
             (begin
               (term-cooked! (current-input-port))
               (let ([result (guard (__exn [#t ((lambda (e) #f) __exn)])
                               (fn))])
                 (term-raw! (current-input-port))
                 (when result
                   (save-undo! state)
                   (line-state-buffer-set!
                     state
                     (string-append "cd " result))
                   (line-state-cursor-set!
                     state
                     (+ 3 (string-length result))))))
             (word-capitalize! state)))
       (refresh-line state out-port)]
      [(char=? ch #\<)
       (let ([count (history-count)])
         (when (> count 0)
           (unless (line-state-history-index state)
             (line-state-saved-line-set!
               state
               (string-copy (line-state-buffer state))))
           (let ([entry (history-get 1)])
             (when entry
               (line-state-history-index-set! state count)
               (line-state-buffer-set! state (string-copy entry))
               (line-state-cursor-set! state (string-length entry))
               (refresh-line state out-port)))))]
      [(char=? ch #\>)
       (when (line-state-history-index state)
         (line-state-history-index-set! state #f)
         (line-state-buffer-set!
           state
           (string-copy (or (line-state-saved-line state) "")))
         (line-state-cursor-set!
           state
           (string-length (line-state-buffer state)))
         (refresh-line state out-port))]
      [(char=? ch #\.)
       (let ([prev (history-get-relative 0)])
         (when prev
           (let ([words (simple-split prev)])
             (when (pair? words)
               (let ([last-word (car (last-pair words))])
                 (buf-insert-string state last-word)
                 (refresh-line state out-port))))))]
      [else (%%void)]))
  (define (word-transform! state transform-fn)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([end (word-forward-pos buf pos)])
          (let* ([new-buf (string-copy buf)])
            (save-undo! state)
            (let loop ([i pos])
              (when (< i end)
                (string-set!
                  new-buf
                  i
                  (transform-fn (string-ref new-buf i)))
                (loop (+ i 1))))
            (line-state-buffer-set! state new-buf)
            (line-state-cursor-set! state end))))))
  (define (word-capitalize! state)
    (let* ([buf (line-state-buffer state)])
      (let* ([pos (line-state-cursor state)])
        (let* ([end (word-forward-pos buf pos)])
          (let* ([new-buf (string-copy buf)])
            (save-undo! state)
            (let loop ([i pos] [first? #t])
              (when (< i end)
                (let ([ch (string-ref new-buf i)])
                  (cond
                    [(char-alphabetic? ch)
                     (string-set!
                       new-buf
                       i
                       (if first? (char-upcase ch) (char-downcase ch)))
                     (loop (+ i 1) #f)]
                    [else (loop (+ i 1) first?)]))))
            (line-state-buffer-set! state new-buf)
            (line-state-cursor-set! state end))))))
  (define (handle-search-key state key out-port)
    (cond
      [(and (char? key) (= (char->integer key) CTRL-R))
       (refresh-search state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-S))
       (refresh-search state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-G))
       (search-cancel state)
       (refresh-line state out-port)]
      [(enter-key? key)
       (search-accept state)
       (display "\r\n" out-port)
       (flush-output-port out-port)
       (line-state-done?-set! state #t)
       (line-state-result-set! state (line-state-buffer state))]
      [(and (char? key)
            (or (= (char->integer key) BACKSPACE)
                (= (char->integer key) CTRL-H)))
       (let ([s (line-state-search-string state)])
         (when (> (string-length s) 0)
           (line-state-search-string-set!
             state
             (substring s 0 (- (string-length s) 1)))
           (search-update state)))
       (refresh-search state out-port)]
      [(or (eq? key 'escape) (symbol? key))
       (search-accept state)
       (refresh-line state out-port)]
      [(and (char? key)
            (>= (char->integer key) 32)
            (not (= (char->integer key) 127)))
       (line-state-search-string-set!
         state
         (string-append
           (line-state-search-string state)
           (string key)))
       (search-update state)
       (refresh-search state out-port)]
      [else
       (search-accept state)
       (handle-emacs-key state key out-port)]))
  (define (handle-tab-completion state out-port)
    (let ([fn (line-state-complete-fn state)])
      (when fn
        (let ([completions (fn (line-state-buffer state)
                               (line-state-cursor state))])
          (cond
            [(null? completions) (%%void)]
            [(= (length completions) 1)
             (let ([completion (car completions)]
                   [word-start (find-word-start-for-completion
                                 (line-state-buffer state)
                                 (line-state-cursor state))])
               (save-undo! state)
               (line-state-buffer-set!
                 state
                 (string-append
                   (substring (line-state-buffer state) 0 word-start)
                   completion
                   " "
                   (substring
                     (line-state-buffer state)
                     (line-state-cursor state)
                     (string-length (line-state-buffer state)))))
               (line-state-cursor-set!
                 state
                 (+ word-start (string-length completion) 1))
               (refresh-line state out-port))]
            [else
             (let* ([prefix (common-prefix completions)])
               (let* ([word-start (find-word-start-for-completion
                                    (line-state-buffer state)
                                    (line-state-cursor state))])
                 (when (> (string-length prefix)
                          (- (line-state-cursor state) word-start))
                   (save-undo! state)
                   (line-state-buffer-set!
                     state
                     (string-append
                       (substring (line-state-buffer state) 0 word-start)
                       prefix
                       (substring
                         (line-state-buffer state)
                         (line-state-cursor state)
                         (string-length (line-state-buffer state)))))
                   (line-state-cursor-set!
                     state
                     (+ word-start (string-length prefix))))
                 (display "\r\n" out-port)
                 (display-completions
                   completions
                   out-port
                   (line-state-columns state))
                 (refresh-line state out-port)))])))))
  (define (find-word-start-for-completion buf cursor)
    (let loop ([i (- cursor 1)])
      (if (or (< i 0)
              (char-whitespace? (string-ref buf i))
              (char=? (string-ref buf i) #\/)
              (char=? (string-ref buf i) #\=))
          (+ i 1)
          (loop (- i 1)))))
  (define (common-prefix strings)
    (if (null? strings)
        ""
        (let ([first (car strings)])
          (let loop ([len (string-length first)])
            (if (<= len 0)
                ""
                (let ([prefix (substring first 0 len)])
                  (if (every?
                        (lambda (s)
                          (and (>= (string-length s) len)
                               (string=? (substring s 0 len) prefix)))
                        strings)
                      prefix
                      (loop (- len 1)))))))))
  (define (every? pred lst)
    (cond
      [(null? lst) #t]
      [(pred (car lst)) (every? pred (cdr lst))]
      [else #f]))
  (define (display-completions completions port columns)
    (let* ([max-len (apply
                      max
                      (map string-length completions))])
      (let* ([col-width (+ max-len 2)])
        (let* ([ncols (max 1 (quotient columns col-width))])
          (let loop ([items completions] [col 0])
            (unless (null? items)
              (let ([item (car items)])
                (display item port)
                (if (= (+ col 1) ncols)
                    (begin (display "\r\n" port) (loop (cdr items) 0))
                    (begin
                      (display
                        (make-string
                          (- col-width (string-length item))
                          #\space)
                        port)
                      (loop (cdr items) (+ col 1)))))))
          (when (and (pair? completions)
                     (not (= 0 (modulo (length completions) ncols))))
            (display "\r\n" port))
          (flush-output-port port)))))
  (define (handle-vi-key state key out-port)
    (if (eq? (line-state-vi-mode state) 'insert)
        (handle-vi-insert state key out-port)
        (handle-vi-command state key out-port)))
  (define (handle-vi-insert state key out-port)
    (cond
      [(eq? key 'escape)
       (line-state-vi-mode-set! state 'command)
       (when (> (line-state-cursor state) 0)
         (line-state-cursor-set!
           state
           (- (line-state-cursor state) 1)))
       (refresh-line state out-port)]
      [(enter-key? key)
       (display "\r\n" out-port)
       (flush-output-port out-port)
       (line-state-done?-set! state #t)
       (line-state-result-set! state (line-state-buffer state))]
      [(and (char? key)
            (or (= (char->integer key) BACKSPACE)
                (= (char->integer key) CTRL-H)))
       (buf-backspace state)
       (refresh-line state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-C))
       (display "^C\r\n" out-port)
       (flush-output-port out-port)
       (line-state-buffer-set! state "")
       (line-state-cursor-set! state 0)
       (line-state-done?-set! state #t)
       (line-state-result-set! state "")]
      [(and (char? key) (= (char->integer key) TAB-KEY))
       (handle-tab-completion state out-port)]
      [(and (char? key) (= (char->integer key) CTRL-D))
       (if (= (string-length (line-state-buffer state)) 0)
           (begin
             (display "\r\n" out-port)
             (flush-output-port out-port)
             (line-state-done?-set! state #t)
             (line-state-result-set! state 'eof))
           (begin
             (buf-delete-char state)
             (refresh-line state out-port)))]
      [(and (char? key)
            (>= (char->integer key) 32)
            (not (= (char->integer key) 127)))
       (buf-insert state key)
       (refresh-line state out-port)]
      [else (%%void)]))
  (define (handle-vi-command state key out-port)
    (cond
      [(and (char? key) (char=? key #\h))
       (when (> (line-state-cursor state) 0)
         (line-state-cursor-set!
           state
           (- (line-state-cursor state) 1))
         (refresh-line state out-port))]
      [(and (char? key) (char=? key #\l))
       (when (< (line-state-cursor state)
                (- (string-length (line-state-buffer state)) 1))
         (line-state-cursor-set!
           state
           (+ (line-state-cursor state) 1))
         (refresh-line state out-port))]
      [(and (char? key) (char=? key #\w))
       (line-state-cursor-set!
         state
         (word-forward-pos
           (line-state-buffer state)
           (line-state-cursor state)))
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\b))
       (line-state-cursor-set!
         state
         (word-backward-pos
           (line-state-buffer state)
           (line-state-cursor state)))
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\0))
       (line-state-cursor-set! state 0)
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\$))
       (let ([len (string-length (line-state-buffer state))])
         (line-state-cursor-set! state (max 0 (- len 1)))
         (refresh-line state out-port))]
      [(and (char? key) (char=? key #\i))
       (line-state-vi-mode-set! state 'insert)]
      [(and (char? key) (char=? key #\a))
       (when (< (line-state-cursor state)
                (string-length (line-state-buffer state)))
         (line-state-cursor-set!
           state
           (+ (line-state-cursor state) 1)))
       (line-state-vi-mode-set! state 'insert)]
      [(and (char? key) (char=? key #\A))
       (line-state-cursor-set!
         state
         (string-length (line-state-buffer state)))
       (line-state-vi-mode-set! state 'insert)]
      [(and (char? key) (char=? key #\I))
       (line-state-cursor-set! state 0)
       (line-state-vi-mode-set! state 'insert)]
      [(and (char? key) (char=? key #\x))
       (buf-delete-char state)
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\D))
       (buf-kill-to-end state)
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\C))
       (buf-kill-to-end state)
       (line-state-vi-mode-set! state 'insert)
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\k))
       (history-prev state)
       (refresh-line state out-port)]
      [(and (char? key) (char=? key #\j))
       (history-next state)
       (refresh-line state out-port)]
      [(eq? key 'up)
       (history-prev state)
       (refresh-line state out-port)]
      [(eq? key 'down)
       (history-next state)
       (refresh-line state out-port)]
      [(enter-key? key)
       (display "\r\n" out-port)
       (flush-output-port out-port)
       (line-state-done?-set! state #t)
       (line-state-result-set! state (line-state-buffer state))]
      [else (%%void)]))
  (define (simple-split str) (string-split-chars str #\space))
  (define (user-name) (or (getenv "USER" #f) "user"))
  (define line-edit
    (case-lambda
      [(prompt)
       (let* ([complete-fn #f] [mode 'emacs])
         (let ([in-port (current-input-port)]
               [out-port (current-error-port)])
           (if (not (guard (__exn [#t ((lambda (e) #f) __exn)])
                      (tty? in-port)))
               (begin
                 (display prompt out-port)
                 (flush-output-port out-port)
                 (let ([line (get-line in-port)])
                   (if (eof-object? line) 'eof line)))
               (let* ([cols (detect-terminal-columns)])
                 (let* ([pw (visible-width-last-line prompt)])
                   (set-terminal-columns! cols)
                   (let ([state (make-line-state "" 0 prompt pw #f "" (list) (list) #f ""
                                  #f cols #f mode 'insert complete-fn #f
                                  #f)])
                     (dynamic-wind
                       (lambda () (term-raw! in-port))
                       (lambda ()
                         (display "\n" out-port)
                         (flush-output-port out-port)
                         (refresh-line state out-port)
                         (let loop ()
                           (let ([key (read-key in-port)])
                             (if (eq? key 'eof)
                                 (begin
                                   (line-state-done?-set! state #t)
                                   (line-state-result-set! state 'eof)
                                   'eof)
                                 (begin
                                   (if (eq? mode 'vi)
                                       (handle-vi-key state key out-port)
                                       (handle-emacs-key
                                         state
                                         key
                                         out-port))
                                   (if (line-state-done? state)
                                       (line-state-result state)
                                       (loop)))))))
                       (lambda () (term-cooked! in-port)))))))))]
      [(prompt complete-fn)
       (let* ([mode 'emacs])
         (let ([in-port (current-input-port)]
               [out-port (current-error-port)])
           (if (not (guard (__exn [#t ((lambda (e) #f) __exn)])
                      (tty? in-port)))
               (begin
                 (display prompt out-port)
                 (flush-output-port out-port)
                 (let ([line (get-line in-port)])
                   (if (eof-object? line) 'eof line)))
               (let* ([cols (detect-terminal-columns)])
                 (let* ([pw (visible-width-last-line prompt)])
                   (set-terminal-columns! cols)
                   (let ([state (make-line-state "" 0 prompt pw #f "" (list) (list) #f ""
                                  #f cols #f mode 'insert complete-fn #f
                                  #f)])
                     (dynamic-wind
                       (lambda () (term-raw! in-port))
                       (lambda ()
                         (display "\n" out-port)
                         (flush-output-port out-port)
                         (refresh-line state out-port)
                         (let loop ()
                           (let ([key (read-key in-port)])
                             (if (eq? key 'eof)
                                 (begin
                                   (line-state-done?-set! state #t)
                                   (line-state-result-set! state 'eof)
                                   'eof)
                                 (begin
                                   (if (eq? mode 'vi)
                                       (handle-vi-key state key out-port)
                                       (handle-emacs-key
                                         state
                                         key
                                         out-port))
                                   (if (line-state-done? state)
                                       (line-state-result state)
                                       (loop)))))))
                       (lambda () (term-cooked! in-port)))))))))]
      [(prompt complete-fn mode)
       (let ([in-port (current-input-port)]
             [out-port (current-error-port)])
         (if (not (guard (__exn [#t ((lambda (e) #f) __exn)])
                    (tty? in-port)))
             (begin
               (display prompt out-port)
               (flush-output-port out-port)
               (let ([line (get-line in-port)])
                 (if (eof-object? line) 'eof line)))
             (let* ([cols (detect-terminal-columns)])
               (let* ([pw (visible-width-last-line prompt)])
                 (set-terminal-columns! cols)
                 (let ([state (make-line-state "" 0 prompt pw #f "" (list) (list) #f "" #f
                                cols #f mode 'insert complete-fn #f #f)])
                   (dynamic-wind
                     (lambda () (term-raw! in-port))
                     (lambda ()
                       (display "\n" out-port)
                       (flush-output-port out-port)
                       (refresh-line state out-port)
                       (let loop ()
                         (let ([key (read-key in-port)])
                           (if (eq? key 'eof)
                               (begin
                                 (line-state-done?-set! state #t)
                                 (line-state-result-set! state 'eof)
                                 'eof)
                               (begin
                                 (if (eq? mode 'vi)
                                     (handle-vi-key state key out-port)
                                     (handle-emacs-key state key out-port))
                                 (if (line-state-done? state)
                                     (line-state-result state)
                                     (loop)))))))
                     (lambda () (term-cooked! in-port))))))))]))
  (define (tty? port)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (tty-mode-set! port #t #t #f #f 0)
      #t))
  (define (visible-width str)
    (let ([len (string-length str)])
      (let loop ([i 0] [width 0] [in-escape? #f])
        (cond
          [(>= i len) width]
          [(and (not in-escape?) (char=? (string-ref str i) #\esc))
           (loop (+ i 1) width #t)]
          [in-escape?
           (if (char-alphabetic? (string-ref str i))
               (loop (+ i 1) width #f)
               (loop (+ i 1) width #t))]
          [else (loop (+ i 1) (+ width 1) #f)]))))
  (define (visible-width-last-line str)
    (let* ([len (string-length str)])
      (let* ([last-newline (let loop ([i (- len 1)])
                             (cond
                               [(< i 0) -1]
                               [(char=? (string-ref str i) #\newline) i]
                               [else (loop (- i 1))]))])
        (let* ([start (+ last-newline 1)])
          (let loop ([i start] [width 0] [in-escape? #f])
            (cond
              [(>= i len) width]
              [(and (not in-escape?) (char=? (string-ref str i) #\esc))
               (loop (+ i 1) width #t)]
              [in-escape?
               (if (char-alphabetic? (string-ref str i))
                   (loop (+ i 1) width #f)
                   (loop (+ i 1) width #t))]
              [else (loop (+ i 1) (+ width 1) #f)]))))))
  (define (count-newlines str)
    (let ([len (string-length str)])
      (let loop ([i 0] [count 0])
        (cond
          [(>= i len) count]
          [(char=? (string-ref str i) #\newline)
           (loop (+ i 1) (+ count 1))]
          [else (loop (+ i 1) count)])))))
