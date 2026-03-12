;;; prompt.ss — Prompt expansion (PS1/PS2/PS4) for gsh

(export #t)
(import :std/sugar
        :std/format
        :gsh/ffi
        :gsh/util
        (only-in :gsh/expander find-matching-paren))

;;; --- Public interface ---

;; Expand prompt escape sequences in a PS string
;; env-get: (lambda (name) -> string or #f)
;; job-count: number of active jobs
;; cmd-number: command number
;; history-number: history number
;; cmd-exec-fn: optional (lambda (cmd-string) -> output-string) for $(...) expansion
(def (expand-prompt ps-string env-get
                    (job-count 0)
                    (cmd-number 0)
                    (history-number 0)
                    (cmd-exec-fn #f))
  (let ((len (string-length ps-string))
        (out (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len)
         (get-output-string out))
        ;; Command substitution $(...)
        ((and cmd-exec-fn
              (char=? (string-ref ps-string i) #\$)
              (< (+ i 1) len)
              (char=? (string-ref ps-string (+ i 1)) #\())
         (let ((close (find-matching-paren ps-string (+ i 2))))
           (if close
             (let* ((cmd-str (substring ps-string (+ i 2) close))
                    (output (with-catch
                             (lambda (e) "")  ;; Silently ignore errors in prompt commands
                             (lambda () (cmd-exec-fn cmd-str)))))
               (display output out)
               (loop (+ close 1)))
             ;; No matching ) - output literally
             (begin
               (display "$(" out)
               (loop (+ i 2))))))
        ;; Backslash escape sequence
        ((and (char=? (string-ref ps-string i) #\\)
              (< (+ i 1) len))
         (let ((ch (string-ref ps-string (+ i 1))))
           (case ch
             ;; Username
             ((#\u)
              (display (or (env-get "USER") (user-name)) out)
              (loop (+ i 2)))
             ;; Hostname (short)
             ((#\h)
              (let* ((host (or (env-get "HOSTNAME")
                              (with-catch (lambda (e) "localhost")
                                          (lambda () (hostname-short)))))
                     (dot (string-index host #\.)))
                (display (if dot (substring host 0 dot) host) out)
                (loop (+ i 2))))
             ;; Hostname (full)
             ((#\H)
              (display (or (env-get "HOSTNAME")
                          (with-catch (lambda (e) "localhost")
                                      (lambda () (hostname-short))))
                       out)
              (loop (+ i 2)))
             ;; Working directory with ~ for home
             ((#\w)
              (let* ((pwd (or (env-get "PWD") (current-directory)))
                     (home (or (env-get "HOME") ""))
                     (display-pwd (if (and (> (string-length home) 0)
                                           (string-prefix? home pwd))
                                   (string-append "~" (substring pwd (string-length home)
                                                                  (string-length pwd)))
                                   pwd)))
                (display display-pwd out)
                (loop (+ i 2))))
             ;; Basename of working directory
             ((#\W)
              (let* ((pwd (or (env-get "PWD") (current-directory)))
                     (home (or (env-get "HOME") "")))
                (if (string=? pwd home)
                  (display "~" out)
                  (display (path-basename pwd) out))
                (loop (+ i 2))))
             ;; Date
             ((#\d)
              ;; Simplified: just show date
              (display (date-string) out)
              (loop (+ i 2)))
             ;; Time formats
             ((#\t)  ;; 24h HH:MM:SS
              (display (time-string-24h) out)
              (loop (+ i 2)))
             ((#\T)  ;; 12h HH:MM:SS
              (display (time-string-12h) out)
              (loop (+ i 2)))
             ((#\@)  ;; 12h am/pm
              (display (time-string-ampm) out)
              (loop (+ i 2)))
             ((#\A)  ;; 24h HH:MM
              (display (time-string-hhmm) out)
              (loop (+ i 2)))
             ;; Newline / carriage return
             ((#\n) (display "\n" out) (loop (+ i 2)))
             ((#\r) (display "\r" out) (loop (+ i 2)))
             ;; Shell name
             ((#\s)
              (display "gsh" out)
              (loop (+ i 2)))
             ;; Shell version
             ((#\v)
              (display "0.1" out)
              (loop (+ i 2)))
             ((#\V)
              (display "0.1.0" out)
              (loop (+ i 2)))
             ;; Number of jobs
             ((#\j)
              (display (number->string job-count) out)
              (loop (+ i 2)))
             ;; Terminal basename
             ((#\l)
              (display "tty" out)
              (loop (+ i 2)))
             ;; Command number
             ((#\#)
              (display (number->string cmd-number) out)
              (loop (+ i 2)))
             ;; History number
             ((#\!)
              (display (number->string history-number) out)
              (loop (+ i 2)))
             ;; $ or # (root check)
             ((#\$)
              (display (if (= (ffi-geteuid) 0) "#" "$") out)
              (loop (+ i 2)))
             ;; Bell
             ((#\a)
              (display "\007" out)
              (loop (+ i 2)))
             ;; Literal backslash
             ((#\\)
              (display "\\" out)
              (loop (+ i 2)))
             ;; strftime format \D{format}
             ((#\D)
              (if (and (< (+ i 2) len) (char=? (string-ref ps-string (+ i 2)) #\{))
                (let ((close (string-index-from ps-string #\} (+ i 3))))
                  (if close
                    (begin
                      ;; Simplified: just show ISO date
                      (display (date-string) out)
                      (loop (+ close 1)))
                    (begin
                      (display "\\D" out)
                      (loop (+ i 2)))))
                (begin
                  (display "\\D" out)
                  (loop (+ i 2)))))
             ;; Non-printing delimiters (bash \[ \]) — skip
             ((#\[) (loop (+ i 2)))
             ((#\]) (loop (+ i 2)))
             ;; Unknown escape: output literally
             (else
              (display "\\" out)
              (display (string ch) out)
              (loop (+ i 2))))))
        ;; Regular character
        (else
         (display (string (string-ref ps-string i)) out)
         (loop (+ i 1)))))))

;; Calculate visible width of a prompt (excluding \[...\] non-printing sequences)
(def (prompt-width prompt-string)
  (let ((len (string-length prompt-string)))
    (let loop ((i 0) (width 0) (in-escape? #f))
      (cond
        ((>= i len) width)
        ;; ANSI escape sequence: \e[...m
        ((and (not in-escape?)
              (char=? (string-ref prompt-string i) #\escape))
         (loop (+ i 1) width #t))
        (in-escape?
         (if (char-alphabetic? (string-ref prompt-string i))
           (loop (+ i 1) width #f)  ;; end of escape
           (loop (+ i 1) width #t)))
        (else
         (loop (+ i 1) (+ width 1) #f))))))

;;; --- Time/date helpers ---

(def (current-time-values)
  ;; Returns (seconds minutes hours day month year weekday)
  ;; Using Gambit's time->seconds and manual calculation
  (let* ((t (time->seconds (current-time)))
         (secs (inexact->exact (floor t))))
    ;; Simple approach: use process to get date components
    ;; For now, return approximate values
    (values (modulo secs 60)
            (modulo (quotient secs 60) 60)
            (modulo (quotient secs 3600) 24)
            0 0 0 0)))

(def (time-string-24h)
  (let-values (((s m h d mo y wd) (current-time-values)))
    (format "~2,'0d:~2,'0d:~2,'0d" h m s)))

(def (time-string-12h)
  (let-values (((s m h d mo y wd) (current-time-values)))
    (let ((h12 (cond ((= h 0) 12) ((> h 12) (- h 12)) (else h))))
      (format "~2,'0d:~2,'0d:~2,'0d" h12 m s))))

(def (time-string-ampm)
  (let-values (((s m h d mo y wd) (current-time-values)))
    (let ((h12 (cond ((= h 0) 12) ((> h 12) (- h 12)) (else h)))
          (ampm (if (>= h 12) "PM" "AM")))
      (format "~2,'0d:~2,'0d ~a" h12 m ampm))))

(def (time-string-hhmm)
  (let-values (((s m h d mo y wd) (current-time-values)))
    (format "~2,'0d:~2,'0d" h m)))

(def (date-string)
  ;; Simplified date string
  (let-values (((s m h d mo y wd) (current-time-values)))
    (format "~a" (seconds->date-string (time->seconds (current-time))))))

(def (seconds->date-string secs)
  ;; Very simplified - will be improved later with proper date library
  (with-catch
   (lambda (e) "???")
   (lambda ()
     (let ((port (open-input-process [path: "/bin/date" arguments: ["+%a %b %d"]])))
       (let ((result (read-line port)))
         (close-port port)
         (if (string? result) result "???"))))))

(def (hostname-short)
  (with-catch
   (lambda (e) "localhost")
   (lambda ()
     (let ((port (open-input-process [path: "/bin/hostname"])))
       (let ((result (read-line port)))
         (close-port port)
         (if (string? result) result "localhost"))))))

(def (path-basename path)
  (let loop ((i (- (string-length path) 1)))
    (cond
      ((< i 0) path)
      ((char=? (string-ref path i) #\/)
       (substring path (+ i 1) (string-length path)))
      (else (loop (- i 1))))))

(def (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(def (string-index-from str ch start)
  (let loop ((i start))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

