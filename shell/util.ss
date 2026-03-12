;;; util.ss — Shared utilities for jsh

(export #t)
(import :std/misc/string
        :std/misc/list
        :std/misc/path
        ./pregexp-compat
        :std/format
        :std/sugar
        :std/iter
        :std/error
        :jsh/ffi)

;;; --- open-process string encoding ---

;; Encode a Scheme string to C-safe Latin-1 for Gambit's open-process.
;; Gambit can't convert chars > 127 to C char-strings.
;; Converts unicode chars to their UTF-8 byte representation as Latin-1 chars.
(def (string->c-safe s)
  (let ((len (string-length s)))
    (let check ((i 0))
      (cond ((>= i len) s)
            ((< (char->integer (string-ref s i)) 128) (check (+ i 1)))
            (else
             (let* ((u8 (string->bytes s))
                    (u8len (u8vector-length u8))
                    (out (make-string u8len)))
               (let loop ((j 0))
                 (if (>= j u8len) out
                     (begin (string-set! out j (integer->char (u8vector-ref u8 j)))
                            (loop (+ j 1)))))))))))

;;; --- waitpid status decoders (shell-level) ---

;; Decode raw waitpid status to a shell exit code (0-255)
(def (status->exit-code raw-status)
  (cond
    ((WIFEXITED raw-status)
     (WEXITSTATUS raw-status))
    ((WIFSIGNALED raw-status)
     (+ 128 (WTERMSIG raw-status)))
    ((WIFSTOPPED raw-status)
     (+ 128 (WSTOPSIG raw-status)))
    (else 255)))

;;; --- Process launching ---

;; Launch an external command via open-process, ensuring SIGPIPE is
;; unblocked for the child.  Gambit blocks SIGPIPE (sigprocmask),
;; which children inherit through fork+exec, causing commands like
;; cat to get EPIPE (exit 1) instead of being killed by SIGPIPE (exit 141).
(def (open-process-with-sigpipe settings)
  (ffi-sigpipe-unblock)
  (let ((proc (open-process settings)))
    (ffi-sigpipe-block)
    proc))

;;; --- SOH packing for ffi-execve / ffi-fork-exec ---

;; Pack a list of strings with SOH (char 1) delimiter
(def *soh* (make-string 1 (integer->char 1)))
(def (pack-with-soh strs)
  (if (null? strs) ""
      (call-with-output-string
        (lambda (port)
          (display (car strs) port)
          (for-each (lambda (s)
                      (display *soh* port)
                      (display s port))
                    (cdr strs))))))

;; Pack a list of integers as SOH-delimited string of their decimal representations
(def (pack-fds-with-soh fds)
  (pack-with-soh (map number->string fds)))

;;; --- String utilities ---

(def (string-trim-whitespace str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))

;; Split string by any character in charset (a string of delimiters)
(def (string-split-chars str charset)
  (let loop ((i 0) (start 0) (result []))
    (cond
      ((>= i (string-length str))
       (reverse (if (> i start)
                  (cons (substring str start i) result)
                  result)))
      ((string-index charset (string-ref str i))
       (loop (+ i 1) (+ i 1)
             (if (> i start)
               (cons (substring str start i) result)
               result)))
      (else
       (loop (+ i 1) start result)))))

;; Check if char is in string
(def (string-index str ch)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

;; Substring test
(def (string-contains? haystack needle)
  (let ((nlen (string-length needle))
        (hlen (string-length haystack)))
    (if (> nlen hlen) #f
        (let loop ((i 0))
          (cond
            ((> (+ i nlen) hlen) #f)
            ((string=? (substring haystack i (+ i nlen)) needle) #t)
            (else (loop (+ i 1))))))))

;; Join strings with separator (O(n) via output port)
(def (string-join lst sep)
  (if (null? lst) ""
      (call-with-output-string
        (lambda (port)
          (display (car lst) port)
          (for-each (lambda (s)
                      (display sep port)
                      (display s port))
                    (cdr lst))))))

;; Find last occurrence of character in string
(def (string-last-index-of str ch)
  (let loop ((i (- (string-length str) 1)))
    (cond
      ((< i 0) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (- i 1))))))

;;; --- Path / filesystem utilities ---

;; Search PATH for an executable, return full path or #f
(def (which name)
  (if (string-contains? name "/")
    ;; Absolute or relative path — resolve to absolute for the executability
    ;; check (ffi-access needs absolute path since Gambit's current-directory
    ;; doesn't match OS cwd), but return original name for display
    (let ((resolved (path-expand name)))
      (and (file-exists? resolved) (executable? resolved) name))
    ;; Search PATH
    (let ((path-dirs (string-split-chars (or (getenv "PATH" #f) "/usr/bin:/bin") ":")))
      (let loop ((dirs path-dirs))
        (if (null? dirs) #f
            (let ((full (string-append (car dirs) "/" name)))
              (if (and (file-exists? full) (executable? full))
                full
                (loop (cdr dirs)))))))))

;; Search PATH for a regular file (not directory), no execute requirement
;; Used by source/. to find files to source
;; Optional path-str parameter overrides getenv lookup (for temp env assignments)
(def (find-file-in-path name (path-str #f))
  (let ((path-dirs (string-split-chars (or path-str (getenv "PATH" #f) "/usr/bin:/bin") ":")))
    (let loop ((dirs path-dirs))
      (if (null? dirs) #f
          (let ((full (string-append (car dirs) "/" name)))
            (if (and (file-exists? full)
                     (not (file-directory? full)))
              full
              (loop (cdr dirs))))))))

;; Check if file is executable (by current user)
(def (executable? path)
  (and (not (file-directory? path))
       (with-catch
        (lambda (e) #f)
        (lambda () (= (ffi-access path 1) 0)))))  ;; X_OK = 1

;; Return home directory
(def (home-directory)
  (or (getenv "HOME" #f)
      (with-catch
       (lambda (e) "/")
       (lambda () (user-info-home (user-info (user-name)))))))

;; Expand ~ and ~user at the start of a path
(def (expand-tilde path)
  (cond
    ((string=? path "~") (home-directory))
    ((and (> (string-length path) 1)
          (char=? (string-ref path 0) #\~)
          (char=? (string-ref path 1) #\/))
     (string-append (home-directory) (substring path 1 (string-length path))))
    ((and (> (string-length path) 1)
          (char=? (string-ref path 0) #\~))
     ;; ~user form
     (let* ((slash-pos (string-index path #\/))
            (username (if slash-pos
                       (substring path 1 slash-pos)
                       (substring path 1 (string-length path)))))
       (with-catch
        (lambda (e) path) ;; return as-is if user not found
        (lambda ()
          (let ((home (user-info-home (user-info username))))
            (if slash-pos
              (string-append home (substring path slash-pos (string-length path)))
              home))))))
    (else path)))

;; Read all lines from a file
(def (read-file-lines path)
  (if (file-exists? path)
    (call-with-input-file path
      (lambda (port)
        (let loop ((lines []))
          (let ((line (read-line port)))
            (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines)))))))
    []))

;;; --- Exception utilities ---

;; Extract a message string from any exception
(def (exception-message e)
  (cond
    ((Error? e)
     (Error-message e))
    ((error-exception? e)
     (error-exception-message e))
    ((string? e) e)
    ;; os-exception from Gambit (e.g. write failure, EFBIG)
    ((os-exception? e)
     (call-with-output-string (lambda (p) (display-exception e p))))
    (else (call-with-output-string (lambda (p) (display-exception e p))))))

;; Run thunk, return result or #f on exception
(def (try-or-false thunk)
  (with-catch (lambda (e) #f) thunk))

;;; --- Misc ---

;; Safe substring that doesn't error on out-of-bounds
(def (safe-substring str start end)
  (let ((len (string-length str)))
    (substring str (min start len) (min end len))))

;; Repeat a character n times
(def (make-string-repeated ch n)
  (make-string n ch))

;; Check if a string is a valid shell name (variable name)
(def (valid-name? str)
  (and (> (string-length str) 0)
       (let ((ch (string-ref str 0)))
         (or (char-alphabetic? ch) (char=? ch #\_)))
       (let loop ((i 1))
         (or (>= i (string-length str))
             (let ((ch (string-ref str i)))
               (and (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))
                    (loop (+ i 1))))))))

;; Check if string is a valid integer
(def (string->integer str)
  (with-catch
   (lambda (e) #f)
   (lambda ()
     (let ((n (string->number str)))
       (and (integer? n) (exact? n) n)))))

(def (string-upcase str)
  (list->string (map char-upcase (string->list str))))

(def (string-downcase str)
  (list->string (map char-downcase (string->list str))))

;; Check if a string contains control characters (chars < 0x20 or 0x7f)
(def (string-has-control-chars? s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (>= i len) #f
        (let ((c (char->integer (string-ref s i))))
          (if (or (< c #x20) (= c #x7f))
            #t
            (loop (+ i 1))))))))

(def (hex-char? ch)
  (or (and (char>=? ch #\0) (char<=? ch #\9))
      (and (char>=? ch #\a) (char<=? ch #\f))
      (and (char>=? ch #\A) (char<=? ch #\F))))

;; Count UTF-8 code points in a string.
;; If the string contains actual unicode chars (char-integer > 127),
;; string-length is already correct. But if it contains raw bytes
;; from command substitution (Latin-1 encoding), we need to count
;; UTF-8 sequences by examining lead bytes.
(def (utf8-string-length str)
  (let ((len (string-length str)))
    ;; Check if string has any chars > 127 (raw bytes from ffi-read-all)
    (let check ((i 0))
      (if (>= i len)
        ;; All chars <= 127 — pure ASCII, string-length is correct
        len
        (let ((c (char->integer (string-ref str i))))
          (if (> c 127)
            ;; Contains high bytes — count UTF-8 sequences
            (utf8-count-codepoints str len)
            (check (+ i 1))))))))

;; Count UTF-8 code points by examining lead bytes
(def (utf8-count-codepoints str len)
  (let loop ((i 0) (count 0))
    (if (>= i len)
      count
      (let ((b (char->integer (string-ref str i))))
        (cond
          ((< b #x80) (loop (+ i 1) (+ count 1)))      ;; ASCII
          ((< b #xC0) (loop (+ i 1) count))             ;; continuation byte
          ((< b #xE0) (loop (+ i 1) (+ count 1)))       ;; 2-byte lead
          ((< b #xF0) (loop (+ i 1) (+ count 1)))       ;; 3-byte lead
          (else        (loop (+ i 1) (+ count 1))))))))

;; Count UTF-8 byte length of a Gambit Unicode string
(def (utf8-byte-count str)
  (let ((len (string-length str)))
    (let loop ((i 0) (bytes 0))
      (if (>= i len) bytes
        (let ((cp (char->integer (string-ref str i))))
          (loop (+ i 1) (+ bytes (cond ((< cp #x80) 1)
                                       ((< cp #x800) 2)
                                       ((< cp #x10000) 3)
                                       (else 4)))))))))

;;; --- File test helpers (for refactored builtins) ---

;; Test if path is a regular file
(def (file-regular? path)
  (and (file-exists? path)
       (with-catch
        (lambda (e) #f)
        (lambda () (eq? (file-info-type (file-info path)) 'regular)))))

;; Test if path is a directory
(def (file-directory? path)
  (and (file-exists? path)
       (with-catch
        (lambda (e) #f)
        (lambda () (eq? (file-info-type (file-info path)) 'directory)))))

;; Test if path is a symbolic link (don't follow symlinks)
(def (file-symlink? path)
  (with-catch
   (lambda (e) #f)
   (lambda () (eq? (file-info-type (file-info path #f)) 'symbolic-link))))

;; Test if path exists and is non-empty
(def (file-nonempty? path)
  (and (file-exists? path)
       (with-catch
        (lambda (e) #f)
        (lambda () (> (file-info-size (file-info path)) 0)))))

;; Test if path is readable
(def (file-readable? path)
  (with-catch
   (lambda (e) #f)
   (lambda () (= (ffi-access path 4) 0))))  ;; R_OK = 4

;; Test if path is writable
(def (file-writable? path)
  (with-catch
   (lambda (e) #f)
   (lambda () (= (ffi-access path 2) 0))))  ;; W_OK = 2

;; Note: file-executable? is already defined above as executable?

;;; --- Raw byte encoding for $'...' byte escapes ---
;;
;; Gerbil strings are Unicode. Byte values 128-255 from $'\NNN' and $'\xNN'
;; would be UTF-8 encoded on output (2 bytes instead of 1). We use Unicode
;; Private Use Area (U+E000-U+E0FF) to mark these as raw bytes. Output
;; functions detect PUA chars and write single bytes via write-u8.

(def raw-byte-base #xE000)

;; Encode a byte value (0-255) as a PUA character for raw byte output.
;; Values < 128 are returned as-is since UTF-8 encoding matches raw byte.
(def (byte->raw-char val)
  (if (>= val #x80)
    (integer->char (+ raw-byte-base val))
    (integer->char val)))

;; Check if a character is a PUA raw byte marker
(def (raw-byte-char? ch)
  (let ((code (char->integer ch)))
    (and (>= code raw-byte-base) (<= code (+ raw-byte-base #xFF)))))

;; Display a shell string, converting PUA raw byte markers to actual bytes.
;; Fast path: if no PUA chars, use regular display.
(def (shell-display str)
  (let ((len (string-length str)))
    (let scan ((i 0))
      (cond
        ((>= i len) (display str))  ;; no PUA chars found, fast path
        ((raw-byte-char? (string-ref str i))
         ;; Found PUA char, use slow path for whole string
         (shell-display-raw str))
        (else (scan (+ i 1)))))))

;; Slow path: display string with PUA chars converted to raw bytes
(def (shell-display-raw str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (when (< i len)
        (let* ((ch (string-ref str i))
               (code (char->integer ch)))
          (if (and (>= code raw-byte-base) (<= code (+ raw-byte-base #xFF)))
            (begin
              (force-output)
              (write-u8 (- code raw-byte-base))
              (force-output))
            (display ch)))
        (loop (+ i 1))))))
