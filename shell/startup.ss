;;; startup.ss — RC file loading and startup sequences for gsh
;;; Handles login/interactive/non-interactive startup file sourcing.

(export #t)
(import :std/sugar
        :std/format
        :gsh/util
        :gsh/environment
        :gsh/script)

;;; --- Public interface ---

;; Load startup files based on shell mode.
;; login?: #t for login shells (first char of $0 is - or --login flag)
;; interactive?: #t for interactive shells (stdin is tty)
(def (load-startup-files! env login? interactive?)
  (cond
    ;; Interactive login shell
    ((and login? interactive?)
     (source-if-exists! "/etc/profile" env)
     ;; First of ~/.gsh_profile, ~/.gsh_login, ~/.profile
     (let ((home (or (env-get env "HOME") (home-directory))))
       (or (source-if-exists! (string-append home "/.gsh_profile") env)
           (source-if-exists! (string-append home "/.gsh_login") env)
           (source-if-exists! (string-append home "/.profile") env))))
    ;; Interactive non-login shell
    (interactive?
     (let ((home (or (env-get env "HOME") (home-directory))))
       (source-if-exists! (string-append home "/.gshrc") env)))
    ;; Non-interactive (script)
    (else
     (let ((env-file (env-get env "GSH_ENV")))
       (when env-file
         (source-if-exists! env-file env))))))

;; Run logout sequence for login shells.
(def (run-logout! env)
  (let ((home (or (env-get env "HOME") (home-directory))))
    (source-if-exists! (string-append home "/.gsh_logout") env)))

;;; --- Helpers ---

(def (source-if-exists! path env)
  ;; Source a file if it exists; return #t if sourced, #f if not found.
  (if (file-exists? path)
    (begin
      (source-file! path env)
      #t)
    #f))
