;;; -*- Gerbil -*-
;;; gerbil-coreutils integration — register coreutils commands as shell builtins
;;;
;;; The coreutils main functions call `exit` on errors (via `die` and `call-with-getopt`).
;;; Since Gambit's `exit` hard-terminates the process, we intercept it by temporarily
;;; overriding the global `exit` binding with a continuation escape during each invocation.

(export register-coreutils!)

(import :gsh/registry
        ;; Each coreutils module exports `main`. Use rename-in to avoid conflicts
        ;; and ensure runtime loading (only-in without names doesn't link modules).
        ;;
        ;; Skipped (native gsh builtins): true, false, echo, printf, pwd, test, kill
        ;;
        ;; --- Phase 1: Trivial ---
        (rename-in :gerbil-coreutils/basename (main cu-basename))
        (rename-in :gerbil-coreutils/dirname  (main cu-dirname))
        (rename-in :gerbil-coreutils/link     (main cu-link))
        (rename-in :gerbil-coreutils/unlink   (main cu-unlink))
        (rename-in :gerbil-coreutils/yes      (main cu-yes))
        (rename-in :gerbil-coreutils/printenv (main cu-printenv))
        (rename-in :gerbil-coreutils/sleep    (main cu-sleep))
        (rename-in :gerbil-coreutils/whoami   (main cu-whoami))
        (rename-in :gerbil-coreutils/logname  (main cu-logname))
        (rename-in :gerbil-coreutils/hostname (main cu-hostname))
        (rename-in :gerbil-coreutils/nproc    (main cu-nproc))
        (rename-in :gerbil-coreutils/tty      (main cu-tty))
        (rename-in :gerbil-coreutils/sync     (main cu-sync))
        (rename-in :gerbil-coreutils/hostid   (main cu-hostid))
        ;; --- Phase 2: Text Filters ---
        (rename-in :gerbil-coreutils/cat      (main cu-cat))
        (rename-in :gerbil-coreutils/head     (main cu-head))
        (rename-in :gerbil-coreutils/tail     (main cu-tail))
        (rename-in :gerbil-coreutils/tac      (main cu-tac))
        (rename-in :gerbil-coreutils/tee      (main cu-tee))
        (rename-in :gerbil-coreutils/wc       (main cu-wc))
        (rename-in :gerbil-coreutils/nl       (main cu-nl))
        (rename-in :gerbil-coreutils/fold     (main cu-fold))
        (rename-in :gerbil-coreutils/expand   (main cu-expand))
        (rename-in :gerbil-coreutils/unexpand (main cu-unexpand))
        (rename-in :gerbil-coreutils/fmt      (main cu-fmt))
        ;; --- Phase 3: Field/Column ---
        (rename-in :gerbil-coreutils/cut      (main cu-cut))
        (rename-in :gerbil-coreutils/paste    (main cu-paste))
        (rename-in :gerbil-coreutils/join     (main cu-join))
        (rename-in :gerbil-coreutils/comm     (main cu-comm))
        (rename-in :gerbil-coreutils/sort     (main cu-sort))
        (rename-in :gerbil-coreutils/uniq     (main cu-uniq))
        (rename-in :gerbil-coreutils/tr       (main cu-tr))
        (rename-in :gerbil-coreutils/numfmt   (main cu-numfmt))
        ;; --- Phase 4: File & Directory Operations ---
        (rename-in :gerbil-coreutils/mkdir    (main cu-mkdir))
        (rename-in :gerbil-coreutils/rmdir    (main cu-rmdir))
        (rename-in :gerbil-coreutils/mktemp   (main cu-mktemp))
        (rename-in :gerbil-coreutils/touch    (main cu-touch))
        (rename-in :gerbil-coreutils/readlink (main cu-readlink))
        (rename-in :gerbil-coreutils/realpath (main cu-realpath))
        (rename-in :gerbil-coreutils/ln       (main cu-ln))
        (rename-in :gerbil-coreutils/cp       (main cu-cp))
        (rename-in :gerbil-coreutils/mv       (main cu-mv))
        (rename-in :gerbil-coreutils/rm       (main cu-rm))
        (rename-in :gerbil-coreutils/install  (main cu-install))
        (rename-in :gerbil-coreutils/shred    (main cu-shred))
        ;; --- Phase 5: File Information & Permissions ---
        (rename-in :gerbil-coreutils/ls       (main cu-ls))
        (rename-in :gerbil-coreutils/chmod    (main cu-chmod))
        (rename-in :gerbil-coreutils/chown    (main cu-chown))
        (rename-in :gerbil-coreutils/chgrp    (main cu-chgrp))
        (rename-in :gerbil-coreutils/stat     (main cu-stat))
        (rename-in :gerbil-coreutils/du       (main cu-du))
        (rename-in :gerbil-coreutils/df       (main cu-df))
        (rename-in :gerbil-coreutils/pathchk  (main cu-pathchk))
        ;; --- Phase 6: Date, Time & Identity ---
        (rename-in :gerbil-coreutils/date     (main cu-date))
        (rename-in :gerbil-coreutils/id       (main cu-id))
        (rename-in :gerbil-coreutils/groups   (main cu-groups))
        (rename-in :gerbil-coreutils/who      (main cu-who))
        (rename-in :gerbil-coreutils/users    (main cu-users))
        (rename-in :gerbil-coreutils/pinky    (main cu-pinky))
        (rename-in :gerbil-coreutils/uptime   (main cu-uptime))
        (rename-in :gerbil-coreutils/uname    (main cu-uname))
        (rename-in :gerbil-coreutils/arch     (main cu-arch))
        ;; --- Phase 7: Text Transforms & Encoding ---
        (rename-in :gerbil-coreutils/seq      (main cu-seq))
        (rename-in :gerbil-coreutils/expr     (main cu-expr))
        (rename-in :gerbil-coreutils/basenc   (main cu-basenc))
        (rename-in :gerbil-coreutils/base64   (main cu-base64))
        (rename-in :gerbil-coreutils/base32   (main cu-base32))
        (rename-in :gerbil-coreutils/od       (main cu-od))
        (rename-in :gerbil-coreutils/cksum    (main cu-cksum))
        (rename-in :gerbil-coreutils/md5sum   (main cu-md5sum))
        (rename-in :gerbil-coreutils/sha1sum  (main cu-sha1sum))
        (rename-in :gerbil-coreutils/sha224sum (main cu-sha224sum))
        (rename-in :gerbil-coreutils/sha256sum (main cu-sha256sum))
        (rename-in :gerbil-coreutils/sha384sum (main cu-sha384sum))
        (rename-in :gerbil-coreutils/sha512sum (main cu-sha512sum))
        (rename-in :gerbil-coreutils/b2sum    (main cu-b2sum))
        (rename-in :gerbil-coreutils/sum      (main cu-sum))
        ;; --- Phase 8: Process & System Control ---
        (rename-in :gerbil-coreutils/env      (main cu-env))
        (rename-in :gerbil-coreutils/timeout  (main cu-timeout))
        (rename-in :gerbil-coreutils/nice     (main cu-nice))
        (rename-in :gerbil-coreutils/nohup    (main cu-nohup))
        (rename-in :gerbil-coreutils/chroot   (main cu-chroot))
        (rename-in :gerbil-coreutils/stdbuf   (main cu-stdbuf))
        ;; --- Phase 9: Advanced File Operations ---
        (rename-in :gerbil-coreutils/truncate (main cu-truncate))
        (rename-in :gerbil-coreutils/mkfifo   (main cu-mkfifo))
        (rename-in :gerbil-coreutils/mknod    (main cu-mknod))
        (rename-in :gerbil-coreutils/split    (main cu-split))
        (rename-in :gerbil-coreutils/csplit   (main cu-csplit))
        (rename-in :gerbil-coreutils/dd       (main cu-dd))
        (rename-in :gerbil-coreutils/dircolors (main cu-dircolors))
        ;; --- Phase 10: Text Layout & Printing ---
        (rename-in :gerbil-coreutils/tsort    (main cu-tsort))
        (rename-in :gerbil-coreutils/shuf     (main cu-shuf))
        (rename-in :gerbil-coreutils/factor   (main cu-factor))
        (rename-in :gerbil-coreutils/pr       (main cu-pr))
        (rename-in :gerbil-coreutils/ptx      (main cu-ptx))
        (rename-in :gerbil-coreutils/stty     (main cu-stty))
        ;; --- Phase 11: Security Context ---
        (rename-in :gerbil-coreutils/chcon    (main cu-chcon))
        (rename-in :gerbil-coreutils/runcon   (main cu-runcon))
        ;; --- Misc ---
        (rename-in :gerbil-coreutils/dir      (main cu-dir))
        (rename-in :gerbil-coreutils/vdir     (main cu-vdir)))

;;; Wrap a coreutils `main` function as a shell builtin handler.
;;; The handler signature is (lambda (args env) ...) returning an integer exit status.
;;;
;;; Coreutils main functions are (def (main . args)) where args does NOT include
;;; the program name (that's set via parameterize on program-name inside each main).
;;; They may call `exit` on errors, so we intercept it via set! + call/cc + dynamic-wind.
(def (make-coreutils-builtin name main-fn)
  (lambda (args env)
    (let ((saved-exit #f))
      (let ((status
             (call-with-current-continuation
              (lambda (k)
                (dynamic-wind
                  (lambda () (set! saved-exit exit) (set! exit (lambda (code) (k code))))
                  (lambda ()
                    (with-exception-catcher
                     (lambda (e)
                       (with-exception-catcher
                        (lambda (_) (void))
                        (lambda ()
                          (display-exception e (current-error-port))))
                       1)
                     (lambda ()
                       (apply main-fn args)
                       0)))
                  (lambda () (set! exit saved-exit)))))))
        (if (fixnum? status) status 0)))))

;;; Register all coreutils commands as shell builtins.
;;; Skips commands gsh already implements natively (true, false, echo, pwd, printf, test, kill).
(def (register-coreutils!)
  (for-each
   (lambda (entry)
     (let ((name (car entry))
           (main-fn (cdr entry)))
       (builtin-register! name (make-coreutils-builtin name main-fn))))
   `(;; --- Phase 1: Trivial ---
     ("basename"  . ,cu-basename)
     ("dirname"   . ,cu-dirname)
     ("link"      . ,cu-link)
     ("unlink"    . ,cu-unlink)
     ("yes"       . ,cu-yes)
     ("printenv"  . ,cu-printenv)
     ("sleep"     . ,cu-sleep)
     ("whoami"    . ,cu-whoami)
     ("logname"   . ,cu-logname)
     ("hostname"  . ,cu-hostname)
     ("nproc"     . ,cu-nproc)
     ("tty"       . ,cu-tty)
     ("sync"      . ,cu-sync)
     ("hostid"    . ,cu-hostid)
     ;; --- Phase 2: Text Filters ---
     ("cat"       . ,cu-cat)
     ("head"      . ,cu-head)
     ("tail"      . ,cu-tail)
     ("tac"       . ,cu-tac)
     ("tee"       . ,cu-tee)
     ("wc"        . ,cu-wc)
     ("nl"        . ,cu-nl)
     ("fold"      . ,cu-fold)
     ("expand"    . ,cu-expand)
     ("unexpand"  . ,cu-unexpand)
     ("fmt"       . ,cu-fmt)
     ;; --- Phase 3: Field/Column ---
     ("cut"       . ,cu-cut)
     ("paste"     . ,cu-paste)
     ("join"      . ,cu-join)
     ("comm"      . ,cu-comm)
     ("sort"      . ,cu-sort)
     ("uniq"      . ,cu-uniq)
     ("tr"        . ,cu-tr)
     ("numfmt"    . ,cu-numfmt)
     ;; --- Phase 4: File & Directory Operations ---
     ("mkdir"     . ,cu-mkdir)
     ("rmdir"     . ,cu-rmdir)
     ("mktemp"    . ,cu-mktemp)
     ("touch"     . ,cu-touch)
     ("readlink"  . ,cu-readlink)
     ("realpath"  . ,cu-realpath)
     ("ln"        . ,cu-ln)
     ("cp"        . ,cu-cp)
     ("mv"        . ,cu-mv)
     ("rm"        . ,cu-rm)
     ("install"   . ,cu-install)
     ("shred"     . ,cu-shred)
     ;; --- Phase 5: File Information & Permissions ---
     ("ls"        . ,cu-ls)
     ("chmod"     . ,cu-chmod)
     ("chown"     . ,cu-chown)
     ("chgrp"     . ,cu-chgrp)
     ("stat"      . ,cu-stat)
     ("du"        . ,cu-du)
     ("df"        . ,cu-df)
     ("pathchk"   . ,cu-pathchk)
     ;; --- Phase 6: Date, Time & Identity ---
     ("date"      . ,cu-date)
     ("id"        . ,cu-id)
     ("groups"    . ,cu-groups)
     ("who"       . ,cu-who)
     ("users"     . ,cu-users)
     ("pinky"     . ,cu-pinky)
     ("uptime"    . ,cu-uptime)
     ("uname"     . ,cu-uname)
     ("arch"      . ,cu-arch)
     ;; --- Phase 7: Text Transforms & Encoding ---
     ("seq"       . ,cu-seq)
     ("expr"      . ,cu-expr)
     ("basenc"    . ,cu-basenc)
     ("base64"    . ,cu-base64)
     ("base32"    . ,cu-base32)
     ("od"        . ,cu-od)
     ("cksum"     . ,cu-cksum)
     ("md5sum"    . ,cu-md5sum)
     ("sha1sum"   . ,cu-sha1sum)
     ("sha224sum" . ,cu-sha224sum)
     ("sha256sum" . ,cu-sha256sum)
     ("sha384sum" . ,cu-sha384sum)
     ("sha512sum" . ,cu-sha512sum)
     ("b2sum"     . ,cu-b2sum)
     ("sum"       . ,cu-sum)
     ;; --- Phase 8: Process & System Control ---
     ("env"       . ,cu-env)
     ("timeout"   . ,cu-timeout)
     ("nice"      . ,cu-nice)
     ("nohup"     . ,cu-nohup)
     ("chroot"    . ,cu-chroot)
     ("stdbuf"    . ,cu-stdbuf)
     ;; --- Phase 9: Advanced File Operations ---
     ("truncate"  . ,cu-truncate)
     ("mkfifo"    . ,cu-mkfifo)
     ("mknod"     . ,cu-mknod)
     ("split"     . ,cu-split)
     ("csplit"    . ,cu-csplit)
     ("dd"        . ,cu-dd)
     ("dircolors" . ,cu-dircolors)
     ;; --- Phase 10: Text Layout & Printing ---
     ("tsort"     . ,cu-tsort)
     ("shuf"      . ,cu-shuf)
     ("factor"    . ,cu-factor)
     ("pr"        . ,cu-pr)
     ("ptx"       . ,cu-ptx)
     ("stty"      . ,cu-stty)
     ;; --- Phase 11: Security Context ---
     ("chcon"     . ,cu-chcon)
     ("runcon"    . ,cu-runcon)
     ;; --- Misc ---
     ("dir"       . ,cu-dir)
     ("vdir"      . ,cu-vdir))))

;;; --- Self-registration (runs at module init time) ---
(register-coreutils!)
