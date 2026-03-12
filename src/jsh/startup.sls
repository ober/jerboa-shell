#!chezscheme
(library (jsh startup)
  (export load-startup-files! run-logout! source-if-exists!)
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
      with-output-to-string string->bytes bytes->string thread?)
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
    (std sugar) (gsh util) (gsh environment) (gsh script))
  (define (load-startup-files! env login? interactive?)
    (cond
      [(and login? interactive?)
       (source-if-exists! "/etc/profile" env)
       (let ([home (or (env-get env "HOME") (home-directory))])
         (or (source-if-exists!
               (string-append home "/.gsh_profile")
               env)
             (source-if-exists! (string-append home "/.gsh_login") env)
             (source-if-exists! (string-append home "/.profile") env)))]
      [interactive?
       (let ([home (or (env-get env "HOME") (home-directory))])
         (source-if-exists! (string-append home "/.gshrc") env))]
      [else
       (let ([env-file (env-get env "GSH_ENV")])
         (when env-file (source-if-exists! env-file env)))]))
  (define (run-logout! env)
    (let ([home (or (env-get env "HOME") (home-directory))])
      (source-if-exists!
        (string-append home "/.gsh_logout")
        env)))
  (define (source-if-exists! path env)
    (if (file-exists? path)
        (begin (source-file! path env) #t)
        #f)))
