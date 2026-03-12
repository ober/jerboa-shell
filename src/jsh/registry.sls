#!chezscheme
(library (jsh registry)
  (export *gsh-tier* *builtins* builtin-register!
    builtin-lookup builtin-list builtin?)
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
    (except (std format) format) (std sort) (std pregexp))
  (define *gsh-tier* (make-parameter "tiny"))
  (define *builtins* (make-hash-table))
  (define (builtin-register! name handler)
    (hash-put! *builtins* name handler))
  (define (builtin-lookup name) (hash-get *builtins* name))
  (define (builtin-list)
    (sort! (hash-keys *builtins*) string<?))
  (define (builtin? name) (and (builtin-lookup name) #t)))
