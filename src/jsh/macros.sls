#!chezscheme
(library (jsh macros)
  (export
    defbuiltin
    with-resolved-var
    when-args-pair
    with-file-info)
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
    (std sugar) (gsh registry) (for (runtime syntax) expand))
  (define-syntax defbuiltin
    (lambda (stx)
      (syntax-case stx ()
        [(defsyntax_defbuiltin name body ...)
         (with-syntax ([$args (datum->syntax
                                #'defsyntax_defbuiltin
                                'args)]
                       [$env (datum->syntax #'defsyntax_defbuiltin 'env)])
           #'(builtin-register!
               name
               (lambda ($args $env) body ...)))])))
  (define-syntax with-resolved-var
    (syntax-rules ()
      [(with-resolved-var (name-binding var-binding) env name body
         ...)
       "Execute body with nameref-resolved name and variable bindings.\n\n  This macro eliminates the repeated pattern of resolving namerefs and\n  looking up variables in the scope chain. Used extensively in environment.ss.\n\n  Usage:\n    (with-resolved-var (resolved-name var) env original-name\n      (when var\n        (shell-var-value var)))\n\n  Expands to:\n    (let* ((resolved-name (resolve-nameref original-name env))\n           (var (find-var-in-chain env resolved-name)))\n      body ...)\n  "]))
  (define-syntax when-args-pair
    (syntax-rules ()
      [(when-args-pair (head tail) args body ...)
       "Execute body when args is a pair, binding head and tail.\n\n  This macro simplifies the common pattern of checking if an argument\n  list is non-empty before accessing car and cdr.\n\n  Usage:\n    (when-args-pair (first rest) args\n      (process first)\n      (loop rest))\n\n  Expands to:\n    (when (pair? args)\n      (let ((head (car args))\n            (tail (cdr args)))\n        body ...))\n  "]))
  (define-syntax with-file-info
    (syntax-rules ()
      [(with-file-info (info-var exists?-var) path body ...)
       "Execute body with cached file existence and info bindings.\n\n  This macro avoids redundant file-exists? and file-info calls by\n  caching the results. Useful when multiple file tests are needed.\n\n  Usage:\n    (with-file-info (info exists?) filepath\n      (cond\n        ((not exists?) 'missing)\n        ((eq? (file-info-type info) 'regular) 'file)\n        (else 'other)))\n\n  Note: info-var will be #f when the file doesn't exist.\n  "])))
