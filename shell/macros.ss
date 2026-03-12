;;; macros.ss — Shell-specific macros for gsh
;;;
;;; This module provides macros to reduce code bloat and improve maintainability
;;; across the gsh codebase. See refactor.md for analysis and rationale.

(export #t)
(import :std/sugar
        :gsh/registry)

;;; --- Builtin Registration ---

;; Use defsyntax with explicit unhygienic identifier introduction
(defsyntax (defbuiltin stx)
  (syntax-case stx ()
    ((defsyntax_defbuiltin name body ...)
     (with-syntax (($args (datum->syntax #'defsyntax_defbuiltin 'args))
                   ($env (datum->syntax #'defsyntax_defbuiltin 'env)))
       #'(builtin-register! name (lambda ($args $env) body ...))))))

;;; --- Nameref Resolution ---

(defrule (with-resolved-var (name-binding var-binding) env name body ...)
  "Execute body with nameref-resolved name and variable bindings.

  This macro eliminates the repeated pattern of resolving namerefs and
  looking up variables in the scope chain. Used extensively in environment.ss.

  Usage:
    (with-resolved-var (resolved-name var) env original-name
      (when var
        (shell-var-value var)))

  Expands to:
    (let* ((resolved-name (resolve-nameref original-name env))
           (var (find-var-in-chain env resolved-name)))
      body ...)
  "
  (let* ((name-binding (resolve-nameref name env))
         (var-binding (find-var-in-chain env name-binding)))
    body ...))

;;; --- Argument List Helpers ---

(defrule (when-args-pair (head tail) args body ...)
  "Execute body when args is a pair, binding head and tail.

  This macro simplifies the common pattern of checking if an argument
  list is non-empty before accessing car and cdr.

  Usage:
    (when-args-pair (first rest) args
      (process first)
      (loop rest))

  Expands to:
    (when (pair? args)
      (let ((head (car args))
            (tail (cdr args)))
        body ...))
  "
  (when (pair? args)
    (let ((head (car args))
          (tail (cdr args)))
      body ...)))

;;; --- File Information Caching ---

(defrule (with-file-info (info-var exists?-var) path body ...)
  "Execute body with cached file existence and info bindings.

  This macro avoids redundant file-exists? and file-info calls by
  caching the results. Useful when multiple file tests are needed.

  Usage:
    (with-file-info (info exists?) filepath
      (cond
        ((not exists?) 'missing)
        ((eq? (file-info-type info) 'regular) 'file)
        (else 'other)))

  Note: info-var will be #f when the file doesn't exist.
  "
  (let ((exists?-var (file-exists? path)))
    (let ((info-var (if exists?-var (file-info path) #f)))
      body ...)))
