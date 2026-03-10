#!chezscheme
;;; static-compat.sls -- Stub for Gambit static loader compat
;;; In the Chez port, this is a no-op since we don't have Gambit's
;;; static module loader to patch.

(library (gsh static-compat)
  (export ensure-static-compat! __load-gxi
    scm-only-load-module-active? patch-loader-post-gxi!
    ensure-static-compile-env! static-binary?)
  (import (chezscheme))

  ;; In the Chez port, there is no Gambit module loader or Gerbil expander
  ;; to patch. All these are no-ops.
  (define (ensure-static-compat!) (void))
  (define (__load-gxi) (void))
  (define (scm-only-load-module-active?) #f)
  (define (patch-loader-post-gxi!) (void))
  (define (ensure-static-compile-env!) (void))
  (define (static-binary?) #f)

  ) ;; end library
