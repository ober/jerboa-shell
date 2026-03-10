#!chezscheme
;;; stage.sls — Build tier configuration for Chez port
;;; Tier: small — core shell + Chez eval (no Gerbil compiler/coreutils)

(library (gsh stage)
  (export)
  (import (chezscheme) (gsh registry))

  (*gsh-tier* "small")

  ) ;; end library
