#!chezscheme
;;; stage.sls — Build tier configuration for Chez port
;;; Tier: small — core shell + Chez eval (no Gerbil compiler/coreutils)

(library (jsh stage)
  (export)
  (import (chezscheme) (jsh registry))

  (*jsh-tier* "small")

  ) ;; end library
