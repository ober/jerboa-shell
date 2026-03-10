#!chezscheme
;;; pregexp-compat.sls — Re-export pregexp functions for gsh modules
(library (gsh pregexp-compat)
  (export pregexp
          pregexp-match-positions
          pregexp-match
          pregexp-split
          pregexp-replace
          pregexp-replace*
          pregexp-quote)
  (import (std pregexp)))
