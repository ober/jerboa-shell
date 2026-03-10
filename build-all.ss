#!chezscheme
;; Build driver: imports all modules to trigger Chez compilation
(import
  (gsh ast) (gsh registry) (gsh macros) (gsh util)
  (gsh environment) (gsh lexer) (gsh arithmetic) (gsh glob)
  (gsh fuzzy) (gsh history) (gsh parser) (gsh functions)
  (gsh signals) (gsh expander) (gsh redirect) (gsh control)
  (gsh jobs) (gsh builtins) (gsh pipeline) (gsh executor)
  (gsh completion) (gsh prompt) (gsh lineedit) (gsh fzf)
  (gsh script) (gsh startup) (gsh main) (gsh stage))
