#!chezscheme
;; Build driver: imports all modules to trigger Chez compilation
;; Generate WPO files so build-binary.ss can do whole-program optimization
(parameterize ([compile-imported-libraries #t]
               [generate-wpo-files #t]
               [optimize-level 3]
               [generate-inspector-information #f])
  (eval '(import
    (jsh ast) (jsh registry) (jsh macros) (jsh util)
    (jsh environment) (jsh lexer) (jsh arithmetic) (jsh glob)
    (jsh fuzzy) (jsh history) (jsh parser) (jsh functions)
    (jsh signals) (jsh expander) (jsh redirect) (jsh control)
    (jsh jobs) (jsh builtins) (jsh pipeline) (jsh executor)
    (jsh completion) (jsh prompt) (jsh lineedit) (jsh fzf)
    (jsh script) (jsh startup) (jsh main) (jsh stage))
  (interaction-environment)))
