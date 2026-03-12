;;; registry.ss — Built-in command registry for gsh
;;;
;;; Separated from builtins.ss to avoid circular dependency with macros.ss

(export #t)
(import :std/sort)

;;; --- Build tier ---
;; Set by stage.ss at module init time. Controls feature availability.
;; Values: "tiny" (shell only), "small" (+eval), "medium" (+compiler), "large" (+coreutils)
(def *gsh-tier* (make-parameter "tiny"))

;;; --- Built-in registry ---

(def *builtins* (make-hash-table))

;; Register a built-in command
(def (builtin-register! name handler)
  (hash-put! *builtins* name handler))

;; Look up a built-in by name
(def (builtin-lookup name)
  (hash-get *builtins* name))

;; List all built-in names
(def (builtin-list)
  (sort! (hash-keys *builtins*) string<?))

;; Check if a name is a built-in
(def (builtin? name)
  (and (builtin-lookup name) #t))
