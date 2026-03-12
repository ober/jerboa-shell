;;; pregexp-compat.ss — Wrapper for :std/pregexp
;;;
;;; This module provides a compatibility layer for pregexp.
;;; Currently just re-exports :std/pregexp functions.

(import :std/pregexp)

(export (import: :std/pregexp))
