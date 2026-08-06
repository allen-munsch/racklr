#lang racket

(require racklr/uir
         "lower-tsx/helpers.rkt"
         "lower-tsx/extract.rkt"
         "lower-tsx/embed.rkt"
         "lower-tsx/conditionals.rkt"
         "lower-tsx/preprocess.rkt"
         "lower-tsx/hooks.rkt"
         "lower-tsx/hook-lower.rkt")

(provide find-all-jsx extract-jsx preprocess-tsx restore-jsx
         preprocess-imports lower-hooks
         advance-past-string preprocess-jsx-expression-embeds
         process-jsx-expr-conditionals
         id-start? id-cont? skip-id context-allows-jsx?)

;; ── TSX Preprocessor ───────────────────────────────────────────────
;; Thin dispatcher — all implementation lives in lower-tsx/ submodules.
