#lang racket

;; ── TypeScript CST → UIR lowering — hub module ──────────────────────
;; The lowering logic lives in lower-typescript/stmt.rkt (statements and
;; declarations) and lower-typescript/expr.rkt (expressions). This module
;; re-exports the entry point so external callers keep requiring
;; `racklr/lower-typescript` unchanged.

(require "lower-typescript/stmt.rkt")

(provide lower-program)
