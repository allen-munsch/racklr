#lang racket

;; ── lower-javascript — hub module ───────────────────────────────────
;; The lowering split (B75): statement/entry lowering in stmt.rkt,
;; expression lowering in expr.rkt, shared CST helpers in helpers.rkt.
;; External callers keep requiring racklr/lower-javascript for lower-program.

(require "lower-javascript/stmt.rkt")

(provide lower-program)
