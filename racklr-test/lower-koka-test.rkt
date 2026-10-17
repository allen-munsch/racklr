#lang racket

;; Round-trip + UIR-shape tests for the Koka source language (Phase 1):
;; parse Koka.g4, lower the CST to UIR, emit Koka text, compare.

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-koka
         racklr/emit-koka)

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (gen-and-load-layout "../grammars-v4/koka/Koka.g4"))

(define (check-koka input-str expected-emitted)
  (define cst (koka-parse input-str))
  (check-true (any-tree? cst) (format "parse ~s" input-str))
  (define uir (lower-koka cst koka-tok-type koka-tok-value))
  (check-true (uir? uir) (format "lower ~s" input-str))
  (check-equal? (emit-koka uir) expected-emitted
                (format "round-trip ~s" input-str)))

;; ── Literals ─────────────────────────────────────────────────────────

(check-koka "42" "42")
(check-koka "\"hello\"" "\"hello\"")

;; ── Variables, calls, field access ───────────────────────────────────

(check-koka "x" "x")
(check-koka "f(x, y)" "f(x, y)")
(check-koka "x.y" "x.y")

;; ── Operators (flat left-assoc, no precedence in pass 1) ────────────

(check-koka "x + 1" "x + 1")
(check-koka "x + 1 * 2" "x + 1 * 2")

;; ── Bindings and control flow ────────────────────────────────────────

(check-koka "val x = 1" "val x = 1")
(check-koka "if x == 0 then\n  1\nelse\n  2"
            "if x == 0 then\n  1\nelse\n  2")

;; ── Functions ────────────────────────────────────────────────────────

(check-koka "fun add(x: int, y: int) : int\n  x + y"
            "fun add(x: int, y: int) : int\n  x + y")
(check-koka "fun f() : <exn, div> int\n  1"
            "fun f() : <exn, div> int\n  1")
(check-koka "fn(x)\n  x + 1"
            "fn(x)\n  x + 1")

;; ── Effect declarations ──────────────────────────────────────────────

(check-koka "effect state\n  fun get() : int"
            "effect state\n  fun get() : int")

;; ── Algebraic data types ─────────────────────────────────────────────

(check-koka "type list\n  Cons(head: int, tail: list)\n  Nil"
            "type list\n  Cons(head: int, tail: list)\n  Nil")

;; ── Effect handlers ──────────────────────────────────────────────────

(check-koka "with handler\n  fun get() -> 42\n  return(x) -> x\n  x"
            "with handler\n  fun get() -> 42\n  return(x) -> x\n  x")

;; ── UIR shape spot-checks ────────────────────────────────────────────

(let ([cst (koka-parse "effect state\n  fun get() : int")])
  (define uir (lower-koka cst koka-tok-type koka-tok-value))
  (define decl (first (uir-block-stmts uir)))
  (check-true (uir-effect-decl? decl) "effect lowers to uir-effect-decl")
  (check-equal? (uir-symbol-name (uir-effect-decl-name decl)) "state" "effect name")
  (check-equal? (length (uir-effect-decl-ops decl)) 1 "one op")
  (check-true (uir-effect-op? (first (uir-effect-decl-ops decl))) "op is uir-effect-op"))

(let ([cst (koka-parse "fun f() : <exn, div> int\n  1")])
  (define uir (lower-koka cst koka-tok-type koka-tok-value))
  (define fn (first (uir-block-stmts uir)))
  (check-true (uir-fn? fn) "fun lowers to uir-fn")
  (check-true (uir-effect-row? (uir-fn-effects fn)) "effects lower to uir-effect-row")
  (check-equal? (length (uir-effect-row-entries (uir-fn-effects fn))) 2 "two effect entries"))

(displayln "All Koka lowerer tests passed.")
