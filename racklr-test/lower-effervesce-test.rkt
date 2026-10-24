#lang racket

;; Round-trip + UIR-shape tests for the Effervesce source language (Phase 1):
;; parse Effervesce.g4, lower the CST to UIR, emit text, re-parse, re-lower,
;; and assert structural equality. Exercises all five .fizz examples.

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-effervesce
         racklr/emit-effervesce)

(define-values (eff-parse eff-tokenize eff-tok-type eff-tok-value)
  (gen-and-load "../effervesce/grammar/Effervesce.g4"))

(define (lower-src src)
  (lower-effervesce (eff-parse src) eff-tok-value eff-tok-type))

(define (round-trips? src)
  (define u1 (lower-src src))
  (and (uir? u1)
       (let* ([emitted (emit-effervesce u1)]
              [u2 (lower-src emitted)])
         (equal? (uir->sexp u1) (uir->sexp u2)))))

(define suite
  (test-suite
   "Effervesce lowerer round-trip"
   (test-case "five example files round-trip structurally"
     (for ([name '("config-reader" "todo-store" "todo-memory" "todo-database" "todo-rules" "todo-peer")])
       (define src (file->string (format "../effervesce/examples/~a.fizz" name)))
       (check-true (round-trips? src) (format "~a.fizz round-trips" name))))
   (test-case "UIR shape: system and capability"
     (define uir (lower-src "system X { capability Read<A> { get() -> A } }"))
     (check-true (uir-eff-system? uir) "system lowers to uir-eff-system")
     (check-equal? (uir-symbol-name (uir-eff-system-name uir)) "X" "system name")
     (define cap (first (uir-eff-system-decls uir)))
     (check-true (uir-eff-capability? cap) "capability lowers to uir-eff-capability")
     (check-equal? (length (uir-eff-capability-type-params cap)) 1 "one type param")
     (check-equal? (length (uir-eff-capability-ops cap)) 1 "one op"))
   (test-case "UIR shape: given and handler rows"
     (define uir (lower-src "system X { given Fs, Log }"))
     (define g (first (uir-eff-system-decls uir)))
     (check-true (uir-eff-given? g) "given lowers to uir-eff-given")
     (check-equal? (length (uir-eff-given-row g)) 2 "two refs in row")
     (check-true (uir-eff-ref? (first (uir-eff-given-row g))) "row item is uir-eff-ref"))
   (test-case "UIR shape: handler op quad"
     (define uir (lower-src "system X { Audit via Log { log(msg) = write(msg) } }"))
     (define h (first (uir-eff-system-decls uir)))
     (check-true (uir-eff-handler? h) "via lowers to uir-eff-handler")
     (check-equal? (length (uir-eff-handler-ops h)) 1 "one op impl")
     (define op (first (uir-eff-handler-ops h)))
     (check-equal? (length op) 4 "op impl is a 4-element quad")
     (check-equal? (uir-symbol-name (car op)) "log" "op name"))
   (test-case "UIR shape: policy predicate"
     (define uir (lower-src "system X { never component needs Read and not component needs Audit }"))
     (define p (first (uir-eff-system-decls uir)))
     (check-true (uir-eff-policy? p) "never lowers to uir-eff-policy")
     (check-eq? (uir-eff-policy-kind p) 'never "policy kind is never")
     (define pred (uir-eff-policy-predicate p))
     (check-true (uir-call? pred) "predicate lowers to uir-call")
     (check-equal? (uir-symbol-name (uir-call-callee pred)) "and" "top-level and"))))

(exit (run-tests suite))
