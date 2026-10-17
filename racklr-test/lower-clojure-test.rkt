#lang racket

;; Round-trip + UIR-shape tests for the Clojure source language
;; (Phase 7): parse Clojure.g4, lower the CST to UIR, emit Clojure text.

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser
         racklr/lower-clojure
         racklr/emit-clojure)

;; ── Load the Clojure parser ──────────────────────────────────────────

(define-values (clj-parse clj-tokenize clj-tok-type clj-tok-value)
  (gen-and-load "../grammars-v4/clojure/Clojure.g4"))

;; ── Round-trip test helper ───────────────────────────────────────────

(define (check-clj input-str expected-emitted)
  (define cst (clj-parse input-str))
  (check-true (any-tree? cst)
              (format "parse ~s" input-str))
  (define uir (lower-clojure cst clj-tok-type clj-tok-value))
  (check-true (uir? uir)
              (format "lower ~s" input-str))
  (define emitted (emit-clojure uir))
  (check-equal? emitted expected-emitted
                (format "round-trip ~s" input-str)))

;; ── Literals ─────────────────────────────────────────────────────────

(check-clj "nil" "nil")
(check-clj "true" "true")
(check-clj "false" "false")
(check-clj "42" "42")
(check-clj "3.14" "3.14")
(check-clj "\"hello\"" "\"hello\"")

;; keywords, symbols, namespace-qualified symbols
(check-clj ":hello" ":hello")
(check-clj "::alias" "::alias")
(check-clj "some-symbol" "some-symbol")
(check-clj "clojure.string/join" "clojure.string/join")

;; ── Collections ──────────────────────────────────────────────────────

(check-clj "[1 2 3]" "[1 2 3]")
(check-clj "{:a 1 :b 2}" "{:a 1 :b 2}")
(check-clj "#{1 2 3}" "#{1 2 3}")

;; ── Calls and special forms ──────────────────────────────────────────

(check-clj "(println \"hi\")" "(println \"hi\")")
(check-clj "(llm/query x)" "(llm/query x)")
(check-clj "(ns hello.core)" "(ns hello.core)")
(check-clj "(def x 1)" "(def x 1)")
(check-clj "(let [a 1] a)" "(let [a 1] a)")
(check-clj "(if x 1 2)" "(if x 1 2)")
(check-clj "(do a b c)" "(do a b c)")

;; ── Functions ────────────────────────────────────────────────────────

(check-clj "(defn f [x] (println x))" "(defn f [x] (println x))")
(check-clj "(defn add [a b] (+ a b))" "(defn add [a b] (+ a b))")
(check-clj "(fn [x] (* x 2))" "(fn [x] (* x 2))")

;; ── Reader macros ────────────────────────────────────────────────────

(check-clj "'(1 2 3)" "'(1 2 3)")
(check-clj "@state" "@state")
(check-clj "#'some-var" "#'some-var")

;; metadata (non-effects) round-trips via with-meta → ^ sugar
(check-clj "^{:doc \"hi\"} (defn f [] 1)" "^{:doc \"hi\"} (defn f [] 1)")

;; ── UIR shape inspections ────────────────────────────────────────────

;; (llm/query x) lowers to a call whose callee is the symbol "llm/query"
(let ([cst (clj-parse "(llm/query x)")])
  (define uir (lower-clojure cst clj-tok-type clj-tok-value))
  (define stmts (uir-block-stmts uir))
  (check-equal? (length stmts) 1 "one top-level form")
  (define call (first stmts))
  (check-true (uir-call? call) "call node")
  (check-true (uir-symbol? (uir-call-callee call)) "callee is symbol")
  (check-equal? (uir-symbol-name (uir-call-callee call)) "llm/query"
                "callee name"))

;; (defn f [x] body) lowers to a uir-fn with name f and one param
(let ([cst (clj-parse "(defn f [x] (println x))")])
  (define uir (lower-clojure cst clj-tok-type clj-tok-value))
  (define fn-node (first (uir-block-stmts uir)))
  (check-true (uir-fn? fn-node) "defn lowers to uir-fn")
  (check-equal? (uir-symbol-name (uir-fn-name fn-node)) "f" "fn name")
  (check-equal? (length (uir-fn-params fn-node)) 1 "one param")
  (check-equal? (uir-symbol-name (first (uir-fn-params fn-node))) "x"
                "param name"))

(displayln "All Clojure lowerer tests passed.")
