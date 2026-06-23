#lang racket

(require rackunit
         racklr/tree
         racklr/gen-test)

;; ── Load the JSON parser ────────────────────────────────────────────

(define-values (json-parse json-tokenize tok-type tok-value)
  (gen-and-load "../grammars-v4/json/JSON.g4"))

;; ── Tokenizer Tests ─────────────────────────────────────────────────

(let ([tks (json-tokenize "42")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'NUMBER)
  (check-equal? (tok-value (first tks)) "42")
  (check-equal? (tok-type (second tks)) 'EOF))

(let ([tks (json-tokenize "\"hello\"")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'STRING)
  (check-equal? (tok-value (first tks)) "\"hello\""))

(let ([tks (json-tokenize "true")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-value (first tks)) "true"))

(let ([tks (json-tokenize "{}")])
  (check-equal? (length tks) 3)
  (check-equal? (tok-type (first tks)) (string->symbol "{"))
  (check-equal? (tok-type (second tks)) (string->symbol "}")))

;; ── JSON Parsing Tests ──────────────────────────────────────────────

;; Numbers
(let ([cst (json-parse "42")])
  (check-equal? (cst-node-tag cst) 'json)
  (define kids (cst-node-children cst))
  (check-true (>= (length kids) 1))
  (check-equal? (cst-node-tag (first kids)) 'value))

(let ([cst (json-parse "-3.14")])
  (check-equal? (cst-node-tag cst) 'json))

;; Strings
(let ([cst (json-parse "\"hello\"")])
  (check-equal? (cst-node-tag cst) 'json))

;; true / false / null
(void (check-parse json-parse "true"))
(void (check-parse json-parse "false"))
(void (check-parse json-parse "null"))

;; Empty object
(void (check-parse json-parse "{}"))

;; Simple object
(void (check-parse json-parse "{\"a\": 1}"))

;; Nested object
(void (check-parse json-parse "{\"a\": {\"b\": 2}}"))

;; Array
(void (check-parse json-parse "[1, 2, 3]"))

;; Empty array
(void (check-parse json-parse "[]"))

;; Complex nested structure
(void (check-parse json-parse "{\"a\": 1, \"b\": [true, false, null], \"c\": {\"d\": \"hello\"}}"))

;; Object with multiple pairs
(void (check-parse json-parse "{\"x\": 1, \"y\": 2, \"z\": 3}"))

;; ── Reject invalid input ────────────────────────────────────────────

(check-reject json-parse "not-json")
(check-reject json-parse "")

;; ── CST Structure Checks ────────────────────────────────────────────

;; Count total tokens/leaves in a CST
(define (count-terminals t)
  "Count terminal children (tokens or leaves) in CST tree."
  (cond [(cst-node? t) (apply + (map count-terminals (cst-node-children t)))]
        [else 1]))

(let ([cst (json-parse "{\"a\":1,\"b\":2}")])
  ;; Should have terminals: {, "a", :, 1, ,, "b", :, 2, }, plus EOF in json node
  (check-true (> (count-terminals cst) 5)))

;; ── Cleanup ─────────────────────────────────────────────────────────

(cleanup)

(displayln "All JSON parser tests passed.")
