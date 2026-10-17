#lang racket

;; Grammar-generation + tokenizer smoke tests for the Koka front end.

(require rackunit
         racklr/tree
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser)

(define-values (koka-parse koka-tokenize koka-tok-type koka-tok-value)
  (gen-and-load-layout "../grammars-v4/koka/Koka.g4"))

;; ── Tokenizer smoke tests ───────────────────────────────────────────

(define (tok-types in)
  (map koka-tok-type (koka-tokenize in)))

;; Layout: a newline followed by deeper indentation inserts INDENT.
(check-not-false (member 'INDENT (tok-types "fun f()\n  x"))
                 "INDENT inserted after deeper newline")
(check-not-false (member 'NEWLINE (tok-types "val x = 1\nval y = 2"))
                 "NEWLINE token present")

;; ── Parse smoke tests ───────────────────────────────────────────────

(define (parses? in)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (any-tree? (koka-parse in))))

(check-true (parses? "42") "literal")
(check-true (parses? "\"hi\"") "string")
(check-true (parses? "x") "identifier")
(check-true (parses? "x + 1") "binary op")
(check-true (parses? "f(x, y)") "call")
(check-true (parses? "val x = 1") "val binding")
(check-true (parses? "fun add(x: int, y: int) : int\n  x + y") "function")
(check-true (parses? "effect state\n  fun get() : int") "effect declaration")
(check-true (parses? "if x == 0 then\n  1\nelse\n  2") "if expression")

(displayln "All Koka grammar/tokenizer tests passed.")
