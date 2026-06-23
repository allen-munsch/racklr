#lang racket

(require rackunit
         racklr/tree
         racklr/g4-lex
         racklr/g4-parse)

;; ── Lexer Tests ───────────────────────────────────────────────────

(let ([toks (g4-lex "grammar test;")])
  (check-equal? (length toks) 3) ;; grammar, test, semicolon (no eof token)
  (check-equal? (token-type (first toks)) 'grammar)
  (check-equal? (token-type (second toks)) 'id)
  (check-equal? (token-value (second toks)) "test")
  (check-equal? (token-type (third toks)) 'semicolon))

;; Lexer: keywords
(let ([toks (g4-lex "fragment skip channel more pushMode popMode type mode options")])
  (check-equal? (token-type (first toks)) 'fragment)
  (check-equal? (token-type (second toks)) 'skip)
  (check-equal? (token-type (third toks)) 'channel)
  (check-equal? (token-type (fourth toks)) 'more)
  (check-equal? (token-type (fifth toks)) 'pushMode)
  (check-equal? (token-type (sixth toks)) 'popMode)
  (check-equal? (token-type (seventh toks)) 'type)
  (check-equal? (token-type (eighth toks)) 'mode)
  (check-equal? (token-type (ninth toks)) 'options))

;; Lexer: token-id (uppercase) vs id (lowercase)
(let ([toks (g4-lex "FOO bar")])
  (check-equal? (token-type (first toks)) 'token-id)
  (check-equal? (token-type (second toks)) 'id))

;; Lexer: string and char-class
(let ([toks (g4-lex "'hello' [a-z]")])
  (check-equal? (token-type (first toks)) 'string)
  (check-equal? (token-value (first toks)) "hello")
  (check-equal? (token-type (second toks)) 'char-class)
  (check-equal? (token-value (second toks)) "[a-z]"))

;; Lexer: symbols
(let ([toks (g4-lex ": ; | ( ) * + ? . .. ~ = , ->")])
  (check-equal? (map token-type toks)
                '(colon semicolon pipe lparen rparen star plus question
                  dot range tilde assign comma arrow)))

;; Lexer: comments are skipped
(let ([toks (g4-lex "grammar // comment\ntest")])
  (check-equal? (token-type (first toks)) 'grammar)
  (check-equal? (token-type (second toks)) 'id))

;; Lexer: block comments skipped
(let ([toks (g4-lex "grammar /* comment */ test")])
  (check-equal? (token-type (first toks)) 'grammar)
  (check-equal? (token-type (second toks)) 'id))

;; ── Parser Tests ──────────────────────────────────────────────────

;; Minimal grammar
(define min-grammar "grammar T; rule : 'x' ;")
(let ([ast (parse-g4 min-grammar)])
  (check-equal? (any-tree-tag ast) 'grammar)
  (check-equal? (any-tree-text (first (any-tree-children ast))) "T")
  (define rules-node (second (any-tree-children ast)))
  (check-equal? (any-tree-tag rules-node) 'rules)
  (define rule1 (first (any-tree-children rules-node)))
  (check-equal? (any-tree-tag rule1) 'parser-rule))

;; Parser rule with alternatives
(define alt-grammar "grammar T; rule : 'a' | 'b' | 'c' ;")
(let ([ast (parse-g4 alt-grammar)])
  (check-true (any-tree? ast)))

;; Lexer rule with fragment
(define frag-grammar "grammar T; FOO : 'bar' ; fragment BAZ : 'qux' ;")
(let ([ast (parse-g4 frag-grammar)])
  (define rules (any-tree-children (second (any-tree-children ast))))
  (check-equal? (length rules) 2)
  (check-equal? (any-tree-tag (first rules)) 'lexer-rule)
  (check-equal? (any-tree-tag (second rules)) 'fragment-rule))

;; Lexer rule with skip command
(define skip-grammar "grammar T; WS : [ \\t\\n]+ -> skip ;")
(let ([ast (parse-g4 skip-grammar)])
  (define rules (any-tree-children (second (any-tree-children ast))))
  (define ws-rule (first rules))
  (check-equal? (any-tree-tag ws-rule) 'lexer-rule)
  ;; Should have a command child
  (define ws-children (any-tree-children ws-rule))
  (check-true (>= (length ws-children) 3))
  (check-equal? (any-tree-tag (third ws-children)) 'command)
  (check-equal? (any-tree-text (third ws-children)) "skip"))

;; Grouped alternatives
(define group-grammar "grammar T; rule : 'a' ('b' | 'c') 'd' ;")
(let ([ast (parse-g4 group-grammar)])
  (check-true (any-tree? ast)))

;; Action block content preserved
(define action-grammar "grammar T; rule : 'x' { print(hello); } 'y' ;")
(let ([ast (parse-g4 action-grammar)])
  (define rules (any-tree-children (second (any-tree-children ast))))
  (define rule1 (first rules))
  ;; Find the action leaf in the rule's children
  (define (find-action node)
    (cond [(cst-leaf? node) (and (eq? (cst-leaf-tag node) 'action) node)]
          [(cst-node? node) (ormap find-action (cst-node-children node))]
          [else #f]))
  (define action-node (find-action rule1))
  (check-true (cst-leaf? action-node))
  (check-equal? (cst-leaf-text action-node) " print(hello); "))

;; Options block
(define opt-grammar "grammar T; options { foo = bar; } rule : 'x' ;")
(let ([ast (parse-g4 opt-grammar)])
  (check-true (any-tree? ast)))

;; ── Full Grammar File Tests ───────────────────────────────────────

;; Parse arithmetic.g4
(let ([ast (parse-g4-file "../grammars-v4/arithmetic/arithmetic.g4")])
  (check-equal? (any-tree-tag ast) 'grammar)
  (check-equal? (any-tree-text (first (any-tree-children ast))) "arithmetic"))

;; Parse JSON.g4
(let ([ast (parse-g4-file "../grammars-v4/json/JSON.g4")])
  (check-equal? (any-tree-tag ast) 'grammar)
  (check-equal? (any-tree-text (first (any-tree-children ast))) "JSON"))

;; ── Round-trip: parse -> sexp -> tree -> sexp ─────────────────────

(let* ([ast (parse-g4-file "../grammars-v4/json/JSON.g4")]
       [s (tree->sexp ast)]
       [back (sexp->tree s)])
  (check-equal? (any-tree-tag back) 'grammar)
  (check-equal? (any-tree-text (first (any-tree-children back))) "JSON"))

(displayln "All g4-parse tests passed.")
