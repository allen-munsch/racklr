#lang racket

(require rackunit
         racklr/tree
         racklr/gen-test)

;; ── Load the Rust parser ──────────────────────────────────────────────

(define-values (rs-parse rs-tokenize tok-type tok-value)
  (gen-and-load "../grammars-v4/rust/RustParser.g4"))

;; ── Tokenizer Tests ──────────────────────────────────────────────────

;; Keywords
(let ([tks (rs-tokenize "fn")])
  (check-equal? (length tks) 2 "fn should produce 2 tokens")
  (check-equal? (tok-type (first tks)) 'KW_FN)
  (check-equal? (tok-value (first tks)) "fn")
  (check-equal? (tok-type (second tks)) 'EOF))

(let ([tks (rs-tokenize "let")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_LET))

(let ([tks (rs-tokenize "if")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_IF))

(let ([tks (rs-tokenize "else")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_ELSE))

(let ([tks (rs-tokenize "return")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_RETURN))

(let ([tks (rs-tokenize "struct")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_STRUCT))

(let ([tks (rs-tokenize "enum")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_ENUM))

(let ([tks (rs-tokenize "match")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_MATCH))

(let ([tks (rs-tokenize "loop")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_LOOP))

(let ([tks (rs-tokenize "while")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_WHILE))

(let ([tks (rs-tokenize "for")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_FOR))

(let ([tks (rs-tokenize "impl")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_IMPL))

(let ([tks (rs-tokenize "true")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_TRUE))

(let ([tks (rs-tokenize "false")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KW_FALSE))

;; Identifiers
(let ([tks (rs-tokenize "foo")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'NON_KEYWORD_IDENTIFIER)
  (check-equal? (tok-value (first tks)) "foo"))

;; Integers
(let ([tks (rs-tokenize "42")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'INTEGER_LITERAL)
  (check-equal? (tok-value (first tks)) "42"))

(let ([tks (rs-tokenize "0xff")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'HEX_LITERAL)
  (check-equal? (tok-value (first tks)) "0xff"))

;; Floats
(let ([tks (rs-tokenize "3.14")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'FLOAT_LITERAL)
  (check-equal? (tok-value (first tks)) "3.14"))

;; Strings
(let ([tks (rs-tokenize "\"hello\"")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'STRING_LITERAL)
  (check-equal? (tok-value (first tks)) "\"hello\""))

;; Chars
(let ([tks (rs-tokenize "'a'")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'CHAR_LITERAL))

;; Operators and delimiters
(let ([tks (rs-tokenize "+")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'PLUS))

(let ([tks (rs-tokenize ";")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'SEMI))

(let ([tks (rs-tokenize "{")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'LCURLYBRACE))

(let ([tks (rs-tokenize "}")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'RCURLYBRACE))

(let ([tks (rs-tokenize "(")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'LPAREN))

(let ([tks (rs-tokenize ")")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'RPAREN))

;; Multiple tokens
(let ([tks (rs-tokenize "let x = 1;")])
  (check-equal? (length tks) 6)
  (check-equal? (tok-type (first tks)) 'KW_LET)
  (check-equal? (tok-type (second tks)) 'NON_KEYWORD_IDENTIFIER)
  (check-equal? (tok-value (second tks)) "x")
  (check-equal? (tok-type (third tks)) 'EQ)
  (check-equal? (tok-type (fourth tks)) 'INTEGER_LITERAL)
  (check-equal? (tok-value (fourth tks)) "1")
  (check-equal? (tok-type (fifth tks)) 'SEMI)
  (check-equal? (tok-type (sixth tks)) 'EOF))

;; Whitespace is hidden (channel HIDDEN)
(let ([tks (rs-tokenize "  let   x  =  1  ;  ")])
  (check-equal? (length tks) 6)
  (check-equal? (tok-type (first tks)) 'KW_LET)
  (check-equal? (tok-type (second tks)) 'NON_KEYWORD_IDENTIFIER)
  (check-equal? (tok-value (second tks)) "x"))

;; ── Parser Tests ─────────────────────────────────────────────────────

;; CST structure: crate -> item* EOF
(define (cst-contains-tag? cst tag)
  (let loop ([node cst])
    (cond [(any-tree? node)
           (or (eq? (any-tree-tag node) tag)
               (for/or ([c (any-tree-children node)])
                 (loop c)))]
          [(pair? node)
           (or (loop (car node)) (loop (cdr node)))]
          [else #f])))

;; Empty crate
(let ([cst (rs-parse "")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate))

;; let statement (inside function body — not valid at crate level)
(let ([cst (rs-parse "fn main() { let x = 1; }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'letStatement)))

;; Function definition (empty body)
(let ([cst (rs-parse "fn foo() {}")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'function_)))

;; Function with return type
(let ([cst (rs-parse "fn add(x: i32, y: i32) -> i32 { x + y }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'function_)))

;; If expression (no else)
(let ([cst (rs-parse "fn f() { if true { 1; } }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'ifExpression)))

;; Struct definition (unit)
(let ([cst (rs-parse "struct Foo;")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'struct_)))

;; Enum definition
(let ([cst (rs-parse "enum Color { Red, Green, Blue }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'enumeration)))

;; Return statement (part of expression rule, no separate CST tag)
(let ([cst (rs-parse "fn f() -> i32 { return 42; }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate))

;; Loop
(let ([cst (rs-parse "fn f() { loop { break; } }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'loopExpression)))

;; While loop
(let ([cst (rs-parse "fn f() { while true { break; } }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'predicateLoopExpression)))

;; Match expression
(let ([cst (rs-parse "fn f(x: i32) -> i32 { match x { 1 => 10, _ => 0 } }")])
  (check-true (any-tree? cst))
  (check-equal? (any-tree-tag cst) 'crate)
  (check-true (cst-contains-tag? cst 'matchExpression)))

;; ── Rejection Tests ──────────────────────────────────────────────────

;; Garbage
(check-reject rs-parse "@#$%")

;; Unclosed brace
(check-reject rs-parse "fn foo() {")

;; Clean up temp files
(cleanup)
