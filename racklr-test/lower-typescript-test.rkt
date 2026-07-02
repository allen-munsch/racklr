#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-typescript
         racklr/emit-javascript)

;; ── Load the TypeScript parser ──────────────────────────────────────

(define-values (ts-parse ts-tokenize tok-type tok-value)
  (gen-and-load "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

;; ── Test helper ─────────────────────────────────────────────────────

(define (check-ts input)
  (define cst (ts-parse input))
  (when (not cst)
    (error 'check-ts "parse failed for: ~s" input))
  (define uir (lower-program cst tok-type tok-value))
  (printf "TS: ~s\n  UIR: ~a\n" input (uir->sexp uir))
  (define emitted (emit-javascript uir))
  (printf "  JS:  ~a\n" emitted)
  (values uir emitted))

;; ── Lowering + Emit Tests: Type Annotation Stripping ────────────────

;; Variable with type annotation — type stripped, JS output should have no ':'
(let-values ([(uir emitted) (check-ts "let x: number = 42;")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "number"))
  (check-true (string-contains? emitted "let"))
  (check-true (string-contains? emitted "42")))

;; Variable without type annotation — works normally
(let-values ([(uir emitted) (check-ts "let y = 42;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "42")))

;; Const with string type annotation
(let-values ([(uir emitted) (check-ts "const name: string = \"hello\";")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "string")))

;; Var with boolean type annotation
(let-values ([(uir emitted) (check-ts "var flag: boolean = true;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "true")))

;; Let with any type annotation
(let-values ([(uir emitted) (check-ts "let x: any = 42;")])
  (check-equal? (uir-tag uir) 'block))

;; ── Functions with Type Annotations ──────────────────────────────────

(let-values ([(uir emitted) (check-ts "function greet(name: string): string { return \"hello \" + name; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "greet"))
  (check-true (string-contains? emitted "return"))
  (check-false (string-contains? emitted ": string")))

(let-values ([(uir emitted) (check-ts "function log(msg: string): void { }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "log")))

;; Arrow function with type annotations
(let-values ([(uir emitted) (check-ts "const add = (a: number, b: number) => a + b;")])
  (check-equal? (uir-tag uir) 'block))

;; ── Generics (type parameter stripping) ─────────────────────────────

(let-values ([(uir emitted) (check-ts "function identity<T>(arg: T): T { return arg; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "function"))
  (check-true (string-contains? emitted "identity")))

;; ── Interface — should be elided (no runtime equivalent) ────────────

(let-values ([(uir emitted) (check-ts "interface Point { x: number; y: number; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "interface"))
  (check-false (string-contains? emitted "Point")))

;; ── Type Alias — should be elided ───────────────────────────────────

(let-values ([(uir emitted) (check-ts "type ID = string | number;")])
  (check-equal? (uir-tag uir) 'block)
  (check-false (string-contains? emitted "type"))
  (check-false (string-contains? emitted "ID")))

;; ── Enum — lowered to a call ────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "enum Color { Red, Green, Blue }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "Color")))

;; ── Namespace — lowered to a call ───────────────────────────────────

(let-values ([(uir emitted) (check-ts "namespace Utils { export function f(): void {} }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "Utils")))

;; ── As expression (type cast) — cast stripped ──────────────────────

(let-values ([(uir emitted) (check-ts "let x = 42 as number;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "42"))
  (check-false (string-contains? emitted "as"))
  (check-false (string-contains? emitted "number")))

;; ── Non-null assertion — stripped ───────────────────────────────────

(let-values ([(uir emitted) (check-ts "let x = foo!;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "foo"))
  (check-false (string-contains? emitted "!")))

;; ── Expression statements ───────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "1 + 2;")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "+")))

(let-values ([(uir emitted) (check-ts "console.log(\"hello\");")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "console")))

;; ── New expression ───────────────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "new Date();")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "new Date()"))
  (check-false (string-contains? emitted "new Date()()")))

;; ── If statement ────────────────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "if (true) { 1; } else { 2; }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "if")))

;; ── Class declaration ────────────────────────────────────────────────

(let-values ([(uir emitted) (check-ts "class Greeter { greet() { return 1; } }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "class Greeter"))
  (check-true (string-contains? emitted "greet()"))
  (check-true (string-contains? emitted "return 1")))

(let-values ([(uir emitted) (check-ts "class Foo extends Bar { baz(x) { return x; } }")])
  (check-equal? (uir-tag uir) 'block)
  (check-true (string-contains? emitted "class Foo"))
  (check-true (string-contains? emitted "extends Bar"))
  (check-true (string-contains? emitted "baz(x)")))

;; ── Clean up temp files ─────────────────────────────────────────────

(cleanup)
