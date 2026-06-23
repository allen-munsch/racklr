#lang racket

(require rackunit
         racklr/tree
         racklr/gen-test)

;; ── Load the TypeScript parser ────────────────────────────────────────

(define-values (ts-parse ts-tokenize tok-type tok-value)
  (gen-and-load "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

;; ── Tokenizer Tests ──────────────────────────────────────────────────

;; TypeScript-specific keywords
(let ([tks (ts-tokenize "as")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'As))

(let ([tks (ts-tokenize "readonly")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'ReadOnly))

(let ([tks (ts-tokenize "type")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'TypeAlias))

(let ([tks (ts-tokenize "interface")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Interface))

(let ([tks (ts-tokenize "enum")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Enum))

(let ([tks (ts-tokenize "namespace")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Namespace))

(let ([tks (ts-tokenize "declare")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Declare))

(let ([tks (ts-tokenize "abstract")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Abstract))

(let ([tks (ts-tokenize "is")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Is))

(let ([tks (ts-tokenize "keyof")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'KeyOf))

(let ([tks (ts-tokenize "any")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Any))

(let ([tks (ts-tokenize "number")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Number))

(let ([tks (ts-tokenize "string")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'String))

(let ([tks (ts-tokenize "boolean")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Boolean))

(let ([tks (ts-tokenize "void")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Void))

(let ([tks (ts-tokenize "never")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'Never))

;; Decorator
(let ([tks (ts-tokenize "@")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'At))

;; Optional chaining operator
(let ([tks (ts-tokenize "?.")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'QuestionMarkDot))

;; ?? operator
(let ([tks (ts-tokenize "??")])
  (check-equal? (length tks) 2)
  (check-equal? (tok-type (first tks)) 'NullCoalesce))

;; ── CST Helper ──────────────────────────────────────────────────────

(define (cst-contains-tag? cst tag)
  (let loop ([node cst])
    (cond [(cst-node? node)
           (or (eq? (cst-node-tag node) tag)
               (for/or ([c (cst-node-children node)])
                 (loop c)))]
          [(pair? node)
           (for/or ([c node]) (loop c))]
          [else #f])))

;; ── Parser Tests: Basic Type Annotations ──────────────────────────────

(let ([cst (ts-parse "let x: number = 42;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "const name: string = \"hello\";")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "var flag: boolean = true;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "let x: any = 42;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Functions with Type Annotations ──────────────────────────────────

(let ([cst (ts-parse "function greet(name: string): string { return \"hello \" + name; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "const add = (a: number, b: number): number => a + b;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "function log(msg: string): void { console.log(msg); }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Generics ─────────────────────────────────────────────────────────

(let ([cst (ts-parse "function identity<T>(arg: T): T { return arg; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "const map = <T, U>(items: T[], fn: (x: T) => U): U[] => items.map(fn);")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Interfaces ───────────────────────────────────────────────────────

(let ([cst (ts-parse "interface Point { x: number; y: number; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "interface Named { name: string; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "interface Shape extends Named { area(): number; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Type Aliases ─────────────────────────────────────────────────────

(let ([cst (ts-parse "type ID = string | number;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "type Handler = (event: string) => void;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Enums ────────────────────────────────────────────────────────────

(let ([cst (ts-parse "enum Color { Red, Green, Blue }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "enum Status { Active = \"ACTIVE\", Inactive = \"INACTIVE\" }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Namespaces ───────────────────────────────────────────────────────

(let ([cst (ts-parse "namespace Utils { export function log(msg: string): void {} }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Classes (with type annotations) ──────────────────────────────────

(let ([cst (ts-parse "class Point { x: number; y: number; }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Async/Await ──────────────────────────────────────────────────────

(let ([cst (ts-parse "async function fetch(): Promise<Response> { return await fetch(\"url\"); }")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

(let ([cst (ts-parse "const load = async (): Promise<Data> => { const r = await fetch(\"\"); return r.json(); };")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Union and Intersection Types ──────────────────────────────────────

(let ([cst (ts-parse "type Result = Success | Failure;")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Object Types ──────────────────────────────────────────────────────

(let ([cst (ts-parse "type Config = { readonly name: string; port?: number; };")])
  (check-true (cst-node? cst))
  (check-equal? (cst-node-tag cst) 'program))

;; ── Rejection Tests ──────────────────────────────────────────────────

(check-reject ts-parse "@#$%")
(check-reject ts-parse "const x: ")
(check-reject ts-parse "1 +")

;; Clean up temp files
(cleanup)
