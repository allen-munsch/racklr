#lang racket

;; JSDoc @effects extraction (the gap unblocked by comment preservation).
;; Unit-tests the row parser plus end-to-end lowering + emit through the
;; comment-preserving TypeScript parser.

(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/effects/jsdoc-annot
         racklr/lower-typescript
         racklr/emit-javascript)

(define-values (ts-parse ts-tokenize tok-type tok-value)
  (gen-and-load-comments "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

;; ── helpers ─────────────────────────────────────────────────────────

(define (row-lookup row name)
  (for/or ([e (in-list (uir-effect-row-entries row))])
    (and (equal? (uir-symbol-name (uir-effect-entry-name e)) name) e)))

(define (lower+emit src)
  (define cst (ts-parse src))
  (unless cst (error 'lower+emit "parse failed: ~s" src))
  (define toks (ts-tokenize src))
  (emit-javascript (lower-program cst tok-type tok-value toks)))

;; ── row parser (extract-jsdoc-row) ──────────────────────────────────

(check-false (extract-jsdoc-row "/* nothing here */"))
(check-false (extract-jsdoc-row "/** @other { llm: {} } */"))

(let ([row (extract-jsdoc-row "/** @effects { llm: { max: 5 } } */")])
  (check-true (uir-effect-row? row))
  (check-equal? (length (uir-effect-row-entries row)) 1)
  (define e (row-lookup row "llm"))
  (check-not-false e)
  (check-equal? (uir-effect-entry-modality e) 'may)
  (check-equal? (uir-effect-entry-budget e) 5))

(let ([row (extract-jsdoc-row "// @effects { db: { writes: \"table\" }, net: { ops: [\"fetch\", \"post\"] } }")])
  (check-equal? (length (uir-effect-row-entries row)) 2)
  (define db (row-lookup row "db"))
  (check-equal? (uir-symbol-name (uir-effect-entry-scope db)) "writes=table")
  (define net (row-lookup row "net"))
  (check-equal? (map uir-symbol-name (uir-effect-entry-ops net)) '("fetch" "post")))

;; Multiline JSDoc with leading ` * ` decoration.
(let ([row (extract-jsdoc-row "/**\n * @effects { llm: { modality: \"must\" } }\n */")])
  (check-equal? (uir-effect-entry-modality (row-lookup row "llm")) 'must))

;; ── end-to-end: function declaration ────────────────────────────────

(let ([out (lower+emit "/** @effects { llm: { max: 5 } } */\nfunction f() { return llm.query('hi'); }")])
  (check-true (string-contains? out "__withEffects"))
  (check-true (string-contains? out "\"llm\""))
  (check-true (string-contains? out "\"max\": 5"))
  (check-true (string-contains? out "const f = __withEffects")))

;; ── end-to-end: const-bound arrow ───────────────────────────────────

(let ([out (lower+emit "/** @effects { llm: { max: 3 } } */\nconst g = () => llm.query('hi');")])
  (check-true (string-contains? out "__withEffects"))
  (check-true (string-contains? out "const g = __withEffects"))
  (check-true (string-contains? out "\"max\": 3")))

;; ── end-to-end: export const arrow ──────────────────────────────────

(let ([out (lower+emit "/** @effects { db: { writes: \"t\" } } */\nexport const h = () => db.write('t');")])
  (check-true (string-contains? out "__withEffects"))
  (check-true (string-contains? out "\"db\""))
  (check-true (string-contains? out "\"writes=t\"")))

;; ── annotation attaches to the *following* declaration, not the one before ──

(let ([out (lower+emit
            "function before() { return 1; }\n/** @effects { llm: {} } */\nfunction after() { return llm.query('hi'); }")])
  (check-true (string-contains? out "const after = __withEffects"))
  (check-false (string-contains? out "const before = __withEffects")))

;; ── no comment: unchanged fast path ─────────────────────────────────

(let ([out (lower+emit "function plain() { return 1; }")])
  (check-false (string-contains? out "__withEffects")))

(cleanup)
(displayln "All jsdoc-annot tests passed.")
