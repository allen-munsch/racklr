#lang racket

;; Effect inference (Pass 2.4).
;;
;; Computes effect rows for functions lacking annotations via a monotone
;; fixpoint over the call graph. Semantics follow docs/effects-proposal.md §6:
;;   - build the call graph over named uir-fn nodes
;;   - initialize every function's row to ⊥ (empty)
;;   - iterate to fixpoint: row = union(direct effects, callee rows)
;;   - direct effects: llm/openai → llm, db/database/sql/Model.objects → db,
;;     fetch/axios → network, revalidatePath/invalidate → cache
;;   - unresolvable (higher-order) calls → an entry named 'unknown with
;;     modality 'unknown, deferred to runtime

(require racklr/uir
         racklr/effects/contract)

(provide
  infer-effects
  annotate-effects
  collect-functions
  classify-call
  direct-effect-of-callee
  empty-row)

;; ── generic structural walk ─────────────────────────────────────────

;; Visit every struct node reachable from `n` (a node or a plain list),
;; applying `visit!` to each struct node exactly once (pre-order).
(define (walk-uir n visit!)
  (cond
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-uir f visit!))]
    [(pair? n)
     (walk-uir (car n) visit!)
     (walk-uir (cdr n) visit!)]
    [else (void)]))

;; Like walk-uir but does not descend into nested uir-fn nodes: a nested
;; function is a separate call-graph node whose own body is analyzed on its
;; own, so its effects must not leak into the enclosing function.
(define (walk-body n visit!)
  (cond
    [(uir-fn? n) (void)]
    [(arrow-call? n) (void)]
    [(function-expr-call? n) (void)]
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-body f visit!))]
    [(pair? n)
     (walk-body (car n) visit!)
     (walk-body (cdr n) visit!)]
    [else (void)]))

;; ── callee classification ───────────────────────────────────────────

;; Leftmost root name of a (possibly dotted/namespaced) callee expression,
;; as a symbol. Namespace-qualified names split on '/': "llm/query" → llm.
(define (dotted-root c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (loop (uir-get-base c))]
      [(dot-call? c) (loop (first (uir-call-args c)))]
      [(uir-var? c) (symbol-root (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (symbol-root (uir-symbol-name c))]
      [else #f])))

;; Root of a (possibly namespace-qualified) symbol name string.
(define (symbol-root name)
  (let loop ([i 0])
    (cond [(>= i (string-length name)) (string->symbol name)]
          [(char=? (string-ref name i) #\/) (string->symbol (substring name 0 i))]
          [else (loop (add1 i))])))

;; A JS/TS/JSX member access: `a.b` lowers to (call (symbol "dot") (list a b)).
(define (dot-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "dot")
       (= (length (uir-call-args c)) 2)))

;; A JS/TS/JSX arrow function expression: (call "=>" (list params body)).
(define (arrow-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "=>")
       (= (length (uir-call-args c)) 2)))

;; A JS/TS anonymous function expression: (call "function" (list params body)),
;; a sibling of `=>`.
(define (function-expr-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "function")
       (= (length (uir-call-args c)) 2)))

;; Field name of a `uir-get` as a symbol. Front ends differ on whether an
;; attribute name is a uir-symbol or a uir-string, so accept both.
(define (get-field-symbol f)
  (cond
    [(uir-symbol? f) (string->symbol (uir-symbol-name f))]
    [(uir-string? f) (string->symbol (uir-string-value f))]
    [else #f]))

;; Does a dotted chain contain a field access named `field-name`? Handles both
;; the Python uir-get encoding and the JS/TS (call "dot" ...) encoding.
(define (dotted-has-field? c field-name)
  (let loop ([c c])
    (cond
      [(uir-get? c)
       (or (eq? (get-field-symbol (uir-get-field c)) field-name)
           (loop (uir-get-base c)))]
      [(dot-call? c)
       (define field (second (uir-call-args c)))
       (or (and (uir-symbol? field)
                (string=? (uir-symbol-name field) (symbol->string field-name)))
           (loop (first (uir-call-args c))))]
      [else #f])))

;; Simple (non-dotted) callee name: a uir-var or uir-symbol.
(define (callee-name c)
  (cond
    [(uir-var? c) (string->symbol (uir-symbol-name (uir-var-name c)))]
    [(uir-symbol? c) (string->symbol (uir-symbol-name c))]
    [else #f]))

;; Directly-performed effect for a call's callee, or #f.
(define (direct-effect-of-callee c)
  (define root (dotted-root c))
  (define nm (callee-name c))
  (cond
    [(and root (memq root '(llm openai anthropic cohere ollama groq agno langgraph langchain llamaindex))) 'llm]
    [(and root (memq root '(db database sql sqlalchemy sqlmodel asyncpg psycopg))) 'db]
    [(dotted-has-field? c 'objects) 'db]  ;; Django Model.objects
    [(and root (memq root '(fetch axios httpx requests aiohttp urllib))) 'network]
    [(and nm (memq nm '(fetch axios))) 'network]
    [(and root (memq root '(redis memcache cache))) 'cache]
    [(and nm (memq nm '(revalidatePath revalidate_path invalidate clearCache purgeCache))) 'cache]
    [else #f]))

;; Classify a call callee against the known function names:
;;   (cons 'edge name)     — call to a function in the program
;;   (cons 'pure #f)       — front-end-encoded construct (operator/builtin)
;;   (cons 'effect name)   — directly-detected effect
;;   (cons 'unknown #f)    — unresolvable (higher-order / IIFE)
(define (classify-call callee known-names)
  (define nm (callee-name callee))
  (cond
    [(and nm (hash-has-key? known-names nm)) (cons 'edge nm)]
    [(synthetic-callee? callee) (cons 'pure #f)]
    [else
     (define eff (direct-effect-of-callee callee))
     (cond
       [eff (cons 'effect eff)]
       [else (cons 'unknown #f)])]))

;; Operator / statement / special-form names the front ends encode as literal
;; `(uir-call (uir-symbol NAME) ...)`. These are language constructs, not
;; runtime-dispatched functions, so they perform no effect and are not
;; `unknown`. (Real calls lower to a uir-var callee in Python/TS and are
;; checked against known-names before reaching here.)
(define synthetic-callees
  (set
   ;; JS/TS/JSX structural calls
   "dot" "index" "while" "dowhile" "with" "for" "forin" "forof" "switch"
   "break" "continue" "debugger" "label" "regex" "function" "async-fn"
   "gen-fn" "import" "export" "throw" "try" "=>" "var" "let" "const"
   "void" "typeof" "delete" "array-bind" "object-bind"
   ;; arithmetic / comparison / assignment / logical / unary
   "+" "-" "*" "/" "%" "=" "==" "===" "!=" "!==" "<" ">" "<=" ">="
   "&&" "||" "**" "+=" "-=" "*=" "/=" "**=" "!" "~" "&" "|" "^"
   "<<" ">>" ">>>" "postfix++" "postfix--" "prefix++" "prefix--"
   ;; Python structural calls and comprehensions
   "not" "tuple" "set" "list" "assert" "raise" "del" "global" "nonlocal"
   "list-comp" "dict-comp" "set-comp" "gen-comp"
   ;; Clojure special forms / data constructors
   "quote" "syntax-quote" "unquote" "unquote-splicing" "deref" "var"
   "vector" "hash-set" "fn*" "with-meta" "namespace" "host-expr"))

(define (synthetic-callee? c)
  (and (uir-symbol? c)
       (set-member? synthetic-callees (uir-symbol-name c))))

;; ── JS/TS/JSX function encodings ───────────────────────────────────

;; The uir-fn wrapped by a JS/TS function value: either the fn itself, or
;; (call "async-fn"/"gen-fn" (list fn)). Returns #f otherwise.
(define (wrapped-fn val)
  (cond
    [(uir-fn? val) val]
    [(and (uir-call? val)
          (uir-symbol? (uir-call-callee val))
          (member (uir-symbol-name (uir-call-callee val)) '("async-fn" "gen-fn"))
          (= (length (uir-call-args val)) 1)
          (uir-fn? (first (uir-call-args val))))
     (first (uir-call-args val))]
    [else #f]))

;; (name . body) for a named JS/TS function definition:
;;   (set! NAME (fn #f ...))                       — function declaration
;;   (set! NAME (call "async-fn"/"gen-fn" (fn)))   — async / generator decl
;;   (set! NAME (call "=>" (list params body)))    — arrow bound to a name
;;   (set! NAME (call "function" (list params body))) — function expr bound
;; Returns #f for anything else.
(define (named-function n)
  (and (uir-set!? n)
       (uir-symbol? (uir-set!-name n))
       (let* ([nm (string->symbol (uir-symbol-name (uir-set!-name n)))]
              [val (uir-set!-value n)]
              [fn (wrapped-fn val)])
         (cond
           [fn (cons nm (uir-fn-body fn))]
           [(arrow-call? val) (cons nm (second (uir-call-args val)))]
           [(function-expr-call? val) (cons nm (second (uir-call-args val)))]
           [else #f]))))

;; ── rows ────────────────────────────────────────────────────────────

(define (empty-row)
  (uir-effect-row '()))

;; Canonicalize row entry order so equal? is order-insensitive.
(define (canonical-row r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

;; A list of effect-name symbols → a row. 'unknown names get modality
;; 'unknown; everything else is a detected 'may.
(define (effects->row syms)
  (canonical-row
   (uir-effect-row
    (for/list ([s (in-list (remove-duplicates syms))])
      (uir-effect-entry
       (uir-symbol (symbol->string s))
       (if (eq? s 'unknown) 'unknown 'may)
       '()
       #f
       #f)))))

;; Union that keeps the deferred 'unknown modality (contract's union
;; collapses unknown→may for same-name merges).
(define (infer-row-union r1 r2)
  (canonical-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (effect-row-union r1 r2)))])
      (if (eq? (effect-name-symbol e) 'unknown)
          (struct-copy uir-effect-entry e [modality 'unknown])
          e)))))

;; ── collection and fixpoint ─────────────────────────────────────────

;; name-symbol → function body, for every named function reachable from
;; `program`: named uir-fn nodes plus the JS/TS `set!`-bound encodings
;; (function declarations, async/generator declarations, and bound arrows).
(define (collect-functions program)
  (define h (make-hash))
  (define (visit! n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (hash-set! h (string->symbol (uir-symbol-name nm)) (uir-fn-body n))
             (void)))]
      [(named-function n)
       => (λ (p) (hash-set! h (car p) (cdr p)))]
      [else (void)]))
  (walk-uir program visit!)
  h)

;; Compute the fixpoint effect rows: name-symbol → uir-effect-row.
(define (infer-effects program)
  (define fns (collect-functions program))
  (define known (make-hash))
  (for ([(n _) (in-hash fns)]) (hash-set! known n #t))

  (define direct-rows (make-hash))  ;; name → uir-effect-row (direct only)
  (define edges (make-hash))        ;; name → (listof name-symbol)

  (for ([(n body) (in-hash fns)])
    (define effects '())
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
          (let ([cls (classify-call (uir-call-callee node) known)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows n (effects->row effects))
    (hash-set! edges n (sort (remove-duplicates callees) symbol<?)))

  (define rows (make-hash))
  (for ([(n _) (in-hash fns)])
    (hash-set! rows n (hash-ref direct-rows n)))

  (let loop ()
    (define changed? #f)
    (for ([(n _) (in-hash fns)])
      (define current (hash-ref rows n))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges n)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows n) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin
            (set! changed? #t)
            (hash-set! rows n new-row))))
    (if changed? (loop) (void)))

  rows)

;; ── write-back ──────────────────────────────────────────────────────

;; Rewrite a program so every named function carries its inferred effect row
;; in the `effects` field: named uir-fn nodes, plus the JS/TS `set!`-bound
;; function declarations (writing back into the wrapped uir-fn).
(define (annotate-effects program)
  (define rows (infer-effects program))
  ;; Materialize an inferred row for emission: 'unknown is a checker signal
  ;; ("could not verify"), not a runtime effect, and the runtime guards reject
  ;; the 'unknown modality. So it is dropped here; an all-unknown row becomes
  ;; empty and the function stays unannotated.
  (define (concrete-row r)
    (uir-effect-row
     (filter (λ (e) (not (eq? (effect-name-symbol e) 'unknown)))
             (uir-effect-row-entries r))))
  (define (with-effects fn nm)
    (define existing (uir-fn-effects fn))
    (define inferred (hash-ref rows nm #f))
    (cond
      ;; A hand-written row wins: it carries budgets/scopes inference cannot
      ;; recover, so re-inference must not overwrite it.
      [(uir-effect-row? existing) fn]
      [(and inferred (pair? (uir-effect-row-entries inferred)))
       (define concrete (concrete-row inferred))
       (if (pair? (uir-effect-row-entries concrete))
           (struct-copy uir-fn fn [effects concrete])
           fn)]
      [else fn]))
  (define (rewrite-value val nm)
    (cond
      [(uir-fn? val) (with-effects val nm)]
      [(and (uir-call? val)
            (uir-symbol? (uir-call-callee val))
            (member (uir-symbol-name (uir-call-callee val)) '("async-fn" "gen-fn"))
            (= (length (uir-call-args val)) 1)
            (uir-fn? (first (uir-call-args val))))
       (struct-copy uir-call val
                    [args (list (with-effects (first (uir-call-args val)) nm))])]
      [else val]))
  (define (rewrite n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (with-effects n (string->symbol (uir-symbol-name nm)))
             n))]
      [(and (uir-set!? n) (uir-symbol? (uir-set!-name n)))
       (struct-copy uir-set! n
                    [value (rewrite-value (uir-set!-value n)
                                          (string->symbol (uir-symbol-name (uir-set!-name n))))])]
      [else n]))
  (cond
    [(list? program) (map rewrite program)]
    [(uir-block? program)
     (struct-copy uir-block program
                  [stmts (map rewrite (uir-block-stmts program))])]
    [(uir-module? program)
     (struct-copy uir-module program
                  [body (map rewrite (uir-module-body program))])]
    [else (rewrite program)]))

(module+ main
  (displayln "racklr/effects/infer — effect inference loaded."))
