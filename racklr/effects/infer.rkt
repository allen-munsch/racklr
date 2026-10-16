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
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-body f visit!))]
    [(pair? n)
     (walk-body (car n) visit!)
     (walk-body (cdr n) visit!)]
    [else (void)]))

;; ── callee classification ───────────────────────────────────────────

;; Leftmost root name of a (possibly dotted) callee expression, as a symbol.
(define (dotted-root c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (loop (uir-get-base c))]
      [(uir-var? c) (string->symbol (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (string->symbol (uir-symbol-name c))]
      [else #f])))

;; Does a dotted chain contain a `uir-get` whose field is `field-name`?
(define (dotted-has-field? c field-name)
  (let loop ([c c])
    (cond
      [(uir-get? c)
       (or (and (uir-symbol? (uir-get-field c))
                (eq? (string->symbol (uir-symbol-name (uir-get-field c))) field-name))
           (loop (uir-get-base c)))]
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
    [(and root (memq root '(llm openai))) 'llm]
    [(and root (memq root '(db database sql))) 'db]
    [(dotted-has-field? c 'objects) 'db]  ;; Django Model.objects
    [(and root (memq root '(fetch axios))) 'network]
    [(and nm (memq nm '(fetch axios))) 'network]
    [(and nm (memq nm '(revalidatePath revalidate_path invalidate clearCache purgeCache))) 'cache]
    [else #f]))

;; Classify a call callee against the known function names:
;;   (cons 'edge name)     — call to a function in the program
;;   (cons 'effect name)   — directly-detected effect
;;   (cons 'unknown #f)    — unresolvable (higher-order / IIFE)
(define (classify-call callee known-names)
  (define nm (callee-name callee))
  (cond
    [(and nm (hash-has-key? known-names nm)) (cons 'edge nm)]
    [else
     (define eff (direct-effect-of-callee callee))
     (cond
       [eff (cons 'effect eff)]
       [else (cons 'unknown #f)])]))

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

;; name-symbol → uir-fn node, for every named uir-fn reachable from `program`.
(define (collect-functions program)
  (define h (make-hash))
  (define (visit! n)
    (if (uir-fn? n)
        (let ([nm (uir-fn-name n)])
          (if (and nm (uir-symbol? nm))
              (hash-set! h (string->symbol (uir-symbol-name nm)) n)
              (void)))
        (void)))
  (walk-uir program visit!)
  h)

;; Compute the fixpoint effect rows: name-symbol → uir-effect-row.
(define (infer-effects program)
  (define fns (collect-functions program))
  (define known (make-hash))
  (for ([(n _) (in-hash fns)]) (hash-set! known n #t))

  (define direct-rows (make-hash))  ;; name → uir-effect-row (direct only)
  (define edges (make-hash))        ;; name → (listof name-symbol)

  (for ([(n fn) (in-hash fns)])
    (define effects '())
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
          (let ([cls (classify-call (uir-call-callee node) known)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body (uir-fn-body fn) visit!)
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

;; Rewrite a program so every top-level named uir-fn carries its inferred
;; effect row in the `effects` field.
(define (annotate-effects program)
  (define rows (infer-effects program))
  (define (rewrite-fn n)
    (if (uir-fn? n)
        (let ([nm (uir-fn-name n)])
          (if (and nm (uir-symbol? nm))
              (struct-copy uir-fn n
                           [effects (hash-ref rows
                                              (string->symbol (uir-symbol-name nm))
                                              (uir-fn-effects n))])
              n))
        n))
  (cond
    [(list? program) (map rewrite-fn program)]
    [(uir-block? program)
     (struct-copy uir-block program
                  [stmts (map rewrite-fn (uir-block-stmts program))])]
    [(uir-module? program)
     (struct-copy uir-module program
                  [body (map rewrite-fn (uir-module-body program))])]
    [else (rewrite-fn program)]))

(module+ main
  (displayln "racklr/effects/infer — effect inference loaded."))
