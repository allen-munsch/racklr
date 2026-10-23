#lang racket

;; racklr/hypergraph.rkt
;;
;; A dependency-free typed-hypergraph kernel: the AtomSpace idea (typed
;; nodes, typed hyperlinks, a lattice over link slots, set-of-links algebra,
;; and graph pattern matching) applied to the two places racklr currently
;; hand-rolls the same math:
;;
;;   1. Effect rows (racklr/effects/contract.rkt). An entry
;;      (name, modality, ops, budget, scope) is a 5-slot hyperedge; a row is
;;      a set of hyperedges; union/intersection/difference/subtyping are the
;;      join/meet/difference/≤ of those sets under a per-slot lattice.
;;
;;   2. Types. A base type is a node; a type constructor is a hyperedge
;;      (fn, listof, ref). Subtyping is a recursive relation over those
;;      hyperedges with variance, and an effect row is embedded in a
;;      function type so effect subtyping composes with type subtyping.
;;
;; The claim this file demonstrates: "combine/compare typed hyperedges under
;; a per-slot lattice" is ONE operation that both domains reduce to, so the
;; hand-written ~170-line effect algebra in contract.rkt collapses into the
;; generic machinery below, and new typed relations become data.

(provide
  ;; atoms
  node? node node-type node-name
  link? link link-type link-out
  var? var var-name
  ;; lattices
  lattice? lattice lattice-join lattice-meet lattice-leq
  ;; generic per-slot algebra over link out-sets
  slots-join slots-meet slots-leq
  ;; effect-row instance
  effect-entry effect-entry? effect-key effect-key?
  effect-entry-join effect-entry-meet effect-entry-leq
  effect-row effect-row-join effect-row-meet effect-row-diff effect-row-leq
  ;; pattern matching / query
  hg-match? hg-query
  ;; generic AST / S-expression materialization
  materialize
  ;; type instance
  t-base t-fn t-listof t-ref
  type-leq
  ;; capability instance (Effervesce-style needs/provides)
  cap-ref cap-satisfied?)

;; ── atoms ───────────────────────────────────────────────────────────

;; A typed, named vertex (AtomSpace Node).
(struct node (type name) #:transparent)

;; A typed hyperedge: `out` is the ordered list of connected atoms
;; (AtomSpace Link; arity is unconstrained, which is what makes it a
;; *hyper*edge rather than an edge).
(struct link (type out) #:transparent)

;; A pattern variable for hg-match?/hg-query.
(struct var (name) #:transparent)

;; ── lattices ────────────────────────────────────────────────────────

;; A bounded lattice over a value domain: join (least upper bound),
;; meet (greatest lower bound), and the partial order ≤. `leq` is
;; (≤ sub super): sub is at most super in the order.
(struct lattice (join meet leq) #:transparent)

;; Modality: 'may | 'must | 'must-not | 'unknown.
(define (modality-join a b)
  (cond [(or (eq? a 'must-not) (eq? b 'must-not)) 'must-not]
        [(or (eq? a 'must) (eq? b 'must)) 'must]
        [else 'may]))
(define (modality-meet a b)
  (cond [(or (eq? a 'must-not) (eq? b 'must-not)) 'must-not]
        [(and (eq? a 'must) (eq? b 'must)) 'must]
        [else 'may]))
(define (modality-leq sub super)
  (case sub
    [(may unknown) (memq super '(may must unknown))]
    [(must) (eq? super 'must)]
    [(must-not) #t]
    [else #f]))

;; Budget: #f is unbounded (⊤ of the ≤ order); join sums (unbounded absorbs),
;; meet takes the min.
(define (budget-join a b)
  (cond [(or (not a) (not b)) #f]
        [else (+ a b)]))
(define (budget-meet a b)
  (cond [(and (not a) (not b)) #f]
        [(not a) b]
        [(not b) a]
        [else (min a b)]))
(define (budget-leq sub super)
  (or (not super) (and sub (<= sub super))))

;; Ops: a list of operation symbols; '() means "all operations" (⊤).
(define (ops-join a b)
  (cond [(or (null? a) (null? b)) '()]
        [else (remove-duplicates (append a b))]))
(define (ops-meet a b)
  (cond [(and (null? a) (null? b)) '()]
        [(null? a) b]
        [(null? b) a]
        [else (filter (λ (x) (memq x b)) a)]))
(define (ops-leq sub super)
  (or (null? super)
      (and (not (null? sub))
           (andmap (λ (s) (memq s super)) sub))))

;; Scope: #f is unconstrained (⊥); equal values merge, conflict → #f.
;; NOTE: contract.rkt never checks scope in subtyping; scope-leq is
;; always-true to preserve that (see the report for why this matters).
(define (scope-merge a b)
  (cond [(not a) b]
        [(not b) a]
        [(equal? a b) a]
        [else #f]))
(define (scope-leq sub super) #t)

(define modality-lattice (lattice modality-join modality-meet modality-leq))
(define budget-lattice   (lattice budget-join   budget-meet   budget-leq))
(define ops-lattice      (lattice ops-join      ops-meet      ops-leq))
(define scope-lattice    (lattice scope-merge   scope-merge   scope-leq))

;; ── generic per-slot algebra ────────────────────────────────────────

;; Join/meet the non-key slots of two equal-type links pointwise, using the
;; lattice at each slot position. Returns nodes (slots-join/meet) or a
;; boolean (slots-leq).
(define (slots-join lattices outs-a outs-b)
  (for/list ([la (in-list lattices)]
             [na (in-list outs-a)]
             [nb (in-list outs-b)])
    (node (node-type na) ((lattice-join la) (node-name na) (node-name nb)))))

(define (slots-meet lattices outs-a outs-b)
  (for/list ([la (in-list lattices)]
             [na (in-list outs-a)]
             [nb (in-list outs-b)])
    (node (node-type na) ((lattice-meet la) (node-name na) (node-name nb)))))

(define (slots-leq lattices outs-a outs-b)
  (andmap (λ (la na nb) ((lattice-leq la) (node-name na) (node-name nb)))
          lattices outs-a outs-b))

;; ── effect-row instance ─────────────────────────────────────────────

;; Slot order of an effect hyperedge: [name, modality, ops, budget, scope].
;; name is the key; the four remaining slots each carry a lattice, in the
;; same order.
(define effect-lattices (list modality-lattice ops-lattice budget-lattice scope-lattice))

(define (effect-entry name modality ops budget scope)
  (link 'effect (list (node 'name name)
                      (node 'modality modality)
                      (node 'ops ops)
                      (node 'budget budget)
                      (node 'scope scope))))

(define (effect-entry? x)
  (and (link? x) (eq? (link-type x) 'effect)))

(define (effect-key l) (node-name (first (link-out l))))
(define (effect-key? x) (and (link? x) (eq? (link-type x) 'effect)))

(define (entry-modality l) (node-name (second (link-out l))))

(define (effect-entry-join a b)
  (cond [(and (effect-entry? a) (effect-entry? b)
              (equal? (effect-key a) (effect-key b)))
         (link 'effect (cons (first (link-out a))
                             (slots-join effect-lattices
                                         (rest (link-out a))
                                         (rest (link-out b)))))]
        [a a]
        [else b]))

(define (effect-entry-meet a b)
  (and (effect-entry? a) (effect-entry? b)
       (equal? (effect-key a) (effect-key b))
       (link 'effect (cons (first (link-out a))
                           (slots-meet effect-lattices
                                       (rest (link-out a))
                                       (rest (link-out b)))))))

;; Entry ≤ entry. `b` may be #f (absent in the super row). Matches
;; contract.rkt's effect-subtype? exactly, including its two special cases:
;; a `must-not` entry constrains nothing, and a `must` entry is checked only
;; for obligation-compatibility (budget/ops are not compared).
(define (effect-entry-leq a b)
  (case (entry-modality a)
    [(must-not) #t]
    [(must) (and b (eq? (entry-modality b) 'must))]
    [else (and b (slots-leq effect-lattices (rest (link-out a)) (rest (link-out b))))]))

;; A row is a set of effect hyperedges keyed by name (plain list; order is
;; irrelevant to every operation below).
(define (effect-row . entries) entries)

(define (effect-row-lookup row name)
  (for/or ([e (in-list row)])
    (and (equal? (effect-key e) name) e)))

(define (effect-row-join r1 r2)
  (define names (remove-duplicates (append (map effect-key r1) (map effect-key r2))))
  (for/list ([n (in-list names)])
    (define a (effect-row-lookup r1 n))
    (define b (effect-row-lookup r2 n))
    (cond [(and a b) (effect-entry-join a b)]
          [a a]
          [else b])))

(define (effect-row-meet r1 r2)
  (define names (remove-duplicates (map effect-key r1)))
  (for/list ([n (in-list names)]
             #:when (and (effect-row-lookup r1 n) (effect-row-lookup r2 n)))
    (effect-entry-meet (effect-row-lookup r1 n) (effect-row-lookup r2 n))))

(define (effect-row-diff r1 r2)
  (define names2 (map effect-key r2))
  (filter (λ (e) (not (memq (effect-key e) names2))) r1))

(define (effect-row-leq L R)
  (andmap (λ (e) (effect-entry-leq e (effect-row-lookup R (effect-key e)))) L))

;; ── pattern matching / query ────────────────────────────────────────

;; Match a pattern (may contain `var` nodes) against an atom. Returns a
;; binding hash on success, #f on failure.
(define (hg-match? pat atom [env (hash)])
  (cond
    [(var? pat)
     (if (hash-has-key? env (var-name pat))
         (hg-match? (hash-ref env (var-name pat)) atom env)
         (hash-set env (var-name pat) atom))]
    [(node? pat)
     (and (node? atom)
          (eq? (node-type pat) (node-type atom))
          (let ([pn (node-name pat)]
                [an (node-name atom)])
            (if (var? pn)
                (if (hash-has-key? env (var-name pn))
                    (and (equal? (hash-ref env (var-name pn)) an) env)
                    (hash-set env (var-name pn) an))
                (and (equal? pn an) env))))]
    [(link? pat)
     (and (link? atom)
          (eq? (link-type pat) (link-type atom))
          (= (length (link-out pat)) (length (link-out atom)))
          (for/fold ([e env])
                    ([p (in-list (link-out pat))]
                     [a (in-list (link-out atom))])
            (define r (hg-match? p a e))
            #:break (not r)
            r))]
    [(and (list? pat) (list? atom))
     (and (= (length pat) (length atom))
          (for/fold ([e env])
                    ([p (in-list pat)]
                     [a (in-list atom)])
            (define r (hg-match? p a e))
            #:break (not r)
            r))]
    [else (and (equal? pat atom) env)]))

;; All atoms in `atoms` matching `pat`, as binding hashes.
(define (hg-query pat atoms)
  (filter values (map (λ (a) (hg-match? pat a)) atoms)))

;; ── type instance ───────────────────────────────────────────────────

;; Base type node.
(define (t-base name) (node 'type name))

;; Function type: (fn arg ret effects), effects = list of effect hyperedges.
(define (t-fn arg ret effects) (link 'fn (list arg ret effects)))

;; List type: (listof elem).
(define (t-listof elem) (link 'listof (list elem)))

;; Capability/parameterized-type reference: (ref Base args...).
(define (t-ref base . args) (link 'ref (cons (t-base base) args)))

;; Subtyping over typed hyperedges, with variance. Covariant: a ≤ b means
;; "a usable where b is expected".
(define (type-leq a b)
  (cond
    [(and (node? a) (node? b) (eq? (node-type a) 'type) (eq? (node-type b) 'type))
     (equal? (node-name a) (node-name b))]
    [(and (link? a) (link? b) (eq? (link-type a) (link-type b)))
     (case (link-type a)
       [(listof) (type-leq (first (link-out a)) (first (link-out b)))]
       [(ref) (and (type-leq (first (link-out a)) (first (link-out b)))
                   (andmap type-leq (rest (link-out a)) (rest (link-out b))))]
       [(fn) (define ao (link-out a)) (define bo (link-out b))
        (and (type-leq (first bo) (first ao))      ; contravariant arg
             (type-leq (second ao) (second bo))    ; covariant ret
             (effect-row-leq (third ao) (third bo)))] ; covariant effect row
       [else #f])]
    [else #f]))

;; ── capability instance (Effervesce-style needs/provides) ───────────

;; A capability reference: (ref Name args...). Same shape as t-ref; kept as
;; a named alias so capability code reads as capabilities.
(define (cap-ref name . args) (apply t-ref name args))

;; Every need is satisfiable by some provide: same capability name and the
;; need's type arguments are subtypes of the provide's (covariant).
(define (cap-satisfied? needs provides)
  (andmap
   (λ (n)
     (and (link? n) (eq? (link-type n) 'ref)
          (for/or ([p (in-list provides)])
            (and (link? p) (eq? (link-type p) 'ref)
                 (eq? (node-name (first (link-out p)))
                      (node-name (first (link-out n))))
                 (type-leq n p)))))
   needs))

;; ── generic AST / S-expression materialization ──────────────────────
;;
;; Turn any value — a transparent Racket struct tree (a UIR AST), a nested
;; list (an S-expression, e.g. `uir->sexp` output), or a scalar — into a
;; queryable hypergraph. Each struct becomes a vertex typed by its tag with a
;; fresh identity; each field becomes a `slot` link (vertex, slot index,
;; child). Lists become `list` vertices; scalars become typed leaf nodes
;; (`sym`, `str`, `num`, `bool`, `nil`). The result is a flat atom set that
;; hg-query / hg-match? / the Effervesce index can search directly.

;; Struct type name as a symbol, with the `struct:` prefix stripped.
(define (struct-tag v)
  (define t (vector-ref (struct->vector v) 0))
  (cond
    [(symbol? t)
     (define s (symbol->string t))
     (string->symbol
      (if (and (> (string-length s) 7)
               (string=? (substring s 0 7) "struct:"))
          (substring s 7)
          s))]
    [else 'struct]))

;; A scalar value as a typed leaf node.
(define (leaf-node v)
  (cond
    [(symbol? v) (node 'sym v)]
    [(string? v) (node 'str v)]
    [(number? v) (node 'num v)]
    [(boolean? v) (node 'bool v)]
    [(null? v) (node 'nil 'nil)]
    [else (node 'atom v)]))

(define (materialize v)
  (define counter (box 0))
  (define atoms '())
  (define (next!)
    (begin0 (unbox counter)
      (set-box! counter (add1 (unbox counter)))))
  (define (ref x)
    (cond
      [(struct? x)
       (define vertex (node (struct-tag x) (next!)))
       (set! atoms (cons vertex atoms))
       (for ([i (in-naturals)]
             [f (in-list (cdr (vector->list (struct->vector x))))])
         (set! atoms (cons (link 'slot (list vertex (node 'index i) (ref f))) atoms)))
       vertex]
      [(pair? x)
       (define vertex (node 'list (next!)))
       (set! atoms (cons vertex atoms))
       (for ([i (in-naturals)] [e (in-list x)])
         (set! atoms (cons (link 'slot (list vertex (node 'index i) (ref e))) atoms)))
       vertex]
      [else
       (define leaf (leaf-node x))
       (set! atoms (cons leaf atoms))
       leaf]))
  (ref v)
  (reverse atoms))

(module+ main
  (displayln "racklr/hypergraph — typed-hypergraph kernel loaded."))
