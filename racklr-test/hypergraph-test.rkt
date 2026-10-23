#lang racket

;; Proves the typed-hypergraph kernel (racklr/hypergraph) is a faithful
;; generalization of the hand-written effect algebra in
;; racklr/effects/contract, then shows it extends to typed function
;; subtyping, graph pattern queries, and Effervesce-style capability
;; composition.

(require rackunit
         racklr/hypergraph
         racklr/uir
         racklr/effects/contract)

;; ── UIR fixture helpers (mirror effects-contract-test) ──────────────

(define (ent name modality #:ops [ops '()] #:budget [budget #f] #:scope [scope #f])
  (uir-effect-entry (uir-symbol name) modality
                    (map uir-symbol ops)
                    budget
                    (if scope (uir-symbol scope) #f)))

(define (row . entries) (uir-effect-row entries))

;; ── UIR ↔ hypergraph converters ─────────────────────────────────────

(define (entry->link e)
  (effect-entry
   (effect-name-symbol e)
   (uir-effect-entry-modality e)
   (entry-ops e)
   (uir-effect-entry-budget e)
   (and (uir-effect-entry-scope e)
        (string->symbol (uir-symbol-name (uir-effect-entry-scope e))))))

(define (link->entry l)
  (define out (link-out l))
  (uir-effect-entry
   (uir-symbol (symbol->string (node-name (first out))))
   (node-name (second out))
   (map (λ (s) (uir-symbol (symbol->string s))) (node-name (third out)))
   (node-name (fourth out))
   (and (node-name (fifth out))
        (uir-symbol (symbol->string (node-name (fifth out)))))))

(define (row->links r) (map entry->link (uir-effect-row-entries r)))
(define (links->row links) (uir-effect-row (map link->entry links)))

(define (canonical r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

;; ── equivalence with contract.rkt ───────────────────────────────────

;; All four row operations must agree with contract.rkt on every pair.
(define (check-equiv r1 r2)
  (check-equal? (canonical (links->row (effect-row-join (row->links r1) (row->links r2))))
                (canonical (effect-row-union r1 r2))
                "join")
  (check-equal? (canonical (links->row (effect-row-meet (row->links r1) (row->links r2))))
                (canonical (effect-row-intersect r1 r2))
                "meet")
  (check-equal? (canonical (links->row (effect-row-diff (row->links r1) (row->links r2))))
                (canonical (effect-row-difference r1 r2))
                "diff")
  (check-equal? (effect-row-leq (row->links r1) (row->links r2))
                (effect-subtype? r1 r2)
                "subtype"))

(define battery
  (list
   ;; disjoint names
   (list (row (ent "llm" 'may)) (row (ent "db" 'may)))
   ;; may+may: budget sums
   (list (row (ent "llm" 'may #:budget 3)) (row (ent "llm" 'may #:budget 4)))
   ;; must-not absorbs on union
   (list (row (ent "db" 'may)) (row (ent "db" 'must-not)))
   ;; must dominates may on union
   (list (row (ent "db" 'must)) (row (ent "db" 'may)))
   ;; ops union
   (list (row (ent "llm" 'may #:ops '("query")))
         (row (ent "llm" 'may #:ops '("embed"))))
   ;; ops "all" (empty) is absorbing
   (list (row (ent "llm" 'may #:ops '())) (row (ent "llm" 'may #:ops '("query"))))
   ;; unbounded budget (absence) dominates
   (list (row (ent "llm" 'may #:budget 5)) (row (ent "llm" 'may)))
   ;; scope: same scope merges
   (list (row (ent "db" 'may #:scope "reads=x")) (row (ent "db" 'may #:scope "reads=x")))
   ;; scope: conflict collapses to unconstrained
   (list (row (ent "db" 'may #:scope "reads=x")) (row (ent "db" 'may #:scope "reads=y")))
   ;; empty rows
   (list (row) (row))
   ;; empty + something
   (list (row) (row (ent "llm" 'must)))
   ;; overlap + disjoint mix
   (list (row (ent "llm" 'may #:budget 2) (ent "db" 'must-not))
         (row (ent "llm" 'may #:budget 7) (ent "net" 'may)))))

(define (test-effect-equivalence)
  (for ([p (in-list battery)])
    (check-equiv (car p) (cadr p))))

;; ── typed effect algebra (the extension contract.rkt lacks) ─────────

(define (test-typed-effect-subtyping)
  ;; Function type with a wider effect row is a supertype.
  (define e-narrow (list (effect-entry 'llm 'may '() 5 #f)))
  (define e-wide   (list (effect-entry 'llm 'may '() 10 #f)
                         (effect-entry 'db  'may '() #f #f)))
  (check-true (type-leq (t-fn (t-base 'int) (t-base 'string) e-narrow)
                        (t-fn (t-base 'int) (t-base 'string) e-wide)))

  ;; A network-performing function cannot satisfy a pure function type.
  (define e-net (list (effect-entry 'network 'may '() #f #f)))
  (check-false (type-leq (t-fn (t-base 'int) (t-base 'string) e-net)
                         (t-fn (t-base 'int) (t-base 'string) '())))

  ;; Argument is contravariant: fn(listof int) and fn(int) are incomparable.
  (check-false (type-leq (t-fn (t-listof (t-base 'int)) (t-base 'string) '())
                         (t-fn (t-base 'int) (t-base 'string) '())))
  (check-false (type-leq (t-fn (t-base 'int) (t-base 'string) '())
                         (t-fn (t-listof (t-base 'int)) (t-base 'string) '())))

  ;; Result is covariant: int ≤ int (identity) is the positive case here.
  (check-true (type-leq (t-fn (t-base 'int) (t-base 'int) '())
                        (t-fn (t-base 'int) (t-base 'int) '())))

  ;; listof is covariant: listof int ≤ listof int.
  (check-true (type-leq (t-listof (t-base 'int)) (t-listof (t-base 'int)))))

;; ── graph pattern query (AtomSpace PatternMatcher idea) ─────────────

(define (test-pattern-query)
  (define graph
    (list (effect-entry 'llm 'may '(query) 5 #f)
          (effect-entry 'db 'may '() #f #f)
          (effect-entry 'network 'may '() #f #f)))
  (define pat (effect-entry (var 'e) 'may (var 'ops) (var 'budget) (var 'scope)))
  (define matches (hg-query pat graph))
  (check-equal? (length matches) 3)
  (check-equal? (sort (map (λ (h) (hash-ref h 'e)) matches) symbol<?)
                '(db llm network))
  ;; Bound ops carry through: llm's ops are (query).
  (define llm-match
    (for/or ([h (in-list matches)])
      (and (eq? (hash-ref h 'e) 'llm) h)))
  (check-equal? (hash-ref llm-match 'ops) '(query)))

;; ── capability composition (Effervesce needs/provides) ──────────────

(define (test-capability-composition)
  ;; Read<Config> is a subtype of Read<Top-ish>: covariant in its argument.
  (check-true (type-leq (cap-ref 'Read (t-base 'int))
                        (cap-ref 'Read (t-base 'int))))
  ;; Different capability names never satisfy each other.
  (check-false (cap-satisfied? (list (cap-ref 'Read (t-base 'int)))
                               (list (cap-ref 'Fs))))
  ;; A component needing {Read, Log} is satisfied by one providing both.
  (check-true (cap-satisfied?
               (list (cap-ref 'Read (t-base 'int)) (cap-ref 'Log))
               (list (cap-ref 'Read (t-base 'int)) (cap-ref 'Log) (cap-ref 'Fs))))
  ;; Missing Log leaves the need unsatisfied.
  (check-false (cap-satisfied?
                (list (cap-ref 'Read (t-base 'int)) (cap-ref 'Log))
                (list (cap-ref 'Read (t-base 'int))))))

;; ── generic AST / S-expression materialization ──────────────────────

(define (test-materialize)
  (define atoms (materialize (uir-call (uir-symbol "fetch") '())))
  (check-true (for/or ([a (in-list atoms)])
                (and (node? a) (eq? (node-type a) 'uir-call))))
  (check-true (for/or ([a (in-list atoms)])
                (and (link? a) (eq? (link-type a) 'slot))))
  (check-true (for/or ([a (in-list atoms)])
                (and (node? a) (eq? (node-type a) 'str)
                     (equal? (node-name a) "fetch"))))
  (check-true (for/or ([a (in-list (materialize (uir->sexp (uir-call (uir-symbol "fetch") '()))))])
                (and (node? a) (eq? (node-type a) 'list))))
  (check-equal? (materialize "x") (list (node 'str "x"))))

(module+ main
  (test-effect-equivalence)
  (test-typed-effect-subtyping)
  (test-pattern-query)
  (test-capability-composition)
  (test-materialize)
  (displayln "All hypergraph tests passed."))
