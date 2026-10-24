#lang racket

;; racklr/effects/whole-program.rkt
;;
;; Whole-program (cross-file) effect inference: a single monotone fixpoint over
;; the union of every component's call graph, with component-qualified node
;; identity and cross-file superclass resolution.

(require racklr/uir
         racklr/effects/imports
         racklr/effects/shape
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/rows
         racklr/effects/classify
         racklr/effects/receiver-type)

(provide infer-effects-all)

;; Component defining a class named `class-name`, from the point of view of
;; `comp` (the file making the reference). A local definition wins; a
;; globally-unique definition resolves; anything else is ambiguous → #f.
(define (resolve-class-component class-name comp class-defs)
  (define defs (remove-duplicates (hash-ref class-defs class-name '())))
  (cond
    [(null? defs) #f]
    [(member comp defs) comp]
    [(= (length defs) 1) (car defs)]
    [else #f]))

;; Ordered list of `(component . Class.method)` keys a `self.m()`/`cls.m()`/
;; `KnownClass.m()` call could target, most-derived first, following the
;; superclass chain across files. Stops at a superclass whose name is ambiguous
;; or unknown (such calls stay `unknown` rather than resolving to the wrong
;; file's class).
(define (mro-method-keys comp class-name mname methods class-defs comp-supers)
  (define seen (make-hash))
  (define out '())
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (define key (cons c (qualified-name-symbol cls mname)))
      (when (hash-has-key? methods key) (set! out (cons key out)))
      (for ([s (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (resolve-class-component s c class-defs))
        (when c2 (loop c2 s)))))
  (reverse out))

;; `(component . name)` keys a method callee could resolve to (the whole-program
;; counterpart of `method-callee-qualified-names`).
(define (method-callee-keys callee comp owner methods class-defs comp-supers)
  (cond
    [(not (uir-get? callee)) '()]
    [else
     (let* ([mname (get-field-symbol (uir-get-field callee))]
            [base (uir-get-base callee)])
       (cond
         [(and mname (super-call? base))
          ;; super().m() → each parent of the enclosing class, resolved to the
          ;; file that defines it (most-derived first).
          (if owner
              (append-map
               (λ (s)
                 (let ([c2 (resolve-class-component s comp class-defs)])
                   (if c2 (mro-method-keys c2 s mname methods class-defs comp-supers) '())))
               (hash-ref comp-supers (cons comp owner) '()))
              '())]
         [(and mname (uir-var? base))
          (let ([base-name (string->symbol (uir-symbol-name (uir-var-name base)))])
            (cond
              [(memq base-name '(self cls))
               (or (and owner (mro-method-keys comp owner mname methods class-defs comp-supers)) '())]
              [else
               (define c (resolve-class-component base-name comp class-defs))
               (or (and c (mro-method-keys c base-name mname methods class-defs comp-supers)) '())]))]
         [else '()]))]))

;; `(component . name)` key for a bare function call `f(...)`: the local
;; definition wins, else a globally-unique definition, else #f (ambiguous).
(define (bare-function-key nm comp fn-defs known)
  (cond
    [(not nm) #f]
    [else
     (define local (cons comp nm))
     (define defs (remove-duplicates (hash-ref fn-defs nm '())))
     (cond
       [(hash-has-key? known local) local]
       [(and (= (length defs) 1) (pair? defs))
        (define k (cons (car defs) nm))
        (and (hash-has-key? known k) k)]
       [else #f])]))

;; Resolve an imported-name receiver through the import graph to an effect
;; capability (or 'pure). A `name.m(...)` call whose `name` was bound by an
;; import to a module or class resolves structurally instead of by name
;; heuristics:
;;   module handle → the module's effect domain (coarse), or 'pure if tagged so
;;   class  handle → Class.method's derived cap, else the class's module domain
;; Returns an effect-cap, 'pure, or #f (not an imported-name call).
(define (imported-effect callee comp import-env)
  (and import-env
       (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [exports (import-env-exports import-env)]
              [caps (import-env-caps import-env)]
              [mname (and (uir-get? callee) (get-field-symbol (uir-get-field callee)))]
              [base (and (uir-get? callee) (uir-get-base callee))]
              [base-name (and base (uir-var? base)
                              (string->symbol (uir-symbol-name (uir-var-name base))))])
         (and modpath mname base-name
              (not (memq base-name '(self cls)))
              (let* ([e (hash-ref exports modpath #f)]
                     [h (and e (hash-ref e (symbol->string base-name) #f))])
                (and (handle? h)
                     (case (handle-kind h)
                       [(class)
                        (define cap (hash-ref caps (format "~a.~a" (handle-name h) mname) #f))
                        (cond
                          [cap cap]
                          [else
                           (define dom (module-effect-tag (handle-path h)))
                           (cond [(eq? dom 'pure) 'pure]
                                 [dom (effect-cap dom '())]
                                 [else #f])])]
                       [(module)
                        (define dom (module-effect-tag (handle-path h)))
                        (cond [(eq? dom 'pure) 'pure]
                              [dom (effect-cap dom '())]
                              [else #f])]
                       [else #f])))))))

;; Effect of a `Class.method` call whose receiver's class was tracked by
;; receiver-type dataflow (`x = KnownClass(...)` / `self.attr = KnownClass(...)`):
;; resolve the class to its defining component and derive the typed capability
;; from that component's module domain. 'pure when the class's module is tagged
;; pure (a pure receiver's method cannot be effectful); #f when the class is
;; unresolvable or its module untagged (falls through to the name heuristics).
(define (receiver-effect comp class-name mname class-defs import-env)
  (and import-env
       (let ([c (resolve-class-component class-name comp class-defs)])
         (and c
              (let ([modpath (hash-ref (import-env-comp-modpath import-env) c #f)])
                (and modpath
                     (let ([domain (module-effect-tag modpath)])
                       (cond
                         [(eq? domain 'pure) 'pure]
                         [domain (effect-cap domain (method-ops domain modpath
                                                                (symbol->string class-name)
                                                                (symbol->string mname)))]
                         [else #f]))))))))

;; A dotted call rooted at a known class name (`X.objects.Y`): the manager /
;; related attribute is installed by a framework metaclass, not by any
;; assignment in a method body, so the receiver-type env cannot see it. Resolve
;; the class name to its defining module's domain — the same class→module→domain
;; resolution used for tracked receivers — so the capability is derived from the
;; call chain rather than from a `objects` name table.
(define (manager-effect callee comp class-defs import-env)
  (and import-env
       (member-access-callee? callee)
       (let* ([base (member-access-base callee)]
              [root (dotted-root callee)]
              [mname (effect-op-of-callee callee)])
         (and base root mname
              (member-access-callee? base)          ; X.attr.m, not X.m
              (not (memq root '(self cls)))
              (hash-has-key? class-defs root)
              (receiver-effect comp root mname class-defs import-env)))))

(define (classify-call-all callee known methods class-defs fn-defs comp-supers comp owner [import-env #f] [rt #f])
  (define nm (callee-name callee))
  (define keys (method-callee-keys callee comp owner methods class-defs comp-supers))
  (define key (for/or ([k (in-list keys)] #:when (hash-has-key? known k)) k))
  (cond
    [(and key (hash-has-key? known key)) (cons 'edge key)]
    [(bare-function-key nm comp fn-defs known) => (λ (k) (cons 'edge k))]
    [else
     ;; A call to a known class name is a constructor (pure instantiation), not
     ;; an effectful call — this also seeds the receiver-type dataflow without
     ;; polluting the enclosing function with `unknown`.
     (define ctor (constructor-class callee class-defs))
     (cond
       [ctor (cons 'pure #f)]
       [else
        ;; A locally-tracked receiver (x = KnownClass(...)) shadows any import
        ;; of the same name, so it is resolved before the import graph.
        (define rcls (and rt (uir-get? callee)
                           (receiver-class-of rt (uir-get-base callee))))
        (define mname (and (uir-get? callee) (get-field-symbol (uir-get-field callee))))
        (define re (and rcls mname (receiver-effect comp rcls mname class-defs import-env)))
        (cond
          [(eq? re 'pure) (cons 'pure #f)]
          [(effect-cap? re) (cons 'effect re)]
           [else
            (define me (manager-effect callee comp class-defs import-env))
            (cond
              [(eq? me 'pure) (cons 'pure #f)]
              [(effect-cap? me) (cons 'effect me)]
              [else
               (define ie (imported-effect callee comp import-env))
               (cond
                 [(eq? ie 'pure) (cons 'pure #f)]
                 [(effect-cap? ie) (cons 'effect ie)]
                 [else (classify-nonedge callee)])])])])]))

;; The derived capability for a call-graph method key `(component . Class.method)`,
;; resolved COMPONENT-AWARE: the component's own module path supplies the domain,
;; so same-named classes in different modules do not collide (unlike the bare-key
;; caps table). Returns an effect-cap or #f. This is the "blessed summary" from
;; the per-library config — a typed op like `db<Model>.write` is asserted at the
;; method's own definition site, so it flows through the fixpoint to every
;; caller even when the method body descends into unclassified plumbing.
(define (method-cap k import-env)
  (and import-env
       (let* ([comp (car k)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)])
         (and modpath
              (let* ([s (symbol->string (cdr k))]
                     [cls-str (regexp-replace #rx"[.].*$" s "")]
                     [m-str (regexp-replace #rx"^[^.]*[.]" s "")])
                (and (not (string=? cls-str s))
                     (let ([domain (module-effect-tag modpath)])
                       (and domain
                            (not (eq? domain 'pure))
                            (effect-cap domain (method-ops domain modpath cls-str m-str))))))))))

;; Self-attribute assignments (`self.attr = KnownClass(...)`) collected across
;; every method of a class, keyed by (cons comp class). This is the per-CLASS
;; counterpart of the per-function receiver env: attributes set in __init__ (or
;; any method) flow to every other method, so `self.attr.m(...)` resolves even
;; when the assignment is not in the method body itself.
(define (class-self-attrs comp-programs class-defs)
  (define h (make-hash))
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (for ([(n body) (in-hash (collect-methods (cdr cp)))])
      (define owner (class-name-of-name n))
      (define sa (receiver-types-self-attrs (infer-receiver-types body class-defs)))
      (define key (cons comp owner))
      (define cur (hash-ref h key #f))
      (cond
        [(not cur) (unless (zero? (hash-count sa)) (hash-set! h key (hash-copy sa)))]
        [else (for ([(attr kls) (in-hash sa)]) (hash-set! cur attr kls))])))
  h)

;; Inherited self-attribute environment for a class, following the MRO across
;; files with the most-derived class first: each attribute keeps the first
;; (most-derived) assignment, so a subclass's `self.attr = ...` shadows a
;; base's. Returns a receiver-types with no locals (locals are per-function).
(define (class-attr-env comp class-name class-attrs class-defs comp-supers)
  (define seen (make-hash))
  (define acc (make-hash))
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (for ([(attr kls) (in-hash (hash-ref class-attrs (cons c cls) (hash)))])
        (unless (hash-has-key? acc attr) (hash-set! acc attr kls)))
      (for ([s (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (resolve-class-component s c class-defs))
        (when c2 (loop c2 s)))))
  (receiver-types (hash) acc))

(define (key<? a b)
  (string<? (format "~a.~a" (car a) (cdr a)) (format "~a.~a" (car b) (cdr b))))

;; Whole-program effect rows: (component . name) → uir-effect-row, computed by
;; a single fixpoint over the union of every program's call graph. Takes a list
;; of `(component . program)` pairs.
(define (infer-effects-all comp-programs #:import-env [import-env #f])
  (define fns (make-hash))           ;; (cons comp name) → body
  (define methods (make-hash))       ;; (cons comp name) → body
  (define comp-supers (make-hash))   ;; (cons comp class) → (listof super-symbol)
  (define class-defs (make-hash))    ;; class-name → (listof comp)
  (define fn-defs (make-hash))       ;; fn-name → (listof comp)
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define program (cdr cp))
    (for ([(n body) (in-hash (collect-functions program))])
      (hash-set! fns (cons comp n) body)
      (hash-update! fn-defs n (λ (l) (cons comp l)) '()))
    (for ([(n body) (in-hash (collect-methods program))])
      (hash-set! methods (cons comp n) body))
    (for ([(n _) (in-hash (collect-class-names program))])
      (hash-update! class-defs n (λ (l) (cons comp l)) '()))
    (for ([(n chain) (in-hash (collect-supers program))])
      (hash-set! comp-supers (cons comp n) chain)))

  (define class-attrs (class-self-attrs comp-programs class-defs))

  (define known (make-hash))
  (for ([(k _) (in-hash fns)]) (hash-set! known k #t))
  (for ([(k _) (in-hash methods)]) (hash-set! known k #t))

  (define all-keys (append (hash-keys fns) (hash-keys methods)))
  (define direct-rows (make-hash))
  (define edges (make-hash))
  (for ([k (in-list all-keys)])
    (define body (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
    (define comp (car k))
    (define owner (and (hash-has-key? methods k) (class-name-of-name (cdr k))))
    (define own-rt (infer-receiver-types body class-defs))
    (define rt
      (if owner
          (receiver-types-merge (class-attr-env comp owner class-attrs class-defs comp-supers)
                                own-rt)
          own-rt))
    (define effects
      (let ([cap (and (hash-has-key? methods k) (method-cap k import-env))])
        (if cap (list cap) '())))
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
          (let ([cls (classify-call-all (uir-call-callee node) known methods
                                        class-defs fn-defs comp-supers comp owner
                                        import-env rt)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows k (effects->row effects))
    (hash-set! edges k (sort (remove-duplicates callees) key<?)))

  (define rows (make-hash))
  (for ([k (in-list all-keys)]) (hash-set! rows k (hash-ref direct-rows k)))
  (let loop ()
    (define changed? #f)
    (for ([k (in-list all-keys)])
      (define current (hash-ref rows k))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges k)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows k) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin (set! changed? #t) (hash-set! rows k new-row))))
    (if changed? (loop) (void)))
  rows)
