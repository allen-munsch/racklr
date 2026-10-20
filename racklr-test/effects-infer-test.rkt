#lang racket

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/infer)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
;; JS/TS/JSX lower `a.b` to (call (symbol "dot") (list a b)), not uir-get.
(define (jsdot b f) (uir-call (sym "dot") (list b (sym f))))

(define (fn name body #:params [params '()])
  (uir-fn (if name (uir-symbol name) #f) params body #f #f))

(define (prog . stmts)
  (uir-block stmts))

(define (modality row name)
  (define e (row-lookup row name))
  (and e (uir-effect-entry-modality e)))

(define (row-names row)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries row))
        symbol<?))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-empty-program)
  (check-true (hash-empty? (infer-effects (uir-null)))))

(define (test-lambda-not-collected)
  ;; An unnamed uir-fn is not a call-graph node.
  (define lam (fn #f (uir-call (dot (v "llm") "query") '())))
  (check-true (hash-empty? (infer-effects (prog lam)))))

(define (test-pure-function)
  (define p (prog (fn "f" (uir-null))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-direct-llm)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define r (hash-ref (infer-effects p) 'summarize))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (row-names r) '(llm)))

(define (test-direct-db-dotted)
  ;; db.user.update(...) → db (root of the dotted chain).
  (define p (prog (fn "update"
                      (uir-call (dot (dot (v "db") "user") "update")
                                (list (uir-record '()) (uir-record '()))))))
  (define r (hash-ref (infer-effects p) 'update))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (row-names r) '(db)))

(define (test-django-model-objects)
  ;; Model.objects.filter(...) → db via the `objects` field.
  (define p (prog (fn "q"
                      (uir-call (dot (dot (v "User") "objects") "filter")
                                (list (uir-record '()))))))
  (define r (hash-ref (infer-effects p) 'q))
  (check-equal? (modality r 'db) 'may))

(define (test-direct-network)
  (define p (prog (fn "f" (uir-call (v "fetch") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-direct-cache)
  (define p (prog (fn "r" (uir-call (v "revalidate_path") (list (uir-string "/p"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'r) 'cache) 'may))

(define (test-transitive)
  ;; h calls g, g calls db → h inherits db.
  (define p (prog
             (fn "g" (uir-call (dot (v "db") "fetch")
                               (list (uir-string "idx") (uir-string "q"))))
             (fn "h" (uir-call (v "g") (list (uir-string "seed"))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'g) 'db) 'may)
  (check-equal? (modality (hash-ref rows 'h) 'db) 'may)
  (check-equal? (row-names (hash-ref rows 'h)) '(db)))

(define (test-higher-order-unknown)
  ;; A call through a parameter (cb) is deferred: entry named unknown.
  (define p (prog (fn "k" (uir-call (v "cb") (list (uir-number "1")))
                      #:params (list (uir-typed-param (sym "cb") #f #f)))))
  (define r (hash-ref (infer-effects p) 'k))
  (check-equal? (modality r 'unknown) 'unknown)
  (check-equal? (row-names r) '(unknown)))

(define (test-nested-fn-does-not-leak)
  ;; A nested function's effects must not leak into the enclosing function
  ;; when the nested function is never called.
  (define p (prog (fn "outer"
                      (uir-block
                       (list (fn "inner" (uir-call (dot (v "llm") "query") '()))
                             (uir-null))))))
  (define rows (infer-effects p))
  ;; outer itself performs nothing directly and calls nothing.
  (check-equal? (hash-ref rows 'outer) (empty-row))
  ;; inner is still a separate node with llm.
  (check-equal? (modality (hash-ref rows 'inner) 'llm) 'may))

(define (test-recursion-converges)
  ;; r calls r on one branch and llm on the other; fixpoint must terminate.
  (define p (prog (fn "r"
                      (uir-if (uir-bool #t)
                              (uir-call (v "r") '())
                              (uir-call (dot (v "llm") "query") '())))))
  (define r (hash-ref (infer-effects p) 'r))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (row-names r) '(llm)))

(define (test-httpx-network)
  (define p (prog (fn "f" (uir-call (dot (v "httpx") "get") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-requests-network)
  (define p (prog (fn "f" (uir-call (dot (v "requests") "get") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-redis-cache)
  (define p (prog (fn "f" (uir-call (dot (v "redis") "get") (list (uir-string "k"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'cache) 'may))

(define (test-anthropic-llm)
  (define p (prog (fn "f" (uir-call (dot (dot (v "anthropic") "messages") "create") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-agno-llm)
  (define p (prog (fn "f" (uir-call (dot (dot (v "agno") "agent") "Agent") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-langgraph-llm)
  (define p (prog (fn "f" (uir-call (dot (dot (v "langgraph") "graph") "StateGraph") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-django-objects-string-field)
  ;; The Python lowerer emits attribute names as uir-string (not uir-symbol);
  ;; the `objects` heuristic must accept that form too.
  (define p (prog (fn "q"
                      (uir-call (uir-get (uir-get (v "Order") (uir-string "objects"))
                                         (uir-string "all"))
                                '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'q) 'db) 'may))

;; ── JS/TS/JSX `dot`-encoded member access ───────────────────────────

(define (test-js-dot-axios-network)
  (define p (prog (fn "f" (uir-call (jsdot (v "axios") "get")
                                    (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-js-dot-db-db)
  (define p (prog (fn "f" (uir-call (jsdot (v "db") "query")
                                    (list (uir-string "SELECT 1"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'db) 'may))

(define (test-js-dot-model-objects-db)
  (define p (prog (fn "q"
                      (uir-call (jsdot (jsdot (v "User") "objects") "filter")
                                (list (uir-record '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'q) 'db) 'may))

(define (test-js-dot-redis-cache)
  (define p (prog (fn "f" (uir-call (jsdot (v "redis") "get")
                                    (list (uir-string "k"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'cache) 'may))

(define (test-js-dot-anthropic-llm)
  (define p (prog (fn "f" (uir-call (jsdot (jsdot (v "anthropic") "messages") "create")
                                    '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-js-dot-bare-fetch-network)
  ;; A bare callee (non-dotted) still classifies: fetch(...) → network.
  (define p (prog (fn "f" (uir-call (v "fetch") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

;; ── effect-op-of-callee ─────────────────────────────────────────────
;; The operation performed by a direct-effect callee, used by the emitters to
;; instrument runtime guards with the specific operation.

(define (test-op-dotted-llm-query)
  (check-equal? (effect-op-of-callee (jsdot (v "llm") "query")) 'query))

(define (test-op-dotted-nested-last-field)
  (check-equal? (effect-op-of-callee (jsdot (jsdot (v "db") "user") "update")) 'update))

(define (test-op-dotted-objects-filter)
  (check-equal? (effect-op-of-callee (jsdot (jsdot (v "User") "objects") "filter")) 'filter))

(define (test-op-bare-fetch)
  (check-equal? (effect-op-of-callee (v "fetch")) 'fetch))

(define (test-op-python-get-string-field)
  (check-equal? (effect-op-of-callee
                 (uir-get (uir-get (v "Order") (uir-string "objects")) (uir-string "all")))
                'all))

;; ── synthetic operator/statement calls are pure, not `unknown` ──────

(define (test-synthetic-operator-pure)
  (define p (prog (fn "f" (uir-call (sym "+")
                                    (list (uir-number "1") (uir-number "2"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-synthetic-dot-pure)
  ;; A bare member access (not itself an effectful call) is pure.
  (define p (prog (fn "f" (jsdot (v "x") "y"))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-annotate-writes-back)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define annotated (annotate-effects p))
  (define f (car (uir-block-stmts annotated)))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

(define (test-annotate-preserves-explicit-row)
  ;; A hand-written row (budgets/scopes) wins over inference; inference must
  ;; not clobber it with the row inferred from the body.
  (define explicit
    (uir-effect-row
     (list (uir-effect-entry (sym "cache") 'must-not '() 3 #f))))
  (define p (prog (struct-copy uir-fn (fn "f" (uir-call (dot (v "llm") "query") '()))
                                 [effects explicit])))
  (define f (car (uir-block-stmts (annotate-effects p))))
  (check-true (uir-fn? f))
  (check-equal? (row-names (uir-fn-effects f)) '(cache))
  (check-equal? (modality (uir-fn-effects f) 'cache) 'must-not))

(define (test-annotate-skips-empty-inferred)
  ;; A pure function with no annotation stays unannotated (effects field #f),
  ;; so emitters do not treat it as effect-using.
  (define p (prog (fn "plain" (uir-null))))
  (define f (car (uir-block-stmts (annotate-effects p))))
  (check-false (uir-fn-effects f)))

(define (test-annotate-skips-unknown-only)
  ;; 'unknown is a checker signal ("could not verify"), not a runtime effect.
  ;; A function whose inferred row is only 'unknown must not gain a runtime
  ;; guard, since the guard rejects the 'unknown modality.
  (define p (prog (fn "f" (uir-call (v "len") '()))))
  (define f (car (uir-block-stmts (annotate-effects p))))
  (check-false (uir-fn-effects f)))

;; ── JS/TS function-declaration encodings ────────────────────────────
;; TS lowers `function NAME(...) {...}` to (set! NAME (fn #f ...)),
;; async to (set! NAME (call "async-fn" (list (fn #f ...)))), generator to
;; (set! NAME (call "gen-fn" (list (fn #f ...)))). collect-functions must key
;; these by the set! name, not by a uir-fn name.

(define (test-set!-fn-decl-collected)
  (define p (prog (uir-set! (sym "summarize")
                            (fn #f (uir-call (dot (v "llm") "query")
                                             (list (uir-string "hi")))))))
  (define r (hash-ref (infer-effects p) 'summarize))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (row-names r) '(llm)))

(define (test-set!-async-fn-decl)
  (define inner (fn #f (uir-call (dot (v "llm") "query")
                                 (list (uir-string "hi")))))
  (define p (prog (uir-set! (sym "summarize")
                            (uir-call (sym "async-fn") (list inner)))))
  (check-equal? (modality (hash-ref (infer-effects p) 'summarize) 'llm) 'may))

(define (test-set!-gen-fn-decl)
  (define inner (fn #f (uir-call (dot (v "llm") "query")
                                 (list (uir-string "hi")))))
  (define p (prog (uir-set! (sym "summarize")
                            (uir-call (sym "gen-fn") (list inner)))))
  (check-equal? (modality (hash-ref (infer-effects p) 'summarize) 'llm) 'may))

(define (test-set!-fn-annotate-writes-back)
  (define p (prog (uir-set! (sym "summarize")
                            (fn #f (uir-call (dot (v "llm") "query")
                                             (list (uir-string "hi")))))))
  (define annotated (annotate-effects p))
  (define decl (car (uir-block-stmts annotated)))
  (check-true (uir-set!? decl))
  (define f (uir-set!-value decl))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

(define (test-set!-async-fn-annotate-writes-back)
  (define inner (fn #f (uir-call (dot (v "llm") "query")
                                 (list (uir-string "hi")))))
  (define p (prog (uir-set! (sym "summarize")
                            (uir-call (sym "async-fn") (list inner)))))
  (define annotated (annotate-effects p))
  (define decl (car (uir-block-stmts annotated)))
  (define wrap (uir-set!-value decl))
  (check-true (uir-call? wrap))
  (define f (first (uir-call-args wrap)))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

;; ── JS/TS arrow-function encodings ──────────────────────────────────
;; TS lowers `(...) => body` to (call "=>" (list params body)). The body is a
;; nested function boundary, and an arrow bound to a name via set! is a named
;; function for inference.

(define (test-arrow-boundary-does-not-leak)
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") '()))))
  (define p (prog (fn "outer" (uir-block (list arrow (uir-null))))))
  (check-equal? (hash-ref (infer-effects p) 'outer) (empty-row)))

(define (test-set!-arrow-inferred)
  (define p (prog (uir-set! (sym "f")
                            (uir-call (sym "=>")
                                      (list (uir-list '())
                                            (uir-call (jsdot (v "llm") "query") '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

;; ── JS/TS anonymous `function` expressions ──────────────────────────
;; TS lowers `function (params) { body }` to (call "function" (list params body)),
;; a sibling of `=>`. The body is a nested function boundary, and a function
;; expression bound to a name via set! is a named function for inference.

(define (test-function-expr-boundary-does-not-leak)
  (define fexpr (uir-call (sym "function")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") '()))))
  (define p (prog (fn "outer" (uir-block (list fexpr (uir-null))))))
  (check-equal? (hash-ref (infer-effects p) 'outer) (empty-row)))

(define (test-set!-function-inferred)
  (define p (prog (uir-set! (sym "f")
                            (uir-call (sym "function")
                                      (list (uir-list '())
                                            (uir-call (jsdot (v "llm") "query") '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

;; ── JS/TS const/let/var-bound arrow & function encodings ─────────────
;; TS lowers `const f = () => ...` to
;; (call "const" (list (set! f (call "=>" (list params body))))) — the set!
;; sits one level deeper than a bare function declaration, so collect-functions
;; must unwrap the var/let/const call to reach the named binding.

(define (var-decl kind binding)
  (uir-call (sym kind) (list binding)))

(define (test-const-arrow-inferred)
  (define p (prog (var-decl "const"
                            (uir-set! (sym "f")
                                      (uir-call (sym "=>")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-let-arrow-inferred)
  (define p (prog (var-decl "let"
                            (uir-set! (sym "f")
                                      (uir-call (sym "=>")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-var-arrow-inferred)
  (define p (prog (var-decl "var"
                            (uir-set! (sym "f")
                                      (uir-call (sym "=>")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-const-arrow-caller-inherits)
  ;; main calls a const-bound arrow f; f's llm effect must propagate to main.
  (define f (var-decl "const"
                      (uir-set! (sym "f")
                                (uir-call (sym "=>")
                                          (list (uir-list '())
                                                (uir-call (jsdot (v "llm") "query") '()))))))
  (define p (prog f
                  (fn "main" (uir-call (v "f") '()))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'f) 'llm) 'may)
  (check-equal? (modality (hash-ref rows 'main) 'llm) 'may))

(define (test-const-function-expr-inferred)
  (define p (prog (var-decl "const"
                            (uir-set! (sym "f")
                                      (uir-call (sym "function")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-const-arrow-annotate-wraps)
  ;; annotate-effects must descend into the const wrapper and wrap the arrow in
  ;; (call "__withEffects" (list row arrow)) so the JS emitter can render a
  ;; guard for a function value with no effects slot.
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") '()))))
  (define p (prog (var-decl "const" (uir-set! (sym "f") arrow))))
  (define decl (car (uir-block-stmts (annotate-effects p))))
  (check-true (uir-call? decl))
  (check-equal? (uir-symbol-name (uir-call-callee decl)) "const")
  (define binding (first (uir-call-args decl)))
  (check-true (uir-set!? binding))
  (define wrapped (uir-set!-value binding))
  (check-true (uir-call? wrapped))
  (check-equal? (uir-symbol-name (uir-call-callee wrapped)) "__withEffects")
  (define row (first (uir-call-args wrapped)))
  (check-true (uir-effect-row? row))
  (check-equal? (modality row 'llm) 'may))

(define (test-const-arrow-annotate-no-wrap-when-pure)
  ;; A pure const-bound arrow gains no __withEffects wrapper.
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (sym "+")
                                          (list (uir-number "1") (uir-number "2"))))))
  (define p (prog (var-decl "const" (uir-set! (sym "f") arrow))))
  (define decl (car (uir-block-stmts (annotate-effects p))))
  (define binding (first (uir-call-args decl)))
  (define wrapped (uir-set!-value binding))
  (check-true (and (uir-call? wrapped)
                   (uir-symbol? (uir-call-callee wrapped))
                   (string=? (uir-symbol-name (uir-call-callee wrapped)) "=>"))
              "pure const arrow is left unwrapped"))

(module+ main
  (test-empty-program)
  (test-lambda-not-collected)
  (test-pure-function)
  (test-direct-llm)
  (test-direct-db-dotted)
  (test-django-model-objects)
  (test-direct-network)
  (test-direct-cache)
  (test-transitive)
  (test-higher-order-unknown)
  (test-nested-fn-does-not-leak)
  (test-recursion-converges)
  (test-httpx-network)
  (test-requests-network)
  (test-redis-cache)
  (test-anthropic-llm)
  (test-agno-llm)
  (test-langgraph-llm)
  (test-django-objects-string-field)
  (test-js-dot-axios-network)
  (test-js-dot-db-db)
  (test-js-dot-model-objects-db)
  (test-js-dot-redis-cache)
  (test-js-dot-anthropic-llm)
  (test-js-dot-bare-fetch-network)
  (test-op-dotted-llm-query)
  (test-op-dotted-nested-last-field)
  (test-op-dotted-objects-filter)
  (test-op-bare-fetch)
  (test-op-python-get-string-field)
  (test-synthetic-operator-pure)
  (test-synthetic-dot-pure)
  (test-annotate-writes-back)
  (test-annotate-preserves-explicit-row)
  (test-annotate-skips-empty-inferred)
  (test-annotate-skips-unknown-only)
  (test-set!-fn-decl-collected)
  (test-set!-async-fn-decl)
  (test-set!-gen-fn-decl)
  (test-set!-fn-annotate-writes-back)
  (test-set!-async-fn-annotate-writes-back)
  (test-arrow-boundary-does-not-leak)
  (test-set!-arrow-inferred)
  (test-function-expr-boundary-does-not-leak)
  (test-set!-function-inferred)
  (test-const-arrow-inferred)
  (test-let-arrow-inferred)
  (test-var-arrow-inferred)
  (test-const-arrow-caller-inherits)
  (test-const-function-expr-inferred)
  (test-const-arrow-annotate-wraps)
  (test-const-arrow-annotate-no-wrap-when-pure)
  (displayln "All infer tests passed."))
