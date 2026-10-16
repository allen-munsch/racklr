#lang racket

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/infer)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))

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

(define (test-annotate-writes-back)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define annotated (annotate-effects p))
  (define f (car (uir-block-stmts annotated)))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

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
  (test-annotate-writes-back)
  (displayln "All infer tests passed."))
