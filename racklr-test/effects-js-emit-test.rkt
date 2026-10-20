#lang racket

;; JS/TS `__withEffects` runtime-guard emission (gap 4 emit half).
;;
;; The emitter must render an effect row as a JS object literal, wrap any
;; `uir-fn` (or async/gen-wrapped declaration) that carries a row in
;; `__withEffects(row, fn)`, and prepend the `__withEffects` runtime preamble
;; once, only when the program actually uses effects.

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/emit-javascript)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (jsdot b f) (uir-call (sym "dot") (list b (sym f))))

(define (llm-row)
  (uir-effect-row
   (list (uir-effect-entry (sym "llm") 'may '() 5 #f))))

(define (empty-row)
  (uir-effect-row '()))

(define (decl . stmts)
  (uir-block stmts))

(define (named-fn name row)
  (uir-set! (sym name)
            (uir-fn #f '() (uir-block '()) #f row)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-no-effects-no-preamble)
  (define out (emit-javascript (decl (named-fn "plain" #f))))
  (check-false (string-contains? out "__withEffects")
               "plain program must not emit the guard")
  (check-true (string-contains? out "function plain")
              "plain declaration still emits"))

(define (test-declaration-with-effects-emits-guard)
  (define out (emit-javascript (decl (named-fn "summarize" (llm-row)))))
  (check-true (string-contains? out "function __withEffects")
              "preamble defines the runtime guard")
  (check-true (string-contains? out "const summarize = __withEffects(")
              "declaration becomes a const assignment through the guard")
  (check-true (string-contains? out "\"llm\"")
              "row name emitted")
  (check-true (string-contains? out "\"max\": 5")
              "budget emitted"))

(define (test-fn-expression-with-effects-emits-guard)
  (define out (emit-javascript (uir-fn #f '() (uir-block '()) #f (llm-row))))
  (check-true (string-contains? out "__withEffects(")
              "fn expression is wrapped")
  (check-true (string-contains? out "\"modality\": \"may\"")
              "default modality emitted"))

(define (test-async-declaration-with-effects)
  (define fn (uir-fn #f '() (uir-block '()) #f (llm-row)))
  (define out (emit-javascript (decl (uir-set! (sym "load")
                                               (uir-call (sym "async-fn") (list fn))))))
  (check-true (string-contains? out "const load = __withEffects(")
              "async declaration wrapped through the guard")
  (check-true (string-contains? out "async function(")
              "async fn expression preserved"))

(define (test-empty-row-still-guarded)
  (define out (emit-javascript (decl (named-fn "pure" (empty-row)))))
  (check-true (string-contains? out "__withEffects({}, ")
              "an explicitly empty row still guards"))

(define (test-inferred-effects-emit-guard)
  ;; emit-javascript runs inference, so an unannotated declaration that
  ;; performs an effect is wrapped in __withEffects without a hand-written row.
  (define p (decl (uir-set! (sym "summarize")
                            (uir-fn #f '()
                                    (uir-block
                                     (list (uir-call (jsdot (v "llm") "query")
                                                     (list (uir-string "hi")))))
                                    #f #f))))
  (define out (emit-javascript p))
  (check-true (string-contains? out "function __withEffects")
              "preamble emitted for inferred effects")
  (check-true (string-contains? out "const summarize = __withEffects(")
              "inferred declaration wrapped")
  (check-true (string-contains? out "\"llm\"")
              "inferred llm row emitted"))

;; const/let/var-bound arrows and function expressions: inference collects them
;; (the set! sits inside the var-decl call) and annotate-effects wraps the value
;; in __withEffects, so the guard surfaces end-to-end.

(define (var-decl kind binding)
  (uir-call (sym kind) (list binding)))

(define (llm-arrow)
  (uir-call (sym "=>")
            (list (uir-list '())
                  (uir-call (jsdot (v "llm") "query") (list (uir-string "hi"))))))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (uir-string (car p)) (cadr p))) pairs)))

;; (call (call (var withEffects) (list ROW)) (list FN)) — the UIR shape a
;; hand-written TS/JS `withEffects(row)(fn)` annotation lowers to.
(define (with-effects-wrapper row fn)
  (uir-call (uir-call (uir-var (sym "withEffects")) (list row))
            (list fn)))

(define (llm-constraint-record)
  (record (list "llm" (record (list "max" (uir-number "5"))))))

(define (test-const-arrow-emits-guard)
  (define out (emit-javascript (decl (var-decl "const" (uir-set! (sym "f") (llm-arrow))))))
  (check-true (string-contains? out "function __withEffects")
              "preamble emitted for a const-bound arrow")
  (check-true (string-contains? out "const f = __withEffects(")
              "const-bound arrow wrapped through the guard")
  (check-true (string-contains? out "() => __perform(\"llm\", \"query\", function() { return llm.query(\"hi\"); })")
              "arrow body is instrumented inside the guard"))

(define (test-let-arrow-emits-guard)
  (define out (emit-javascript (decl (var-decl "let" (uir-set! (sym "f") (llm-arrow))))))
  (check-true (string-contains? out "let f = __withEffects(")
              "let-bound arrow wrapped through the guard"))

(define (test-var-arrow-emits-guard)
  (define out (emit-javascript (decl (var-decl "var" (uir-set! (sym "f") (llm-arrow))))))
  (check-true (string-contains? out "var f = __withEffects(")
              "var-bound arrow wrapped through the guard"))

(define (test-const-function-expr-emits-guard)
  (define fexpr (uir-call (sym "function")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") (list (uir-string "hi"))))))
  (define out (emit-javascript (decl (var-decl "const" (uir-set! (sym "f") fexpr)))))
  (check-true (string-contains? out "const f = __withEffects(")
              "const-bound function expression wrapped through the guard")
  (check-true (string-contains? out "function() {")
              "function expression preserved inside the guard"))

(define (test-const-pure-arrow-no-guard)
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (sym "+") (list (uir-number "1") (uir-number "2"))))))
  (define out (emit-javascript (decl (var-decl "const" (uir-set! (sym "f") arrow)))))
  (check-false (string-contains? out "__withEffects")
               "pure const-bound arrow emits no guard"))

(define (test-withEffects-annotation-emits-guard)
  (define w (with-effects-wrapper (llm-constraint-record) (llm-arrow)))
  (define out (emit-javascript (decl (var-decl "const" (uir-set! (sym "summarize") w)))))
  (check-true (string-contains? out "function __withEffects")
              "preamble emitted for a withEffects annotation")
  (check-true (string-contains? out "const summarize = __withEffects(")
              "withEffects annotation becomes a guarded declaration")
  (check-true (string-contains? out "\"llm\"")
              "row name emitted")
  (check-true (string-contains? out "\"max\": 5")
              "budget emitted")
  (check-true (string-contains? out "() => __perform(\"llm\", \"query\", function() { return llm.query(\"hi\"); })")
              "wrapped arrow body is instrumented"))

(define (test-bare-withEffects-emits-guard)
  (define w (with-effects-wrapper (llm-constraint-record) (llm-arrow)))
  (define out (emit-javascript (decl w)))
  (check-true (string-contains? out "function __withEffects")
              "preamble emitted for a bare withEffects annotation")
  (check-true (string-contains? out "__withEffects(")
              "bare withEffects wrapper rendered as the guard"))

(define (test-direct-effect-call-instrumented)
  ;; A direct effect call is wrapped in __perform(name, op, thunk) so the
  ;; runtime guard can enforce budgets/prohibitions at call time.
  (define p (decl (uir-set! (sym "summarize")
                            (uir-fn #f '()
                                    (uir-block
                                     (list (uir-call (jsdot (v "llm") "query")
                                                     (list (uir-string "hi")))))
                                    #f #f))))
  (define out (emit-javascript p))
  (check-true (string-contains? out "__perform(\"llm\", \"query\", function() { return llm.query(\"hi\"); })")
              "direct llm.query call is instrumented"))

(define (test-preamble-defines-perform)
  (define out (emit-javascript (decl (named-fn "summarize" (llm-row)))))
  (check-true (string-contains? out "function __perform(")
              "preamble defines __perform")
  (check-true (string-contains? out "__effects_stack")
              "preamble defines the effect-context stack"))

(define (test-top-level-effect-call-emits-preamble)
  ;; A direct effect call outside any function has no effect row, but it is
  ;; instrumented, so the preamble (with __perform) must still be emitted.
  (define out (emit-javascript (decl (uir-call (jsdot (v "llm") "query")
                                                (list (uir-string "hi"))))))
  (check-true (string-contains? out "function __perform(")
              "top-level effect call still emits the runtime preamble")
  (check-true (string-contains? out "__perform(\"llm\", \"query\",")
              "top-level effect call is instrumented"))

(module+ main
  (test-no-effects-no-preamble)
  (test-declaration-with-effects-emits-guard)
  (test-fn-expression-with-effects-emits-guard)
  (test-async-declaration-with-effects)
  (test-empty-row-still-guarded)
  (test-inferred-effects-emit-guard)
  (test-const-arrow-emits-guard)
  (test-let-arrow-emits-guard)
  (test-var-arrow-emits-guard)
  (test-const-function-expr-emits-guard)
  (test-const-pure-arrow-no-guard)
  (test-withEffects-annotation-emits-guard)
  (test-bare-withEffects-emits-guard)
  (test-direct-effect-call-instrumented)
  (test-preamble-defines-perform)
  (test-top-level-effect-call-emits-preamble)
  (displayln "All JS emit effect-guard tests passed."))
