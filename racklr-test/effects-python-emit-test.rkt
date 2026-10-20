#lang racket

;; Python `@__withEffects` emission with inference + runtime instrumentation.
;;
;; `emit-python` must run effect inference on its input, so an unannotated
;; function that performs an effect (directly or transitively) is emitted with
;; the `@__withEffects(...)` guard and the runtime preamble, while a pure
;; function stays unannotated. Direct effect calls are instrumented with
;; `__perform`.

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/emit-python)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))

(define (fn name body #:params [params '()])
  (uir-fn (if name (uir-symbol name) #f) params body #f #f))

(define (prog . stmts)
  (uir-block stmts))

(define (count-substr s sub)
  (length (regexp-match* (regexp-quote sub) s)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-no-effects-no-preamble)
  (define out (emit-python (prog (fn "plain" (uir-null)))))
  (check-false (string-contains? out "__withEffects")
               "a pure function must not emit the guard"))

(define (test-inferred-effects-emit-guard)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define out (emit-python p))
  (check-true (string-contains? out "def __withEffects")
              "preamble emitted for inferred effects")
  (check-true (string-contains? out "@__withEffects(")
              "effectful function guarded")
  (check-true (string-contains? out "\"llm\"")
              "inferred llm row emitted"))

(define (test-transitive-inferred-effects)
  ;; main calls summarize, which performs llm; both functions are guarded.
  (define summarize
    (fn "summarize" (uir-call (dot (v "llm") "query")
                              (list (uir-string "hi")))))
  (define p (prog summarize
                  (fn "main" (uir-call (v "summarize") '()))))
  (define out (emit-python p))
  (check-equal? (count-substr out "@__withEffects(") 2
                "both summarize and main carry the guard"))

(define (test-direct-effect-call-instrumented)
  (define out (emit-python (prog (fn "summarize"
                                     (uir-call (dot (v "llm") "query")
                                               (list (uir-string "hi")))))))
  (check-true (string-contains? out "__perform(\"llm\", \"query\", lambda:")
              "direct llm.query call is instrumented"))

(define (test-preamble-defines-perform)
  (define out (emit-python (prog (fn "summarize"
                                     (uir-call (dot (v "llm") "query")
                                               (list (uir-string "hi")))))))
  (check-true (string-contains? out "def __perform(")
              "preamble defines __perform")
  (check-true (string-contains? out "__effects_stack")
              "preamble defines the effect-context stack"))

(define (test-top-level-effect-call-emits-preamble)
  ;; A direct effect call outside any function has no effect row, but it is
  ;; instrumented, so the preamble (with __perform) must still be emitted.
  (define out (emit-python (prog (uir-call (dot (v "llm") "query")
                                           (list (uir-string "hi"))))))
  (check-true (string-contains? out "def __perform(")
              "top-level effect call still emits the runtime preamble")
  (check-true (string-contains? out "__perform(\"llm\", \"query\",")
              "top-level effect call is instrumented"))

(module+ main
  (test-no-effects-no-preamble)
  (test-inferred-effects-emit-guard)
  (test-transitive-inferred-effects)
  (test-direct-effect-call-instrumented)
  (test-preamble-defines-perform)
  (test-top-level-effect-call-emits-preamble)
  (displayln "All python-emit effect-guard tests passed."))
