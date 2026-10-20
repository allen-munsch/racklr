#lang racket

;; Python `@__validate_effects` emission with inference wired in.
;;
;; `emit-python` must run effect inference on its input, so an unannotated
;; function that performs an effect (directly or transitively) is emitted with
;; the `@__validate_effects(...)` guard and the runtime preamble, while a
;; pure function stays unannotated.

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
  (check-false (string-contains? out "__validate_effects")
               "a pure function must not emit the guard"))

(define (test-inferred-effects-emit-guard)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define out (emit-python p))
  (check-true (string-contains? out "def __validate_effects")
              "preamble emitted for inferred effects")
  (check-true (string-contains? out "@__validate_effects(")
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
  (check-equal? (count-substr out "@__validate_effects(") 2
                "both summarize and main carry the guard"))

(module+ main
  (test-no-effects-no-preamble)
  (test-inferred-effects-emit-guard)
  (test-transitive-inferred-effects)
  (displayln "All python-emit effect-guard tests passed."))
