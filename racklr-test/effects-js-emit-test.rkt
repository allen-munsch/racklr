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

(module+ main
  (test-no-effects-no-preamble)
  (test-declaration-with-effects-emits-guard)
  (test-fn-expression-with-effects-emits-guard)
  (test-async-declaration-with-effects)
  (test-empty-row-still-guarded)
  (displayln "All JS emit effect-guard tests passed."))
