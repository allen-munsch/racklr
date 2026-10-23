#lang racket

;; TypeScript/JS effect-annotation extraction (Pass 1.5, TS/JS/TSX front-ends).
;;
;; Recognizes the `withEffects(row)(fn)` wrapper and turns the row literal
;; into a uir-effect-row:
;;
;;   const summarize = withEffects({ llm: { max: 5 } })(async (input) => ...);
;;
;; The wrapper lowers to a curried call in UIR:
;;
;;   (call (call (var withEffects) (list ROW)) (list FN))
;;
;; where ROW is a uir-record with the same key/value convention as the Python
;; `@effects(...)` decorator: each key is an opaque effect name; the value is
;; a constraint record with optional keys "max", "scope", "reads", "writes",
;; "ops", and "modality" ("may" | "must" | "must-not"). Absence of a name from
;; the row is prohibition, per the contract algebra.

(require racklr/uir)

(provide
  with-effects-call?
  extract-with-effects-row
  extract-with-effects-fn
  record->effect-row)

;; Name of a callee expression (uir-var or uir-symbol) as a symbol.
(define (callee-symbol c)
  (string->symbol (uir-symbol-name (if (uir-var? c) (uir-var-name c) c))))

;; Is this expression a `withEffects(row)(fn)` curried call?
(define (with-effects-call? e)
  (and (uir-call? e)
       (let ([callee (uir-call-callee e)])
         (and (uir-call? callee)
              (let ([c (uir-call-callee callee)])
                (and (or (uir-var? c) (uir-symbol? c))
                     (eq? (callee-symbol c) 'withEffects)))))))

;; Look up a string key in a uir-record, returning the value or #f.
(define (record-lookup rec key-str)
  (for/or ([e (in-list (uir-record-entries rec))])
    (and (uir-string? (car e))
         (string=? (uir-string-value (car e)) key-str)
         (cdr e))))

(define (entry-modality constraints)
  (define m (record-lookup constraints "modality"))
  (if (and m (uir-string? m))
      (case (string->symbol (uir-string-value m))
        [(must) 'must]
        [(must-not must_not deny) 'must-not]
        [else 'may])
      'may))

(define (entry-budget constraints)
  (define m (record-lookup constraints "max"))
  (and m (uir-number? m) (string->number (uir-number-value m))))

(define (entry-scope constraints)
  (define s (record-lookup constraints "scope"))
  (cond
    [(and s (uir-string? s)) (uir-symbol (uir-string-value s))]
    [(record-lookup constraints "reads")
     => (λ (v) (and (uir-string? v)
                    (uir-symbol (format "reads=~a" (uir-string-value v)))))]
    [(record-lookup constraints "writes")
     => (λ (v) (and (uir-string? v)
                    (uir-symbol (format "writes=~a" (uir-string-value v)))))]
    [else #f]))

(define (entry-ops constraints)
  (define o (record-lookup constraints "ops"))
  (if (and o (uir-list? o))
      (map (λ (x) (and (uir-string? x) (uir-symbol (uir-string-value x))))
           (uir-list-items o))
      '()))

;; One (cons uir-string-name constraint-value) entry -> uir-effect-entry.
(define (constraint->entry e)
  (define name (car e))
  (define constraints (cdr e))
  (define name-str (if (uir-string? name) (uir-string-value name) "?effect"))
  (if (and constraints (uir-record? constraints))
      (uir-effect-entry
       (uir-symbol name-str)
       (entry-modality constraints)
       (entry-ops constraints)
       (entry-budget constraints)
       (entry-scope constraints))
      (uir-effect-entry (uir-symbol name-str) 'may '() #f #f)))

(define (record->effect-row rec)
  (uir-effect-row (map constraint->entry (uir-record-entries rec))))

;; Extract the uir-effect-row from a `withEffects(row)(fn)` wrapper, or #f.
(define (extract-with-effects-row e)
  (and (with-effects-call? e)
       (let* ([inner (uir-call-callee e)]
              [args (uir-call-args inner)])
         (and (= (length args) 1)
              (uir-record? (first args))
              (record->effect-row (first args))))))

;; Extract the wrapped function from a `withEffects(row)(fn)` wrapper, or #f.
(define (extract-with-effects-fn e)
  (and (with-effects-call? e)
       (let ([args (uir-call-args e)])
         (and (= (length args) 1) (first args)))))

(module+ main
  (displayln "racklr/effects/ts-annot — TS/JS withEffects extraction loaded."))
