#lang racket

;; Python effect-annotation extraction (Pass 1.5, Python front-end).
;;
;; Recognizes the `@effects({...})` decorator and turns it into a
;; uir-effect-row. The annotation is a single positional dict literal:
;;
;;   @effects({
;;     "llm": {"max": 5},
;;     "db":  {"reads": "search_index"},
;;   })
;;   def summarize(...): ...
;;
;; Each key is an opaque effect name; the value is a constraint dict with
;; optional keys:
;;   "max"      -> integer budget
;;   "scope"    -> resource/information-flow qualifier string
;;   "reads"    -> shorthand for scope "reads=<value>"
;;   "writes"   -> shorthand for scope "writes=<value>"
;;   "ops"      -> list of operation-name strings (empty means all)
;;   "modality" -> "may" | "must" | "must-not" (default "may")
;;
;; Absence of a name from the row is prohibition, per the contract algebra.

(require racklr/uir)

(provide
  effects-decorator?
  extract-effects-row)

;; Name of a callee expression (uir-var or uir-symbol) as a symbol.
(define (callee-symbol c)
  (string->symbol (uir-symbol-name (if (uir-var? c) (uir-var-name c) c))))

;; Is this decorator a call to `effects`?
(define (effects-decorator? d)
  (and (uir-call? d)
       (let ([c (uir-call-callee d)])
         (and (or (uir-var? c) (uir-symbol? c))
              (eq? (callee-symbol c) 'effects)))))

;; Look up a string key in a uir-record, returning the value or #f.
(define (record-lookup rec key-str)
  (for/or ([e (in-list (uir-record-entries rec))])
    (and (uir-string? (car e))
         (string=? (uir-string-value (car e)) key-str)
         (cdr e))))

(define (str->symbol s)
  (string->symbol s))

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

;; Extract a uir-effect-row from an `@effects(...)` decorator, or #f.
(define (extract-effects-row d)
  (and (effects-decorator? d)
       (let ([args (uir-call-args d)])
         (and (= (length args) 1)
              (uir-record? (first args))
              (uir-effect-row
               (map constraint->entry (uir-record-entries (first args))))))))

(module+ main
  (displayln "racklr/effects/python-annot — Python @effects extraction loaded."))
