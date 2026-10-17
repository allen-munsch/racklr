#lang racket

;; Clojure effect-annotation extraction (Phase 7, effects front-end).
;;
;; Recognizes `^{:effects {...}}` metadata on a defn and turns it into a
;; uir-effect-row. The annotation is a Clojure map with keyword keys:
;;
;;   ^{:effects {:llm {:max 5} :db {:reads "search_index"}}}
;;   (defn summarize [x] (llm/query x))
;;
;; Each key is an opaque effect name (keyword → bare string); the value is
;; a constraint map with optional keyword keys:
;;   :max      -> integer budget
;;   :scope    -> resource/information-flow qualifier
;;   :reads    -> shorthand for scope "reads=<value>"
;;   :writes   -> shorthand for scope "writes=<value>"
;;   :ops      -> vector of operation names (empty means all)
;;   :modality -> :may | :must | :must-not  (default :may)
;;
;; Absence of a name from the row is prohibition, per the contract algebra.

(require racklr/uir)

(provide
  keyword-symbol-bare
  extract-effects-row)

;; Bare name of a Clojure keyword symbol: ":llm" → "llm", "::x" → "x".
(define (keyword-symbol-bare s)
  (if (and (string? s) (>= (string-length s) 1) (char=? (string-ref s 0) #\:))
      (let ([t (substring s 1)])
        (if (and (>= (string-length t) 1) (char=? (string-ref t 0) #\:))
            (substring t 1)
            t))
      s))

;; Bare string of a keyword symbol or string literal value, else #f.
(define (value-bare v)
  (cond [(uir-symbol? v) (keyword-symbol-bare (uir-symbol-name v))]
        [(uir-string? v) (uir-string-value v)]
        [else #f]))

;; Look up a keyword key (by bare name) in a uir-record, returning the value.
(define (record-lookup-keyword rec key)
  (for/or ([e (in-list (uir-record-entries rec))])
    (and (uir-symbol? (car e))
         (string=? (keyword-symbol-bare (uir-symbol-name (car e))) key)
         (cdr e))))

(define (entry-modality constraints)
  (define m (record-lookup-keyword constraints "modality"))
  (define bare (and m (value-bare m)))
  (if bare
      (case (string->symbol bare)
        [(must) 'must]
        [(must-not must_not deny) 'must-not]
        [else 'may])
      'may))

(define (entry-budget constraints)
  (define m (record-lookup-keyword constraints "max"))
  (and m (uir-number? m) (string->number (uir-number-value m))))

(define (entry-scope constraints)
  (define s (record-lookup-keyword constraints "scope"))
  (define r (record-lookup-keyword constraints "reads"))
  (define w (record-lookup-keyword constraints "writes"))
  (cond
    [(and s (value-bare s)) (uir-symbol (value-bare s))]
    [(and r (value-bare r)) (uir-symbol (format "reads=~a" (value-bare r)))]
    [(and w (value-bare w)) (uir-symbol (format "writes=~a" (value-bare w)))]
    [else #f]))

;; :ops may be a vector literal (lowered as (vector ...)) or a uir-list.
(define (entry-ops constraints)
  (define o (record-lookup-keyword constraints "ops"))
  (define items
    (cond
      [(and o (uir-list? o)) (uir-list-items o)]
      [(and o (uir-call? o)
            (uir-symbol? (uir-call-callee o))
            (string=? (uir-symbol-name (uir-call-callee o)) "vector"))
       (uir-call-args o)]
      [else '()]))
  (for/list ([x items])
    (define b (value-bare x))
    (and b (uir-symbol b))))

;; One (cons keyword-name constraint-map) entry → uir-effect-entry.
(define (constraint->entry e)
  (define key (car e))
  (define constraints (cdr e))
  (define name (and (uir-symbol? key) (keyword-symbol-bare (uir-symbol-name key))))
  (uir-effect-entry
   (uir-symbol (or name "?"))
   (entry-modality constraints)
   (entry-ops constraints)
   (entry-budget constraints)
   (entry-scope constraints)))

;; Extract the effects row from a metadata map: {:effects {:name {...} ...}}.
(define (extract-effects-row meta)
  (and (uir-record? meta)
       (let ([fx (or (record-lookup-keyword meta "effects")
                     (record-lookup-keyword meta "__effects__"))])
         (and (uir-record? fx)
              (uir-effect-row
               (map constraint->entry (uir-record-entries fx)))))))

(module+ main
  (displayln "racklr/effects/clojure-annot — Clojure ^{:effects ...} extraction loaded."))
