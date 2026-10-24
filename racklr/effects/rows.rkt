#lang racket

;; racklr/effects/rows.rkt
;;
;; The effect-row algebra: building rows from classified items, canonical
;; ordering, and the fixpoint union that preserves the deferred `unknown`
;; modality.

(require racklr/uir
         racklr/effects/contract
         racklr/effects/imports)

(provide empty-row effects->row infer-row-union)

(define (empty-row)
  (uir-effect-row '()))

;; Canonicalize row entry order so equal? is order-insensitive.
(define (canonical-row r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

;; A list of effect-name symbols and/or effect-caps → a row. 'unknown names get
;; modality 'unknown; everything else is a detected 'may. Caps are merged by
;; domain with their op sets unioned; a coarse entry (empty ops = "all ops")
;; dominates, so it is never narrowed by a precise one.
(define (effects->row items)
  (define (merge-ops a b)
    (cond [(null? a) '()] [(null? b) '()] [else (remove-duplicates (append a b))]))
  (define dom-ops (make-hash))
  (for ([it (in-list items)])
    (define-values (d o)
      (cond
        [(effect-cap? it) (values (effect-cap-domain it) (effect-cap-ops it))]
        [(eq? it 'unknown) (values 'unknown '())]
        [else (values it '())]))
    (hash-update! dom-ops d (λ (prev) (merge-ops prev o)) o))
  (canonical-row
   (uir-effect-row
    (for/list ([(d o) (in-hash dom-ops)])
      (uir-effect-entry
       (uir-symbol (symbol->string d))
       (if (eq? d 'unknown) 'unknown 'may)
       (map (λ (s) (uir-symbol (symbol->string s))) o)
       #f
       #f)))))

;; Union that keeps the deferred 'unknown modality (contract's union
;; collapses unknown→may for same-name merges).
(define (infer-row-union r1 r2)
  (canonical-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (effect-row-union r1 r2)))])
      (if (eq? (effect-name-symbol e) 'unknown)
          (struct-copy uir-effect-entry e [modality 'unknown])
          e)))))
