#lang racket

;; racklr/effects/receiver-type.rkt
;;
;; Per-function receiver-type environment: tracks the class of local variables
;; and `self`/`cls` attributes from constructor assignments
;; (`x = KnownClass(...)`) and simple aliases (`x = y`, `x = self.attr`), so a
;; later `x.m(...)` call can resolve through the receiver class's derived effect
;; capability instead of the flat method-name tables. This is the first half of
;; receiver-type dataflow: the class name is inferred from the call chain (a
;; constructor whose callee names a known class), not from a name table.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/walk)

(provide
  receiver-types? receiver-types receiver-types-locals receiver-types-self-attrs
  receiver-types-merge
  infer-receiver-types
  receiver-class-of
  constructor-class)

(struct receiver-types (locals self-attrs) #:transparent)
;; locals:     hash name-symbol → class-name-symbol
;; self-attrs: hash attr-symbol → class-name-symbol (on self / cls)

(define (self-or-cls-var? v)
  (and (uir-var? v)
       (memq (string->symbol (uir-symbol-name (uir-var-name v))) '(self cls))))

;; Name symbol bound by a `set!` left-hand side. The Python lowerer emits a
;; uir-var target; the JS/TS lowerer emits a uir-symbol target. Returns #f for
;; a compound target (a uir-get attribute assignment is handled by the caller).
(define (set!-lhs-symbol lhs)
  (cond
    [(uir-var? lhs) (string->symbol (uir-symbol-name (uir-var-name lhs)))]
    [(uir-symbol? lhs) (string->symbol (uir-symbol-name lhs))]
    [else #f]))

;; Class name the constructor callee names, when it is a known class: a bare
;; callee (`QuerySet(...)`) or a dotted callee whose final field names a known
;; class (`models.CharField(...)`). Returns a class-name symbol or #f.
(define (constructor-class callee class-defs)
  (define nm (callee-name callee))
  (cond
    [(and nm (hash-has-key? class-defs nm)) nm]
    [(uir-get? callee)
     (define f (get-field-symbol (uir-get-field callee)))
     (and f (hash-has-key? class-defs f) f)]
    [else #f]))

;; Build the receiver-type environment for one function body. Assignments are
;; processed in source (pre-order) order; `class-defs` is the set of known class
;; names (a hash keyed by class-name symbol), which gates which constructor
;; callees count as a known receiver type.
(define (infer-receiver-types body class-defs)
  (define locals (make-hash))
  (define self-attrs (make-hash))
  (define (value-class value)
    (cond
      [(uir-call? value) (constructor-class (uir-call-callee value) class-defs)]
      [(uir-var? value)
       (hash-ref locals (string->symbol (uir-symbol-name (uir-var-name value))) #f)]
      [(uir-get? value)
       (define attr (get-field-symbol (uir-get-field value)))
       (and attr (self-or-cls-var? (uir-get-base value))
            (hash-ref self-attrs attr #f))]
      [else #f]))
  (define (process-set! n)
    (define cls (value-class (uir-set!-value n)))
    (when cls
      (define lhs (uir-set!-name n))
      (cond
        [(and (uir-get? lhs) (get-field-symbol (uir-get-field lhs))
              (self-or-cls-var? (uir-get-base lhs)))
         (hash-set! self-attrs (get-field-symbol (uir-get-field lhs)) cls)]
        [(set!-lhs-symbol lhs)
         => (λ (nm) (hash-set! locals nm cls))]
        [else (void)])))
  (walk-body body (λ (n) (when (uir-set!? n) (process-set! n))))
  (receiver-types locals self-attrs))

;; Overlay `overlay`'s entries onto `base` (overlay wins on a key collision).
(define (hash-overlay base overlay)
  (define out (hash-copy base))
  (for ([(k v) (in-hash overlay)]) (hash-set! out k v))
  out)

;; Merge two receiver-type environments; `overlay`'s locals and self-attrs win.
;; Used to layer a function's own receiver tracking over its class's inherited
;; self-attributes (collected across __init__ and all methods).
(define (receiver-types-merge base overlay)
  (receiver-types
   (hash-overlay (receiver-types-locals base) (receiver-types-locals overlay))
   (hash-overlay (receiver-types-self-attrs base) (receiver-types-self-attrs overlay))))

;; Class name (symbol) tracked for a receiver expression `base`, or #f. `base`
;; is either a local variable (`qs`) or a `self`/`cls` attribute (`self.cursor`).
(define (receiver-class-of env base)
  (cond
    [(uir-var? base)
     (hash-ref (receiver-types-locals env)
               (string->symbol (uir-symbol-name (uir-var-name base))) #f)]
    [(uir-get? base)
     (define attr (get-field-symbol (uir-get-field base)))
     (and attr (self-or-cls-var? (uir-get-base base))
          (hash-ref (receiver-types-self-attrs env) attr #f))]
    [else #f]))
