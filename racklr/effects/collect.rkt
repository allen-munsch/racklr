#lang racket

;; racklr/effects/collect.rkt
;;
;; Program-surface collection: harvest named functions, methods, class names and
;; superclass maps from a UIR program, plus the MRO helper used to resolve
;; inherited methods.

(require racklr/uir
         racklr/effects/walk
         racklr/effects/shape)

(provide
  collect-functions
  collect-methods
  collect-class-names
  collect-supers
  mro-chain
  class-name-of-name
  owner-of-name)

;; name-symbol → function body, for every named function reachable from
;; `program`: named uir-fn nodes plus the JS/TS `set!`-bound encodings
;; (function declarations, async/generator declarations, and bound arrows).
(define (collect-functions program)
  (define h (make-hash))
  (define (visit! n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (hash-set! h (string->symbol (uir-symbol-name nm)) (uir-fn-body n))
             (void)))]
      [(named-function n)
       => (λ (p) (hash-set! h (car p) (cdr p)))]
      [else (void)]))
  (walk-uir program visit!)
  h)

;; Class name symbol (or #f) for a `uir-class` node.
(define (class-name-symbol n)
  (and (uir-class? n)
       (let ([nm (uir-class-name n)])
         (and (uir-symbol? nm)
              (string->symbol (uir-symbol-name nm))))))

;; Super class name symbols for a `uir-class` node, in left-to-right base order.
;; The Python lowerer emits each positional base as a UIR expr: a bare name
;; (`BaseView`) lowers to a uir-var, and a dotted name (`models.Model`) lowers to
;; a uir-get chain whose last field is the class name. A subscripted generic
;; base (`Generic[T]`) or other non-symbol base yields no name for that position.
(define (class-super-names n)
  (and (uir-class? n)
       (filter values
               (map (lambda (sup)
                      (cond
                        [(uir-var? sup) (string->symbol (uir-symbol-name (uir-var-name sup)))]
                        [(uir-get? sup) (get-field-symbol (uir-get-field sup))]
                        [else #f]))
                    (uir-class-super n)))))

;; Class-qualified name symbol → method body, for every method of every
;; `uir-class` node reachable from `program`. Methods are keyed as `Class.method`
;; so a `self.m()` inside that class (or `Other.m()` elsewhere) resolves to the
;; same call-graph node as a bare function.
(define (collect-methods program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname
        (for ([m (in-list (uir-class-methods n))])
          (when (uir-method? m)
            (define mname (and (uir-method-name m)
                               (uir-symbol? (uir-method-name m))
                               (string->symbol (uir-symbol-name (uir-method-name m)))))
            (when mname
              (hash-set! h (qualified-name-symbol cname mname) (uir-method-body m))))))))
  (walk-uir program visit!)
  h)

;; Set of all class name symbols reachable from `program`.
(define (collect-class-names program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname (hash-set! h cname #t))))
  (walk-uir program visit!)
  h)

;; Class name symbol → (listof super-name-symbol), for every `uir-class` node
;; reachable from `program`. Each list preserves left-to-right superclass order
;; (Python's C3 MRO); classes with no superclass are absent.
(define (collect-supers program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (define sups (class-super-names n))
      (when (and cname (pair? sups))
        (hash-set! h cname sups))))
  (walk-uir program visit!)
  h)

;; Linearized superclass chain for `class-name` (most derived first, then each
;; super), computed as a depth-first walk over the superclass map with a cycle
;; guard. Includes `class-name` itself. Used to resolve inherited methods:
;; `self.m()` searches the chain for the first class declaring `m`.
(define (mro-chain class-name supers)
  ;; Pre-order DFS over the superclass graph (leftmost base first), visiting each
  ;; class once. This approximates Python's C3 linearization for effect
  ;; resolution: `self.m()` searches the chain for the first class declaring `m`,
  ;; which is exactly the leftmost-depth-first order mixins follow.
  (define seen (make-hash))
  (define out '())
  (let loop ([cls class-name])
    (when (and cls (not (hash-has-key? seen cls)))
      (hash-set! seen cls #t)
      (set! out (cons cls out))
      (for ([s (in-list (hash-ref supers cls '()))])
        (loop s))))
  (reverse out))

;; Class name symbol before the first dot in a `Class.method` node name, or #f
;; for a bare function name.
(define (class-name-of-name n)
  (define s (symbol->string n))
  (define owner-str (regexp-replace #rx"[.].*$" s ""))
  (and (not (string=? owner-str s)) (string->symbol owner-str)))

;; Enclosing class name symbol (or #f) for a call-graph node keyed `Class.method`.
(define (owner-of-name methods n)
  (and (hash-has-key? methods n) (class-name-of-name n)))
