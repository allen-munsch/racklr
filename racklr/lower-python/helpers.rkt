#lang racket

(require racklr/tree)

(provide node-children first-token token-like? token-value-of
         cst-tokens-deep cst-tokens unwrap-expr)

;; ── CST-walking helpers ────────────────────────────────────────────

;; Get only the CST-node children (skip '() 'none source-pos conses)
(define (node-children cst)
  (filter cst-node? (any-tree-children cst)))

;; Get the first token child of a CST node
(define (first-token cst tk-type tk-value)
  (for/or ([c (any-tree-children cst)]
           #:when (token-like? c tk-type))
    (cons (tk-type c) (tk-value c))))

;; Check if something responds to tk-type (is a generated token)
(define (token-like? x tk-type)
  (and (not (cst-node? x))
       (not (null? x))
       (not (eq? x 'none))
       (not (pair? x))
       (with-handlers ([exn:fail? (λ (_) #f)])
         (tk-type x) #t)))

;; Get token value from a token child
(define (token-value-of x tk-value)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (tk-value x)))

;; Get all tokens recursively from a CST subtree
(define (cst-tokens-deep cst tk-type tk-value)
  (define result '())
  (let walk ([node cst])
    (for ([c (any-tree-children node)])
      (cond [(token-like? c tk-type)
             (set! result (cons (list (tk-type c) (tk-value c)) result))]
            [(cst-node? c) (walk c)]
            [(and (list? c) (pair? c))
             (for ([cc c]) (when (cst-node? cc) (walk cc)))])))
  (reverse result))

;; Get all token children types/values (immediate only)
(define (cst-tokens cst tk-type tk-value)
  (for/list ([c (any-tree-children cst)]
             #:when (token-like? c tk-type))
    (list (tk-type c) (tk-value c))))

;; Walk through single-child indirections: test → or_test → and_test → ...
;; Returns the first non-intermediate node (with a "real" tag)
(define (unwrap-expr cst)
  (define expr-tags '(test test_nocond testlist testlist_star_expr))
  (if (member (any-tree-tag cst) expr-tags)
      (let ([kids (node-children cst)])
        (if (= (length kids) 1)
            (unwrap-expr (first kids))
            cst))
      cst))

