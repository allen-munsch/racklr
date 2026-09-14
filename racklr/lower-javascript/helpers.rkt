#lang racket

;; ── Shared CST helpers for the JavaScript lowering split (B75) ──────
;; tok? / cst-kids / tag-of / kids-of / find-kid / find-list /
;; find-node-or-list are used by both stmt.rkt and expr.rkt.

(require racklr/tree)

(provide tok? cst-kids tag-of kids-of find-kid find-list find-node-or-list)

(define (tok? x tk-type)
  (and (not (cst-node? x)) (not (null? x)) (not (eq? x 'none))
       (not (pair? x))
       (with-handlers ([exn:fail? (λ (_) #f)])
         (tk-type x) #t)))

(define (cst-kids n) (filter cst-node? (cst-node-children n)))
(define (tag-of n) (cst-node-tag n))
(define (kids-of n) (cst-node-children n))

(define (find-kid n tag)
  (for/or ([k (kids-of n)] #:when (and (cst-node? k) (eq? (tag-of k) tag))) k))

(define (find-list n)
  (for/or ([k (kids-of n)] #:when (pair? k)) k))

(define (find-node-or-list n)
  (for/or ([k (kids-of n)] #:when (or (cst-node? k) (pair? k))) k))
