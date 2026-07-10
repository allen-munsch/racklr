#lang racket

(require racklr/tree)

(provide tok? cst-kids tag-of kids-of find-kid find-list find-node-or-list
         extract-var-kind extract-identifier-name)

;; ── CST-walking helpers for TypeScript lowering ───────────────────────

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

(define (extract-var-kind vm tk-type)
  (define kid (first (kids-of vm)))
  (cond [(and (tok? kid tk-type) (eq? (tk-type kid) 'Var)) "var"]
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Const)) "const"]
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Let)) "let"]
        [(and (cst-node? kid) (eq? (tag-of kid) 'let_)) "let"]
        [else "var"]))

(define (extract-identifier-name node tk-type tk-value)
  (define in (find-kid node 'identifierName))
  (and in
       (let ([ident (find-kid in 'identifier)])
         (and ident
              (let ([tok (first (kids-of ident))])
                (and (tok? tok tk-type) (eq? (tk-type tok) 'Identifier)
                     (tk-value tok)))))))
