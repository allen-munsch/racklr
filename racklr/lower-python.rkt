#lang racket

(require racklr/tree
         racklr/uir
         "lower-python/helpers.rkt"
         "lower-python/expr.rkt"
         "lower-python/pattern.rkt"
         "lower-python/stmt.rkt")

(provide lower-python)

(define (lower-python cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (cond
    [(eq? tag 'single_input)
     (define kids (node-children cst))
     (if (null? kids)
         (uir-null)
         (let* ([first-kid (first kids)]
                [t (any-tree-tag first-kid)])
           (cond [(eq? t 'simple_stmts) (lower-simple-stmts first-kid tk-type tk-value)]
                 [(eq? t 'compound_stmt) (lower-compound first-kid tk-type tk-value)]
                 [else (uir-symbol (format "?single-~a" t))])))]
    [(eq? tag 'file_input)
      (define stmts
        (for/list ([c (any-tree-children cst)]
                   #:when (and (list? c) (pair? c)))
          (for/list ([cc c]
                     #:when (and (cst-node? cc)
                                 (eq? (any-tree-tag cc) 'group)))
            (define gk (node-children cc))
            (if (and (pair? gk) (eq? (any-tree-tag (first gk)) 'stmt))
                (lower-python (first gk) tk-type tk-value)
                (uir-null)))))
      (uir-block (apply append stmts))]
    [(eq? tag 'stmt)
     (define kids (node-children cst))
     (if (null? kids)
         (uir-null)
         (let* ([first-kid (first kids)]
                [t (any-tree-tag first-kid)])
           (cond [(eq? t 'simple_stmts) (lower-simple-stmts first-kid tk-type tk-value)]
                 [(eq? t 'compound_stmt) (lower-compound first-kid tk-type tk-value)]
                 [else (uir-symbol (format "?stmt-~a" t))])))]
    [(eq? tag 'compound_stmt)
     (lower-compound cst tk-type tk-value)]
    [else (uir-symbol (format "?top-~a" tag))]))

(module+ main
  (displayln "racklr/lower-python — Python3 CST → UIR lowering loaded."))
