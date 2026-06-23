#lang racket

(require racklr/tree
         racklr/uir
         racklr/gen-json-parser)

(provide lower-json)

(define (lower-token tok)
  (match (token-type tok)
    ['STRING
     (define raw (token-value tok))
     (uir-string (substring raw 1 (sub1 (string-length raw))))]
    ['NUMBER (uir-number (token-value tok))]
    ['true (uir-bool #t)]
    ['false (uir-bool #f)]
    ['null (uir-null)]
    [other (error 'lower "unexpected token type: ~e" other)]))

(define (lower-value node)
  (match-define (cst-node 'value (list child) _) node)
  (cond [(token? child) (lower-token child)]
        [(cst-node? child)
         (match (cst-node-tag child)
           ['obj (lower-obj child)]
           ['arr (lower-arr child)]
           [other (error 'lower "unexpected value child tag: ~e" other)])]))

(define (lower-pair node)
  (match-define (cst-node 'pair (list key-tok _colon child) _) node)
  (define raw-key (token-value key-tok))
  (cons (uir-string (substring raw-key 1 (sub1 (string-length raw-key))))
        (lower-value child)))

(define (lower-obj node)
  (define children (cst-node-children node))
  (cond [(= (length children) 2)
         ;; empty object: {, }
         (uir-record '())]
        [else
         ;; non-empty: {, pair-node, list-of-group-nodes, }
         (define first-pair (list-ref children 1))
         (define group-nodes (list-ref children 2))
         (define entries
           (cons (lower-pair first-pair)
                 (for/list ([g group-nodes])
                   (define gkids (cst-node-children g))
                   (lower-pair (list-ref gkids 1)))))
         (uir-record entries)]))

(define (lower-arr node)
  (define children (cst-node-children node))
  (cond [(= (length children) 2)
         ;; empty array: [, ]
         (uir-list '())]
        [else
         ;; non-empty: [, value-node, list-of-group-nodes, ]
         (define first-value (list-ref children 1))
         (define group-nodes (list-ref children 2))
         (define items
           (cons (lower-value first-value)
                 (for/list ([g group-nodes])
                   (define gkids (cst-node-children g))
                   (lower-value (list-ref gkids 1)))))
         (uir-list items)]))

(define (lower-json cst)
  (match-define (cst-node 'json (list val-child _eof) _) cst)
  (lower-value val-child))

(module+ main
  (displayln "racklr/lower-json — JSON CST → UIR lowering loaded."))
