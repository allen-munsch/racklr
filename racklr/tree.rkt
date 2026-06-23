#lang racket

(provide
 ;; Source positions
 source-pos source-pos? source-pos-line source-pos-col source-pos-offset
 pos
 ;; Tree nodes
 cst-node cst-node? cst-node-tag cst-node-children cst-node-range
 cst-leaf cst-leaf? cst-leaf-tag cst-leaf-text cst-leaf-range
 ;; Predicates
 any-tree? any-tree-tag any-tree-children any-tree-text any-tree-range
 ;; Construction helpers
 leaf node
 ;; Serialization
 tree->sexp sexp->tree
 tree->json json->tree)

;; ── Source Position ───────────────────────────────────────────────

(struct source-pos (line col offset) #:transparent
  #:guard (λ (line col offset name)
            (unless (and (exact-nonnegative-integer? line)
                         (exact-nonnegative-integer? col)
                         (exact-nonnegative-integer? offset))
              (error 'source-pos "line, col, offset must be nonnegative integers"))
            (values line col offset)))

(define (pos line col offset)
  (source-pos line col offset))

;; ── Tree Nodes ────────────────────────────────────────────────────

;; A range is a pair (start-pos . end-pos)
;; No dedicated struct — just cons of source-pos, to keep it lightweight.

(struct cst-node (tag children range) #:transparent)
;; tag: symbol — the grammar rule or node type (e.g. 'expr, 'statement, '+)
;; children: list of cst-node | cst-leaf
;; range: (cons source-pos source-pos) covering start to end

(struct cst-leaf (tag text range) #:transparent)
;; tag: symbol — token type (e.g. 'identifier, 'integer, 'string)
;; text: string — the literal source text
;; range: (cons source-pos source-pos)

;; ── Generic Accessors ─────────────────────────────────────────────

(define (any-tree? v)
  (or (cst-node? v) (cst-leaf? v)))

(define (any-tree-tag t)
  (cond [(cst-node? t) (cst-node-tag t)]
        [(cst-leaf? t) (cst-leaf-tag t)]
        [else (error 'any-tree-tag "not a tree: ~e" t)]))

(define (any-tree-range t)
  (cond [(cst-node? t) (cst-node-range t)]
        [(cst-leaf? t) (cst-leaf-range t)]
        [else (error 'any-tree-range "not a tree: ~e" t)]))

(define (any-tree-children t)
  (if (cst-node? t)
      (cst-node-children t)
      (error 'any-tree-children "leaf has no children: ~e" t)))

(define (any-tree-text t)
  (if (cst-leaf? t)
      (cst-leaf-text t)
      (error 'any-tree-text "node has no text: ~e" t)))

;; ── Construction Helpers ──────────────────────────────────────────

(define (node tag children #:start [start #f] #:end [end #f])
  (define range
    (cond [(and start end) (cons start end)]
          [else
           ;; Infer range from children
           (define kids (filter any-tree? children))
           (if (null? kids)
               (cons (pos 0 0 0) (pos 0 0 0))
               (cons (car (any-tree-range (first kids)))
                     (cdr (any-tree-range (last kids)))))]) )
  (cst-node tag children range))

(define (leaf tag text #:start [start #f] #:end [end #f])
  (define range
    (if (and start end)
        (cons start end)
        (let ([p (pos 0 0 0)])
          (cons p p))))
  (cst-leaf tag text range))

;; ── Serialization ─────────────────────────────────────────────────

(define (tree->sexp t)
  (cond [(cst-leaf? t)
         `(leaf ,(cst-leaf-tag t) ,(cst-leaf-text t)
                ,(source-pos-line (car (cst-leaf-range t)))
                ,(source-pos-col (car (cst-leaf-range t)))
                ,(source-pos-offset (car (cst-leaf-range t)))
                ,(source-pos-line (cdr (cst-leaf-range t)))
                ,(source-pos-col (cdr (cst-leaf-range t)))
                ,(source-pos-offset (cdr (cst-leaf-range t))))]
        [(cst-node? t)
         `(node ,(cst-node-tag t)
                ,(map tree->sexp (cst-node-children t))
                ,(source-pos-line (car (cst-node-range t)))
                ,(source-pos-col (car (cst-node-range t)))
                ,(source-pos-offset (car (cst-node-range t)))
                ,(source-pos-line (cdr (cst-node-range t)))
                ,(source-pos-col (cdr (cst-node-range t)))
                ,(source-pos-offset (cdr (cst-node-range t))))]
        [else (error 'tree->sexp "not a tree: ~e" t)]))

(define (sexp->tree s)
  (match s
    [`(leaf ,tag ,text ,sl ,sc ,so ,el ,ec ,eo)
     (cst-leaf tag text (cons (pos sl sc so) (pos el ec eo)))]
    [`(node ,tag ,children ,sl ,sc ,so ,el ,ec ,eo)
     (cst-node tag (map sexp->tree children) (cons (pos sl sc so) (pos el ec eo)))]
    [_ (error 'sexp->tree "invalid tree sexp: ~e" s)]))

(require json)

;; Walk sexp to convert symbols to strings for JSON compat
(define (sexp->jsexpr s)
  (cond [(symbol? s) (symbol->string s)]
        [(list? s) (map sexp->jsexpr s)]
        [(pair? s) (cons (sexp->jsexpr (car s)) (sexp->jsexpr (cdr s)))]
        [else s]))

;; Reverse: convert type-tag strings back to symbols
;; A tree node in jsexpr form looks like: ("leaf" tag text sl sc so el ec eo)
;; or: ("node" tag children sl sc so el ec eo)
;; where `tag` is a string. A children list has tree nodes as elements.
;; We distinguish: if the list has at least 2 elements and the second is a string,
;; it's a tree node — convert first two elements to symbols.
(define (jsexpr->sexp j)
  (match j
    [(? list? l)
     (if (and (pair? l) (pair? (cdr l)) (string? (cadr l)))
         (cons (string->symbol (car l))
               (cons (string->symbol (cadr l))
                     (map jsexpr->sexp (cddr l))))
         (map jsexpr->sexp l))]
    [(? pair? p) (cons (jsexpr->sexp (car p)) (jsexpr->sexp (cdr p)))]
    [_ j]))

(define (tree->json t)
  (jsexpr->string (sexp->jsexpr (tree->sexp t))))

(define (json->tree jstr)
  (sexp->tree (jsexpr->sexp (string->jsexpr jstr))))

(module+ main
  (displayln "racklr/tree — Universal CST types loaded."))
