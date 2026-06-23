#lang racket

(require rackunit
         racklr/tree)

;; ── Source Position ───────────────────────────────────────────────

(let ([p (pos 1 2 3)])
  (check-equal? (source-pos-line p) 1)
  (check-equal? (source-pos-col p) 2)
  (check-equal? (source-pos-offset p) 3)
  (check-true (source-pos? p)))

(check-exn exn:fail?
           (λ () (pos -1 0 0)))

(check-exn exn:fail?
           (λ () (pos 0 0.5 0)))

;; ── Leaf ──────────────────────────────────────────────────────────

(let ([l (leaf 'identifier "foo" #:start (pos 1 1 0) #:end (pos 1 4 3))])
  (check-true (cst-leaf? l))
  (check-false (cst-node? l))
  (check-true (any-tree? l))
  (check-equal? (cst-leaf-tag l) 'identifier)
  (check-equal? (cst-leaf-text l) "foo")
  (check-equal? (source-pos-line (car (cst-leaf-range l))) 1)
  (check-equal? (source-pos-col (car (cst-leaf-range l))) 1)
  (check-equal? (source-pos-offset (car (cst-leaf-range l))) 0)
  (check-equal? (source-pos-line (cdr (cst-leaf-range l))) 1)
  (check-equal? (source-pos-col (cdr (cst-leaf-range l))) 4)
  (check-equal? (source-pos-offset (cdr (cst-leaf-range l))) 3))

;; ── Node ──────────────────────────────────────────────────────────

(define kid1 (leaf 'number "42" #:start (pos 1 1 0) #:end (pos 1 3 2)))
(define kid2 (leaf 'number "7" #:start (pos 1 5 4) #:end (pos 1 6 5)))
(define n (node '+ (list kid1 kid2)))

(check-true (cst-node? n))
(check-false (cst-leaf? n))
(check-true (any-tree? n))
(check-equal? (cst-node-tag n) '+)
(check-equal? (length (cst-node-children n)) 2)

;; Range inferred from children
(check-equal? (source-pos-offset (car (cst-node-range n))) 0)
(check-equal? (source-pos-offset (cdr (cst-node-range n))) 5)

;; ── Generic Accessors ─────────────────────────────────────────────

(check-equal? (any-tree-tag kid1) 'number)
(check-equal? (any-tree-tag n) '+)
(check-equal? (any-tree-text kid1) "42")
(check-equal? (any-tree-children n) (list kid1 kid2))
(check-exn exn:fail? (λ () (any-tree-text n)))
(check-exn exn:fail? (λ () (any-tree-children kid1)))
(check-exn exn:fail? (λ () (any-tree-tag "not-a-tree")))

;; ── Explicit Range Override ────────────────────────────────────────

(let ([n2 (node 'expr (list kid1)
                #:start (pos 5 0 100)
                #:end (pos 5 10 110))])
  (check-equal? (source-pos-offset (car (cst-node-range n2))) 100)
  (check-equal? (source-pos-offset (cdr (cst-node-range n2))) 110))

;; ── Serialization Round-Trip (S-Exp) ──────────────────────────────

(let* ([original n]
       [s (tree->sexp original)]
       [back (sexp->tree s)])
  (check-equal? (any-tree-tag back) '+)
  (check-equal? (length (any-tree-children back)) 2)
  (check-equal? (any-tree-text (first (any-tree-children back))) "42")
  (check-equal? (any-tree-text (second (any-tree-children back))) "7")
  (check-equal? (source-pos-offset (car (any-tree-range back))) 0)
  (check-equal? (source-pos-offset (cdr (any-tree-range back))) 5))

;; ── JSON Round-Trip ───────────────────────────────────────────────

(let* ([j (tree->json n)]
       [back (json->tree j)])
  (check-equal? (any-tree-tag back) '+)
  (check-equal? (length (any-tree-children back)) 2)
  (check-equal? (any-tree-text (first (any-tree-children back))) "42"))

;; ── Empty Node ────────────────────────────────────────────────────

(let ([empty (node 'empty (list))])
  (check-true (cst-node? empty))
  (check-equal? (length (cst-node-children empty)) 0))

(displayln "All tests passed.")
