#lang racket

(require racklr/tree racklr/uir "helpers.rkt" "expr.rkt" "pattern.rkt")

(provide lower-simple-stmts lower-simple-stmt lower-stmt
         lower-return find-rhs-in-children lower-expr-stmt
         lower-import-name lower-import-from
         lower-assert lower-raise lower-del lower-global lower-nonlocal
         collect-names lower-yield lower-async
         lower-compound lower-if lower-if-rest lower-while lower-for
         lower-match lower-case-block lower-try lower-except-clause lower-with
         lower-decorated lower-decorator
         lower-funcdef lower-parameters lower-tfpdef
         lower-classdef lower-method
         lower-suite lower-block-or-stmt lower-block)

(define (lower-simple-stmts cst tk-type tk-value)
  (define kids (node-children cst))
  (if (= (length kids) 1)
      (lower-simple-stmt (first kids) tk-type tk-value)
      (uir-block (map (λ (k) (lower-simple-stmt k tk-type tk-value)) kids))))

(define (lower-simple-stmt cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (let* ([inner (first kids)]
             [unwrapped (if (eq? (any-tree-tag inner) 'group)
                            (let ([gkids (node-children inner)])
                              (if (null? gkids) inner (first gkids)))
                            inner)])
        (lower-stmt unwrapped tk-type tk-value))))

(define (lower-stmt cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (define kids (node-children cst))
  ;; Unwrap through intermediate wrappers
  (cond
    [(eq? tag 'group) (if (null? kids) (uir-null) (lower-stmt (first kids) tk-type tk-value))]
    [(eq? tag 'flow_stmt) (if (null? kids) (uir-null) (lower-stmt (first kids) tk-type tk-value))]
    [(eq? tag 'import_stmt) (if (null? kids) (uir-null) (lower-stmt (first kids) tk-type tk-value))]
    [(eq? tag 'pass_stmt) (uir-null)]
    [(eq? tag 'return_stmt) (lower-return cst tk-type tk-value)]
    [(eq? tag 'expr_stmt) (lower-expr-stmt cst tk-type tk-value)]
    [(eq? tag 'break_stmt) (uir-block (list (uir-symbol "break")))]
    [(eq? tag 'continue_stmt) (uir-block (list (uir-symbol "continue")))]
    [(eq? tag 'import_name) (lower-import-name cst tk-type tk-value)]
    [(eq? tag 'import_from) (lower-import-from cst tk-type tk-value)]
    [(eq? tag 'assert_stmt) (lower-assert cst tk-type tk-value)]
    [(eq? tag 'yield_stmt) (lower-yield cst tk-type tk-value)]
    [(eq? tag 'raise_stmt) (lower-raise cst tk-type tk-value)]
    [(eq? tag 'del_stmt) (lower-del cst tk-type tk-value)]
    [(eq? tag 'global_stmt) (lower-global cst tk-type tk-value)]
    [(eq? tag 'nonlocal_stmt) (lower-nonlocal cst tk-type tk-value)]
    [(eq? tag 'block) (lower-block cst tk-type tk-value)]
    [(eq? tag 'stmt) (if (null? kids) (uir-null) (lower-block-or-stmt (first kids) tk-type tk-value))]
    [else (uir-symbol (format "?stmt-~a" tag))]))

(define (lower-return cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-return (uir-null))
      (uir-return (lower-expr (first kids) tk-type tk-value))))

;; Walk through group/list wrappers to find a testlist_star_expr (RHS)
(define (find-rhs-in-children nodes)
  (for/or ([n nodes])
    (let walk ([x n])
      (cond [(cst-node? x)
             (if (eq? (any-tree-tag x) 'testlist_star_expr)
                 x
                 (find-rhs-in-children (any-tree-children x)))]
            [(and (list? x) (pair? x))
             (find-rhs-in-children x)]
            [else #f]))))

(define (lower-expr-stmt cst tk-type tk-value)
  (define kids (node-children cst))
  (cond [(null? kids) (uir-null)]
        [(= (length kids) 1)
         (lower-expr (first kids) tk-type tk-value)]
        [else
         (begin
         ;; Check for annotated assignment: expr_stmt -> testlist_star_expr, group(annassign ...)
         (define annassign-node
           (for/or ([k (cdr kids)])
             (and (eq? (any-tree-tag k) 'group)
                  (let ([gk (node-children k)])
                    (and (pair? gk) (eq? (any-tree-tag (first gk)) 'annassign)
                         (first gk))))))
         (if annassign-node
             (let* ([lhs (lower-expr (first kids) tk-type tk-value)]
                    [ann-kids (node-children annassign-node)]
                    ;; annassign children (cst nodes only): type-test (initializer-group)?
                    ;; (COLON token is filtered by node-children)
                    [type-node (if (pair? ann-kids) (first ann-kids) #f)]
                    [type-expr (if type-node (lower-expr type-node tk-type tk-value) (uir-null))]
                    [init-group (if (>= (length ann-kids) 2) (second ann-kids) #f)]
                    [value-expr (if init-group
                                   (let ([igk (node-children init-group)])
                                     (if (pair? igk)
                                         (lower-expr (first igk) tk-type tk-value)
                                         #f))
                                   #f)])
               (uir-ann-set! lhs type-expr value-expr))
             (let ()
          (define augassign-node
            (for/or ([k (cdr kids)])
              (and (eq? (any-tree-tag k) 'group)
                   (let ([gk (node-children k)])
                     (and (pair? gk) (eq? (any-tree-tag (first gk)) 'augassign)
                          (first gk))))))
          (if augassign-node
              (let* ([lhs (first kids)]
                     [group-node (for/or ([k (cdr kids)]) (and (eq? (any-tree-tag k) 'group) k))]
                     [gk (node-children group-node)]
                     [rhs-node (if (>= (length gk) 2) (second gk) #f)]
                      [op-tok (cst-tokens-deep augassign-node tk-type tk-value)]
                      [op-name (if (pair? op-tok) (second (car op-tok)) "?aug")]
                     [op-base (string-replace op-name "=" "")]
                     [lhs-expr (lower-expr lhs tk-type tk-value)])
                (uir-set! lhs-expr
                          (uir-call (uir-symbol op-base)
                                    (list lhs-expr
                                          (if rhs-node (lower-expr rhs-node tk-type tk-value) (uir-null))))))
              (let* ([lhs (first kids)]
                     [rhs (find-rhs-in-children (cdr kids))])
                (if rhs
                    (uir-set! (lower-expr lhs tk-type tk-value)
                              (lower-expr rhs tk-type tk-value))
                    (lower-expr lhs tk-type tk-value)))))))]))

(define (lower-import-name cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define names (for/list ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (uir-import (uir-symbol (string-join names "."))
              '()))

(define (lower-import-from cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define names (for/list ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (uir-import (uir-symbol (string-join names "."))
              '()))

(define (lower-assert cst tk-type tk-value)
  ;; assert_stmt: 'assert' test (',' test)?
  ;; Lower to uir-call for round-trip fidelity
  (define kids (node-children cst))
  (if (null? kids)
      (uir-call (uir-symbol "assert") '())
      (let ([test-expr (lower-expr (first kids) tk-type tk-value)])
        (if (>= (length kids) 2)
            (uir-call (uir-symbol "assert") (list test-expr (lower-expr (second kids) tk-type tk-value)))
            (uir-call (uir-symbol "assert") (list test-expr))))))

;; raise_stmt: 'raise' (test ('from' test)?)?
(define (lower-raise cst tk-type tk-value)
  ;; raise_stmt children: [RAISE token, group]
  ;; The group wraps: test (optional: ('from' test))
  (define kids (node-children cst))
  (define all-toks (cst-tokens-deep cst tk-type tk-value))
  (define has-from? (for/or ([t all-toks] #:when (eq? (first t) 'FROM)) #t))
  (cond [(null? kids) (uir-call (uir-symbol "raise") '())]
        [else
         (let* ([group-node (first kids)]
                [gk (node-children group-node)]
                [exc-expr (if (pair? gk) (lower-expr (first gk) tk-type tk-value) (uir-null))])
           (if has-from?
               ;; raise expr from cause — find the second test after FROM
               (let* ([cause-expr
                       (let loop ([remaining (cdr (any-tree-children group-node))])
                         (cond [(null? remaining) #f]
                               [(and (cst-node? (car remaining))
                                     (eq? (any-tree-tag (car remaining)) 'test))
                                (lower-expr (car remaining) tk-type tk-value)]
                               [(cst-node? (car remaining))
                                ;; Recurse into groups to find the test
                                (let ([found (loop (any-tree-children (car remaining)))])
                                  (if found found (loop (cdr remaining))))]
                               [(and (list? (car remaining)) (pair? (car remaining)))
                                (let ([found (loop (car remaining))])
                                  (if found found (loop (cdr remaining))))]
                               [else (loop (cdr remaining))]))])
                 (uir-call (uir-symbol "raise") (list exc-expr (or cause-expr (uir-null)))))
               ;; raise expr (no from)
               (uir-call (uir-symbol "raise") (list exc-expr))))]))


;; del x, y, ... → uir-call to "del" with list of exprs
(define (lower-del cst tk-type tk-value)
  (define exprs (lower-exprlist cst tk-type tk-value))
  (uir-call (uir-symbol "del") exprs))

;; global x, y, ... → uir-call to "global" with list of symbols
(define (lower-global cst tk-type tk-value)
  (define names (collect-names cst tk-type tk-value))
  (uir-call (uir-symbol "global") (map uir-symbol names)))

;; nonlocal x, y, ... → uir-call to "nonlocal" with list of symbols
(define (lower-nonlocal cst tk-type tk-value)
  (define names (collect-names cst tk-type tk-value))
  (uir-call (uir-symbol "nonlocal") (map uir-symbol names)))

;; Collect all NAME token values from a CST node (recursively, handles list-wrapping)
(define (collect-names cst tk-type tk-value)
  (define names '())
  (let walk ([node cst])
    (when (cst-node? node)
      (for ([c (any-tree-children node)])
        (cond [(cst-node? c) (walk c)]
              [(and (list? c) (pair? c)) (for-each walk c)]
              [(and (not (null? c)) (not (eq? c 'none))
                    (with-handlers ([exn:fail? (lambda (_) #f)])
                      (eq? (tk-type c) 'NAME)))
               (set! names (cons (tk-value c) names))]))))
  (reverse names))

;; Lower exprlist into a list of UIR expressions
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
      ;; file_input: (NEWLINE | stmt)* — stmts are list-wrapped from parse-star
      ;; Each list item is a group containing either NEWLINE token or a stmt node
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

;; ── Compound statements ────────────────────────────────────────────

(define (lower-compound cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (let ([first-kid (first kids)])
        (match (any-tree-tag first-kid)
          ['if_stmt (lower-if first-kid tk-type tk-value)]
          ['while_stmt (lower-while first-kid tk-type tk-value)]
          ['for_stmt (lower-for first-kid tk-type tk-value)]
          ['try_stmt (lower-try first-kid tk-type tk-value)]
          ['with_stmt (lower-with first-kid tk-type tk-value)]
           ['funcdef (lower-funcdef first-kid tk-type tk-value)]
           ['classdef (lower-classdef first-kid tk-type tk-value)]
           ['async_stmt (lower-async first-kid tk-type tk-value)]
           ['decorated (lower-decorated first-kid tk-type tk-value)]
            ['match_stmt (lower-match first-kid tk-type tk-value)]
            [_ (uir-symbol (format "?comp-~a" (any-tree-tag first-kid)))]))))

(define (lower-if cst tk-type tk-value)
  ;; if_stmt children: [IF tok, test, COLON tok, block, ...]
  ;; After block: optional LIST of elif groups, optional else group
  (define (wrap-body uir)
    (if (uir-block? uir) uir (uir-block (list uir))))
  (define raw-kids (any-tree-children cst))
  (define test-expr (lower-expr (second raw-kids) tk-type tk-value))
  (define then-body (wrap-body (lower-block-or-stmt (fourth raw-kids) tk-type tk-value)))
  ;; Collect elif/else after position 4
  (define rest (cddddr raw-kids))
  ;; Flatten lists and filter to CST nodes
  (define groups
    (apply append
           (for/list ([k rest])
             (cond [(list? k) (filter cst-node? k)]
                   [(cst-node? k) (list k)]
                   [else '()]))))
  (define else-body (lower-if-rest groups tk-type tk-value))
  (uir-if test-expr then-body else-body))

(define (lower-if-rest groups tk-type tk-value)
  (define (wrap-body uir)
    (if (uir-block? uir) uir (uir-block (list uir))))
  (if (null? groups)
      (uir-null)
      (let* ([group (first groups)]
             [gkids (node-children group)])
        (if (= (length gkids) 1)
            ;; Else group: [block]
            (wrap-body (lower-block-or-stmt (first gkids) tk-type tk-value))
            ;; Elif group: [test, block]
            (let ([elif-test (lower-expr (first gkids) tk-type tk-value)]
                  [elif-body (wrap-body (lower-block-or-stmt (second gkids) tk-type tk-value))]
                  [elif-else (lower-if-rest (cdr groups) tk-type tk-value)])
              (uir-if elif-test elif-body elif-else))))))

(define (lower-while cst tk-type tk-value)
  (define kids (node-children cst))
  (define test-expr (lower-expr (first kids) tk-type tk-value))
  (define body (lower-block-or-stmt (second kids) tk-type tk-value))
  (define else-body
    (if (>= (length kids) 3)
        (lower-block-or-stmt (first (node-children (third kids))) tk-type tk-value)
        (uir-null)))
  (uir-while test-expr body else-body))

(define (lower-for cst tk-type tk-value)
  (define kids (node-children cst))
  ;; for_stmt kids: [exprlist, testlist, block, (else-block)]
  (define exprlist (if (>= (length kids) 1) (first kids) #f))
  (define testlist (if (>= (length kids) 2) (second kids) #f))
  (define block (if (>= (length kids) 3) (third kids) #f))
  (define else-block (if (>= (length kids) 4) (fourth kids) #f))
  ;; Extract loop variable name from exprlist
  (define var-toks (if exprlist (cst-tokens-deep exprlist tk-type tk-value) '()))
  (define var-name (for/or ([t var-toks] #:when (eq? (first t) 'NAME)) (second t)))
  ;; Lower iterable expression
  (define iterable (if testlist (lower-expr testlist tk-type tk-value) (uir-symbol "?iter")))
  ;; Lower body
  (define body (if block (lower-block-or-stmt block tk-type tk-value) (uir-null)))
  ;; Lower else body
  (define else-body (if else-block (lower-block-or-stmt else-block tk-type tk-value) (uir-null)))
  (uir-for-each (uir-symbol (or var-name "?var"))
                iterable
                body
                else-body))

;; ── Yield ──────────────────────────────────────────────────────────

(define (lower-yield cst tk-type tk-value)
  ;; yield_stmt > yield_expr > yield_arg
  (define kids (node-children cst))
  (if (null? kids)
      (uir-yield (uir-null) #f)
      (let* ([yield-expr (first kids)]
             [ye-kids (node-children yield-expr)])
        ;; yield_expr children: (YIELD token) yield_arg
        ;; yield_arg children: empty for bare yield, or [testlist/test] for yield value
        ;; For 'yield from': yield_arg has a FROM token as first raw child
        (if (null? ye-kids)
            (uir-yield (uir-null) #f)
            (let* ([yield-arg (first ye-kids)]
                   [ya-kids (node-children yield-arg)])
              ;; Check for FROM token in raw children of yield_arg
              (define has-from?
                (for/or ([c (any-tree-children yield-arg)])
                  (and (not (cst-node? c))
                       (not (null? c))
                       (not (eq? c 'none))
                       (with-handlers ([exn:fail? (lambda (_) #f)])
                         (eq? (tk-type c) 'FROM)))))
              (if (null? ya-kids)
                  (uir-yield (uir-null) has-from?)
                  (let* ([wrapped (first ya-kids)] ;; testlist or test
                         [value (lower-expr wrapped tk-type tk-value)])
                    (uir-yield value has-from?))))))))

;; ── Async ──────────────────────────────────────────────────────────

;; async_stmt wraps [group (funcdef | with_stmt | for_stmt)]
(define (lower-async cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (let* ([group-node (first kids)]
             [gk (node-children group-node)])
        (if (null? gk)
            (uir-null)
            (let ([inner (first gk)])
              (match (any-tree-tag inner)
                ['funcdef (lower-funcdef inner tk-type tk-value)]
                ['for_stmt (lower-for inner tk-type tk-value)]
                ['with_stmt (lower-with inner tk-type tk-value)]
                [_ (uir-symbol (format "?async-~a" (any-tree-tag inner)))]))))))

;; ── Decorated (decorators) ─────────────────────────────────────────

(define (lower-decorated cst tk-type tk-value)
  ;; decorated: [decorators, group(funcdef/classdef)]
  (define kids (node-children cst))
  (define decos-node (if (>= (length kids) 1) (first kids) #f))
  (define func-node (if (>= (length kids) 2) (second kids) #f))
  ;; Lower the decorators list
  (define decorators
    (if decos-node
        ;; decorators has a list of decorator nodes in its children
        (let* ([raw (any-tree-children decos-node)]
               [decorator-list
                (for/or ([c raw])
                  (and (list? c) (pair? c) (cst-node? (car c)) c))])
          (if decorator-list
              (map (lambda (d) (lower-decorator d tk-type tk-value)) decorator-list)
              '()))
        '()))
  ;; Lower the inner function/class
  (define inner
    (if func-node
        ;; func-node is a group wrapping funcdef/classdef
        (let* ([gk (node-children func-node)]
               [inner-node (if (pair? gk) (first gk) #f)])
          (if inner-node
              (match (any-tree-tag inner-node)
                ['funcdef (lower-funcdef inner-node tk-type tk-value)]
                ['classdef (lower-classdef inner-node tk-type tk-value)]
                [_ (uir-symbol (format "?dec-inner-~a" (any-tree-tag inner-node)))])
              (uir-null)))
        (uir-null)))
  (uir-decorated decorators inner))

;; Lower a single decorator: @name or @name(args)
(define (lower-decorator cst tk-type tk-value)
  ;; decorator children: [dotted_name, optional arglist]
  (define kids (node-children cst))
  (define name-node (if (pair? kids) (first kids) #f))
  ;; arglist may be in a list wrapper
  (define arglist-node
    (if (>= (length kids) 2)
        (second kids)
        #f))
  (define name-expr
    (if name-node
        (lower-expr name-node tk-type tk-value)
        (uir-symbol "?decorator")))
  (define args
    (if (and arglist-node (eq? (any-tree-tag arglist-node) 'arglist))
        (lower-arglist arglist-node tk-type tk-value)
        '()))
  (uir-call name-expr args))


;; ── Match/case ──────────────────────────────────────────────────────

;; match_stmt: [subject_expr, case_block+ (list-wrapped)]
(define (lower-match cst tk-type tk-value)
  (define raw-kids (any-tree-children cst))
  ;; subject_expr is the first cst-node child
  (define subj-node
    (for/or ([c raw-kids] #:when (cst-node? c)) c))
  (define subject
    (if (and subj-node (eq? (any-tree-tag subj-node) 'subject_expr))
        (lower-expr (first (node-children subj-node)) tk-type tk-value)
        (uir-symbol "?match-subject")))
  ;; case_blocks are inside a list-wrapped child
  (define case-list
    (for/or ([c raw-kids] #:when (and (list? c) (pair? c)))
      (filter cst-node? c)))
  (define cases
    (if case-list
        (map (λ (k) (lower-case-block k tk-type tk-value)) case-list)
        '()))
  (uir-match subject cases))

;; case_block: [patterns, guard?, block]
(define (lower-case-block cst tk-type tk-value)
  (define kids (node-children cst))
  (define pat-node (first kids))
  (define pat (lower-patterns pat-node tk-type tk-value))
  (define-values (guard body-node)
    (if (and (>= (length kids) 3) (eq? (any-tree-tag (second kids)) 'guard))
        (values (lower-expr (second (any-tree-children (second kids))) tk-type tk-value) (third kids))
        (values #f (second kids))))
  (uir-case pat guard (lower-block body-node tk-type tk-value)))

;; ── Pattern lowering ───────────────────────────────────────────────

;; patterns: open_sequence_pattern | pattern
(define (lower-except-clause except-group tk-type tk-value)
  ;; except-group is a group node containing [except_clause, block]
  (define kids (node-children except-group))
  (define except-node (if (>= (length kids) 1) (first kids) #f))
  (define block-node (if (>= (length kids) 2) (second kids) #f))
  ;; except_clause: [EXCEPT token, group (test + optional as name)]
  ;; The group wraps (test ('as' name)?)?
  (define except-kids (if except-node (node-children except-node) '()))
  (define opt-group (findf (lambda (k) (eq? (any-tree-tag k) 'group)) except-kids))
  (define opt-kids (if opt-group (node-children opt-group) '()))
  ;; opt-kids: [test] or [test, group]. The group wraps ('as' name)
  (define exc-type
    (if (>= (length opt-kids) 1)
        (lower-expr (first opt-kids) tk-type tk-value)
        (uir-null)))
  (define exc-name
    (if (>= (length opt-kids) 2)
        (let* ([as-group (second opt-kids)]
               [as-kids (node-children as-group)]
               [name-node (if (>= (length as-kids) 1) (first as-kids) #f)])
          (if name-node (lower-name name-node tk-type tk-value) (uir-null)))
        (uir-null)))
  (define handler-body (if block-node (lower-block-or-stmt block-node tk-type tk-value) (uir-null)))
  (list exc-type exc-name handler-body))

(define (lower-try cst tk-type tk-value)
  ;; try_stmt: group containing [TRY, COLON, block, group]
  (define outer-group (first (node-children cst)))
  (define all-kids (any-tree-children outer-group))
  ;; Find block (try body) — it's the first block node
  (define try-body-node (findf (lambda (k) (and (cst-node? k) (eq? (any-tree-tag k) 'block))) all-kids))
  (define try-body (if try-body-node (lower-block-or-stmt try-body-node tk-type tk-value) (uir-null)))
  ;; Find inner group (handles except/else/finally) — it's the last group
  (define group-nodes (filter cst-node? all-kids))
  (define inner-group (findf (lambda (k) (eq? (any-tree-tag k) 'group)) (cdr (memf (lambda (k) (eq? (any-tree-tag k) 'block)) group-nodes))))
  (define inner-kids (if inner-group (any-tree-children inner-group) '()))
  ;; Two alternatives:
  ;; Alt 1 (with except): [list-of-except-groups, else-or-none, finally-or-none]
  ;; Alt 2 (finally only): [FINALLY, COLON, block]
  (define catches
    (cond [(and (>= (length inner-kids) 1) (list? (first inner-kids)))
           (map (lambda (eg) (lower-except-clause eg tk-type tk-value)) (first inner-kids))]
          [else '()]))
  (define else-body
    (cond [(and (>= (length inner-kids) 2) (list? (first inner-kids)))
           ;; Alt 1: second child is else-or-none
           (let ([en (second inner-kids)])
             (if (cst-node? en)
                 (let ([bk (findf (lambda (k) (and (cst-node? k) (eq? (any-tree-tag k) 'block)))
                                  (any-tree-children en))])
                   (if bk (lower-block-or-stmt bk tk-type tk-value) (uir-null)))
                 #f))]
          [else #f]))
  (define finally-body
    (cond [(and (>= (length inner-kids) 1) (list? (first inner-kids)))
           ;; Alt 1: third child is finally-or-none
           (if (>= (length inner-kids) 3)
               (let ([fn (third inner-kids)])
                 (if (cst-node? fn)
                     (let ([bk (findf (lambda (k) (and (cst-node? k) (eq? (any-tree-tag k) 'block)))
                                      (any-tree-children fn))])
                       (if bk (lower-block-or-stmt bk tk-type tk-value) (uir-null)))
                     #f))
               #f)]
          [(and (>= (length inner-kids) 2) (not (list? (first inner-kids))))
           ;; Alt 2: [FINALLY, COLON, block] — block is third child
           (let ([bk (third inner-kids)])
             (if (and (cst-node? bk) (eq? (any-tree-tag bk) 'block))
                 (lower-block-or-stmt bk tk-type tk-value)
                 #f))]
          [else #f]))
  (uir-try try-body catches else-body finally-body))

;; ── With statement ─────────────────────────────────────────────────

(define (lower-with cst tk-type tk-value)
  ;; with_stmt: [with_item*, block]
  ;; All children except the last block are with_items
  (define kids (node-children cst))
  (define block-node (findf (lambda (k) (eq? (any-tree-tag k) 'block)) kids))
  (define body (if block-node (lower-block-or-stmt block-node tk-type tk-value) (uir-null)))
  ;; Extract with_items (all CST nodes that aren't the block)
  (define item-nodes (filter (lambda (k) (and (cst-node? k) (not (eq? (any-tree-tag k) 'block)))) kids))
  (define items
    (for/list ([item item-nodes])
      (define ikids (node-children item))
      ;; with_item: [test, (group for as)]
      (define ctx-expr (if (>= (length ikids) 1)
                           (lower-expr (first ikids) tk-type tk-value)
                           (uir-null)))
      (define as-name
        (if (>= (length ikids) 2)
            (let* ([as-group (second ikids)]
                   [akids (node-children as-group)]
                   [as-expr (if (>= (length akids) 1) (first akids) #f)])
              (if as-expr (lower-expr as-expr tk-type tk-value) #f))
            #f))
      (list ctx-expr as-name)))
  (uir-with items body))

(define (lower-funcdef cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define name (for/or ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (define kids (node-children cst))
  ;; funcdef: 'def' NAME parameters ('->' test)? ':' suite
  ;; But node-children gives: name, parameters, (optional group with '->' test), suite
  (define params-node (findf (λ (k) (eq? (any-tree-tag k) 'parameters)) kids))
  (define suite-node (findf (λ (k) (eq? (any-tree-tag k) 'suite)) kids))
  ;; return type annotation: group node containing ARROW token + test
  (define return-type-node
    (for/or ([k kids])
      (and (eq? (any-tree-tag k) 'group)
           (let ([gk (node-children k)])
             (and (pair? gk) (eq? (any-tree-tag (first gk)) 'test)
                  ;; Check for ARROW token in raw children
                  (for/or ([rc (any-tree-children k)])
                    (and (not (cst-node? rc)) (not (null? rc)) (not (eq? rc 'none))
                         (with-handlers ([exn:fail? (lambda (_) #f)])
                           (eq? (tk-type rc) 'ARROW))))
                  (first gk))))))
  (define params (if params-node (lower-parameters params-node tk-type tk-value) '()))
  (define body (if suite-node (lower-suite suite-node tk-type tk-value) (uir-null)))
  (define return-type (if return-type-node
                          (lower-expr return-type-node tk-type tk-value)
                          #f))
  (uir-fn (if name (uir-symbol name) #f) params body return-type))

(define (lower-parameters cst tk-type tk-value)
  ;; parameters: '(' typedargslist? ')'
  ;; typedargslist: tfpdef (',' tfpdef ('=' test)?)* ','?
  ;; Default values appear as siblings of tfpdef in comma-separated groups
  (define result '())
  (define pending-default #f)  ;; default waiting for next tfpdef
  (let walk ([node cst])
    (cond [(cst-node? node)
           (cond [(eq? (any-tree-tag node) 'tfpdef)
                  (define param (lower-tfpdef node tk-type tk-value))
                  (when pending-default
                    (set! param (uir-typed-param (uir-typed-param-name param)
                                                 (uir-typed-param-type param)
                                                 pending-default))
                    (set! pending-default #f))
                  (set! result (cons param result))]
                 [else
                  ;; Check for default group (sibling of tfpdef): group with ASSIGN+test
                  (define raw-kids (any-tree-children node))
                  (for ([c raw-kids])
                    (cond [(cst-node? c) (walk c)]
                          [(and (list? c) (pair? c)) (for-each walk c)]
                          [else (void)]))
                  ;; Also check if THIS node is a default group
                  (when (and (eq? (any-tree-tag node) 'group)
                             (pair? result))
                    (define gk (node-children node))
                    (when (and (pair? gk) (eq? (any-tree-tag (first gk)) 'test)
                               (for/or ([rc (any-tree-children node)])
                                 (and (not (cst-node? rc)) (not (null? rc)) (not (eq? rc 'none))
                                      (with-handlers ([exn:fail? (lambda (_) #f)])
                                        (eq? (tk-type rc) 'ASSIGN)))))
                      (set! pending-default (lower-expr (first gk) tk-type tk-value))))])]
          [(and (list? node) (pair? node)) (for-each walk node)]
          [else (void)]))
  ;; After walk, apply any pending default to the last param
  (define final-result
    (if pending-default
        (let ([last (car result)])
          (cons (uir-typed-param (uir-typed-param-name last)
                                 (uir-typed-param-type last)
                                 pending-default)
                (cdr result)))
        result))
  (reverse final-result))

(define (lower-tfpdef cst tk-type tk-value)
  ;; tfpdef: name (':' test)? ('=' test)?
  ;; Or vfpdef (lambda params): name (':' test)? ('=' test)?
  (define raw-kids (any-tree-children cst))
  ;; Find name: first NAME token or name node child
  (define name
    (for/or ([c raw-kids])
      (cond [(and (cst-node? c) (eq? (any-tree-tag c) 'name))
             (let ([nt (cst-tokens c tk-type tk-value)])
               (if (pair? nt) (second (car nt)) #f))]
            [(and (not (cst-node? c)) (not (null? c)) (not (eq? c 'none))
                  (with-handlers ([exn:fail? (lambda (_) #f)])
                    (eq? (tk-type c) 'NAME)))
             (tk-value c)]
            [else #f])))
  ;; Find type annotation: group containing COLON + test
  (define type-expr
    (for/or ([c raw-kids])
      (and (cst-node? c) (eq? (any-tree-tag c) 'group)
           (let ([gk (node-children c)])
             (and (pair? gk) (eq? (any-tree-tag (first gk)) 'test)
                  ;; Check for COLON token
                  (for/or ([rc (any-tree-children c)])
                    (and (not (cst-node? rc)) (not (null? rc)) (not (eq? rc 'none))
                         (with-handlers ([exn:fail? (lambda (_) #f)])
                           (eq? (tk-type rc) 'COLON))))
                  (lower-expr (first gk) tk-type tk-value))))))
  ;; Find default: group containing ASSIGN + test
  (define default-expr
    (for/or ([c raw-kids])
      (and (cst-node? c) (eq? (any-tree-tag c) 'group)
           (let ([gk (node-children c)])
             (and (pair? gk) (eq? (any-tree-tag (first gk)) 'test)
                  ;; Check for ASSIGN token
                  (for/or ([rc (any-tree-children c)])
                    (and (not (cst-node? rc)) (not (null? rc)) (not (eq? rc 'none))
                         (with-handlers ([exn:fail? (lambda (_) #f)])
                           (eq? (tk-type rc) 'ASSIGN))))
                  (lower-expr (first gk) tk-type tk-value))))))
  ;; Also detect default in the 3rd child of vfpdef (list-wrapped group)
  (define default-from-list
    (for/or ([c raw-kids])
      (and (list? c) (pair? c)
           (for/or ([cc c])
             (and (cst-node? cc) (eq? (any-tree-tag cc) 'group)
                  (let ([gk (node-children cc)])
                    (and (pair? gk) (eq? (any-tree-tag (first gk)) 'test)
                         (for/or ([rc (any-tree-children cc)])
                           (and (not (cst-node? rc)) (not (null? rc)) (not (eq? rc 'none))
                                (with-handlers ([exn:fail? (lambda (_) #f)])
                                  (eq? (tk-type rc) 'ASSIGN))))
                         (lower-expr (first gk) tk-type tk-value))))))))
  (if (or type-expr (or default-expr default-from-list))
      (uir-typed-param (uir-symbol (or name "?"))
                       (or type-expr #f)
                       (or default-expr default-from-list #f))
      (uir-symbol (or name "?"))))

;; Lower lambdef into uir-fn (anonymous function)
(define (lower-suite cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (lower-simple-stmts (first kids) tk-type tk-value)))

(define (lower-classdef cst tk-type tk-value)
  ;; classdef: 'class' name ('(' arglist? ')')? ':' block
  ;; block: NEWLINE INDENT stmt+ DEDENT
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define name
    (for/or ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (define kids (node-children cst))
  (define suite-node (or (findf (λ (k) (eq? (any-tree-tag k) 'suite)) kids)
                         (findf (λ (k) (eq? (any-tree-tag k) 'block)) kids)))
  ;; Extract superclass from arglist: group node with '(' arglist? ')'
  (define super-node
    (for/or ([k kids])
      (and (eq? (any-tree-tag k) 'group)
           (let ([gk (node-children k)])
             (and (pair? gk) (eq? (any-tree-tag (first gk)) 'arglist)
                  ;; Get first test from arglist → argument → test
                  (let* ([al (first gk)]
                         [al-kids (node-children al)])
                    (and (pair? al-kids)
                         (let ([arg (first al-kids)])
                           (and (eq? (any-tree-tag arg) 'argument)
                                (let ([arg-kids (node-children arg)])
                                  (and (pair? arg-kids)
                                       (let ([test-node (first arg-kids)])
                                         (eq? (any-tree-tag test-node) 'test)
                                         test-node))))))))))))
  (define super (if super-node (lower-expr super-node tk-type tk-value) (uir-null)))
  ;; Walk the suite/block to extract fields (assignments) and methods (funcdef)
  (define fields '())
  (define methods '())
  (define (walk stmt)
    (define tag (any-tree-tag stmt))
    (cond [(eq? tag 'simple_stmts)
           ;; Contains simple_stmt → group → expr_stmt or pass_stmt
           (let ([ss-kids (node-children stmt)])
             (when (pair? ss-kids)
               (define inner (first ss-kids)) ;; simple_stmt
               (when (and (cst-node? inner) (eq? (any-tree-tag inner) 'simple_stmt))
                 (define sg (node-children inner))
                 (when (pair? sg)
                   (define g (first sg)) ;; group
                   (when (and (cst-node? g) (eq? (any-tree-tag g) 'group))
                     (define gk (node-children g))
                     (when (pair? gk)
                       (match (any-tree-tag (first gk))
                         ['expr_stmt
                          ;; Extract field name from first test
                          (define xk (node-children (first gk)))
                          (when (and (pair? xk) (>= (length xk) 2))
                            (define lhs (first xk))
                            (define rhs (second xk))
                            ;; Get field name as string
                            (define fname
                              (let ([lhs-toks (cst-tokens-deep lhs tk-type tk-value)])
                                (for/or ([t lhs-toks] #:when (eq? (first t) 'NAME)) (second t))))
                            (cond [(and (cst-node? rhs) (eq? (any-tree-tag rhs) 'group)
                                        (let ([rgk (node-children rhs)])
                                          (and (pair? rgk)
                                               (eq? (any-tree-tag (first rgk)) 'annassign))))
                                   ;; Annotated field: x: int = 5
                                   (let* ([ann (first (node-children rhs))]
                                          [ann-kids (node-children ann)]
                                          [type-node (and (pair? ann-kids) (first ann-kids))]
                                          [type-expr (if type-node (lower-expr type-node tk-type tk-value) (uir-null))]
                                          [init-node (and (>= (length ann-kids) 2)
                                                          (let ([ig (second ann-kids)])
                                                            (and (cst-node? ig)
                                                                 (let ([igk (node-children ig)])
                                                                   (and (pair? igk) (first igk))))))]
                                          [init (if init-node (lower-expr init-node tk-type tk-value) (uir-null))])
                                      (set! fields (cons (uir-field (uir-symbol (or fname "?")) type-expr init) fields)))]
                                   [else
                                    ;; Plain field: x = 1
                                    (define rhs-value (find-rhs-in-children (list rhs)))
                                    (set! fields (cons (uir-field (uir-symbol (or fname "?")) (uir-null)
                                                                  (if rhs-value
                                                                      (lower-expr rhs-value tk-type tk-value)
                                                                      (uir-null))) fields))]))]
                         ['pass_stmt (void)]
                         [_ (void)])))))))]
          [(eq? tag 'compound_stmt)
           ;; Contains funcdef or classdef
           (define cc (node-children stmt))
           (when (pair? cc)
             (match (any-tree-tag (first cc))
               ['funcdef
                (set! methods (cons
                               (lower-method (first cc) tk-type tk-value)
                               methods))]
               ['classdef (void)]
               [_ (void)]))]
          [(eq? tag 'stmt)
           (for ([c (node-children stmt)]) (when (cst-node? c) (walk c)))]
          [else (void)]))
  (when suite-node
    (for ([k (any-tree-children suite-node)])
      (cond [(cst-node? k) (walk k)]
            [(and (list? k) (pair? k)) (for-each (lambda (cc) (when (cst-node? cc) (walk cc))) k)]
            [else (void)])))
  (uir-class (uir-symbol (or name "?class")) super
             (reverse fields) (reverse methods)))

(define (lower-method cst tk-type tk-value)
  ;; funcdef inside a class becomes a method
  (define fn (lower-funcdef cst tk-type tk-value))
  (uir-method (uir-fn-name fn) (uir-fn-params fn) (uir-fn-body fn) 'public))

(define (lower-block-or-stmt cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (cond
    [(eq? tag 'suite) (lower-suite cst tk-type tk-value)]
    [(eq? tag 'block) (lower-block cst tk-type tk-value)]
    [(eq? tag 'simple_stmts) (lower-simple-stmts cst tk-type tk-value)]
    [(eq? tag 'simple_stmt) (lower-simple-stmt cst tk-type tk-value)]
    [(eq? tag 'compound_stmt) (lower-compound cst tk-type tk-value)]
    [else (lower-stmt cst tk-type tk-value)]))

(define (lower-block cst tk-type tk-value)
  ;; block: NEWLINE INDENT stmt+ DEDENT
  ;; stmt+ is list-wrapped, so we must look inside list children
  (define all-raw (any-tree-children cst))
  (define stmt-kids
    (apply append
           (for/list ([c all-raw])
             (cond [(cst-node? c) (list c)]
                   [(list? c) (filter cst-node? c)]
                   [else '()]))))
  (if (or (null? stmt-kids) (>= (length stmt-kids) 2))
      (uir-block (map (λ (k) (lower-block-or-stmt k tk-type tk-value)) stmt-kids))
      (lower-block-or-stmt (first stmt-kids) tk-type tk-value)))
