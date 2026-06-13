#lang racket

(require "tree.rkt"
         "uir.rkt")

(provide lower-python)

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
            [(cst-node? c) (walk c)])))
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

;; Lower a sequence expression into UIR
(define (lower-expr cst tk-type tk-value)
  (define u (unwrap-expr cst))
  (define tag (any-tree-tag u))
  (define kids (node-children u))
  (cond
    [(eq? tag 'test)
     ;; Could be ternary: test 'if' test 'else' test
     (if (= (length kids) 2)
         (let* ([then-expr (lower-expr (first kids) tk-type tk-value)]
                [if-group (second kids)]
                [gk (node-children if-group)]
                [cond-expr (if (>= (length gk) 1) (lower-expr (first gk) tk-type tk-value) (uir-null))]
                [else-expr (if (>= (length gk) 2) (lower-expr (second gk) tk-type tk-value) (uir-null))])
           (uir-if cond-expr then-expr else-expr))
         ;; Non-ternary test — unwrap
         (if (and (= (length kids) 1) (member (any-tree-tag (first kids)) '(test or_test and_test not_test comparison expr atom_expr)))
             (lower-expr (first kids) tk-type tk-value)
             (uir-symbol "?test")))]
    [(eq? tag 'comparison) (lower-comparison u tk-type tk-value)]
    [(eq? tag 'expr) (lower-binop u tk-type tk-value)]
    [(eq? tag 'or_test) (lower-binop u tk-type tk-value)]    ;; 'or' operator
    [(eq? tag 'and_test) (lower-binop u tk-type tk-value)]   ;; 'and' operator
    [(eq? tag 'not_test) (lower-not-test u tk-type tk-value)] ;; 'not' operator
    [(eq? tag 'atom_expr) (lower-atom-expr u tk-type tk-value)]
    [(eq? tag 'atom) (lower-atom u tk-type tk-value)]
    [(eq? tag 'name) (lower-name u tk-type tk-value)]
    [(eq? tag 'number) (uir-number (first (cst-tokens u tk-type tk-value)))]
    [(eq? tag 'lambdef) (lower-lambdef u tk-type tk-value)]
    [(eq? tag 'group) (if (null? kids) (uir-null) (lower-expr (first kids) tk-type tk-value))]
    [(eq? tag 'dotted_name) (lower-dotted-name u tk-type tk-value)]
    [else (uir-symbol (format "?~a" tag))]))

;; Lower a dotted_name (a.b.c) into a symbol or attribute chain
(define (lower-dotted-name cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define names (for/list ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (if (null? names)
      (uir-symbol "?dotted")
      (uir-var (uir-symbol (car names)))))

;; Handle 'not' operator: not_test → 'not' not_test | comparison
(define (lower-not-test cst tk-type tk-value)
  ;; Check for NOT token in raw children
  (define has-not?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c)) (not (null? c)) (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'NOT)))))
  (define kids (node-children cst))
  (if (or (not has-not?) (= (length kids) 0))
      ;; No 'not' prefix — pass through
      (if (= (length kids) 1)
          (lower-expr (first kids) tk-type tk-value)
          (uir-symbol "?not_test"))
      ;; Has 'not' prefix: lower the inner expression and wrap
      (uir-call (uir-symbol "not")
                (list (if (pair? kids)
                         (lower-expr (first kids) tk-type tk-value)
                         (uir-null))))))

;; Handle comparison: expr (comparison_op expr)*
(define (lower-comparison cst tk-type tk-value)
  ;; Extract op-right pairs from both direct nodes and list-wrapped pairs
  (define pairs
    (let loop ([remaining (cdr (any-tree-children cst))])
      (if (null? remaining)
          '()
          (let ([c (car remaining)])
            (cond [(cst-node? c)
                   ;; Direct node - expect pair: op then right
                   (let ([rest (cdr remaining)])
                     (if (and (pair? rest) (cst-node? (car rest)))
                         (cons (list c (car rest)) (loop (cdr rest)))
                         (loop (cdr remaining))))]
                  [(and (list? c) (pair? c))
                   ;; List-wrapped: could be (group) with [token, expr] or [comp_op, expr]
                   (cond [(and (null? (cdr c)) (cst-node? (car c))
                               (eq? (any-tree-tag (car c)) 'group))
                          ;; (group) wrapping something — check all children
                          (let* ([g (car c)]
                                 [raw (any-tree-children g)]
                                 [gk (node-children g)])
                            (cond [(and (>= (length gk) 2) (cst-node? (first gk)))
                                   ;; [comp_op, expr] pair
                                   (cons (list (first gk) (second gk))
                                         (loop (cdr remaining)))]
                                  [(= (length gk) 1)
                                   ;; single CST-node child (the right operand)
                                   ;; The operator is a raw token in raw children
                                   (let ([right (first gk)])
                                     (cons (list g right)  ;; use group as op-node (has the token)
                                           (loop (cdr remaining))))]
                                  [else (loop (cdr remaining))]))]
                         [(and (pair? (cdr c)) (cst-node? (car c)) (cst-node? (cadr c)))
                          ;; (group, expr) pair
                          (cons (list (car c) (cadr c)) (loop (cdr remaining)))]
                         [else (loop (cdr remaining))])]
                  [else (loop (cdr remaining))])))))
  (define kids (node-children cst))
  (if (null? pairs)
      (if (= (length kids) 1)
          (lower-expr (first kids) tk-type tk-value)
          (uir-symbol "?comparison"))
      (let fold ([pairs pairs]
                 [acc (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))])
        (if (null? pairs)
            acc
            (let* ([p (car pairs)]
                   [op-node (car p)]
                   [right (cadr p)]
                   [op-tok (cst-tokens op-node tk-type tk-value)])
              (fold (cdr pairs)
                    (uir-call (uir-symbol (if (pair? op-tok) (second (car op-tok)) "?op"))
                              (list acc (lower-expr right tk-type tk-value)))))))))

;; Handle binary expressions: expr (binop expr)*
(define (lower-binop cst tk-type tk-value)
  ;; Extract op-right pairs from both direct nodes and list-wrapped pairs
  (define pairs
    (let loop ([remaining (cdr (any-tree-children cst))])
      (if (null? remaining)
          '()
          (let ([c (car remaining)])
            (cond [(cst-node? c)
                   (let ([rest (cdr remaining)])
                     (if (and (pair? rest) (cst-node? (car rest)))
                         (cons (list c (car rest)) (loop (cdr rest)))
                         (loop (cdr remaining))))]
                  [(and (list? c) (pair? c))
                   ;; List-wrapped: could be (group) with [token, expr] or [comp_op, expr]
                   (cond [(and (null? (cdr c)) (cst-node? (car c))
                               (eq? (any-tree-tag (car c)) 'group))
                          (let* ([g (car c)]
                                 [raw (any-tree-children g)]
                                 [gk (node-children g)])
                            (cond [(and (>= (length gk) 2) (cst-node? (first gk)))
                                   (cons (list (first gk) (second gk))
                                         (loop (cdr remaining)))]
                                  [(= (length gk) 1)
                                   (let ([right (first gk)])
                                     (cons (list g right)
                                           (loop (cdr remaining))))]
                                  [else (loop (cdr remaining))]))]
                         [(and (pair? (cdr c)) (cst-node? (car c)) (cst-node? (cadr c)))
                          (cons (list (car c) (cadr c)) (loop (cdr remaining)))]
                         [else (loop (cdr remaining))])]
                  [else (loop (cdr remaining))])))))
  (define kids (node-children cst))
  (if (null? pairs)
      (if (= (length kids) 1)
          (lower-expr (first kids) tk-type tk-value)
          (uir-symbol "?expr"))
      (let fold ([pairs pairs]
                 [acc (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))])
        (if (null? pairs)
            acc
            (let* ([p (car pairs)]
                   [op-node (car p)]
                   [right (cadr p)]
                   [op-tok (cst-tokens op-node tk-type tk-value)])
              (fold (cdr pairs)
                    (uir-call (uir-symbol (if (pair? op-tok) (second (car op-tok)) "?op"))
                              (list acc (lower-expr right tk-type tk-value)))))))))

;; Handle atom_expr: trailer (attr access, call, subscript)
(define (lower-atom-expr cst tk-type tk-value)
  ;; Check for AWAIT prefix in raw children (non-CST-node token)
  (define has-await?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'AWAIT)))))
  (define kids (node-children cst))
  ;; Also extract list-wrapped trailers from raw children
  (define list-trailers
    (apply append
           (for/list ([c (any-tree-children cst)]
                      #:when (and (list? c) (pair? c)))
             (for/list ([item c]
                        #:when (and (cst-node? item)
                                    (eq? (any-tree-tag item) 'trailer)))
               item))))
  (define all-trailers (append (if (>= (length kids) 2) (cdr kids) '()) list-trailers))
  (define result
    (if (and (= (length kids) 1) (null? list-trailers))
        (lower-expr (first kids) tk-type tk-value)
        ;; base then trailers
        (let loop ([remaining all-trailers]
                   [acc (if (pair? kids) (lower-expr (first kids) tk-type tk-value) (uir-null))])
          (if (null? remaining)
              acc
              (let* ([trailer (first remaining)]
                     [next (lower-trailer trailer acc tk-type tk-value)])
                (loop (cdr remaining) next))))))
  (if has-await? (uir-await result) result))

(define (lower-trailer cst base tk-type tk-value)
  ;; trailer can be: '(' arglist? ')' call, '[' subscriptlist ']' index, '.' NAME attr
  (define kids (node-children cst))
  ;; Check for DOT token
  (define has-dot?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'DOT)))))
  ;; Check for OPEN_BRACK token
  (define has-brack?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'OPEN_BRACK)))))
  ;; Check for OPEN_PAREN token (call)
  (define has-open-paren?
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c))
           (not (null? c))
           (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (eq? (tk-type c) 'OPEN_PAREN)))))
  (cond
    [has-dot?
     ;; Attribute access: base.attr
     (let ([name-node (find-node kids 'name)])
       (if name-node
           (let ([name-str (first-token name-node tk-type tk-value)])
             (uir-get base (uir-string (if name-str (cdr name-str) "?name"))))
           (uir-get base (uir-string "?attr"))))]
    [has-brack?
     ;; Subscript: base[key]
     (let ([sub-node (find-node kids 'subscriptlist)])
       (if sub-node
           (uir-get base (lower-subscriptlist sub-node tk-type tk-value))
           (uir-get base (uir-null))))]
    [(null? kids)
     (if has-open-paren?
         (uir-call base '())
         base)]
    [(eq? (any-tree-tag (first kids)) 'arglist)
     (uir-call base (lower-arglist (first kids) tk-type tk-value))]
    [else base]))

;; Find a node with given tag in a list
(define (find-node kids tag)
  (for/or ([k kids]
           #:when (eq? (any-tree-tag k) tag))
    k))

;; Lower subscriptlist into a UIR expression
;; For single subscript: return the expression
;; For multiple subscripts: return as uir-list (tuple)
(define (lower-subscriptlist cst tk-type tk-value)
  (define sub-exprs
    (map (lambda (sub)
           (define sub-kids (node-children sub))
           (if (null? sub-kids)
               (uir-null)
               (lower-expr (first sub-kids) tk-type tk-value)))
         (find-subscripts cst)))
  (if (null? sub-exprs)
      (uir-null)
      (if (= (length sub-exprs) 1)
          (first sub-exprs)
          (uir-list sub-exprs))))

;; Recursively find all subscript_ nodes (handles list-wrapping from commas)
(define (find-subscripts node)
  (cond [(and (cst-node? node) (eq? (any-tree-tag node) 'subscript_))
         (list node)]
        [(cst-node? node)
         (append-map find-subscripts (any-tree-children node))]
        [(and (list? node) (pair? node))
         (append-map find-subscripts node)]
        [else '()]))

(define (lower-arglist cst tk-type tk-value)
  (define kids (node-children cst))
  (filter-map (λ (k)
                (let ([t (any-tree-tag k)])
                  (cond [(eq? t 'argument) (lower-argument k tk-type tk-value)]
                        [(eq? t 'group) (lower-group-expr k tk-type tk-value)]
                        [else #f])))
              kids))

(define (lower-argument cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (lower-expr (first kids) tk-type tk-value)))

(define (lower-group-expr cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (lower-expr (first kids) tk-type tk-value)))

;; Handle atom: NAME | NUMBER | STRING+ | NONE | TRUE | FALSE | [] | {} | ()
(define (lower-atom cst tk-type tk-value)
  (define toks (cst-tokens cst tk-type tk-value))
  ;; Also check for tokens wrapped in single-element lists (e.g., STRING+ produces (list STRING))
  (define list-toks
    (for/list ([c (any-tree-children cst)]
               #:when (and (list? c) (pair? c) (null? (cdr c))
                           (not (cst-node? (car c)))
                           (with-handlers ([exn:fail? (lambda (_) #f)])
                             (tk-type (car c)) #t)))
      (list (tk-type (car c)) (tk-value (car c)))))
  (define all-toks (append toks list-toks))
  (define kids (node-children cst))
  ;; Detect bracket-delimited collections via raw children
  (define bracket-type
    (for/or ([c (any-tree-children cst)])
      (and (not (cst-node? c)) (not (null? c)) (not (eq? c 'none))
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (match (tk-type c)
               ['OPEN_BRACK 'list]
               ['OPEN_BRACE 'brace]
               ['OPEN_PAREN 'tuple]
               [_ #f])))))
  (cond
    ;; List literal: [expr, ...] — check for comprehension first
    [(and (eq? bracket-type 'list) (pair? kids)
          (eq? (any-tree-tag (first kids)) 'testlist_comp))
     (if (has-comp-for? (first kids))
         (lower-comprehension (first kids) tk-type tk-value "list-comp")
         (uir-list (lower-collection-items (first kids) tk-type tk-value)))]
    [(eq? bracket-type 'list) (uir-list '())]
    ;; Dict or set: {expr, ...} — check for comprehension first
    [(and (eq? bracket-type 'brace) (pair? kids)
          (eq? (any-tree-tag (first kids)) 'dictorsetmaker))
     (if (has-comp-for? (first kids))
         (let* ([dsm (first kids)]
                [has-colon? (for/or ([t (walk-collection dsm tk-type)] #:when (eq? (first t) 'colon)) #t)])
           (lower-comprehension dsm tk-type tk-value
                               (if has-colon? "dict-comp" "set-comp")))
         (lower-dictorsetmaker (first kids) tk-type tk-value))]
    [(eq? bracket-type 'brace) (uir-record '())]
    ;; Tuple: (expr, ...) — find test/expr items
     [(eq? bracket-type 'tuple)
      (uir-call (uir-symbol "tuple") (lower-tuple-items cst tk-type tk-value))]
    [(and (null? all-toks) (null? kids)) (uir-null)]
    [(pair? kids)
     (let ([inner (first kids)])
       (match (any-tree-tag inner)
         ['name (lower-name inner tk-type tk-value)]
         ['NONE (uir-null)]
         ['TRUE (uir-bool #t)]
         ['FALSE (uir-bool #f)]
         [_ (uir-symbol (format "?atom-~a" (any-tree-tag inner)))]))]
    [(pair? all-toks)
     (match (first (car all-toks))
       ['NUMBER (uir-number (second (car all-toks)))]
       ['FLOAT_NUMBER (uir-number (second (car all-toks)))]
        ['STRING (lower-string-token (second (car all-toks)))]
       ['TRUE (uir-bool #t)]
       ['FALSE (uir-bool #f)]
       ['NONE (uir-null)]
       [_ (uir-symbol (format "?atom-~a" (first (car all-toks))))])]
    [else (uir-null)]))

(define (lower-name cst tk-type tk-value)
  (define toks (cst-tokens cst tk-type tk-value))
  (if (pair? toks)
      (uir-var (uir-symbol (second (car toks))))
      (uir-symbol "?name")))

;; Strip surrounding quotes from a STRING token value
(define (unquote-string s)
  (if (and (>= (string-length s) 2)
           (or (eq? (string-ref s 0) #\") (eq? (string-ref s 0) #\'))
           (eq? (string-ref s 0) (string-ref s (sub1 (string-length s)))))
      (substring s 1 (sub1 (string-length s)))
      s))

;; Lower a STRING token value, detecting f-strings
(define (lower-string-token s)
  (cond [(and (>= (string-length s) 3)
              (or (eq? (string-ref s 0) #\f) (eq? (string-ref s 0) #\F))
              (or (eq? (string-ref s 1) #\") (eq? (string-ref s 1) #\'))
              (eq? (string-ref s 1) (string-ref s (sub1 (string-length s)))))
         ;; f-string: strip 'f' prefix and quotes
         (uir-fstring (substring s 2 (sub1 (string-length s))))]
        [else
         (uir-string (unquote-string s))]))

;; Unified walker: recursively walk a CST node, collecting all nodes of interest
;; Returns list of '(test node) and '(colon) markers in tree order
(define (walk-collection node tk-type)
  (if (any-tree? node)
      (if (eq? (any-tree-tag node) 'test)
          (list (list 'test node))
          (append-map (lambda (c)
                        (cond [(cst-node? c) (walk-collection c tk-type)]
                              [(and (list? c) (pair? c))
                               (append-map (lambda (cc) (walk-collection cc tk-type)) c)]
                              [(and (not (null? c)) (not (eq? c 'none))
                                    (with-handlers ([exn:fail? (lambda (_) #f)])
                                      (eq? (tk-type c) 'COLON)))
                               (list (list 'colon))]
                              [else '()]))
                      (any-tree-children node)))
      (if (and (not (null? node)) (not (eq? node 'none))
               (with-handlers ([exn:fail? (lambda (_) #f)])
                 (eq? (tk-type node) 'COLON)))
          (list (list 'colon))
          '())))

;; Recursively find all test nodes and lower them (for list/tuple items)
(define (lower-collection-items cst tk-type tk-value)
  (for/list ([t (walk-collection cst tk-type)] #:when (eq? (first t) 'test))
    (lower-expr (second t) tk-type tk-value)))

;; Lower dictorsetmaker: detect dict vs set via COLON, extract pairs or items
(define (lower-dictorsetmaker cst tk-type tk-value)
  (define tests-and-colons (walk-collection cst tk-type))
  (define has-colon? (for/or ([t tests-and-colons] #:when (eq? (first t) 'colon)) #t))
  (if has-colon?
      (let* ([tests (filter (lambda (t) (eq? (first t) 'test)) tests-and-colons)]
             [entries
              (let loop ([ts tests])
                (if (or (null? ts) (null? (cdr ts)))
                    '()
                    (let* ([key-expr (lower-expr (second (car ts)) tk-type tk-value)]
                           [val-expr (lower-expr (second (cadr ts)) tk-type tk-value)]
                           [key-str
                            (cond [(uir-string? key-expr) (uir-string-value key-expr)]
                                  [(uir-number? key-expr) (uir-number-value key-expr)]
                                  [(uir-symbol? key-expr) (uir-symbol-name key-expr)]
                                  [(uir-var? key-expr) (uir-symbol-name (uir-var-name key-expr))]
                                  [else (format "~a" (uir-tag key-expr))])])
                      (cons (cons (uir-string key-str) val-expr)
                            (loop (cddr ts))))))])
        (uir-record entries))
      ;; Set: just collect test items
      (uir-call (uir-symbol "set")
       (for/list ([t tests-and-colons] #:when (eq? (first t) 'test))
         (lower-expr (second t) tk-type tk-value)))))

;; Lower tuple items: extract test/yield_expr nodes from a tuple atom
(define (lower-tuple-items cst tk-type tk-value)
  ;; The atom contains a group wrapping the contents
  ;; Walk the tree and collect all test nodes
  (lower-collection-items cst tk-type tk-value))

;; ── Comprehensions ──────────────────────────────────────────────────

;; Check if a CST node contains a comp_for
(define (has-comp-for? node)
  (cond [(any-tree? node)
         (or (eq? (any-tree-tag node) 'comp_for)
             (for/or ([c (any-tree-children node)])
               (cond [(cst-node? c) (has-comp-for? c)]
                     [(and (list? c) (pair? c))
                      (for/or ([cc c]) (has-comp-for? cc))]
                     [else #f])))]
        [else #f]))

;; Lower a comp_for into (list loop-var iterable filter-expr)
(define (lower-comp-for cst tk-type tk-value)
  (define raw-kids (any-tree-children cst))
  (define var-names
    (for*/list ([c raw-kids] #:when (cst-node? c)
                #:when (eq? (any-tree-tag c) 'exprlist)
                [t (cst-tokens-deep c tk-type tk-value)]
                #:when (eq? (first t) 'NAME))
      (second t)))
  (define loop-var (if (pair? var-names) (uir-symbol (car var-names)) (uir-symbol "?var")))
  (define iterable
    (for/or ([c raw-kids])
      (and (cst-node? c) (eq? (any-tree-tag c) 'or_test)
           (lower-expr c tk-type tk-value))))
  (define comp-iter
    (for/or ([c raw-kids])
      (and (cst-node? c) (eq? (any-tree-tag c) 'comp_iter) c)))
  (define filter-expr
    (if comp-iter
        (let ([ci-kids (node-children comp-iter)])
          (if (and (pair? ci-kids) (eq? (any-tree-tag (first ci-kids)) 'comp_if))
              (let* ([comp-if (first ci-kids)]
                     [cif-kids (node-children comp-if)]
                     [test-node (if (pair? cif-kids) (first cif-kids) #f)])
                (if test-node (lower-expr test-node tk-type tk-value) (uir-null)))
              (uir-null)))
        (uir-null)))
  (list loop-var (or iterable (uir-null)) filter-expr))

;; Lower a comprehension to UIR: (uir-call (uir-symbol "TYPE-comp") (list result loop-var iterable filter))
(define (lower-comprehension cst tk-type tk-value constructor)
  (define comp-for-node
    (let find ([node cst])
      (cond [(any-tree? node)
             (if (eq? (any-tree-tag node) 'comp_for)
                 node
                 (for/or ([c (any-tree-children node)])
                   (cond [(cst-node? c) (find c)]
                         [(and (list? c) (pair? c))
                          (for/or ([cc c]) (find cc))]
                         [else #f])))]
            [else #f])))
  ;; Extract the result expression from the first test sibling
  (define result-expr
    (let* ([ts (walk-collection cst tk-type)]
           [first-test (for/or ([t ts] #:when (eq? (first t) 'test)) (second t))])
      (if first-test (lower-expr first-test tk-type tk-value) (uir-null))))
  (define comp-parts (lower-comp-for comp-for-node tk-type tk-value))
  (uir-call (uir-symbol constructor)
            (list result-expr (car comp-parts) (cadr comp-parts) (caddr comp-parts))))

;; ── Statement lowering ─────────────────────────────────────────────

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
         (define augassign-node
           (for/or ([k (cdr kids)])
             (and (eq? (any-tree-tag k) (quote group))
                  (let ([gk (node-children k)])
                    (and (pair? gk) (eq? (any-tree-tag (first gk)) (quote augassign))
                         (first gk))))))
         (if augassign-node
             (let* ([lhs (first kids)]
                    [group-node (for/or ([k (cdr kids)]) (and (eq? (any-tree-tag k) (quote group)) k))]
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
                   (lower-expr lhs tk-type tk-value)))))]))

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
(define (lower-exprlist cst tk-type tk-value)
  (define exprs '())
  (let walk ([node cst])
    (cond [(and (cst-node? node) (eq? (any-tree-tag node) 'expr))
           (set! exprs (cons (lower-expr node tk-type tk-value) exprs))]
          [(cst-node? node)
           (for-each walk (any-tree-children node))]
          [(and (list? node) (pair? node))
           (for-each walk node)]))
  (reverse exprs))

;; ── Top-level entry ────────────────────────────────────────────────

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
     (define kids (node-children cst))
     (if (null? kids)
         (uir-null)
         ;; file_input: (NEWLINE | stmt)*
         (uir-block (filter-map (λ (k)
                                 (define t (any-tree-tag k))
                                 (if (eq? t 'stmt)
                                     (lower-stmt k tk-type tk-value)
                                     #f))
                               kids)))]
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
           [_ (uir-symbol (format "?comp-~a" (any-tree-tag first-kid)))]))))

(define (lower-if cst tk-type tk-value)
  (define kids (node-children cst))
  (define test-expr (lower-expr (first kids) tk-type tk-value))
  (define then-body (lower-block-or-stmt (second kids) tk-type tk-value))
  (define else-body
    (if (>= (length kids) 3)
        (let ([elif-or-else (third kids)])
          (if (eq? (any-tree-tag elif-or-else) 'else_block)
              (lower-block-or-stmt elif-or-else tk-type tk-value)
              (lower-if elif-or-else tk-type tk-value)))
        (uir-null)))
  (uir-if test-expr then-body else-body))

(define (lower-while cst tk-type tk-value)
  (define kids (node-children cst))
  (define test-expr (lower-expr (first kids) tk-type tk-value))
  (define body (lower-block-or-stmt (second kids) tk-type tk-value))
  (uir-if test-expr body (uir-null)))

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

;; ── Try/except/finally ────────────────────────────────────────────

;; Helper: lower an except_clause into (list exception-type exception-name body)
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
  ;; funcdef: 'def' NAME parameters ':' suite
  (define params-node (findf (λ (k) (eq? (any-tree-tag k) 'parameters)) kids))
  (define suite-node (findf (λ (k) (eq? (any-tree-tag k) 'suite)) kids))
  (define params (if params-node (lower-parameters params-node tk-type tk-value) '()))
  (define body (if suite-node (lower-suite suite-node tk-type tk-value) (uir-null)))
  (uir-fn (if name (uir-symbol name) #f) params body))

(define (lower-parameters cst tk-type tk-value)
  (define toks (cst-tokens cst tk-type tk-value))
  (for/list ([t toks] #:when (eq? (first t) 'NAME))
    (uir-symbol (second t))))

;; Lower lambdef into uir-fn (anonymous function)
(define (lower-lambdef cst tk-type tk-value)
  (define kids (node-children cst))
  ;; lambdef: [varargslist?, test]
  ;; varargslist is not a direct kid — it's in a list wrapper
  (define raw-kids (any-tree-children cst))
  ;; Find varargslist: first CST node (could be direct or in a list)
  (define params
    (for/or ([c raw-kids])
      (cond [(cst-node? c)
             (if (eq? (any-tree-tag c) 'varargslist)
                 (lower-varargslist c tk-type tk-value)
                 #f)]
            [(and (list? c) (pair? c) (cst-node? (car c)))
             (if (eq? (any-tree-tag (car c)) 'varargslist)
                 (lower-varargslist (car c) tk-type tk-value)
                 #f)]
            [else #f])))
  ;; Find test: the last CST node (may be wrapped in list)
  (define body-expr
    (for/last ([c raw-kids])
      (cond [(cst-node? c)
             (if (eq? (any-tree-tag c) 'test)
                 (lower-expr c tk-type tk-value)
                 #f)]
            [(and (list? c) (pair? c) (cst-node? (car c)))
             (if (eq? (any-tree-tag (car c)) 'test)
                 (lower-expr (car c) tk-type tk-value)
                 #f)]
            [else #f])))
  (uir-fn #f (or params '()) (or body-expr (uir-null))))

;; Lower varargslist for lambda parameters (group of vfpdef nodes)
(define (lower-varargslist cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (for/list ([t toks] #:when (eq? (first t) 'NAME))
    (uir-symbol (second t))))

(define (lower-suite cst tk-type tk-value)
  (define kids (node-children cst))
  (if (null? kids)
      (uir-null)
      (lower-simple-stmts (first kids) tk-type tk-value)))

(define (lower-classdef cst tk-type tk-value)
  (define toks (cst-tokens cst tk-type tk-value))
  (define name (for/or ([t toks] #:when (eq? (first t) 'NAME)) (second t)))
  (define kids (node-children cst))
  (define suite-node (findf (λ (k) (eq? (any-tree-tag k) 'suite)) kids))
  (define body (if suite-node (lower-suite suite-node tk-type tk-value) (uir-null)))
  (uir-class (uir-symbol (or name "?class")) (uir-null) '()
             (list (uir-method (uir-symbol "__init__") '() body 'public))))

(define (lower-block-or-stmt cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (cond
    [(eq? tag 'suite) (lower-suite cst tk-type tk-value)]
    [(eq? tag 'block) (lower-block cst tk-type tk-value)]
    [(eq? tag 'simple_stmts) (lower-simple-stmts cst tk-type tk-value)]
    [(eq? tag 'simple_stmt) (lower-simple-stmt cst tk-type tk-value)]
    [else (lower-stmt cst tk-type tk-value)]))

(define (lower-block cst tk-type tk-value)
  (define kids (node-children cst))
  (if (or (null? kids) (>= (length kids) 2))
      (uir-block (map (λ (k) (lower-block-or-stmt k tk-type tk-value)) kids))
      (lower-block-or-stmt (first kids) tk-type tk-value)))

(module+ main
  (displayln "racklr/lower-python — Python3 CST → UIR lowering loaded."))
