#lang racket

(require "tree.rkt"
         "uir.rkt")

(provide lower-program)

;; ── JavaScript CST → UIR lowering pass ───────────────────────────────
;; lower-program takes: cst, tk-type, tk-value
;; tk-type and tk-value are the token accessors from the generated parser

;; Helpers
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

;; ── Entry ────────────────────────────────────────────────────────────

(define (lower-program cst tk-type tk-value)
  (define se (find-node-or-list cst))
  (if se (lower-source-elements se tk-type tk-value) (uir-null)))

(define (lower-source-elements v tk-type tk-value)
  (cond [(pair? v)
         (uir-block (filter-map (λ (e) (lower-source-elements e tk-type tk-value)) v))]
        [(cst-node? v)
         (case (tag-of v)
           [(sourceElements)
            (define lst (find-list v))
            (if lst
                (lower-source-elements lst tk-type tk-value)
                (uir-null))]
            [else
             ;; It's a sourceElement, statement, or other wrapper
             (define kid (first (cst-kids v)))
             (if kid
                 (lower-statement kid tk-type tk-value)
                 (uir-null))])]
        [else (uir-null)]))

(define (lower-source-element node tk-type tk-value)
  (define kid (first (cst-kids node)))
  (if kid
      (lower-statement kid tk-type tk-value)
      (uir-null)))

;; ── Statements ───────────────────────────────────────────────────────

(define (lower-statement node tk-type tk-value)
  (case (tag-of node)
    [(statement)
     (define inner (first (cst-kids node)))
     (if inner
         (lower-statement inner tk-type tk-value)
         (uir-null))]
    [(expressionStatement) (lower-expr-stmt node tk-type tk-value)]
    [(variableStatement) (lower-var-stmt node tk-type tk-value)]
    [(functionDeclaration) (lower-fn-decl node tk-type tk-value)]
    [(returnStatement) (lower-return-stmt node tk-type tk-value)]
    [(ifStatement) (lower-if-stmt node tk-type tk-value)]
    [(iterationStatement) (lower-iter-stmt node tk-type tk-value)]
    [(throwStatement) (lower-throw-stmt node tk-type tk-value)]
    [(breakStatement) (uir-call (uir-symbol "break") '())]
    [(continueStatement) (uir-call (uir-symbol "continue") '())]
    [(tryStatement) (lower-try-stmt node tk-type tk-value)]
    [(classDeclaration) (lower-class-decl node tk-type tk-value)]
    [(switchStatement) (lower-switch-stmt node tk-type tk-value)]
    [(importStatement) (lower-import-stmt node tk-type tk-value)]
    [(exportStatement) (lower-export-stmt node tk-type tk-value)]
    [(withStatement) (lower-with-stmt node tk-type tk-value)]
    [(debuggerStatement) (uir-call (uir-symbol "debugger") '())]
    [(yieldStatement) (lower-yield-stmt node tk-type tk-value)]
    [(labelledStatement) (lower-labelled-stmt node tk-type tk-value)]
    [(block) (lower-block node tk-type tk-value)]
    [(emptyStatement_) (uir-null)]
    [else (uir-null)]))

(define (lower-expr-stmt node tk-type tk-value)
  (define es (find-kid node 'expressionSequence))
  (lower-expression-sequence es tk-type tk-value))

(define (lower-var-stmt node tk-type tk-value)
  (define vdl (find-kid node 'variableDeclarationList))
  (unless vdl (uir-null))
  (define vm (find-kid vdl 'varModifier))
  (define kind (if vm (extract-var-kind vm tk-type) "var"))
  ;; Collect all variableDeclaration children: first as direct node,
  ;; then any more from the tail list (each wrapped in a group node).
  (define decls
    (let loop ([ks (kids-of vdl)] [acc '()])
      (cond [(null? ks) (reverse acc)]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'variableDeclaration))
             (loop (cdr ks) (cons (car ks) acc))]
            [(pair? (car ks))
             ;; Tail list of (group ...) nodes, each containing Comma + variableDeclaration
             (define tail-decls
               (for/list ([g (car ks)] #:when (cst-node? g))
                 (find-kid g 'variableDeclaration)))
             (loop (cdr ks) (append (reverse acc) tail-decls))]
            [else (loop (cdr ks) acc)])))
  (define uir-decls (map (λ (d) (uir-call (uir-symbol kind) (list (lower-var-decl d tk-type tk-value)))) decls))
  (if (= (length uir-decls) 1)
      (car uir-decls)
      (uir-block uir-decls)))

(define (extract-var-kind vm tk-type)
  (define kid (first (kids-of vm)))
  (cond [(and (tok? kid tk-type) (eq? (tk-type kid) 'Var)) "var"]
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Const)) "const"]
        [(and (cst-node? kid) (eq? (tag-of kid) 'let_)) "let"]
        [else "var"]))

(define (lower-var-decl node tk-type tk-value)
  (define assignable (find-kid node 'assignable))
  (define ident-node
    (and assignable (find-kid assignable 'identifier)))
  (define var-name
    (if ident-node
        (uir-symbol (tk-value (first (kids-of ident-node))))
        (uir-symbol "?")))
  (define group (find-kid node 'group))
  (define rhs
    (if group
        (let ([se (find-kid group 'singleExpression)])
          (if se
              (lower-single-expression se tk-type tk-value)
              (uir-null)))
        (uir-null)))
  (if (uir-null? rhs)
      (uir-var var-name)
      (uir-set! var-name rhs)))

(define (lower-fn-decl node tk-type tk-value)
  (define kids (kids-of node))
  ;; Detect async/generator: check all tokens
  (define is-async (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'Async))))
  (define is-generator (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'Multiply))))
  (define ident-node (find-kid node 'identifier))
  (define name
    (if ident-node
        (uir-symbol (tk-value (first (kids-of ident-node))))
        (uir-symbol "?")))
  (define body-node (find-kid node 'functionBody))
  (define body
    (if body-node
        (lower-fn-body body-node tk-type tk-value)
        (uir-block '())))
  (define fpl (find-kid node 'formalParameterList))
  (define params
    (if fpl
        (lower-formal-params fpl tk-type tk-value)
        '()))
  (define fn-uir (uir-fn #f params body))
  (cond [is-async (uir-set! name (uir-call (uir-symbol "async-fn") (list fn-uir)))]
        [is-generator (uir-set! name (uir-call (uir-symbol "gen-fn") (list fn-uir)))]
        [else (uir-set! name fn-uir)]))

(define (lower-fn-body node tk-type tk-value)
  (define se (find-node-or-list node))
  (if se (lower-source-elements se tk-type tk-value) (uir-block '())))

(define (lower-formal-params fpl tk-type tk-value)
  (define params '())
  (define (extract-param k)
    (define assignable (find-kid k 'assignable))
    (when assignable
      (define ident (find-kid assignable 'identifier))
      (when ident
        (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params)))))
  (let loop ([ks (kids-of fpl)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'formalParameterArg))
           (extract-param (car ks)) (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (when (eq? (tag-of g) 'formalParameterArg) (extract-param g)))
           (loop (cdr ks))]
          [else (loop (cdr ks))]))
  (reverse params))

(define (lower-return-stmt node tk-type tk-value)
  (define grp (find-kid node 'group))
  (define es (and grp (find-kid grp 'expressionSequence)))
  (uir-return (if es (lower-expression-sequence es tk-type tk-value) (uir-null))))

(define (lower-block node tk-type tk-value)
  (define stmt-list (find-kid node 'statementList))
  (if stmt-list
      (let ([lst (find-list stmt-list)])
        (if lst
            (lower-source-elements lst tk-type tk-value)
            (uir-null)))
      (uir-null)))

(define (lower-if-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (third kids) tk-type tk-value))
  (define consequent (lower-statement (fifth kids) tk-type tk-value))
  (define group (find-kid node 'group))
  (define alternate
    (if group
        (lower-statement (second (kids-of group)) tk-type tk-value)
        (uir-null)))
  (uir-if test consequent alternate))

(define (lower-throw-stmt node tk-type tk-value)
  (define es (find-kid node 'expressionSequence))
  (uir-call (uir-symbol "throw") (list (if es (lower-expression-sequence es tk-type tk-value) (uir-null)))))

(define (lower-try-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define try-body (lower-block (list-ref kids 1) tk-type tk-value))
  (define group (find-kid node 'group))
  (define catch-var (uir-null))
  (define catch-body (uir-null))
  (define finally-body (uir-null))
  (when group
    (for ([k (kids-of group)])
      (when (cst-node? k)
        (case (tag-of k)
          [(catchProduction)
           (define catch-group (find-kid k 'group))
           (when catch-group
             (define assignable (find-kid catch-group 'assignable))
             (when assignable
               (define ident (find-kid assignable 'identifier))
               (when ident
                 (set! catch-var (uir-var (uir-symbol (tk-value (first (kids-of ident)))))))))
           (set! catch-body (lower-block (find-kid k 'block) tk-type tk-value))]
          [(finallyProduction)
           (set! finally-body (lower-block (find-kid k 'block) tk-type tk-value))]))))
  (uir-call (uir-symbol "try") (list try-body catch-var catch-body finally-body)))

(define (lower-class-decl node tk-type tk-value)
  (define ident-node (find-kid node 'identifier))
  (define class-name
    (if ident-node
        (uir-symbol (tk-value (first (kids-of ident-node))))
        (uir-symbol "?")))
  (define tail (find-kid node 'classTail))
  (define super (uir-null))
  (when tail
    (define grp (find-kid tail 'group))
    (when grp
      (define se (find-kid grp 'singleExpression))
      (when se
        (set! super (lower-single-expression se tk-type tk-value)))))
  (uir-class class-name super '() '()))

(define (lower-switch-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
  (define case-block (list-ref kids 4))
  (define case-clauses-node (find-kid case-block 'caseClauses))
  (define default-group (find-kid case-block 'group))
  (define cases (uir-null))
  (define default-case (uir-null))
  (when case-clauses-node
    (define case-list (find-list case-clauses-node))
    (when case-list
      (set! cases (uir-block
                    (for/list ([cc case-list] #:when (cst-node? cc))
                      (lower-case-clause cc tk-type tk-value))))))
  (when default-group
    (define dc (find-kid default-group 'defaultClause))
    (when dc
      (define stmt-list (find-kid dc 'statementList))
      (when stmt-list
        (define lst (find-list stmt-list))
        (when lst
          (set! default-case (lower-source-elements lst tk-type tk-value))))))
  (uir-call (uir-symbol "switch") (list test cases default-case)))

(define (lower-case-clause node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (list-ref kids 1) tk-type tk-value))
  (define stmt-list-node (list-ref kids 3))
  (define lst (and (cst-node? stmt-list-node) (find-list stmt-list-node)))
  (define body (if lst (lower-source-elements lst tk-type tk-value) (uir-null)))
  (uir-call (uir-symbol "case") (list test body)))

(define (lower-iter-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define first-tok (and (tok? (first kids) tk-type) (tk-type (first kids))))
  (case first-tok
    [(While)
     ;; kids: [While, OpenParen, expressionSequence, CloseParen, statement]
     (define test (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
     (define body (lower-statement (list-ref kids 4) tk-type tk-value))
     (uir-call (uir-symbol "while") (list test body))]
    [(Do)
     ;; kids: [Do, statement, While, OpenParen, expressionSequence, CloseParen, eos]
     (define body (lower-statement (list-ref kids 1) tk-type tk-value))
     (define test (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
     (uir-call (uir-symbol "dowhile") (list body test))]
    [(For)
     (define has-in? (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'In))))
     (define has-of? (for/or ([k kids]) (and (tok? k tk-type) (eq? (tk-type k) 'Of))))
     (cond [has-in?
            ;; for (left in right) body
            ;; left is a group node wrapping singleVariableDeclaration or expressionSequence
            (define k2 (list-ref kids 2))
            (define left-expr
              (if (cst-node? k2)
                  (case (tag-of k2)
                    [(group) (lower-for-var k2 tk-type tk-value)]
                    [(expressionSequence) (lower-expression-sequence k2 tk-type tk-value)]
                    [else (uir-null)])
                  (uir-null)))
            (define right (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
            (define body (lower-statement (last kids) tk-type tk-value))
            (uir-call (uir-symbol "forin") (list left-expr right body))]
           [has-of?
            ;; for (left of right) body
            (define of-pos
              (for/or ([i (in-naturals)] [k kids] #:when (and (tok? k tk-type) (eq? (tk-type k) 'Of))) i))
            (define lk (list-ref kids (or (and of-pos (sub1 of-pos)) 2)))
            (define left-expr
              (if (cst-node? lk)
                  (case (tag-of lk)
                    [(group) (lower-for-var lk tk-type tk-value)]
                    [(expressionSequence) (lower-expression-sequence lk tk-type tk-value)]
                    [else (uir-null)])
                  (uir-null)))
            (define right (lower-expression-sequence (list-ref kids (or (and of-pos (+ of-pos 1)) 5)) tk-type tk-value))
            (define body (lower-statement (last kids) tk-type tk-value))
            (uir-call (uir-symbol "forof") (list left-expr right body))]
           [else
            ;; Regular for: [For, OpenParen, init?, SemiColon, test?, SemiColon, update?, CloseParen, statement]
            (define init (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
            (define test (lower-expression-sequence (list-ref kids 4) tk-type tk-value))
            (define update (lower-expression-sequence (list-ref kids 6) tk-type tk-value))
            (define body (lower-statement (list-ref kids 8) tk-type tk-value))
             (uir-call (uir-symbol "for") (list init test update body))])]
    [else (uir-null)]))

(define (lower-for-var group-node tk-type tk-value)
  ;; Extract variable name from a for-in/for-of group containing singleVariableDeclaration
  (define svd (find-kid group-node 'singleVariableDeclaration))
  (if svd
      (let ([vd (find-kid svd 'variableDeclaration)])
        (if vd
            (let ([assignable (find-kid vd 'assignable)])
              (define ident (and assignable (find-kid assignable 'identifier)))
              (if ident
                  (uir-var (uir-symbol (tk-value (first (kids-of ident)))))
                  (uir-symbol "?")))
            (uir-symbol "?")))
      (uir-symbol "?")))

;; ── Expressions ──────────────────────────────────────────────────────

(define (lower-expression-sequence node tk-type tk-value)
  (cond [(not node) (uir-null)]
        [(cst-node? node)
         (case (tag-of node)
           [(expressionSequence)
            (define kid (first (cst-kids node)))
            (lower-single-expression kid tk-type tk-value)]
           [(singleExpression) (lower-single-expression node tk-type tk-value)]
           [else (uir-null)])]
        [else (uir-null)]))

(define (lower-single-expression node tk-type tk-value)
  (define kids (kids-of node))
  (define (unary-prefix? k0) (and (tok? k0 tk-type) (cst-node? (second kids))))
  (define (unary-postfix? k0 k1) (and (cst-node? k0) (tok? k1 tk-type)))
  (cond [(= (length kids) 1)
         (lower-expr-atom (first kids) tk-type tk-value)]
         [(= (length kids) 2)
          (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'Await))
                 ;; Await expression: await + expr
                 (uir-await (lower-single-expression (second kids) tk-type tk-value))]
                [(unary-prefix? (first kids))
                ;; Unary prefix: op + expr
                (define raw (tk-value (first kids)))
                (define op-sym
                  (if (set-member? (set "++" "--") raw)
                      (string-append "prefix" raw)
                      raw))
                (uir-call (uir-symbol op-sym)
                          (list (lower-single-expression (second kids) tk-type tk-value)))]
               [(unary-postfix? (first kids) (second kids))
                ;; Unary postfix: expr + op (++, --)
                (define op-sym
                  (string-append "postfix" (tk-value (second kids))))
                (uir-call (uir-symbol op-sym)
                          (list (lower-single-expression (first kids) tk-type tk-value)))]
               [else
                ;; Function call: callee + arguments
                (define callee (lower-single-expression (first kids) tk-type tk-value))
                (define args-node (second kids))
                (if (and (cst-node? args-node) (eq? (tag-of args-node) 'arguments))
                    (uir-call callee (lower-arguments args-node tk-type tk-value))
                    (uir-null))])]
        [(= (length kids) 3)
         (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'New))
                (cond
                  ;; new.target
                  [(and (tok? (second kids) tk-type) (eq? (tk-type (second kids)) 'Dot))
                   (define target (lower-single-expression (third kids) tk-type tk-value))
                   (uir-call (uir-symbol "dot") (list (uir-symbol "new") target))]
                  ;; new Foo(args)
                  [else
                   (define class-name (lower-single-expression (second kids) tk-type tk-value))
                   (define args-node (third kids))
                   (uir-new class-name
                            (if (cst-node? args-node)
                                (lower-arguments args-node tk-type tk-value)
                                '()))])]
               [else
                ;; Binary infix: left + op + right
                (define left (lower-single-expression (first kids) tk-type tk-value))
                (define op-elt (second kids))
                (define right (lower-single-expression (third kids) tk-type tk-value))
                (define op-tok
                  (cond [(cst-node? op-elt) (first (kids-of op-elt))]
                        [(tok? op-elt tk-type) op-elt]
                        [else #f]))
                (if (and op-tok (tok? op-tok tk-type))
                    (uir-call (uir-symbol (tk-value op-tok))
                              (list left right))
                    (uir-null))])]
        [(= (length kids) 5)
         (cond [(and (tok? (second kids) tk-type) (eq? (tk-type (second kids)) 'QuestionMark))
                ;; Ternary: test ? consequent : alternate
                (uir-if (lower-single-expression (first kids) tk-type tk-value)
                        (lower-single-expression (third kids) tk-type tk-value)
                        (lower-single-expression (fifth kids) tk-type tk-value))]
               [else
                ;; Member access: object + (Dot/OpenBracket) + property/key
                (define obj (lower-single-expression (first kids) tk-type tk-value))
                (define op-elt (third kids))
                (cond [(and (tok? op-elt tk-type) (eq? (tk-type op-elt) 'Dot))
                       (uir-call (uir-symbol "dot")
                                 (list obj (uir-symbol (lower-identifier-name (list-ref kids 4) tk-type tk-value))))]
                      [(and (tok? op-elt tk-type) (eq? (tk-type op-elt) 'OpenBracket))
                       (uir-call (uir-symbol "index")
                                 (list obj (lower-expression-sequence (list-ref kids 3) tk-type tk-value)))]
                      [else (uir-null)])])]
        [else (uir-null)]))

(define (lower-expr-atom value tk-type tk-value)
  (cond [(cst-node? value)
         (case (tag-of value)
           [(literal) (lower-literal value tk-type tk-value)]
           [(identifier) (lower-identifier value tk-type tk-value)]
           [(singleExpression) (lower-single-expression value tk-type tk-value)]
            [(objectLiteral) (lower-object-literal value tk-type tk-value)]
            [(arrayLiteral) (lower-array-literal value tk-type tk-value)]
            [(anonymousFunction) (lower-arrow-fn value tk-type tk-value)]
            [(yieldStatement) (lower-yield-stmt value tk-type tk-value)]
            [else (uir-null)])]
        [(tok? value tk-type)
         (uir-symbol (tk-value value))]
        [else (uir-null)]))

(define (lower-identifier node tk-type tk-value)
  (define tok (first (kids-of node)))
  (uir-var (uir-symbol (tk-value tok))))

(define (lower-literal node tk-type tk-value)
  (define kid (first (kids-of node)))
  (cond [(cst-node? kid)
         (case (tag-of kid)
           [(numericLiteral)
            (uir-number (tk-value (first (kids-of kid))))]
           [(stringLiteral)
            (uir-string (tk-value (first (kids-of kid))))]
           [(regularExpressionLiteral)
            (uir-call (uir-symbol "regex") (list (uir-string (tk-value (first (kids-of kid))))))]
           [else (uir-null)])]
        [(tok? kid tk-type)
         (case (tk-type kid)
           [(BooleanLiteral)
            (uir-bool (string=? (tk-value kid) "true"))]
            [(NullLiteral) (uir-null)]
            [(RegularExpressionLiteral)
             (uir-call (uir-symbol "regex") (list (uir-string (tk-value kid))))]
            [(StringLiteral)
            (define raw (tk-value kid))
            (define len (string-length raw))
            (if (and (> len 1)
                     (or (char=? (string-ref raw 0) (string-ref raw (sub1 len)))
                         (eqv? (string-ref raw 0) #\"))
                     (or (char=? (string-ref raw 0) #\")
                         (char=? (string-ref raw 0) #\')))
                (uir-string (substring raw 1 (sub1 len)))
                (uir-string raw))]
           [else (uir-null)])]
        [else (uir-null)]))

(define (lower-arguments node tk-type tk-value)
  (define kids (kids-of node))
  (define arg-group (second kids))
  (cond [(and (cst-node? arg-group) (eq? (tag-of arg-group) 'group))
         (define grp-kids (kids-of arg-group))
         (define arg-nodes
           (let loop ([ks grp-kids] [acc '()])
             (cond [(null? ks) acc]
                   [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'argument))
                    (loop (cdr ks) (append acc (list (car ks))))]
                   [(pair? (car ks))
                    (define tail-args
                      (for/list ([g (car ks)] #:when (cst-node? g))
                        (find-kid g 'argument)))
                    (loop (cdr ks) (append acc tail-args))]
                   [else (loop (cdr ks) acc)])))
          (map (λ (a)
                 (define first-kid (first (kids-of a)))
                 (cond [(and (tok? first-kid tk-type) (eq? (tk-type first-kid) 'Ellipsis))
                        ;; Spread argument: ...expr
                        (define grp (find-kid a 'group))
                        (define se (and grp (first (cst-kids grp))))
                        (if se
                            (uir-call (uir-symbol "spread") (list (lower-single-expression se tk-type tk-value)))
                            (uir-null))]
                       [else
                        (define grp (first (cst-kids a)))
                        (define se (and (cst-node? grp) (eq? (tag-of grp) 'group)
                                        (first (cst-kids grp))))
                        (if se
                            (lower-single-expression se tk-type tk-value)
                            (uir-null))]))
               arg-nodes)]
        [else '()]))

(define (lower-identifier-name node tk-type tk-value)
  (define ident (or (find-kid node 'identifier)
                    (let ([in (find-kid node 'identifierName)])
                      (and in (find-kid in 'identifier)))))
  (if ident
      (tk-value (first (kids-of ident)))
      "?"))




(define (lower-getter-setter-name node tk-type tk-value)
  (define cen (find-kid node (quote classElementName)))
  (if cen
      (lower-identifier-name (find-kid cen (quote propertyName)) tk-type tk-value)
      (let ([ident (find-kid node (quote identifier))])
        (if ident (tk-value (first (kids-of ident))) "?"))))

(define (lower-object-literal node tk-type tk-value)
  (define grp (find-kid node (quote group)))
  (define entries (quote ()))
  (define (extract-prop k)
    (define ks (kids-of k))
    ;; Check for spread: { ...expr }
    (define is-spread (and (>= (length ks) 2)
                           (tok? (first ks) tk-type)
                           (eq? (tk-type (first ks)) 'Ellipsis)))
    (define pname (and (not is-spread) (find-kid k (quote propertyName))))
    (define se (and (not is-spread) (find-kid k (quote singleExpression))))
    (define fb (and (not is-spread) (find-kid k (quote functionBody))))
    (define getter (and (not is-spread) (find-kid k (quote getter))))
    (define setter (and (not is-spread) (find-kid k (quote setter))))
    ;; Check for computed property: {[expr]: val}
    ;; The grammar matches this as propertyName('[' singleExpression ']') : singleExpression
    ;; So pname is non-#f but wraps a computed key.
    (define is-computed
      (and pname
           (not getter) (not setter)
           (>= (length (kids-of pname)) 1)
           (tok? (first (kids-of pname)) tk-type)
           (string=? (tk-value (first (kids-of pname))) "[")))
    ;; Check for shorthand: {x}
    (define is-shorthand
      (and (not is-spread) (not is-computed) (not pname) (not getter) (not setter) (not fb) se))
    (cond
      [is-spread
       (define spread-se (find-kid k (quote singleExpression)))
       (when spread-se
         (set! entries (cons (cons (uir-string "...")
                                   (uir-call (uir-symbol "spread")
                                             (list (lower-single-expression spread-se tk-type tk-value))))
                             entries)))]
      [is-computed
       ;; pname = propertyName wrapping '[' singleExpression ']'
       (define pname-ks (kids-of pname))
       (define key-expr-node (second pname-ks))
       (when (and (cst-node? key-expr-node) se)
         (define key-uir (lower-single-expression key-expr-node tk-type tk-value))
         (define val-uir (lower-single-expression se tk-type tk-value))
         (set! entries (cons (cons key-uir val-uir) entries)))]
      [is-shorthand
       ;; Shorthand {x} is {x: x}. Extract identifier name from CST.
       (define ident-node
         (or (find-kid se (quote identifier))
             (find-kid se (quote identifierName))))
       (define name
         (if ident-node
             (uir-string (tk-value (first (kids-of ident-node))))
             (uir-string "?")))
       (define shorthand-uir (lower-single-expression se tk-type tk-value))
       (set! entries (cons (cons name shorthand-uir) entries))]
      [getter
       (define getter-name (lower-getter-setter-name getter tk-type tk-value))
       (define body (lower-fn-body fb tk-type tk-value))
       (set! entries (cons (cons (uir-string (string-append "get " getter-name))
                                 (uir-fn #f (quote ()) body))
                           entries))]
      [setter
       (define setter-name (lower-getter-setter-name setter tk-type tk-value))
       (define params (list (uir-symbol "v")))
       (define body (lower-fn-body fb tk-type tk-value))
       (set! entries (cons (cons (uir-string (string-append "set " setter-name))
                                 (uir-fn #f params body))
                           entries))]
      [(and pname fb (not se))
       (define key (uir-string (lower-identifier-name pname tk-type tk-value)))
       (define body (lower-fn-body fb tk-type tk-value))
       (set! entries (cons (cons key (uir-fn #f (quote ()) body)) entries))]
      [(and pname se)
       (let ([key (uir-string (lower-identifier-name pname tk-type tk-value))]
             [val (lower-single-expression se tk-type tk-value)])
         (set! entries (cons (cons key val) entries)))]))
  (when grp
    (let loop ([ks (kids-of grp)])
      (cond [(null? ks) (void)]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote propertyAssignment)))
             (extract-prop (car ks))
             (loop (cdr ks))]
            [(pair? (car ks))
             (for ([g (car ks)] #:when (cst-node? g))
               (define pa (find-kid g (quote propertyAssignment)))
               (when pa (extract-prop pa)))
             (loop (cdr ks))]
            [else (loop (cdr ks))])))
  (uir-record (reverse entries)))

(define (lower-array-literal node tk-type tk-value)
  (define grp (find-kid node (quote group)))
  (define elist (and grp (find-kid grp (quote elementList))))
  (define items (quote ()))
  (define (extract-array-element k)
    (define kids (kids-of k))
    (cond [(and (>= (length kids) 2)
                (tok? (first kids) tk-type)
                (eq? (tk-type (first kids)) (quote Ellipsis)))
           ;; Spread element: ...expr
           (define se (find-kid k (quote singleExpression)))
           (when se
             (set! items (cons (uir-call (uir-symbol "spread")
                                         (list (lower-single-expression se tk-type tk-value)))
                               items)))]
          [else
           (define se (find-kid k (quote singleExpression)))
           (when se
             (set! items (cons (lower-single-expression se tk-type tk-value) items)))]))
  (when elist
    (for ([k (kids-of elist)])
      (cond
        ((cst-node? k)
         (case (tag-of k)
           ((arrayElement) (extract-array-element k))))
        ((pair? k)
         (for ([g k] #:when (cst-node? g))
           (define ae (find-kid g (quote arrayElement)))
           (when ae (extract-array-element ae)))))))
  (uir-list (reverse items)))

(define (lower-arrow-fn node tk-type tk-value)
  ;; Distinguish: arrow has arrowFunctionParameters, function expr has Function_ token
  (define arrow-params-node (find-kid node (quote arrowFunctionParameters)))
  (if arrow-params-node
      (lower-arrow-fn-impl node tk-type tk-value)
      (lower-function-expr node tk-type tk-value)))

(define (lower-arrow-fn-impl node tk-type tk-value)
  (define params-node (find-kid node (quote arrowFunctionParameters)))
  (define body-node (find-kid node (quote arrowFunctionBody)))
  (define params (quote ()))
  (define (extract-param k)
    (define assignable (find-kid k (quote assignable)))
    (when assignable
      (let ([ident (find-kid assignable (quote identifier))])
        (when ident
          (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params))))))
  (define (extract-rest-param k)
    (define se (find-kid k (quote singleExpression)))
    (when se
      (let ([ident (find-kid se (quote identifier))])
        (if ident
            (set! params (cons (uir-call (uir-symbol "rest")
                                         (list (uir-symbol (tk-value (first (kids-of ident))))))
                               params))
            (set! params (cons (uir-call (uir-symbol "rest")
                                         (list (lower-single-expression se tk-type tk-value)))
                               params))))))
  (when params-node
    (define fpl (find-kid params-node (quote formalParameterList)))
    (when fpl
      (let loop ([ks (kids-of fpl)])
        (cond [(null? ks) (void)]
              [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote formalParameterArg)))
               (extract-param (car ks))
               (loop (cdr ks))]
              [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote lastFormalParameterArg)))
               (extract-rest-param (car ks))
               (loop (cdr ks))]
              [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote group)))
               (for ([g (kids-of (car ks))] #:when (cst-node? g))
                 (cond [(eq? (tag-of g) (quote formalParameterArg))
                        (extract-param g)]
                       [(eq? (tag-of g) (quote lastFormalParameterArg))
                        (extract-rest-param g)]))
               (loop (cdr ks))]
              [(pair? (car ks))
               (for ([g (car ks)] #:when (cst-node? g))
                 (when (eq? (tag-of g) (quote formalParameterArg))
                   (extract-param g))
                 (when (eq? (tag-of g) (quote lastFormalParameterArg))
                   (extract-rest-param g)))
               (loop (cdr ks))]
              [else (loop (cdr ks))]))))
  (define body
    (if body-node
        (let ([se (first (cst-kids body-node))])
          (lower-single-expression se tk-type tk-value))
        (uir-null)))
  (uir-call (uir-symbol "=>") (list (uir-list (reverse params)) body)))

(define (lower-function-expr node tk-type tk-value)
  ;; Regular function expression: anonymousFunction with Function_ token
  (define params (quote ()))
  (define body (uir-null))
  (define kids (kids-of node))
  ;; kids: [Function_, OpenParen, (params or CloseParen), ...]
  (define (extract-param k)
    (define assignable (find-kid k (quote assignable)))
    (when assignable
      (define ident (find-kid assignable (quote identifier)))
      (when ident
        (set! params (cons (uir-symbol (tk-value (first (kids-of ident)))) params)))))
  (define (extract-rest-param k)
    (define se (find-kid k (quote singleExpression)))
    (when se
      (let ([ident (find-kid se (quote identifier))])
        (if ident
            (set! params (cons (uir-call (uir-symbol "rest")
                                         (list (uir-symbol (tk-value (first (kids-of ident))))))
                               params))
            (set! params (cons (uir-call (uir-symbol "rest")
                                         (list (lower-single-expression se tk-type tk-value)))
                               params))))))
  (define fpl (find-kid node (quote formalParameterList)))
  (when fpl
    (let loop ([ks (kids-of fpl)])
      (cond [(null? ks) (void)]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote formalParameterArg)))
             (extract-param (car ks))
             (loop (cdr ks))]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote lastFormalParameterArg)))
             (extract-rest-param (car ks))
             (loop (cdr ks))]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote group)))
             (for ([g (kids-of (car ks))] #:when (cst-node? g))
               (cond [(eq? (tag-of g) (quote formalParameterArg))
                      (extract-param g)]
                     [(eq? (tag-of g) (quote lastFormalParameterArg))
                      (extract-rest-param g)]))
             (loop (cdr ks))]
            [(pair? (car ks))
             (for ([g (car ks)] #:when (cst-node? g))
               (when (eq? (tag-of g) (quote formalParameterArg))
                 (extract-param g))
               (when (eq? (tag-of g) (quote lastFormalParameterArg))
                 (extract-rest-param g)))
             (loop (cdr ks))]
            [else (loop (cdr ks))])))
  (define body-node (find-kid node (quote functionBody)))
  (when body-node
    (define se (find-node-or-list body-node))
    (set! body (if se (lower-source-elements se tk-type tk-value) (uir-block (quote ())))))
  (uir-call (uir-symbol "function") (list (uir-list (reverse params)) body)))

;; ── ES Modules (import/export) ───────────────────────────────────────

(define (lower-import-stmt node tk-type tk-value)
  (define ifb (find-kid node (quote importFromBlock)))
  (unless ifb (uir-null))
  (define kids (kids-of ifb))
  (define source (uir-null))
  ;; Find importFrom node for the source (named/default/namespace)
  (define if-node (find-kid ifb (quote importFrom)))
  (when if-node
    (define src-tok (for/or ([k (kids-of if-node)] #:when (and (tok? k tk-type) (eq? (tk-type k) (quote StringLiteral)))) k))
    (when src-tok
      (define raw (tk-value src-tok))
      (set! source (uir-string (substring raw 1 (sub1 (string-length raw)))))))
  (define first-kid (first (kids-of ifb)))
  (cond [(not first-kid) (uir-null)]
        ;; Bare import: import 'm'; — source is the first kid itself
        [(and (tok? first-kid tk-type) (eq? (tk-type first-kid) (quote StringLiteral)))
         (define raw (tk-value first-kid))
         (uir-call (uir-symbol "import") (list (uir-string (substring raw 1 (sub1 (string-length raw))))))]
        ;; Named/default/namespace via group
        [else
         (define group (find-kid ifb (quote group)))
         (unless group (uir-null))
         (define inner (first (cst-kids group)))
         (unless inner (uir-null))
         (case (tag-of inner)
           ;; import { x, y } or import { x, y } from 'm'
           [(importModuleItems)
            (define bindings (quote ()))
            ;; Iterate over importModuleItems children directly
            ;; Pattern: [LIST containing group], group, ...each wrapping importAliasName
            (let loop ([ks (kids-of inner)])
              (cond [(null? ks) (void)]
                    [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote group)))
                     (define ian (find-kid (car ks) (quote importAliasName)))
                     (let ([b (and ian (extract-import-alias ian tk-type tk-value))])
                       (when b (set! bindings (cons b bindings))))
                     (loop (cdr ks))]
                    [(pair? (car ks))
                     (for ([g (car ks)] #:when (cst-node? g))
                       (define ian (find-kid g (quote importAliasName)))
                       (let ([b (and ian (extract-import-alias ian tk-type tk-value))])
                         (when b (set! bindings (cons b bindings)))))
                     (loop (cdr ks))]
                    [else (loop (cdr ks))]))
            (define uir-bindings (uir-record (reverse bindings)))
            (uir-call (uir-symbol "import")
                      (if (uir-null? source)
                          (list uir-bindings)
                          (list uir-bindings source)))]
           ;; import x from 'm' or import * as ns from 'm'
           [(importNamespace)
            (define ns-kids (kids-of inner))
            (cond
              ;; import * as ns from 'm' — two group children:
              ;; first group has Multiply, second group has As + identifierName
              [(and (>= (length ns-kids) 2)
                    (cst-node? (first ns-kids))
                    (eq? (tag-of (first ns-kids)) (quote group))
                    (cst-node? (second ns-kids))
                    (eq? (tag-of (second ns-kids)) (quote group)))
               (define ns-name
                 (let* ([g2 (second ns-kids)]
                        [ident (find-kid (first (cst-kids g2)) (quote identifier))])
                   (if ident
                       (uir-symbol (tk-value (first (kids-of ident))))
                       (uir-symbol "?"))))
               (uir-call (uir-symbol "import")
                         (list (uir-list (list (uir-symbol "*") ns-name)) source))]
              ;; import x from 'm'
              [else
               (define g (find-kid inner (quote group)))
               (define ident-name
                 (if g
                     (let ([ident (find-kid (first (cst-kids g)) (quote identifier))])
                       (if ident
                           (uir-symbol (tk-value (first (kids-of ident))))
                           (uir-symbol "?")))
                     (uir-symbol "?")))
               (uir-call (uir-symbol "import")
                         (list ident-name source))])]
           [else (uir-null)])]))

(define (extract-import-alias node tk-type tk-value)
  (define men (find-kid node (quote moduleExportName)))
  ;; importedBinding may be wrapped in a group node (with As token)
  (define local
    (let ([g (find-kid node (quote group))])
      (or (find-kid node (quote importedBinding))
          (and g (find-kid g (quote importedBinding))))))
  (and men
       (let* ([men-name
               (let ([in (find-kid men (quote identifier))]
                     [inn (find-kid men (quote identifierName))])
                 (cond [in (tk-value (first (kids-of in)))]
                       [inn
                        (let ([ident (find-kid inn (quote identifier))])
                          (if ident (tk-value (first (kids-of ident))) "?"))]
                       [else "?"]))]
              [local-name
               (if local
                   (let ([ident (or (find-kid local (quote identifier))
                                    (let ([in (find-kid local (quote identifierName))])
                                      (and in (find-kid in (quote identifier)))))])
                     (if ident
                         (tk-value (first (kids-of ident)))
                         ;; Fallback: importedBinding may have a direct Identifier token
                         (let ([k (first (kids-of local))])
                           (if (and (not (null? k)) (not (eq? k (quote none)))
                                    (with-handlers ([exn:fail? (lambda (_) #f)])
                                      (eq? (tk-type k) (quote Identifier))))
                               (tk-value k)
                               men-name))))
                   men-name)])
         (cons (uir-string men-name) (uir-symbol local-name)))))

(define (lower-export-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define has-default (and (>= (length kids) 2)
                           (tok? (second kids) tk-type)
                           (eq? (tk-type (second kids)) (quote Default))))
  (cond
    ;; export default expr
    [has-default
     (define se (find-kid node (quote singleExpression)))
     (if se
         (uir-call (uir-symbol "export")
                   (list (uir-symbol "default") (lower-single-expression se tk-type tk-value)))
         (uir-null))]
    [else
     (define efb (find-kid node (quote exportFromBlock)))
     (define decl (find-kid node (quote declaration)))
     (cond
       ;; export { x, y } or export { x, y } from 'm'
       [efb
        (define items-group
          (let ([emi (find-kid efb (quote exportModuleItems))])
            (and emi (find-kid emi (quote group)))))
        (define bindings (quote ()))
        (when items-group
          (let loop ([ks (kids-of items-group)])
            (cond [(null? ks) (void)]
                  [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) (quote exportAliasName)))
                   (let ([b (extract-export-alias (car ks) tk-type tk-value)])
                     (when b (set! bindings (cons b bindings))))
                   (loop (cdr ks))]
                  [(pair? (car ks))
                   (for ([g (car ks)] #:when (cst-node? g))
                     (define ean (find-kid g (quote exportAliasName)))
                     (let ([b (extract-export-alias ean tk-type tk-value)])
                       (when b (set! bindings (cons b bindings)))))
                   (loop (cdr ks))]
                  [else (loop (cdr ks))])))
        (define source (uir-null))
        (define if-node (find-kid efb (quote importFrom)))
        (when if-node
          (define src-tok (for/or ([k (kids-of if-node)] #:when (and (tok? k tk-type) (eq? (tk-type k) (quote StringLiteral)))) k))
          (when src-tok
            (define raw (tk-value src-tok))
            (set! source (uir-string (substring raw 1 (sub1 (string-length raw)))))))
        (uir-call (uir-symbol "export")
                  (if (uir-null? source)
                      (list (uir-record (reverse bindings)))
                      (list (uir-record (reverse bindings)) source)))]
       ;; export const x = 1; / export function f() {} / export class Foo {}
       [decl
        (define first-decl (first (cst-kids decl)))
        (if first-decl
            (let ([inner (lower-statement first-decl tk-type tk-value)])
              (uir-call (uir-symbol "export") (list (uir-symbol "decl") inner)))
            (uir-null))]
       [else (uir-null)])]))

(define (extract-export-alias node tk-type tk-value)
  (define men (find-kid node (quote moduleExportName)))
  (define alias (find-kid node (quote As)))
  (and men
       (let* ([men-name
               (let ([in (find-kid men (quote identifier))]
                     [inn (find-kid men (quote identifierName))])
                 (cond [in (tk-value (first (kids-of in)))]
                       [inn
                        (let ([ident (find-kid inn (quote identifier))])
                          (if ident (tk-value (first (kids-of ident))) "?"))]
                       [else "?"]))]
              [export-name
               (if alias
                   (let* ([al-men (for/or ([k (kids-of node)] 
                                           #:when (and (cst-node? k) 
                                                       (eq? (tag-of k) (quote moduleExportName))
                                                       (not (eq? k men)))) k)])
                     (if al-men
                         (let ([in (find-kid al-men (quote identifier))]
                               [inn (find-kid al-men (quote identifierName))])
                           (cond [in (tk-value (first (kids-of in)))]
                                 [inn
                                  (let ([ident (find-kid inn (quote identifier))])
                                    (if ident (tk-value (first (kids-of ident))) "?"))]
                                 [else "?"]))
                         men-name))
                   men-name)])
         (cons (uir-string export-name) (uir-symbol men-name)))))

;; ── Misc statements ──────────────────────────────────────────────────

(define (lower-with-stmt node tk-type tk-value)
  (define kids (kids-of node))
  (define test (lower-expression-sequence (list-ref kids 2) tk-type tk-value))
  (define body (lower-statement (list-ref kids 4) tk-type tk-value))
  (uir-call (uir-symbol "with") (list test body)))

(define (lower-yield-stmt node tk-type tk-value)
  ;; kids: [group(Yield/YieldStar), optional group(expressionSequence), eos]
  (define kids (kids-of node))
  (define first-kid (first kids))
  ;; first kid is group containing the yield token
  (define tok-kid (and (cst-node? first-kid) (first (kids-of first-kid))))
  (define is-star (and (tok? tok-kid tk-type) (eq? (tk-type tok-kid) 'YieldStar)))
  ;; second kid is optional group with expressionSequence
  (define grp (find-kid node 'group))
  ;; But find-kid returns first matching child; we need the SECOND group
  (define expr-group
    (let loop ([ks (kids-of node)] [found-first? #f])
      (cond [(null? ks) #f]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'group))
             (if found-first? (car ks) (loop (cdr ks) #t))]
            [else (loop (cdr ks) found-first?)])))
  (define val
    (if expr-group
        (lower-expression-sequence (first (cst-kids expr-group)) tk-type tk-value)
        (uir-null)))
  (uir-yield val is-star))

(define (lower-await-expr node tk-type tk-value)
  ;; kids: [Await, singleExpression]
  (define kids (kids-of node))
  (define se (find-kid node 'singleExpression))
  (uir-await (if se (lower-single-expression se tk-type tk-value) (uir-null))))

(define (lower-labelled-stmt node tk-type tk-value)
  ;; kids: [identifier, ':' token, statement]
  (define kids (kids-of node))
  (define ident-node (first kids))
  (define label-name
    (if ident-node
        (tk-value (first (kids-of ident-node)))
        "?"))
  (define stmt-node (third kids))
  (define body (lower-statement stmt-node tk-type tk-value))
  (uir-call (uir-symbol "label") (list (uir-symbol label-name) body)))

(module+ main
  (displayln "lower-javascript loaded."))
