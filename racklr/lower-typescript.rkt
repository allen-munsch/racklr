#lang racket

(require racklr/tree
         racklr/uir)

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
             ;; Check for export token: the grammar parses `export` as a bare
             ;; keyword token inside sourceElement, not as an exportStatement node.
             (define has-export
               (for/or ([k (kids-of v)] #:when (and (tok? k tk-type) (eq? (tk-type k) 'Export))) #t))
             (if has-export
                 (lower-export-source-element v tk-type tk-value)
                 (let ([kid (first (cst-kids v))])
                   (if kid
                       (lower-statement kid tk-type tk-value)
                       (uir-null))))])]
        [else (uir-null)]))

(define (lower-source-element node tk-type tk-value)
  (define kid (first (cst-kids node)))
  (if kid
      (lower-statement kid tk-type tk-value)
      (uir-null)))

;; ── Export via sourceElement (grammar parses export as bare keyword) ──

(define (lower-export-source-element node tk-type tk-value)
  ;; sourceElement children: [Export token, statement, maybe eos]
  (define stmt-node (find-kid node 'statement))
  (unless stmt-node (uir-null))
  (define inner-stmt (first (cst-kids stmt-node)))
  (unless inner-stmt (uir-null))

  ;; Check for export default: look for a Default keyword in the expression
  (define (has-default-keyword? n)
    (for/or ([k (kids-of n)])
      (or (and (tok? k tk-type) (eq? (tk-type k) 'Default))
          (and (cst-node? k) (has-default-keyword? k)))))

  ;; Get the expressionStatement (may be inner-stmt itself or a child)
  (define es
    (if (eq? (tag-of inner-stmt) 'expressionStatement)
        inner-stmt
        (find-kid inner-stmt 'expressionStatement)))

  ;; Check for export { x, y } — inner statement is a block
  (cond
    [(eq? (tag-of inner-stmt) 'block)
     (lower-export-named-block inner-stmt tk-type tk-value)]

    ;; export default expr — first singleExpression wraps [identifierName(=default), singleExpression(expr)]
    [(and es
          (let ([es-seq (find-kid es 'expressionSequence)])
            (and es-seq
                 (let ([se (find-kid es-seq 'singleExpression)])
                   (and se (has-default-keyword? se))))))
     (define es-seq (find-kid es 'expressionSequence))
     (define se (find-kid es-seq 'singleExpression))
     ;; singleExpression children: [identifierName(=default), singleExpression(expr)]
     (define se-kids (filter cst-node? (kids-of se)))
     (if (>= (length se-kids) 2)
         (uir-call (uir-symbol "export")
                   (list (uir-symbol "default")
                         (lower-single-expression (second se-kids) tk-type tk-value)))
         (uir-null))]

    ;; export const/let/var/function/class declaration
    [else
     (define lowered (lower-statement inner-stmt tk-type tk-value))
     (uir-call (uir-symbol "export") (list (uir-symbol "decl") lowered))]))

(define (lower-export-named-block block-node tk-type tk-value)
  ;; block: [OpenBrace, statementList, CloseBrace]
  ;; statementList: list of statement → expressionStatement → expressionSequence
  ;; Extract identifiers for export { name1, name2 }
  (define stmt-list-node (find-kid block-node 'statementList))
  (unless stmt-list-node (uir-null))
  (define stmt-list (find-list stmt-list-node))
  (unless stmt-list (uir-null))
  (define names '())
  (for ([stmt (in-list stmt-list)]
        #:when (cst-node? stmt))
    (define es (or (eq? (tag-of stmt) 'expressionStatement)
                   (find-kid stmt 'expressionStatement)))
    ;; Handle both: stmt IS expressionStatement, or stmt CONTAINS one
    (define es-node (if (eq? (tag-of stmt) 'expressionStatement)
                        stmt
                        (find-kid stmt 'expressionStatement)))
    (when es-node
      (define es-seq (find-kid es-node 'expressionSequence))
      (when es-seq
        ;; First singleExpression is usually a named export
        (define first-se (find-kid es-seq 'singleExpression))
        (when first-se
          (define name (extract-identifier-name first-se tk-type tk-value))
          (when name (set! names (cons (cons (uir-string name) (uir-symbol name)) names))))
        ;; Check for comma-separated list of more names
        (define tail (find-list es-seq))
        (when tail
          (for ([g (in-list tail)]
                #:when (cst-node? g))
            (define se (find-kid g 'singleExpression))
            (when se
              (define name (extract-identifier-name se tk-type tk-value))
              (when name (set! names (cons (cons (uir-string name) (uir-symbol name)) names)))))))))
  (if (null? names)
      (uir-null)
      (uir-call (uir-symbol "export") (list (uir-record (reverse names))))))

;; Helpers for export lowering

(define (extract-identifier-name node tk-type tk-value)
  (define in (find-kid node 'identifierName))
  (and in
       (let ([ident (find-kid in 'identifier)])
         (and ident
              (let ([tok (first (kids-of ident))])
                (and (tok? tok tk-type) (eq? (tk-type tok) 'Identifier)
                     (tk-value tok)))))))

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
    [(enumDeclaration) (lower-enum-decl node tk-type tk-value)]
    [(namespaceDeclaration) (lower-namespace-decl node tk-type tk-value)]
    [(abstractDeclaration) (lower-abstract-decl node tk-type tk-value)]
    [(interfaceDeclaration) (uir-null)]
    [(typeAliasDeclaration) (uir-null)]
    [else (uir-null)]))

(define (lower-expr-stmt node tk-type tk-value)
  (define es (find-kid node 'expressionSequence))
  (lower-expression-sequence es tk-type tk-value))

(define (lower-var-stmt node tk-type tk-value)
  (define vdl (find-kid node 'variableDeclarationList))
  (unless vdl (uir-null))
  (define vm (find-kid node 'varModifier))
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
        [(and (tok? kid tk-type) (eq? (tk-type kid) 'Let)) "let"]
        [(and (cst-node? kid) (eq? (tag-of kid) 'let_)) "let"]
        [else "var"]))

(define (lower-var-decl node tk-type tk-value)
  ;; TypeScript variableDeclaration:
  ;;   (identifierOrKeyWord | arrayLiteral | objectLiteral) typeAnnotation? singleExpression? ('=' typeParameters? singleExpression)?
  ;; CST: group(identifierOrKeyWord->identifier), typeAnnotation?, none?, group(= rhs)?
  (define kids (kids-of node))
  ;; Find identifier group (first group child)
  (define id-group
    (for/or ([k kids] #:when (and (cst-node? k) (eq? (tag-of k) 'group))) k))
  (define var-name
    (if id-group
        (let* ([iok (find-kid id-group 'identifierOrKeyWord)]
               [ident (and iok (find-kid iok 'identifier))]
               [tok (and ident (first (kids-of ident)))])
          (if (and tok (tok? tok tk-type))
              (uir-symbol (tk-value tok))
              (uir-symbol "?")))
        (uir-symbol "?")))
  ;; Find initializer group (second group child)
  (define init-group
    (let loop ([ks kids] [found-first? #f])
      (cond [(null? ks) #f]
            [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'group))
             (if found-first? (car ks) (loop (cdr ks) #t))]
            [else (loop (cdr ks) found-first?)])))
  (define rhs
    (if init-group
        (let ([se (find-kid init-group 'singleExpression)])
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
  ;; TypeScript: function body is inside callSignature -> group -> group -> functionBody
  ;; or the declaration ends with SemiColon (bodyless)
  (define body
    (let* ([outer-group (find-kid node 'group)]
           [inner-group (and outer-group (find-kid outer-group 'group))]
           [fb-node (and inner-group (find-kid inner-group 'functionBody))])
      (if fb-node
          (lower-fn-body fb-node tk-type tk-value)
          (uir-block '()))))
  ;; TypeScript: parameters are in callSignature -> parameterList
  (define cs (find-kid node 'callSignature))
  (define pl (and cs (find-kid cs 'parameterList)))
  (define params
    (if pl
        (lower-formal-params pl tk-type tk-value)
        '()))
  (define fn-uir (uir-fn #f params body #f))
  (cond [is-async (uir-set! name (uir-call (uir-symbol "async-fn") (list fn-uir)))]
        [is-generator (uir-set! name (uir-call (uir-symbol "gen-fn") (list fn-uir)))]
        [else (uir-set! name fn-uir)]))

(define (lower-fn-body node tk-type tk-value)
  (define se (find-node-or-list node))
  (if se (lower-source-elements se tk-type tk-value) (uir-block '())))

(define (lower-formal-params fpl tk-type tk-value)
  (define params '())
  (define (extract-name k)
    ;; Try various paths to find the identifier name
    ;; k could be parameter, requiredParameter, optionalParameter, restParameter, formalParameterArg
    (define (find-ident n)
      (or (find-kid n 'identifierOrPattern)
          (find-kid n 'identifierName)
          (find-kid n 'identifier)
          (and (member (tag-of n) '(parameter formalParameterArg restParameter))
               (or (find-ident (find-kid n 'requiredParameter))
                   (find-ident (find-kid n 'optionalParameter))
                   (find-ident (find-kid n 'restParameter))))))
    (define io (find-ident k))
    (when io
      (let* ([ident (or (find-kid io 'identifier)
                        (let ([in (find-kid io 'identifierName)])
                          (and in (find-kid in 'identifier)))
                        io)]
             [tok (and ident (first (kids-of ident)))])
        (when (and tok (tok? tok tk-type))
          (set! params (cons (uir-symbol (tk-value tok)) params))))))
  (let loop ([ks (kids-of fpl)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks))
                (member (tag-of (car ks)) '(parameter formalParameterArg restParameter)))
           (extract-name (car ks)) (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (when (member (tag-of g) '(parameter formalParameterArg restParameter))
               (extract-name g)))
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
  (define heritage (find-kid node 'classHeritage))
  (define super (uir-null))
  (when heritage
    (define extends-clause (find-kid heritage 'classExtendsClause))
    (when extends-clause
      (define tr (find-kid extends-clause 'typeReference))
      (when tr
        (define tn (find-kid tr 'typeName))
        (when tn
          (define ident (find-kid tn 'identifier))
          (when ident
            (set! super (uir-symbol (tk-value (first (kids-of ident))))))))))
  (define methods '())
  (when tail
    (define tail-kids (kids-of tail))
    ;; tail-kids: [OpenBrace, list-of-classElement, CloseBrace]
    (when (>= (length tail-kids) 2)
      (define element-list (second tail-kids))
      (when (pair? element-list)
        (set! methods
              (for/list ([elem (in-list element-list)]
                         #:when (cst-node? elem))
                (lower-class-element elem tk-type tk-value))))))
  (uir-class class-name super '() methods))

(define (extract-param-list fpl tk-type tk-value)
  ;; Extract parameter names from formalParameterList or parameterList
  ;; Handles both formalParameterArg (TS) and parameter/requiredParameter (JS) styles
  (define params '())
  (define (find-ident-deep k)
    ;; Recursively find first identifier token
    (let loop ([n k])
      (cond [(and (cst-node? n) (eq? (tag-of n) 'identifier))
             (tk-value (first (kids-of n)))]
            [(cst-node? n)
             (for/or ([c (kids-of n)] #:when (cst-node? c))
               (loop c))]
            [else #f])))
  (define (extract-name k)
    (define name (find-ident-deep k))
    (when name
      (set! params (cons (uir-symbol name) params))))
  (for ([k (kids-of fpl)])
    (cond [(and (cst-node? k) (or (eq? (tag-of k) 'formalParameterArg)
                                  (eq? (tag-of k) 'parameter)))
           (define inner (or (find-kid k 'requiredParameter)
                            (find-kid k 'assignable)))
           (if inner (extract-name inner) (extract-name k))]
          [(and (cst-node? k) (eq? (tag-of k) 'requiredParameter))
           (extract-name k)]
          [(and (cst-node? k) (eq? (tag-of k) 'group))
           ;; Comma-separated tail: group wraps (Comma param)+
           (for ([g (kids-of k)] #:when (cst-node? g))
             (cond [(or (eq? (tag-of g) 'formalParameterArg)
                        (eq? (tag-of g) 'parameter))
                    (define inner (or (find-kid g 'requiredParameter)
                                     (find-kid g 'assignable)))
                    (if inner (extract-name inner) (extract-name g))]
                   [(eq? (tag-of g) 'requiredParameter)
                    (extract-name g)]))]
          [(pair? k)
           (for ([g (in-list k)] #:when (cst-node? g))
             (cond [(or (eq? (tag-of g) 'formalParameterArg)
                        (eq? (tag-of g) 'parameter))
                    (define inner (or (find-kid g 'requiredParameter)
                                     (find-kid g 'assignable)))
                    (if inner (extract-name inner) (extract-name g))]
                   [(eq? (tag-of g) 'requiredParameter)
                    (extract-name g)]))]))
  (reverse params))

(define (lower-class-element node tk-type tk-value)
  ;; classElement → propertyMemberDeclaration
  (define pmd (find-kid node 'propertyMemberDeclaration))
  (unless pmd (uir-null))
  ;; Method name
  (define pname-node (find-kid pmd 'propertyName))
  (define method-name
    (if pname-node
        (let ([in (find-kid pname-node 'identifierName)])
          (if in
              (let ([ident (find-kid in 'identifier)])
                (if ident
                    (uir-symbol (tk-value (first (kids-of ident))))
                    (uir-symbol "?")))
              (uir-symbol "?")))
        (uir-symbol "?")))
  ;; Parameters from callSignature
  (define cs (find-kid pmd 'callSignature))
  (define params
    (if cs
        (let ([fpl (or (find-kid cs 'formalParameterList)
                       (find-kid cs 'parameterList))])
          (if fpl
              (extract-param-list fpl tk-type tk-value)
              '()))
        '()))
  ;; Body from group → group → functionBody → sourceElements
  (define outer-group (find-kid pmd 'group))
  (define body (uir-null))
  (when outer-group
    (define inner-group (find-kid outer-group 'group))
    (when inner-group
      (define fb (find-kid inner-group 'functionBody))
      (when fb
        (define se (find-kid fb 'sourceElements))
        (when se
          (set! body (lower-source-elements se tk-type tk-value))))))
  ;; Visibility: check propertyMemberBase for static/private/public tokens  
  (define pmb (find-kid pmd 'propertyMemberBase))
  (define visibility 'public)
  (uir-method method-name params body visibility))

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

(define (lower-for-init eseque tk-type tk-value)
  "lower for-init expressionSequence, handling declaration keyword patterns."
  (cond [(not (and eseque (cst-node? eseque))) (uir-null)]
        [(not (eq? (tag-of eseque) 'expressionSequence)) (uir-null)]
        [else
         (define se-kids (cst-kids eseque))
         (cond [(null? se-kids) (uir-null)]
               [else
                (define first-se (first se-kids))
                (define fsk (cst-kids first-se))
                (cond
                  ;; Declaration keyword + init: singleExpression wraps (identifierName keyword, singleExpression assignment)
                  [(and (= (length fsk) 2)
                        (eq? (tag-of (first fsk)) 'identifierName)
                        (let ([kw-val (get-keyword-value (first fsk) tk-value)])
                          (and kw-val (member kw-val '("let" "var" "const")))))
                   (let ([kw-val (get-keyword-value (first fsk) tk-value)]
                         [assn (lower-single-expression (second fsk) tk-type tk-value)])
                     (uir-call (uir-symbol kw-val) (list assn)))]
                  ;; Normal single expression init
                  [else (lower-single-expression first-se tk-type tk-value)])])]))

(define (get-keyword-value node tk-value)
  ;; Walk cst-kids down to a 'keyword node, return its token value
  (let loop ([n node])
    (cond [(not (cst-node? n)) #f]
          [(eq? (cst-node-tag n) 'keyword)
           (for/or ([k (cst-node-children n)] #:when (tk-value k))
             (tk-value k))]
          [else (for/or ([k (cst-kids n)]) (loop k))])))

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
             (define init (lower-for-init (list-ref kids 2) tk-type tk-value))
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
                 ;; TypeScript non-null assertion: expr! → just expr
                 (if (eq? (tk-type (second kids)) 'Not)
                     (lower-single-expression (first kids) tk-type tk-value)
                     (let ([op-sym (string-append "postfix" (tk-value (second kids)))])
                       (uir-call (uir-symbol op-sym)
                                 (list (lower-single-expression (first kids) tk-type tk-value)))))]
                [(and (cst-node? (first kids))
                      (not (tok? (second kids) tk-type))
                      (not (cst-node? (second kids))))
                 ;; IdentifierAsExpression: identifierName + null/none trailing context
                 (lower-expr-atom (first kids) tk-type tk-value)]
                [else
                 ;; Detect TS infix: left + (operator . right) nested in right node
                 (define left-kid (first kids))
                 (define right-kid (second kids))
                 (cond [(and (cst-node? right-kid) (eq? (tag-of right-kid) 'singleExpression)
                             (not (null? (kids-of right-kid)))
                             (tok? (first (kids-of right-kid)) tk-type)
                             ;; Infix: 2 kids (op token, right operand). Call: 3+ kids (paren, args, paren).
                             (= (length (kids-of right-kid)) 2)
                             (cst-node? (second (kids-of right-kid))))
                        (define op (tk-value (first (kids-of right-kid))))
                        (define rhs (second (kids-of right-kid)))
                        (uir-call (uir-symbol op)
                                  (list (lower-single-expression left-kid tk-type tk-value)
                                        (lower-single-expression rhs tk-type tk-value)))]
                        [else
                         ;; Function call: callee + arguments
                         (define callee (lower-single-expression (first kids) tk-type tk-value))
                         (define args-node (second kids))
                         (cond [(and (cst-node? args-node) (eq? (tag-of args-node) 'arguments))
                                (uir-call callee (lower-arguments args-node tk-type tk-value))]
                               [(and (cst-node? args-node) (eq? (tag-of args-node) 'singleExpression)
                                     (>= (length (kids-of args-node)) 3)
                                     (tok? (first (kids-of args-node)) tk-type)
                                     (eq? (tk-type (first (kids-of args-node))) 'OpenParen))
                                ;; TS call: callee + singleExpression(OpenParen, expressionSequence, CloseParen)
                                (define inner-kids (kids-of args-node))
                                (define args-es (second inner-kids))
                                (define arg-exprs
                                  (let loop ([ks (kids-of args-es)] [acc '()])
                                    (cond [(null? ks) (reverse acc)]
                                          [(cst-node? (car ks))
                                           (loop (cdr ks)
                                                 (cons (lower-single-expression (car ks) tk-type tk-value) acc))]
                                          [(pair? (car ks))
                                           ;; Comma-separated remaining args wrapped in groups
                                           (define more-args
                                             (for/list ([g (car ks)] #:when (cst-node? g))
                                               (define inner (second (kids-of g)))
                                               (lower-single-expression inner tk-type tk-value)))
                                           (loop (cdr ks) (append (reverse more-args) acc))]
                                          [else (loop (cdr ks) acc)])))
                                (uir-call callee arg-exprs)]
                               [else (uir-null)])])])]
        [(= (length kids) 3)
         (cond [(and (tok? (first kids) tk-type) (eq? (tk-type (first kids)) 'New))
                (cond
                  ;; new.target
                  [(and (tok? (second kids) tk-type) (eq? (tk-type (second kids)) 'Dot))
                   (define target (lower-single-expression (third kids) tk-type tk-value))
                   (uir-call (uir-symbol "dot") (list (uir-symbol "new") target))]
                  ;; new Foo(args) — but parser may nest call inside second kid
                  [else
                   (define snd (second kids))
                   (define-values (class-name args)
                     (cond
                       ;; Case: second kid is singleExpression wrapping [classExpr, arguments]
                       ;; e.g. new Date() → [New, singleExpression([Date, arguments]), 'none]
                       [(and (cst-node? snd) (eq? (tag-of snd) 'singleExpression)
                             (= (length (kids-of snd)) 2)
                             (let ([snd-kids (kids-of snd)])
                               (and (cst-node? (first snd-kids))
                                    (cst-node? (second snd-kids))
                                    (eq? (tag-of (second snd-kids)) 'arguments))))
                        (define snd-kids (kids-of snd))
                        (values (lower-single-expression (first snd-kids) tk-type tk-value)
                                (lower-arguments (second snd-kids) tk-type tk-value))]
                       [else
                        (values (lower-single-expression snd tk-type tk-value)
                                (let ([args-node (third kids)])
                                  (if (cst-node? args-node)
                                      (lower-arguments args-node tk-type tk-value)
                                      '())))]))
                   (uir-new class-name args)])]
                               [else
                 ;; Binary infix: left + op + right
                 (define left (lower-single-expression (first kids) tk-type tk-value))
                 (define op-elt (second kids))
                 (define right (lower-single-expression (third kids) tk-type tk-value))
                 ;; Extract operator token: handle nested singleExpression -> identifierName -> identifier -> token
                 (define op-tok
                   (let loop ([n op-elt])
                     (cond [(tok? n tk-type) n]
                           [(cst-node? n)
                            (let ([k (first (kids-of n))])
                              (if (tok? k tk-type) k (loop k)))]
                           [else #f])))
                 ;; TypeScript as-expression: left as type -> just left
                 (if (and op-tok (tok? op-tok tk-type) (eq? (tk-type op-tok) (quote As)))
                     left
                     (if (and op-tok (tok? op-tok tk-type))
                         (uir-call (uir-symbol (tk-value op-tok))
                                   (list left right))
                         (uir-null)))])]        [(>= (length kids) 5)
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
                       [else
                        ;; TS: extra wrapper nodes. Try scanning for the op token.
                        (define kid-count (length kids))
                        (define op-idx
                          (for/or ([i (in-range 1 (sub1 kid-count))]
                                   [k (in-list (drop kids 1))]
                                   #:when (and (tok? k tk-type)
                                               (member (tk-type k) '(Dot OpenBracket))))
                            i))
                        (if op-idx
                            (cond [(eq? (tk-type (list-ref kids op-idx)) 'Dot)
                                   (define left (lower-single-expression (list-ref kids (sub1 op-idx)) tk-type tk-value))
                                   (define right-node (list-ref kids (add1 op-idx)))
                                   (define right
                                     (if (cst-node? right-node)
                                         (uir-symbol (lower-identifier-name right-node tk-type tk-value))
                                         (uir-symbol "?")))
                                   (uir-call (uir-symbol "dot") (list left right))]
                                  [(eq? (tk-type (list-ref kids op-idx)) 'OpenBracket)
                                   (define left (lower-single-expression (list-ref kids (sub1 op-idx)) tk-type tk-value))
                                   (define seq (list-ref kids (add1 op-idx)))
                                   (uir-call (uir-symbol "index") (list left (lower-expression-sequence seq tk-type tk-value)))]
                                  [else (uir-null)])
                            (uir-null))])])]
        [else (uir-null)]))

(define (lower-expr-atom value tk-type tk-value)
  (cond [(cst-node? value)
         (case (tag-of value)
           [(literal) (lower-literal value tk-type tk-value)]
            [(identifier) (lower-identifier value tk-type tk-value)]
            [(identifierName)
             ;; TS: identifierName -> identifier | reservedWord | ...
             (define inner (first (kids-of value)))
             (cond [(tok? inner tk-type)
                    (case (tk-type inner)
                      [(BooleanLiteral) (uir-bool (string=? (tk-value inner) "true"))]
                      [(NullLiteral) (uir-null)]
                      [else (uir-var (uir-symbol (tk-value inner)))])]
                   [(cst-node? inner)
                    (case (tag-of inner)
                      [(reservedWord)
                       (define tok (first (kids-of inner)))
                       (cond [(tok? tok tk-type)
                              (case (tk-type tok)
                                [(BooleanLiteral) (uir-bool (string=? (tk-value tok) "true"))]
                                [(NullLiteral) (uir-null)]
                                [else (uir-var (uir-symbol (tk-value tok)))])]
                             [else (uir-null)])]
                      [(identifier)
                       (define tok (first (kids-of inner)))
                       (if (tok? tok tk-type)
                           (uir-var (uir-symbol (tk-value tok)))
                           (uir-null))]
                      [else (uir-null)])]
                   [else (uir-null)])]
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
                    [(and (cst-node? (car ks))
                          (member (tag-of (car ks)) '(argument argumentList)))
                     (if (eq? (tag-of (car ks)) 'argument)
                         (loop (cdr ks) (append acc (list (car ks))))
                         ;; TS wraps arguments in argumentList; dive into its kids
                         (loop (append (kids-of (car ks)) (cdr ks)) acc))]
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
                                 (uir-fn #f (quote ()) body #f))
                           entries))]
      [setter
       (define setter-name (lower-getter-setter-name setter tk-type tk-value))
       (define params (list (uir-symbol "v")))
       (define body (lower-fn-body fb tk-type tk-value))
       (set! entries (cons (cons (uir-string (string-append "set " setter-name))
                                 (uir-fn #f params body #f))
                           entries))]
      [(and pname fb (not se))
       (define key (uir-string (lower-identifier-name pname tk-type tk-value)))
       (define body (lower-fn-body fb tk-type tk-value))
       (set! entries (cons (cons key (uir-fn #f (quote ()) body #f)) entries))]
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
  ;; Distinguish: arrow has arrowFunctionParameters, function expr has Function_ token.
  ;; TS: anonymousFunction may wrap arrowFunctionDeclaration; check inside that too.
  (define afd (find-kid node (quote arrowFunctionDeclaration)))
  (define arrow-params-node
    (or (find-kid node (quote arrowFunctionParameters))
        (and afd (find-kid afd (quote arrowFunctionParameters)))))
  (if arrow-params-node
      (lower-arrow-fn-impl (or afd node) tk-type tk-value)
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
                  (cond [(eq? (tag-of g) (quote formalParameterArg))
                         (extract-param g)]
                        [(eq? (tag-of g) (quote lastFormalParameterArg))
                         (extract-rest-param g)]
                        [(eq? (tag-of g) (quote group))
                         ;; TS wraps remaining params in group(Comma, formalParameterArg)
                         (for ([gg (kids-of g)] #:when (cst-node? gg))
                           (cond [(eq? (tag-of gg) (quote formalParameterArg))
                                  (extract-param gg)]
                                 [(eq? (tag-of gg) (quote lastFormalParameterArg))
                                  (extract-rest-param gg)]))]))
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
         (define emi (find-kid efb (quote exportModuleItems)))
         (define bindings (quote ()))
         (when emi
           (let loop ([ks (kids-of emi)])
             (cond [(null? ks) (void)]
                   [(and (cst-node? (car ks))
                         (eq? (tag-of (car ks)) (quote exportAliasName)))
                    (let ([b (extract-export-alias (car ks) tk-type tk-value)])
                      (when b (set! bindings (cons b bindings))))
                    (loop (cdr ks))]
                   [(and (cst-node? (car ks))
                         (eq? (tag-of (car ks)) (quote group)))
                    ;; Parse-opt wraps the last item in a direct group node
                    (define ean (find-kid (car ks) (quote exportAliasName)))
                    (when ean
                      (let ([b (extract-export-alias ean tk-type tk-value)])
                        (when b (set! bindings (cons b bindings)))))
                    (loop (cdr ks))]
                   [(pair? (car ks))
                    (for ([g (car ks)] #:when (cst-node? g))
                      (define ean
                        (or (find-kid g (quote exportAliasName))
                            (and (eq? (tag-of g) (quote exportAliasName)) g)))
                      (when ean
                        (let ([b (extract-export-alias ean tk-type tk-value)])
                          (when b (set! bindings (cons b bindings))))))
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

;; ============================================================================
;; TypeScript-specific lowering functions
;; ============================================================================

(define (lower-enum-decl node tk-type tk-value)
  ;; enumDeclaration -> 'enum' (Identifier) '{' enumBody '}'
  ;; enumBody -> enumMember (',' enumMember)* ','?
  ;; Lower to: (uir-call (uir-symbol "<name>") members...)
  (define name-node (find-kid node 'identifier))
  (define name
    (if name-node
        (uir-symbol (tk-value (first (kids-of name-node))))
        (uir-symbol "?")))
  (define enum-body (find-kid node 'enumBody))
  (define members
    (if enum-body
        (lower-enum-members enum-body tk-type tk-value)
        '()))
  (uir-call name members))

(define (lower-enum-members node tk-type tk-value)
  ;; enumBody -> enumMemberList (enumMember (',' enumMember)* ','?)
  ;; enumMemberList children: first enumMember direct, rest in list of (group Comma enumMember)
  (define eml (find-kid node 'enumMemberList))
  (unless eml '())
  (define members '())
  (let loop ([ks (kids-of eml)])
    (cond [(null? ks) (void)]
          [(and (cst-node? (car ks)) (eq? (tag-of (car ks)) 'enumMember))
           (set! members (cons (lower-enum-member (car ks) tk-type tk-value) members))
           (loop (cdr ks))]
          [(pair? (car ks))
           (for ([g (car ks)] #:when (cst-node? g))
             (define em (find-kid g 'enumMember))
             (when em (set! members (cons (lower-enum-member em tk-type tk-value) members))))
           (loop (cdr ks))]
          [else (loop (cdr ks))]))
  (reverse members))

(define (lower-enum-member node tk-type tk-value)
  ;; enumMember -> identifierOrKeyWord? ('=' singleExpression)?
  ;; TS enum members may use propertyName or bare identifierOrKeyWord.
  ;; Extract the member name as a string.
  (define name
    (uir-string
     (let* ([pn (find-kid node 'propertyName)]
            [name-node (if pn
                           (or (find-kid pn 'identifierOrKeyWord)
                               (find-kid pn 'identifierName))
                           (or (find-kid node 'identifierOrKeyWord)
                               (find-kid node 'identifierName)))]
            [ident (and name-node (find-kid name-node 'identifier))]
            [tok (and ident (first (kids-of ident)))])
       (if (and tok (tok? tok tk-type))
           (tk-value tok)
           "?"))))
  (define se (find-kid node 'singleExpression))
  (define value
    (if se
        (lower-single-expression se tk-type tk-value)
        (uir-null)))
  (uir-call (uir-symbol "enum-member") (list name value)))

(define (lower-namespace-decl node tk-type tk-value)
  ;; namespaceDeclaration -> 'namespace' namespaceName '{' statementList? '}'
  ;; namespaceName -> identifier ('.' identifier)*
  ;; statementList child is a LIST of statements
  (define ns-name-node (find-kid node 'namespaceName))
  (define name
    (if ns-name-node
        (let* ([ident (or (find-kid ns-name-node 'identifier)
                          (let ([in (find-kid ns-name-node 'identifierName)])
                            (and in (find-kid in 'identifier))))]
               [tok (and ident (first (kids-of ident)))])
          (if (and tok (tok? tok tk-type))
              (uir-symbol (tk-value tok))
              (uir-symbol "?")))
        (uir-symbol "?")))
  (define body
    (let ([stmt-list (find-kid node 'statementList)])
      (if stmt-list
          (let ([lst (find-list stmt-list)])
            (if lst
                (lower-source-elements lst tk-type tk-value)
                (uir-block '())))
          (uir-block '()))))
  (uir-call (uir-symbol "namespace") (list name body)))

(define (lower-abstract-decl node tk-type tk-value)
  ;; abstractDeclaration -> 'abstract' classDeclaration | functionDeclaration
  ;; Just forward to the inner declaration (stripping 'abstract')
  (define kids (kids-of node))
  (define inner (for/or ([k kids] #:when (cst-node? k)) k))
  (if inner
      (lower-statement inner tk-type tk-value)
      (uir-null)))


