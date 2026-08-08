#lang racket

(require racklr/tree
         racklr/uir)

(provide lower-jsx)

;; ── Helpers ─────────────────────────────────────────────────────────

(define (tok? n tk-type) 
  (and (not (cst-node? n)) (not (null? n)) (not (eq? n 'none))
       (with-handlers ([exn:fail? (λ (_) #f)]) (tk-type n) #t)))

(define (cst-kids n) (filter cst-node? (cst-node-children n)))
(define (tag-of n) (cst-node-tag n))
(define (kids-of n) (cst-node-children n))

;; Find the first child with a given tag
(define (find-child-tag n tag)
  (for/or ([k (kids-of n)] #:when (and (cst-node? k) (eq? (tag-of k) tag))) k))

;; Find the first child that is a CST node or list
(define (find-first-kid n)
  (for/or ([k (kids-of n)] #:when (or (cst-node? k) (pair? k))) k))

;; Unwrap a value that might be a list-wrapped CST node from parser repetition
(define (unwrap v) (if (pair? v) (first v) v))

;; Flatten a CST node's children, removing tokens and unwrapping
;; one level of parser-generated LIST wrappers from repetition rules.
(define (flat-kids n)
  (define (flatten-one ch)
    (if (and (pair? ch) (not (cst-node? ch)))
        (filter (λ (x) (or (cst-node? x) (pair? x))) ch)
        (if (cst-node? ch) (list ch) '())))
  (append*
   (for/list ([ch (in-list (kids-of n))])
     (flatten-one ch))))

;; ── Link component → <a> element transform ─────────────────────────
;; <Link href="/p">text</Link> → <a href="#/p" onClick="navigate">text</a>

(define (link-attrs->anchor-attrs attrs)
  (define href-val
    (for/or ([attr (in-list attrs)])
      (match-define (uir-attribute name-uir value-uir) attr)
      (and (eq? (uir-symbol-name name-uir) 'href)
           value-uir)))
  (define href-path
    (if (and href-val (uir-string? href-val))
        (uir-string-value href-val)
        "/"))
  (define onclick-expr
    (format "function() { window.location.hash = \"~a\"; }" href-path))
  (list (uir-attribute (uir-symbol 'href)
                       (uir-string (string-append "#" href-path)))
        (uir-attribute (uir-symbol 'onClick)
                       (uir-jsx-expr onclick-expr))))

;; ── Token helpers ───────────────────────────────────────────────────

(define ((tok-type-match? name) n tk-type)
  (and (tok? n tk-type) (eq? (tk-type n) name)))

(define (tok-type-eq n tk-type name)
  (and (tok? n tk-type) (eq? (tk-type n) name)))

(define (tok-value-of n tk-type tk-value)
  (and (tok? n tk-type) (tk-value n)))

;; ── Lowering ────────────────────────────────────────────────────────

;; Entry: lower a JSX CST into UIR
;; Returns a uir-element
(define (lower-jsx cst #:tk-type tk-type #:tk-value tk-value)
  (lower-jsx-element cst tk-type tk-value))

;; jsxElement → uir-element (or uir-list/nil for fragments)
(define (lower-jsx-element node tk-type tk-value)
  (define kids (kids-of node))
  
  ;; Fragment check: jsxElement → jsxFragment
  (define first-kid (first kids))
  (if (and (cst-node? first-kid) (eq? (tag-of first-kid) 'jsxFragment))
      (lower-jsx-fragment first-kid tk-type tk-value)
      (lower-jsx-element/normal kids tk-type tk-value)))

;; Fragment lowering: <></> or <>children</>
(define (lower-jsx-fragment node tk-type tk-value)
  ;; Layout: JsxOpen JsxOpeningEnd jsxChildren? JsxClose JsxClosingEnd
  (define fkids (kids-of node))
  (define children-node (third fkids))
  (define children
    (if (cst-node? children-node)
        (lower-jsx-children children-node tk-type tk-value)
        '()))
  (cond [(null? children) (uir-null)]
        [(= (length children) 1) (first children)]
        [else (uir-list children)]))

;; Normal (non-fragment) jsxElement → uir-element
(define (lower-jsx-element/normal kids tk-type tk-value)
  ;; kids layout depends on self-closing vs with-children alternative:
  ;; Self-closing: JsxOpen JsxName jsxAttributes? JsxOpeningSlashEnd
  ;; With-children: JsxOpen JsxName jsxAttributes? JsxOpeningEnd
  ;;                  jsxChildren? JsxClose JsxClosingName JsxClosingEnd

  ;; Extract tag name (second child, a JsxName token)
  (define tag-name-tok (second kids))
  (define tag-str (tk-value tag-name-tok))
  (define tag-sym (string->symbol tag-str))
  
  ;; Is tag a component (uppercase first char) or HTML element?
  ;; Special case: Link component → <a> with hash-based href + onclick
  (define (is-link? tag-name) (string=? tag-name "Link"))
  
  (define tag-name-uir
    (cond [(is-link? tag-str) (uir-string "a")]
          [(eq? tag-sym 'Image) (uir-string "img")]  ;; B34: next/image → <img>
          [(eq? tag-sym 'Head) (uir-string "head")]   ;; B34: next/head → placeholder
          [(eq? tag-sym 'ErrorPage)                   ;; B59: next/error → error div
           (uir-string "div")]
          [(and (char-upper-case? (string-ref tag-str 0))
                (string-suffix? tag-str ".Provider"))
           ;; Provider component: will be handled below, not as a regular component
           'provider]
          [(char-upper-case? (string-ref tag-str 0))
           (uir-symbol tag-sym)]   ;; component: React variable reference
          [else (uir-string tag-str)])) ;; HTML element: string literal
  
  ;; Extract attributes (third child is jsxAttributes node or 'none)
  (define attrs-node (third kids))
  (define raw-attrs
    (if (cst-node? attrs-node)
        (lower-jsx-attrs attrs-node tk-type tk-value)
        '()))
  
  ;; Transform Link attributes to <a> with hash-based href + onclick
  (define attrs
    (if (is-link? tag-str)
        (link-attrs->anchor-attrs raw-attrs)
        raw-attrs))
  
  ;; B59: ErrorPage → extract statusCode for error text child
  (define error-children
    (if (eq? tag-sym 'ErrorPage)
        (let ([status-code
               (for/or ([a (in-list attrs)])
                 (and (uir-attribute? a)
                      (eq? (uir-symbol-name (uir-attribute-name a)) 'statusCode)
                      (let ([v (uir-attribute-value a)])
                        (cond [(uir-number? v) (uir-number-value v)]
                              [(uir-jsx-expr? v) (uir-jsx-expr-content v)]
                              [else #f]))))])
          (list (uir-text-node (format "Error ~a" (if status-code status-code "404")))))
        #f))
  
  ;; Check if self-closing or with children
  (define maybe-slash (fourth kids))
  
  (cond [(eq? tag-name-uir 'provider)
         ;; <Context.Provider value={v}> children </Context.Provider>
         ;; → IIFE with push/pop on _provider stack
         (define ctx-name (string->symbol (substring tag-str 0 (- (string-length tag-str) 9))))
         (define value-attr
           (for/or ([a (in-list raw-attrs)])
             (and (uir-attribute? a)
                  (eq? (uir-symbol-name (uir-attribute-name a)) 'value)
                  a)))
         (define value-expr (if value-attr (uir-attribute-value value-attr) (uir-null)))
         (define provider-children
           (cond [(tok-type-eq maybe-slash tk-type 'JsxOpeningSlashEnd) '()]
                 [(tok-type-eq maybe-slash tk-type 'JsxOpeningEnd)
                  (define children-node (fifth kids))
                  (if (cst-node? children-node)
                      (lower-jsx-children children-node tk-type tk-value)
                      '())]
                 [else '()]))
         (define child-expr
           (cond [(null? provider-children) (uir-null)]
                 [(null? (cdr provider-children)) (car provider-children)]
                 [else (uir-list provider-children)]))
         (uir-call
          (uir-fn #f '()
            (uir-block
             (list
              (uir-call (uir-call (uir-symbol "dot")
                                  (list (uir-call (uir-symbol "dot")
                                                  (list (uir-symbol ctx-name) (uir-symbol "_provider")))
                                        (uir-symbol "push")))
                        (list value-expr))
              (uir-call (uir-symbol "var")
                        (list (uir-set! (uir-symbol "_r") child-expr)))
              (uir-call (uir-call (uir-symbol "dot")
                                  (list (uir-call (uir-symbol "dot")
                                                  (list (uir-symbol ctx-name) (uir-symbol "_provider")))
                                        (uir-symbol "pop")))
                        '())
              (uir-return (uir-symbol "_r"))))
            #f)
          '())]
        
        [(tok-type-eq maybe-slash tk-type 'JsxOpeningSlashEnd)
         ;; Self-closing: <div/>
         (uir-element tag-name-uir attrs (or error-children '()) '())]
        
         [(tok-type-eq maybe-slash tk-type 'JsxOpeningEnd)
          ;; With children (maybe empty)
          (define children-node (fifth kids))
          (define children
            (let ([raw (if (cst-node? children-node)
                           (lower-jsx-children children-node tk-type tk-value)
                           '())])
              (if error-children
                  (append error-children raw)
                  raw)))
         ;; B31: For components, pass nested JSX children as a 'children' prop
         (define component-attrs
           (if (and (uir-symbol? tag-name-uir) (not (null? children)))
               (let* ([has-children-prop?
                       (for/or ([a (in-list attrs)])
                         (and (uir-attribute? a)
                              (eq? (uir-symbol-name (uir-attribute-name a)) 'children)))]
                      [final-attrs
                       (if has-children-prop?
                           attrs  ;; explicit children prop wins
                           (let ([children-val (if (= (length children) 1)
                                                   (first children)
                                                   (uir-list children))])
                             (append attrs (list (uir-attribute (uir-symbol 'children)
                                                                children-val)))))])
                 final-attrs)
               attrs))
         (uir-element tag-name-uir component-attrs children '())]
        
        [else (error 'lower-jsx-element "unexpected structure: ~e" kids)]))

;; jsxAttributes → (listof uir-attribute)
(define (lower-jsx-attrs node-or-list tk-type tk-value)
  ;; jsxAttributes wraps a list of jsxAttribute nodes
  (define node (unwrap node-or-list))
  (define attr-nodes (flat-kids node))
  (for/list ([an (in-list attr-nodes)])
    (lower-jsx-attr an tk-type tk-value)))

;; jsxAttribute → uir-attribute
(define (lower-jsx-attr node-or-list tk-type tk-value)
  (define node (unwrap node-or-list))
  (define kids (kids-of node))
  ;; jsxAttribute: JsxName (JsxAssign jsxAttributeValue)?
  (define name-tok (first kids))
  (define name-str (tk-value name-tok))
  (define name-uir (uir-symbol (string->symbol name-str)))
  
  (define value-node
    (for/or ([k (kids-of node)] #:when (cst-node? k)) k))
  
  (define value-uir
    (if value-node
        (lower-jsx-attr-value value-node tk-type tk-value)
        (uir-bool #t))) ;; boolean attribute: just presence
  
  (uir-attribute name-uir value-uir))

;; jsxAttributeValue → uir-string or uir-jsx-expr
(define (lower-jsx-attr-value node-or-list tk-type tk-value)
  (define node (unwrap node-or-list))
  ;; Handle 'group' wrapper (JsxAssign actualValue) from grammar
  (define actual-node
    (if (and (cst-node? node) (eq? (tag-of node) 'group))
        (for/or ([k (in-list (kids-of node))] #:when (cst-node? k)) k)
        node))
  
  (define kids (kids-of actual-node))
  
  ;; Search kids for JsxString (literal string value) or jsxExpression
  (define str-tok
    (for/or ([ch (in-list kids)] #:when (tok? ch tk-type))
      (and (eq? (tk-type ch) 'JsxString) ch)))
  
  (define expr-node
    (for/or ([ch (in-list kids)] #:when (cst-node? ch))
      (and (eq? (tag-of ch) 'jsxExpression) ch)))
  
  (cond [str-tok
         (define raw (tk-value str-tok))
         (define unquoted (substring raw 1 (sub1 (string-length raw))))
         (uir-string unquoted)]
        [expr-node
         (lower-jsx-expr expr-node tk-type tk-value)]
        [else (error 'lower-jsx-attr-value "unexpected attr value: ~e" kids)]))

;; jsxChildren → (listof uir-text-node | uir-jsx-expr | uir-element)
(define (lower-jsx-children node-or-list tk-type tk-value)
  (define node (unwrap node-or-list))
  (define child-nodes (flat-kids node))
  (for/list ([cn (in-list child-nodes)])
    (lower-jsx-child cn tk-type tk-value)))

;; jsxChild → uir-text-node | uir-jsx-expr | uir-element
(define (lower-jsx-child node-or-list tk-type tk-value)
  (define n (if (pair? node-or-list) (first node-or-list) node-or-list))
  
  ;; Search children for jsxExpression, jsxElement, or HtmlChardata
  (define kids (kids-of n))
  
  (define expr-node
    (for/or ([ch (in-list kids)] #:when (cst-node? ch))
      (and (memq (tag-of ch) '(jsxExpression jsxElement)) ch)))
  
  (define text-tok
    (for/or ([ch (in-list kids)] #:when (tok? ch tk-type))
      (and (eq? (tk-type ch) 'HtmlChardata) ch)))
  
  (cond [text-tok
         (uir-text-node (uir-string (tk-value text-tok)))]
        [(and expr-node (eq? (tag-of expr-node) 'jsxExpression))
         (lower-jsx-expr expr-node tk-type tk-value)]
        [(and expr-node (eq? (tag-of expr-node) 'jsxElement))
         (lower-jsx-element expr-node tk-type tk-value)]
        [else (error 'lower-jsx-child "unexpected children: ~e" kids)]))

;; jsxExpression → uir-jsx-expr (raw expression text)
(define (lower-jsx-expr node tk-type tk-value)
  ;; Extract all expression tokens and concatenate, including braces
  ;; that are part of the JS expression (object literals, blocks, etc).
  (define text-parts '())
  (let walk ([n node])
    (cond [(cst-node? n)
           (for ([ch (kids-of n)])
             (walk ch))]
          [(pair? n)
           (for ([ch n]) (walk ch))]
          [(or (tok-type-eq n tk-type 'ExpressionText)
               (tok-type-eq n tk-type 'ExpressionOpenBrace)
               (tok-type-eq n tk-type 'ExpressionCloseBrace))
           (set! text-parts (cons (tk-value n) text-parts))]))
  (uir-jsx-expr (string-join (reverse text-parts) "")))
