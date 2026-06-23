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

;; jsxElement → uir-element
(define (lower-jsx-element node tk-type tk-value)
  (define kids (kids-of node))
  ;; kids layout depends on self-closing vs with-children alternative:
  ;; Self-closing: JsxOpen JsxName jsxAttributes? JsxOpeningSlashEnd
  ;; With-children: JsxOpen JsxName jsxAttributes? JsxOpeningEnd
  ;;                  jsxChildren? JsxClose JsxClosingName JsxClosingEnd

  ;; Extract tag name (second child, a JsxName token)
  (define tag-name-tok (second kids))
  (define tag-str (tk-value tag-name-tok))
  (define tag-sym (string->symbol tag-str))
  
  ;; Is tag a component (uppercase first char) or HTML element?
  (define tag-name-uir
    (if (char-upper-case? (string-ref tag-str 0))
        (uir-symbol tag-sym)   ;; component: React variable reference
        (uir-string tag-str))) ;; HTML element: string literal
  
  ;; Extract attributes (third child is jsxAttributes node or 'none)
  (define attrs-node (third kids))
  (define attrs
    (if (cst-node? attrs-node)
        (lower-jsx-attrs attrs-node tk-type tk-value)
        '()))
  
  ;; Check if self-closing or with children
  (define maybe-slash (fourth kids))
  
  (cond [(tok-type-eq maybe-slash tk-type 'JsxOpeningSlashEnd)
         ;; Self-closing: <div/>
         (uir-element tag-name-uir attrs '() '())]
        
        [(tok-type-eq maybe-slash tk-type 'JsxOpeningEnd)
         ;; With children (maybe empty)
         (define children-node (fifth kids))
         (define children
           (if (cst-node? children-node)
               (lower-jsx-children children-node tk-type tk-value)
               '()))
         (uir-element tag-name-uir attrs children '())]
        
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
  ;; Extract all ExpressionText tokens and concatenate
  (define text-parts '())
  (let walk ([n node])
    (cond [(cst-node? n)
           (for ([ch (kids-of n)])
             (walk ch))]
          [(pair? n)
           (for ([ch n]) (walk ch))]
          [(tok-type-eq n tk-type 'ExpressionText)
           (set! text-parts (cons (tk-value n) text-parts))]))
  (uir-jsx-expr (string-join (reverse text-parts) "")))
