#lang racket

(require "uir/types.rkt")
(require racklr/tree)

(provide
 (all-from-out "uir/types.rkt")
 ;; Predicate
 uir? uir-tag
 ;; Serialization
 uir->sexp sexp->uir
 uir->json json->uir)

;; ── Predicate ──────────────────────────────────────────────────────

(define (uir? v)
  (or (uir-null? v)
      (uir-bool? v)
      (uir-number? v)
      (uir-string? v)
      (uir-fstring? v)
      (uir-list? v)
      (uir-record? v)
      (uir-symbol? v)
      (uir-fn? v)
      (uir-typed-param? v)
      (uir-call? v)
      (uir-let? v)
      (uir-var? v)
      (uir-set!? v)
      (uir-ann-set!? v)
      (uir-if? v)
      (uir-block? v)
      (uir-return? v)
      (uir-for-each? v)
      (uir-while? v)
      (uir-try? v)
      (uir-with? v)
      (uir-await? v)
      (uir-yield? v)
      (uir-decorated? v)
      (uir-get? v)
      (uir-paren? v)
      (uir-class? v)
      (uir-method? v)
      (uir-field? v)
      (uir-new? v)
      (uir-interface? v)
      (uir-module? v)
      (uir-import? v)
      (uir-export? v)
      (uir-enum? v)
      (uir-enum-variant? v)
      (uir-component? v)
      (uir-element? v)
      (uir-attribute? v)
      (uir-event? v)
      (uir-slot? v)
      (uir-text-node? v)
      (uir-style? v)
      (uir-effect? v)
      (uir-state? v)
      (uir-jsx-expr? v)
      (uir-match? v)
      (uir-case? v)
      (uir-pat-literal? v)
      (uir-pat-capture? v)
      (uir-pat-wildcard? v)
      (uir-pat-value? v)
      (uir-pat-or? v)
      (uir-pat-as? v)
      (uir-pat-sequence? v)
      (uir-pat-star? v)
      (uir-pat-mapping? v)
      (uir-pat-double-star? v)
      (uir-pat-class? v)
      (uir-pat-group? v)))

(define (uir-tag v)
  (cond [(uir-null? v)   'null]
        [(uir-bool? v)   'bool]
        [(uir-number? v) 'number]
        [(uir-string? v) 'string]
        [(uir-fstring? v) 'fstring]
        [(uir-list? v)   'list]
        [(uir-record? v) 'record]
        [(uir-symbol? v) 'symbol]
        [(uir-fn? v)     'fn]
        [(uir-typed-param? v) 'typed-param]
        [(uir-call? v)   'call]
        [(uir-let? v)    'let]
        [(uir-var? v)    'var]
        [(uir-set!? v)  'set!]
        [(uir-ann-set!? v) 'ann-set!]
        [(uir-if? v)     'if]
        [(uir-block? v)  'block]
        [(uir-return? v) 'return]
        [(uir-for-each? v) 'for-each]
        [(uir-while? v) 'while]
        [(uir-try? v)    'try]
        [(uir-with? v)   'with]
        [(uir-await? v)  'await]
        [(uir-yield? v)  'yield]
        [(uir-decorated? v) 'decorated]
        [(uir-get? v) 'get]
        [(uir-paren? v) 'paren]
        [(uir-class? v)     'class]
        [(uir-method? v)    'method]
        [(uir-field? v)     'field]
        [(uir-new? v)       'new]
        [(uir-interface? v) 'interface]
        [(uir-module? v)    'module]
        [(uir-import? v)    'import]
        [(uir-export? v)    'export]
        [(uir-enum? v)         'enum]
        [(uir-enum-variant? v) 'enum-variant]
        [(uir-component? v)  'component]
        [(uir-element? v)    'element]
        [(uir-attribute? v)  'attribute]
        [(uir-event? v)      'event]
        [(uir-slot? v)       'slot]
        [(uir-text-node? v)  'text-node]
        [(uir-style? v)      'style]
        [(uir-effect? v)     'effect]
        [(uir-state? v)      'state]
        [(uir-jsx-expr? v)  'jsx-expr]
        [(uir-match? v)          'match]
        [(uir-case? v)           'case]
        [(uir-pat-literal? v)    'pat-literal]
        [(uir-pat-capture? v)    'pat-capture]
        [(uir-pat-wildcard? v)   'pat-wildcard]
        [(uir-pat-value? v)      'pat-value]
        [(uir-pat-or? v)         'pat-or]
        [(uir-pat-as? v)         'pat-as]
        [(uir-pat-sequence? v)   'pat-sequence]
        [(uir-pat-star? v)       'pat-star]
        [(uir-pat-mapping? v)    'pat-mapping]
        [(uir-pat-double-star? v) 'pat-double-star]
        [(uir-pat-class? v)      'pat-class]
        [(uir-pat-group? v)      'pat-group]
        [else (error 'uir-tag "not a UIR node: ~e" v)]))

;; ── Serialization ──────────────────────────────────────────────────

(define (uir->sexp v)
  (cond [(uir-null? v) '(null)]
        [(uir-bool? v) `(bool ,(uir-bool-value v))]
        [(uir-number? v) `(number ,(uir-number-value v))]
        [(uir-string? v) `(string ,(uir-string-value v))]
        [(uir-fstring? v) `(fstring ,(uir-fstring-value v))]
        [(uir-list? v) `(list ,@(map uir->sexp (uir-list-items v)))]
        [(uir-record? v)
         `(record ,@(map (lambda (e)
                          (list (uir->sexp (car e))
                                (uir->sexp (cdr e))))
                        (uir-record-entries v)))]
        [(uir-symbol? v) `(symbol ,(uir-symbol-name v))]
        [(uir-fn? v) `(fn ,(let ([n (uir-fn-name v)]) (if n (uir->sexp n) #f))
                          ,(map uir->sexp (uir-fn-params v))
                          ,(uir->sexp (uir-fn-body v))
                          ,(let ([rt (uir-fn-return-type v)]) (if rt (uir->sexp rt) #f)))]
        [(uir-typed-param? v) `(typed-param ,(uir->sexp (uir-typed-param-name v))
                                           ,(uir->sexp (uir-typed-param-type v))
                                           ,(uir->sexp (uir-typed-param-default v)))]
        [(uir-call? v) `(call ,(uir->sexp (uir-call-callee v))
                              ,(map uir->sexp (uir-call-args v)))]
        [(uir-let? v) `(let ,(uir->sexp (uir-let-name v))
                            ,(uir->sexp (uir-let-value v))
                            ,(uir->sexp (uir-let-body v)))]
        [(uir-var? v) `(var ,(uir->sexp (uir-var-name v)))]
        [(uir-set!? v) `(set! ,(uir->sexp (uir-set!-name v))
                              ,(uir->sexp (uir-set!-value v)))]
        [(uir-ann-set!? v) `(ann-set! ,(uir->sexp (uir-ann-set!-lhs v))
                                      ,(uir->sexp (uir-ann-set!-type v))
                                      ,(uir->sexp (uir-ann-set!-value v)))]
        [(uir-if? v) `(if ,(uir->sexp (uir-if-test v))
                          ,(uir->sexp (uir-if-then v))
                          ,(uir->sexp (uir-if-else v)))]
        [(uir-block? v) `(block ,(map uir->sexp (uir-block-stmts v)))]
        [(uir-return? v) `(return ,(uir->sexp (uir-return-value v)))]
        [(uir-for-each? v) `(for-each ,(uir->sexp (uir-for-each-var v))
                                      ,(uir->sexp (uir-for-each-iterable v))
                                      ,(uir->sexp (uir-for-each-body v))
                                      ,(uir->sexp (uir-for-each-else-body v)))]
        [(uir-while? v) `(while ,(uir->sexp (uir-while-test v))
                                ,(uir->sexp (uir-while-body v))
                                ,(uir->sexp (uir-while-else-body v)))]
        [(uir-try? v) `(try ,(uir->sexp (uir-try-body v))
                            ,(map (lambda (cat)
                                    (list (uir->sexp (car cat))
                                          (uir->sexp (cadr cat))
                                          (uir->sexp (caddr cat))))
                                  (uir-try-catches v))
                            ,(let ([eb (uir-try-else-body v)])
                               (if eb (uir->sexp eb) 'unspecified))
                            ,(let ([fb (uir-try-finally-body v)])
                               (if fb (uir->sexp fb) 'unspecified)))]
        [(uir-with? v) `(with ,(map (lambda (item)
                                      (list (uir->sexp (car item))
                                            (if (cadr item)
                                                (uir->sexp (cadr item))
                                                'unspecified)))
                                    (uir-with-items v))
                              ,(uir->sexp (uir-with-body v)))]
        [(uir-await? v) `(await ,(uir->sexp (uir-await-expr v)))]
        [(uir-yield? v) `(yield ,(uir->sexp (uir-yield-value v))
                                ,(uir-yield-from? v))]
        [(uir-decorated? v) `(decorated ,(map uir->sexp (uir-decorated-decorators v))
                                       ,(uir->sexp (uir-decorated-inner v)))]
        [(uir-get? v) `(get ,(uir->sexp (uir-get-base v))
                            ,(uir->sexp (uir-get-field v)))]
        [(uir-paren? v) `(paren ,(uir->sexp (uir-paren-inner v)))]
        [(uir-class? v) `(class ,(uir->sexp (uir-class-name v))
                                ,(uir->sexp (uir-class-super v))
                                ,(map uir->sexp (uir-class-fields v))
                                ,(map uir->sexp (uir-class-methods v)))]
        [(uir-method? v) `(method ,(uir->sexp (uir-method-name v))
                                  ,(map uir->sexp (uir-method-params v))
                                  ,(uir->sexp (uir-method-body v))
                                  ,(uir-method-visibility v))]
        [(uir-field? v) `(field ,(uir->sexp (uir-field-name v))
                                ,(uir->sexp (uir-field-type v))
                                ,(uir->sexp (uir-field-init v)))]
        [(uir-new? v) `(new ,(uir->sexp (uir-new-class v))
                            ,(map uir->sexp (uir-new-args v)))]
        [(uir-interface? v) `(interface ,(uir->sexp (uir-interface-name v))
                                        ,(map uir->sexp (uir-interface-methods v)))]
        [(uir-module? v) `(module ,(uir->sexp (uir-module-name v))
                                  ,(map uir->sexp (uir-module-imports v))
                                  ,(map uir->sexp (uir-module-exports v))
                                  ,(uir->sexp (uir-module-body v)))]
        [(uir-import? v) `(import ,(uir->sexp (uir-import-source v))
                                  ,(map uir->sexp (uir-import-names v)))]
        [(uir-export? v) `(export ,(map uir->sexp (uir-export-names v)))]
        [(uir-enum? v) `(enum ,(uir->sexp (uir-enum-name v))
                              ,(map uir->sexp (uir-enum-variants v)))]
        [(uir-enum-variant? v) `(enum-variant ,(uir->sexp (uir-enum-variant-name v))
                                              ,(map uir->sexp (uir-enum-variant-fields v))
                                              ,(let ([d (uir-enum-variant-discriminant v)])
                                                 (if d (uir->sexp d) #f)))]
        [(uir-component? v) `(component ,(uir->sexp (uir-component-name v))
                                        ,(map uir->sexp (uir-component-props v))
                                        ,(map uir->sexp (uir-component-states v))
                                        ,(map uir->sexp (uir-component-effects v))
                                        ,(uir->sexp (uir-component-template v)))]
        [(uir-element? v) `(element ,(uir->sexp (uir-element-tag v))
                                    ,(map uir->sexp (uir-element-attrs v))
                                    ,(map uir->sexp (uir-element-children v))
                                    ,(map uir->sexp (uir-element-events v)))]
        [(uir-attribute? v) `(attribute ,(uir->sexp (uir-attribute-name v))
                                        ,(uir->sexp (uir-attribute-value v)))]
        [(uir-event? v) `(event ,(uir->sexp (uir-event-name v))
                                ,(uir->sexp (uir-event-handler v)))]
        [(uir-slot? v) `(slot ,(uir->sexp (uir-slot-name v))
                              ,(uir->sexp (uir-slot-fallback v)))]
        [(uir-text-node? v) `(text-node ,(uir-string-value (uir-text-node-content v)))]
        [(uir-style? v) `(style ,@(map (lambda (p)
                                        (list (uir->sexp (car p)) (uir->sexp (cdr p))))
                                      (uir-style-styles v)))]
        [(uir-effect? v) `(effect ,(map uir->sexp (uir-effect-deps v))
                                  ,(uir->sexp (uir-effect-body v)))]
        [(uir-state? v) `(state ,(uir->sexp (uir-state-name v))
                                ,(uir->sexp (uir-state-init v)))]
        [(uir-jsx-expr? v) `(jsx-expr ,(uir-jsx-expr-content v))]
        [else (error 'uir->sexp "not a UIR node: ~e" v)]))

(define (sexp->uir s)
  (match s
    ['(null) (uir-null)]
    [`(bool ,v) (uir-bool v)]
    [`(number ,v) (uir-number v)]
    [`(string ,v) (uir-string v)]
    [`(fstring ,v) (uir-fstring v)]
    [`(list ,xs ...) (uir-list (map sexp->uir xs))]
    [`(record ,es ...)
     (uir-record (map (lambda (e)
                       (match e
                         [`(,k ,v) (cons (sexp->uir k) (sexp->uir v))]
                         [_ (error 'sexp->uir "invalid record entry: ~e" e)]))
                     es))]
    [`(symbol ,n) (uir-symbol n)]
    [`(fn ,name ,ps ,body ,rt) (uir-fn (if name (sexp->uir name) #f) (map sexp->uir ps) (sexp->uir body) (if rt (sexp->uir rt) #f))]
    [`(typed-param ,name ,type ,default) (uir-typed-param (sexp->uir name) (sexp->uir type) (sexp->uir default))]
    [`(call ,c ,as) (uir-call (sexp->uir c) (map sexp->uir as))]
    [`(let ,n ,v ,body) (uir-let (sexp->uir n) (sexp->uir v) (sexp->uir body))]
    [`(var ,n) (uir-var (sexp->uir n))]
    [`(set! ,n ,v) (uir-set! (sexp->uir n) (sexp->uir v))]
    [`(ann-set! ,lhs ,type ,value) (uir-ann-set! (sexp->uir lhs) (sexp->uir type) (sexp->uir value))]
    [`(if ,tst ,thn ,els) (uir-if (sexp->uir tst) (sexp->uir thn) (sexp->uir els))]
    [`(block ,ss) (uir-block (map sexp->uir ss))]
    [`(return ,v) (uir-return (sexp->uir v))]
    [`(for-each ,var ,iter ,body ,else)
     (uir-for-each (sexp->uir var) (sexp->uir iter) (sexp->uir body) (sexp->uir else))]
    [`(try ,body ,catches ,else-body ,finally-body)
     (uir-try (sexp->uir body)
              (map (lambda (cat)
                     (match cat
                       [`(,et ,en ,b) (list (sexp->uir et) (sexp->uir en) (sexp->uir b))]
                       [_ (error 'sexp->uir "invalid catch: ~e" cat)]))
                   catches)
              (if (eq? else-body 'unspecified) #f (sexp->uir else-body))
              (if (eq? finally-body 'unspecified) #f (sexp->uir finally-body)))]
     [`(with ,items ,body)
      (uir-with (map (lambda (item)
                      (match item
                        [`(,ctx ,as-name)
                         (list (sexp->uir ctx)
                               (if (eq? as-name 'unspecified) #f (sexp->uir as-name)))]
                        [_ (error 'sexp->uir "invalid with-item: ~e" item)]))
                    items)
                (sexp->uir body))]
    [`(await ,expr) (uir-await (sexp->uir expr))]
    [`(yield ,value ,from?) (uir-yield (sexp->uir value) from?)]
    [`(decorated ,decos ,inner) (uir-decorated (map sexp->uir decos) (sexp->uir inner))]
    [`(get ,base ,field) (uir-get (sexp->uir base) (sexp->uir field))]
    [`(paren ,inner) (uir-paren (sexp->uir inner))]
    [`(class ,name ,super ,fields ,methods)
     (uir-class (sexp->uir name) (sexp->uir super)
                (map sexp->uir fields) (map sexp->uir methods))]
    [`(method ,name ,params ,body ,vis)
     (uir-method (sexp->uir name) (map sexp->uir params) (sexp->uir body) vis)]
    [`(field ,name ,type ,init)
     (uir-field (sexp->uir name) (sexp->uir type) (sexp->uir init))]
    [`(new ,cls ,args)
     (uir-new (sexp->uir cls) (map sexp->uir args))]
    [`(interface ,name ,methods)
     (uir-interface (sexp->uir name) (map sexp->uir methods))]
    [`(module ,name ,imports ,exports ,body)
     (uir-module (sexp->uir name) (map sexp->uir imports)
                 (map sexp->uir exports) (sexp->uir body))]
    [`(import ,source ,names)
     (uir-import (sexp->uir source) (map sexp->uir names))]
    [`(export ,names)
     (uir-export (map sexp->uir names))]
    [`(enum ,name ,variants)
     (uir-enum (sexp->uir name) (map sexp->uir variants))]
    [`(enum-variant ,name ,fields ,discriminant)
     (uir-enum-variant (sexp->uir name) (map sexp->uir fields)
                       (if discriminant (sexp->uir discriminant) #f))]
    [`(component ,name ,props ,states ,effects ,tmpl)
     (uir-component (sexp->uir name) (map sexp->uir props)
                    (map sexp->uir states) (map sexp->uir effects)
                    (sexp->uir tmpl))]
    [`(element ,tag ,attrs ,children ,events)
     (uir-element (sexp->uir tag) (map sexp->uir attrs)
                  (map sexp->uir children) (map sexp->uir events))]
    [`(attribute ,n ,v) (uir-attribute (sexp->uir n) (sexp->uir v))]
    [`(event ,n ,h) (uir-event (sexp->uir n) (sexp->uir h))]
    [`(slot ,n ,fb) (uir-slot (sexp->uir n) (sexp->uir fb))]
    [`(text-node ,txt) (uir-text-node (uir-string txt))]
    [`(style ,es ...)
     (uir-style (map (lambda (e)
                      (match e
                        [`(,k ,v) (cons (sexp->uir k) (sexp->uir v))]
                        [_ (error 'sexp->uir "invalid style entry: ~e" e)]))
                    es))]
    [`(effect ,ds ,body) (uir-effect (map sexp->uir ds) (sexp->uir body))]
    [`(state ,n ,init) (uir-state (sexp->uir n) (sexp->uir init))]
     [`(jsx-expr ,content) (uir-jsx-expr content)]
     [_ (error 'sexp->uir "invalid uir sexp: ~e" s)]))

(require json)

(define (uir-sexp->jsexpr s)
  (cond [(symbol? s) (symbol->string s)]
        [(list? s) (map uir-sexp->jsexpr s)]
        [(pair? s) (cons (uir-sexp->jsexpr (car s)) (uir-sexp->jsexpr (cdr s)))]
        [else s]))

(define uir-tag-set (set 'null 'bool 'number 'string 'fstring 'list 'record 'symbol
                          'fn 'call 'let 'var 'set! 'if 'block 'return 'for-each 'try 'with 'await 'yield 'decorated 'get 'paren
                          'class 'method 'field 'new 'interface 'module 'import 'export 'enum 'enum-variant
                          'component 'element 'attribute 'event 'slot 'text-node 'style 'effect 'state 'jsx-expr
                          'match 'case 'pat-literal 'pat-capture 'pat-wildcard 'pat-value 'pat-or 'pat-as
                          'pat-sequence 'pat-star 'pat-mapping 'pat-double-star 'pat-class 'pat-group))

(define (uir-jsexpr->sexp j)
  (match j
    [(? list? l)
     (if (and (pair? l) (string? (car l))
              (set-member? uir-tag-set (string->symbol (car l))))
         (cons (string->symbol (car l))
               (map uir-jsexpr->sexp (cdr l)))
         (map uir-jsexpr->sexp l))]
    [(? pair? p) (cons (uir-jsexpr->sexp (car p)) (uir-jsexpr->sexp (cdr p)))]
    [_ j]))

(define (uir->json v)
  (jsexpr->string (uir-sexp->jsexpr (uir->sexp v))))

(define (json->uir jstr)
  (sexp->uir (uir-jsexpr->sexp (string->jsexpr jstr))))

(module+ main
  (displayln "racklr/uir — Universal Intermediate Representation loaded."))
