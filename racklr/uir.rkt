#lang racket

(require racklr/tree)

(provide
 ;; Tier 0: Core computational IR
 uir-null uir-null?
 uir-bool uir-bool? uir-bool-value
 uir-number uir-number? uir-number-value
 uir-string uir-string? uir-string-value
 uir-fstring uir-fstring? uir-fstring-value
 uir-list uir-list? uir-list-items
 uir-record uir-record? uir-record-entries
 uir-symbol uir-symbol? uir-symbol-name
 ;; Functions and control flow
  uir-fn uir-fn? uir-fn-name uir-fn-params uir-fn-body
  uir-fn-return-type
  uir-typed-param uir-typed-param? uir-typed-param-name uir-typed-param-type uir-typed-param-default
 uir-call uir-call? uir-call-callee uir-call-args
 uir-let uir-let? uir-let-name uir-let-value uir-let-body
  uir-var uir-var? uir-var-name
  uir-set! uir-set!? uir-set!-name uir-set!-value
  uir-ann-set! uir-ann-set!? uir-ann-set!-lhs uir-ann-set!-type uir-ann-set!-value
  uir-if uir-if? uir-if-test uir-if-then uir-if-else
 uir-block uir-block? uir-block-stmts
 uir-return uir-return? uir-return-value
  uir-for-each uir-for-each? uir-for-each-var uir-for-each-iterable uir-for-each-body uir-for-each-else-body
  uir-while uir-while? uir-while-test uir-while-body uir-while-else-body
  uir-try uir-try? uir-try-body uir-try-catches uir-try-else-body uir-try-finally-body
  uir-with uir-with? uir-with-items uir-with-body
  uir-await uir-await? uir-await-expr
  uir-yield uir-yield? uir-yield-value uir-yield-from?
  uir-decorated uir-decorated? uir-decorated-decorators uir-decorated-inner
  uir-get uir-get? uir-get-base uir-get-field
  uir-paren uir-paren? uir-paren-inner
  ;; Pattern matching (match/case)
  uir-match uir-match? uir-match-subject uir-match-cases
  uir-case uir-case? uir-case-pattern uir-case-guard uir-case-body
  uir-pat-literal uir-pat-literal? uir-pat-literal-value
  uir-pat-capture uir-pat-capture? uir-pat-capture-name
  uir-pat-wildcard uir-pat-wildcard?
  uir-pat-value uir-pat-value? uir-pat-value-path
  uir-pat-or uir-pat-or? uir-pat-or-alts
  uir-pat-as uir-pat-as? uir-pat-as-pattern uir-pat-as-name
  uir-pat-sequence uir-pat-sequence? uir-pat-sequence-elements
  uir-pat-star uir-pat-star? uir-pat-star-name
  uir-pat-mapping uir-pat-mapping? uir-pat-mapping-entries uir-pat-mapping-rest
  uir-pat-double-star uir-pat-double-star? uir-pat-double-star-name
  uir-pat-class uir-pat-class? uir-pat-class-cls uir-pat-class-positional uir-pat-class-keyword
  uir-pat-group uir-pat-group? uir-pat-group-pattern
  ;; Tier 1: OOP / Module IR
 uir-class uir-class? uir-class-name uir-class-super uir-class-fields uir-class-methods
 uir-method uir-method? uir-method-name uir-method-params uir-method-body uir-method-visibility
 uir-field uir-field? uir-field-name uir-field-type uir-field-init
 uir-new uir-new? uir-new-class uir-new-args
 uir-interface uir-interface? uir-interface-name uir-interface-methods
 uir-module uir-module? uir-module-name uir-module-imports uir-module-exports uir-module-body
 uir-import uir-import? uir-import-source uir-import-names
 uir-export uir-export? uir-export-names
 ;; Tier 2: UI Component IR
 uir-component uir-component? uir-component-name uir-component-props uir-component-states uir-component-effects uir-component-template
 uir-element uir-element? uir-element-tag uir-element-attrs uir-element-children uir-element-events
 uir-attribute uir-attribute? uir-attribute-name uir-attribute-value
 uir-event uir-event? uir-event-name uir-event-handler
 uir-slot uir-slot? uir-slot-name uir-slot-fallback
 uir-text-node uir-text-node? uir-text-node-content
 uir-style uir-style? uir-style-styles
 uir-effect uir-effect? uir-effect-deps uir-effect-body
 uir-state uir-state? uir-state-name uir-state-init
 uir-jsx-expr uir-jsx-expr? uir-jsx-expr-content
 ;; Predicate
 uir? uir-tag
 ;; Serialization
 uir->sexp sexp->uir
 uir->json json->uir)

;; ── Tier 0: Core Computational IR ──────────────────────────────────

;; Literals
(struct uir-null () #:transparent)
(struct uir-bool (value) #:transparent)
(struct uir-number (value) #:transparent)  ;; string to preserve precision
(struct uir-string (value) #:transparent)
(struct uir-fstring (value) #:transparent)  ;; f-string literal

;; Data structures
(struct uir-list (items) #:transparent)    ;; list of uir?
(struct uir-record (entries) #:transparent) ;; list of (cons uir-string uir?)

;; Identifiers (for variable references, function names, field keys)
(struct uir-symbol (name) #:transparent)

;; Functions and control flow
(struct uir-fn (name params body return-type) #:transparent)  ;; name: #f or uir-symbol, return-type: #f or uir?
(struct uir-typed-param (name type default) #:transparent)   ;; name: uir-symbol, type/default: #f or uir?
(struct uir-call (callee args) #:transparent)    ;; callee: uir?, args: (listof uir?)
(struct uir-let (name value body) #:transparent) ;; name: uir-symbol, value: uir?, body: uir?
(struct uir-var (name) #:transparent)            ;; name: uir-symbol
(struct uir-set! (name value) #:transparent)     ;; name: uir-symbol, value: uir?
;; Annotated assignment: x: int = 5 or x: int (no value)
(struct uir-ann-set! (lhs type value) #:transparent)  ;; value can be #f for annotation-only
(struct uir-if (test then else) #:transparent)   ;; test/then/else: all uir?
(struct uir-block (stmts) #:transparent)         ;; stmts: (listof uir?)
(struct uir-return (value) #:transparent)        ;; value: uir?

;; For-each loop: for var in iterable: body (else: else-body)
;; var: uir-symbol, iterable: uir?, body: uir?, else-body: uir? (uir-null if no else)
(struct uir-for-each (var iterable body else-body) #:transparent)

;; While loop: test, body, optional else-body (uir-null if no else)
(struct uir-while (test body else-body) #:transparent)

;; Try/catch: try body, catch clauses, optional else and finally
;; catches: (listof (list exception-type exception-name body))
;;   exception-type: uir? (uir-null for bare except)
;;   exception-name: uir-symbol or uir-null
;; else-body/finally-body: uir? or #f (#f means not specified, uir-null means pass)
(struct uir-try (body catches else-body finally-body) #:transparent)

;; With statement: context managers + body
;; items: (listof (list context-expr as-name)) where as-name is uir-symbol or uir-null
(struct uir-with (items body) #:transparent)

;; Await expression: await expr
(struct uir-await (expr) #:transparent)

;; Yield expression: yield value (or yield from value)
;; from?: #t for 'yield from', #f for 'yield'
(struct uir-yield (value from?) #:transparent)

;; Decorated definition: @deco\ndef foo(): ...
;; decorators: (listof uir-call) — each decorator is a call expression
;; inner: uir? — the function or class being decorated
(struct uir-decorated (decorators inner) #:transparent)

;; Property access or subscript: base.field or base[key]
;; base: uir? — the object being accessed
;; field: uir? — uir-string for .attr, any uir for [key]
(struct uir-get (base field) #:transparent)

;; Parenthesized expression — preserves explicit grouping for round-trip
(struct uir-paren (inner) #:transparent)

;; ── Match/Case (Python 3.10+) ────────────────────────────────────

;; Match statement: match subject: case pattern [if guard]: body ...
(struct uir-match (subject cases) #:transparent)     ;; subject: uir?, cases: (listof uir-case)
(struct uir-case (pattern guard body) #:transparent)  ;; pattern: uir-pat?, guard: #f or uir?, body: uir?

;; Pattern IR — represents destructuring patterns in match/case

;; Literal pattern: case 42: / case "hello": / case None: / case True:
(struct uir-pat-literal (value) #:transparent)  ;; value: uir? (number, string, boolean, None as uir-symbol)

;; Capture pattern: case x: (binds a variable)
(struct uir-pat-capture (name) #:transparent)   ;; name: uir-symbol

;; Wildcard pattern: case _:
(struct uir-pat-wildcard () #:transparent)

;; Value pattern: case SomeClass.ATTR: (dotted name lookup)
(struct uir-pat-value (path) #:transparent)     ;; path: uir-symbol or uir-get chain

;; OR pattern: case 1 | 2 | 3:
(struct uir-pat-or (alts) #:transparent)        ;; alts: (listof uir-pat?)

;; AS pattern: case (pat) as name:
(struct uir-pat-as (pattern name) #:transparent) ;; pattern: uir-pat?, name: uir-symbol

;; Sequence pattern: case [a, b, *rest]: / case (a, b):
(struct uir-pat-sequence (elements) #:transparent) ;; elements: (listof uir-pat?, may include uir-pat-star)

;; Star pattern: *name or *_
(struct uir-pat-star (name) #:transparent)       ;; name: uir-symbol or #f for wildcard star

;; Mapping pattern: case {key: val, **rest}:
(struct uir-pat-mapping (entries rest) #:transparent) ;; entries: (listof (cons uir-pat? uir-pat?)), rest: #f or uir-pat-double-star

;; Double-star pattern: **name
(struct uir-pat-double-star (name) #:transparent)     ;; name: uir-symbol

;; Class pattern: case ClassName(pos1, pos2, key=val):
(struct uir-pat-class (cls positional keyword) #:transparent) ;; cls: uir-symbol or uir-get, positional: (listof uir-pat?), keyword: (listof (cons uir-symbol uir-pat?))

;; Group pattern: case (pattern):
(struct uir-pat-group (pattern) #:transparent)

;; ── Tier 1: OOP / Module IR ───────────────────────────────────────

;; Class definition: name, optional superclass, fields, methods
;; fields: (listof uir-field), methods: (listof uir-method)
(struct uir-class (name super fields methods) #:transparent)

;; Method: name, params, body, visibility
;; visibility: 'public, 'private, or 'protected (Racket symbol)
(struct uir-method (name params body visibility) #:transparent)

;; Field declaration: name, optional type annotation, initial value (or uir-null)
(struct uir-field (name type init) #:transparent)

;; Object instantiation: class-name, constructor args
(struct uir-new (class args) #:transparent)

;; Interface definition: name, abstract method signatures
(struct uir-interface (name methods) #:transparent)

;; Module: name, imports, exports, body
(struct uir-module (name imports exports body) #:transparent)

;; Import: source module, imported names (or (uir-symbol "*") for wildcard)
(struct uir-import (source names) #:transparent)

;; Export: exported names (or (uir-symbol "*") for re-export-all)
(struct uir-export (names) #:transparent)

;; ── Tier 2: UI Component IR ───────────────────────────────────────

;; Component: name, props, reactive states, effects, template
;; props/states/effects: list of their respective UIR types
;; template: uir-element (the root render tree)
(struct uir-component (name props states effects template) #:transparent)

;; DOM element: tag name, attributes, children, event bindings
;; attrs: (listof uir-attribute), children: (listof uir-element or uir-text-node or uir-slot)
;; events: (listof uir-event)
(struct uir-element (tag attrs children events) #:transparent)

;; HTML attribute: name, static or dynamic value (uir-string or uir-expression)
(struct uir-attribute (name value) #:transparent)

;; Event binding: e.g., (uir-event "click" (uir-fn ...)) 
(struct uir-event (name handler) #:transparent)

;; Slot for component children: name and optional fallback content
(struct uir-slot (name fallback) #:transparent)

;; Plain text content in the DOM
(struct uir-text-node (content) #:transparent)

;; Inline or referenced styles: styles as (listof (cons uir-string uir?))
;; This mirrors uir-record for CSS key-value pairs
(struct uir-style (styles) #:transparent)

;; Reactive effect: dependencies and body (like React useEffect)
;; deps: list of state references, body: uir-fn or uir-block
(struct uir-effect (deps body) #:transparent)

;; Reactive state cell: name and initial value
(struct uir-state (name init) #:transparent)

;; JSX embedded expression {expression-content} — raw text passthrough
;; Used when the lowering pass captures opaque expression text from JSX
(struct uir-jsx-expr (content) #:transparent)

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
                          'fn 'call 'let 'var 'set! 'if 'block 'return 'for-each 'try 'with 'await 'yield 'decorated 'get
                          'class 'method 'field 'new 'interface 'module 'import 'export
                          'component 'element 'attribute 'event 'slot 'text-node 'style 'effect 'state 'jsx-expr))

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
