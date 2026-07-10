#lang racket

(require racklr/uir)

(provide lower-hooks)

;; ── UIR-based Hook Lowering ─────────────────────────────────────────
;; Walks UIR after TypeScript lowering and transforms React hook calls
;; into vanilla JavaScript equivalents.
;;
;; All hooks are handled here structurally (UIR). No regex preprocessing.

(define -dep-counter 0)

(define (lower-hooks uir)
  (set! -dep-counter 0)
  
  ;; Helper: emit ctx._provider.length > 0 ? ctx._provider[ctx._provider.length - 1] : ctx._default
  (define (make-use-context ctx-walked)
    (uir-if
     (uir-call (uir-symbol ">")
               (list (uir-call (uir-symbol "dot")
                               (list (uir-call (uir-symbol "dot")
                                               (list ctx-walked (uir-symbol "_provider")))
                                     (uir-symbol "length")))
                     (uir-number "0")))
     (uir-call (uir-symbol "index")
               (list (uir-call (uir-symbol "dot")
                               (list ctx-walked (uir-symbol "_provider")))
                     (uir-call (uir-symbol "-")
                               (list (uir-call (uir-symbol "dot")
                                               (list (uir-call (uir-symbol "dot")
                                                               (list ctx-walked (uir-symbol "_provider")))
                                                     (uir-symbol "length")))
                                     (uir-number "1")))))
     (uir-call (uir-symbol "dot")
               (list ctx-walked (uir-symbol "_default")))))
  
  ;; Helper: check if uir-symbol name (a symbol) ends with ".Provider"
  (define (provider-symbol? sym)
    (define s (symbol->string sym))
    (and (>= (string-length s) 9)
         (string-suffix? s ".Provider")))
  
  ;; Helper: transform <Context.Provider value={v}> children </Context.Provider>
  ;; into an IIFE that pushes v, renders children, pops, and returns result.
  (define (transform-provider elem walk)
    (define tag-sym (uir-symbol-name (uir-element-tag elem)))
    (define tag-str (symbol->string tag-sym))
    (define ctx-name (string->symbol (substring tag-str 0 (- (string-length tag-str) 9))))
    
    (define attrs (uir-element-attrs elem))
    (define value-uir
      (for/or ([attr attrs])
        (and (uir-attribute? attr)
             (eq? (uir-symbol-name (uir-attribute-name attr)) 'value)
             (walk (uir-attribute-value attr)))))
    
    (define walked-children (map walk (uir-element-children elem)))
    (define child-expr
      (cond [(null? walked-children) (uir-null)]
            [(null? (cdr walked-children)) (car walked-children)]
            [else (uir-list walked-children)]))
    
    ;; IIFE: (function() { ctx._provider.push(v); var _r = child; ctx._provider.pop(); return _r; })()
    (uir-call
     (uir-fn #f '()
       (uir-block
        (list
         (uir-call (uir-call (uir-symbol "dot")
                             (list (uir-call (uir-symbol "dot")
                                             (list (uir-symbol ctx-name) (uir-symbol "_provider")))
                                   (uir-symbol "push")))
                   (list (or value-uir (uir-null))))
         (uir-call (uir-symbol "var")
                   (list (uir-set! (uir-symbol "_r") child-expr)))
         (uir-call (uir-call (uir-symbol "dot")
                             (list (uir-call (uir-symbol "dot")
                                             (list (uir-symbol ctx-name) (uir-symbol "_provider")))
                                   (uir-symbol "pop")))
                   '())
         (uir-return (uir-symbol "_r")))))
     '()))
  
  (let walk ([node uir])
    (match node
    
    ;; ── const cb = useCallback(fn, deps) → const cb = fn ────────────────
    [(uir-call (uir-symbol "const")
               (list (uir-set! (? uir-symbol? name-uir)
                               (uir-call (uir-var (uir-symbol "useCallback"))
                                         (list callback-fn _)))))
     (uir-call (uir-symbol "const")
               (list (uir-set! name-uir (walk callback-fn))))]
    
    ;; ── const val = useMemo(() => expr, deps) → const val = expr ────────
    [(uir-call (uir-symbol "const")
               (list (uir-set! (? uir-symbol? name-uir)
                               (uir-call (uir-var (uir-symbol "useMemo"))
                                         (list (uir-call (uir-symbol "=>")
                                                         (list _ body-expr)) _)))))
     (uir-call (uir-symbol "const")
               (list (uir-set! name-uir (walk body-expr))))]
    
    ;; ── const ref = useRef(init) → let ref = { current: init } ──────────
    [(uir-call (uir-symbol "const")
               (list (uir-set! (? uir-symbol? name-uir)
                               (uir-call (uir-var (uir-symbol "useRef"))
                                         (list init)))))
     (uir-call (uir-symbol "let")
               (list (uir-set! name-uir
                               (uir-record (list (cons (uir-string "current") (walk init)))))))]

    ;; ── const [state, setState] = useState(init) ────────────────────────
    [(uir-call (uir-symbol "const")
               (list (uir-call (uir-symbol "array-bind")
                               (list (uir-list (list (? uir-symbol? state-name)
                                                     (? uir-symbol? setter-name)))
                                     (uir-call (uir-var (uir-symbol "useState"))
                                               (list init))))))
     (define state-key (string->symbol (format "_s_~a" (uir-symbol-name state-name))))
     (uir-block
      (list
       (uir-call (uir-symbol "var") (list (uir-var state-name)))
       (uir-if (uir-call (uir-symbol "!==")
                         (list (uir-call (uir-symbol "dot")
                                         (list (uir-symbol "window")
                                               (uir-symbol state-key)))
                               (uir-symbol "undefined")))
               (uir-set! state-name
                         (uir-call (uir-symbol "dot")
                                   (list (uir-symbol "window")
                                         (uir-symbol state-key))))
               (uir-block
                (list (uir-set! state-name init)
                      (uir-set! (uir-call (uir-symbol "dot")
                                          (list (uir-symbol "window")
                                                (uir-symbol state-key)))
                                init))))
       (uir-call (uir-symbol "var")
                 (list (uir-set! setter-name
                                 (uir-fn #f
                                         (list (uir-symbol "v"))
                                         (uir-block
                                          (list
                                           (uir-set! (uir-call (uir-symbol "dot")
                                                               (list (uir-symbol "window")
                                                                     (uir-symbol state-key)))
                                                     (uir-symbol "v"))
                                           (uir-if (uir-call (uir-symbol "dot")
                                                             (list (uir-symbol "window")
                                                                   (uir-symbol "_rerender")))
                                                   (uir-call (uir-call (uir-symbol "dot")
                                                                       (list (uir-symbol "window")
                                                                             (uir-symbol "_rerender")))
                                                             '())
                                                   (uir-null))))
                                         #f))))))]

    ;; ── useEffect(callback, deps) → IIFE + cleanup + deps diffing ────
    [(uir-call (uir-var (uir-symbol "useEffect"))
                (list callback deps))
     (set! -dep-counter (+ -dep-counter 1))
     (define dep-key (string->symbol (format "_deps_~a" -dep-counter)))
     (uir-block
      (list
       ;; Compare deps via JSON.stringify — cheap and works for primitives/arrays
       (uir-if (uir-call (uir-symbol "||")
                         (list (uir-call (uir-symbol "===")
                                        (list (uir-call (uir-symbol "dot")
                                                        (list (uir-symbol "window")
                                                              (uir-symbol dep-key)))
                                              (uir-symbol "undefined")))
                               (uir-call (uir-symbol "!==")
                                        (list (uir-call (uir-call (uir-symbol "dot")
                                                                  (list (uir-symbol "JSON")
                                                                        (uir-symbol "stringify")))
                                                        (list deps))
                                              (uir-call (uir-symbol "dot")
                                                        (list (uir-symbol "window")
                                                              (uir-symbol dep-key)))))))
               (uir-block
                (list
                 (uir-set! (uir-call (uir-symbol "dot")
                                     (list (uir-symbol "window")
                                           (uir-symbol dep-key)))
                           (uir-call (uir-call (uir-symbol "dot")
                                              (list (uir-symbol "JSON")
                                                    (uir-symbol "stringify")))
                                    (list deps)))
                 (uir-call (uir-symbol "var")
                           (list (uir-set! (uir-symbol "_fx")
                                           (uir-call callback '()))))
                 (uir-if (uir-call (uir-symbol "===")
                                   (list (uir-call (uir-symbol "typeof")
                                                   (list (uir-symbol "_fx")))
                                         (uir-string "function")))
                         (uir-block
                          (list
                           (uir-call (uir-call (uir-symbol "dot")
                                               (list (uir-call (uir-symbol "dot")
                                                               (list (uir-symbol "window")
                                                                     (uir-symbol "_cleanups")))
                                                     (uir-symbol "push")))
                                     (list (uir-symbol "_fx")))))
                         (uir-null))))
               (uir-null))))]

    ;; ── const ctx = createContext(default) → const ctx = { _default: default, _provider: [] }
    [(uir-call (uir-symbol "const")
               (list (uir-set! (? uir-symbol? name-uir)
                               (uir-call (uir-var (uir-symbol "createContext"))
                                         (list default-val)))))
     (uir-call (uir-symbol "const")
               (list (uir-set! name-uir
                               (uir-record (list (cons (uir-string "_default")
                                                       (walk default-val))
                                                 (cons (uir-string "_provider")
                                                       (uir-list '())))))))]

    ;; ── useContext(ctx) → check _provider stack, fallback to _default ──
    [(uir-call (uir-var (uir-symbol "useContext"))
               (list ctx-expr))
     (make-use-context (walk ctx-expr))]

    ;; ── const x = useContext(ctx) → const x = (check provider stack) ────
    [(uir-call (uir-symbol "const")
               (list (uir-set! (? uir-symbol? name-uir)
                               (uir-call (uir-var (uir-symbol "useContext"))
                                         (list ctx-expr)))))
     (uir-call (uir-symbol "const")
               (list (uir-set! name-uir
                               (make-use-context (walk ctx-expr)))))]

    ;; ── const [state, dispatch] = useReducer(reducer, init) ────────────
     [(uir-call (uir-symbol "const")
                (list (uir-call (uir-symbol "array-bind")
                                (list (uir-list (list (? uir-symbol? rd-state-name)
                                                      (? uir-symbol? dispatch-name)))
                                      (uir-call (uir-var (uir-symbol "useReducer"))
                                                (list reducer init))))))
      (uir-block
       (list
        (uir-call (uir-symbol "let")
                  (list (uir-set! rd-state-name init)))
        (uir-call (uir-symbol "let")
                  (list (uir-set! dispatch-name
                                  (uir-fn #f
                                          (list (uir-symbol "action"))
                                          (uir-block
                                           (list
                                            (uir-set! rd-state-name
                                                      (uir-call reducer
                                                                (list rd-state-name
                                                                      (uir-symbol "action"))))))
                                          #f))))))]

    ;; ══════════════════════════════════════════════════════════════════
    ;; ── React API stubs (B47) ────────────────────────────────────────
    ;; ══════════════════════════════════════════════════════════════════

    ;; ── const Comp = forwardRef(fn) → const Comp = fn ─────────────────
    [(uir-call (uir-symbol "const")
                (list (uir-set! (? uir-symbol? name-uir)
                                (uir-call (uir-var (uir-symbol "forwardRef"))
                                          (list render-fn)))))
      (uir-call (uir-symbol "const")
                (list (uir-set! name-uir (walk render-fn))))]

    ;; ── const Comp = memo(component) → const Comp = component ────────
    [(uir-call (uir-symbol "const")
                (list (uir-set! (? uir-symbol? name-uir)
                                (uir-call (uir-var (uir-symbol "memo"))
                                          (list component)))))
      (uir-call (uir-symbol "const")
                (list (uir-set! name-uir (walk component))))]

    ;; ── const Comp = lazy(loader) → const Comp = stub component ──────
    [(uir-call (uir-symbol "const")
                (list (uir-set! (? uir-symbol? name-uir)
                                (uir-call (uir-var (uir-symbol "lazy"))
                                          (list _loader)))))
      ;; Stub: return a component that renders a placeholder div
      (uir-call (uir-symbol "const")
                (list (uir-set! name-uir
                                (uir-fn #f
                                        (list (uir-symbol "props"))
                                        (uir-block
                                         (list
                                          (uir-return
                                           (uir-call (uir-symbol "document.createElement")
                                                     (list (uir-string "div"))))))
                                        #f))))]

    ;; ── Suspense: <Suspense fallback={...}>children</Suspense> ────────
    ;; In JSX lowering: (uir-call Suspense (list (uir-record ((fallback . fb))) children))
    ;; → just return children, ignore fallback
    [(uir-call (uir-var (uir-symbol "Suspense"))
                (list fallback-record children))
      (walk children)]

    ;; ── createPortal(children, container) → children (passthrough) ────
    [(uir-call (uir-var (uir-symbol "createPortal"))
                (list children _container))
      (walk children)]

    ;; ── Recurse into uir-block ──────────────────────────────────────
    [(? uir-block?)
     (struct-copy uir-block node
                  [stmts (map walk (uir-block-stmts node))])]
    
    ;; ── Recurse into uir-call args ──────────────────────────────────
    [(? uir-call?)
     (struct-copy uir-call node
                  [callee (walk (uir-call-callee node))]
                  [args (map walk (uir-call-args node))])]
    
    ;; ── Recurse into uir-if ─────────────────────────────────────────
    [(? uir-if?)
     (struct-copy uir-if node
                  [test (walk (uir-if-test node))]
                  [then (walk (uir-if-then node))]
                  [else (if (uir-if-else node) (walk (uir-if-else node)) #f)])]
    
    ;; ── Recurse into uir-return ─────────────────────────────────────
    [(? uir-return?)
     (struct-copy uir-return node
                  [value (walk (uir-return-value node))])]
    
    ;; ── Recurse into uir-set! ───────────────────────────────────────
    [(? uir-set!?)
     (struct-copy uir-set! node
                  [value (walk (uir-set!-value node))])]
    
    ;; ── Recurse into uir-ann-set! ───────────────────────────────────
    [(? uir-ann-set!?)
     (struct-copy uir-ann-set! node
                  [lhs (walk (uir-ann-set!-lhs node))]
                  [type (and (uir-ann-set!-type node) (walk (uir-ann-set!-type node)))]
                  [value (and (uir-ann-set!-value node) (walk (uir-ann-set!-value node)))])]
    
    ;; ── Recurse into uir-get ────────────────────────────────────────
    [(? uir-get?)
     (struct-copy uir-get node
                  [base (walk (uir-get-base node))])]
    
    ;; ── Recurse into uir-paren ──────────────────────────────────────
    [(? uir-paren?)
     (struct-copy uir-paren node
                  [inner (walk (uir-paren-inner node))])]
    
    ;; ── Recurse into uir-fn ─────────────────────────────────────────
    [(? uir-fn?)
     (struct-copy uir-fn node
                  [body (walk (uir-fn-body node))]
                  [return-type (and (uir-fn-return-type node) (walk (uir-fn-return-type node)))])]
    
    ;; ── Recurse into uir-list ───────────────────────────────────────
    [(? uir-list?)
     (struct-copy uir-list node
                  [items (map walk (uir-list-items node))])]
    
    ;; ── Recurse into uir-record ─────────────────────────────────────
    [(? uir-record?)
     (struct-copy uir-record node
                  [entries (for/list ([e (uir-record-entries node)])
                             (cons (car e) (walk (cdr e))))])]
    
    ;; ── Intercept Provider elements, recurse into normal elements ──
    [(? uir-element?)
     (define tag (uir-element-tag node))
     (if (and (uir-symbol? tag) (provider-symbol? (uir-symbol-name tag)))
         (transform-provider node walk)
         (struct-copy uir-element node
                      [children (map walk (uir-element-children node))]
                      [events (for/list ([ev (uir-element-events node)])
                                (struct-copy uir-event ev
                                             [handler (walk (uir-event-handler ev))]))]))]
    
    ;; ── Recurse into uir-for-each ───────────────────────────────────
    [(? uir-for-each?)
     (struct-copy uir-for-each node
                  [iterable (walk (uir-for-each-iterable node))]
                  [body (walk (uir-for-each-body node))]
                  [else-body (and (uir-for-each-else-body node) (walk (uir-for-each-else-body node)))])]
    
    ;; ── Recurse into uir-while ──────────────────────────────────────
    [(? uir-while?)
     (struct-copy uir-while node
                  [test (walk (uir-while-test node))]
                  [body (walk (uir-while-body node))]
                  [else-body (and (uir-while-else-body node) (walk (uir-while-else-body node)))])]
    
    ;; ── Recurse into uir-call (general: catches non-hook calls) ─────
    [(? uir-call?)
     (struct-copy uir-call node
                  [callee (walk (uir-call-callee node))]
                  [args (map walk (uir-call-args node))])]
    
    ;; ── Recurse into uir-block ──────────────────────────────────────
    [(? uir-block?)
     (struct-copy uir-block node
                  [stmts (map walk (uir-block-stmts node))])]
    
    ;; ── Recurse into uir-if ─────────────────────────────────────────
    [(? uir-if?)
     (struct-copy uir-if node
                  [test (walk (uir-if-test node))]
                  [then (walk (uir-if-then node))]
                  [else (walk (uir-if-else node))])]
    
    ;; ── Recurse into uir-return ─────────────────────────────────────
    [(? uir-return?)
     (struct-copy uir-return node
                  [value (walk (uir-return-value node))])]
    
    ;; ── Recurse into uir-set! ───────────────────────────────────────
    [(? uir-set!?)
     (struct-copy uir-set! node
                  [value (walk (uir-set!-value node))])]
    
    ;; ── Recurse into uir-let ────────────────────────────────────────
    [(? uir-let?)
     (struct-copy uir-let node
                  [value (walk (uir-let-value node))]
                  [body (walk (uir-let-body node))])]
    
    ;; ── Transform hook calls in JSX expression text ───────────────────
    [(? uir-jsx-expr?)
     (define expr-text (uir-jsx-expr-content node))
     (define transformed
       (regexp-replace* #rx"useContext\\(([A-Za-z_][A-Za-z0-9_]*)\\)"
                        expr-text
                        "\\1._provider.length > 0 ? \\1._provider[\\1._provider.length - 1] : \\1._default"))
     (struct-copy uir-jsx-expr node [content transformed])]
    
    ;; ── Recurse into uir-spread ──────────────────────────────────
    [(? uir-spread?)
     (uir-spread (walk (uir-spread-expr node)))]
    
    [_ node])))
