#lang racket

(require racklr/uir
         "helpers.rkt"
         "extract.rkt"
         "embed.rkt"
         "conditionals.rkt")

(provide preprocess-tsx restore-jsx)

;; ── Public API ──────────────────────────────────────────────────────

(define (preprocess-tsx source-text
                        #:jsx-parse [jsx-parser #f]
                        #:jsx-lower-tk-type [tk-type #f]
                        #:jsx-lower-tk-value [tk-value #f])
  (define regions (find-all-jsx source-text))
  (define sorted-regions (sort regions > #:key first))
  
  (define jsx-map (make-hash))
  (define processed source-text)
  
  (for ([(region idx) (in-indexed sorted-regions)])
    (match-define (list start end jsx-str) region)
    (define idx-str
      (if (< idx 26)
          (string (integer->char (+ (char->integer #\a) idx)))
          (number->string idx)))
    (define sentinel (format "__JSX_~a__" idx-str))
    (hash-set! jsx-map sentinel jsx-str)
    (define before (substring processed 0 start))
    (define after (substring processed end (string-length processed)))
    (set! processed (string-append before sentinel after)))
  
  (define jsx-uir (make-hash))
  (when (and jsx-parser tk-type tk-value)
    ;; Lower each JSX string to UIR
    (for ([(sentinel jsx-src) (in-hash jsx-map)])
      (define trimmed (string-trim jsx-src))
      (define cst
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (jsx-parser trimmed)))
      (when cst
        (define lowered
          (with-handlers ([exn:fail? (lambda (e) #f)])
            ((dynamic-require 'racklr/lower-jsx 'lower-jsx)
             cst #:tk-type tk-type #:tk-value tk-value)))
        (when lowered
          ;; Process conditional JSX in expression children
          (define processed-cond (process-jsx-expr-conditionals lowered jsx-parser tk-type tk-value))
          (hash-set! jsx-uir sentinel processed-cond)))))
  
  (values processed jsx-map jsx-uir))

(define (restore-jsx uir jsx-uir-map)
  (let walk ([node uir])
    (match node
      [(? uir-var?)
       ;; Check if this var references a JSX sentinel
       (define inner (uir-var-name node))
       (if (and (uir-symbol? inner)
                (hash-has-key? jsx-uir-map (uir-symbol-name inner)))
           (hash-ref jsx-uir-map (uir-symbol-name inner))
           (struct-copy uir-var node [name (walk inner)]))]
      [(? uir-symbol?)
       (define name (uir-symbol-name node))
       (if (hash-has-key? jsx-uir-map name)
           (hash-ref jsx-uir-map name)
           node)]
      [(? uir-list?)
       (struct-copy uir-list node
                    [items (map walk (uir-list-items node))])]
      [(? uir-record?)
       (struct-copy uir-record node
                    [entries (for/list ([e (uir-record-entries node)])
                               (cons (car e) (walk (cdr e))))])]
      [(? uir-call?)
       (struct-copy uir-call node
                    [callee (walk (uir-call-callee node))]
                    [args (map walk (uir-call-args node))])]
      [(? uir-let?)
       (struct-copy uir-let node
                    [name (uir-let-name node)]
                    [value (walk (uir-let-value node))]
                    [body (walk (uir-let-body node))])]
      [(? uir-set!?)
       (struct-copy uir-set! node
                    [name (uir-set!-name node)]
                    [value (walk (uir-set!-value node))])]
      [(? uir-ann-set!?)
       (struct-copy uir-ann-set! node
                    [lhs (walk (uir-ann-set!-lhs node))]
                    [type (and (uir-ann-set!-type node) (walk (uir-ann-set!-type node)))]
                    [value (and (uir-ann-set!-value node) (walk (uir-ann-set!-value node)))])]
      [(? uir-if?)
       (struct-copy uir-if node
                    [test (walk (uir-if-test node))]
                    [then (walk (uir-if-then node))]
                    [else (walk (uir-if-else node))])]
      [(? uir-block?)
       (struct-copy uir-block node
                    [stmts (map walk (uir-block-stmts node))])]
      [(? uir-return?)
       (struct-copy uir-return node
                    [value (walk (uir-return-value node))])]
      [(? uir-for-each?)
       (struct-copy uir-for-each node
                    [var (uir-for-each-var node)]
                    [iterable (walk (uir-for-each-iterable node))]
                    [body (walk (uir-for-each-body node))]
                    [else-body (and (uir-for-each-else-body node) (walk (uir-for-each-else-body node)))])]
      [(? uir-while?)
       (struct-copy uir-while node
                    [test (walk (uir-while-test node))]
                    [body (walk (uir-while-body node))]
                    [else-body (and (uir-while-else-body node) (walk (uir-while-else-body node)))])]
      [(? uir-get?)
       (struct-copy uir-get node
                    [base (walk (uir-get-base node))]
                    [field (uir-get-field node)])]
      [(? uir-paren?)
       (struct-copy uir-paren node
                    [inner (walk (uir-paren-inner node))])]
      [(? uir-fn?)
       (struct-copy uir-fn node
                    [name (uir-fn-name node)]
                    [params (for/list ([p (uir-fn-params node)])
                              (walk p))]
                    [body (walk (uir-fn-body node))]
                     [return-type (and (uir-fn-return-type node) (walk (uir-fn-return-type node)))])]
      [(? uir-spread?)
       (uir-spread (walk (uir-spread-expr node)))]
      [_ node])))
