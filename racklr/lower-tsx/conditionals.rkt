#lang racket

(require racket/string
         racklr/uir)

(provide process-jsx-expr-conditionals)

;; ── Post-lowering: replace conditional uir-jsx-expr with uir-if ──────

(define (process-jsx-expr-conditionals uir jsx-parser tk-type tk-value)
  (define (lower-jsx-text text)
    (define trimmed (string-trim text))
    (define cst
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (jsx-parser trimmed)))
    (and cst
         (with-handlers ([exn:fail? (lambda (e) #f)])
           ((dynamic-require (quote racklr/lower-jsx) (quote lower-jsx))
            cst #:tk-type tk-type #:tk-value tk-value))))
  
  (define (walk node)
    (cond [(uir-element? node)
           (struct-copy uir-element node
                        [children (for/list ([child (in-list (uir-element-children node))])
                                    (walk child))]
                        [events (for/list ([ev (in-list (uir-element-events node))])
                                  (struct-copy uir-event ev
                                               [handler (walk (uir-event-handler ev))]))])]
          [(uir-if? node)
           (struct-copy uir-if node
                        [test (walk (uir-if-test node))]
                        [then (walk (uir-if-then node))]
                        [else (and (uir-if-else node) (walk (uir-if-else node)))])]
          [(uir-jsx-expr? node)
           (define expr (uir-jsx-expr-content node))
           (if (regexp-match? #rx"<[a-zA-Z]" expr)
               (or (process-cond-jsx-expr expr) node)
               node)]
          [else node]))
  
  (define (process-cond-jsx-expr expr)
    (define m-and (regexp-match #rx"^(.+?) *&& *(<[a-zA-Z].+)$" expr))
    (define m-tern (regexp-match #rx"^(.+?) *[?] *(<[a-zA-Z].+) *: *(<[a-zA-Z].+)$" expr))
    (define m-inline (regexp-match #rx"^ *(<[a-zA-Z].+) *$" expr))
    (cond [m-and
           (define cond-expr (uir-jsx-expr (string-trim (cadr m-and))))
           (define jsx-lowered (lower-jsx-text (caddr m-and)))
           (if jsx-lowered
               (uir-if cond-expr jsx-lowered (uir-null))
               #f)]
          [m-tern
           (define cond-expr (uir-jsx-expr (string-trim (cadr m-tern))))
           (define then-lowered (lower-jsx-text (string-trim (caddr m-tern))))
           (define else-lowered (lower-jsx-text (string-trim (cadddr m-tern))))
           (if (and then-lowered else-lowered)
               (uir-if cond-expr then-lowered else-lowered)
               #f)]
          [m-inline
           (define jsx-lowered (lower-jsx-text (cadr m-inline)))
           (or jsx-lowered #f)]
          [else #f]))
  
  (walk uir))
