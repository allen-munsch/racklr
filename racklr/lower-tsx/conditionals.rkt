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
  
  (define (find-jsx-in-text text)
    ;; Find the first < followed by a letter, return substring from there
    (define trimmed (string-trim text))
    (define m (regexp-match-positions #rx"<[a-zA-Z]" trimmed))
    (and m (substring trimmed (caar m))))

  (define (process-cond-jsx-expr expr)
    (define trimmed (string-trim expr))
    ;; B57: use (?s:) to match across newlines, \s* for whitespace,
    ;; capture rest after && / ? / : then extract JSX from it.
    (define m-and (regexp-match #rx"(?s:^(.+?)\\s*&&\\s*(.+)$)" trimmed))
    (define m-tern (regexp-match #rx"(?s:^(.+?)\\s*[?]\\s*(.+)\\s*:\\s*(.+)$)" trimmed))
    (define m-inline (regexp-match #rx"(?s:^\\s*(<[a-zA-Z].+)\\s*$)" trimmed))
    (cond [m-and
           (let* ([cond-expr (uir-jsx-expr (string-trim (cadr m-and)))]
                  [jsx-text (find-jsx-in-text (caddr m-and))]
                  [jsx-lowered (and jsx-text (lower-jsx-text jsx-text))])
             (and jsx-lowered (uir-if cond-expr jsx-lowered (uir-null))))]
          [m-tern
           (let* ([cond-expr (uir-jsx-expr (string-trim (cadr m-tern)))]
                  [then-text (find-jsx-in-text (caddr m-tern))]
                  [else-text (find-jsx-in-text (cadddr m-tern))]
                  [then-lowered (and then-text (lower-jsx-text then-text))]
                  [else-lowered (and else-text (lower-jsx-text else-text))])
             (and then-lowered else-lowered
                  (uir-if cond-expr then-lowered else-lowered)))]
          [m-inline
           (lower-jsx-text (cadr m-inline))]
          [else #f]))
  
  (walk uir))
