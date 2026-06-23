#lang racket

(require racklr/uir)

(provide emit-jsx)

;; ── JSX UIR → React.createElement JavaScript emitter ──────────────────────

(define (emit-jsx uir)
  (match uir
    [(uir-string v)
     (format "~s" v)]
    
    [(uir-symbol v) v]
    
    [(uir-text-node text-uir)
     (emit-jsx text-uir)]
    
    [(uir-jsx-expr expr)
     expr]
    
    [(uir-attribute name-uir value-uir)
     (cons (emit-jsx name-uir) (emit-jsx value-uir))]
    
    [(uir-element tag-uir attrs children key-uir)
     (define tag-str (emit-jsx tag-uir))
     (define props-str (emit-props attrs key-uir))
     (define children-strs (map emit-jsx children))
     (format "React.createElement(~a, ~a~a)"
             tag-str
             props-str
             (if (null? children)
                 ""
                 (format ", ~a" (string-join children-strs ", "))))]
    
    [_ (error 'emit-jsx "unexpected UIR type: ~a" uir)]))

(define (emit-props attrs key-uir)
  (define attr-pairs
    (for/list ([attr (in-list attrs)])
      (match-define (cons name-str val-str) (emit-jsx attr))
      (format "~a: ~a" name-str val-str)))
  
  (define key-pair
    (match key-uir
      [(uir-string k)
       (list (format "key: ~s" k))]
      [_ '()]))
  
  (define all-pairs (append attr-pairs key-pair))
  
  (if (null? all-pairs)
      "null"
      (format "{ ~a }" (string-join all-pairs ", "))))
