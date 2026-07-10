#lang racket

(require racket/string
         "helpers.rkt"
         "extract.rkt")

(provide preprocess-jsx-expression-embeds)

;; ── Preprocess embedded JSX in expression braces ────────────────────
;; Scans a JSX string for <tag... patterns inside {...} expression braces,
;; parses+lowers+emits the inner JSX, and replaces it in the expression text.

(define (preprocess-jsx-expression-embeds jsx-str jsx-parser tk-type tk-value emit-fn)
  ;; emit-fn: (uir-node) -> string (e.g., emit-javascript)
  (define len (string-length jsx-str))
  (define results '())  ;; list of (start end replacement-text)
  
  ;; Scan tracking brace-depth (inside {...}) and string state
  (let scan ([pos 0] [brace-depth 0])
    (cond [(>= pos len)
           (reverse results)]
          
          [(char=? (string-ref jsx-str pos) #\{)
           (scan (+ pos 1) (+ brace-depth 1))]
          
          [(and (> brace-depth 0) (char=? (string-ref jsx-str pos) #\}))
           (scan (+ pos 1) (- brace-depth 1))]
          
          [(and (char=? (string-ref jsx-str pos) #\')
                (< (+ pos 1) len))
           (scan (advance-past-string jsx-str pos #\') brace-depth)]
          
          [(and (char=? (string-ref jsx-str pos) #\")
                (< (+ pos 1) len))
           (scan (advance-past-string jsx-str pos #\") brace-depth)]
          
          [(and (> brace-depth 0)
                (char=? (string-ref jsx-str pos) #\<)
                (< (+ pos 1) len)
                (id-start? (string-ref jsx-str (+ pos 1))))
           (let-values ([(inner end-pos) (extract-jsx jsx-str pos)])
             (if inner
                 (let* ([inner-trimmed (string-trim inner)]
                        [cst (with-handlers ([exn:fail? (lambda (e) #f)])
                               (jsx-parser inner-trimmed))]
                        [lowered (and cst
                                      (with-handlers ([exn:fail? (lambda (e) #f)])
                                        ((dynamic-require 'racklr/lower-jsx 'lower-jsx)
                                         cst #:tk-type tk-type #:tk-value tk-value)))]
                        [emitted (and lowered (emit-fn lowered))])
                   (if emitted
                       (begin
                         (set! results (cons (list pos (+ end-pos 1) emitted) results))
                         (scan (+ end-pos 1) brace-depth))
                       (scan (+ pos 1) brace-depth)))
                 (scan (+ pos 1) brace-depth)))]
          
          [else
           (scan (+ pos 1) brace-depth)]))
  
  ;; Apply replacements to build modified jsx-str
  ;; Process right-to-left so positions stay valid
  (define sorted (sort results > #:key first))
  (define processed jsx-str)
  (for ([region sorted])
    (match-define (list start end replacement) region)
    (define before (substring processed 0 start))
    (define after (substring processed end (string-length processed)))
    (set! processed (string-append before replacement after)))
  
  ;; Transform cond && <JSX_emission> → cond ? <JSX_emission> : null
  (let ([m (regexp-match #rx"^(.+?)\\s*\\&\\&\\s*(\\(function\\(\\).+)$" processed)])
    (if m
        (string-append (cadr m) " ? " (caddr m) " : null")
        processed)))
