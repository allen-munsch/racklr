#lang racket

(require "helpers.rkt")

(provide extract-jsx find-all-jsx)

;; ── JSX Extraction: Bracket-counting scanner ──────────────────────

;; Scan from start (pointing at '<') to find the matching end of a JSX
;; expression. Uses simple bracket counting: <tag opens, </tag closes,
;; { opens expr, } closes expr. Handles nested elements recursively.
;; Also handles React.Fragment syntax: <>...< />

;;
;; Returns (values jsx-string end-position) or (values #f start).

(define (extract-jsx s start)
  ;; s must have '<' at position start
  (unless (and (< start (string-length s))
               (char=? (string-ref s start) #\<))
    (error 'extract-jsx "expected < at position ~a in ~s" start s))
  
  (define len (string-length s))
  
  (define (skip-tag-name pos)
    ;; Return position after tag name, handling dotted names like Foo.Bar.Baz
    (if (and (< pos len) (id-start? (string-ref s pos)))
        (let loop ([p (skip-id s pos)])
          (if (and (< p len) (char=? (string-ref s p) #\.)
                   (< (+ p 1) len) (id-start? (string-ref s (+ p 1))))
              (loop (skip-id s (+ p 1)))
              p))
        pos))
  
  (let scan ([pos (+ start 1)]    ;; skip initial '<'
             [depth 1]             ;; we've entered one <tag>
             [brace-depth 0]
             [in-expr #f]
             [state 'in-tag-name])  ;; in-tag-name | in-attrs | in-children | in-closing-tag-name
    (cond [(= depth 0) (values (substring s start pos) (- pos 1))]
          [(>= pos len) (values #f start)]
          
          ;; Fragment open: <> — immediately transition to in-children
          [(and (eq? state 'in-tag-name) (char=? (string-ref s pos) #\>))
           (scan (+ pos 1) depth brace-depth #f 'in-children)]
          
          ;; Handle string literals first (any state)
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (scan (advance-past-string s pos (string-ref s pos))
                 depth brace-depth in-expr state)]
          
          ;; Handle expression braces in JSX children/attrs
          [(char=? (string-ref s pos) #\{)
           (scan (+ pos 1) depth (+ brace-depth 1) #t state)]
          
          [(and (> brace-depth 0) (char=? (string-ref s pos) #\}))
           (scan (+ pos 1) depth (- brace-depth 1) in-expr state)]
          
          ;; Handle < in children — could be </tag>, </>, nested <tag>, or <>
          [(and (char=? (string-ref s pos) #\<)
                (< (+ pos 1) len)
                (not (eq? state 'in-tag-name)))
           (define c2 (string-ref s (+ pos 1)))
           (cond [(char=? c2 #\/)
                  ;; </> fragment close or </tag> closing tag
                  (cond [(and (< (+ pos 2) len) (char=? (string-ref s (+ pos 2)) #\>))
                         ;; </> fragment close
                         (scan (+ pos 3) (- depth 1) brace-depth #f 'in-children)]
                        [else
                         ;; </ ... closing tag
                         (let ([after-slash (+ pos 2)])
                           ;; Skip whitespace after </
                           (define name-start
                             (let ws-loop ([p after-slash])
                               (if (and (< p len) (char-whitespace? (string-ref s p)))
                                   (ws-loop (+ p 1))
                                   p)))
                           ;; Read closing tag name
                           (define name-end (skip-tag-name name-start))
                           ;; Skip to >
                           (define close-start
                             (let ws-loop2 ([p name-end])
                               (if (and (< p len) (char-whitespace? (string-ref s p)))
                                   (ws-loop2 (+ p 1))
                                   p)))
                           (if (and (< close-start len) (char=? (string-ref s close-start) #\>))
                               (scan (+ close-start 1) (- depth 1) brace-depth #f 'in-children)
                               (values #f start)))])]
                 [(char=? c2 #\>)
                  ;; <> fragment open in children
                  (scan (+ pos 2) (+ depth 1) brace-depth #f 'in-children)]
                 [(id-start? c2)
                  ;; Nested element: <tag...
                  ;; Recursively extract the nested JSX
                  (let-values ([(inner end-pos) (extract-jsx s pos)])
                    (if inner
                        (scan (+ end-pos 1) depth brace-depth #f state)
                        (scan (+ pos 1) depth brace-depth #f state)))]
                 [else
                  (scan (+ pos 1) depth brace-depth #f state)])]
          
          ;; Handle /> self-closing
          [(and (char=? (string-ref s pos) #\/)
                (< (+ pos 1) len)
                (char=? (string-ref s (+ pos 1)) #\>))
           (scan (+ pos 2) (- depth 1) brace-depth #f 'in-children)]
          
          [(char=? (string-ref s pos) #\>)
           ;; End of opening tag, entering children
           (scan (+ pos 1) depth brace-depth #f 'in-children)]
          
          [else
           (scan (+ pos 1) depth brace-depth #f state)])))

;; ── Find all JSX regions ────────────────────────────────────────────

(define (find-all-jsx s)
  (define results '())
  (define len (string-length s))
  (let scan-loop ([i 0])
    (when (< i len)
      (cond [(and (char=? (string-ref s i) #\<)
                  (< (+ i 1) len)
                  (or (id-start? (string-ref s (+ i 1)))
                      ;; Fragment open: <>
                      (char=? (string-ref s (+ i 1)) #\>)))
             (if (context-allows-jsx? s i)
                 (let-values ([(jsx end-pos) (extract-jsx s i)])
                   (if jsx
                       (begin
                         (set! results (cons (list i (+ end-pos 1) jsx) results))
                         (scan-loop (+ end-pos 1)))
                       (scan-loop (+ i 1))))
                 (scan-loop (+ i 1)))]
            [else (scan-loop (+ i 1))])))
  (reverse results))
