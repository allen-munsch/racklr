#lang racket

(provide id-start? id-cont? skip-id
         prev-non-space context-allows-jsx?
         advance-past-string)

;; ── Identifier and context helpers ───────────────────────────────────

(define (id-start? c)
  (or (char-alphabetic? c) (char=? c #\_) (char=? c #\$)))

(define (id-cont? c)
  (or (id-start? c) (char-numeric? c)))

(define (skip-id s start)
  (define len (string-length s))
  (let loop ([pos (+ start 1)])
    (if (and (< pos len) (id-cont? (string-ref s pos)))
        (loop (+ pos 1))
        pos)))

(define (prev-non-space s pos)
  (let loop ([p (- pos 1)])
    (cond [(< p 0) #f]
          [(char-whitespace? (string-ref s p)) (loop (- p 1))]
          [else (string-ref s p)])))

(define (context-allows-jsx? s pos)
  ;; Check if the context before position pos suggests JSX is allowed.
  ;; Walk backwards from pos, skipping whitespace.
  ;; If the previous non-space char is:
  ;;   - A letter: check if it's part of a JSX-allowing keyword (return, yield, etc.)
  ;;   - An operator/punctuation: usually allows JSX (after =, :, (, etc.)
  ;;   - A digit: reject (comparison like x < 5)
  ;;   - Nothing (start of file): allow
  (let loop ([p (- pos 1)])
    (cond [(< p 0) #t]  ;; start of file
          [(char-whitespace? (string-ref s p)) (loop (- p 1))]
          [(char-numeric? (string-ref s p)) #f]  ;; x < 5
          [(char-alphabetic? (string-ref s p))
           ;; Walk back to start of identifier/keyword
           (define word-end (+ p 1))
           (let kw-loop ([q p])
             (if (and (>= q 0) (char-alphabetic? (string-ref s q)))
                 (kw-loop (- q 1))
                 (let* ([word-start (+ q 1)]
                        [word (substring s word-start word-end)])
                   ;; JSX-allowing keywords
                   (or (string-ci=? word "return")
                       (string-ci=? word "yield")
                       (string-ci=? word "case")
                       (string-ci=? word "default")
                       (string-ci=? word "throw")
                       (string-ci=? word "typeof")
                       (string-ci=? word "instanceof")
                       (string-ci=? word "new")
                       (string-ci=? word "delete")
                       (string-ci=? word "void")
                       (string-ci=? word "await")
                       ;; Also allow after 'as' (type assertion in TS)
                       (string-ci=? word "as")))))]
          [else #t])))  ;; operator/punctuation -> allow

(define (advance-past-string s pos quote-char)
  ;; Move pos past a string literal starting with quote-char
  (define strlen (string-length s))
  (let str-loop ([p (+ pos 1)])
    (cond [(>= p strlen) p]
          [(char=? (string-ref s p) quote-char)
           (if (and (> p pos) (char=? (string-ref s (- p 1)) #\\))
               (str-loop (+ p 1))  ;; escaped quote
               (+ p 1))]
          [(char=? (string-ref s p) #\\)
           (str-loop (+ p 2))]
          [else (str-loop (+ p 1))])))
