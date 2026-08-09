#lang racket
(require racklr/tree)
(provide token token? token-type token-value token-start token-end tokenize parse)
(struct token (type value start end) #:transparent)

(define (mlit s p in)
  (define sl (string-length s))
  (if (and (<= (+ p sl) (string-length in))
           (string=? (substring in p (+ p sl)) s))
      (list (+ p sl) s)
      #f))

(define (mrange lo hi p in)
  (if (>= p (string-length in)) #f
      (let ([c (string-ref in p)])
        (if (char<=? lo c hi) (list (+ p 1) (string c)) #f))))

(define (mcclass pat p in)
  (if (>= p (string-length in)) #f
      (let ([c (string-ref in p)])
        (if (cc-match c pat) (list (+ p 1) (string c)) #f))))

(define (mstar f p in)
  (let loop ([pp p] [a ""])
    (define r (f pp in))
    (if r (loop (car r) (string-append a (cadr r))) (list pp a))))

(define (mplus f p in)
  (define r (f p in))
  (and r (let ([rest (mstar f (car r) in)])
           (list (car rest) (string-append (cadr r) (cadr rest))))))

(define (mopt f p in)
  (define r (f p in))
  (or r (list p "")))

(define (mnot f p in)
  (if (>= p (string-length in)) #f
      (let ([r (f p in)])
        (if r #f (list (+ p 1) (string (string-ref in p)))))))

(define (malt fs p in)
  (let loop ([xs fs])
    (and (pair? xs) (or ((car xs) p in) (loop (cdr xs))))))

(define (mseq fs p in)
  (let loop ([xs fs] [pp p] [a ""])
    (if (null? xs) (list pp a)
        (let ([r ((car xs) pp in)])
          (and r (loop (cdr xs) (car r) (string-append a (cadr r))))))))

(define (cc-match c pat)
   (define pl (string-length pat))
   (let loop ([i 1])
     (cond [(>= i (- pl 1)) #f]
           ;; Handle \p{XX} and \P{XX} Unicode property escapes
           [(and (char=? (string-ref pat i) #\\) (member (string-ref pat (+ i 1)) '(#\p #\P)))
            (define is-negated (char=? (string-ref pat (+ i 1)) #\P))
            (define start (+ i 3)) ;; skip \p{ or \P{
            (let find-end ([j start])
              (if (char=? (string-ref pat j) #\})
                  (let ([prop (substring pat start j)])
                    (define cat (char-general-category c))
                    (define cat-str (symbol->string cat))
                    (define matches?
                      (cond [(string=? prop "L") (char-ci=? (string-ref cat-str 0) #\L)]
                            [(string=? prop "Nl") (eq? cat 'nl)]
                            [(string=? prop "Mn") (eq? cat 'mn)]
                            [(string=? prop "Mc") (eq? cat 'mc)]
                            [(string=? prop "Nd") (eq? cat 'nd)]
                            [(string=? prop "Pc") (eq? cat 'pc)]
                            [else (eprintf "Warning: unhandled Unicode property ~s~n" prop) #f]))
                    (and (if is-negated (not matches?) matches?) #t))
                  (find-end (+ j 1))))]
           ;; Handle character range: a-z
           [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\-)
                 (char<=? (string-ref pat i) c (string-ref pat (+ i 2)))) #t]
           [(char=? (string-ref pat i) c) #t]
           [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\-)) (loop (+ i 3))]
           [else (loop (+ i 1))])))
(define (JsxOpen-match p in) ((lambda (p i) (mlit "<" p i)) p in))
(define (JsxClose-match p in) ((lambda (p i) (mlit "</" p i)) p in))
(define (JsxOpenBrace-match p in) ((lambda (p i) (mlit "{" p i)) p in))
(define (HtmlChardata-match p in) ((lambda (p i) (mplus (lambda (p i) (mnot (lambda (p i) (mcclass "[<{]" p i)) p i)) p i)) p in))
(define (WhiteSpaces-match p in) ((lambda (p i) (mplus (lambda (p i) (mcclass "[\t\n\r ]" p i)) p i)) p in))
(define (JsxOpeningEnd-match p in) ((lambda (p i) (mlit ">" p i)) p in))
(define (JsxOpeningSlashEnd-match p in) ((lambda (p i) (mlit "/>" p i)) p in))
(define (JsxOpeningOpenBrace-match p in) ((lambda (p i) (mlit "{" p i)) p in))
(define (JsxName-match p in) (mseq (list (lambda (p i) (mcclass "[a-zA-Z_]" p i)) (lambda (p i) (mstar (lambda (p i) (mcclass "[-a-zA-Z0-9_.]" p i)) p i))) p in))
(define (JsxAssign-match p in) ((lambda (p i) (mlit "=" p i)) p in))
(define (JsxString-match p in) (malt (list (lambda (p in) (mseq (list (lambda (p i) (mlit "\"" p i)) (lambda (p i) (mstar (lambda (p i) (malt (list (lambda (p in) ((lambda (p i) (mnot (lambda (p i) (mcclass "[\"]" p i)) p i)) p in)) (lambda (p in) (mseq (list (lambda (p i) (mlit "\\" p i)) (lambda (p i) (mnot (lambda (p2 i2) #f) p i))) p in))) p in)) p i)) (lambda (p i) (mlit "\"" p i))) p in)) (lambda (p in) (mseq (list (lambda (p i) (mlit "'" p i)) (lambda (p i) (mstar (lambda (p i) (malt (list (lambda (p in) ((lambda (p i) (mnot (lambda (p i) (mcclass "[']" p i)) p i)) p in)) (lambda (p in) (mseq (list (lambda (p i) (mlit "\\" p i)) (lambda (p i) (mnot (lambda (p2 i2) #f) p i))) p in))) p in)) p i)) (lambda (p i) (mlit "'" p i))) p in))) p in))
(define (JsxOpeningWhiteSpaces-match p in) ((lambda (p i) (mplus (lambda (p i) (mcclass "[\t\n\r\v\f  ]" p i)) p i)) p in))
(define (JsxClosingEnd-match p in) ((lambda (p i) (mlit ">" p i)) p in))
(define (JsxClosingName-match p in) (mseq (list (lambda (p i) (mcclass "[a-zA-Z_]" p i)) (lambda (p i) (mstar (lambda (p i) (mcclass "[-a-zA-Z0-9_.]" p i)) p i))) p in))
(define (JsxClosingWhiteSpaces-match p in) ((lambda (p i) (mplus (lambda (p i) (mcclass "[\t\n\r ]" p i)) p i)) p in))
(define (ExpressionOpenBrace-match p in) ((lambda (p i) (mlit "{" p i)) p in))
(define (ExpressionCloseBrace-match p in) ((lambda (p i) (mlit "}" p i)) p in))
(define (ExpressionText-match p in) ((lambda (p i) (mplus (lambda (p i) (mnot (lambda (p i) (mcclass "[{}]" p i)) p i)) p i)) p in))
(define (ExpressionWhiteSpaces-match p in) ((lambda (p i) (mplus (lambda (p i) (mcclass "[\t\n\r ]" p i)) p i)) p in))

(define (tokenize in)
   (define il (string-length in))
    (define template-depth (box 0))
    (define brace-depth (box 0))
    (let loop ([p 0] [l 1] [c 1] [o 0] [tks '()] [mode 'default] [mstack '()] [pending #f])
     (if (>= p il)
         (let ([final-tks (if pending
                             (cons (token 'UNKNOWN pending (pos l c o) (pos l c o)) tks)
                             tks)])
           (reverse (cons (token 'EOF "" (pos l c o) (pos l c o)) final-tks)))
         (let ([ch (string-ref in p)])
           (cond
            [(and (char-whitespace? ch) (not (eq? mode 'TEMPLATE)))
               (if (char=? ch #\newline)
                   (loop (+ p 1) (+ l 1) 1 (+ o 1) tks mode mstack pending)
                   (loop (+ p 1) l (+ c 1) (+ o 1) tks mode mstack pending))]
            [(eq? mode 'default)
             (let __mloop ([__rules (list
               (cons JsxOpen-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpen v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'JSX_OPENING (cons mode mstack) pending)))
               (cons JsxClose-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxClose v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'JSX_CLOSING (cons mode (if (null? mstack) '() (cdr mstack))) pending)))
               (cons JsxOpenBrace-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpenBrace v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'EXPRESSION (cons mode mstack) pending)))
               (cons HtmlChardata-match (lambda (np v) (define sl (string-length v)) (define tk (token 'HtmlChardata v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons WhiteSpaces-match (lambda (np v) (define sl (string-length v)) (define tk (token 'WhiteSpaces v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))]
                                      [__best-np p] [__best-v #f] [__best-handle #f])
               (if (null? __rules)
                   (if __best-handle
                       (__best-handle __best-np __best-v)
                       (error 'tokenize "no matching rule in mode default at ~a:~a" l c))
                   (let ([__r ((caar __rules) p in)])
                     (if (and __r (> (car __r) __best-np))
                         (__mloop (cdr __rules) (car __r) (cadr __r) (cdar __rules))
                         (__mloop (cdr __rules) __best-np __best-v __best-handle)))))]
            [(eq? mode 'JSX_OPENING)
             (let __mloop ([__rules (list
               (cons JsxOpeningEnd-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpeningEnd v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'JSX_CHILDREN (cons mode (if (null? mstack) '() (cdr mstack))) pending)))
               (cons JsxOpeningSlashEnd-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpeningSlashEnd v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) (if (null? mstack) 'default (car mstack)) (if (null? mstack) '() (cdr mstack)) pending)))
               (cons JsxOpeningOpenBrace-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpeningOpenBrace v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'EXPRESSION (cons mode mstack) pending)))
               (cons JsxName-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxName v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons JsxAssign-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxAssign v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons JsxString-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxString v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons JsxOpeningWhiteSpaces-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpeningWhiteSpaces v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))]
                                      [__best-np p] [__best-v #f] [__best-handle #f])
               (if (null? __rules)
                   (if __best-handle
                       (__best-handle __best-np __best-v)
                       (error 'tokenize "no matching rule in mode JSX_OPENING at ~a:~a" l c))
                   (let ([__r ((caar __rules) p in)])
                     (if (and __r (> (car __r) __best-np))
                         (__mloop (cdr __rules) (car __r) (cadr __r) (cdar __rules))
                         (__mloop (cdr __rules) __best-np __best-v __best-handle)))))]
            [(eq? mode 'JSX_CHILDREN)
             (let __mloop ([__rules (list
               (cons JsxOpen-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpen v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'JSX_OPENING (cons mode mstack) pending)))
               (cons JsxClose-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxClose v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'JSX_CLOSING (cons mode (if (null? mstack) '() (cdr mstack))) pending)))
               (cons JsxOpenBrace-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxOpenBrace v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'EXPRESSION (cons mode mstack) pending)))
               (cons HtmlChardata-match (lambda (np v) (define sl (string-length v)) (define tk (token 'HtmlChardata v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons WhiteSpaces-match (lambda (np v) (define sl (string-length v)) (define tk (token 'WhiteSpaces v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))]
                                      [__best-np p] [__best-v #f] [__best-handle #f])
               (if (null? __rules)
                   (if __best-handle
                       (__best-handle __best-np __best-v)
                       (error 'tokenize "no matching rule in mode JSX_CHILDREN at ~a:~a" l c))
                   (let ([__r ((caar __rules) p in)])
                     (if (and __r (> (car __r) __best-np))
                         (__mloop (cdr __rules) (car __r) (cadr __r) (cdar __rules))
                         (__mloop (cdr __rules) __best-np __best-v __best-handle)))))]
            [(eq? mode 'JSX_CLOSING)
             (let __mloop ([__rules (list
               (cons JsxClosingEnd-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxClosingEnd v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) (if (null? mstack) 'default (car mstack)) (if (null? mstack) '() (cdr mstack)) pending)))
               (cons JsxClosingName-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxClosingName v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons JsxClosingWhiteSpaces-match (lambda (np v) (define sl (string-length v)) (define tk (token 'JsxClosingWhiteSpaces v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))]
                                      [__best-np p] [__best-v #f] [__best-handle #f])
               (if (null? __rules)
                   (if __best-handle
                       (__best-handle __best-np __best-v)
                       (error 'tokenize "no matching rule in mode JSX_CLOSING at ~a:~a" l c))
                   (let ([__r ((caar __rules) p in)])
                     (if (and __r (> (car __r) __best-np))
                         (__mloop (cdr __rules) (car __r) (cadr __r) (cdar __rules))
                         (__mloop (cdr __rules) __best-np __best-v __best-handle)))))]
            [(eq? mode 'EXPRESSION)
             (let __mloop ([__rules (list
               (cons ExpressionOpenBrace-match (lambda (np v) (define sl (string-length v)) (define tk (token 'ExpressionOpenBrace v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) 'EXPRESSION (cons mode mstack) pending)))
               (cons ExpressionCloseBrace-match (lambda (np v) (define sl (string-length v)) (define tk (token 'ExpressionCloseBrace v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) (if (null? mstack) 'default (car mstack)) (if (null? mstack) '() (cdr mstack)) pending)))
               (cons ExpressionText-match (lambda (np v) (define sl (string-length v)) (define tk (token 'ExpressionText v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)))
               (cons ExpressionWhiteSpaces-match (lambda (np v) (define sl (string-length v)) (define tk (token 'ExpressionWhiteSpaces v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending))))]
                                      [__best-np p] [__best-v #f] [__best-handle #f])
               (if (null? __rules)
                   (if __best-handle
                       (__best-handle __best-np __best-v)
                       (error 'tokenize "no matching rule in mode EXPRESSION at ~a:~a" l c))
                   (let ([__r ((caar __rules) p in)])
                     (if (and __r (> (car __r) __best-np))
                         (__mloop (cdr __rules) (car __r) (cadr __r) (cdar __rules))
                         (__mloop (cdr __rules) __best-np __best-v __best-handle)))))]
            [else (error 'tokenize "unexpected char ~a in mode ~a at ~a:~a" ch mode l c)])))))


(define (ctok tks pos)
  (if (< pos (length tks)) (list-ref tks pos)
      (token 'EOF "" (source-pos 0 0 0) (source-pos 0 0 0))))

(define (expect-tok tks pos type)
  (define t (ctok tks pos))
  (if (eq? (token-type t) type) (list (+ pos 1) t) #f))

(define (expect-lit tks pos val)
  (define t (ctok tks pos))
  (if (string=? (token-value t) val) (list (+ pos 1) t) #f))

(define (parse-star tks pos fn)
  (let loop ([p pos] [kids '()])
    (define r (fn tks p))
    (if r (loop (car r) (cons (cadr r) kids)) (list p (reverse kids)))))

(define (parse-plus tks pos fn)
  (define r (fn tks pos))
  (and r (let* ([rest (parse-star tks (car r) fn)])
           (list (car rest) (cons (cadr r) (cadr rest))))))

(define (parse-opt tks pos fn)
  (define r (fn tks pos))
  (if r r (list pos 'none)))

(define (parse-group tks pos fns)
  (let loop ([fs fns])
    (if (null? fs) #f
        (let ([r ((car fs) tks pos)])
          (if r r (loop (cdr fs)))))))

(define (child-range child)
  ;; Extract (start-pos . end-pos) from either a token, a tree node, or a list
  (cond [(null? child) (cons (pos 0 0 0) (pos 0 0 0))]
        [(pair? child)
         ;; List from parse-star/parse-plus: combine first/last
         (cons (child-start (car child)) (child-end (car (reverse child))))]
        [(any-tree? child) (any-tree-range child)]
        [(eq? child 'none) (cons (pos 0 0 0) (pos 0 0 0))]
        [else (cons (token-start child) (token-end child))]))

(define (child-start child)
  (car (child-range child)))

(define (child-end child)
  (cdr (child-range child)))
(define (parse-jsxElement tks pos)
  (or (let ([r0 (parse-jsxFragment tks pos)]) (and r0 (list (car r0) (node 'jsxElement (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      (let ([r0 (expect-tok tks pos 'JsxOpen)]) (and r0 (let ([r1 (expect-tok tks (car r0) 'JsxName)]) (and r1 (let ([r2 (parse-opt tks (car r1) (lambda (t p) (parse-jsxAttributes t p)))]) (and r2 (let ([r3 (expect-tok tks (car r2) 'JsxOpeningSlashEnd)]) (and r3 (list (car r3) (node 'jsxElement (list (cadr r0) (cadr r1) (cadr r2) (cadr r3)) #:start (child-start (cadr r0)) #:end (child-end (cadr r3))))))))))))
      (let ([r0 (expect-tok tks pos 'JsxOpen)]) (and r0 (let ([r1 (expect-tok tks (car r0) 'JsxName)]) (and r1 (let ([r2 (parse-opt tks (car r1) (lambda (t p) (parse-jsxAttributes t p)))]) (and r2 (let ([r3 (expect-tok tks (car r2) 'JsxOpeningEnd)]) (and r3 (let ([r4 (parse-opt tks (car r3) (lambda (t p) (parse-jsxChildren t p)))]) (and r4 (let ([r5 (expect-tok tks (car r4) 'JsxClose)]) (and r5 (let ([r6 (expect-tok tks (car r5) 'JsxClosingName)]) (and r6 (let ([r7 (expect-tok tks (car r6) 'JsxClosingEnd)]) (and r7 (list (car r7) (node 'jsxElement (list (cadr r0) (cadr r1) (cadr r2) (cadr r3) (cadr r4) (cadr r5) (cadr r6) (cadr r7)) #:start (child-start (cadr r0)) #:end (child-end (cadr r7))))))))))))))))))))
      #f))

(define (parse-jsxFragment tks pos)
  (or (let ([r0 (expect-tok tks pos 'JsxOpen)]) (and r0 (let ([r1 (expect-tok tks (car r0) 'JsxOpeningEnd)]) (and r1 (let ([r2 (parse-opt tks (car r1) (lambda (t p) (parse-jsxChildren t p)))]) (and r2 (let ([r3 (expect-tok tks (car r2) 'JsxClose)]) (and r3 (let ([r4 (expect-tok tks (car r3) 'JsxClosingEnd)]) (and r4 (list (car r4) (node 'jsxFragment (list (cadr r0) (cadr r1) (cadr r2) (cadr r3) (cadr r4)) #:start (child-start (cadr r0)) #:end (child-end (cadr r4))))))))))))))
      #f))

(define (parse-jsxAttributes tks pos)
  (or (let ([r0 (parse-plus tks pos (lambda (t p) (parse-jsxAttribute t p)))]) (and r0 (list (car r0) (node 'jsxAttributes (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      #f))

(define (parse-jsxAttribute tks pos)
  (or (let ([r0 (expect-tok tks pos 'JsxName)]) (and r0 (let ([r1 (parse-opt tks (car r0) (lambda (t p) (parse-group t p (list (lambda (t p) (let ([r0 (expect-tok tks p 'JsxAssign)]) (and r0 (let ([r1 (parse-jsxAttributeValue tks (car r0))]) (and r1 (list (car r1) (node 'group (list (cadr r0) (cadr r1)) #:start (child-start (cadr r0)) #:end (child-end (cadr r1)))))))))))))]) (and r1 (list (car r1) (node 'jsxAttribute (list (cadr r0) (cadr r1)) #:start (child-start (cadr r0)) #:end (child-end (cadr r1))))))))
      #f))

(define (parse-jsxAttributeValue tks pos)
  (or (let ([r0 (expect-tok tks pos 'JsxString)]) (and r0 (list (car r0) (node 'jsxAttributeValue (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      (let ([r0 (expect-tok tks pos 'JsxOpeningOpenBrace)]) (and r0 (let ([r1 (parse-jsxExpression tks (car r0))]) (and r1 (let ([r2 (expect-tok tks (car r1) 'ExpressionCloseBrace)]) (and r2 (list (car r2) (node 'jsxAttributeValue (list (cadr r0) (cadr r1) (cadr r2)) #:start (child-start (cadr r0)) #:end (child-end (cadr r2))))))))))
      #f))

(define (parse-jsxChildren tks pos)
  (or (let ([r0 (parse-plus tks pos (lambda (t p) (parse-jsxChild t p)))]) (and r0 (list (car r0) (node 'jsxChildren (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      #f))

(define (parse-jsxChild tks pos)
  (or (let ([r0 (expect-tok tks pos 'HtmlChardata)]) (and r0 (list (car r0) (node 'jsxChild (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      (let ([r0 (expect-tok tks pos 'JsxOpenBrace)]) (and r0 (let ([r1 (parse-jsxExpression tks (car r0))]) (and r1 (let ([r2 (expect-tok tks (car r1) 'ExpressionCloseBrace)]) (and r2 (list (car r2) (node 'jsxChild (list (cadr r0) (cadr r1) (cadr r2)) #:start (child-start (cadr r0)) #:end (child-end (cadr r2))))))))))
      (let ([r0 (parse-jsxElement tks pos)]) (and r0 (list (car r0) (node 'jsxChild (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      #f))

(define (parse-jsxExpression tks pos)
  (or (let ([r0 (parse-star tks pos (lambda (t p) (parse-group t p (list (lambda (t p) (let ([r0 (expect-tok tks p 'ExpressionText)]) (and r0 (list (car r0) (node 'group (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))) (lambda (t p) (let ([r0 (expect-tok tks p 'ExpressionOpenBrace)]) (and r0 (let ([r1 (parse-jsxExpression tks (car r0))]) (and r1 (let ([r2 (expect-tok tks (car r1) 'ExpressionCloseBrace)]) (and r2 (list (car r2) (node 'group (list (cadr r0) (cadr r1) (cadr r2)) #:start (child-start (cadr r0)) #:end (child-end (cadr r2)))))))))))))))]) (and r0 (list (car r0) (node 'jsxExpression (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      #f))
(provide parse-jsxElement parse-jsxFragment parse-jsxAttributes parse-jsxAttribute parse-jsxAttributeValue parse-jsxChildren parse-jsxChild parse-jsxExpression)

(define (parse in)
  (define tks (tokenize in))
  (match-define (list fp res) (parse-jsxElement tks 0))
  res)