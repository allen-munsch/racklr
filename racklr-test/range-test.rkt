#lang racket
(require racklr/tree)
(provide token token-type token-value token-start token-end tokenize parse)
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
          [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\-)
                (char<=? (string-ref pat i) c (string-ref pat (+ i 2)))) #t]
          [(char=? (string-ref pat i) c) #t]
          [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\-)) (loop (+ i 3))]
          [else (loop (+ i 1))])))
(define (DIGIT-match p in) ((lambda (p i) (mrange #\0 #\9 p i)) p in))

(define (tokenize in)
  (define il (string-length in))
  (let loop ([p 0] [l 1] [c 1] [o 0] [tks '()])
    (if (>= p il)
        (reverse (cons (token 'EOF "" (pos l c o) (pos l c o)) tks))
        (let ([ch (string-ref in p)])
          (cond
            [(char-whitespace? ch)
             (if (char=? ch #\newline)
                 (loop (+ p 1) (+ l 1) 1 (+ o 1) tks)
                 (loop (+ p 1) l (+ c 1) (+ o 1) tks))]
            [(DIGIT-match p in) => (lambda (r) (match-define (list np v) r) (define sl (string-length v)) (define tk (token 'DIGIT v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks)))]
            [else (error 'tokenize "unexpected char ~a at ~a:~a" ch l c)])))))


(define (ctok tks pos)
  (if (< pos (length tks)) (list-ref tks pos)
      (token 'EOF "" (pos 0 0 0) (pos 0 0 0))))

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
(define (parse-r tks pos)
  (or (let ([r0 (expect-tok tks pos 'DIGIT)]) (and r0 (list (car r0) (node 'r (list (cadr r0)) #:start (child-start (cadr r0)) #:end (child-end (cadr r0))))))
      #f))

(define (parse in)
  (define tks (tokenize in))
  (match-define (list fp res) (parse-r tks 0))
  res)