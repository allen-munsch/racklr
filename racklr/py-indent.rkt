#lang racket

(require racklr/tree)  ;; for pos

(provide insert-indents)

;; ── Python INDENT/DEDENT token insertion ───────────────────────────
;; Post-processes a flat token list to insert synthetic INDENT and DEDENT
;; tokens, mimicking ANTLR's Python3LexerBase.
;;
;; The generated lexer produces NEWLINE tokens whose value includes the
;; newline character(s) plus the leading whitespace of the next line
;; (e.g. "\n    ").  This module strips that indentation from the NEWLINE
;; value, computes the indent level, and inserts INDENT/DEDENT accordingly.
;;
;; Usage:
;;   (insert-indents tokens tk-type tk-value make-token)
;;
;; where tk-type, tk-value are the accessors from the generated lexer,
;; and make-token is (lambda (type value) ...) to construct a new token.
;; INDENT/DEDENT tokens have pos 0,0,0 since they're synthetic.

;; Python indent computation: spaces count 1, tabs advance to next multiple of 8
(define (compute-indent ws)
  (let loop ([i 0] [col 0])
    (if (>= i (string-length ws))
        col
        (let ([c (string-ref ws i)])
          (if (char=? c #\tab)
              (loop (+ i 1) (+ col (- 8 (modulo col 8))))
              (loop (+ i 1) (+ col 1)))))))

(define (paren-open? type)
  (member type '(OPEN_PAREN OPEN_BRACK OPEN_BRACE)))

(define (paren-close? type)
  (member type '(CLOSE_PAREN CLOSE_BRACK CLOSE_BRACE)))

;; Strip the newline prefix from a NEWLINE token value, returning
;; (values newline-str indent-ws).
;; E.g. "\n    " → "\n", "    "
;;      "\r\n"  → "\r\n", ""
(define (split-newline-value v)
  (define vlen (string-length v))
  (cond [(and (>= vlen 2) (char=? (string-ref v 0) #\return)
              (char=? (string-ref v 1) #\newline))
         (values "\r\n" (substring v 2 vlen))]
        [(and (>= vlen 1) (or (char=? (string-ref v 0) #\newline)
                              (char=? (string-ref v 0) #\return)
                              (char=? (string-ref v 0) #\page)))
         (values (substring v 0 1) (substring v 1 vlen))]
        [else (values v "")]))

;; Dummy position for synthetic tokens
(define dummy-pos (pos 0 0 0))

;; Main entry point.
;; tokens: list of token structs from the generated lexer
;; tk-type: token → type (symbol)
;; tk-value: token → value (string)
;; tk-start: token → pos
;; tk-end: token → pos
;; make-token: type value start end → new token
(define (insert-indents tokens tk-type tk-value tk-start tk-end make-token)
  (define indent-stack (list 0))
  (define opened 0)
  (define result '())

  (define (emit t) (set! result (cons t result)))

  (define (emit-indent indent-ws)
    (emit (make-token 'INDENT indent-ws dummy-pos dummy-pos)))

  (define (emit-dedent)
    (emit (make-token 'DEDENT "" dummy-pos dummy-pos)))

  ;; Process tokens; stop before EOF (we handle EOF separately)
  (let loop ([remaining tokens])
    (if (null? remaining)
        ;; End of input: emit DEDENTs for remaining indent levels, then EOF
        (begin
          (for ([_ (in-range (sub1 (length indent-stack)))])
            (emit-dedent))
          (reverse result))
        (let ([t (car remaining)]
              [rest (cdr remaining)])
          (define type (tk-type t))
          (define value (tk-value t))
          (cond
            ;; Track paren/bracket/brace nesting
            [(paren-open? type)
             (set! opened (+ opened 1))
             (emit t)
             (loop rest)]
            [(paren-close? type)
             (set! opened (max 0 (- opened 1)))
             (emit t)
             (loop rest)]
            [(and (eq? type 'NEWLINE) (= opened 0))
             ;; Process indentation
             (define-values (nl-str indent-ws) (split-newline-value value))
             (define indent (compute-indent indent-ws))
             (define current-top (car indent-stack))

             ;; Emit a clean NEWLINE (just the newline, no indent spaces)
             (emit (make-token 'NEWLINE nl-str (tk-start t) (tk-end t)))

             (cond
               [(> indent current-top)
                ;; Deeper indent: push and emit INDENT
                (set! indent-stack (cons indent indent-stack))
                (emit-indent indent-ws)]
               [(< indent current-top)
                ;; Shallower indent: pop and emit DEDENTs until match
                (let pop-loop ()
                  (when (and (pair? indent-stack) (> (car indent-stack) indent))
                    (set! indent-stack (cdr indent-stack))
                    (emit-dedent)
                    (pop-loop)))
                (when (or (null? indent-stack) (not (= (car indent-stack) indent)))
                  (error 'insert-indents
                         "unindent does not match any outer indentation level (indent ~a, stack ~a)"
                         indent indent-stack))]
               [else
                ;; Same indent level: nothing extra
                (void)])
             (loop rest)]
            [else
             (emit t)
             (loop rest)])))))

(module+ main
  (displayln "racklr/py-indent — Python INDENT/DEDENT token insertion loaded."))
