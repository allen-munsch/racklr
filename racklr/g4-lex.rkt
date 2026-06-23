#lang racket

(require racklr/tree)

(provide g4-lex token token-type token-value token-pos token?)

;; ── Token Type ────────────────────────────────────────────────────

(struct token (type value pos) #:transparent)

;; ── Character constants ───────────────────────────────────────────

(define OPEN-BRACKET  (integer->char 91))
(define CLOSE-BRACKET (integer->char 93))

;; ── Lexer ─────────────────────────────────────────────────────────

(define (g4-lex input-str)
  (define len (string-length input-str))
  (define (char-at i) (if (< i len) (string-ref input-str i) eof))

  (let loop ([i 0] [line 1] [col 1] [offset 0] [tokens (list)])
    (define c (char-at i))
    (cond
      [(eof-object? c) (reverse tokens)]

      [(char=? c #\newline)  (loop (+ i 1) (+ line 1) 1 (+ offset 1) tokens)]
      [(char-whitespace? c)  (loop (+ i 1) line (+ col 1) (+ offset 1) tokens)]

      ;; Line comment //
      [(and (char=? c #\/) (char=? (char-at (+ i 1)) #\/))
       (define end-i (scan-until input-str (+ i 2) line (+ col 2) (+ offset 2)
                                 (lambda (ch) (or (eof-object? ch) (char=? ch #\newline)))))
       (loop (car end-i) (cadr end-i) (caddr end-i) (cadddr end-i) tokens)]

      ;; Block comment
      [(and (char=? c #\/) (char=? (char-at (+ i 1)) #\*))
       (define result (scan-block-comment input-str (+ i 2) line (+ col 2) (+ offset 2)))
       (loop (car result) (cadr result) (caddr result) (cadddr result) tokens)]

      ;; ->
      [(and (char=? c #\-) (char=? (char-at (+ i 1)) #\>))
       (loop (+ i 2) line (+ col 2) (+ offset 2)
             (cons (token 'arrow "->" (pos line col offset)) tokens))]

      ;; ..
      [(and (char=? c #\.) (char=? (char-at (+ i 1)) #\.))
       (loop (+ i 2) line (+ col 2) (+ offset 2)
             (cons (token 'range ".." (pos line col offset)) tokens))]

      ;; .
      [(char=? c #\.)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'dot "." (pos line col offset)) tokens))]

      ;; ;
      [(char=? c #\;)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'semicolon ";" (pos line col offset)) tokens))]

      ;; :
      [(char=? c #\:)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'colon ":" (pos line col offset)) tokens))]

      ;; |
      [(char=? c #\|)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'pipe "|" (pos line col offset)) tokens))]

      ;; (
      [(char=? c #\()
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'lparen "(" (pos line col offset)) tokens))]

      ;; )
      [(char=? c #\))
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'rparen ")" (pos line col offset)) tokens))]

      ;; *
      [(char=? c #\*)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'star "*" (pos line col offset)) tokens))]

      ;; +
      [(char=? c #\+)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'plus "+" (pos line col offset)) tokens))]

      ;; ?
      [(char=? c #\?)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'question "?" (pos line col offset)) tokens))]

      ;; ~
      [(char=? c #\~)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'tilde "~" (pos line col offset)) tokens))]

      ;; !
      [(char=? c #\!)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'bang "!" (pos line col offset)) tokens))]

      ;; - (standalone, not ->)
      [(char=? c #\-)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'dash "-" (pos line col offset)) tokens))]

      ;; =
      [(char=? c #\=)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'assign "=" (pos line col offset)) tokens))]

      ;; #
      [(char=? c #\#)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'hash "#" (pos line col offset)) tokens))]

      ;; <
      [(char=? c #\<)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'langle "<" (pos line col offset)) tokens))]

      ;; >
      [(char=? c #\>)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'rangle ">" (pos line col offset)) tokens))]

      ;; ,
      [(char=? c #\,)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'comma "," (pos line col offset)) tokens))]

      ;; { — emit as lbrace token
      [(char=? c OPEN-BRACE)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'lbrace "{" (pos line col offset)) tokens))]

      ;; } — emit as rbrace token
      [(char=? c CLOSE-BRACE)
       (loop (+ i 1) line (+ col 1) (+ offset 1)
             (cons (token 'rbrace "}" (pos line col offset)) tokens))]

      ;; [...] character class
      [(char=? c OPEN-BRACKET)
       (define result (scan-char-class input-str (+ i 1) line (+ col 1) (+ offset 1)))
       (define cc (list-ref result 4))
       (loop (car result) (cadr result) (caddr result) (cadddr result)
             (cons (token 'char-class cc (pos line col offset)) tokens))]

      ;; '...' string
      [(char=? c #\')
       (define result (scan-string input-str (+ i 1) line (+ col 1) (+ offset 1)))
       (define s (list-ref result 4))
       (loop (car result) (cadr result) (caddr result) (cadddr result)
             (cons (token 'string s (pos line col offset)) tokens))]

      ;; "..." double-quoted string (for code in action blocks)
      [(char=? c #\")
       (define result (scan-dstring input-str (+ i 1) line (+ col 1) (+ offset 1)))
       (define s (list-ref result 4))
       (loop (car result) (cadr result) (caddr result) (cadddr result)
             (cons (token 'dstring s (pos line col offset)) tokens))]

      ;; Identifier or keyword
      [(or (char-alphabetic? c) (char=? c #\_))
       (define result (scan-id input-str i line col offset))
       (define str (list-ref result 4))
       (define kw (string->symbol str))
       (define tok-type
         (case kw
           [(grammar) 'grammar] [(lexer) 'lexer] [(parser) 'parser]
           [(fragment) 'fragment] [(mode) 'mode] [(import) 'import]
           [(options) 'options] [(skip) 'skip] [(channel) 'channel]
           [(more) 'more] [(pushMode) 'pushMode] [(popMode) 'popMode]
           [(type) 'type]
           [else (if (char-upper-case? (string-ref str 0)) 'token-id 'id)]))
       (loop (car result) (cadr result) (caddr result) (cadddr result)
             (cons (token tok-type str (pos line col offset)) tokens))]

      [else
       (error 'lex "unexpected character '~a' at ~a:~a" c line col)])))

;; ── Scan helpers ──────────────────────────────────────────────────

(define OPEN-BRACE  (integer->char 123))
(define CLOSE-BRACE (integer->char 125))

(define (scan-until input-str i line col offset stop?)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (if (stop? ch)
        (list i l c o)
        (if (char=? ch #\newline)
            (loop (+ i 1) (+ l 1) 1 (+ o 1))
            (loop (+ i 1) l (+ c 1) (+ o 1))))))

(define (scan-block-comment input-str i line col offset)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (cond
      [(eof-object? ch) (error 'lex "unterminated block comment at ~a:~a" l c)]
      [(and (char=? ch #\*) (< (+ i 1) len)
            (char=? (string-ref input-str (+ i 1)) #\/))
       (list (+ i 2) l (+ c 2) (+ o 2))]
      [(char=? ch #\newline) (loop (+ i 1) (+ l 1) 1 (+ o 1))]
      [else (loop (+ i 1) l (+ c 1) (+ o 1))])))

(define (scan-braces input-str i line col offset depth)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset] [depth depth])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (cond
      [(eof-object? ch) (error 'lex "unterminated action at ~a:~a" l c)]
      [(char=? ch OPEN-BRACE) (loop (+ i 1) l (+ c 1) (+ o 1) (+ depth 1))]
      [(char=? ch CLOSE-BRACE) (if (= depth 1)
                                    (list (+ i 1) l (+ c 1) (+ o 1))
                                    (loop (+ i 1) l (+ c 1) (+ o 1) (- depth 1)))]
      [(char=? ch #\newline) (loop (+ i 1) (+ l 1) 1 (+ o 1) depth)]
      [else (loop (+ i 1) l (+ c 1) (+ o 1) depth)])))

(define (scan-char-class input-str i line col offset)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset] [acc (list)])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (cond
      [(eof-object? ch) (error 'lex "unterminated char class at ~a:~a" l c)]
      [(char=? ch #\newline) (loop (+ i 1) (+ l 1) 1 (+ o 1) (cons ch acc))]
      [(char=? ch #\\)
       (define nc (if (< (+ i 1) len) (string-ref input-str (+ i 1)) eof))
       (loop (+ i 2) l (+ c 2) (+ o 2) (cons nc (cons ch acc)))]
      [(char=? ch CLOSE-BRACKET)
       (list (+ i 1) l (+ c 1) (+ o 1)
             (format "[~a]" (list->string (reverse acc))))]
      [else (loop (+ i 1) l (+ c 1) (+ o 1) (cons ch acc))])))

(define (scan-string input-str i line col offset)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset] [acc (list)])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (cond
      [(eof-object? ch) (error 'lex "unterminated string at ~a:~a" l c)]
      [(char=? ch #\newline)
       (loop (+ i 1) (+ l 1) 1 (+ o 1) (cons ch acc))]
      [(char=? ch #\\)
       (define nc (if (< (+ i 1) len) (string-ref input-str (+ i 1)) eof))
       (loop (+ i 2) l (+ c 2) (+ o 2) (cons nc (cons ch acc)))]
      [(char=? ch #\') (list (+ i 1) l (+ c 1) (+ o 1)
                              (list->string (reverse acc)))]
      [else (loop (+ i 1) l (+ c 1) (+ o 1) (cons ch acc))])))

(define (scan-dstring input-str i line col offset)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset] [acc (list)])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (cond
      [(eof-object? ch) (error 'lex "unterminated double-quoted string at ~a:~a" l c)]
      [(char=? ch #\newline)
       (loop (+ i 1) (+ l 1) 1 (+ o 1) (cons ch acc))]
      [(char=? ch #\\)
       (define nc (if (< (+ i 1) len) (string-ref input-str (+ i 1)) eof))
       (loop (+ i 2) l (+ c 2) (+ o 2) (cons nc (cons ch acc)))]
      [(char=? ch #\") (list (+ i 1) l (+ c 1) (+ o 1)
                              (list->string (reverse acc)))]
      [else (loop (+ i 1) l (+ c 1) (+ o 1) (cons ch acc))])))

(define (scan-id input-str i line col offset)
  (define len (string-length input-str))
  (let loop ([i i] [l line] [c col] [o offset] [acc (list)])
    (define ch (if (< i len) (string-ref input-str i) eof))
    (if (and (not (eof-object? ch))
             (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))
        (loop (+ i 1) l (+ c 1) (+ o 1) (cons ch acc))
        (list i l c o (list->string (reverse acc))))))

(module+ main
  (displayln "racklr/g4-lex — ANTLR4 grammar lexer loaded."))
