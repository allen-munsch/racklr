#lang racket

(require racklr/tree
         racklr/g4-parse)

(provide generate-parser-module)

;; ── Helpers for tokenVocab ─────────────────────────────────────────

(define (extract-option options-node key)
  (for/or ([opt (any-tree-children options-node)]
           #:when (string=? (any-tree-text (first (any-tree-children opt))) key))
    (any-tree-text (second (any-tree-children opt)))))

(define (load-lexer-grammar-rules lexer-path)
  (define lexer-cst (parse-g4-file lexer-path))
  (define rules-node (second (any-tree-children lexer-cst)))
  (define rules (any-tree-children rules-node))
  (values (filter (lambda (r) (eq? (any-tree-tag r) 'lexer-rule)) rules)
          (filter (lambda (r) (eq? (any-tree-tag r) 'fragment-rule)) rules)
          (filter (lambda (r) (eq? (any-tree-tag r) 'mode)) rules)))

(define (lexer-has-newline-rule? lexer-rules)
  (for/or ([r lexer-rules])
    (and (eq? (any-tree-tag r) 'lexer-rule)
         (string=? (any-tree-text (first (any-tree-children r))) "NEWLINE"))))

;; Simple parser generator.
;; Given a grammar CST, returns Racket source string for a generated parser.
;; The generated parser provides: token struct, tokenize, parse.

(define (generate-parser-module grammar-cst #:source-path [source-path #f] #:indent-tokens? [indent-tokens? #f])
  (define rules-node (second (any-tree-children grammar-cst)))
  (define rules (any-tree-children rules-node))
  (define parser-rules (filter (lambda (r) (eq? (any-tree-tag r) 'parser-rule)) rules))
  (define lexer-rules (filter (lambda (r) (eq? (any-tree-tag r) 'lexer-rule)) rules))
  (define frag-rules (filter (lambda (r) (eq? (any-tree-tag r) 'fragment-rule)) rules))
  (define mode-nodes (filter (lambda (r) (eq? (any-tree-tag r) 'mode)) rules))

  ;; Handle tokenVocab: load lexer grammar from separate file
  (define-values (extra-lexer-rules extra-frag-rules extra-mode-nodes)
    (if source-path
        (let* ((options-node (findf (lambda (r) (eq? (any-tree-tag r) 'options)) rules))
               (token-vocab (and options-node (extract-option options-node "tokenVocab"))))
          (if token-vocab
              (let ((lexer-path (build-path (path-only source-path)
                                            (string-append token-vocab ".g4"))))
                (if (file-exists? lexer-path)
                    (load-lexer-grammar-rules lexer-path)
                    (begin (eprintf "Warning: tokenVocab ~a.g4 not found at ~a~n"
                                    token-vocab lexer-path)
                           (values '() '() '()))))
              (values '() '() '())))
        (values '() '() '())))

  ;; Merge lexer rules from tokenVocab
  (set! lexer-rules (append lexer-rules extra-lexer-rules))
  (set! frag-rules (append frag-rules extra-frag-rules))
  (set! mode-nodes (append mode-nodes extra-mode-nodes))

  ;; Extract lexer/fragment rules from mode nodes
  (define mode-lexer-rules
    (for*/list ([mn mode-nodes]
                [r (rest (any-tree-children mn))])
      r))

  ;; Check for left-recursive parser rules
  (detect-left-recursion parser-rules)
  (define all-lexer (append lexer-rules frag-rules mode-lexer-rules))
  (define parser-literals (collect-parser-literals parser-rules))
  ;; Build synthetic lexer rules for parser literals not already covered
  (define implicit-lexer-rules (build-implicit-lexer-rules parser-literals lexer-rules))
  (define all-token-rules (append lexer-rules implicit-lexer-rules))

  ;; Build mode map: mode-name -> list of token rules
  (define mode-map (build-mode-map lexer-rules mode-nodes implicit-lexer-rules))

  (define has-parser-rules (not (null? parser-rules)))
  (define has-newline? (lexer-has-newline-rule? all-lexer))

   (string-join
    (list (gen-header #:indent-tokens? indent-tokens?)
          (gen-match-helpers)
          (gen-lexer-matchers all-lexer)
          (gen-lexer-matchers implicit-lexer-rules)
          (gen-tokenizer mode-map #:has-newline? has-newline? #:indent-tokens? indent-tokens?)
          (gen-parser-helpers)
          (if has-parser-rules (gen-parser-rules parser-rules) "")
          (if has-parser-rules (gen-parser-provides parser-rules) "")
          (if has-parser-rules (gen-entry parser-rules) (gen-lexer-entry)))
    "\n"))

;; ── Implicit Lexer Rules for Parser Literals ─────────────────────
;; In ANTLR4, string literals like '{' or 'true' used directly in parser
;; rules are implicitly tokenized. We extract them and generate lexer rules.

(define (collect-parser-literals parser-rules)
  (define literals '())
  (define (walk node)
    (when node
      (when (and (any-tree? node) (eq? (any-tree-tag node) 'literal))
        (define txt (any-tree-text node))
        (set! literals (cons txt literals)))
      (when (cst-node? node)
        (for ([ch (cst-node-children node)])
          (walk ch)))))
  (for ([r parser-rules])
    (walk r))
  (remove-duplicates literals))

(define (build-implicit-lexer-rules literals existing-lexer-rules)
  ;; Collect names of existing lexer rules (non-fragment ones)
  (define existing-names
    (for/list ([r existing-lexer-rules]
                #:when (eq? (any-tree-tag r) 'lexer-rule))
      (any-tree-text (first (any-tree-children r)))))
  ;; Build a synthetic rule CST for each literal not already covered
  ;; A lexer rule CST: (node 'lexer-rule ((leaf 'name ...) (node 'alternatives ((node 'alternative ((leaf 'literal ...)))))))
  ;; For simplicity, we just create minimal structures the generator can walk
  (for/list ([lit literals]
              #:unless (member lit existing-names))
    ;; Only handle simple single-char or keyword literals
    ;; Skip literals that would conflict with existing rules
    (node 'lexer-rule
          (list (leaf 'name lit #:start (pos 0 0 0) #:end (pos 0 0 0))
                (node 'alternatives
                      (list (node 'alternative
                                  (list (leaf 'literal lit #:start (pos 0 0 0) #:end (pos 0 0 0))))))))))

;; ── Sections ──────────────────────────────────────────────────────

(define (gen-header #:indent-tokens? [indent-tokens? #f])
  (string-append
   "#lang racket
(require racklr/tree)
(provide token token? token-type token-value token-start token-end tokenize parse)
(struct token (type value start end) #:transparent)"
   (if indent-tokens?
       (gen-indent-helpers)
       "")))

;; ── Python INDENT/DEDENT helpers (embedded in generated lexer) ────
(define (gen-indent-helpers)
  "
;; Python indent computation: spaces count 1, tabs advance to next multiple of 8
(define (compute-indent ws)
  (let loop ([i 0] [col 0])
    (if (>= i (string-length ws))
        col
        (let ([c (string-ref ws i)])
          (if (char=? c #\\tab)
              (loop (+ i 1) (+ col (- 8 (modulo col 8))))
              (loop (+ i 1) (+ col 1)))))))

(define (paren-open? type)
  (member type '(OPEN_PAREN OPEN_BRACK OPEN_BRACE)))

(define (paren-close? type)
  (member type '(CLOSE_PAREN CLOSE_BRACK CLOSE_BRACE)))

;; Strip the newline prefix from a NEWLINE token value, returning
;; (values newline-str indent-ws).
(define (split-newline-value v)
  (define vlen (string-length v))
  (cond [(and (>= vlen 2) (char=? (string-ref v 0) #\\return)
              (char=? (string-ref v 1) #\\newline))
         (values \"\\r\\n\" (substring v 2 vlen))]
        [(and (>= vlen 1) (or (char=? (string-ref v 0) #\\newline)
                              (char=? (string-ref v 0) #\\return)
                              (char=? (string-ref v 0) #\\page)))
         (values (substring v 0 1) (substring v 1 vlen))]
        [else (values v \"\")]))

;; Post-process token list to insert INDENT/DEDENT tokens
;; raw-tokens: list of (token ...) including EOF at end
;; Returns: new list with INDENT/DEDENT inserted (EOF preserved at end)
(define (insert-indents raw-tokens)
  (define indent-stack (list 0))
  (define opened 0)
  (define dummy-pos (pos 0 0 0))
  (define result '())
  (define (emit t) (set! result (cons t result)))
  (define (emit-indent indent-ws)
    (emit (token 'INDENT indent-ws dummy-pos dummy-pos)))
  (define (emit-dedent)
    (emit (token 'DEDENT \"\" dummy-pos dummy-pos)))

  (let loop ([remaining raw-tokens])
    (if (null? remaining)
        (begin
          (for ([_ (in-range (sub1 (length indent-stack)))])
            (emit-dedent))
          (reverse result))
        (let ([t (car remaining)]
              [rest (cdr remaining)])
          (define type (token-type t))
          (define value (token-value t))
          (cond
            [(eq? type 'EOF)
             ;; EOF: emit pending DEDENTs then EOF
             (for ([_ (in-range (sub1 (length indent-stack)))])
               (emit-dedent))
             (emit t)
             (reverse result)]
            [(paren-open? type)
             (set! opened (+ opened 1))
             (emit t)
             (loop rest)]
            [(paren-close? type)
             (set! opened (max 0 (- opened 1)))
             (emit t)
             (loop rest)]
            [(and (eq? type 'NEWLINE) (= opened 0))
             (define-values (nl-str indent-ws) (split-newline-value value))
             (define indent (compute-indent indent-ws))
             (define current-top (car indent-stack))
             (emit (token 'NEWLINE nl-str (token-start t) (token-end t)))
             (cond
               [(> indent current-top)
                (set! indent-stack (cons indent indent-stack))
                (emit-indent indent-ws)]
               [(< indent current-top)
                (let pop-loop ()
                  (when (and (pair? indent-stack) (> (car indent-stack) indent))
                    (set! indent-stack (cdr indent-stack))
                    (emit-dedent)
                    (pop-loop)))
                (when (or (null? indent-stack) (not (= (car indent-stack) indent)))
                  (error 'insert-indents
                         \"unindent does not match any outer indentation level\"))]
               [else (void)])
             (loop rest)]
            [else
             (emit t)
             (loop rest)])))))
")

;; Mangle a string into a valid Racket identifier
(define (mangle str)
  (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" str)
      str
      (string-join
       (for/list ([c str])
         (cond [(char-alphabetic? c) (string c)]
               [(char-numeric? c) (string c)]
               [(char=? c #\_) "_"]
               [else (format "_x~a_" (number->string (char->integer c) 16))]))
       "")))

;; ── ANTLR4 escape sequences for char classes ──────────────────────
;; Convert ANTLR4 escape sequences in character class strings to actual chars.
;; Handles: \\t \\n \\r \\uXXXX (Unicode code points)
(define (unescape-char-class s)
  ;; Pre-process: strip surrogate code point ranges (\\uD800-\\uDFFF) from
  ;; the character class. In Racket's code-point model, surrogates don't exist
  ;; as characters, so ranges involving them match nothing. We remove them
  ;; from the pattern to prevent spurious matches in cc-match.
  ;;
  ;; Three patterns to handle, in order:
  ;;   \\uXXXX-\\uYYYY where both are surrogates → remove entire range
  ;;   \\uXXXX- at start of range (lower bound surrogate) → remove \\uXXXX-
  ;;   -\\uXXXX at end of range (upper bound surrogate) → remove -\\uXXXX
  ;;   Standalone surrogate \\uXXXX → remove it
  ;;
  ;; Surrogate hex: D800-DFFF. High surrogates: D[89AB]xxx. Low: D[CDEF]xxx.
  
  (define s0
    (let loop ([s s])
      (cond
        ;; Entire surrogate-surrogate range (high-high, low-low, or high-low)
        [(regexp-match-positions #px"\\\\uD[89AB][0-9A-Fa-f]{2}-\\\\uD[89AB][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (define i (caar m))
              (define j (cdar m))
              (loop (string-append (substring s 0 i) (substring s j))))]
        [(regexp-match-positions #px"\\\\uD[CDEF][0-9A-Fa-f]{2}-\\\\uD[CDEF][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (define i (caar m))
              (define j (cdar m))
              (loop (string-append (substring s 0 i) (substring s j))))]
        ;; Surrogate at end of range: X-\\uXXXX
        [(regexp-match-positions #px"-\\\\uD[89AB][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [(regexp-match-positions #px"-\\\\uD[CDEF][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        ;; Surrogate at start of range: \\uXXXX-
        [(regexp-match-positions #px"\\\\uD[89AB][0-9A-Fa-f]{2}-" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [(regexp-match-positions #px"\\\\uD[CDEF][0-9A-Fa-f]{2}-" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        ;; Standalone surrogate: just remove it
        [(regexp-match-positions #px"\\\\uD[89AB][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [(regexp-match-positions #px"\\\\uD[CDEF][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [else s])))
  
  ;; Now convert remaining (non-surrogate) \\uXXXX to actual characters
  ;; Use pregexp (not regexp) because regexp doesn't support {n} quantifiers
  ;; regexp-replace* with pregexp passes (matched . groups) to the lambda
  (define s1
    (regexp-replace* #px"\\\\u([0-9A-Fa-f]{4})" s0
      (lambda (matched hex-str)
        (define code (string->number hex-str 16))
        (string (integer->char code)))))
  ;; Then handle \\t \\n \\r
  (regexp-replace* #rx"\\\\t" (regexp-replace* #rx"\\\\n" (regexp-replace* #rx"\\\\r" s1 "\r") "\n") "\t"))

;; Convert ANTLR4 escape sequences in literal strings to actual chars.
;; E.g. "\\n" → "\n" (actual newline), "\\\\" → "\\", "\\'" → "'"
(define (unescape-g4-literal s)
  (regexp-replace* #rx"\\\\n" (regexp-replace* #rx"\\\\r" (regexp-replace* #rx"\\\\t" (regexp-replace* #rx"\\\\f" (regexp-replace* #rx"\\\\\\\\" (regexp-replace* #rx"\\\\'" s "'") "\\\\") "") "\t") "\r") "\n"))

(define (gen-match-helpers)
  "
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
  (let loop ([pp p] [a \"\"])
    (define r (f pp in))
    (if r (loop (car r) (string-append a (cadr r))) (list pp a))))

(define (mplus f p in)
  (define r (f p in))
  (and r (let ([rest (mstar f (car r) in)])
           (list (car rest) (string-append (cadr r) (cadr rest))))))

(define (mopt f p in)
  (define r (f p in))
  (or r (list p \"\")))

(define (mnot f p in)
  (if (>= p (string-length in)) #f
      (let ([r (f p in)])
        (if r #f (list (+ p 1) (string (string-ref in p)))))))

(define (malt fs p in)
  (let loop ([xs fs])
    (and (pair? xs) (or ((car xs) p in) (loop (cdr xs))))))

(define (mseq fs p in)
  (let loop ([xs fs] [pp p] [a \"\"])
    (if (null? xs) (list pp a)
        (let ([r ((car xs) pp in)])
          (and r (loop (cdr xs) (car r) (string-append a (cadr r))))))))

(define (cc-match c pat)
   (define pl (string-length pat))
   (let loop ([i 1])
     (cond [(>= i (- pl 1)) #f]
           ;; Handle \\p{XX} and \\P{XX} Unicode property escapes
           [(and (char=? (string-ref pat i) #\\\\) (member (string-ref pat (+ i 1)) '(#\\p #\\P)))
            (define is-negated (char=? (string-ref pat (+ i 1)) #\\P))
            (define start (+ i 3)) ;; skip \\p{ or \\P{
            (let find-end ([j start])
              (if (char=? (string-ref pat j) #\\})
                  (let ([prop (substring pat start j)])
                    (define cat (char-general-category c))
                    (define cat-str (symbol->string cat))
                    (define matches?
                      (cond [(string=? prop \"L\") (char-ci=? (string-ref cat-str 0) #\\L)]
                            [(string=? prop \"Nl\") (eq? cat 'Nl)]
                            [(string=? prop \"Mn\") (eq? cat 'Mn)]
                            [(string=? prop \"Mc\") (eq? cat 'Mc)]
                            [(string=? prop \"Nd\") (eq? cat 'Nd)]
                            [(string=? prop \"Pc\") (eq? cat 'Pc)]
                            [else (eprintf \"Warning: unhandled Unicode property ~s~n\" prop) #f]))
                    (and (if is-negated (not matches?) matches?) #t))
                  (find-end (+ j 1))))]
           ;; Handle character range: a-z
           [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\\-)
                 (char<=? (string-ref pat i) c (string-ref pat (+ i 2)))) #t]
           [(char=? (string-ref pat i) c) #t]
           [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\\-)) (loop (+ i 3))]
           [else (loop (+ i 1))])))")

(define (gen-lexer-matchers all-lexer)
  (string-join
   (map gen-one-lexer-matcher all-lexer)
   "\n"))

(define (gen-one-lexer-matcher rule)
  (define raw-name (any-tree-text (first (any-tree-children rule))))
  (define name (mangle raw-name))
  (define alts-node (second (any-tree-children rule)))
  (define alts (any-tree-children alts-node))
  (format "(define (~a-match p in) ~a)"
          name
          (gen-match-alt alts)))

(define (gen-match-alt alts)
  (if (= (length alts) 1)
      (gen-match-seq (any-tree-children (car alts)))
      (format "(malt (list ~a) p in)"
              (string-join (map (lambda (a) (gen-match-seq-fn (any-tree-children a))) alts) " "))))

(define (gen-match-seq-fn elems)
  (format "(lambda (p in) ~a)" (gen-match-seq elems)))

(define (gen-match-seq elems)
  ;; Filter out action elements (embedded code blocks)
  (define real-elems (filter (lambda (e) (not (eq? (any-tree-tag e) 'action))) elems))
  (if (= (length real-elems) 1)
      (format "(~a p in)" (gen-match-elem (car real-elems)))
      (format "(mseq (list ~a) p in)"
              (string-join (map gen-match-elem real-elems) " "))))

;; Convert ANTLR4 character representation to Racket character literal text.
;; Examples: "a" → "#\\a", "\\n" → "#\\newline", "\\u1885" → "#\\u1885"
(define (g4-char->racket ch-str)
  (define racket-escapes
    '(("\\n" . "newline") ("\\r" . "return") ("\\t" . "tab")
      ("\\f" . "page") ("\\\\" . "\\\\") ("\\'" . "'") ("\\\"" . "\"")))
  (cond
    [(assoc ch-str racket-escapes) => (lambda (pair) (format "#\\~a" (cdr pair)))]
    [(and (>= (string-length ch-str) 2) (string=? (substring ch-str 0 2) "\\u"))
     (format "#\\u~a" (substring ch-str 2))]
    [else (format "#\\~a" ch-str)]))

(define (gen-match-elem elem)
  (define tag (any-tree-tag elem))
  (cond
    [(eq? tag 'literal)  (format "(lambda (p i) (mlit ~s p i))" (unescape-g4-literal (any-tree-text elem)))]
    [(eq? tag 'char-class) (format "(lambda (p i) (mcclass ~s p i))" (unescape-char-class (any-tree-text elem)))]
    [(eq? tag 'range)
      (define lo (any-tree-text (first (any-tree-children elem))))
      (define hi (any-tree-text (second (any-tree-children elem))))
      (format "(lambda (p i) (mrange ~a ~a p i))" (g4-char->racket lo) (g4-char->racket hi))]
    [(eq? tag 'token-ref)
      (define ref-name (any-tree-text elem))
      (if (string=? ref-name "EOF")
          "(lambda (p i) (if (>= p (string-length i)) (list p \"EOF\") #f))"
          (format "~a-match" (mangle ref-name)))]
    [(eq? tag 'star)    (format "(lambda (p i) (mstar ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'plus)    (format "(lambda (p i) (mplus ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'optional) (format "(lambda (p i) (mopt ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'negated)
     (define child (first (any-tree-children elem)))
     (format "(lambda (p i) (mnot ~a p i))" (gen-match-elem child))]
    [(eq? tag 'group)
      (define sub-alts (any-tree-children elem))
      (format "(lambda (p i) ~a)" (gen-match-alt sub-alts))]
    [(eq? tag 'action) (format "(lambda (p i) (list p \"\"))")] ;; skip actions in lexer
    [(eq? tag 'any) (format "(lambda (p i) (mnot (lambda (p2 i2) #f) p i))")] ;; . matches any char
    [else (error "unknown match elem tag:" tag)]))

(define (build-mode-map lexer-rules mode-nodes implicit-rules)
  (define mode-entries
    (for/list ([mn mode-nodes])
      (define children (any-tree-children mn))
      (define mode-name (any-tree-text (first children)))
      (define mode-rules (rest children))
      (cons mode-name mode-rules)))
  (cons (cons 'default (append lexer-rules implicit-rules))
        mode-entries))

(define (gen-tokenizer mode-map #:has-newline? [has-newline? #f] #:indent-tokens? [indent-tokens? #f])
  (define mode-clauses
    (string-join
     (for/list ([(mode-name rules) (in-dict mode-map)])
       (gen-mode-section mode-name rules))
     "\n"))
  (define whitespace-handler
    (if has-newline?
        ;; Skip non-newline whitespace only; let newlines be matched by NEWLINE rule
        "            [(and (char-whitespace? ch) (not (char=? ch #\\newline)))
              (loop (+ p 1) l (+ c 1) (+ o 1) tks mode mstack pending)]\n"
        ;; Skip all whitespace including newlines
        "            [(char-whitespace? ch)
              (if (char=? ch #\\newline)
                  (loop (+ p 1) (+ l 1) 1 (+ o 1) tks mode mstack pending)
                  (loop (+ p 1) l (+ c 1) (+ o 1) tks mode mstack pending))]\n"))
  (string-append
   (format "(define (~a in)~n" (if indent-tokens? "tokenize-raw" "tokenize"))
   "   (define il (string-length in))
   (let loop ([p 0] [l 1] [c 1] [o 0] [tks '()] [mode 'default] [mstack '()] [pending #f])
     (if (>= p il)
         (let ([final-tks (if pending
                             (cons (token 'UNKNOWN pending (pos l c o) (pos l c o)) tks)
                             tks)])
           (reverse (cons (token 'EOF \"\" (pos l c o) (pos l c o)) final-tks)))
         (let ([ch (string-ref in p)])
           (cond\n"
   whitespace-handler
   mode-clauses
   "\n            [else (error 'tokenize \"unexpected char ~a in mode ~a at ~a:~a\" ch mode l c)])))))"
   (if indent-tokens?
       "\n(define (tokenize in) (insert-indents (tokenize-raw in)))"
       "")
   "\n"))

(define (gen-mode-section mode-name rules)
  ;; Generate a longest-match loop: try all matchers at position p,
  ;; pick the one that advances furthest. If tie, first defined wins.
  ;; This implements ANTLR4's maximal munch lexer strategy.
  (define clauses-str
    (string-join (map gen-token-clause rules) "\n"))
  (string-append
   (format "            [(eq? mode '~a)\n             (let __mloop ([__rules (list\n" mode-name)
   clauses-str
   ")]\n                                      [__best-np p] [__best-v #f] [__best-handle #f])\n"
   "               (if (null? __rules)\n"
   "                   (if __best-handle\n"
   "                       (__best-handle __best-np __best-v)\n"
    (format "                       (error 'tokenize \"no matching rule in mode ~a at ~~a:~~a\" l c))\n" mode-name)
   "                   (let ([__r ((caar __rules) p in)])\n"
   "                     (if (and __r (> (car __r) __best-np))\n"
   "                         (__mloop (cdr __rules) (car __r) (cadr __r) (cdar __rules))\n"
    "                         (__mloop (cdr __rules) __best-np __best-v __best-handle)))))]"))

(define (gen-token-clause rule)
  (define children (any-tree-children rule))
  (define raw-name (any-tree-text (first children)))
  (define mangled (mangle raw-name))
  (define commands
    (if (> (length children) 2)
        (drop children 2)
        '()))
  (define token-expr
    (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" raw-name)
        (format "'~a" raw-name)
        (format "(string->symbol ~s)" raw-name)))
  (define cmd-body (gen-command-body commands token-expr))
  (format "               (cons ~a-match (lambda (np v) ~a))"
          mangled cmd-body))

(define (gen-command-body commands token-expr)
  (if (null? commands)
      (format "(define sl (string-length v)) (define tk (token ~a v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) mode mstack pending)" token-expr)
      (let loop ([cmds commands]
                 [skip? #f]
                 [more? #f]
                 [emit-type token-expr]
                 [next-mode "mode"]
                 [next-mstack "mstack"]
                 [next-pending "pending"])
        (if (null? cmds)
            (cond [skip? (format "(loop np l c o tks ~a ~a ~a)" next-mode next-mstack next-pending)]
                  [more? (format "(loop np l c o tks ~a ~a ~a)" next-mode next-mstack next-pending)]
                  [else (format "(define sl (string-length v)) (define tk (token ~a v (pos l c o) (pos l (+ c sl) (+ o sl)))) (loop np l (+ c sl) (+ o sl) (cons tk tks) ~a ~a ~a)"
                                emit-type next-mode next-mstack next-pending)])
            (let ([cmd (car cmds)])
              (define txt (any-tree-text cmd))
              (cond [(string=? txt "skip")
                     (loop (cdr cmds) #t more? emit-type next-mode next-mstack next-pending)]
                    [(string-prefix? txt "pushMode(")
                     (define mname (substring txt 9 (sub1 (string-length txt))))
                     (loop (cdr cmds) skip? more? emit-type (format "'~a" mname) (format "(cons mode ~a)" next-mstack) next-pending)]
                    [(string=? txt "popMode")
                     (loop (cdr cmds) skip? more? emit-type
                           "(if (null? mstack) 'default (car mstack))"
                           "(if (null? mstack) '() (cdr mstack))"
                           next-pending)]
                    [(string=? txt "more")
                     (loop (cdr cmds) skip? #t emit-type next-mode next-mstack
                           "(if pending (string-append pending v) v)")]
                    [(string-prefix? txt "type(")
                     (define tname (substring txt 5 (sub1 (string-length txt))))
                     (define texpr
                       (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" tname)
                           (format "'~a" tname)
                           (format "(string->symbol ~s)" tname)))
                     (loop (cdr cmds) skip? more? texpr next-mode next-mstack next-pending)]
                    [else (loop (cdr cmds) skip? more? emit-type next-mode next-mstack next-pending)]))))))
(define (gen-parser-helpers)
  "
(define (ctok tks pos)
  (if (< pos (length tks)) (list-ref tks pos)
      (token 'EOF \"\" (source-pos 0 0 0) (source-pos 0 0 0))))

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
  (cdr (child-range child)))")

(define (gen-parser-rules parser-rules)
  (string-join (map gen-one-parser-rule parser-rules) "\n\n"))

(define (gen-one-parser-rule rule)
  (define name (any-tree-text (first (any-tree-children rule))))
  (define alts-node (second (any-tree-children rule)))
  (define all-alts (any-tree-children alts-node))
  ;; Split alternatives into primary (non-left-recursive) and
  ;; binary (left-recursive, starting with self-ref) groups
  (define primaries '())
  (define binaries '())
  (for ([alt all-alts])
    (define elems (any-tree-children alt))
    (define real-elems (filter (lambda (e) (not (member (any-tree-tag e) '(action label)))) elems))
    (if (and (pair? real-elems)
             (eq? (any-tree-tag (first real-elems)) 'rule-ref)
             (string=? (any-tree-text (first real-elems)) name))
        (set! binaries (cons alt binaries))
        (set! primaries (cons alt primaries))))
  (set! primaries (reverse primaries))
  (set! binaries (reverse binaries))
  (if (null? binaries)
      ;; No left recursion — use original direct-or pattern
      (format "(define (parse-~a tks pos)\n  (or ~a\n      #f))"
              name
              (string-join (map (lambda (a) (gen-parser-alt a name)) primaries) "\n      "))
      ;; Left recursion — generate iterative accumulation form
      (gen-leftrec-rule name primaries binaries)))

;; Generate a left-recursion-free iterative parser for a rule with
;; primary alternatives (base cases) and binary alternatives (op rhs).
;; The pattern: parse a primary, then loop trying binary suffixes,
;; accumulating left-associatively.
(define (gen-leftrec-rule name primaries binaries)
  (define primary-code
    (if (null? primaries)
        "#f"
        (string-join (map (lambda (a) (gen-parser-alt a name)) primaries) "\n               ")))
  (define bin-clauses
    (for/list ([alt binaries])
      (define elems (any-tree-children alt))
      (define real-elems (filter (lambda (e) (not (member (any-tree-tag e) '(action label)))) elems))
      ;; Drop the first element (the left-recursive self-ref to this rule)
      (define suffix (cdr real-elems))
      (gen-leftrec-clause suffix name)))
  (string-append
   (format "(define (parse-~a tks pos)\n" name)
   "  (define (prim tks pos)\n"
   (format "    (or ~a\n        #f))\n" primary-code)
   "  (let ([r (prim tks pos)])\n"
   "    (and r\n"
   "         (let bin-loop ([p (car r)] [acc (cadr r)])\n"
   (format "           (or ~a\n"
           (string-join bin-clauses "\n               "))
   "               (list p acc))))))\n"))

;; Generate one let* clause for a binary left-recursive suffix.
;; The suffix is the sequence of elements after the initial self-ref.
;; Any self-ref (rule-ref to name) in the suffix calls (prim tks pos).
(define (gen-leftrec-clause suffix name)
  (define N (length suffix))
  (if (zero? N)
      "(list p acc)" ;; degenerate: no suffix (shouldn't happen for valid grammar)
      (let build ([i 0])
        (define e (list-ref suffix i))
        (define pos-ref (if (zero? i) "p" (format "(car r~a)" (- i 1))))
        (define expr (lrec-elem-expr e pos-ref name))
        (define body
          (if (= i (- N 1))
              ;; Final element: feed into bin-loop with accumulated node
              (format "(and r~a (bin-loop (car r~a) (node '~a (list acc ~a) #:start (child-start acc) #:end (child-end (cadr r~a)))))"
                      i i name
                      (string-join
                       (for/list ([j (in-range N)])
                         (format "(cadr r~a)" j))
                       " ")
                      (- N 1))
              ;; Intermediate: chain to next element
              (format "(and r~a ~a)" i (build (+ i 1)))))
        (format "(let ([r~a ~a]) ~a)" i expr body))))

;; Like elem-expr but replaces self-refs with (prim tks pos) calls.
(define (lrec-elem-expr e pos-ref self-name)
  (define tag (any-tree-tag e))
  (cond
    [(and (eq? tag 'rule-ref) (string=? (any-tree-text e) self-name))
     (format "(prim tks ~a)" pos-ref)]
    [else (elem-expr e pos-ref)]))

;; elem-expr without the surrounding let — just the expression part
(define (elem-expr e pos-ref)
  (define tag (any-tree-tag e))
  (cond
    [(eq? tag 'literal)
     (format "(expect-lit tks ~a ~s)" pos-ref (any-tree-text e))]
    [(eq? tag 'token-ref)
     (format "(expect-tok tks ~a '~a)" pos-ref (any-tree-text e))]
    [(eq? tag 'rule-ref)
     (format "(parse-~a tks ~a)" (any-tree-text e) pos-ref)]
    [(eq? tag 'star)
     (define child (first (any-tree-children e)))
     (format "(parse-star tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
    [(eq? tag 'plus)
     (define child (first (any-tree-children e)))
     (format "(parse-plus tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
    [(eq? tag 'optional)
     (define child (first (any-tree-children e)))
     (format "(parse-opt tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
    [(eq? tag 'group)
     (define sub-alts (any-tree-children e))
     (format "(parse-group tks ~a (list ~a))" pos-ref
             (string-join
              (for/list ([sa sub-alts])
                (format "(lambda (t p) ~a)"
                        (gen-parser-seq (any-tree-children sa) "group" "p")))
              " "))]
    [(eq? tag 'action) (format "(list ~a (list))" pos-ref)]
    [(eq? tag 'negated) (format "(list ~a (list))" pos-ref)]
    [(eq? tag 'labeled)
     (define inner-elem (second (any-tree-children e)))
     (elem-expr inner-elem pos-ref)]
    [(eq? tag 'append-labeled)
     (define inner-elem (second (any-tree-children e)))
     (elem-expr inner-elem pos-ref)]
    [else (error "unknown parser elem tag:" tag)]))

(define (gen-parser-alt alt rule-name)
  (define elems (any-tree-children alt))
  ;; Filter out action elements and alternative labels
  (define real-elems (filter (lambda (e) (not (member (any-tree-tag e) '(action label)))) elems))
  (if (null? real-elems)
      (format "(list pos (node '~a (list)))" rule-name)
      (gen-parser-seq real-elems rule-name)))

(define (gen-parser-seq elems rule-name [pos-var "pos"])
  (define N (length elems))
  (define (elem-expr e pos-ref)
    (define tag (any-tree-tag e))
    (cond
      [(eq? tag 'literal)
       (format "(expect-lit tks ~a ~s)" pos-ref (any-tree-text e))]
      [(eq? tag 'token-ref)
       (format "(expect-tok tks ~a '~a)" pos-ref (any-tree-text e))]
      [(eq? tag 'rule-ref)
       (format "(parse-~a tks ~a)" (any-tree-text e) pos-ref)]
      [(eq? tag 'star)
       (define child (first (any-tree-children e)))
       (format "(parse-star tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
      [(eq? tag 'plus)
       (define child (first (any-tree-children e)))
       (format "(parse-plus tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
      [(eq? tag 'optional)
       (define child (first (any-tree-children e)))
       (format "(parse-opt tks ~a (lambda (t p) ~a))" pos-ref (gen-parser-single child))]
      [(eq? tag 'group)
       (define sub-alts (any-tree-children e))
       (format "(parse-group tks ~a (list ~a))" pos-ref
               (string-join
                (for/list ([sa sub-alts])
                  (format "(lambda (t p) ~a)"
                          (gen-parser-seq (any-tree-children sa) "group" "p")))
                " "))]
      [(eq? tag 'action) (format "(list ~a (list))" pos-ref)] ;; skip actions
      [(eq? tag 'negated) (format "(list ~a (list))" pos-ref)] ;; skip negated token sets
      [(eq? tag 'labeled)
       (define inner-elem (second (any-tree-children e)))
       (elem-expr inner-elem pos-ref)]
      [(eq? tag 'append-labeled)
       (define inner-elem (second (any-tree-children e)))
       (elem-expr inner-elem pos-ref)]
      [else (error "unknown parser elem tag:" tag)]))
  (if (zero? N)
      (format "(list ~a (node '~a (list)))" pos-var rule-name)
      (let build ([i 0])
        (define e (list-ref elems i))
        (define tag (any-tree-tag e))
        (define pos-ref (if (zero? i) pos-var (format "(car r~a)" (- i 1))))
        (define body
          (if (= i (- N 1))
              ;; Final element: produce the result list, still checking for failure
              (format "(and r~a (list (car r~a) (node '~a (list ~a) #:start (child-start (cadr r0)) #:end (child-end (cadr r~a)))))"
                      i i rule-name
                      (string-join (for/list ([j (in-range N)])
                                     (format "(cadr r~a)" j)) " ")
                      (- N 1))
              ;; Intermediate: and ri (build (+ i 1))
              (format "(and r~a ~a)" i (build (+ i 1)))))
        (format "(let ([r~a ~a]) ~a)" i (elem-expr e pos-ref) body))))

(define (gen-parser-single elem)
  (define tag (any-tree-tag elem))
  (cond
    [(eq? tag 'literal)  (format "(expect-lit t p ~s)" (any-tree-text elem))]
    [(eq? tag 'token-ref) (format "(expect-tok t p '~a)" (any-tree-text elem))]
    [(eq? tag 'rule-ref)  (format "(parse-~a t p)" (any-tree-text elem))]
    [(eq? tag 'group)
     (define sub-alts (any-tree-children elem))
     (format "(parse-group t p (list ~a))"
             (string-join
              (for/list ([sa sub-alts])
                (format "(lambda (t p) ~a)"
                        (gen-parser-seq (any-tree-children sa) "group" "p")))
              " "))]
    [(eq? tag 'optional)
     (define child (first (any-tree-children elem)))
     (format "(parse-opt t p (lambda (t p) ~a))" (gen-parser-single child))]
    [(eq? tag 'star)
     (define child (first (any-tree-children elem)))
     (format "(parse-star t p (lambda (t p) ~a))" (gen-parser-single child))]
    [(eq? tag 'plus)
      (define child (first (any-tree-children elem)))
      (format "(parse-plus t p (lambda (t p) ~a))" (gen-parser-single child))]
    [(eq? tag 'action) "(list p (list))"] ;; skip actions within suffixed elements
    [(eq? tag 'negated) "(list p (list))"] ;; skip negated within suffixed elements
    [(eq? tag 'labeled)
     (define inner-elem (second (any-tree-children elem)))
     (gen-parser-single inner-elem)]
    [(eq? tag 'append-labeled)
     (define inner-elem (second (any-tree-children elem)))
     (gen-parser-single inner-elem)]
    [else (error "unsupported parser single elem:" tag)]))

(define (gen-parser-provides parser-rules)
  (define names
    (for/list ([rule parser-rules])
      (any-tree-text (first (any-tree-children rule)))))
  (format "(provide ~a)"
          (string-join (map (λ (n) (format "parse-~a" n)) names) " ")))

(define (gen-entry parser-rules)
  (define first-name (any-tree-text (first (any-tree-children (first parser-rules)))))
  (format "
(define (parse in)
  (define tks (tokenize in))
  (match-define (list fp res) (parse-~a tks 0))
  res)" first-name))

(define (detect-left-recursion parser-rules)
  ;; Check each parser rule for direct left-recursive alternatives
  ;; A left-recursive alternative starts with a rule-ref to the same rule
  ;; Left-recursive alternatives are filtered out during code generation.
  (for ([rule parser-rules])
    (define name (any-tree-text (first (any-tree-children rule))))
    (define alts-node (second (any-tree-children rule)))
    (define alts (any-tree-children alts-node))
    (for ([alt alts])
      (define elems (any-tree-children alt))
      ;; Skip alternative label leaves
      (define first-elem (findf (lambda (e) (not (eq? (any-tree-tag e) 'label))) elems))
      (when (and first-elem
                 (eq? (any-tree-tag first-elem) 'rule-ref)
                 (string=? (any-tree-text first-elem) name))
        (eprintf "~nWARNING: Left-recursive alternative in rule '~a' — eliminating via iteration.~n" name)
        (eprintf "  The generated parser handles this using an accumulator loop.~n~n")))))

(define (gen-lexer-entry)
  ;; For lexer-only grammars: provide a parse that tokenizes and returns tokens as-is
  "
(define (parse in)
  (tokenize in))")

(module+ main
  (displayln "gend-parser loaded."))
