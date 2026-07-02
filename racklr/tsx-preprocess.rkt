#lang racket

(require racket/string
         racklr/uir)

(provide find-all-jsx extract-jsx preprocess-tsx restore-jsx preprocess-hooks
          advance-past-string preprocess-jsx-expression-embeds
          process-jsx-expr-conditionals
          id-start? id-cont? skip-id context-allows-jsx?)

;; ── TSX Preprocessor ───────────────────────────────────────────────

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

;; ── JSX Extraction: Bracket-counting scanner ──────────────────────

;; Scan from start (pointing at '<') to find the matching end of a JSX
;; expression. Uses simple bracket counting: <tag opens, </tag closes,
;; { opens expr, } closes expr. Handles nested elements recursively.
;;
;; Returns (values jsx-string end-position) or (values #f start).

(define (extract-jsx s start)
  ;; s must have '<' at position start
  (unless (and (< start (string-length s))
               (char=? (string-ref s start) #\<))
    (error 'extract-jsx "expected < at position ~a in ~s" start s))
  
  (define len (string-length s))
  
  (define (skip-tag-name pos)
    ;; Return position after tag name (or same pos if no name found)
    (if (and (< pos len) (id-start? (string-ref s pos)))
        (skip-id s pos)
        pos))
  
  (let scan ([pos (+ start 1)]    ;; skip initial '<'
             [depth 1]             ;; we've entered one <tag>
             [brace-depth 0]
             [in-expr #f]
             [state 'in-tag-name])  ;; in-tag-name | in-attrs | in-children | in-closing-tag-name
    (cond [(= depth 0) (values (substring s start pos) (- pos 1))]
          [(>= pos len) (values #f start)]
          
          ;; Handle string literals first (any state)
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (scan (advance-past-string s pos (string-ref s pos))
                 depth brace-depth in-expr state)]
          
          ;; Handle expresion braces in JSX children/attrs
          [(char=? (string-ref s pos) #\{)
           (scan (+ pos 1) depth (+ brace-depth 1) #t state)]
          
          [(and (> brace-depth 0) (char=? (string-ref s pos) #\}))
           (scan (+ pos 1) depth (- brace-depth 1) in-expr state)]
          
          ;; Handle < in children — could be </tag> or nested <tag>
          [(and (char=? (string-ref s pos) #\<)
                (< (+ pos 1) len)
                (not (eq? state 'in-tag-name)))
           (define c2 (string-ref s (+ pos 1)))
           (cond [(char=? c2 #\/)
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
                        (values #f start)))]
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
                  (id-start? (string-ref s (+ i 1))))
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
           ;; Found JSX tag inside expression braces
           (let-values ([(inner end-pos) (extract-jsx jsx-str pos)])
             (if inner
                 (let* ([trimmed (string-trim inner)]
                        [cst (with-handlers ([exn:fail? (lambda (e) #f)])
                               (jsx-parser trimmed))]
                        [lowered (if cst
                                     (with-handlers ([exn:fail? (lambda (e) #f)])
                                       ((dynamic-require 'racklr/lower-jsx 'lower-jsx)
                                        cst #:tk-type tk-type #:tk-value tk-value))
                                     #f)]
                        [emitted (if lowered
                                     (emit-fn lowered)
                                     inner)])
                   (set! results (cons (list pos (+ end-pos 1) emitted) results))
                   (scan (+ end-pos 1) brace-depth))
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
  
  (define (process-cond-jsx-expr expr)
    (define m-and (regexp-match #rx"^(.+?) *&& *(<[a-zA-Z].+)$" expr))
    (define m-tern (regexp-match #rx"^(.+?) *[?] *(<[a-zA-Z].+) *: *(<[a-zA-Z].+)$" expr))
    (define m-inline (regexp-match #rx"^ *(<[a-zA-Z].+) *$" expr))
    (cond [m-and
           (define cond-expr (uir-jsx-expr (string-trim (cadr m-and))))
           (define jsx-lowered (lower-jsx-text (caddr m-and)))
           (if jsx-lowered
               (uir-if cond-expr jsx-lowered (uir-null))
               #f)]
          [m-tern
           (define cond-expr (uir-jsx-expr (string-trim (cadr m-tern))))
           (define then-lowered (lower-jsx-text (string-trim (caddr m-tern))))
           (define else-lowered (lower-jsx-text (string-trim (cadddr m-tern))))
           (if (and then-lowered else-lowered)
               (uir-if cond-expr then-lowered else-lowered)
               #f)]
          [m-inline
           (define jsx-lowered (lower-jsx-text (cadr m-inline)))
           (or jsx-lowered #f)]
          [else #f]))
  
  (walk uir))

;; ── Public API ──────────────────────────────────────────────────────

(define (preprocess-tsx source-text
                        #:jsx-parse [jsx-parser #f]
                        #:jsx-lower-tk-type [tk-type #f]
                        #:jsx-lower-tk-value [tk-value #f])
  (define regions (find-all-jsx source-text))
  (define sorted-regions (sort regions > #:key first))
  
  (define jsx-map (make-hash))
  (define processed source-text)
  
  (for ([(region idx) (in-indexed sorted-regions)])
    (match-define (list start end jsx-str) region)
    (define idx-str
      (if (< idx 26)
          (string (integer->char (+ (char->integer #\a) idx)))
          (number->string idx)))
    (define sentinel (format "__JSX_~a__" idx-str))
    (hash-set! jsx-map sentinel jsx-str)
    (define before (substring processed 0 start))
    (define after (substring processed end (string-length processed)))
    (set! processed (string-append before sentinel after)))
  
  (define jsx-uir (make-hash))
  (when (and jsx-parser tk-type tk-value)
    ;; Lower each JSX string to UIR
    (for ([(sentinel jsx-src) (in-hash jsx-map)])
      (define trimmed (string-trim jsx-src))
      (define cst
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (jsx-parser trimmed)))
      (when cst
        (define lowered
          (with-handlers ([exn:fail? (lambda (e) #f)])
            ((dynamic-require 'racklr/lower-jsx 'lower-jsx)
             cst #:tk-type tk-type #:tk-value tk-value)))
        (when lowered
          ;; Process conditional JSX in expression children
          (define processed (process-jsx-expr-conditionals lowered jsx-parser tk-type tk-value))
          (hash-set! jsx-uir sentinel processed)))))
  
  (values processed jsx-map jsx-uir))

(define (restore-jsx uir jsx-uir-map)
  (let walk ([node uir])
    (match node
      [(? uir-var?)
       ;; Check if this var references a JSX sentinel
       (define inner (uir-var-name node))
       (if (and (uir-symbol? inner)
                (hash-has-key? jsx-uir-map (uir-symbol-name inner)))
           (hash-ref jsx-uir-map (uir-symbol-name inner))
           (struct-copy uir-var node [name (walk inner)]))]
      [(? uir-symbol?)
       (define name (uir-symbol-name node))
       (if (hash-has-key? jsx-uir-map name)
           (hash-ref jsx-uir-map name)
           node)]
      [(? uir-list?)
       (struct-copy uir-list node
                    [items (map walk (uir-list-items node))])]
      [(? uir-record?)
       (struct-copy uir-record node
                    [entries (for/list ([e (uir-record-entries node)])
                               (cons (car e) (walk (cdr e))))])]
      [(? uir-call?)
       (struct-copy uir-call node
                    [callee (walk (uir-call-callee node))]
                    [args (map walk (uir-call-args node))])]
      [(? uir-let?)
       (struct-copy uir-let node
                    [name (uir-let-name node)]
                    [value (walk (uir-let-value node))]
                    [body (walk (uir-let-body node))])]
      [(? uir-set!?)
       (struct-copy uir-set! node
                    [name (uir-set!-name node)]
                    [value (walk (uir-set!-value node))])]
      [(? uir-ann-set!?)
       (struct-copy uir-ann-set! node
                    [lhs (walk (uir-ann-set!-lhs node))]
                    [type (and (uir-ann-set!-type node) (walk (uir-ann-set!-type node)))]
                    [value (and (uir-ann-set!-value node) (walk (uir-ann-set!-value node)))])]
      [(? uir-if?)
       (struct-copy uir-if node
                    [test (walk (uir-if-test node))]
                    [then (walk (uir-if-then node))]
                    [else (walk (uir-if-else node))])]
      [(? uir-block?)
       (struct-copy uir-block node
                    [stmts (map walk (uir-block-stmts node))])]
      [(? uir-return?)
       (struct-copy uir-return node
                    [value (walk (uir-return-value node))])]
      [(? uir-for-each?)
       (struct-copy uir-for-each node
                    [var (uir-for-each-var node)]
                    [iterable (walk (uir-for-each-iterable node))]
                    [body (walk (uir-for-each-body node))]
                    [else-body (and (uir-for-each-else-body node) (walk (uir-for-each-else-body node)))])]
      [(? uir-while?)
       (struct-copy uir-while node
                    [test (walk (uir-while-test node))]
                    [body (walk (uir-while-body node))]
                    [else-body (and (uir-while-else-body node) (walk (uir-while-else-body node)))])]
      [(? uir-get?)
       (struct-copy uir-get node
                    [base (walk (uir-get-base node))]
                    [field (uir-get-field node)])]
      [(? uir-paren?)
       (struct-copy uir-paren node
                    [inner (walk (uir-paren-inner node))])]
      [(? uir-fn?)
       (struct-copy uir-fn node
                    [name (uir-fn-name node)]
                    [params (for/list ([p (uir-fn-params node)])
                              (walk p))]
                    [body (walk (uir-fn-body node))]
                     [return-type (and (uir-fn-return-type node) (walk (uir-fn-return-type node)))])]
      [_ node])))

;; ── React Hooks → Vanilla JS source-level preprocessing ─────────────

(define (preprocess-hooks source)
  ;; Step 1: Remove react imports
  ;;   import { X, Y } from "react";
  ;;   import React, { X } from "react";
  ;;   import React from "react";
  (define rx-import-braces #px"import[[:space:]]+\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define rx-import-combo  #px"import[[:space:]]+[[:word:]]+[[:space:]]*,[[:space:]]*\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define rx-import-default #px"import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  
  (define s0 (regexp-replace* rx-import-braces source ""))
  (define s1 (regexp-replace* rx-import-combo s0 ""))
  (define s2 (regexp-replace* rx-import-default s1 ""))
  
  ;; Step 2: Transform const [state, setState] = useState(init);
  ;;         → let state = init; let setState = function(v) { state = v; };
  (define rx-useState #px"const[[:space:]]+\\[[[:space:]]*([[:word:]]+)[[:space:]]*,[[:space:]]*([[:word:]]+)[[:space:]]*\\][[:space:]]*=[[:space:]]*useState[[:space:]]*\\([[:space:]]*([^)]*)[[:space:]]*\\)[[:space:]]*;?")
  (define s3 (regexp-replace* rx-useState s2 "let \\1 = \\3; let \\2 = function(v) { \\1 = v; };"))
  
  ;; Step 3: Transform useEffect(callback, deps) → (callback)();
  (define rx-useEffect #px"useEffect[[:space:]]*\\([[:space:]]*(\\([^)]*\\)[[:space:]]*=>[^,]+)[[:space:]]*,[[:space:]]*\\[[^\\]]*\\][[:space:]]*\\)[[:space:]]*;?")
  (define s4 (regexp-replace* rx-useEffect s3 "(\\1)();"))
  
  s4)
