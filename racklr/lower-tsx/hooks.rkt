#lang racket

(require racket/string
         "helpers.rkt")

(provide preprocess-hooks preprocess-imports)

;; ── Brace-matching helper (needed by insert-type-separators) ─────────

(define (find-matching-brace s open-pos)
  (define len (string-length s))
  (unless (and (< open-pos len) (char=? (string-ref s open-pos) #\{))
    (error 'find-matching-brace "expected { at ~a" open-pos))
  (let loop ([pos (+ open-pos 1)] [depth 1])
    (cond [(= depth 0) (- pos 1)]
          [(>= pos len) #f]
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (loop (advance-past-string s pos (string-ref s pos)) depth)]
          [(char=? (string-ref s pos) #\{) (loop (+ pos 1) (+ depth 1))]
          [(char=? (string-ref s pos) #\}) (loop (+ pos 1) (- depth 1))]
          [else (loop (+ pos 1) depth)])))

;; ── B63: Strip ; from type/interface member lines (ANTLR grammar doesn't accept ;) ─

(define (strip-type-semicolons source)
  ;; Scan for `type X = {` or `interface X ... {` blocks, and within each,
  ;; strip trailing `;` from member lines.
  (define rx-type-start #px"(?:type|interface)\\s+\\w+\\s*(?:[^{]*?)\\s*\\{")
  (let loop ([s source] [start 0])
    (define m (regexp-match-positions rx-type-start s start (string-length s)))
    (if (not m)
        s
        (let* ([match-end (cdar m)]
               [brace-pos (- match-end 1)]  ;; position of the {
               [after-brace (substring s brace-pos)]
               [close-pos (find-matching-brace after-brace 0)])
          (if (not close-pos)
              ;; Unmatched brace — skip past the { and continue
              (loop s (+ brace-pos 1))
              (let* ([block-content (substring after-brace 1 close-pos)]
                     [fixed-block (strip-semicolons-from-lines block-content)]
                     [before (substring s 0 brace-pos)]
                     [close-abs (+ brace-pos close-pos)]
                     [after (substring s (add1 close-abs))])
                (define new-s (string-append before "{" fixed-block "}" after))
                (loop new-s (+ (string-length before) 1 (string-length fixed-block) 1))))))))

(define (strip-semicolons-from-lines block-str)
  ;; Strip trailing ; from each non-empty line in the block.
  (define lines (string-split block-str "\n"))
  (string-join
   (for/list ([line (in-list lines)])
     (cond [(regexp-match #px";\\s*$" (string-trim line))
            ;; Replace the last ; on the line, preserving indentation and trailing content
            (regexp-replace #px";(\\s*)$" line "\\1")]
           [else line]))
   "\n"))

;; ── Import stripping (regex — keeps source valid for TS parser) ─────

(define (preprocess-imports source)
  ;; Step 1: Remove react imports
  (define rx-import-braces #px"import[[:space:]]+\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define rx-import-combo  #px"import[[:space:]]+[[:word:]]+[[:space:]]*,[[:space:]]*\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define rx-import-default #px"import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  
  (define s0 (regexp-replace* rx-import-braces source ""))
  (define s1 (regexp-replace* rx-import-combo s0 ""))
  (define s2 (regexp-replace* rx-import-default s1 ""))

  ;; Step 1.5: Remove CSS module imports (B28)
  (define rx-css-module #px"import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"'][^\"']*\\.module\\.css[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define s3 (regexp-replace* rx-css-module s2 ""))

  ;; Step 1.6: Normalize double-spaces after { (ANTLR tokenizer quirk: {  → different token)
  (define s4 (regexp-replace* #px"\\{\\s{2,}" s3 "{ "))

  ;; Step 1.7: Strip ; from type/interface member lines (B63)
  ;; The ANTLR TS grammar does not accept ; as a type member separator.
  (strip-type-semicolons s4))

;; ── Identifier scanning helpers ─────────────────────────────────────

(define (id-start? c)
  (or (char-alphabetic? c) (char=? c #\_) (char=? c #\$)))

(define (id-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\$)))

(define (skip-id s pos)
  (define len (string-length s))
  (let loop ([p pos])
    (if (and (< p len) (id-char? (string-ref s p)))
        (loop (+ p 1))
        p)))

;; ── Paren/brace-counting helpers ─────────────────────────────────────

(define (skip-whitespace s pos)
  (define len (string-length s))
  (let loop ([p pos])
    (if (and (< p len) (char-whitespace? (string-ref s p)))
        (loop (+ p 1))
        p)))

;; Find matching close-paren for open-paren at open-pos.
;; Returns position of matching close-paren, or #f.
(define (find-matching-paren s open-pos)
  (define len (string-length s))
  (unless (and (< open-pos len) (char=? (string-ref s open-pos) #\())
    (error 'find-matching-paren "expected ( at ~a" open-pos))
  (let loop ([pos (+ open-pos 1)] [depth 1])
    (cond [(= depth 0) (- pos 1)]  ;; position of the matching )
          [(>= pos len) #f]
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (loop (advance-past-string s pos (string-ref s pos)) depth)]
          [(char=? (string-ref s pos) #\() (loop (+ pos 1) (+ depth 1))]
          [(char=? (string-ref s pos) #\)) (loop (+ pos 1) (- depth 1))]
          [else (loop (+ pos 1) depth)])))

;; Extract N comma-separated arguments from inside parens.
;; s: source, open-pos: position of '(' for the call.
;; Returns (list arg1-str arg2-str ... argN-str) up to N args.
(define (extract-call-args s open-pos #:max-args [max-args 3])
  (define close-pos (find-matching-paren s open-pos))
  (unless close-pos (error 'extract-call-args "unmatched paren at ~a" open-pos))
  (define args-str (substring s (+ open-pos 1) close-pos))
  (split-args args-str max-args))

;; Split a comma-separated argument string into up to N arguments,
;; respecting nested parens, braces, brackets, and strings.
(define (split-args s max-n)
  (define len (string-length s))
  (define args '())
  (define arg-start 0)
  (let loop ([pos 0])
    (cond [(or (>= pos len) (>= (length args) (- max-n 1)))
           ;; Last arg: take everything remaining
           (define last (string-trim (substring s arg-start len)))
           (set! args (append args (list last)))]
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (loop (advance-past-string s pos (string-ref s pos)))]
          [(char=? (string-ref s pos) #\() (loop (find-matching-paren s pos))]
          [(char=? (string-ref s pos) #\{) (loop (find-matching-brace s pos))]
          [(char=? (string-ref s pos) #\[)
           (define rb (find-matching-bracket s pos))
           (loop (or rb (+ pos 1)))]
          [(char=? (string-ref s pos) #\,)
           (define arg (string-trim (substring s arg-start pos)))
           (set! args (append args (list arg)))
           (set! arg-start (+ pos 1))
           (loop (+ pos 1))]
          [else (loop (+ pos 1))]))
  (if (>= (length args) max-n)
      (take args max-n)
      args))

;; Find matching close-bracket for open-bracket at open-pos.
(define (find-matching-bracket s open-pos)
  (define len (string-length s))
  (unless (and (< open-pos len) (char=? (string-ref s open-pos) #\[))
    (error 'find-matching-bracket "expected [ at ~a" open-pos))
  (let loop ([pos (+ open-pos 1)] [depth 1])
    (cond [(= depth 0) (- pos 1)]
          [(>= pos len) #f]
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (loop (advance-past-string s pos (string-ref s pos)) depth)]
          [(char=? (string-ref s pos) #\[) (loop (+ pos 1) (+ depth 1))]
          [(char=? (string-ref s pos) #\]) (loop (+ pos 1) (- depth 1))]
          [else (loop (+ pos 1) depth)])))

;; ── Robust hook preprocessing ────────────────────────────────────────

(define (preprocess-hooks source)
  ;; Strip imports first
  (define s (preprocess-imports source))
  
  ;; Scan positionally for hook calls and replace them inline.
  ;; Process right-to-left so positions stay valid.
  (define replacements '())  ;; list of (start end replacement-text)
  (define len (string-length s))
  
  (let scan ([pos 0])
    (when (< pos len)
      (define c (string-ref s pos))
      (cond
        ;; Skip strings
        [(or (char=? c #\') (char=? c #\"))
         (scan (advance-past-string s pos c))]
        
        ;; Look for hook calls: identifier starting at pos
        [(and (id-start? c) (< (+ pos 4) len))
         (define id-end (skip-id s pos))
         (define id-name (substring s pos id-end))
          
          (match id-name
           ["useState"
            (define open-paren (skip-whitespace s id-end))
            (when (and (< open-paren len) (char=? (string-ref s open-paren) #\())
              (define close-paren (find-matching-paren s open-paren))
              (when close-paren
                ;; Walk back from 'useState' to find 'const [state, setter] ='
                (define before (let bloop ([p (- pos 1)])
                                 (cond [(< p 0) -1]
                                       [(char-whitespace? (string-ref s p)) (bloop (- p 1))]
                                       [else p])))
                (when (and (>= before 0) (char=? (string-ref s before) #\=))
                  ;; Walk back past '=' and whitespace to find ']'
                  (define bracket-end (let eloop ([p (- before 1)])
                                        (cond [(< p 0) -1]
                                              [(char-whitespace? (string-ref s p)) (eloop (- p 1))]
                                              [(char=? (string-ref s p) #\]) p]
                                              [else -1])))
                  (when (>= bracket-end 0)
                    ;; Walk back through destructuring to find '['
                    (define bracket-start (let dloop ([p (- bracket-end 1)] [depth 1])
                                            (cond [(= depth 0) (+ p 1)]
                                                  [(< p 1) -1]
                                                  [(char=? (string-ref s p) #\]) (dloop (- p 1) (+ depth 1))]
                                                  [(char=? (string-ref s p) #\[) (dloop (- p 1) (- depth 1))]
                                                  [else (dloop (- p 1) depth)])))
                    (when (>= bracket-start 0)
                      ;; Extract [state, setter] → names
                      (define bracket-content (string-trim (substring s (+ bracket-start 1) bracket-end)))
                      (define names (string-split bracket-content ","))
                      (when (= (length names) 2)
                        (define state-name (string-trim (first names)))
                        (define setter-name (string-trim (second names)))
                        ;; Extract init value (first arg)
                        (define args (extract-call-args s open-paren #:max-args 2))
                        (when (pair? args)
                          (define init-val (first args))
                          (define replacement
                            (format "var ~a;\nif(window._s_~a!==undefined){~a=window._s_~a;}else{~a=~a;window._s_~a=~a;}\nvar ~a=function(v){window._s_~a=v;if(window._rerender){window._rerender();}};"
                                    state-name state-name state-name state-name state-name init-val state-name init-val
                                    setter-name state-name))
                          ;; Find full range to replace: from 'const' to close paren + optional semicolon
                          ;; Use bracket-start to find start of 'const' keyword
                          (define real-start
                            (let loop ([p (- bracket-start 1)])
                              (cond [(< p 0) 0]
                                    [(char-whitespace? (string-ref s p)) (loop (- p 1))]
                                    [else
                                     (let iloop ([q p])
                                       (if (and (>= q 0) (id-char? (string-ref s q)))
                                           (iloop (- q 1))
                                           (+ q 1)))])))
                          (define after-close (skip-whitespace s (+ close-paren 1)))
                          (define end-pos (if (and (< after-close len) (char=? (string-ref s after-close) #\;))
                                              (+ after-close 1) after-close))
                          (when (and (>= real-start 0) (>= end-pos 0))
                            (set! replacements (cons (list real-start end-pos replacement) replacements)))))))))
              (scan (+ id-end 1)))]
           
           ["useEffect"
            (define open-paren (skip-whitespace s id-end))
            (when (and (< open-paren len) (char=? (string-ref s open-paren) #\())
              (define close-paren (find-matching-paren s open-paren))
              (when close-paren
                (define args (extract-call-args s open-paren #:max-args 2))
                (when (>= (length args) 1)
                  (define callback (first args))
                  (define replacement
                    ;; Invoke callback as IIFE, capture cleanup return value
                    (format "var _fx = (~a)(); if (typeof _fx === 'function') { if (!window._cleanups) window._cleanups = []; window._cleanups.push(_fx); }"
                            callback))
                  (define start-pos (let sloop ([p (- pos 1)])
                                      (cond [(< p 0) 0]
                                            [(char-whitespace? (string-ref s p)) (sloop (- p 1))]
                                            [else (+ p 1)])))
                  (define after-close (skip-whitespace s (+ close-paren 1)))
                  (define end-pos (if (and (< after-close len) (char=? (string-ref s after-close) #\;))
                                      (+ after-close 1) after-close))
                  (set! replacements (cons (list start-pos end-pos replacement) replacements)))))
            (scan (+ id-end 1))]
           
            ["createContext"
            (define open-paren (skip-whitespace s id-end))
            (when (and (< open-paren len) (char=? (string-ref s open-paren) #\())
              (define close-paren (find-matching-paren s open-paren))
              (when close-paren
                (define args (extract-call-args s open-paren #:max-args 2))
                (when (pair? args)
                  (define default-val (string-trim (first args)))
                  (define replacement (string-append "{ _default: " default-val " }"))
                  (define start-pos (let sloop ([p (- pos 1)])
                                      (cond [(< p 0) 0]
                                            [(char-whitespace? (string-ref s p)) (sloop (- p 1))]
                                            [else (+ p 1)])))
                  (define end-pos (+ close-paren 1))
                  (set! replacements (cons (list start-pos end-pos replacement) replacements)))))
            (scan (+ id-end 1))]
           
           ["useContext"
            (define open-paren (skip-whitespace s id-end))
            (when (and (< open-paren len) (char=? (string-ref s open-paren) #\())
              (define close-paren (find-matching-paren s open-paren))
              (when close-paren
                (define args (extract-call-args s open-paren #:max-args 2))
                (when (pair? args)
                  (define ctx-expr (string-trim (first args)))
                  (define replacement (string-append ctx-expr "._default"))
                  (define start-pos (let sloop ([p (- pos 1)])
                                      (cond [(< p 0) 0]
                                            [(char-whitespace? (string-ref s p)) (sloop (- p 1))]
                                            [else (+ p 1)])))
                  (define end-pos (+ close-paren 1))
                  (set! replacements (cons (list start-pos end-pos replacement) replacements)))))
            (scan (+ id-end 1))]
           
           ["useReducer"
            (define open-paren (skip-whitespace s id-end))
            (when (and (< open-paren len) (char=? (string-ref s open-paren) #\())
              (define close-paren (find-matching-paren s open-paren))
              (when close-paren
                ;; Walk back to find 'const [state, dispatch] ='
                (define before (let bloop ([p (- pos 1)])
                                 (cond [(< p 0) -1]
                                       [(char-whitespace? (string-ref s p)) (bloop (- p 1))]
                                       [else p])))
                (when (and (>= before 0) (char=? (string-ref s before) #\=))
                  (define bracket-end (let eloop ([p (- before 1)])
                                        (cond [(< p 0) -1]
                                              [(char-whitespace? (string-ref s p)) (eloop (- p 1))]
                                              [(char=? (string-ref s p) #\]) p]
                                              [else -1])))
                  (when (>= bracket-end 0)
                    (define bracket-start (let dloop ([p (- bracket-end 1)] [depth 1])
                                            (cond [(= depth 0) (+ p 1)]
                                                  [(< p 1) -1]
                                                  [(char=? (string-ref s p) #\]) (dloop (- p 1) (+ depth 1))]
                                                  [(char=? (string-ref s p) #\[) (dloop (- p 1) (- depth 1))]
                                                  [else (dloop (- p 1) depth)])))
                    (when (>= bracket-start 0)
                      (define bracket-content (string-trim (substring s (+ bracket-start 1) bracket-end)))
                      (define names (string-split bracket-content ","))
                      (when (= (length names) 2)
                        (define state-name (string-trim (first names)))
                        (define dispatch-name (string-trim (second names)))
                        (define args (extract-call-args s open-paren #:max-args 2))
                        (when (>= (length args) 2)
                          (define reducer (string-trim (first args)))
                          (define initial (string-trim (second args)))
                          (define replacement
                            (format "let ~a = ~a; let ~a = function(action) { ~a = ~a(~a, action); };"
                                    state-name initial dispatch-name state-name reducer state-name))
                          ;; Find full range to replace: from 'const' to close paren + optional semicolon
                          (define real-start
                            (let loop ([p (- bracket-start 1)])
                              (cond [(< p 0) 0]
                                    [(char-whitespace? (string-ref s p)) (loop (- p 1))]
                                    [else
                                     (let iloop ([q p])
                                       (if (and (>= q 0) (id-char? (string-ref s q)))
                                           (iloop (- q 1))
                                           (+ q 1)))])))
                          (define after-close (skip-whitespace s (+ close-paren 1)))
                          (define end-pos (if (and (< after-close len) (char=? (string-ref s after-close) #\;))
                                              (+ after-close 1) after-close))
                          (when (and (>= real-start 0) (>= end-pos 0))
                            (set! replacements (cons (list real-start end-pos replacement) replacements))))))))))
            (scan (+ id-end 1))]
           
           [_ (scan (+ pos 1))])]
        
        [else (scan (+ pos 1))])))
  
  ;; Apply replacements right-to-left
  (define sorted (sort replacements > #:key first))
  (define result s)
  (for ([r sorted])
    (match-define (list start end replacement) r)
    (define before (substring result 0 start))
    (define after (substring result end (string-length result)))
    (set! result (string-append before replacement after)))
  
  result)
