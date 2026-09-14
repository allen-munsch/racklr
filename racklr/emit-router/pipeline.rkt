#lang racket

;; ── Page compilation pipeline ────────────────────────────────────────
;; page->js: preprocess → parse → lower → hook-lower → restore-jsx → emit.
;; Extracted from emit-router.rkt (B73) so the pipeline can be tested
;; independently of the HTML template, SPA bootstrap, and routing logic.
;; Parser functions are passed explicitly (no global parameter state).

(require racket/string
         racket/file
         racket/path
         racklr/tsx-preprocess
         racklr/emit-javascript
         racklr/uir
         racklr/eval-gsp-node
         (prefix-in ts-lower: racklr/lower-typescript))

(provide page->js)

(define (extract-server-props full-js)
  ;; Find getServerSideProps function, then extract return { props: { ... } }
  (define rx-gssp #rx"function getServerSideProps")
  (define m (regexp-match-positions rx-gssp full-js))
  (and m
       (let* ([fn-start (caar m)]
              [rx-ret #rx"return \\{ *props: *"]
              [rm (regexp-match-positions rx-ret full-js fn-start)])
         (and rm
              (let ([props-start (cdar rm)])
                (and (< props-start (string-length full-js))
                     (let loop ([pos props-start] [depth 1])
                       (cond [(= depth 0) (substring full-js props-start (- pos 1))]
                             [(>= pos (string-length full-js)) #f]
                             [(char=? (string-ref full-js pos) #\{) (loop (+ pos 1) (+ depth 1))]
                             [(char=? (string-ref full-js pos) #\}) (loop (+ pos 1) (- depth 1))]
                             [(or (char=? (string-ref full-js pos) #\")
                                  (char=? (string-ref full-js pos) #\'))
                              (define end (advance-past-string full-js pos (string-ref full-js pos)))
                              (loop end depth)]
                             [else (loop (+ pos 1) depth)]))))))))

(define (extract-static-props full-js)
  ;; Find the full getStaticProps function body and wrap as IIFE.
  ;; B61: If the function references Node.js APIs, evaluate via node at build time.
  (define rx-gsp #rx"function getStaticProps")
  (define m (regexp-match-positions rx-gsp full-js))
  (and m
       (let* ([fn-pos (cdar m)]
              [body-start (let loop ([pos fn-pos])
                            (and (< pos (string-length full-js))
                                 (if (char=? (string-ref full-js pos) #\{)
                                     (+ pos 1)
                                     (loop (+ pos 1)))))])
         (and body-start
              (let loop ([pos body-start] [depth 1])
                (cond [(= depth 0)
                       (define body (substring full-js body-start (- pos 1)))
                       ;; Check if body uses Node.js APIs (B61)
                       (define uses-node?
                         (or (regexp-match? #rx"fs\\." body)
                             (regexp-match? #rx"path\\." body)
                             (regexp-match? #rx"process\\." body)))
                       (if uses-node?
                           (eval-gsp-polyfill body)
                           (string-append "(function() {" body "})().props"))]
                      [(>= pos (string-length full-js)) #f]
                      [(char=? (string-ref full-js pos) #\{) (loop (+ pos 1) (+ depth 1))]
                      [(char=? (string-ref full-js pos) #\}) (loop (+ pos 1) (- depth 1))]
                      [(or (char=? (string-ref full-js pos) #\")
                           (char=? (string-ref full-js pos) #\'))
                       (let ([end (advance-past-string full-js pos (string-ref full-js pos))])
                         (loop end depth))]
                      [else (loop (+ pos 1) depth)]))))))

;; B61: Racket polyfill for Node.js APIs in getStaticProps/getStaticPaths.
;; Handles: process.cwd(), fs.readdirSync(literal), path.join(process.cwd(), literal)
;; Returns a JSON string for the props value, or #f if no polyfill applies.
(define (eval-gsp-polyfill body)
  (define escaped-quote
    (lambda (s) (regexp-replace* #rx"\"" s "\\\\\"")))

  (define (parse-frontmatter contents)
    (define lines (string-split contents "\n"))
    (if (and (pair? lines) (string=? (car lines) "---"))
        (let loop ([rest (cdr lines)] [pairs '()])
          (cond [(null? rest) #f]
                [(string=? (car rest) "---")
                 (reverse pairs)]
                [else
                 (define kv (regexp-match #rx"^([a-zA-Z_][a-zA-Z0-9_]*):[ \t]*(.+)$" (car rest)))
                 (if kv
                     (let ([value (caddr kv)]
                           [len (string-length (caddr kv))])
                       (loop (cdr rest)
                             (cons (cons (cadr kv)
                                         (if (and (> len 1)
                                                  (char=? (string-ref value 0) #\")
                                                  (char=? (string-ref value (- len 1)) #\"))
                                             (substring value 1 (- len 1))
                                             value))
                                   pairs)))
                     (loop (cdr rest) pairs))]))
        #f))

  ;; Resolve path.join(process.cwd(), "dir") → racket path
  (define (resolve-join-dir body)
    (define join-m (regexp-match #rx"path\\.join\\(process\\.cwd\\(\\),[ \n]*\"([^\"]+)\"\\)" body))
    (and join-m
         (build-path (current-directory) (cadr join-m))))

  ;; Resolve fs.readdirSync("dir") → list of filenames
  (define (resolve-readdir body #:base-dir [base-dir (current-directory)])
    (define readdir-m (regexp-match #rx"fs\\.readdirSync\\([ \n]*\"([^\"]+)\"[ \n]*\\)" body))
    (and readdir-m
         (let ([dir (build-path base-dir (cadr readdir-m))])
           (and (directory-exists? dir)
                (sort (map path->string (directory-list dir)) string<?)))))

  ;; Resolve process.cwd()
  (define cwd-str (path->string (current-directory)))

  ;; Build JSON props from detected patterns
  (define entries '())

  ;; Check for path.join + fs.readdirSync combo
  (define joined-dir (resolve-join-dir body))
  (define readdir-result
    (resolve-readdir body #:base-dir (or joined-dir (current-directory))))

  (when readdir-result
    (set! entries
          (cons (format "\"filenames\":[~a]"
                        (string-join
                         (map (lambda (f)
                                (string-append "\"" (escaped-quote f) "\""))
                              readdir-result)
                         ","))
                entries)))

  ;; Check for fs.readdirSync (already handled above via readdir-result)

  ;; Check for fs.readFileSync(path, 'utf8') with literal path
  (define readfile-m (regexp-match #rx"fs\\.readFileSync\\([ \n]*['\"]([^'\"]+)['\"][ \n]*,[ \n]*['\"]utf-?8['\"][ \n]*\\)" body))
  (when readfile-m
    (let ([fpath (build-path (or joined-dir (current-directory)) (cadr readfile-m))])
      (when (file-exists? fpath)
        (define contents (file->string fpath))
        (define fm (parse-frontmatter contents))
        (when (and fm (pair? fm))
          (set! entries
                (cons (string-join
                       (map (lambda (kv)
                              (format "\"~a\":\"~a\""
                                      (car kv)
                                      (escaped-quote (cdr kv))))
                            fm)
                       ",")
                      entries))))))

  ;; Check for process.cwd() directly
  (when (regexp-match? #rx"process\\.cwd\\(\\)" body)
    (set! entries
          (cons (format "\"cwd\":\"~a\"" (escaped-quote cwd-str))
                entries)))

  (if (null? entries)
      #f
      (string-append "{" (string-join (reverse entries) ",") "}")))

(define (s-u-v x)
  (cond [(uir-string? x) (uir-string-value x)]
        [(string? x) x]
        [else ""]))

(define (emit-head-attr-value-html v)
  (match v
    [(uir-string s) s]
    [(? string? s) s]
    [(uir-number n) n]
    [_ ""]))

(define (emit-head-node-html node)
  (match node
    [(? uir-element? e)
     (define tag (match (uir-element-tag e)
                   [(uir-string s) s]
                   [(? string? s) s]))
     (define attrs-str
       (string-join
        (for/list ([attr (uir-element-attrs e)])
          (match attr
            [(uir-attribute name value)
             (format " ~a=\"~a\""
                     (match name
                       [(uir-symbol s) (symbol->string s)]
                       [(? symbol? s) (symbol->string s)])
                     (emit-head-attr-value-html value))]
            [_ ""]))
        ""))
     (define children-str
       (string-join (map emit-head-node-html (uir-element-children e)) ""))
     (if (string=? children-str "")
         (format "<~a~a>" tag attrs-str)
         (format "<~a~a>~a</~a>" tag attrs-str children-str tag))]
    [(? uir-text-node? n)
     (match (uir-text-node-content n)
       [(uir-string s) s]
       [(? string? s) s])]
    [(? uir-jsx-expr? _) ""]
    [_ ""]))

(define (flatten-uir node)
  (cons node
        (append
         (match node
           [(? uir-element? e)
            (append (append-map flatten-uir (uir-element-children e))
                    (append-map flatten-uir (uir-element-attrs e)))]
           [(? uir-call? c)
            (append (flatten-uir (uir-call-callee c))
                    (append-map flatten-uir (uir-call-args c)))]
           [(? uir-block? b) (append-map flatten-uir (uir-block-stmts b))]
           [(? uir-set!? s) (flatten-uir (uir-set!-value s))]
           [(? uir-let? l) (append (flatten-uir (uir-let-value l))
                                   (flatten-uir (uir-let-body l)))]
           [(? uir-if? i) (append (flatten-uir (uir-if-test i))
                                  (flatten-uir (uir-if-then i))
                                  (flatten-uir (uir-if-else i)))]
           [(? uir-return? r) (flatten-uir (uir-return-value r))]
           [(? uir-fn? f) (flatten-uir (uir-fn-body f))]
           [(? uir-list? l) (append-map flatten-uir (uir-list-items l))]
           [(? uir-record? r) (append-map (lambda (p) (flatten-uir (cdr p)))
                                          (uir-record-entries r))]
           [(? uir-get? g) (flatten-uir (uir-get-base g))]
           [(? uir-spread? s) (flatten-uir (uir-spread-expr s))]
           [(? uir-paren? p) (flatten-uir (uir-paren-inner p))]
           [(? uir-jsx-expr? _) '()]
           [_ '()]))))

(define (collect-head-html uir)
  (define all-nodes (flatten-uir uir))
  (define head-children
    (append-map (lambda (n)
                  (if (and (uir-element? n)
                           (uir-string? (uir-element-tag n))
                           (string=? (uir-string-value (uir-element-tag n)) "head"))
                      (uir-element-children n)
                      '()))
                all-nodes))
  (if (null? head-children)
      ""
      (string-join (map emit-head-node-html head-children) "\n  ")))

;; Walk UIR and remove <head> elements from body (B60)
(define (strip-head-elements node)
  (match node
    [(? uir-element? e)
     (define tag (uir-element-tag e))
     (if (and (uir-string? tag) (string=? (uir-string-value tag) "head"))
         (uir-null)
         (struct-copy uir-element e
                      [children (map strip-head-elements (uir-element-children e))]))]
    [(? uir-call? c)
     (struct-copy uir-call c
                  [callee (strip-head-elements (uir-call-callee c))]
                  [args (map strip-head-elements (uir-call-args c))])]
    [(? uir-block? b)
     (struct-copy uir-block b
                  [stmts (map strip-head-elements (uir-block-stmts b))])]
    [(? uir-set!? s)
     (struct-copy uir-set! s [value (strip-head-elements (uir-set!-value s))])]
    [(? uir-let? l)
     (struct-copy uir-let l
                  [value (strip-head-elements (uir-let-value l))]
                  [body (strip-head-elements (uir-let-body l))])]
    [(? uir-if? i)
     (struct-copy uir-if i
                  [test (strip-head-elements (uir-if-test i))]
                  [then (strip-head-elements (uir-if-then i))]
                  [else (strip-head-elements (uir-if-else i))])]
    [(? uir-return? r)
     (struct-copy uir-return r [value (strip-head-elements (uir-return-value r))])]
    [(? uir-fn? f)
     (struct-copy uir-fn f [body (strip-head-elements (uir-fn-body f))])]
    [(? uir-list? l)
     (struct-copy uir-list l [items (map strip-head-elements (uir-list-items l))])]
    [(? uir-record? r)
     (struct-copy uir-record r
                  [entries (map (lambda (p) (cons (car p)
                                                  (strip-head-elements (cdr p))))
                                (uir-record-entries r))])]
    [(? uir-get? g)
     (struct-copy uir-get g [base (strip-head-elements (uir-get-base g))])]
    [(? uir-spread? s)
     (struct-copy uir-spread s [expr (strip-head-elements (uir-spread-expr s))])]
    [(? uir-paren? p)
     (struct-copy uir-paren p [inner (strip-head-elements (uir-paren-inner p))])]
    [_ node]))

(define (page->js ts-parse ts-tok-type ts-tok-value jsx-parse jsx-tok-type jsx-tok-value
                  source #:css-mapping [css-mapping #f]
                  #:project-root [project-root #f]
                  #:original-source [original-source #f])
  (define clean-src (preprocess-imports source))
  ;; B62: Strip relative data imports when Node eval handles data-fetching.
  ;; The original-source is the pre-resolve-imports source with intact imports.
  (define import-src (or original-source clean-src))
  (define clean-src2
    (if (and project-root (has-cross-file-imports? import-src))
        (regexp-replace* #px"import\\s+[^;]*from\\s+['\"]\\.{1,2}[^'\"]+['\"]\\s*;?\\s*\n?"
                         clean-src "")
        clean-src))
  (define-values (processed jsx-map jsx-uir)
    (preprocess-tsx clean-src2
                    #:jsx-parse jsx-parse
                    #:jsx-lower-tk-type jsx-tok-type
                    #:jsx-lower-tk-value jsx-tok-value))
  (define ts-cst (ts-parse processed))
  (define ts-uir (ts-lower:lower-program ts-cst ts-tok-type ts-tok-value))
  (define hooks-lowered (lower-hooks ts-uir))
  (define uir (restore-jsx hooks-lowered jsx-uir))
  (define head-html (collect-head-html uir))
  (define uir-no-head (strip-head-elements uir))
  (define full-js (emit-javascript uir-no-head))
  ;; Replace styles.CLASSNAME → "HASHED_CLASSNAME" if css-mapping provided
  (define css-replaced
    (if css-mapping
        (for/fold ([js full-js]) ([(class hashed) (in-hash css-mapping)])
          (regexp-replace* (regexp-quote (string-append "styles." class)) js
                           (string-append "\"" hashed "\"")))
        full-js))
  ;; Extract getStaticProps data before stripping (B17)
  ;; B62: If source has cross-file imports, try Node evaluation first
  (define static-props
    (if (and project-root (has-cross-file-imports? import-src))
        (or (eval-gsp-via-node import-src project-root)
            (extract-static-props css-replaced))
        (extract-static-props css-replaced)))
  ;; Extract getServerSideProps data (B26) — dynamic, server-side only placeholder
  (define server-props (extract-server-props css-replaced))
  ;; Strip data-fetching functions (getStaticProps, getServerSideProps)
  (define (strip-data-functions s)
    (define rx #rx"(export )?(async )?function (getStaticProps|getServerSideProps|getStaticPaths)")
    (define m (regexp-match-positions rx s))
    (if m
        (let* ([fn-start (caar m)]
               [sig-end (cdar m)]
               [body-start (let loop ([pos sig-end])
                             (and (< pos (string-length s))
                                  (if (char=? (string-ref s pos) #\{)
                                      (+ pos 1)
                                      (loop (+ pos 1)))))])
          (and body-start
               (let loop ([pos body-start] [depth 1])
                 (cond [(= depth 0)
                        (let trim-loop ([p pos])
                          (if (and (< p (string-length s))
                                   (memv (string-ref s p) '(#\; #\space #\newline)))
                              (trim-loop (+ p 1))
                              (strip-data-functions
                               (string-append (substring s 0 fn-start)
                                              (substring s p (string-length s))))))]
                       [(>= pos (string-length s)) s]
                       [(char=? (string-ref s pos) #\{) (loop (+ pos 1) (+ depth 1))]
                       [(char=? (string-ref s pos) #\}) (loop (+ pos 1) (- depth 1))]
                       [(or (char=? (string-ref s pos) #\")
                            (char=? (string-ref s pos) #\'))
                        (let ([end (advance-past-string s pos (string-ref s pos))])
                          (loop end depth))]
                       [else (loop (+ pos 1) depth)]))))
        s))
  (define no-data-fetching (strip-data-functions css-replaced))
  ;; Strip export keywords — page value is used inline in object literal.
  ;; Use multi-line mode so ^ matches after newlines.
  (define no-export-named  (regexp-replace* #rx"(?m:^export \\{[^}]*\\};?\n?)" no-data-fetching ""))
  (define no-export-default (regexp-replace* #rx"(?m:^export default )" no-export-named ""))
  (define no-export-decl   (regexp-replace* #rx"(?m:^export )" no-export-default ""))
  ;; Strip trailing junk (e.g. "null;" after exports)
  (define no-null (regexp-replace #rx"\\s*null;\\s*$" no-export-decl ""))
  ;; Strip trailing semicolons — values used inline in object literal
  (values (string-trim (regexp-replace #rx";\\s*$" no-null ""))
          static-props
          server-props
          head-html))
