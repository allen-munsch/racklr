#lang racket

(require racket/string
         racket/file
         racklr/eval-gsp-node
         racklr/esbuild-resolve
         racklr/tsx-preprocess
         racklr/emit-javascript
         racklr/lower-jsx
         racklr/lower-tsx/css-modules
         racklr/uir
         (prefix-in ts-lower: racklr/lower-typescript))

(provide emit-pages-html
         make-emit-pages-html
         discover-pages)

;; ── Factory: create emit-pages-html with pre-loaded parsers ───────────
;; Callers load parsers once and pass them in, avoiding gen-and-load
;; current-directory issues.

(define (make-emit-pages-html ts-parse ts-tokenize ts-tok-type ts-tok-value
                              jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  ;; Returns emit-pages-html function with baked-in parsers.

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

  ;; ── getStaticPaths extraction (B61) ──────────────────────────────
  ;; Returns list of param hashes for each path, or #f.
  (define (extract-static-paths full-js)
    (define rx-gsp #rx"function getStaticPaths")
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
                         (extract-paths-from-body (substring full-js body-start (- pos 1)))]
                        [(>= pos (string-length full-js)) #f]
                        [(char=? (string-ref full-js pos) #\{) (loop (+ pos 1) (+ depth 1))]
                        [(char=? (string-ref full-js pos) #\}) (loop (+ pos 1) (- depth 1))]
                        [(or (char=? (string-ref full-js pos) #\")
                             (char=? (string-ref full-js pos) #\'))
                         (let ([end (advance-past-string full-js pos (string-ref full-js pos))])
                           (loop end depth))]
                        [else (loop (+ pos 1) depth)]))))))

  (define (extract-paths-from-body body)
    (define paths-result '())
    (define rx-params #rx"params:[ \n]*\\{([^}]+)\\}")
    (define rx-kv #rx"([a-zA-Z_][a-zA-Z0-9_]*):[ \n]*['\"]([^'\"]+)['\"]")
    (let loop ([pos 0])
      (define m (regexp-match-positions rx-params body pos))
      (if m
          (let ([params-body (substring body (caar m) (cdar m))])
            (define param-hash (make-hash))
            (let kv-loop ([kpos 0])
              (define km (regexp-match-positions rx-kv params-body kpos))
              (if (and km (pair? (cdr km)) (pair? (cddr km)))
                  (let* ([key-start (caadr km)] [key-end (cdadr km)]
                         [val-start (caaddr km)] [val-end (cdaddr km)]
                         [key-str (substring params-body key-start key-end)]
                         [val-str (substring params-body val-start val-end)])
                    (hash-set! param-hash (string->symbol key-str) val-str)
                    (kv-loop (cdaddr km)))
                  (void)))
            (when (positive? (hash-count param-hash))
              (set! paths-result (cons param-hash paths-result)))
            (loop (cdar m)))
          (void)))
    (if (null? paths-result) #f (reverse paths-result)))

  ;; ── Head extraction (B60) ──────────────────────────────────────────
  ;; Walk UIR, collect <head> element children, emit as HTML for injection
  ;; into the HTML template <head> section.

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

  (define (page->js source #:css-mapping [css-mapping #f]
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

  (lambda (pages
           #:title [title "App"]
           #:all-files [all-files #f]
           #:path-to-entry [path-to-entry (lambda (p) p)]
           #:layout [layout #f]
           #:css-modules [css-modules (hash)]
           #:project-root [project-root #f])
    ;; pages: hash of URL-path (string) → source (string)
    ;;   Each source is a page component.
    ;;   If #:all-files is provided: hash of filename → source for the full project.
    ;;     #:path-to-entry maps URL path → filename in all-files (default: identity).
    ;;     Uses in-process import resolution to bundle per-page.
    ;;   #:layout (optional) TSX source for a shared layout component.
    ;;     Receives page props + { children: page-element }.
    ;;   #:css-modules: hash of URL-path → CSS content (string) for .module.css files.

    ;; Process CSS modules: for each URL path, produce (hashed-css, class→hashed mapping)
    (define css-module-data
      (for/hash ([(path css-content) (in-hash css-modules)])
        (define filename
          (cond [(equal? path "/") "index.module.css"]
                [else (string-append (string-replace path "/" "") ".module.css")]))
        (define-values (hashed-css mapping) (process-css-module filename css-content))
        (values path (list hashed-css mapping))))

    (define (resolve-page page-src url-path)
      (if all-files
          (resolve-imports all-files #:entry (path-to-entry url-path))
          page-src))

    ;; Compile optional layout through the pipeline (B19)
    (define layout-fn
      (and layout
           (let*-values ([(clean-layout) (preprocess-imports layout)]
                         [(processed _layout-jsx-map layout-jsx-uir)
                          (preprocess-tsx clean-layout
                                         #:jsx-parse jsx-parse
                                         #:jsx-lower-tk-type jsx-tok-type
                                         #:jsx-lower-tk-value jsx-tok-value)])
             (define layout-cst (ts-parse processed))
             (define layout-uir (ts-lower:lower-program layout-cst ts-tok-type ts-tok-value))
             (define hooks-lowered (lower-hooks layout-uir))
             (define layout-uir-jsx (restore-jsx hooks-lowered layout-jsx-uir))
             (define layout-js (emit-javascript layout-uir-jsx))
             ;; Strip export default and trailing semicolons
             (define no-export (regexp-replace* #rx"(?m:^export default )" layout-js ""))
             (define no-null (regexp-replace #rx"\\s*null;\\s*$" no-export ""))
             (string-trim (regexp-replace #rx";\\s*$" no-null "")))))

    (define page-entries
      (append*
       (for/list ([(path src-cons) (in-hash pages)])
         (define src (if (pair? src-cons) (car src-cons) src-cons))
         (define src-route-params (if (pair? src-cons) (cdr src-cons) #f))
         (define resolved (resolve-page src path))
         ;; Check for getStaticPaths before processing (B61)
         (define static-paths (extract-static-paths resolved))
         (define css-mapping
           (match (hash-ref css-module-data path #f)
             [(list _ mapping) mapping]
             [#f #f]))
          (define-values (page-js static-props server-props head-html)
            (page->js resolved #:css-mapping css-mapping
                      #:project-root project-root
                      #:original-source src))
         (define (make-entry p rp)
           (list p page-js static-props server-props rp head-html))
         (if static-paths
             ;; Generate one entry per static path, with concrete params
             (for/list ([params-hash (in-list static-paths)])
               (define slug-val (hash-ref params-hash 'slug #f))
               (make-entry (if slug-val
                               (regexp-replace #rx"/\\:[^/]+" path (string-append "/" slug-val))
                               path)
                           params-hash))
             (list (make-entry path src-route-params))))))

    ;; B33: Generate dynamic route pattern matching
    (define dynamic-patterns
      (for/list ([entry (in-list page-entries)]
                 #:when (fifth entry))
        (define path (first entry))
        (define params-hash (fifth entry))
        (define param-names (hash-keys params-hash))
        (list path param-names)))
    
    (define dynamic-match-js
      (if (null? dynamic-patterns)
          ""
          (string-join
           (list ""
                 "function _matchDynamic(path) {"
                 (string-join
                  (for/list ([dp (in-list dynamic-patterns)])
                    (match-define (list pattern param-names) dp)
                    (define esc-pattern (regexp-replace* #rx"\\." pattern "\\\\."))
                    (define param-name (first param-names))
                    (if (equal? param-names (list param-name))
                        (format "  var _m = path.match(/^~a\\/([^/]+)$/);\n  if (_m) return { ~a: _m[1] };"
                                (regexp-replace #rx"/\\:[^/]+$" esc-pattern "")
                                param-name)
                        ""))
                  "\n")
                 "  return null;"
                 "}")
           "\n")))
    
    (define mount-param-extract
      (if (null? dynamic-patterns)
          "          var params = null;"
          "          var params = _matchDynamic(path);"))

    ;; Assemble router JS
    (define router-lines
      (list
       (if layout-fn
           (format "var _layout = ~a;" layout-fn)
           "var _layout = null;")
       ""
       "var _pageData = {"
       (string-join
        (for/list ([entry (in-list page-entries)])
          (define path (first entry))
          (define props (third entry))
          (format "  \"~a\": ~a" path (or props "null")))
        ",\n")
       "};"
       ""
       "var _pages = {"
       (string-join
        (for/list ([entry (in-list page-entries)])
          (define path (first entry))
          (define js (second entry))
          (format "  \"~a\": ~a" path js))
        ",\n")
       "};"
       ""
       "var _serverData = {"
       (string-join
        (for/list ([entry (in-list page-entries)])
          (define path (first entry))
          (define srv (fourth entry))
          (format "  \"~a\": ~a" path (if srv (format "/* server-only */ ~a" srv) "null")))
        ",\n")
       "};"
       ""
       dynamic-match-js
       ""
       "function _mount(path) {"
       "  var app = document.getElementById(\"_app\");"
       "  var _cs = window._cleanups || [];"
       "  for (var _i = 0; _i < _cs.length; _i++) _cs[_i]();"
       "  window._cleanups = [];"
       "  app.innerHTML = \"\";"
       "  window._currentPath = path;"
       "  window._rerender = function() { _mount(window._currentPath); };"
       "  var pageFn = _pages[path];"
       (if (null? dynamic-patterns)
           "  var params = null;"
           "  var params = null;")
       "  if (!pageFn) {"
       "    params = _matchDynamic(path);"
       "    if (params) {"
       "      pageFn = _pages[path] || Object.keys(_pages).find(function(k) {"
       "        var r = new RegExp('^' + k.replace(/:[^/]+/g, '([^/]+)') + '$');"
       "        return r.test(path);"
       "      });"
       "      pageFn = pageFn ? _pages[pageFn] : null;"
       "    }"
       "  }"
       "  if (pageFn) {"
       "    var pageData = _pageData[path];"
       "    var serverData = _serverData[path];"
       "    if (pageData || serverData) pageData = Object.assign({}, serverData || {}, pageData || {});"
       "    if (params) pageData = Object.assign({}, pageData || {}, params);"
       "    var el = pageFn(pageData);"
       "    if (_layout) {"
       "      var merged = Object.assign({}, pageData || {}, { children: el });"
       "      el = _layout(merged);"
       "    }"
       "    if (el) app.appendChild(el);"
       "  }"
       "}"
       ""
       "window.addEventListener(\"DOMContentLoaded\", function() {"
       "  _mount(window.location.hash.slice(1) || \"/\");"
       "});"
       ""
       "window.addEventListener(\"hashchange\", function() {"
       "  _mount(window.location.hash.slice(1) || \"/\");"
       "});"))

    (define router-str (string-join router-lines "\n"))

    ;; Build navigation links
    (define nav-links
      (string-join
       (for/list ([entry (in-list page-entries)])
          (define path (first entry))
        (format "      <a href=\"#~a\">~a</a>"
                path
                (if (equal? path "/") "Home"
                    (string-titlecase (regexp-replace #rx"^/" path "")))))
       " | "))

    ;; Collect all <Head> content from pages for injection into HTML <head> (B60)
    (define head-content
      (string-join
       (for/list ([entry (in-list page-entries)]
                  #:unless (string=? (sixth entry) ""))
         (sixth entry))
       "\n  "))

    ;; Collect hashed CSS for <style> tags
    (define style-tags
      (string-join
       (for/list ([(path data) (in-hash css-module-data)])
         (match-define (list hashed-css _mapping) data)
         (format "  <style>/* ~a */\n~a\n  </style>" path hashed-css))
       "\n"))

    (string-append
     "<!DOCTYPE html>\n"
     "<html lang=\"en\">\n"
     "<head>\n"
     "  <meta charset=\"UTF-8\">\n"
     "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
     (format "  <title>~a</title>\n" title)
     (if (string=? head-content "")
         ""
         (string-append head-content "\n"))
     (if (positive? (hash-count css-module-data))
         (string-append style-tags "\n")
         "")
     "</head>\n"
     "<body>\n"
     "  <nav style=\"padding: 1rem; border-bottom: 1px solid #ccc; margin-bottom: 1rem;\">\n"
     nav-links "\n"
     "  </nav>\n"
     "  <div id=\"_app\"></div>\n"
     "  <script>\n"
     router-str "\n"
     "  </script>\n"
     "</body>\n"
     "</html>\n")))

;; ── B18: Pages directory discovery ───────────────────────────────────

(define (discover-pages dir)
  ;; Scan a directory for *.tsx files. Map filenames to URL paths.
  ;; B33: Dynamic route params: [slug].tsx → params stored alongside source.
  ;; Returns a hash: URL-path → (cons source params-hash-or-#f).
  (for/hash ([p (in-list (directory-list dir #:build? #f))]
              #:when (and (regexp-match #rx"\\.tsx$" (path->string p))
                          (not (string-prefix? (path->string p) "."))))
    (define name (path->string p))
    (define source (file->string (build-path dir p)))
    (define base (regexp-replace #rx"\\.tsx$" name ""))
    (define dm (regexp-match #rx"\\[([^]]+)\\]" base))
    (define-values (url-path params)
      (if dm
          (let* ([param (cadr dm)]
                 [static-part (string-replace base (car dm) "")]
                 [url (if (equal? static-part "")
                          (string-append "/:" param)
                          (string-append "/" static-part "/:" param))])
            (values url (hash param 'dynamic)))
          (let ([url (if (equal? name "index.tsx") "/"
                         (string-append "/" base))])
            (values url #f))))
    (values url-path (cons source params))))

;; ── Convenience: same API for callers that want auto-loading ──────────

(define emit-pages-html
  (let ([factory #f])
    (lambda args
      (unless factory
        (dynamic-require 'racklr/gen-test 'void) ;; ensure gen-test loaded
        (define g (dynamic-require 'racklr/gen-test 'gen-and-load))
        (define-values (p t tt tv)
          (g "grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))
        (define-values (jp jt jtt jtv)
          (g "grammars-v4/javascript/jsx-cleaned/JSXParser.g4"))
        (set! factory (make-emit-pages-html p t tt tv jp jt jtt jtv)))
      (apply factory args))))
