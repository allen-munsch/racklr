#lang racket

(require racket/string
         racklr/esbuild-resolve
         racklr/tsx-preprocess
         racklr/emit-javascript
         racklr/lower-tsx/css-modules
         (prefix-in ts-lower: racklr/lower-typescript)
         "template.rkt"
         "pipeline.rkt")

(provide make-emit-pages-html)

;; ── Factory: create emit-pages-html with pre-loaded parsers ───────────
;; Callers load parsers once and pass them in, avoiding gen-and-load
;; current-directory issues.

(define (make-emit-pages-html ts-parse ts-tokenize ts-tok-type ts-tok-value
                              jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  ;; Returns emit-pages-html function with baked-in parsers.

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
           (page->js ts-parse ts-tok-type ts-tok-value jsx-parse jsx-tok-type jsx-tok-value
                     resolved #:css-mapping css-mapping
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

    ;; Assemble the router JS. The static SPA skeleton and HTML template
    ;; live in template.rkt; the dynamic pieces (page data, layout, route
    ;; matching, nav links) are computed here and passed in.
    (define page-data-str
      (string-join
       (for/list ([entry (in-list page-entries)])
         (define path (first entry))
         (define props (third entry))
         (format "  \"~a\": ~a" path (or props "null")))
       ",\n"))

    (define pages-str
      (string-join
       (for/list ([entry (in-list page-entries)])
         (define path (first entry))
         (define js (second entry))
         (format "  \"~a\": ~a" path js))
       ",\n"))

    (define server-data-str
      (string-join
       (for/list ([entry (in-list page-entries)])
         (define path (first entry))
         (define srv (fourth entry))
         (format "  \"~a\": ~a" path (if srv (format "/* server-only */ ~a" srv) "null")))
       ",\n"))

    (define router-str
      (spa-router-template #:layout-fn layout-fn
                           #:page-data-str page-data-str
                           #:pages-str pages-str
                           #:server-data-str server-data-str
                           #:dynamic-match-js dynamic-match-js))

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

    (html-page-template #:title title
                        #:head-content head-content
                        #:style-tags style-tags
                        #:nav-links nav-links
                        #:router router-str)))
