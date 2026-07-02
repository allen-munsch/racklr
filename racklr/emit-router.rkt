#lang racket

(require racket/string
         racket/file
         racklr/esbuild-resolve
         racklr/tsx-preprocess
         racklr/emit-javascript
         racklr/lower-jsx
         (prefix-in ts-lower: racklr/lower-typescript))

(provide emit-pages-html
         make-emit-pages-html)

;; ── Factory: create emit-pages-html with pre-loaded parsers ───────────
;; Callers load parsers once and pass them in, avoiding gen-and-load
;; current-directory issues.

(define (make-emit-pages-html ts-parse ts-tokenize ts-tok-type ts-tok-value
                              jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  ;; Returns emit-pages-html function with baked-in parsers.

  (define (page->js source)
    (define hookless (preprocess-hooks source))
    (define-values (processed jsx-map jsx-uir)
      (preprocess-tsx hookless
                      #:jsx-parse jsx-parse
                      #:jsx-lower-tk-type jsx-tok-type
                      #:jsx-lower-tk-value jsx-tok-value))
    (define ts-cst (ts-parse processed))
    (define ts-uir (ts-lower:lower-program ts-cst ts-tok-type ts-tok-value))
    (define uir (restore-jsx ts-uir jsx-uir))
    (define full-js (emit-javascript uir))
    ;; Strip data-fetching functions (getStaticProps, getServerSideProps)
    (define no-data-fetching
      (regexp-replace* #rx"(?m:^(export )?(async )?function (getStaticProps|getServerSideProps)\\([^)]*\\) \\{.*\\};?\n?)" full-js ""))
    ;; Strip export keywords — page value is used inline in object literal.
    ;; Use multi-line mode so ^ matches after newlines.
    (define no-export-named  (regexp-replace* #rx"(?m:^export \\{[^}]*\\};?\n?)" no-data-fetching ""))
    (define no-export-default (regexp-replace* #rx"(?m:^export default )" no-export-named ""))
    (define no-export-decl   (regexp-replace* #rx"(?m:^export )" no-export-default ""))
    ;; Strip trailing junk (esbuild sometimes emits "null;" after exports)
    (define no-null (regexp-replace #rx"\\s*null;\\s*$" no-export-decl ""))
    ;; Strip trailing semicolons — values used inline in object literal
    (string-trim (regexp-replace #rx";\\s*$" no-null "")))

  (lambda (pages
           #:title [title "App"]
           #:all-files [all-files #f]
           #:path-to-entry [path-to-entry (lambda (p) p)])
    ;; pages: hash of URL-path (string) → source (string)
    ;;   Each source is a page component.
    ;;   If #:all-files is provided: hash of filename → source for the full project.
    ;;     #:path-to-entry maps URL path → filename in all-files (default: identity).
    ;;     Uses esbuild to resolve imports per-page.

    (define (resolve-page page-src url-path)
      (if all-files
          (resolve-imports all-files #:entry (path-to-entry url-path))
          page-src))

    (define page-entries
      (for/list ([(path src) (in-hash pages)])
        (define resolved (resolve-page src path))
        (define page-js (page->js resolved))
        (cons path page-js)))

    ;; Assemble router JS
    (define router-lines
      (list
       "var _pages = {"
       (string-join
        (for/list ([entry (in-list page-entries)])
          (define path (car entry))
          (define js (cdr entry))
          (format "  \"~a\": ~a" path js))
        ",\n")
       "};"
       ""
       "function _mount(path) {"
       "  var app = document.getElementById(\"_app\");"
       "  app.innerHTML = \"\";"
       "  var pageFn = _pages[path];"
       "  if (pageFn) {"
       "    var el = pageFn();"
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
         (define path (car entry))
        (format "      <a href=\"#~a\">~a</a>"
                path
                (if (equal? path "/") "Home"
                    (string-titlecase (regexp-replace #rx"^/" path "")))))
       " | "))

    (string-append
     "<!DOCTYPE html>\n"
     "<html lang=\"en\">\n"
     "<head>\n"
     "  <meta charset=\"UTF-8\">\n"
     "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
     (format "  <title>~a</title>\n" title)
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
