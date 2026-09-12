#lang racket

(require racket/file
         racket/string)

(provide discover-pages
         find-app-tsx)

;; ── B18: Pages directory discovery ───────────────────────────────────

(define (discover-pages dir)
  ;; Scan a directory for *.tsx files. Map filenames to URL paths.
  ;; B33: Dynamic route params: [slug].tsx → params stored alongside source.
  ;; B66: Skip _app.tsx and _document.tsx — those are handled separately.
  ;; Returns a hash: URL-path → (cons source params-hash-or-#f).
  (for/hash ([p (in-list (directory-list dir #:build? #f))]
              #:when (and (regexp-match #rx"\\.tsx$" (path->string p))
                          (not (string-prefix? (path->string p) "."))
                          (not (member (path->string p) '("_app.tsx" "_document.tsx")))))
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

;; ── B66: _app.tsx discovery ──────────────────────────────────────────

(define (find-app-tsx dir)
  ;; Read _app.tsx from a pages directory. Returns source string or #f.
  (define p (build-path dir "_app.tsx"))
  (and (file-exists? p) (file->string p)))
