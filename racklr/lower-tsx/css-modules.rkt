#lang racket

(require racket/string
         racket/format)

(provide process-css-module)

;; ── CSS Module Processor ───────────────────────────────────────────
;; Parses a .module.css file, hashes class names, returns hashed CSS
;; and a mapping from original class names to hashed class names.

;; Simple DJB2-style hash → 4 hex chars
(define (short-hash str)
  (define h (for/fold ([h 5381]) ([c (in-string str)])
              (+ (* h 33) (char->integer c))))
  (define hex (number->string (modulo (abs h) 65536) 16))
  (string-append (make-string (- 4 (string-length hex)) #\0) hex))

;; Extract the base name from a CSS module filename:
;; "Home.module.css" → "Home", "components/Foo.module.css" → "Foo"
(define (module-base-name path)
  (define name (last (string-split path "/")))
  (cond [(regexp-match #rx"^(.+)\\.module\\.css$" name) =>
         (lambda (m) (cadr m))]
        [else (regexp-replace #rx"\\.module\\.css$" name "")]))

;; Process a single .module.css file.
;; Returns (values hashed-css-string hash-of-original→hashed)
;;
;; Example:
;;   .title { color: red; } .container { padding: 1rem; }
;;   → ".Home_title_a1b2 { color: red; }\n.Home_container_c3d4 { padding: 1rem; }\n"
;;   + #hash(("title" . "Home_title_a1b2") ("container" . "Home_container_c3d4"))

(define (process-css-module filename css-content)
  (define base (module-base-name filename))
  (define mapping (make-hash))
  (define css-mapping (make-hash))  ;; original → hashed for composing
  
  ;; ── Pass 1: class name hashing ─────────────────────────────────────
  ;; Match .className with optional :pseudo-class, then optional whitespace and {
  ;; Group 1: class name (hashed). Group 2: optional :pseudo-class (preserved).
  (define rx-class #px"\\.([[:word:]-]+)(:[[:word:]-]+)?[[:space:]]*\\{")
  
  ;; Stripping rules (Racket regex mode: (?m:...) for multiline)
  (define rx-at-value #px"(?m:^[ \t]*@value[^;]*;[ \t]*\n?)")
  (define rx-at-import #px"(?m:^[ \t]*@import[^;]*;[ \t]*\n?)")
  (define rx-composes #px"composes:[ \t]*([[:word:]-]+)[^;]*;")
  
  (define s1 css-content)
  
  ;; Strip @value and @import declarations
  (define s2 (regexp-replace* rx-at-value s1 ""))
  (define s3 (regexp-replace* rx-at-import s2 ""))
  
  ;; Strip composes declarations (they are CSS Modules extensions, not valid CSS)
  (define s4 (regexp-replace* rx-composes s3 ""))
  
  ;; ── Pass 3: Hash class names in rules ──────────────────────────────
  (define processed
    (regexp-replace* rx-class s4
                     (lambda (whole class-name pseudo)
                       (define hashed
                         (hash-ref! mapping class-name
                                    (lambda ()
                                      (define h (short-hash (string-append base "_" class-name)))
                                      (string-append base "_" class-name "_" h))))
                       (string-append "." hashed (or pseudo "") " {"))))
  
  ;; Strip comments (/* ... */) — simple, non-nested
  (define result
    (regexp-replace* #rx"/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/" processed ""))
  
  (values result mapping))

(module+ test
  (require rackunit)
  
  (define test-css ".title { color: red; }\n.container { padding: 1rem; }\n.title:hover { color: blue; }")
  (define-values (hashed mapping) (process-css-module "Home.module.css" test-css))
  
  (printf "Hashed CSS:\n~a\n" hashed)
  (printf "Mapping:\n~a\n" mapping)
  
  ;; Verify class names are hashed
  (check-false (string-contains? hashed ".title {"))
  (check-false (string-contains? hashed ".container {"))
  
  ;; Verify hashed names follow pattern
  (define title-hashed (hash-ref mapping "title"))
  (check-true (string-prefix? title-hashed "Home_title_"))
  (check-true (string-contains? hashed (string-append "." title-hashed " {")))
  
  (define container-hashed (hash-ref mapping "container"))
  (check-true (string-prefix? container-hashed "Home_container_"))
  (check-true (string-contains? hashed (string-append "." container-hashed " {")))
  
  ;; Verify pseudo-classes are handled: .title:hover → .Home_title_XXXX:hover
  (check-true (string-contains? hashed (string-append "." title-hashed ":hover")))
  
  ;; Verify mapping values are distinct
  (check-false (equal? title-hashed container-hashed))
  
  ;; Edge case: single class
  (define-values (h2 m2) (process-css-module "Box.module.css" ".red { color: red; }"))
  (check-true (string-prefix? (hash-ref m2 "red") "Box_red_"))
  
  ;; Edge case: class names with hyphens
  (define-values (h3 m3) (process-css-module "Nav.module.css" ".nav-item { display: flex; }"))
  (check-true (string-prefix? (hash-ref m3 "nav-item") "Nav_nav-item_"))
  
  (printf "CSS module tests passed.\n"))
