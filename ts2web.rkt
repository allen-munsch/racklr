#lang racket

;; ts2web.rkt — Convert TypeScript source files to self-contained HTML pages
;; Usage: racket ts2web.rkt <input.ts> [--output output.html] [--title "Page Title"]

(require racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-typescript
         racklr/emit-html)

;; ── Command-line parsing ────────────────────────────────────────────

(define args (current-command-line-arguments))

(define input-file
  (for/first ([a (in-vector args)]
              #:when (string-suffix? a ".ts"))
    a))

(define output-file
  (let ([idx #f])
    (for/or ([i (in-range (vector-length args))])
      (cond [(and (member (vector-ref args i) '("--output" "-o"))
                   (< (add1 i) (vector-length args)))
             (set! idx (add1 i))
             (vector-ref args idx)]
            [(and (eq? idx i) #t) #f]  ;; skip consumed arg
            [else #f]))))

(define title
  (let ([idx #f])
    (for/or ([i (in-range (vector-length args))])
      (cond [(and (member (vector-ref args i) '("--title" "-t"))
                   (< (add1 i) (vector-length args)))
             (set! idx (add1 i))
             (vector-ref args idx)]
            [(and (eq? idx i) #t) #f]
            [else #f]))))

(unless input-file
  (eprintf "Usage: racket ts2web.rkt <input.ts> [--output out.html] [--title \"Title\"]\n")
  (exit 1))

(define out (or output-file
                (let ([base (path-replace-suffix input-file #".html")])
                  (if (string-suffix? base ".html")
                      base
                      (string-append base ".html")))))

;; ── Helpers ───────────────────────────────────────────────────────────

(define (strip-ts-comments src)
  ;; Strip // line comments and /* */ block comments.
  (regexp-replace* #rx"/\\*.*?\\*/"  ;; block comments first
    (regexp-replace* #rx"//[^\n]*" src "")
    ""))

;; ── Pipeline ─────────────────────────────────────────────────────────

(printf "Loading parser...\n")
(define-values (ts-parse ts-tokenize tok-type tok-value)
  (gen-and-load "grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

(printf "Reading ~a...\n" input-file)
(define src (strip-ts-comments (file->string input-file)))

(printf "Parsing...\n")
(define cst (ts-parse src))
(unless cst
  (eprintf "Error: parse failed for ~a\n" input-file)
  (exit 1))

(printf "Lowering...\n")
(define uir (lower-program cst tok-type tok-value))

(printf "Emitting HTML...\n")
(define html (emit-html uir #:title (or title
                                        (let ([fn (file-name-from-path input-file)])
                                          (if (string-suffix? fn ".ts")
                                              (substring fn 0 (- (string-length fn) 3))
                                              fn)))))

(printf "Writing ~a...\n" out)
(display-to-file html out #:exists 'replace)

(printf "Done: ~a → ~a\n" input-file out)
