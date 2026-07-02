#lang racket

(require racket/system
         racket/string
         racket/file
         racket/path)

(provide resolve-imports)

;; ── Multi-file module resolution via esbuild ─────────────────────────
;; esbuild bundles multi-file TSX/TS/JSX into a single file with JSX
;; preserved and types stripped. Used only for import resolution.

;; The generated parser's lexer emits comment tokens (SingleLineComment,
;; MultiLineComment) but the parser grammar doesn't expect them — ANTLR4's
;; channel(HIDDEN) isn't implemented in racklr. Strip them here.
(define (strip-comments s)
  (regexp-replace* #px"/\\*[\\s\\S]*?\\*/"       ;; block comments (across lines)
    (regexp-replace* #px"//[^\n]*" s "")        ;; line comments  
    ""))

(define (resolve-imports files #:entry [entry "app.tsx"])
  ;; files: hash of filename (string) → source content (string)
  ;; Returns: bundled output string with JSX preserved, types stripped
  (define tmpdir (make-temporary-directory "racklr-esbuild-~a"))

  (for ([(fname src) (in-hash files)])
    (define p (build-path tmpdir fname))
    (make-parent-directory* p)
    (display-to-file src p #:exists 'replace))

  (define out-file (build-path tmpdir "_racklr_out.js"))
  (define cmd
    (format "cd ~a && npx esbuild --bundle ~a --jsx=preserve --format=esm --outfile=~a 2>&1"
            (path->string tmpdir)
            entry
            (path->string out-file)))

  (define ok? (system cmd))

  (define out-str
    (if (file-exists? out-file)
        (file->string out-file)
        ""))

  (delete-directory/files tmpdir)

  (unless ok?
    (error 'resolve-imports "esbuild failed: ~a" out-str))

  ;; Strip comments — the generated parser's lexer doesn't skip HIDDEN-channel tokens
  (strip-comments out-str))
