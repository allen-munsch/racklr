#lang racket

(require racket/string
         "helpers.rkt")

(provide preprocess-imports)

;; NOTE: the regex-based `preprocess-hooks` was removed here (B71). Hooks are
;; lowered structurally by lower-tsx/hook-lower.rkt (B35); this module now only
;; handles import preprocessing and type/interface member-separator insertion.

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

;; ── B56/B63: Insert ; between type/interface members separated by newlines ─
;; The ANTLR TS grammar requires `;` or `,` between members of `type X = {}`
;; and `interface X {}` blocks; standard TS allows bare newlines. Semicolons
;; are accepted by the grammar, so we *add* them rather than strip them.

(define (insert-type-separators source)
  ;; Scan for `type X = {` or `interface X ... {` blocks, and within each,
  ;; add `;` at the end of member lines that lack a separator.
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
                     [fixed-block (fix-type-member-lines block-content)]
                     [before (substring s 0 brace-pos)]
                     [close-abs (+ brace-pos close-pos)]
                     [after (substring s (add1 close-abs))]
                     ;; The grammar's `typeAliasDeclaration` ends in `eos`
                     ;; (SemiColon | EOF); a bare newline before the next
                     ;; statement does not terminate a `type X = {...}`. Add a
                     ;; `;` after the closing `}` when the next non-space char
                     ;; isn't already `;` (interfaces accept it, so this is
                     ;; harmless there too).
                     [needs-semi (not (regexp-match? #px"^[[:space:]]*;" after))])
                (define new-s (string-append before "{" fixed-block "}"
                                             (if needs-semi ";" "") after))
                (loop new-s (+ (string-length before) 1
                               (string-length fixed-block) 1
                               (if needs-semi 1 0)))))))))

(define (fix-type-member-lines block-str)
  ;; For each line in the block: if it's non-empty and doesn't end with
  ;; {, }, ,, or ;, add ; at the end. #:trim? #f preserves the leading
  ;; and trailing newlines of the block.
  (define lines (string-split block-str "\n" #:trim? #f))
  (string-join
   (for/list ([line (in-list lines)])
     (define trimmed (string-trim line))
     (cond [(equal? trimmed "") line]
           [(regexp-match #rx"[{},;]\\s*$" trimmed) line]
           [else (string-append line ";")]))
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

  ;; Step 1.5b: Remove npm polyfilled imports (B65) — classnames, date-fns
  (define (strip-npm-import source pkg)
    (define rx-braces   (pregexp (string-append "import[[:space:]]+\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']" pkg "[\"'][[:space:]]*;?[[:space:]]*\n?")))
    (define rx-combo    (pregexp (string-append "import[[:space:]]+[[:word:]]+[[:space:]]*,[[:space:]]*\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']" pkg "[\"'][[:space:]]*;?[[:space:]]*\n?")))
    (define rx-default  (pregexp (string-append "import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"']" pkg "[\"'][[:space:]]*;?[[:space:]]*\n?")))
    (regexp-replace* rx-braces
      (regexp-replace* rx-combo
        (regexp-replace* rx-default source "") "") ""))
  (define s3b (foldl (lambda (pkg src) (strip-npm-import src pkg)) s3 '("classnames" "date-fns")))

  ;; Step 1.6: Normalize double-spaces after { (only literal spaces — do not
  ;; collapse `{\n`, which would join the opening brace onto the first member)
  (define s4 (regexp-replace* #px"\\{ {2,}" s3b "{ "))

  ;; Step 1.7: Insert ; between type/interface members separated by newlines
  (insert-type-separators s4))
