#lang racket

(require racket/string
         racket/path
         racket/file
         racket/hash)

(provide resolve-imports)

;; ── Pure-Racket import resolution ────────────────────────────────────
;; Resolves ./ and ../ imports across a set of files, produces a single
;; bundled output with imports stripped and declarative exports normalized.
;; Also resolves bare specifiers (npm packages) from node_modules.
;; No external dependencies (was: shelled out to npx esbuild).

;; The generated parser's lexer emits comment tokens (SingleLineComment,
;; MultiLineComment) but the parser grammar doesn't expect them — ANTLR4's
;; channel(HIDDEN) isn't implemented in racklr. Strip them here.
(define (strip-comments s)
  (regexp-replace* #px"/\\*[\\s\\S]*?\\*/"
    (regexp-replace* #px"//[^\n]*" s "")
    ""))

;; ── Import parsing ────────────────────────────────────────────────────

(define import-rx #px"import\\s+(?:[^\"']*\\s+from\\s+)?[\"'](\\.{1,2}/[^\"']+)[\"']")
(define import-path-rx #px"[\"'](\\.{1,2}/[^\"']+)[\"']")
(define bare-import-rx #px"import\\s+(?:[^\"']*\\s+from\\s+)?[\"']([^./][^\"']+)[\"']")

(define (find-relative-imports source)
  (define import-lines (regexp-match* import-rx source))
  (for*/list ([imp (in-list import-lines)]
              [m (in-value (regexp-match import-path-rx imp))]
              #:when m)
    (second m)))

(define (find-bare-imports source)
  ;; Extract bare specifier imports (e.g. "react", "lodash/map")
  (define import-lines (regexp-match* bare-import-rx source))
  (for/list ([imp (in-list import-lines)])
    ;; imp is the full match string; extract the specifier
    (define m (regexp-match #rx"[\"']([^\"']+)[\"']" imp))
    (and m (second m))))

;; ── Path resolution ──────────────────────────────────────────────────

(define (resolve-import-path import-path base-file)
  ;; import-path: "./foo" or "../foo"
  ;; base-file: "app.tsx" or "dir/app.tsx"
  ;; Returns: normalized path without extension, e.g. "foo" or "lib/bar"
  (define-values (base _name _dir?) (split-path base-file))
  (define base-dir
    (cond [(path? base) (path->string base)]
          [(eq? base 'relative) ""]
          [else ""]))

  ;; Normalize: "./foo" → ("." "foo"), "../foo" → (".." "foo")
  (define import-parts (regexp-match #px"^(\\.{1,2})/(.+)$" import-path))
  (unless import-parts
    (error 'resolve-import-path "unexpected import path: ~a" import-path))
  (define prefix (second import-parts))
  (define rest (third import-parts))

  (if (equal? prefix ".")
      ;; "./foo" — resolve relative to base-dir
      (if (equal? base-dir "")
          rest
          (string-append base-dir rest))
      ;; "../foo" — go up one directory from base-dir
      ;; base-dir from split-path has trailing /; strip it before matching.
      (let* ([clean (regexp-replace #px"/$" base-dir "")]
             [parent (if (regexp-match #px"/" clean)
                         (regexp-replace #px"/[^/]+$" clean "")
                         "")])
        (if (equal? parent "")
            rest
            (string-append parent "/" rest)))))

(define (find-file resolved-path files)
  (for/or ([ext (in-list (list ".tsx" ".ts" ".jsx" ".js"
                                "/index.tsx" "/index.ts"))])
    (define candidate (string-append resolved-path ext))
    (and (hash-has-key? files candidate) candidate)))

;; ── Node module resolution ──────────────────────────────────────────

(define known-externals
  ;; Packages that are handled by the runtime/shim layer — strip imports silently.
  (set "react" "react-dom" "next" "fs" "path" "classnames" "date-fns"))

(define (resolve-node-module specifier #:project-root [project-root "."])
  ;; Try to resolve a bare specifier from node_modules.
  ;; Returns (list file-path source-content) or #f.
  (define nm-root (build-path project-root "node_modules"))
  (unless (directory-exists? nm-root) #f)
  
  ;; specifier can be "react" or "lodash/map" etc.
  (define parts (string-split specifier "/"))
  (define pkg-name (first parts))
  (define subpath (string-join (cdr parts) "/"))
  
  (define pkg-dir (build-path nm-root pkg-name))
  (unless (directory-exists? pkg-dir) #f)
  
  ;; Read package.json for "main" or "module" field
  (define pkg-json-path (build-path pkg-dir "package.json"))
  (define entry
    (if (file-exists? pkg-json-path)
        (let* ([raw (file->string pkg-json-path)]
               [m (regexp-match #px"\"main\"\\s*:\\s*\"([^\"]+)\"" raw)])
          (if m (second m) "index.js"))
        "index.js"))
  
  ;; Resolve the actual file
  (define full-path
    (if (equal? subpath "")
        (build-path pkg-dir entry)
        (let ([sub-dir (build-path pkg-dir subpath)])
          (if (directory-exists? sub-dir)
              (build-path sub-dir (if (file-exists? (build-path sub-dir "index.js")) "index.js" entry))
              (build-path pkg-dir (string-append subpath ".js"))))))
  
  ;; Check extensions
  (define full-path-str (path->string full-path))
  (for/or ([ext (in-list '(".js" ".mjs" ".cjs" "/index.js" "/index.mjs" ""))])
    (define try-path (string-append full-path-str ext))
    (and (file-exists? try-path)
         (list try-path (file->string try-path)))))

;; ── Dependency graph ─────────────────────────────────────────────────

(define (build-dep-graph files #:project-root [project-root "."])
  (for/hash ([(fname src) (in-hash files)])
    (define rel-imports (find-relative-imports src))
    (define bare-imports (find-bare-imports src))
    (define resolved
      (append
       ;; Relative imports
       (for/list ([imp (in-list rel-imports)]
                  #:when (find-file (resolve-import-path imp fname) files))
         (define rp (resolve-import-path imp fname))
         (find-file rp files))
       ;; Bare specifier imports from node_modules
       (for/list ([spec (in-list bare-imports)]
                  #:unless (set-member? known-externals (first (string-split spec "/"))))
         (define resolved (resolve-node-module spec #:project-root project-root))
         (and resolved (first resolved)))))
    (values fname (filter values resolved))))

(define (topsort-from-entry graph entry)
  (define visited (mutable-set))
  (define order '())

  (define (visit fname)
    (unless (set-member? visited fname)
      (set-add! visited fname)
      (for ([dep (in-list (hash-ref graph fname '()))])
        (visit dep))
      (set! order (cons fname order))))

  (when (hash-has-key? graph entry)
    (visit entry))
  (reverse order))

;; ── Source stripping ──────────────────────────────────────────────────

(define (strip-imports-exports source)
  ;; Strip import statements and normalize exports.
  ;; - import ... → removed entirely
  ;; - export const/let/var/function/class/async → keep declaration (strip "export ")
  ;; - export default function/class/async → keep declaration (strip "export default ")
  ;; - export { X }, export default X, export type X → removed entirely

  ;; Phase 1: Remove import statements ([^;] already matches newlines, so no (?s) needed)
  (define s1 (regexp-replace* #px"(?m:^[ \t]*import[^;]*?;[ \t]*\n?)" source ""))

  ;; Phase 2: Remove export {X};, export type X; export interface X; and simple re-exports
  ;;   export default Identifier; — but NOT export default function/class/(
  (define s2 (regexp-replace* #px"(?m:^[ \t]*export[ \t]+(?:\\{[^}]*\\}[ \t]*;|type[^;]*?;|interface[^;]*?;|default[ \t]+\\w+[ \t]*;)[ \t]*\n?)" s1 ""))

  ;; Phase 3: Strip "export " / "export default " from declarative exports
  ;; export [default] const/let/var/function/class/async → declaration
  (define s3 (regexp-replace* #px"(?m:^([ \t]*)export[ \t]+(?:default[ \t]+)?(?=(?:const|let|var|function|class|async)\\b))" s2 "\\1"))

  s3)

;; ── Public API ────────────────────────────────────────────────────────

(define (resolve-imports files #:entry [entry "app.tsx"] #:project-root [project-root "."])
  ;; files: hash of filename (string) → source content (string)
  ;; Entry: the main file to start from.
  ;; Project root: root for node_modules resolution.
  ;; Returns: bundled output string — all files (including node_modules deps)
  ;; concatenated in dependency order with imports and exports stripped.
  
  ;; First, resolve node_modules deps and add them to the file set
  (define all-files (make-hash))
  (hash-union! all-files files)
  
  ;; Collect all bare imports and resolve from node_modules
  (for ([(fname src) (in-hash files)])
    (for ([spec (in-list (find-bare-imports src))]
          #:unless (set-member? known-externals (first (string-split spec "/"))))
      (define resolved (resolve-node-module spec #:project-root project-root))
      (when resolved
        (match-define (list nm-path nm-src) resolved)
        (hash-set! all-files nm-path nm-src))))
  
  (define graph (build-dep-graph all-files #:project-root project-root))
  (define order (topsort-from-entry graph entry))

  (define parts
    (for/list ([fname (in-list order)])
      (define src (hash-ref all-files fname))
      (strip-imports-exports src)))

  (if (empty? order)
      (let ([src (hash-ref all-files entry
                            (lambda ()
                              (error 'resolve-imports "entry ~a not found" entry)))])
        (strip-comments (strip-imports-exports src)))
      (strip-comments (string-join parts "\n"))))