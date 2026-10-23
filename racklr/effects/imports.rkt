#lang racket

;; racklr/effects/imports.rkt
;;
;; Static import/export resolution for Python source — the "tiny VM" that
;; mechanizes the dependency surface. Instead of hand-listing every framework
;; method name, this harvests each module's export surface from its parsed UIR,
;; resolves `import` / `from ... import` bindings to module/class/function
;; handles WITHOUT executing any module body (no import-time side effects, no
;; DB, no installed deps), and attaches an effect tag to a class by its module
;; path. A dotted call `receiver.method(...)` then resolves to `Class.method`
;; and inherits the class's tag, so the `dict.get` vs `QuerySet.get` collision
;; is decided by the receiver's class, not the method-name string.
;;
;; The abstract domain is four values:
;;   module   → (exports: name → handle)
;;   class    → (MRO + method table + effect tag)
;;   function → (params + return annotation + body)
;;   unknown  → bottom (sound; deferred)
;;
;; It terminates on every input because the domain is finite and the export
;; table only grows (monotone), so the fixpoint is reachable.

(require racklr/uir)

(provide
  ;; import extraction
  import-record? import-record-form import-record-source import-record-bindings
  import-record
  extract-imports
  ;; handles
  handle? handle-kind handle-path handle-name
  ;; harvested module surface
  class-info? class-info-name class-info-super class-info-methods
  module-info? module-info-path module-info-classes module-info-functions module-info-imports
  module-spec? module-spec-path module-spec-uir module-spec-imports
  module-spec
  build-module-table
  harvest-module
  ;; resolution + tagging
  resolve-exports
  resolve-module-path
  parent-path
  first-component last-component
  module-effect-tag
  class-effect-tag
  derive-class-method-effects
  builtin-class-methods)

;; ── import extraction (token state machine) ─────────────────────────
;;
;; `tokens` is a flat list of (cons type-symbol value-string), e.g.
;; (list (cons 'FROM "from") (cons 'NAME "django") (cons 'DOT ".") ...).
;; The scanner walks for IMPORT/FROM at statement start (preceded by
;; NEWLINE/INDENT/DEDENT/COLON/SEMI_COLON or the first token) and reads to the
;; statement end.

(struct import-record (form source bindings) #:transparent)
;; form: 'import | 'from
;; source: module path string ("a.b", or leading "." for relative)
;; bindings: (listof (cons local-name target))
;;   'import: target is the module path the local name binds to
;;   'from:   target is "<module>.<member>" (local "*" for star imports)

(define (first-component mod)
  (let loop ([i 0])
    (cond [(>= i (string-length mod)) mod]
          [(char=? (string-ref mod i) #\.) (substring mod 0 i)]
          [else (loop (add1 i))])))

(define (last-component s)
  (let loop ([i (sub1 (string-length s))])
    (cond [(< i 0) s]
          [(char=? (string-ref s i) #\.) (substring s (add1 i))]
          [else (loop (sub1 i))])))

(define (parent-path p)
  (let loop ([i (sub1 (string-length p))])
    (cond [(< i 0) #f]
          [(char=? (string-ref p i) #\.) (substring p 0 i)]
          [else (loop (sub1 i))])))

(define (relative-dot-count src)
  (let loop ([i 0])
    (cond [(>= i (string-length src)) i]
          [(char=? (string-ref src i) #\.) (loop (add1 i))]
          [else i])))

(define (extract-imports tokens)
  (define n (length tokens))
  (define tv (list->vector tokens))
  (define (typ i) (and (< i n) (car (vector-ref tv i))))
  (define (val i) (and (< i n) (cdr (vector-ref tv i))))
  (define (stmt-start? i)
    (or (= i 0)
        (memq (typ (sub1 i)) '(NEWLINE INDENT DEDENT COLON SEMI_COLON))))

  ;; dotted name starting at i → (values name-string next-index) or (values #f i)
  (define (read-dotted i)
    (cond
      [(and (< i n) (eq? (typ i) 'NAME))
       (define parts '())
       (let loop ([j i])
         (set! parts (append parts (list (val j))))
         (cond
           [(and (< (+ j 1) n) (eq? (typ (+ j 1)) 'DOT)
                 (< (+ j 2) n) (eq? (typ (+ j 2)) 'NAME))
            (loop (+ j 2))]
           [else (values (string-join parts ".") (add1 j))]))]
      [else (values #f i)]))

  ;; `import a.b as c, d.e`
  (define (parse-import-name i)
    (define bindings '())
    (let loop ([j (add1 i)])
      (define-values (mod k) (read-dotted j))
      (define alias #f)
      (define l k)
      (when (and (< l n) (eq? (typ l) 'AS))
        (set! alias (val (add1 l)))
        (set! l (+ l 2)))
      (define local (or alias (first-component mod)))
      (define target (or alias (first-component mod)))
      (set! bindings (append bindings (list (cons local target))))
      (cond
        [(and (< l n) (eq? (typ l) 'COMMA)) (loop (add1 l))]
        [else (values (import-record 'import mod bindings) l)])))

  ;; `from [dots] [mod] import names`  (or `from . import x`)
  (define (parse-import-from i)
    (define dots 0)
    (let scan-dots ([j (add1 i)])
      (cond
        [(and (< j n) (eq? (typ j) 'DOT)) (set! dots (+ dots 1)) (scan-dots (add1 j))]
        [(and (< j n) (eq? (typ j) 'ELLIPSIS)) (set! dots (+ dots 3)) (scan-dots (add1 j))]
        [else
         (define-values (mod k) (read-dotted j))
         (define src (string-append (make-string dots #\.) (or mod "")))
         (cond
           [(and (< k n) (eq? (typ k) 'IMPORT))
            (define bindings '())
            (define m (add1 k))
            (cond
              [(and (< m n) (eq? (typ m) 'STAR))
               (values (import-record 'from src (list (cons "*" src))) (add1 m))]
              [else
               (let loop-names ([p m])
                 (cond
                   [(and (< p n) (memq (typ p) '(OPEN_PAREN CLOSE_PAREN)))
                    (loop-names (add1 p))]
                   [(and (< p n) (eq? (typ p) 'NAME))
                    (define nm (val p))
                    (define q (add1 p))
                    (define alias #f)
                    (when (and (< q n) (eq? (typ q) 'AS))
                      (set! alias (val (add1 q)))
                      (set! q (+ q 2)))
                    (define local (or alias nm))
                    (define target
                      (cond
                        [(string=? src "") nm]
                        [(char=? (string-ref src (sub1 (string-length src))) #\.)
                         (string-append src nm)]
                        [else (string-append src "." nm)]))
                    (set! bindings (append bindings (list (cons local target))))
                    (cond
                      [(and (< q n) (eq? (typ q) 'COMMA)) (loop-names (add1 q))]
                      [else (values (import-record 'from src bindings) q)])]
                   [else (values (import-record 'from src bindings) p)]))])]
           [else (values #f j)])])))

  (define out '())
  (let loop ([i 0])
    (when (< i n)
      (cond
        [(and (eq? (typ i) 'IMPORT) (stmt-start? i))
         (define-values (rec j) (parse-import-name i))
         (set! out (append out (list rec)))
         (loop j)]
        [(and (eq? (typ i) 'FROM) (stmt-start? i))
         (define-values (rec j) (parse-import-from i))
         (when rec (set! out (append out (list rec))))
         (loop j)]
        [else (loop (add1 i))])))
  out)

;; ── handles ─────────────────────────────────────────────────────────

(struct handle (kind path name) #:transparent)
;; kind: 'module | 'class | 'fn | 'unknown
;; path: module path string (for class/fn); the module path itself (for module)
;; name: class/function name (for class/fn); #f for module

;; ── harvested module surface ────────────────────────────────────────

(struct class-info (name super methods) #:transparent)
;; methods: (hash method-name-string → uir-method)

(struct module-info (path classes functions imports) #:transparent)
;; classes: (hash class-name-string → class-info)
;; functions: (hash function-name-string → uir-fn)
;; imports: (listof import-record)

(struct module-spec (path uir imports) #:transparent)

(define (class-super-name super-expr)
  (cond
    [(uir-var? super-expr) (uir-symbol-name (uir-var-name super-expr))]
    [(uir-get? super-expr)
     (define f (uir-get-field super-expr))
     (cond [(uir-symbol? f) (uir-symbol-name f)]
           [(uir-string? f) f]
           [else #f])]
    [else #f]))

(define (harvest-module path uir imports)
  (define classes (make-hash))
  (define functions (make-hash))
  (define (walk-stmt s)
    (cond
      [(uir-class? s)
       (define cname (and (uir-symbol? (uir-class-name s))
                          (uir-symbol-name (uir-class-name s))))
       (define methods (make-hash))
       (for ([m (in-list (uir-class-methods s))])
         (define mn (and (uir-symbol? (uir-method-name m))
                         (uir-symbol-name (uir-method-name m))))
         (when mn (hash-set! methods mn m)))
       (when cname
         (hash-set! classes cname
                    (class-info cname (class-super-name (uir-class-super s)) methods)))]
      [(uir-fn? s)
       (define fn (and (uir-symbol? (uir-fn-name s))
                       (uir-symbol-name (uir-fn-name s))))
       (when fn (hash-set! functions fn s))]
      [(uir-decorated? s) (walk-stmt (uir-decorated-inner s))]
      [else (void)]))
  (for ([s (in-list (uir-block-stmts uir))]) (walk-stmt s))
  (module-info path classes functions imports))

(define (build-module-table specs)
  (for/hash ([spec (in-list specs)])
    (values (module-spec-path spec)
            (harvest-module (module-spec-path spec)
                            (module-spec-uir spec)
                            (module-spec-imports spec)))))

;; ── module path resolution ──────────────────────────────────────────

;; Resolve an import source against the importing module's path. Absolute
;; sources resolve directly against the module table; relative sources (leading
;; dots) resolve against the importing module's package.
(define (resolve-module-path src from-path mods packages)
  (define dots (relative-dot-count src))
  (cond
    [(= dots 0) (and (hash-has-key? mods src) src)]
    [else
     ;; package containing from-path: itself if it is a package, else its parent
     (define pkg (if (set-member? packages from-path) from-path (parent-path from-path)))
     ;; for k dots: base = pkg for k=1, parent(pkg) for k=2, ...
     (define base
       (let loop ([k 1] [b pkg])
         (cond [(>= k dots) b]
               [else (loop (add1 k) (parent-path b))])))
     (define rest (substring src dots))
     (cond
       [(string=? rest "") (and base (hash-has-key? mods base) base)]
       [else
        (define target (if (and base (not (string=? base "")))
                           (string-append base "." rest)
                           rest))
        (and (hash-has-key? mods target) target)])]))

;; ── export resolution (monotone fixpoint) ───────────────────────────

(define (public-name? nm)
  (not (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\_))))

(define (resolve-exports mods packages)
  ;; mods: (hash path → module-info). Returns (hash path → (hash name → handle)).
  (define exports (make-hash))
  (for ([(path mi) (in-hash mods)])
    (define e (make-hash))
    (for ([cname (in-hash-keys (module-info-classes mi))])
      (hash-set! e cname (handle 'class path cname)))
    (for ([fname (in-hash-keys (module-info-functions mi))])
      (hash-set! e fname (handle 'fn path fname)))
    (hash-set! exports path e))

  (let loop ([changed #t])
    (when changed
      (define next-changed #f)
      (for ([(path mi) (in-hash mods)])
        (define e (hash-ref exports path))
        (for ([rec (in-list (module-info-imports mi))])
          (define src (import-record-source rec))
          (define target-mod (resolve-module-path src path mods packages))
          (define te (and target-mod (hash-ref exports target-mod #f)))
          (cond
            [(eq? (import-record-form rec) 'from)
             (for ([b (in-list (import-record-bindings rec))])
               (define local (car b))
               (define member (last-component (cdr b)))
               (define h (and te (hash-ref te member #f)))
               (when (and h (not (hash-has-key? e local)))
                 (hash-set! e local h)
                 (set! next-changed #t)))]
            [else
             ;; 'import: binding target is already a module path
             (for ([b (in-list (import-record-bindings rec))])
               (define local (car b))
               (define modpath (cdr b))
               (when (and (hash-has-key? mods modpath) (not (hash-has-key? e local)))
                 (hash-set! e local (handle 'module modpath #f))
                 (set! next-changed #t)))])))
      (loop next-changed)))
  exports)

;; ── effect tagging by module path ───────────────────────────────────

;; A class's effect is a property of WHERE it lives, not of each method's name.
;; This is the only hand-authored layer, and it is at class/module granularity
;; (dozens of entries), not method granularity (thousands).
(define (module-effect-tag path)
  (cond
    [(string-prefix? path "django.db") 'db]
    [(string-prefix? path "django.http") 'io]
    [(string-prefix? path "django.core.mail") 'email]
    [(string-prefix? path "django.core.cache") 'cache]
    [(string-prefix? path "django.test.client") 'network]
    [else #f]))

(define (class-effect-tag path cname)
  (module-effect-tag path))

;; Builtin receiver classes (the seed environment): method names tagged pure.
;; This is what resolves `dict.get` vs `QuerySet.get` by class identity.
(define builtin-class-methods
  (hash "dict"  (set "get" "keys" "values" "items" "update" "pop" "popitem"
                     "setdefault" "clear" "copy" "fromkeys")
        "list"  (set "append" "extend" "insert" "pop" "remove" "clear" "index"
                     "count" "sort" "reverse" "copy")
        "set"   (set "add" "remove" "discard" "pop" "clear" "union"
                     "intersection" "difference" "symmetric_difference" "update"
                     "issubset" "issuperset" "isdisjoint" "copy")
        "str"   (set "join" "format" "format_map" "split" "rsplit" "strip"
                     "lstrip" "rstrip" "lower" "upper" "replace" "startswith"
                     "endswith" "find" "index" "count")))

;; Derive Class.method → effect for every class in a tagged module. Returns
;; (hash "Class.method" → effect-symbol).
(define (derive-class-method-effects mods)
  (define out (make-hash))
  (for ([(path mi) (in-hash mods)])
    (define tag (module-effect-tag path))
    (when tag
      (for ([(cname ci) (in-hash (module-info-classes mi))])
        (for ([mname (in-hash-keys (class-info-methods ci))])
          (hash-set! out (format "~a.~a" cname mname) tag)))))
  out)

(module+ main
  (displayln "racklr/effects/imports — static import/export resolver loaded."))
