#lang racket

(require rackunit
         racklr/uir
         racklr/effects/imports)

;; ── helpers ─────────────────────────────────────────────────────────

(define (tok type value) (cons type value))
(define (toks . xs) xs)

(define (cls name super methods)
  (uir-class (uir-symbol name) (and super (uir-var (uir-symbol super)))
             '() methods))
(define (mth name)
  (uir-method (uir-symbol name) '() (uir-null) 'public))

(define (fn-def name)
  (uir-fn (uir-symbol name) '() (uir-null) #f #f))

(define (prog . stmts) (uir-block stmts))

;; ── import extraction ───────────────────────────────────────────────

(define (test-extract-import-name)
  (define recs (extract-imports
                (list (tok 'NEWLINE "\n")
                      (tok 'IMPORT "import") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'DOT ".") (tok 'NAME "models")
                      (tok 'NEWLINE "\n"))))
  (check-equal? (length recs) 1)
  (define r (first recs))
  (check-equal? (import-record-form r) 'import)
  (check-equal? (import-record-source r) "django.db.models")
  (check-equal? (import-record-bindings r) (list (cons "django" "django"))))

(define (test-extract-import-alias)
  (define recs (extract-imports
                (list (tok 'IMPORT "import") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'DOT ".") (tok 'NAME "models")
                      (tok 'AS "as") (tok 'NAME "models")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-bindings r) (list (cons "models" "models"))))

(define (test-extract-import-from-named)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'DOT ".") (tok 'NAME "models")
                      (tok 'IMPORT "import") (tok 'NAME "QuerySet")
                      (tok 'COMMA ",") (tok 'NAME "Count")
                      (tok 'AS "as") (tok 'NAME "C")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-form r) 'from)
  (check-equal? (import-record-source r) "django.db.models")
  (check-equal? (import-record-bindings r)
                (list (cons "QuerySet" "django.db.models.QuerySet")
                      (cons "C" "django.db.models.Count"))))

(define (test-extract-import-from-star)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'NAME "django")
                      (tok 'DOT ".") (tok 'NAME "db")
                      (tok 'IMPORT "import") (tok 'STAR "*")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-bindings r) (list (cons "*" "django.db"))))

(define (test-extract-import-relative)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'DOT ".") (tok 'NAME "base")
                      (tok 'IMPORT "import") (tok 'NAME "run")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-source r) ".base")
  (check-equal? (import-record-bindings r) (list (cons "run" ".base.run"))))

(define (test-extract-import-from-dot-only)
  (define recs (extract-imports
                (list (tok 'FROM "from") (tok 'DOT ".")
                      (tok 'IMPORT "import") (tok 'NAME "x")
                      (tok 'NEWLINE "\n"))))
  (define r (first recs))
  (check-equal? (import-record-source r) ".")
  (check-equal? (import-record-bindings r) (list (cons "x" ".x"))))

(define (test-extract-import-multiple-statements)
  (define recs (extract-imports
                (list (tok 'IMPORT "import") (tok 'NAME "os")
                      (tok 'NEWLINE "\n")
                      (tok 'IMPORT "import") (tok 'NAME "sys")
                      (tok 'AS "as") (tok 'NAME "s")
                      (tok 'NEWLINE "\n"))))
  (check-equal? (length recs) 2)
  (check-equal? (import-record-source (first recs)) "os")
  (check-equal? (import-record-source (second recs)) "sys")
  (check-equal? (import-record-bindings (second recs)) (list (cons "s" "s"))))

;; ── harvest ─────────────────────────────────────────────────────────

(define (test-harvest-classes-and-functions)
  (define mi (harvest-module "app" (prog (cls "A" #f (list (mth "m")))
                                          (fn-def "helper")) '()))
  (check-true (hash-has-key? (module-info-classes mi) "A"))
  (check-true (hash-has-key? (module-info-functions mi) "helper"))
  (check-true (hash-has-key? (class-info-methods
                              (hash-ref (module-info-classes mi) "A")) "m")))

(define (test-harvest-super-name)
  (define mi (harvest-module "app" (prog (cls "A" "Base" (list (mth "m")))) '()))
  (check-equal? (class-info-super (hash-ref (module-info-classes mi) "A")) "Base"))

;; ── module-path resolution ──────────────────────────────────────────

(define (test-resolve-absolute)
  (check-equal? (resolve-module-path "django.db.models" "app" (hash "django.db.models" #t) (set))
                "django.db.models"))

(define (test-resolve-relative-sibling)
  (define mods (hash "app" #t "app.base" #t))
  (check-equal? (resolve-module-path ".base" "app" mods (set "app")) "app.base"))

(define (test-resolve-relative-parent)
  ;; Python relative-import semantics: from `pkg.sub` (a module in package
  ;; `pkg`), one leading dot = package `pkg`, two = top level.
  (define mods (hash "util" #t "pkg.util" #t "pkg.sub" #t))
  (check-equal? (resolve-module-path ".util" "pkg.sub" mods (set "pkg")) "pkg.util")
  (check-equal? (resolve-module-path "..util" "pkg.sub" mods (set "pkg")) "util"))

;; ── export resolution + tagging ─────────────────────────────────────

(define (test-resolve-exports-import-class)
  (define mods
    (hash "django.db.models"
          (harvest-module "django.db.models" (prog (cls "QuerySet" #f (list (mth "get")))) '())
          "app"
          (harvest-module "app" (prog) (list (import-record 'from "django.db.models"
                                                            (list (cons "QuerySet" "django.db.models.QuerySet")))))))
  (define ex (resolve-exports mods (set "django.db.models" "app")))
  (define h (hash-ref (hash-ref ex "app") "QuerySet"))
  (check-equal? (handle-kind h) 'class)
  (check-equal? (handle-path h) "django.db.models")
  (check-equal? (handle-name h) "QuerySet"))

(define (test-resolve-exports-transitive)
  ;; a imports from b; b re-exports from c. a's binding resolves transitively.
  (define mods
    (hash "c" (harvest-module "c" (prog (cls "Thing" #f '())) '())
          "b" (harvest-module "b" (prog) (list (import-record 'from "c" (list (cons "Thing" "c.Thing")))))
          "a" (harvest-module "a" (prog) (list (import-record 'from "b" (list (cons "Thing" "b.Thing")))))))
  (define ex (resolve-exports mods (set "c" "b" "a")))
  (define h (hash-ref (hash-ref ex "a") "Thing"))
  (check-equal? (handle-path h) "c"))

(define (test-module-effect-tag)
  (check-equal? (module-effect-tag "django.db.models.query") 'db)
  (check-equal? (module-effect-tag "django.http.response") 'io)
  (check-equal? (module-effect-tag "django.core.mail.message") 'email)
  (check-equal? (module-effect-tag "django.core.cache.backends.base") 'cache)
  (check-equal? (module-effect-tag "os.path") #f))

(define (test-derive-class-method-effects)
  (define mods
    (hash "django.db.models" (harvest-module "django.db.models"
                                             (prog (cls "QuerySet" #f (list (mth "get") (mth "filter")))) '())))
  (define eff (derive-class-method-effects mods))
  (check-equal? (hash-ref eff "QuerySet.get") 'db)
  (check-equal? (hash-ref eff "QuerySet.filter") 'db))

(define (test-collision-resolves-by-class)
  ;; Same method name `get`, different class, different effect: the class is
  ;; what decides. builtin `dict.get` is pure; `QuerySet.get` is db.
  (check-true (set-member? (hash-ref builtin-class-methods "dict") "get"))
  (define mods (hash "django.db.models"
                     (harvest-module "django.db.models" (prog (cls "QuerySet" #f (list (mth "get")))) '())))
  (define eff (derive-class-method-effects mods))
  (check-equal? (hash-ref eff "QuerySet.get") 'db))

(module+ main
  (test-extract-import-name)
  (test-extract-import-alias)
  (test-extract-import-from-named)
  (test-extract-import-from-star)
  (test-extract-import-relative)
  (test-extract-import-from-dot-only)
  (test-extract-import-multiple-statements)
  (test-harvest-classes-and-functions)
  (test-harvest-super-name)
  (test-resolve-absolute)
  (test-resolve-relative-sibling)
  (test-resolve-relative-parent)
  (test-resolve-exports-import-class)
  (test-resolve-exports-transitive)
  (test-module-effect-tag)
  (test-derive-class-method-effects)
  (test-collision-resolves-by-class)
  (displayln "All imports tests passed."))
