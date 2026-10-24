#lang racket

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/infer
         racklr/effects/library-tokens
         racklr/effects/pure-roots)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
;; JS/TS/JSX lower `a.b` to (call (symbol "dot") (list a b)), not uir-get.
(define (jsdot b f) (uir-call (sym "dot") (list b (sym f))))

(define (fn name body #:params [params '()])
  (uir-fn (if name (uir-symbol name) #f) params body #f #f))

(define (prog . stmts)
  (uir-block stmts))

(define (modality row name)
  (define e (row-lookup row name))
  (and e (uir-effect-entry-modality e)))

(define (row-names row)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries row))
        symbol<?))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-empty-program)
  (check-true (hash-empty? (infer-effects (uir-null)))))

(define (test-lambda-not-collected)
  ;; An unnamed uir-fn is not a call-graph node.
  (define lam (fn #f (uir-call (dot (v "llm") "query") '())))
  (check-true (hash-empty? (infer-effects (prog lam)))))

(define (test-pure-function)
  (define p (prog (fn "f" (uir-null))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-injectable-effect-tokens)
  ;; direct-effect-of-callee honors an injected token lexicon; the default
  ;; lexicon does not know the custom root.
  (define custom
    (struct-copy effect-tokens default-effect-tokens
                 [llm-roots (set "mymodel")]))
  (check-equal? (direct-effect-of-callee (dot (v "mymodel") "generate") custom) 'llm)
  (check-false (direct-effect-of-callee (dot (v "mymodel") "generate")))
  ;; declarative-callee? honors an injected lexicon too.
  (define custom2
    (struct-copy effect-tokens default-effect-tokens
                 [declarative-names (set "MyField")]))
  (check-true (declarative-callee? (v "MyField") custom2))
  (check-false (declarative-callee? (v "MyField")))
  ;; time-methods and root-method-effects honor an injected lexicon too.
  (define custom3
    (struct-copy effect-tokens default-effect-tokens
                 [time-methods (set "pause")]
                 [root-method-effects (hash 'mylib (hash "launch" 'process))]))
  (check-equal? (direct-effect-of-callee (dot (v "clock") "pause") custom3) 'time)
  (check-equal? (direct-effect-of-callee (dot (v "mylib") "launch") custom3) 'process)
  (check-false (direct-effect-of-callee (dot (v "clock") "pause")))
  (check-false (direct-effect-of-callee (dot (v "mylib") "launch")))
  ;; the three pure-roots name tables honor an injected lexicon too.
  (define custom4
    (struct-copy effect-tokens default-effect-tokens
                 [pure-stdlib-bare-names (set "mypartial")]))
  (check-true (pure-stdlib-bare-callee? (v "mypartial") custom4))
  (check-false (pure-stdlib-bare-callee? (v "mypartial")))
  (define custom5
    (struct-copy effect-tokens default-effect-tokens
                 [gettext-names (set "mytrans")]))
  (check-true (gettext-callee? (v "mytrans") custom5))
  (check-false (gettext-callee? (v "mytrans")))
  (define custom6
    (struct-copy effect-tokens default-effect-tokens
                 [pure-config-method-names (set "myconfig")]))
  (check-true (pure-config-method-callee? (dot (v "parser") "myconfig") custom6))
  (check-false (pure-config-method-callee? (dot (v "parser") "myconfig"))))

(define (test-direct-llm)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define r (hash-ref (infer-effects p) 'summarize))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (row-names r) '(llm)))

(define (test-direct-db-dotted)
  ;; db.user.update(...) → db (root of the dotted chain).
  (define p (prog (fn "update"
                      (uir-call (dot (dot (v "db") "user") "update")
                                (list (uir-record '()) (uir-record '()))))))
  (define r (hash-ref (infer-effects p) 'update))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (row-names r) '(db)))

(define (test-django-model-objects)
  ;; Model.objects.filter(...) → db via the `objects` field.
  (define p (prog (fn "q"
                      (uir-call (dot (dot (v "User") "objects") "filter")
                                (list (uir-record '()))))))
  (define r (hash-ref (infer-effects p) 'q))
  (check-equal? (modality r 'db) 'may))

(define (test-direct-network)
  (define p (prog (fn "f" (uir-call (v "fetch") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-direct-cache)
  (define p (prog (fn "r" (uir-call (v "revalidate_path") (list (uir-string "/p"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'r) 'cache) 'may))

(define (test-transitive)
  ;; h calls g, g calls db → h inherits db.
  (define p (prog
             (fn "g" (uir-call (dot (v "db") "fetch")
                               (list (uir-string "idx") (uir-string "q"))))
             (fn "h" (uir-call (v "g") (list (uir-string "seed"))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'g) 'db) 'may)
  (check-equal? (modality (hash-ref rows 'h) 'db) 'may)
  (check-equal? (row-names (hash-ref rows 'h)) '(db)))

(define (test-higher-order-unknown)
  ;; A call through a parameter (cb) is deferred: entry named unknown.
  (define p (prog (fn "k" (uir-call (v "cb") (list (uir-number "1")))
                      #:params (list (uir-typed-param (sym "cb") #f #f)))))
  (define r (hash-ref (infer-effects p) 'k))
  (check-equal? (modality r 'unknown) 'unknown)
  (check-equal? (row-names r) '(unknown)))

(define (test-nested-fn-does-not-leak)
  ;; A nested function's effects must not leak into the enclosing function
  ;; when the nested function is never called.
  (define p (prog (fn "outer"
                      (uir-block
                       (list (fn "inner" (uir-call (dot (v "llm") "query") '()))
                             (uir-null))))))
  (define rows (infer-effects p))
  ;; outer itself performs nothing directly and calls nothing.
  (check-equal? (hash-ref rows 'outer) (empty-row))
  ;; inner is still a separate node with llm.
  (check-equal? (modality (hash-ref rows 'inner) 'llm) 'may))

(define (test-recursion-converges)
  ;; r calls r on one branch and llm on the other; fixpoint must terminate.
  (define p (prog (fn "r"
                      (uir-if (uir-bool #t)
                              (uir-call (v "r") '())
                              (uir-call (dot (v "llm") "query") '())))))
  (define r (hash-ref (infer-effects p) 'r))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (row-names r) '(llm)))

(define (test-httpx-network)
  (define p (prog (fn "f" (uir-call (dot (v "httpx") "get") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-requests-network)
  (define p (prog (fn "f" (uir-call (dot (v "requests") "get") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-redis-cache)
  (define p (prog (fn "f" (uir-call (dot (v "redis") "get") (list (uir-string "k"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'cache) 'may))

(define (test-anthropic-llm)
  (define p (prog (fn "f" (uir-call (dot (dot (v "anthropic") "messages") "create") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-agno-llm)
  (define p (prog (fn "f" (uir-call (dot (dot (v "agno") "agent") "Agent") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-langgraph-llm)
  (define p (prog (fn "f" (uir-call (dot (dot (v "langgraph") "graph") "StateGraph") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-django-objects-string-field)
  ;; The Python lowerer emits attribute names as uir-string (not uir-symbol);
  ;; the `objects` heuristic must accept that form too.
  (define p (prog (fn "q"
                      (uir-call (uir-get (uir-get (v "Order") (uir-string "objects"))
                                         (uir-string "all"))
                                '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'q) 'db) 'may))

;; ── JS/TS/JSX `dot`-encoded member access ───────────────────────────

(define (test-js-dot-axios-network)
  (define p (prog (fn "f" (uir-call (jsdot (v "axios") "get")
                                    (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-js-dot-db-db)
  (define p (prog (fn "f" (uir-call (jsdot (v "db") "query")
                                    (list (uir-string "SELECT 1"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'db) 'may))

(define (test-js-dot-model-objects-db)
  (define p (prog (fn "q"
                      (uir-call (jsdot (jsdot (v "User") "objects") "filter")
                                (list (uir-record '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'q) 'db) 'may))

(define (test-js-dot-redis-cache)
  (define p (prog (fn "f" (uir-call (jsdot (v "redis") "get")
                                    (list (uir-string "k"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'cache) 'may))

(define (test-js-dot-anthropic-llm)
  (define p (prog (fn "f" (uir-call (jsdot (jsdot (v "anthropic") "messages") "create")
                                    '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-js-dot-bare-fetch-network)
  ;; A bare callee (non-dotted) still classifies: fetch(...) → network.
  (define p (prog (fn "f" (uir-call (v "fetch") (list (uir-string "/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'network) 'may))

(define (test-handler-primitive-is-pure)
  ;; __handle is a runtime primitive: calling it adds no direct effect and no
  ;; 'unknown pollution to the enclosing function (its handler/thunk arguments
  ;; are arrow walk boundaries).
  (define p
    (prog (fn "app"
              (uir-call (v "__handle")
                        (list (uir-string "llm")
                              (uir-call (sym "=>") (list (uir-list '()) (uir-string "x")))
                              (uir-call (sym "=>") (list (uir-list '()) (uir-null))))))))
  (check-equal? (hash-ref (infer-effects p) 'app) (empty-row)))

;; ── effect-op-of-callee ─────────────────────────────────────────────
;; The operation performed by a direct-effect callee, used by the emitters to
;; instrument runtime guards with the specific operation.

(define (test-op-dotted-llm-query)
  (check-equal? (effect-op-of-callee (jsdot (v "llm") "query")) 'query))

(define (test-op-dotted-nested-last-field)
  (check-equal? (effect-op-of-callee (jsdot (jsdot (v "db") "user") "update")) 'update))

(define (test-op-dotted-objects-filter)
  (check-equal? (effect-op-of-callee (jsdot (jsdot (v "User") "objects") "filter")) 'filter))

(define (test-op-bare-fetch)
  (check-equal? (effect-op-of-callee (v "fetch")) 'fetch))

(define (test-op-python-get-string-field)
  (check-equal? (effect-op-of-callee
                 (uir-get (uir-get (v "Order") (uir-string "objects")) (uir-string "all")))
                'all))

;; ── synthetic operator/statement calls are pure, not `unknown` ──────

(define (test-synthetic-operator-pure)
  (define p (prog (fn "f" (uir-call (sym "+")
                                    (list (uir-number "1") (uir-number "2"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-synthetic-dot-pure)
  ;; A bare member access (not itself an effectful call) is pure.
  (define p (prog (fn "f" (jsdot (v "x") "y"))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-python-identity-membership-operators-pure)
  ;; `is`, `is not`, `in`, `not in` lower to uir-symbol callees; they are
  ;; language constructs, not effectful calls, so they must not pollute the
  ;; row with `unknown`.
  (define p (prog (fn "f"
                       (uir-block
                        (list (uir-call (sym "is") (list (v "a") (v "b")))
                              (uir-call (sym "is not") (list (v "a") (v "b")))
                              (uir-call (sym "in") (list (v "a") (v "xs")))
                              (uir-call (sym "not in") (list (v "a") (v "xs"))))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-python-boolean-operators-pure)
  ;; `and` / `or` are short-circuit structural operators.
  (define p (prog (fn "f"
                       (uir-block
                        (list (uir-call (sym "and") (list (v "a") (v "b")))
                              (uir-call (sym "or") (list (v "a") (v "b"))))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-python-floor-div-pure)
  ;; `//` was missing from the synthetic operator table; it is pure arithmetic.
  (define p (prog (fn "f" (uir-call (sym "//")
                                    (list (uir-number "7") (uir-number "2"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-python-builtin-pure)
  ;; Side-effect-free Python builtins lower to uir-var callees and must be
  ;; classified pure, not `unknown`, so `len(x)`/`dict(a=1)` don't pollute the
  ;; effect algebra.
  (define p (prog (fn "f" (uir-call (v "len")
                                    (list (uir-var (sym "x")))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-exception-constructor-pure)
  ;; `ValueError(...)` builds an exception object to raise; it performs no
  ;; effect itself (the `raise` statement is the effectful construct). Must not
  ;; be marked `unknown`.
  (define p (prog (fn "f" (uir-call (v "ValueError")
                                    (list (uir-string "bad"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-dotted-exception-constructor-pure)
  ;; Dotted exception names (`django.core.exceptions.ValidationError`) resolve
  ;; through the final field's "Error" suffix too.
  (define p (prog (fn "f" (uir-call (dot (dot (dot (v "django") "core")
                                              "exceptions")
                                        "ValidationError")
                                    (list (uir-string "bad"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-non-error-suffix-stays-unknown)
  ;; Case-sensitive suffix: `error` (lowercase, a real function) must remain
  ;; `unknown`, not be swept into the pure exception set.
  (define p (prog (fn "f" (uir-call (v "error") '()))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-warning-and-exception-suffix-pure)
  ;; `Warning(...)` and `GDALException(...)` construct exception/warning objects
  ;; (pure); the raise site is the effect. The suffix match must extend beyond
  ;; "Error" to "Warning"/"Exception", both PascalCase so snake_case callables
  ;; stay unknown.
  (define p (prog (fn "w" (uir-call (v "Warning") (list (uir-string "m"))))
                  (fn "e" (uir-call (v "GDALException") (list (uir-string "m"))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'w) (empty-row))
  (check-equal? (hash-ref rows 'e) (empty-row)))

(define (test-pure-stdlib-bare)
  ;; Directly-imported functools/ctypes value constructors lower to bare
  ;; callees and must be pure, not `unknown`.
  (define p (prog (fn "f" (uir-call (v "partial") (list (v "g"))))
                  (fn "g" (uir-call (v "wraps") (list (v "h"))))
                  (fn "h" (uir-call (v "byref") (list (v "x"))))
                  (fn "i" (uir-call (v "POINTER") (list (v "c_int"))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row))
  (check-equal? (hash-ref rows 'h) (empty-row))
  (check-equal? (hash-ref rows 'i) (empty-row)))

(define (test-chain-from-iterable-pure)
  ;; `chain.from_iterable` (itertools.chain imported bare) is pure.
  (define p (prog (fn "f" (uir-call (dot (v "chain") "from_iterable")
                                    (list (uir-var (sym "it")))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-warnings-warn-io)
  ;; `warnings.warn` writes to stderr, so it is an io effect, not pure/unknown.
  (define p (prog (fn "f" (uir-call (dot (v "warnings") "warn")
                                    (list (uir-string "deprecated"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'io) 'may)
  (check-equal? (row-names r) '(io)))

(define (test-dunder-method-pure)
  ;; Protocol ("dunder") methods on an arbitrary receiver initialize / compare /
  ;; format / index the instance and perform no domain effect. `__call__` and
  ;; `__exit__` can dispatch to effectful code, so they stay `unknown`.
  (define p (prog (fn "i" (uir-call (dot (v "x") "__init__") (list (v "a"))))
                  (fn "r" (uir-call (dot (v "x") "__repr__") '()))
                  (fn "e" (uir-call (dot (v "x") "__eq__") (list (v "y"))))
                  (fn "c" (uir-call (dot (v "x") "__call__") (list (v "y"))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'i) (empty-row))
  (check-equal? (hash-ref rows 'r) (empty-row))
  (check-equal? (hash-ref rows 'e) (empty-row))
  (check-equal? (modality (hash-ref rows 'c) 'unknown) 'unknown))

(define (test-gettext-alias-pure)
  ;; `_` is the gettext translation alias imported in Django modules; it maps
  ;; strings to strings with no effect, so `_("x")` must not be `unknown`.
  (define p (prog (fn "f" (uir-call (v "_") (list (uir-string "x"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-gettext-family-pure)
  ;; Bare stdlib gettext imports (`gettext`, `ngettext`, `gettext_lazy`) are
  ;; pure translation lookups, not `unknown`.
  (define p (prog (fn "f" (uir-call (v "gettext") (list (uir-string "x"))))
                  (fn "g" (uir-call (v "ngettext") (list (uir-string "a") (uir-string "b") (uir-number "1"))))
                  (fn "h" (uir-call (v "gettext_lazy") (list (uir-string "x"))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row))
  (check-equal? (hash-ref rows 'h) (empty-row)))

(define (test-string-literal-method-pure)
  ;; `" ".join(xs)` and `"{}".format(v)` have a string-literal receiver, so
  ;; their methods are pure str ops, not `unknown` (and not db/io).
  (define p (prog (fn "f" (uir-call (uir-get (uir-string " ") (uir-string "join"))
                                     (list (uir-var (sym "xs")))))
                  (fn "g" (uir-call (uir-get (uir-string "{}") (uir-string "format"))
                                    (list (uir-var (sym "v")))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row)))

(define (test-django-exception-constructor-pure)
  ;; Django/HTTP exception classes that don't end in "Error" must still be
  ;; classified pure (construction does no effect; `raise` is separate).
  (define p (prog (fn "f" (uir-call (v "ImproperlyConfigured") (list (uir-string "bad"))))
                  (fn "g" (uir-call (v "Http404") (list (uir-string "nope"))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row)))

(define (test-sys-exit-process)
  ;; sys.exit raises SystemExit — a process effect.
  (define p (prog (fn "f" (uir-call (dot (v "sys") "exit") (list (uir-number "1"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'process) 'may))

(define (test-container-method-pure)
  ;; `errors.append(x)`, `mapping.items()`, `s.keys()` on arbitrary receivers
  ;; are pure collection operations — not effects and not `unknown`.
  (define p (prog (fn "f" (uir-call (dot (v "errors") "append") (list (v "x"))))
                  (fn "g" (uir-call (dot (v "mapping") "items") '()))
                  (fn "h" (uir-call (dot (v "s") "keys") '()))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row))
  (check-equal? (hash-ref rows 'h) (empty-row)))

(define (test-container-ambiguous-name-stays-unknown)
  ;; `.add` / `.remove` / `.count` are NOT in the pure-container set (they
  ;; collide with RelatedManager/QuerySet db ops), so an arbitrary-object call
  ;; with one of those names must not be swept into pure.
  (define p (prog (fn "f" (uir-call (dot (v "manager") "add") (list (v "x"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-cursor-execute-db)
  ;; cursor.execute / connection.commit / router.db_for_write are SQL
  ;; cursor/connection/router operations on arbitrary receivers → db.
  (define p (prog (fn "f" (uir-call (dot (v "cursor") "execute") (list (uir-string "SELECT 1"))))
                  (fn "g" (uir-call (dot (v "connection") "commit") '()))
                  (fn "h" (uir-call (dot (v "router") "db_for_write") (list (v "model"))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'f) 'db) 'may)
  (check-equal? (modality (hash-ref rows 'g) 'db) 'may)
  (check-equal? (modality (hash-ref rows 'h) 'db) 'may))

(define (test-django-declarative-constructor-pure)
  ;; Model field + query expression constructors are pure class instantiation,
  ;; both dotted (`models.CharField`) and bare (from-imported `CharField`).
  (define p (prog (fn "f" (uir-call (dot (v "models") "CharField") (list (uir-string "max_length"))))
                  (fn "g" (uir-call (v "ForeignKey") (list (uir-string "auth.User"))))
                  (fn "h" (uir-call (v "Q") (list (v "q"))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row))
  (check-equal? (hash-ref rows 'h) (empty-row)))

(define (test-argparse-config-method-pure)
  ;; `parser.add_argument(...)` / `parser.parse_args(...)` mutate a local
  ;; argparse object with no external effect, so they must not be `unknown`.
  (define p (prog (fn "f" (uir-call (dot (v "parser") "add_argument") (list (uir-string "--x"))))
                  (fn "g" (uir-call (dot (v "parser") "parse_args") '()))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'f) (empty-row))
  (check-equal? (hash-ref rows 'g) (empty-row)))

;; ── Django ORM dotted-through-object calls ───────────────────────────
;; `self.queryset.filter(...)`, `obj.save()`, `qs.values_list(...)` have an
;; arbitrary-object receiver, so they cannot resolve as an intra-program method
;; edge; the final method name identifies them as Django ORM operations and
;; maps them to `db`.

(define (test-dotted-orm-filter-db)
  ;; self.queryset.filter(...) → db (receiver is an arbitrary object chain).
  (define p (prog (fn "q" (uir-call (dot (dot (v "self") "queryset") "filter")
                                     (list (uir-record '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'q) 'db) 'may))

(define (test-dotted-orm-save-db)
  ;; obj.save() → db (Model instance save).
  (define p (prog (fn "s" (uir-call (dot (v "obj") "save") '()))))
  (check-equal? (modality (hash-ref (infer-effects p) 's) 'db) 'may))

(define (test-dotted-orm-values-list-db)
  ;; qs.values_list(...) → db.
  (define p (prog (fn "v" (uir-call (dot (v "qs") "values_list")
                                    (list (uir-string "id"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'v) 'db) 'may))

(define (test-bare-filter-is-pure-not-db)
  ;; A bare `filter(...)` is the Python builtin (pure), not an ORM call: the
  ;; ORM heuristic requires a dotted/member-access callee.
  (define p (prog (fn "f" (uir-call (v "filter") (list (v "pred") (v "xs"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-dotted-orm-does-not-steal-effect-roots)
  ;; `requests.get(...)` has a final field `get` (also an ORM read op) but its
  ;; root is a network effect; the network root must win over the ORM heuristic.
  (define p (prog (fn "f" (uir-call (dot (v "requests") "get")
                                    (list (uir-string "/x"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'network) 'may)
  (check-equal? (row-names r) '(network)))


;; ── stdlib effect roots (io / process / time / email) ────────────────
;; `open`/`print`/`os.*`/`subprocess`/`time.sleep`/`django.core.mail.send_mail`
;; perform I/O / process / time / email effects and must resolve instead of
;; falling to `unknown`.

(define (test-bare-open-io)
  (define p (prog (fn "f" (uir-call (v "open") (list (uir-string "/tmp/x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'io) 'may))

(define (test-bare-print-io)
  (define p (prog (fn "f" (uir-call (v "print") (list (uir-string "x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'io) 'may))

(define (test-os-makedirs-io)
  (define p (prog (fn "f" (uir-call (dot (v "os") "makedirs")
                                    (list (uir-string "/tmp/d"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'io) 'may))

(define (test-stream-write-io)
  ;; self.stdout.write(...) → io (dotted method name).
  (define p (prog (fn "f" (uir-call (dot (dot (v "self") "stdout") "write")
                                    (list (uir-string "x"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'io) 'may))

(define (test-subprocess-process)
  (define p (prog (fn "f" (uir-call (dot (v "subprocess") "run")
                                    (list (v "cmd"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'process) 'may))

(define (test-os-system-process)
  (define p (prog (fn "f" (uir-call (dot (v "os") "system")
                                    (list (uir-string "ls"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'process) 'may))

(define (test-time-sleep-time)
  (define p (prog (fn "f" (uir-call (dot (v "time") "sleep")
                                    (list (uir-number "1"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'time) 'may))

(define (test-django-send-mail-email)
  ;; django.core.mail.send_mail(...) → email.
  (define p (prog (fn "f" (uir-call (dot (dot (dot (v "django") "core") "mail")
                                          "send_mail")
                                    (list (uir-string "s") (uir-string "b"))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'email) 'may))

;; ── pure stdlib dotted calls ─────────────────────────────────────────
;; `json.dumps`/`re.compile`/`math.floor`/`datetime.now`/`os.path.join` are
;; pure and must classify pure (empty row), not `unknown`. Effectful stdlib
;; members (`json.load`, `os.path.exists`) stay `unknown`.

(define (test-os-path-join-pure)
  (define p (prog (fn "f" (uir-call (dot (dot (v "os") "path") "join")
                                    (list (uir-string "a") (uir-string "b"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-json-dumps-pure)
  (define p (prog (fn "f" (uir-call (dot (v "json") "dumps")
                                    (list (uir-record '()))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-json-load-stays-unknown)
  ;; json.load performs file I/O; it is deliberately absent from the pure table.
  (define p (prog (fn "f" (uir-call (dot (v "json") "load")
                                    (list (v "fp"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-re-compile-pure)
  (define p (prog (fn "f" (uir-call (dot (v "re") "compile")
                                    (list (uir-string "a+"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-math-floor-pure)
  ;; math is a whole-module-pure root.
  (define p (prog (fn "f" (uir-call (dot (v "math") "floor")
                                    (list (uir-number "1.5"))))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-datetime-now-pure)
  ;; datetime is a whole-module-pure root.
  (define p (prog (fn "f" (uir-call (dot (v "datetime") "now") '()))))
  (check-equal? (hash-ref (infer-effects p) 'f) (empty-row)))

(define (test-os-path-getsize-stays-unknown)
  ;; os.path.getsize is a filesystem/metadata probe, not a pure path op; it must
  ;; not be swept into the pure table (or into io).
  (define p (prog (fn "f" (uir-call (dot (dot (v "os") "path") "getsize")
                                    (list (uir-string "/tmp/x"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'unknown) 'unknown))

;; ── root-aware ORM heuristic ─────────────────────────────────────────
;; The name-based ORM fallback must not claim dotted calls on builtin-type or
;; stdlib-module receivers: `os.path.exists`, `dict.get`, `list.reverse`,
;; `set.update` are not Django ORM operations.

(define (test-os-path-exists-not-db)
  ;; os.path.exists has the ORM read-op name `exists` but a stdlib root; it must
  ;; not classify as `db`. It is a metadata probe, so it stays `unknown`.
  (define p (prog (fn "f" (uir-call (dot (dot (v "os") "path") "exists")
                                    (list (uir-string "/tmp/x"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-false (row-lookup r 'db))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-dict-get-not-db)
  ;; dict.get collides with the QuerySet `get` op; the builtin-type root must win.
  (define p (prog (fn "f" (uir-call (dot (v "dict") "get")
                                    (list (v "d") (uir-string "k"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-false (row-lookup r 'db))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-list-reverse-not-db)
  ;; list.reverse collides with the QuerySet `reverse` op.
  (define p (prog (fn "f" (uir-call (dot (v "list") "reverse")
                                    (list (v "xs"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-false (row-lookup r 'db))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-set-update-not-db)
  ;; set.update collides with the QuerySet `update` op.
  (define p (prog (fn "f" (uir-call (dot (v "set") "update")
                                    (list (v "s") (v "other"))))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-false (row-lookup r 'db))
  (check-equal? (modality r 'unknown) 'unknown))

(define (test-arbitrary-obj-filter-still-db)
  ;; The root-awareness must NOT break the actual ORM case: an arbitrary
  ;; (non-stdlib, non-builtin) receiver root still resolves to db.
  (define p (prog (fn "f" (uir-call (dot (v "queryset") "filter")
                                    (list (uir-record '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'db) 'may))


;; ── Python intra-program method calls ───────────────────────────────
;; `self.m()` / `cls.m()` resolve to the enclosing class's method node
;; (Class.method); `Other.m()` resolves to Other.method. This turns the dominant
;; Django call pattern (self.<method>) into call-graph edges instead of
;; `unknown`.

(define (py-class name methods)
  (uir-class (uir-symbol name) '() '() methods))
(define (py-method name body)
  (uir-method (uir-symbol name) '() body 'public))

(define (test-self-method-resolution)
  ;; A.save hits db; A.run calls self.save() -> run inherits db transitively.
  (define p (prog (py-class "A"
                            (list (py-method "save" (uir-call (dot (v "db") "commit") '()))
                                  (py-method "run"  (uir-call (dot (v "self") "save") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'A.save) 'db) 'may)
  (check-equal? (modality (hash-ref rows 'A.run) 'db) 'may))

(define (test-cls-method-resolution)
  (define p (prog (py-class "A"
                            (list (py-method "save" (uir-call (dot (v "llm") "query") '()))
                                  (py-method "run"  (uir-call (dot (v "cls") "save") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'A.run) 'llm) 'may))

(define (test-other-class-method-resolution)
  ;; A bare function f calls B.m() through the class name -> edge B.m.
  (define p (prog (py-class "B" (list (py-method "m" (uir-call (dot (v "llm") "query") '()))))
                  (fn "f" (uir-call (dot (v "B") "m") '()))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'f) 'llm) 'may))

(define (test-self-method-scoped-to-owner)
  ;; A.m is pure, B.m hits db; A.run calls self.m() and must resolve to A.m
  ;; (pure), not B.m.
  (define p (prog (py-class "A" (list (py-method "m" (uir-null))
                                      (py-method "run" (uir-call (dot (v "self") "m") '()))))
                  (py-class "B" (list (py-method "m" (uir-call (dot (v "db") "x") '()))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'A.run) (empty-row)))

(define (test-unqualified-object-method-stays-unknown)
  ;; `obj.m()` where obj is not self/cls/known-class is unresolvable -> unknown.
  (define p (prog (fn "f" (uir-call (dot (v "obj") "m") '()))))
  (define r (hash-ref (infer-effects p) 'f))
  (check-equal? (modality r 'unknown) 'unknown))

;; ── inherited (MRO) method resolution ───────────────────────────────
;; `self.m()` / `cls.m()` / `Other.m()` must resolve through the superclass
;; chain, so a method defined on a base class is found instead of falling to
;; `unknown`.

(define (py-class/super name super methods)
  (uir-class (uir-symbol name) (if (uir-null? super) '() (list super)) '() methods))

(define (test-inherited-self-method-resolution)
  ;; Base.save hits db; Derived inherits it. Derived.run calls self.save() and
  ;; must inherit db through the superclass chain.
  (define p (prog (py-class/super "Base" (uir-null)
                                  (list (py-method "save" (uir-call (dot (v "db") "commit") '()))))
                  (py-class/super "Derived" (v "Base")
                                  (list (py-method "run" (uir-call (dot (v "self") "save") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'Base.save) 'db) 'may)
  (check-equal? (modality (hash-ref rows 'Derived.run) 'db) 'may))

(define (test-multiple-inheritance-mixin-resolution)
  ;; Derived(A, Mixin) inherits Mixin.save (db) through its SECOND base, even
  ;; though the first base A is undefined. Left-to-right MRO finds Mixin.save.
  (define p (prog (py-class "Mixin"
                            (list (py-method "save" (uir-call (dot (v "db") "commit") '()))))
                  (uir-class (uir-symbol "Derived")
                             (list (uir-var (uir-symbol "A")) (uir-var (uir-symbol "Mixin")))
                             '()
                             (list (py-method "run" (uir-call (dot (v "self") "save") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'Derived.run) 'db) 'may))

(define (test-inherited-cls-method-resolution)
  (define p (prog (py-class/super "Base" (uir-null)
                                  (list (py-method "save" (uir-call (dot (v "llm") "query") '()))))
                  (py-class/super "Derived" (v "Base")
                                  (list (py-method "run" (uir-call (dot (v "cls") "save") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'Derived.run) 'llm) 'may))

(define (test-inherited-other-class-method-resolution)
  ;; A bare function calls Derived.m() where m is inherited from Base.
  (define p (prog (py-class/super "Base" (uir-null)
                                  (list (py-method "m" (uir-call (dot (v "llm") "query") '()))))
                  (py-class/super "Derived" (v "Base") '())
                  (fn "f" (uir-call (dot (v "Derived") "m") '()))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'f) 'llm) 'may))

(define (test-inherited-method-most-derived-wins)
  ;; Base.m is pure, Derived overrides it to hit db. self.m() inside Derived
  ;; must resolve to Derived.m (db), not the inherited pure Base.m.
  (define p (prog (py-class/super "Base" (uir-null)
                                  (list (py-method "m" (uir-null))))
                  (py-class/super "Derived" (v "Base")
                                  (list (py-method "m" (uir-call (dot (v "db") "x") '()))
                                        (py-method "run" (uir-call (dot (v "self") "m") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'Derived.run) 'db) 'may))

(define (test-dotted-super-name-resolution)
  ;; class A(models.Model): a dotted super name lowers to a uir-get whose last
  ;; field is the class name; self.save() must resolve to Model.save.
  (define p (prog (py-class/super "Model" (uir-null)
                                  (list (py-method "save" (uir-call (dot (v "db") "commit") '()))))
                  (py-class/super "A" (uir-get (v "models") (uir-string "Model"))
                                  (list (py-method "run" (uir-call (dot (v "self") "save") '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'A.run) 'db) 'may))

(define (test-super-call-resolution)
  ;; Derived.__init__ calls super().__init__(); Base.__init__ hits db. The
  ;; super() call must resolve to Base.__init__ (skipping Derived itself), so
  ;; Derived.__init__ inherits db instead of `unknown`.
  (define p (prog (py-class/super "Base" (uir-null)
                                  (list (py-method "__init__" (uir-call (dot (v "db") "commit") '()))))
                  (py-class/super "Derived" (v "Base")
                                  (list (py-method "__init__"
                                                   (uir-call (uir-get (uir-call (v "super") '())
                                                                      (uir-string "__init__"))
                                                             '()))))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'Derived.__init__) 'db) 'may))

(define (test-super-call-skips-self-override)
  ;; Derived overrides m (db); Base.m is pure. super().m() must resolve to
  ;; Base.m (pure), NOT Derived.m (db), because super() skips the class itself.
  (define p (prog (py-class/super "Base" (uir-null)
                                  (list (py-method "m" (uir-null))))
                  (py-class/super "Derived" (v "Base")
                                  (list (py-method "m" (uir-call (dot (v "db") "x") '()))
                                        (py-method "run"
                                                   (uir-call (uir-get (uir-call (v "super") '())
                                                                      (uir-string "m"))
                                                             '()))))))
  (define rows (infer-effects p))
  (check-equal? (hash-ref rows 'Derived.run) (empty-row)))

(define (test-super-call-whole-program)
  ;; Base.__init__ (db) lives in another file; Derived.__init__ calls
  ;; super().__init__(). Per-file inference cannot see Base, so it stays
  ;; unknown; whole-program inference resolves the cross-file super() edge.
  (define base (prog (py-class/super "Base" (uir-null)
                                     (list (py-method "__init__" (uir-call (dot (v "db") "commit") '()))))))
  (define app (prog (py-class/super "Derived" (v "Base")
                                    (list (py-method "__init__"
                                                     (uir-call (uir-get (uir-call (v "super") '())
                                                                        (uir-string "__init__"))
                                                               '()))))))
  (check-equal? (modality (hash-ref (infer-effects app) 'Derived.__init__) 'unknown) 'unknown)
  (define rows (infer-effects-all (list (cons "base.rkt" base) (cons "app.rkt" app))))
  (check-equal? (modality (hash-ref rows (cons "app.rkt" 'Derived.__init__)) 'db) 'may))

(define (test-whole-program-cross-file-inheritance)
  ;; BaseService.compute (db) lives in one program; A.run() in a separate
  ;; program calls self.compute() through A's superclass chain. Per-file
  ;; inference cannot see BaseService (it is in another file), so A.run stays
  ;; unknown; whole-program inference resolves the cross-file edge to db.
  (define base (prog (py-class/super "BaseService" (uir-null)
                                     (list (py-method "compute" (uir-call (dot (v "db") "commit") '()))))))
  (define app (prog (py-class/super "A" (v "BaseService")
                                    (list (py-method "run" (uir-call (dot (v "self") "compute") '()))))))
  (check-equal? (modality (hash-ref (infer-effects app) 'A.run) 'unknown) 'unknown)
  (define rows (infer-effects-all (list (cons "base.rkt" base) (cons "app.rkt" app))))
  (check-equal? (modality (hash-ref rows (cons "base.rkt" 'BaseService.compute)) 'db) 'may)
  (check-equal? (modality (hash-ref rows (cons "app.rkt" 'A.run)) 'db) 'may))

(define (test-whole-program-collision-stays-distinct)
  ;; Two files each define their own class Command with a handle() method; the
  ;; methods have different effects. A self.handle() call must resolve to the
  ;; LOCAL file's Command.handle, not the merged cross-file body.
  (define a (prog (py-class/super "Command" (uir-null)
                                  (list (py-method "handle" (uir-call (dot (v "db") "commit") '()))
                                        (py-method "run" (uir-call (dot (v "self") "handle") '()))))))
  (define b (prog (py-class/super "Command" (uir-null)
                                  (list (py-method "handle" (uir-call (dot (v "llm") "query") '()))))))
  (define rows (infer-effects-all (list (cons "a.rkt" a) (cons "b.rkt" b))))
  ;; a.rkt's Command.run → a.rkt's Command.handle → db (NOT b.rkt's llm).
  (check-equal? (modality (hash-ref rows (cons "a.rkt" 'Command.handle)) 'db) 'may)
  (check-equal? (modality (hash-ref rows (cons "a.rkt" 'Command.run)) 'db) 'may)
  (check-equal? (modality (hash-ref rows (cons "b.rkt" 'Command.handle)) 'llm) 'may))

(define (test-imported-class-cap-effect)
  ;; `conn.execute(...)` where `conn` was imported to a class whose `execute`
  ;; is tagged db.write: resolves through the import graph to a typed db.write.
  (define p (prog (fn "run"
                      (uir-call (dot (v "conn") "execute")
                                (list (uir-string "SELECT 1"))))))
  (define env
    (import-env (hash "app.py" "app")
                (hash "app" (hash "conn" (handle 'class "mydb.conn" "Connection")))
                (hash "Connection.execute" (effect-cap 'db '(db<Connection>.write)))))
  (define r (hash-ref (infer-effects-all (list (cons "app.py" p)) #:import-env env)
                      (cons "app.py" 'run)))
  (check-equal? (row-names r) '(db))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (entry-ops (row-lookup r 'db)) '(db<Connection>.write)))

(define (test-imported-receiver-dataflow)
  ;; `qs.filter(...)` where `qs` was imported as a CLASS from a db-tagged module
  ;; (no blessed caps entry): resolves class→module→domain and derives the typed
  ;; read op from the method name, the same as a locally-tracked receiver.
  (define p (prog (fn "run"
                      (uir-call (dot (v "qs") "filter")
                                (list (uir-record '()))))))
  (define env
    (import-env (hash "app.py" "app")
                (hash "app" (hash "qs" (handle 'class "django.db.models" "QuerySet")))
                (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "app.py" p)) #:import-env env)
                      (cons "app.py" 'run)))
  (check-equal? (row-names r) '(db))
  (check-equal? (modality r 'db) 'may)
  (check-equal? (entry-ops (row-lookup r 'db)) '(db<QuerySet>.read)))

(define (test-imported-module-pure)
  ;; `sqlmod.parse(...)` where `sqlmod` resolves to module `sqlparse` (tagged
  ;; pure): classified pure (no entry), not `unknown`.
  (define p (prog (fn "fmt"
                      (uir-call (dot (v "sqlmod") "parse")
                                (list (uir-string "x"))))))
  (define env
    (import-env (hash "app.py" "app")
                (hash "app" (hash "sqlmod" (handle 'module "sqlparse" #f)))
                (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "app.py" p)) #:import-env env)
                      (cons "app.py" 'fmt)))
  (check-equal? (row-names r) '()))

;; ── receiver-type dataflow ──────────────────────────────────────────
;; `x = KnownClass(...)` / `self.attr = KnownClass(...)` tracks the receiver's
;; class, so a later `x.m(...)` resolves through the class's derived capability
;; instead of the flat method-name tables.

(define (test-receiver-type-local-constructor)
  ;; `qs = QuerySet(); qs.filter(...)` resolves through the tracked receiver
  ;; class to a typed db.read, not the flat ORM method-name heuristic.
  (define models (prog (py-class "QuerySet" (list (py-method "filter" (uir-null))))))
  (define app
    (prog (fn "run"
              (uir-block
               (list (uir-set! (v "qs") (uir-call (v "QuerySet") '()))
                     (uir-call (dot (v "qs") "filter") '()))))))
  (define env
    (import-env (hash "models.py" "django.db.models"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "models.py" models)
                                               (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'run)))
  (check-equal? (row-names r) '(db))
  (check-equal? (entry-ops (row-lookup r 'db)) '(db<QuerySet>.read)))

(define (test-receiver-type-self-attr)
  ;; `self.qs = QuerySet(); self.qs.filter(...)` in the same method: the
  ;; self-attribute is tracked and resolved to a typed db.read.
  (define models (prog (py-class "QuerySet" (list (py-method "filter" (uir-null))))))
  (define app
    (prog (py-class "Repo"
                    (list (py-method "run"
                                     (uir-block
                                      (list (uir-set! (uir-get (v "self") (sym "qs"))
                                                      (uir-call (v "QuerySet") '()))
                                            (uir-call (dot (uir-get (v "self") (sym "qs")) "filter")
                                                      '()))))))))
  (define env
    (import-env (hash "models.py" "django.db.models"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "models.py" models)
                                               (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'Repo.run)))
  (check-equal? (row-names r) '(db))
  (check-equal? (entry-ops (row-lookup r 'db)) '(db<QuerySet>.read)))

(define (test-receiver-type-pure-class)
  ;; A receiver tracked to a class in a pure module is pure even when its
  ;; method name collides with an ORM verb (`filter`).
  (define parser (prog (py-class "MyParser" (list (py-method "filter" (uir-null))))))
  (define app
    (prog (fn "run"
              (uir-block
               (list (uir-set! (v "p") (uir-call (v "MyParser") '()))
                     (uir-call (dot (v "p") "filter") '()))))))
  (define env
    (import-env (hash "parser.py" "sqlparse"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "parser.py" parser)
                                                (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'run)))
  (check-equal? (row-names r) '()))

(define (test-receiver-type-class-attr-cross-method)
  ;; `self.stdout = OutputWrapper()` set in __init__ flows to handle() (same
  ;; class, via a subclass in the same file), so self.stdout.write resolves
  ;; through the tracked receiver to a typed io<OutputWrapper>.write rather than
  ;; the coarse `io` name heuristic.
  (define io (prog (py-class "OutputWrapper" (list (py-method "write" (uir-null))))))
  (define app
    (prog (py-class "BaseCommand"
                    (list (py-method "__init__"
                                     (uir-set! (uir-get (v "self") (sym "stdout"))
                                               (uir-call (v "OutputWrapper") '())))))
          (py-class/super "Command" (v "BaseCommand")
                          (list (py-method "handle"
                                           (uir-call (dot (uir-get (v "self") (sym "stdout")) "write")
                                                     (list (uir-string "x"))))))))
  (define env
    (import-env (hash "io.py" "django.core.management.base"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "io.py" io) (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'Command.handle)))
  (check-equal? (row-names r) '(io))
  (check-equal? (entry-ops (row-lookup r 'io)) '(io<OutputWrapper>.write)))

(define (test-receiver-type-class-attr-cross-file)
  ;; BaseCommand.__init__ (one file) sets self.stdout; Command.handle (another
  ;; file, a subclass) uses it. The per-class env follows the MRO across files.
  (define io (prog (py-class "OutputWrapper" (list (py-method "write" (uir-null))))))
  (define base
    (prog (py-class "BaseCommand"
                    (list (py-method "__init__"
                                     (uir-set! (uir-get (v "self") (sym "stdout"))
                                               (uir-call (v "OutputWrapper") '())))))))
  (define app
    (prog (py-class/super "Command" (v "BaseCommand")
                          (list (py-method "handle"
                                           (uir-call (dot (uir-get (v "self") (sym "stdout")) "write")
                                                     (list (uir-string "x"))))))))
  (define env
    (import-env (hash "io.py" "django.core.management.base"
                      "base.py" "app"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "io.py" io)
                                               (cons "base.py" base)
                                               (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'Command.handle)))
  (check-equal? (row-names r) '(io))
  (check-equal? (entry-ops (row-lookup r 'io)) '(io<OutputWrapper>.write)))

(define (test-receiver-type-manager-attr)
  ;; `User.objects.filter(...)`: `objects` is set by the framework metaclass, not
  ;; by an assignment in any method body, so the receiver-type env cannot see it.
  ;; Resolve `User` to its defining module's domain instead of the `objects`
  ;; name table, yielding the typed db<User>.read op.
  (define models (prog (py-class "User" (list (py-method "save" (uir-null))))))
  (define app
    (prog (fn "q" (uir-call (dot (dot (v "User") "objects") "filter")
                            (list (uir-record '()))))))
  (define env
    (import-env (hash "models.py" "django.db.models"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "models.py" models)
                                               (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'q)))
  (check-equal? (row-names r) '(db))
  (check-equal? (entry-ops (row-lookup r 'db)) '(db<User>.read)))

(define (test-receiver-type-manager-attr-pure)
  ;; A class in a pure module used as a manager root is pure even though the
  ;; method name (`filter`) collides with an ORM verb.
  (define parser (prog (py-class "MyParser" (list (py-method "filter" (uir-null))))))
  (define app
    (prog (fn "q" (uir-call (dot (dot (v "MyParser") "objects") "filter")
                            (list (uir-record '()))))))
  (define env
    (import-env (hash "parser.py" "sqlparse"
                      "app.py" "app")
                (hash) (hash)))
  (define r (hash-ref (infer-effects-all (list (cons "parser.py" parser)
                                               (cons "app.py" app))
                                         #:import-env env)
                      (cons "app.py" 'q)))
  (check-equal? (row-names r) '()))

(define (test-annotate-writes-back)
  (define p (prog (fn "summarize"
                      (uir-call (dot (v "llm") "query")
                                (list (uir-string "hi"))))))
  (define annotated (annotate-effects p))
  (define f (car (uir-block-stmts annotated)))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

(define (test-annotate-preserves-explicit-row)
  ;; A hand-written row (budgets/scopes) wins over inference; inference must
  ;; not clobber it with the row inferred from the body.
  (define explicit
    (uir-effect-row
     (list (uir-effect-entry (sym "cache") 'must-not '() 3 #f))))
  (define p (prog (struct-copy uir-fn (fn "f" (uir-call (dot (v "llm") "query") '()))
                                 [effects explicit])))
  (define f (car (uir-block-stmts (annotate-effects p))))
  (check-true (uir-fn? f))
  (check-equal? (row-names (uir-fn-effects f)) '(cache))
  (check-equal? (modality (uir-fn-effects f) 'cache) 'must-not))

(define (test-annotate-skips-empty-inferred)
  ;; A pure function with no annotation stays unannotated (effects field #f),
  ;; so emitters do not treat it as effect-using.
  (define p (prog (fn "plain" (uir-null))))
  (define f (car (uir-block-stmts (annotate-effects p))))
  (check-false (uir-fn-effects f)))

(define (test-annotate-skips-unknown-only)
  ;; 'unknown is a checker signal ("could not verify"), not a runtime effect.
  ;; A function whose inferred row is only 'unknown must not gain a runtime
  ;; guard, since the guard rejects the 'unknown modality.
  (define p (prog (fn "f" (uir-call (v "mystery") '()))))
  (define f (car (uir-block-stmts (annotate-effects p))))
  (check-false (uir-fn-effects f)))

;; ── JS/TS function-declaration encodings ────────────────────────────
;; TS lowers `function NAME(...) {...}` to (set! NAME (fn #f ...)),
;; async to (set! NAME (call "async-fn" (list (fn #f ...)))), generator to
;; (set! NAME (call "gen-fn" (list (fn #f ...)))). collect-functions must key
;; these by the set! name, not by a uir-fn name.

(define (test-set!-fn-decl-collected)
  (define p (prog (uir-set! (sym "summarize")
                            (fn #f (uir-call (dot (v "llm") "query")
                                             (list (uir-string "hi")))))))
  (define r (hash-ref (infer-effects p) 'summarize))
  (check-equal? (modality r 'llm) 'may)
  (check-equal? (row-names r) '(llm)))

(define (test-set!-async-fn-decl)
  (define inner (fn #f (uir-call (dot (v "llm") "query")
                                 (list (uir-string "hi")))))
  (define p (prog (uir-set! (sym "summarize")
                            (uir-call (sym "async-fn") (list inner)))))
  (check-equal? (modality (hash-ref (infer-effects p) 'summarize) 'llm) 'may))

(define (test-set!-gen-fn-decl)
  (define inner (fn #f (uir-call (dot (v "llm") "query")
                                 (list (uir-string "hi")))))
  (define p (prog (uir-set! (sym "summarize")
                            (uir-call (sym "gen-fn") (list inner)))))
  (check-equal? (modality (hash-ref (infer-effects p) 'summarize) 'llm) 'may))

(define (test-set!-fn-annotate-writes-back)
  (define p (prog (uir-set! (sym "summarize")
                            (fn #f (uir-call (dot (v "llm") "query")
                                             (list (uir-string "hi")))))))
  (define annotated (annotate-effects p))
  (define decl (car (uir-block-stmts annotated)))
  (check-true (uir-set!? decl))
  (define f (uir-set!-value decl))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

(define (test-set!-async-fn-annotate-writes-back)
  (define inner (fn #f (uir-call (dot (v "llm") "query")
                                 (list (uir-string "hi")))))
  (define p (prog (uir-set! (sym "summarize")
                            (uir-call (sym "async-fn") (list inner)))))
  (define annotated (annotate-effects p))
  (define decl (car (uir-block-stmts annotated)))
  (define wrap (uir-set!-value decl))
  (check-true (uir-call? wrap))
  (define f (first (uir-call-args wrap)))
  (check-true (uir-fn? f))
  (check-equal? (modality (uir-fn-effects f) 'llm) 'may))

;; ── JS/TS arrow-function encodings ──────────────────────────────────
;; TS lowers `(...) => body` to (call "=>" (list params body)). The body is a
;; nested function boundary, and an arrow bound to a name via set! is a named
;; function for inference.

(define (test-arrow-boundary-does-not-leak)
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") '()))))
  (define p (prog (fn "outer" (uir-block (list arrow (uir-null))))))
  (check-equal? (hash-ref (infer-effects p) 'outer) (empty-row)))

(define (test-set!-arrow-inferred)
  (define p (prog (uir-set! (sym "f")
                            (uir-call (sym "=>")
                                      (list (uir-list '())
                                            (uir-call (jsdot (v "llm") "query") '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

;; ── JS/TS anonymous `function` expressions ──────────────────────────
;; TS lowers `function (params) { body }` to (call "function" (list params body)),
;; a sibling of `=>`. The body is a nested function boundary, and a function
;; expression bound to a name via set! is a named function for inference.

(define (test-function-expr-boundary-does-not-leak)
  (define fexpr (uir-call (sym "function")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") '()))))
  (define p (prog (fn "outer" (uir-block (list fexpr (uir-null))))))
  (check-equal? (hash-ref (infer-effects p) 'outer) (empty-row)))

(define (test-set!-function-inferred)
  (define p (prog (uir-set! (sym "f")
                            (uir-call (sym "function")
                                      (list (uir-list '())
                                            (uir-call (jsdot (v "llm") "query") '()))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

;; ── JS/TS const/let/var-bound arrow & function encodings ─────────────
;; TS lowers `const f = () => ...` to
;; (call "const" (list (set! f (call "=>" (list params body))))) — the set!
;; sits one level deeper than a bare function declaration, so collect-functions
;; must unwrap the var/let/const call to reach the named binding.

(define (var-decl kind binding)
  (uir-call (sym kind) (list binding)))

(define (test-const-arrow-inferred)
  (define p (prog (var-decl "const"
                            (uir-set! (sym "f")
                                      (uir-call (sym "=>")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-let-arrow-inferred)
  (define p (prog (var-decl "let"
                            (uir-set! (sym "f")
                                      (uir-call (sym "=>")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-var-arrow-inferred)
  (define p (prog (var-decl "var"
                            (uir-set! (sym "f")
                                      (uir-call (sym "=>")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-const-arrow-caller-inherits)
  ;; main calls a const-bound arrow f; f's llm effect must propagate to main.
  (define f (var-decl "const"
                      (uir-set! (sym "f")
                                (uir-call (sym "=>")
                                          (list (uir-list '())
                                                (uir-call (jsdot (v "llm") "query") '()))))))
  (define p (prog f
                  (fn "main" (uir-call (v "f") '()))))
  (define rows (infer-effects p))
  (check-equal? (modality (hash-ref rows 'f) 'llm) 'may)
  (check-equal? (modality (hash-ref rows 'main) 'llm) 'may))

(define (test-const-function-expr-inferred)
  (define p (prog (var-decl "const"
                            (uir-set! (sym "f")
                                      (uir-call (sym "function")
                                                (list (uir-list '())
                                                      (uir-call (jsdot (v "llm") "query") '())))))))
  (check-equal? (modality (hash-ref (infer-effects p) 'f) 'llm) 'may))

(define (test-const-arrow-annotate-wraps)
  ;; annotate-effects must descend into the const wrapper and wrap the arrow in
  ;; (call "__withEffects" (list row arrow)) so the JS emitter can render a
  ;; guard for a function value with no effects slot.
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (jsdot (v "llm") "query") '()))))
  (define p (prog (var-decl "const" (uir-set! (sym "f") arrow))))
  (define decl (car (uir-block-stmts (annotate-effects p))))
  (check-true (uir-call? decl))
  (check-equal? (uir-symbol-name (uir-call-callee decl)) "const")
  (define binding (first (uir-call-args decl)))
  (check-true (uir-set!? binding))
  (define wrapped (uir-set!-value binding))
  (check-true (uir-call? wrapped))
  (check-equal? (uir-symbol-name (uir-call-callee wrapped)) "__withEffects")
  (define row (first (uir-call-args wrapped)))
  (check-true (uir-effect-row? row))
  (check-equal? (modality row 'llm) 'may))

(define (test-const-arrow-annotate-no-wrap-when-pure)
  ;; A pure const-bound arrow gains no __withEffects wrapper.
  (define arrow (uir-call (sym "=>")
                          (list (uir-list '())
                                (uir-call (sym "+")
                                          (list (uir-number "1") (uir-number "2"))))))
  (define p (prog (var-decl "const" (uir-set! (sym "f") arrow))))
  (define decl (car (uir-block-stmts (annotate-effects p))))
  (define binding (first (uir-call-args decl)))
  (define wrapped (uir-set!-value binding))
  (check-true (and (uir-call? wrapped)
                   (uir-symbol? (uir-call-callee wrapped))
                   (string=? (uir-symbol-name (uir-call-callee wrapped)) "=>"))
              "pure const arrow is left unwrapped"))

(module+ main
  (test-empty-program)
  (test-lambda-not-collected)
  (test-pure-function)
  (test-injectable-effect-tokens)
  (test-direct-llm)
  (test-direct-db-dotted)
  (test-django-model-objects)
  (test-direct-network)
  (test-direct-cache)
  (test-transitive)
  (test-higher-order-unknown)
  (test-nested-fn-does-not-leak)
  (test-recursion-converges)
  (test-httpx-network)
  (test-requests-network)
  (test-redis-cache)
  (test-anthropic-llm)
  (test-agno-llm)
  (test-langgraph-llm)
  (test-django-objects-string-field)
  (test-js-dot-axios-network)
  (test-js-dot-db-db)
  (test-js-dot-model-objects-db)
  (test-js-dot-redis-cache)
  (test-js-dot-anthropic-llm)
  (test-js-dot-bare-fetch-network)
  (test-handler-primitive-is-pure)
  (test-op-dotted-llm-query)
  (test-op-dotted-nested-last-field)
  (test-op-dotted-objects-filter)
  (test-op-bare-fetch)
  (test-op-python-get-string-field)
  (test-synthetic-operator-pure)
  (test-synthetic-dot-pure)
  (test-python-identity-membership-operators-pure)
  (test-python-boolean-operators-pure)
  (test-python-floor-div-pure)
  (test-python-builtin-pure)
  (test-exception-constructor-pure)
  (test-dotted-exception-constructor-pure)
  (test-non-error-suffix-stays-unknown)
  (test-warning-and-exception-suffix-pure)
  (test-pure-stdlib-bare)
  (test-chain-from-iterable-pure)
  (test-warnings-warn-io)
  (test-dunder-method-pure)
  (test-gettext-alias-pure)
  (test-gettext-family-pure)
  (test-string-literal-method-pure)
  (test-django-exception-constructor-pure)
  (test-sys-exit-process)
  (test-container-method-pure)
  (test-container-ambiguous-name-stays-unknown)
  (test-cursor-execute-db)
  (test-django-declarative-constructor-pure)
  (test-argparse-config-method-pure)
  (test-dotted-orm-filter-db)
  (test-dotted-orm-save-db)
  (test-dotted-orm-values-list-db)
  (test-bare-filter-is-pure-not-db)
  (test-dotted-orm-does-not-steal-effect-roots)
  (test-bare-open-io)
  (test-bare-print-io)
  (test-os-makedirs-io)
  (test-stream-write-io)
  (test-subprocess-process)
  (test-os-system-process)
  (test-time-sleep-time)
  (test-django-send-mail-email)
  (test-os-path-join-pure)
  (test-json-dumps-pure)
  (test-json-load-stays-unknown)
  (test-re-compile-pure)
  (test-math-floor-pure)
  (test-datetime-now-pure)
  (test-os-path-getsize-stays-unknown)
  (test-os-path-exists-not-db)
  (test-dict-get-not-db)
  (test-list-reverse-not-db)
  (test-set-update-not-db)
  (test-arbitrary-obj-filter-still-db)
  (test-self-method-resolution)
  (test-cls-method-resolution)
  (test-other-class-method-resolution)
  (test-self-method-scoped-to-owner)
  (test-unqualified-object-method-stays-unknown)
  (test-inherited-self-method-resolution)
  (test-multiple-inheritance-mixin-resolution)
  (test-inherited-cls-method-resolution)
  (test-inherited-other-class-method-resolution)
  (test-inherited-method-most-derived-wins)
  (test-dotted-super-name-resolution)
  (test-super-call-resolution)
  (test-super-call-skips-self-override)
  (test-super-call-whole-program)
  (test-whole-program-cross-file-inheritance)
  (test-whole-program-collision-stays-distinct)
  (test-imported-class-cap-effect)
  (test-imported-receiver-dataflow)
  (test-imported-module-pure)
  (test-receiver-type-local-constructor)
  (test-receiver-type-self-attr)
  (test-receiver-type-pure-class)
  (test-receiver-type-class-attr-cross-method)
  (test-receiver-type-class-attr-cross-file)
  (test-receiver-type-manager-attr)
  (test-receiver-type-manager-attr-pure)
  (test-annotate-writes-back)
  (test-annotate-preserves-explicit-row)
  (test-annotate-skips-empty-inferred)
  (test-annotate-skips-unknown-only)
  (test-set!-fn-decl-collected)
  (test-set!-async-fn-decl)
  (test-set!-gen-fn-decl)
  (test-set!-fn-annotate-writes-back)
  (test-set!-async-fn-annotate-writes-back)
  (test-arrow-boundary-does-not-leak)
  (test-set!-arrow-inferred)
  (test-function-expr-boundary-does-not-leak)
  (test-set!-function-inferred)
  (test-const-arrow-inferred)
  (test-let-arrow-inferred)
  (test-var-arrow-inferred)
  (test-const-arrow-caller-inherits)
  (test-const-function-expr-inferred)
  (test-const-arrow-annotate-wraps)
  (test-const-arrow-annotate-no-wrap-when-pure)
  (displayln "All infer tests passed."))
