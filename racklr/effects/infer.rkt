#lang racket

;; Effect inference (Pass 2.4).
;;
;; Computes effect rows for functions lacking annotations via a monotone
;; fixpoint over the call graph. Semantics follow docs/effects-proposal.md §6:
;;   - build the call graph over named uir-fn nodes
;;   - initialize every function's row to ⊥ (empty)
;;   - iterate to fixpoint: row = union(direct effects, callee rows)
;;   - direct effects: llm/openai → llm, db/database/sql/Model.objects → db,
;;     fetch/axios → network, revalidatePath/invalidate → cache
;;   - unresolvable (higher-order) calls → an entry named 'unknown with
;;     modality 'unknown, deferred to runtime

(require racklr/uir
         racklr/effects/contract
         racklr/effects/ts-annot)

(provide
  infer-effects
  infer-effects-all
  annotate-effects
  collect-functions
  collect-methods
  classify-call
  direct-effect-of-callee
  effect-op-of-callee
  empty-row)

;; ── generic structural walk ─────────────────────────────────────────

;; Visit every struct node reachable from `n` (a node or a plain list),
;; applying `visit!` to each struct node exactly once (pre-order).
(define (walk-uir n visit!)
  (cond
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-uir f visit!))]
    [(pair? n)
     (walk-uir (car n) visit!)
     (walk-uir (cdr n) visit!)]
    [else (void)]))

;; Like walk-uir but does not descend into nested uir-fn nodes: a nested
;; function is a separate call-graph node whose own body is analyzed on its
;; own, so its effects must not leak into the enclosing function.
(define (walk-body n visit!)
  (cond
    [(uir-fn? n) (void)]
    [(arrow-call? n) (void)]
    [(function-expr-call? n) (void)]
    [(struct? n)
     (visit! n)
     (for ([f (in-list (cdr (vector->list (struct->vector n))))])
       (walk-body f visit!))]
    [(pair? n)
     (walk-body (car n) visit!)
     (walk-body (cdr n) visit!)]
    [else (void)]))

;; ── callee classification ───────────────────────────────────────────

;; Leftmost root name of a (possibly dotted/namespaced) callee expression,
;; as a symbol. Namespace-qualified names split on '/': "llm/query" → llm.
(define (dotted-root c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (loop (uir-get-base c))]
      [(dot-call? c) (loop (first (uir-call-args c)))]
      [(uir-var? c) (symbol-root (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (symbol-root (uir-symbol-name c))]
      [else #f])))

;; Root of a (possibly namespace-qualified) symbol name string.
(define (symbol-root name)
  (let loop ([i 0])
    (cond [(>= i (string-length name)) (string->symbol name)]
          [(char=? (string-ref name i) #\/) (string->symbol (substring name 0 i))]
          [else (loop (add1 i))])))

;; A JS/TS/JSX member access: `a.b` lowers to (call (symbol "dot") (list a b)).
(define (dot-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "dot")
       (= (length (uir-call-args c)) 2)))

;; A JS/TS/JSX arrow function expression: (call "=>" (list params body)).
(define (arrow-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "=>")
       (= (length (uir-call-args c)) 2)))

;; A JS/TS anonymous function expression: (call "function" (list params body)),
;; a sibling of `=>`.
(define (function-expr-call? c)
  (and (uir-call? c)
       (uir-symbol? (uir-call-callee c))
       (string=? (uir-symbol-name (uir-call-callee c)) "function")
       (= (length (uir-call-args c)) 2)))

;; Field name of a `uir-get` as a symbol. Front ends differ on whether an
;; attribute name is a uir-symbol or a uir-string, so accept both.
(define (get-field-symbol f)
  (cond
    [(uir-symbol? f) (string->symbol (uir-symbol-name f))]
    [(uir-string? f) (string->symbol (uir-string-value f))]
    [else #f]))

;; Does a dotted chain contain a field access named `field-name`? Handles both
;; the Python uir-get encoding and the JS/TS (call "dot" ...) encoding.
(define (dotted-has-field? c field-name)
  (let loop ([c c])
    (cond
      [(uir-get? c)
       (or (eq? (get-field-symbol (uir-get-field c)) field-name)
           (loop (uir-get-base c)))]
      [(dot-call? c)
       (define field (second (uir-call-args c)))
       (or (and (uir-symbol? field)
                (string=? (uir-symbol-name field) (symbol->string field-name)))
           (loop (first (uir-call-args c))))]
      [else #f])))

;; Simple (non-dotted) callee name: a uir-var or uir-symbol.
(define (callee-name c)
  (cond
    [(uir-var? c) (string->symbol (uir-symbol-name (uir-var-name c)))]
    [(uir-symbol? c) (string->symbol (uir-symbol-name c))]
    [else #f]))

;; Operation performed by a callee: the last field of a dotted chain, or the
;; bare callee name. Used by the emitters to instrument runtime guards with the
;; specific operation being performed. Returns a symbol or #f.
(define (effect-op-of-callee c)
  (let loop ([c c])
    (cond
      [(uir-get? c) (get-field-symbol (uir-get-field c))]
      [(dot-call? c) (get-field-symbol (second (uir-call-args c)))]
      [(uir-var? c) (string->symbol (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (string->symbol (uir-symbol-name c))]
      [else #f])))

;; A Python member access (uir-get) or JS/TS/JSX (call "dot" ...) callee: a
;; dotted-through-object chain, as opposed to a bare var/symbol callee. Used to
;; distinguish `obj.filter(...)` (ORM) from a bare `filter(...)` (builtin).
(define (member-access-callee? c)
  (or (uir-get? c) (dot-call? c)))

;; Django ORM method names. A dotted call whose final field is one of these is
;; a QuerySet / Manager / Model DB operation even when the receiver is an
;; arbitrary object (`self.queryset.filter`, `obj.save`, `qs.values_list`) and
;; therefore cannot be resolved as an intra-program method edge. This is a
;; name-based heuristic: the same names occur on non-ORM objects, so the effect
;; roots checked earlier in `direct-effect-of-callee` (`requests.get` → network,
;; `redis.get` → cache, `anthropic.messages.create` → llm) take precedence and
;; this clause runs last.
(define django-orm-methods
  (set
   ;; QuerySet / Manager read API
   "all" "annotate" "aggregate" "alias" "as_manager" "count" "dates"
   "datetimes" "db_manager" "defer" "difference" "distinct" "earliest"
   "exclude" "exists" "explain" "extra" "filter" "first" "get" "in_bulk"
   "intersection" "iterator" "last" "latest" "none" "only" "order_by"
   "prefetch_related" "raw" "reverse" "select_related" "union" "using"
   "values" "values_list"
   ;; QuerySet / Manager write API
   "bulk_create" "bulk_update" "create" "delete" "get_or_create" "update"
   "update_or_create"
   ;; Model instance DB methods
   "save" "refresh_from_db"
   ;; SQL cursor / connection / router methods (`cursor.execute`, `conn.commit`,
   ;; `connection.fetchall`, `router.db_for_write`). The receiver is an arbitrary
   ;; object (a cursor, a connection wrapper, a router) that cannot resolve as an
   ;; intra-program edge, so the final method name identifies it as a DB effect.
   "execute" "executemany" "fetchone" "fetchall" "fetchmany" "commit"
   "rollback" "cursor" "create_function" "create_aggregate"
   "create_deterministic_function" "db_for_read" "db_for_write"))

;; Builtin type roots whose method namespace collides with the ORM method-name
;; heuristic. A dotted call rooted at one of these (`dict.get`, `list.reverse`,
;; `set.update`, `str.count`) is a plain Python builtin method, never a Django
;; ORM operation.
(define builtin-type-roots
  (set "dict" "list" "set" "str" "int" "float" "complex" "bool" "bytes"
       "bytearray" "tuple" "frozenset" "type" "object" "range" "slice"
       "memoryview"))

;; Standard-library module roots whose attribute namespace collides with the
;; ORM heuristic (`os.path.exists`, `pathlib.Path.exists`). A known stdlib
;; module is never a Django model/queryset receiver, so the ORM fallback must
;; not claim its calls.
(define stdlib-module-roots
  (set "os" "sys" "io" "time" "re" "json" "pathlib" "logging" "email"
       "warnings" "traceback" "inspect" "random" "uuid" "importlib" "pickle"
       "shelve" "sqlite3" "csv" "glob" "fnmatch" "tarfile" "zipfile" "gzip"
       "bz2" "lzma" "codecs" "subprocess" "multiprocessing" "shutil" "tempfile"
       "fileinput" "smtplib" "http" "socket" "urllib3" "math" "cmath"
       "itertools" "functools" "operator" "collections" "string" "textwrap"
       "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy" "decimal"
       "fractions" "ipaddress" "struct" "array" "bisect" "heapq" "hashlib" "hmac"
       "base64" "binascii" "unicodedata" "calendar" "keyword" "datetime"))

;; Is `root` a builtin type or stdlib module name? Used to keep the name-based
;; ORM fallback from claiming dotted calls on non-Django receivers.
(define (non-orm-root? root)
  (and root
       (or (set-member? builtin-type-roots (symbol->string root))
           (set-member? stdlib-module-roots (symbol->string root)))))

(define (django-orm-callee? c)
  (and (member-access-callee? c)
       (let* ([op (effect-op-of-callee c)]
              [root (dotted-root c)])
         (and op
              (set-member? django-orm-methods (symbol->string op))
              (not (non-orm-root? root))))))

;; ── stdlib effect roots ─────────────────────────────────────────────
;;
;; Standard-library / framework calls that perform an effect but are not
;; covered by the primary roots (llm/db/network/cache). These map to the
;; opaque effect names `io`, `process`, `time`, and `email` (plus `network`
;; for raw-socket/HTTP-client modules). Detection is root- or method-name
;; based: a final-field name can occur on unrelated objects, so the stronger
;; roots checked earlier in `direct-effect-of-callee` (`requests.get` →
;; network, `redis.get` → cache, `anthropic.messages.create` → llm) win, and
;; `django-orm-callee?` runs after these heuristics.

;; Bare builtins that perform console/file I/O. `open`/`print`/`input` are
;; deliberately absent from `python-builtins` (they perform I/O); they map to
;; `io` here.
(define io-builtins (set "open" "print" "input"))

;; Module roots whose every call is an effect (unambiguous).
(define stdlib-root-effects
  (hash 'subprocess 'process
        'multiprocessing 'process
        'shutil 'io
        'tempfile 'io
        'fileinput 'io
        'smtplib 'email
        'http 'network
        'socket 'network
        'urllib3 'network))

;; Dotted methods that perform file/stream I/O regardless of receiver
;; (`f.read`, `self.stdout.write`, `response.write`, `io.open`, `os.open`,
;; `pathlib.Path.open`, `tarfile.open`, ...).
(define io-methods
  (set "open" "read" "readline" "readlines" "read1" "readinto" "readinto1"
       "write" "writelines" "flush" "seek" "tell" "truncate" "fileno"))

;; Dotted methods that send email (Django `django.core.mail.send_mail` and
;; friends; Flask-Mail uses the same names).
(define email-methods
  (set "send_mail" "send_mass_mail" "mail_admins" "mail_managers"))

;; `os.*` filesystem operations (root-qualified so they do not collide with
;; the generic `io-methods` above, and so pure `os.path.*` helpers like
;; `os.path.join` are not swept in).
(define os-io-methods
  (set "remove" "unlink" "rename" "renames" "replace" "mkdir" "makedirs"
       "rmdir" "removedirs" "chdir" "chmod" "chown" "utime" "link" "symlink"
       "readlink" "listdir" "scandir" "stat" "lstat" "fsync" "fdatasync"))

;; `os.*` process-spawning / process-control operations.
(define os-process-methods
  (set "system" "popen" "popen2" "popen3" "popen4"
       "spawnl" "spawnle" "spawnlp" "spawnlpe" "spawnv" "spawnve" "spawnvp"
       "spawnvpe" "execl" "execle" "execlp" "execlpe" "execv" "execve"
       "execvp" "execvpe" "fork" "forkpty" "kill" "killpg" "startfile"
       "posix_spawn" "posix_spawnp"))

;; Effect name for a stdlib/framework callee, or #f.
(define (stdlib-effect-of c)
  (define root (dotted-root c))
  (define nm (callee-name c))
  (define op (effect-op-of-callee c))
  (cond
    [(and nm (set-member? io-builtins (symbol->string nm))) 'io]
    [(hash-ref stdlib-root-effects root #f) => (λ (e) e)]
    [(and op (member-access-callee? c)
          (set-member? io-methods (symbol->string op))) 'io]
    [(and op (member-access-callee? c)
          (set-member? email-methods (symbol->string op))) 'email]
    [(and op (member-access-callee? c) (eq? op 'sleep)) 'time]
    [(and op (eq? root 'os)
          (set-member? os-process-methods (symbol->string op))) 'process]
    [(and op (eq? root 'os)
          (set-member? os-io-methods (symbol->string op))) 'io]
    [(and op (eq? root 'sys) (eq? op 'exit)) 'process]
    [else #f]))

;; ── pure stdlib roots ───────────────────────────────────────────────
;;
;; Common pure (side-effect-free) stdlib/framework dotted calls that would
;; otherwise fall to `unknown`: `json.dumps`, `re.compile`, `math.floor`,
;; `datetime.now`, `os.path.join`, ... Two tables: whole-module roots (every
;; callable in the module is pure) and a per-root method set for modules with a
;; mix of pure and effectful members.
;;
;; IMPORTANT: this check runs *after* `direct-effect-of-callee` in
;; `classify-call`, so the effect roots (llm/db/network/cache/io/process/time/
;; email) win over whole-module purity. A module is listed whole-module-pure
;; only when none of its members are effectful beyond what those roots catch;
;; `gzip`/`codecs`/`pathlib`/`json` are NOT whole-module-pure for that reason.
(define pure-stdlib-roots
  (set "math" "cmath" "itertools" "functools" "operator" "collections" "string"
       "textwrap" "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy"
       "decimal" "fractions" "ipaddress" "struct" "array" "bisect" "heapq"
       "hashlib" "hmac" "base64" "binascii" "unicodedata" "calendar" "keyword"
       "datetime"))

;; Modules with a mix of pure and effectful members: only the listed methods
;; are pure. `json.dump`/`json.load` (file I/O) are deliberately absent, as are
;; the filesystem/metadata `os.path.*` probes (`exists`/`isfile`/`getsize`/…),
;; which stay `unknown` rather than being falsely marked pure.
(define pure-stdlib-methods
  (hash 'json (set "dumps" "loads" "JSONEncoder" "JSONDecoder")
        're   (set "compile" "match" "search" "fullmatch" "findall" "finditer"
                   "split" "sub" "subn" "escape" "template")
        'os   (set "join" "dirname" "basename" "splitext" "split" "splitdrive"
                   "normpath" "normcase" "isabs" "commonprefix" "commonpath"
                   "relpath" "expanduser" "expandvars" "abspath")))

(define (pure-stdlib-callee? c)
  (and (member-access-callee? c)
       (let ([root (dotted-root c)]
             [op (effect-op-of-callee c)])
         (and root op
              (or (set-member? pure-stdlib-roots (symbol->string root))
                  (let ([ms (hash-ref pure-stdlib-methods root #f)])
                    (and ms (set-member? ms (symbol->string op)))))))))

;; "Class.method" name symbol for a class name and method name (both symbols).
(define (qualified-name-symbol class-name method-name)
  (string->symbol (format "~a.~a" class-name method-name)))

;; Is `base` a `super()` invocation? `super().__init__(...)` lowers to a
;; uir-get whose base is (uir-call (uir-var "super") ...). Such a call targets
;; the ENCLOSING class's parent chain (most-derived first, C3), not the class
;; itself — resolving it structurally (no name heuristic) turns the dominant
;; Django `super().__init__` pattern into a call-graph edge instead of
;; `unknown`.
(define (super-call? base)
  (and (uir-call? base)
       (eq? (callee-name (uir-call-callee base)) 'super)))

;; Class-qualified method-name symbols (Class.method) for a Python method call
;; `base.method`, when `base` is `self` / `cls` (the enclosing method's class,
;; expanded through its superclass chain) or a known class name (expanded through
;; its own chain). Returns the full ordered list of candidate names (most
;; derived first) so inherited methods resolve; the empty list means the call is
;; unresolvable (an arbitrary `obj.method`).
(define (method-callee-qualified-names c owner class-names supers)
  (cond
    [(not (uir-get? c)) '()]
    [else
     (let* ([mname (get-field-symbol (uir-get-field c))]
            [base (uir-get-base c)])
       (cond
         [(and mname (super-call? base))
          ;; super().m() → the enclosing class's parents, most-derived first.
          (if owner
              (map (λ (cls) (qualified-name-symbol cls mname))
                   (cdr (mro-chain owner supers)))
              '())]
         [(and mname (uir-var? base))
          (let ([base-name (string->symbol (uir-symbol-name (uir-var-name base)))])
            (cond
              [(memq base-name '(self cls))
               (if owner
                   (map (λ (cls) (qualified-name-symbol cls mname))
                        (mro-chain owner supers))
                   '())]
              [(hash-has-key? class-names base-name)
               (map (λ (cls) (qualified-name-symbol cls mname))
                    (mro-chain base-name supers))]
              [else '()]))]
         [else '()]))]))

;; Directly-performed effect for a call's callee, or #f.
(define (direct-effect-of-callee c)
  (define root (dotted-root c))
  (define nm (callee-name c))
  (cond
    [(and root (memq root '(llm openai anthropic cohere ollama groq agno langgraph langchain llamaindex))) 'llm]
    [(and root (memq root '(db database sql sqlalchemy sqlmodel asyncpg psycopg))) 'db]
    [(dotted-has-field? c 'objects) 'db]  ;; Django Model.objects
    [(and root (memq root '(fetch axios httpx requests aiohttp urllib))) 'network]
    [(and nm (memq nm '(fetch axios))) 'network]
    [(and root (memq root '(redis memcache cache))) 'cache]
    [(and nm (memq nm '(revalidatePath revalidate_path invalidate clearCache purgeCache))) 'cache]
    [(django-orm-callee? c) 'db]
    [(stdlib-effect-of c) => (λ (e) e)]
    [else #f]))

;; Classify a call callee against the known function names:
;;   (cons 'edge name)     — call to a function in the program
;;   (cons 'pure #f)       — front-end-encoded construct (operator/builtin)
;;   (cons 'effect name)   — directly-detected effect
;;   (cons 'unknown #f)    — unresolvable (higher-order / IIFE)
;;
;; `owner` is the enclosing class name symbol (or #f); `class-names` the set of
;; all class names in the program. A Python `self.m()` / `cls.m()` resolves to
;; the edge `Owner.m`, and `Other.m()` to `Other.m`, so intra-program method
;; calls are call-graph edges instead of `unknown`.
(define (classify-call callee known-names [owner #f] [class-names (hash)] [supers (hash)])
  (define nm (callee-name callee))
  (define qnms (method-callee-qualified-names callee owner class-names supers))
  (define qnm (for/or ([q (in-list qnms)] #:when (hash-has-key? known-names q)) q))
  (cond
    [(and qnm (hash-has-key? known-names qnm)) (cons 'edge qnm)]
    [(and nm (hash-has-key? known-names nm)) (cons 'edge nm)]
    [else (classify-nonedge callee)]))

;; Shared tail of `classify-call` and `classify-call-all`: after intra-program
;; edge resolution has failed, classify the callee by name heuristics (synthetic
;; constructs, pure builtins, exception constructors, runtime primitives, then
;; direct effect roots and pure stdlib).
(define (classify-nonedge callee)
  (cond
    [(synthetic-callee? callee) (cons 'pure #f)]
    [(pure-builtin-callee? callee) (cons 'pure #f)]
    [(gettext-callee? callee) (cons 'pure #f)]
    [(exception-constructor-callee? callee) (cons 'pure #f)]
    [(django-exception-callee? callee) (cons 'pure #f)]
    [(django-declarative-callee? callee) (cons 'pure #f)]
    [(runtime-primitive-callee? callee) (cons 'pure #f)]
    [else
     (define eff (direct-effect-of-callee callee))
     (cond
       [eff (cons 'effect eff)]
       [(pure-stdlib-callee? callee) (cons 'pure #f)]
       [(str-literal-method-callee? callee) (cons 'pure #f)]
       [(pure-container-method-callee? callee) (cons 'pure #f)]
       [(pure-config-method-callee? callee) (cons 'pure #f)]
       [else (cons 'unknown #f)])]))

;; Operator / statement / special-form names the front ends encode as literal
;; `(uir-call (uir-symbol NAME) ...)`. These are language constructs, not
;; runtime-dispatched functions, so they perform no effect and are not
;; `unknown`. (Real calls lower to a uir-var callee in Python/TS and are
;; checked against known-names before reaching here.)
(define synthetic-callees
  (set
   ;; JS/TS/JSX structural calls
   "dot" "index" "while" "dowhile" "with" "for" "forin" "forof" "switch"
   "break" "continue" "debugger" "label" "regex" "function" "async-fn"
   "gen-fn" "import" "export" "throw" "try" "=>" "var" "let" "const"
   "void" "typeof" "delete" "array-bind" "object-bind"
   ;; arithmetic / comparison / assignment / logical / unary
   "+" "-" "*" "/" "//" "%" "=" "==" "===" "!=" "!==" "<" ">" "<=" ">="
   "&&" "||" "**" "+=" "-=" "*=" "/=" "**=" "!" "~" "&" "|" "^"
   "<<" ">>" ">>>" "postfix++" "postfix--" "prefix++" "prefix--"
   ;; Python identity / membership / boolean operators (lower-comparison /
   ;; lower-binop emit these as uir-symbol callees)
   "is" "is not" "in" "not in" "and" "or"
   ;; Python structural calls and comprehensions
   "not" "tuple" "set" "list" "assert" "raise" "del" "global" "nonlocal"
   "list-comp" "dict-comp" "set-comp" "gen-comp" "kwarg"
   ;; Clojure special forms / data constructors
   "quote" "syntax-quote" "unquote" "unquote-splicing" "deref" "var"
   "vector" "hash-set" "fn*" "with-meta" "namespace" "host-expr"))

(define (synthetic-callee? c)
  (and (uir-symbol? c)
       (set-member? synthetic-callees (uir-symbol-name c))))

;; Runtime primitives the emitters inject (`__handle`). They are called as
;; ordinary (uir-var) callees in user source, perform no effect themselves,
;; and their handler/thunk arguments are separate walk boundaries.
(define (runtime-primitive-callee? c)
  (memq (callee-name c) '(__handle)))

;; Side-effect-free Python builtins. They lower to uir-var callees (unlike
;; the uir-symbol-encoded constructs in `synthetic-callees`), so without this
;; table a call like `len(x)` / `dict(a=1)` / `isinstance(x, T)` is classified
;; `unknown` and pollutes the inferred effect algebra. Effectful builtins
;; (open, input, print, eval, exec, compile, breakpoint, help) are deliberately
;; absent: they perform I/O or code execution and stay `unknown` (or, later,
;; map to a dedicated effect) rather than being falsely marked pure.
(define python-builtins
  (set
   "abs" "all" "any" "ascii" "bin" "bool" "bytearray" "bytes" "callable"
   "chr" "classmethod" "complex" "delattr" "dict" "dir" "divmod"
   "enumerate" "filter" "float" "format" "frozenset" "getattr" "globals"
   "hasattr" "hash" "hex" "id" "int" "isinstance" "issubclass" "iter"
   "len" "list" "locals" "map" "max" "memoryview" "min" "next" "object"
   "oct" "ord" "pow" "property" "range" "repr" "reversed" "round" "set"
   "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super" "tuple"
   "type" "vars" "zip" "__import__"))

(define (pure-builtin-callee? c)
  (define nm (callee-name c))
  (and nm (set-member? python-builtins (symbol->string nm))))

;; Exception constructors (`ValueError`, `TypeError`, `ValidationError`, ...)
;; build an exception object to raise and perform no effect themselves; the
;; `raise` statement that throws it is already covered by the synthetic "raise"
;; callee. Detected by a bare callee name / final dotted field ending in
;; "Error". Python exceptions are PascalCase with an "Error" suffix while
;; effectful callables are snake_case (`send_mail`), so the case-sensitive
;; suffix is a reliable discriminator.
(define (exception-constructor-callee? c)
  (define op (effect-op-of-callee c))
  (and op (string-suffix? (symbol->string op) "Error")))

;; Gettext translation lookups are pure string-mapping functions with no effect
;; (no I/O, no network). They are usually imported as a bare alias, most often
;; `_`, so the name must be a bare callee (a uir-var), not only a dotted field.
;; `gettext_noop` is also defined as a function in the Django tree and resolves
;; as a pure edge anyway; the alias `_` and the stdlib `gettext`/`ngettext`
;; imports do not, so this table is what keeps them from `unknown`.
(define gettext-names
  (set "_" "gettext" "gettext_lazy" "gettext_noop" "ngettext" "ngettext_lazy"
       "pgettext" "pgettext_lazy" "npgettext" "npgettext_lazy"
       "ugettext" "ugettext_lazy" "ungettext" "ungettext_lazy"))

(define (gettext-callee? c)
  (define op (effect-op-of-callee c))
  (and op (set-member? gettext-names (symbol->string op))))

;; Django / HTTP exception classes that do NOT end in "Error" (so the
;; case-sensitive "Error" suffix heuristic misses them). Constructing an
;; exception to raise performs no effect; the `raise` statement is the
;; effectful construct. Names are the common non-Error-suffixed Django
;; exception classes (`ImproperlyConfigured`, `Http404`, ...).
(define django-exception-names
  (set "ImproperlyConfigured" "Http404" "PermissionDenied" "SuspiciousOperation"
       "DisallowedHost" "DisallowedRedirect" "FieldDoesNotExist"
       "ObjectDoesNotExist" "MultipleObjectsReturned" "EmptyResultSet"
       "ViewDoesNotExist" "NoReverseMatch" "AppRegistryNotReady" "BadSignature"
       "PageNotAnInteger" "EmptyPage" "DoesNotExist" "SuspiciousMultipartForm"
       "SuspiciousFileOperation" "RequestAborted" "RequestDataTooBig"
       "TooManyFieldsSent" "Resolver404" "MiddlewareNotUsed"))

(define (django-exception-callee? c)
  (define op (effect-op-of-callee c))
  (and op (set-member? django-exception-names (symbol->string op))))

;; Django declarative constructors that are pure class instantiation, not
;; effectful calls: model fields (`models.CharField`, `ForeignKey`, ...) and
;; query-expression objects (`F`, `Q`, `Value`, `Case`, `When`, `Subquery`, ...).
;; Both the dotted (`models.CharField`) and bare (from `... import CharField`)
;; forms resolve through the final field name. A locally-defined class of the
;; same name is an intra-program edge and never reaches this tail.
(define django-declarative-names
  (set
   ;; model field constructors
   "AutoField" "BigAutoField" "SmallAutoField" "BigIntegerField" "BinaryField"
   "BooleanField" "CharField" "DateField" "DateTimeField" "DecimalField"
   "DurationField" "EmailField" "FileField" "FilePathField" "FloatField"
   "ForeignKey" "GenericIPAddressField" "IPAddressField" "ImageField"
   "IntegerField" "JSONField" "ManyToManyField" "ManyToManyRel" "ManyToOneRel"
   "NullBooleanField" "OneToOneField" "OneToOneRel" "PositiveBigIntegerField"
   "PositiveIntegerField" "PositiveSmallIntegerField" "SlugField"
   "SmallIntegerField" "TextField" "TimeField" "URLField" "UUIDField"
   ;; query expression / aggregate constructors
   "F" "Q" "Value" "Case" "When" "Subquery" "Exists" "OuterRef" "Func"
   "Coalesce" "Concat" "Count" "Sum" "Avg" "Min" "Max" "StdDev" "Variance"
   "ExpressionWrapper" "CombinedExpression" "DurationValue" "Greatest" "Least"))

(define (django-declarative-callee? c)
  (define op (effect-op-of-callee c))
  (and op (set-member? django-declarative-names (symbol->string op))))

;; Framework config/registration methods on arbitrary receiver objects that
;; mutate no external state of interest: `parser.add_argument(...)`,
;; `parser.set_defaults(...)`, `parser.parse_args(...)` (argparse) and the
;; xml.sax handler builders. These perform no llm/db/network/cache/io/process/
;; time/email effect and would otherwise pollute the algebra as `unknown`.
(define pure-config-method-names
  (set "add_argument" "add_subparsers" "set_defaults" "parse_args"
       "parse_known_args" "addQuickElement" "startElement" "endElement"
       "characters" "ignorableWhitespace"))

(define (pure-config-method-callee? c)
  (and (member-access-callee? c)
       (let ([op (effect-op-of-callee c)])
         (and op (set-member? pure-config-method-names (symbol->string op))))))

;; String-literal method calls (`"".join(x)`, `"{}".format(v)`) are pure str
;; operations. The receiver is a uir-string literal, so `dotted-root` is #f and
;; none of the var/symbol-rooted tables can see it; the literal base is what
;; disambiguates them from an arbitrary `obj.join(...)`.
(define str-method-names
  (set "join" "format" "format_map" "split" "rsplit" "splitlines" "strip"
       "lstrip" "rstrip" "lower" "upper" "capitalize" "title" "swapcase"
       "casefold" "replace" "translate" "zfill" "center" "ljust" "rjust"
       "expandtabs" "count" "find" "rfind" "index" "rindex" "startswith"
       "endswith" "isalnum" "isalpha" "isascii" "isdecimal" "isdigit"
       "isidentifier" "islower" "isnumeric" "isprintable" "isspace" "istitle"
       "isupper" "encode" "partition" "rpartition" "removeprefix" "removesuffix"
       "maketrans"))

(define (str-literal-method-callee? c)
  (and (uir-get? c)
       (uir-string? (uir-get-base c))
       (let ([op (get-field-symbol (uir-get-field c))])
         (and op (set-member? str-method-names (symbol->string op))))))

;; Pure collection-method calls on an arbitrary receiver (list/dict/set/deque
;; read or in-place mutation). In the effect-row algebra these perform no
;; effect: mutating a local collection is not llm/db/network/cache/io/process/
;; time/email. The name list DELIBERATELY omits every name that collides with a
;; Django QuerySet/RelatedManager operation (`get`, `update`, `add`, `remove`,
;; `clear`, `set`, `create`, `values`, `reverse`, `filter`, `exclude`,
;; `order_by`, `save`, `delete`, `count`, `exists`, `first`, `last`, `union`,
;; `intersection`, `difference`, `symmetric_difference`) — those reach
;; `direct-effect-of-callee` first and must classify `db`; anything still
;; carrying one of those names at this tail is an arbitrary object, not a
;; collection, and stays `unknown`.
(define pure-container-method-names
  (set "append" "extend" "insert" "pop" "popitem" "sort" "index" "keys"
       "items" "setdefault" "copy" "fromkeys" "discard"
       "issubset" "issuperset" "isdisjoint"))

(define (pure-container-method-callee? c)
  (and (member-access-callee? c)
       (let ([op (effect-op-of-callee c)])
         (and op (set-member? pure-container-method-names (symbol->string op))))))

;; ── JS/TS/JSX function encodings ───────────────────────────────────

;; The uir-fn wrapped by a JS/TS function value: either the fn itself, or
;; (call "async-fn"/"gen-fn" (list fn)). Returns #f otherwise.
(define (wrapped-fn val)
  (cond
    [(uir-fn? val) val]
    [(and (uir-call? val)
          (uir-symbol? (uir-call-callee val))
          (member (uir-symbol-name (uir-call-callee val)) '("async-fn" "gen-fn"))
          (= (length (uir-call-args val)) 1)
          (uir-fn? (first (uir-call-args val))))
     (first (uir-call-args val))]
    [else #f]))

;; (name . body) for a named JS/TS function definition:
;;   (set! NAME (fn #f ...))                       — function declaration
;;   (set! NAME (call "async-fn"/"gen-fn" (fn)))   — async / generator decl
;;   (set! NAME (call "=>" (list params body)))    — arrow bound to a name
;;   (set! NAME (call "function" (list params body))) — function expr bound
;; Returns #f for anything else.
(define (named-function n)
  (and (uir-set!? n)
       (uir-symbol? (uir-set!-name n))
       (let* ([nm (string->symbol (uir-symbol-name (uir-set!-name n)))]
              [val (uir-set!-value n)]
              [fn (wrapped-fn val)])
         (cond
           [fn (cons nm (uir-fn-body fn))]
           [(arrow-call? val) (cons nm (second (uir-call-args val)))]
           [(function-expr-call? val) (cons nm (second (uir-call-args val)))]
           [else #f]))))

;; ── rows ────────────────────────────────────────────────────────────

(define (empty-row)
  (uir-effect-row '()))

;; Canonicalize row entry order so equal? is order-insensitive.
(define (canonical-row r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

;; A list of effect-name symbols → a row. 'unknown names get modality
;; 'unknown; everything else is a detected 'may.
(define (effects->row syms)
  (canonical-row
   (uir-effect-row
    (for/list ([s (in-list (remove-duplicates syms))])
      (uir-effect-entry
       (uir-symbol (symbol->string s))
       (if (eq? s 'unknown) 'unknown 'may)
       '()
       #f
       #f)))))

;; Union that keeps the deferred 'unknown modality (contract's union
;; collapses unknown→may for same-name merges).
(define (infer-row-union r1 r2)
  (canonical-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (effect-row-union r1 r2)))])
      (if (eq? (effect-name-symbol e) 'unknown)
          (struct-copy uir-effect-entry e [modality 'unknown])
          e)))))

;; ── collection and fixpoint ─────────────────────────────────────────

;; name-symbol → function body, for every named function reachable from
;; `program`: named uir-fn nodes plus the JS/TS `set!`-bound encodings
;; (function declarations, async/generator declarations, and bound arrows).
(define (collect-functions program)
  (define h (make-hash))
  (define (visit! n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (hash-set! h (string->symbol (uir-symbol-name nm)) (uir-fn-body n))
             (void)))]
      [(named-function n)
       => (λ (p) (hash-set! h (car p) (cdr p)))]
      [else (void)]))
  (walk-uir program visit!)
  h)

;; Class name symbol (or #f) for a `uir-class` node.
(define (class-name-symbol n)
  (and (uir-class? n)
       (let ([nm (uir-class-name n)])
         (and (uir-symbol? nm)
              (string->symbol (uir-symbol-name nm))))))

;; Super class name symbol for a `uir-class` node, or #f. The Python lowerer
;; emits the first positional base as its UIR: a bare name (`BaseView`) lowers
;; to a uir-var, and a dotted name (`models.Model`) lowers to a uir-get chain
;; whose last field is the class name. A subscripted generic base (`Generic[T]`)
;; lowers to a non-symbol field and yields #f.
(define (class-super-name n)
  (and (uir-class? n)
       (let ([sup (uir-class-super n)])
         (cond
           [(uir-var? sup) (string->symbol (uir-symbol-name (uir-var-name sup)))]
           [(uir-get? sup) (get-field-symbol (uir-get-field sup))]
           [else #f]))))

;; Class-qualified name symbol → method body, for every method of every
;; `uir-class` node reachable from `program`. Methods are keyed as `Class.method`
;; so a `self.m()` inside that class (or `Other.m()` elsewhere) resolves to the
;; same call-graph node as a bare function.
(define (collect-methods program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname
        (for ([m (in-list (uir-class-methods n))])
          (when (uir-method? m)
            (define mname (and (uir-method-name m)
                               (uir-symbol? (uir-method-name m))
                               (string->symbol (uir-symbol-name (uir-method-name m)))))
            (when mname
              (hash-set! h (qualified-name-symbol cname mname) (uir-method-body m))))))))
  (walk-uir program visit!)
  h)

;; Set of all class name symbols reachable from `program`.
(define (collect-class-names program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (when cname (hash-set! h cname #t))))
  (walk-uir program visit!)
  h)

;; Class name symbol → (listof super-name-symbol), for every `uir-class` node
;; reachable from `program`. Each list preserves left-to-right superclass order
;; (Python's C3 MRO); classes with no superclass are absent.
(define (collect-supers program)
  (define h (make-hash))
  (define (visit! n)
    (when (uir-class? n)
      (define cname (class-name-symbol n))
      (define sup (class-super-name n))
      (when (and cname sup)
        (hash-set! h cname (list sup)))))
  (walk-uir program visit!)
  h)

;; Linearized superclass chain for `class-name` (most derived first, then each
;; super), computed as a depth-first walk over the superclass map with a cycle
;; guard. Includes `class-name` itself. Used to resolve inherited methods:
;; `self.m()` searches the chain for the first class declaring `m`.
(define (mro-chain class-name supers)
  (define seen (make-hash))
  (let loop ([cls class-name] [acc '()])
    (cond
      [(hash-has-key? seen cls) (reverse acc)]
      [else
       (hash-set! seen cls #t)
       (define sup (hash-ref supers cls #f))
       (if sup
           (loop (car sup) (cons cls acc))
           (reverse (cons cls acc)))])))

;; Class name symbol before the first dot in a `Class.method` node name, or #f
;; for a bare function name.
(define (class-name-of-name n)
  (define s (symbol->string n))
  (define owner-str (regexp-replace #rx"[.].*$" s ""))
  (and (not (string=? owner-str s)) (string->symbol owner-str)))

;; Enclosing class name symbol (or #f) for a call-graph node keyed `Class.method`.
(define (owner-of-name methods n)
  (and (hash-has-key? methods n) (class-name-of-name n)))

;; Compute effect rows: name-symbol → uir-effect-row, via a monotone fixpoint
;; over the call graph. Every callee named by an `edge` classification must be
;; present in `known` (the intra-program name table); a name missing from
;; `known` falls back to an empty row.
(define (compute-effect-rows fns methods class-names supers known)
  (define direct-rows (make-hash))  ;; name → uir-effect-row (direct only)
  (define edges (make-hash))        ;; name → (listof name-symbol)

  (define all-names (append (hash-keys fns) (hash-keys methods)))
  (for ([n (in-list all-names)])
    (define body (if (hash-has-key? fns n)
                     (hash-ref fns n)
                     (hash-ref methods n)))
    (define owner (owner-of-name methods n))
    (define effects '())
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
          (let ([cls (classify-call (uir-call-callee node) known owner class-names supers)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows n (effects->row effects))
    (hash-set! edges n (sort (remove-duplicates callees) symbol<?)))

  (define rows (make-hash))
  (for ([n (in-list all-names)])
    (hash-set! rows n (hash-ref direct-rows n)))

  (let loop ()
    (define changed? #f)
    (for ([n (in-list all-names)])
      (define current (hash-ref rows n))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges n)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows n) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin
            (set! changed? #t)
            (hash-set! rows n new-row))))
    (if changed? (loop) (void)))

  rows)

;; Compute the fixpoint effect rows for a single program: name-symbol →
;; uir-effect-row. Only calls into functions/methods defined in `program` (or a
;; program passed together with it) resolve as edges; anything else falls to
;; `unknown`.
(define (infer-effects program)
  (define fns (collect-functions program))
  (define methods (collect-methods program))
  (define class-names (collect-class-names program))
  (define supers (collect-supers program))
  (define known (make-hash))
  (for ([(n _) (in-hash fns)]) (hash-set! known n #t))
  (for ([(n _) (in-hash methods)]) (hash-set! known n #t))
  (compute-effect-rows fns methods class-names supers known))

;; ── whole-program (cross-file) inference ─────────────────────────────
;;
;; `infer-effects` is per-file: `self.m()` resolves only when the method is
;; declared on the class or a superclass *in the same file*. Django's base
;; classes (`Model`, `BaseCommand`, `ListView`, `View`, ...) live in a different
;; module from their subclasses, so a `self.save()` in one file could not reach
;; `Model.save` defined in another. Whole-program inference builds the call
;; graph, class-name table and superclass map from every file at once, so a
;; cross-file inheritance chain resolves to an edge instead of `unknown`.
;;
;; Node identity is `(component . name)`, where `component` is the caller's file
;; identifier. This keeps same-named classes in different files distinct
;; (`Command.handle` is defined in ~30 management-command modules and they are
;; NOT the same method), unlike a bare-name union which would merge them and
;; corrupt the algebra. A cross-file edge is only drawn when the callee is
;; resolvable to a single component: a superclass name is followed only if it is
;; defined in exactly one file (or the current file), and a bare function call
;; resolves to the local definition or a globally-unique one. Ambiguous names
;; fall through to the ordinary `unknown` classification — conservative and
;; correct, never a spurious edge.

;; Component defining a class named `class-name`, from the point of view of
;; `comp` (the file making the reference). A local definition wins; a
;; globally-unique definition resolves; anything else is ambiguous → #f.
(define (resolve-class-component class-name comp class-defs)
  (define defs (remove-duplicates (hash-ref class-defs class-name '())))
  (cond
    [(null? defs) #f]
    [(member comp defs) comp]
    [(= (length defs) 1) (car defs)]
    [else #f]))

;; Ordered list of `(component . Class.method)` keys a `self.m()`/`cls.m()`/
;; `KnownClass.m()` call could target, most-derived first, following the
;; superclass chain across files. Stops at a superclass whose name is ambiguous
;; or unknown (such calls stay `unknown` rather than resolving to the wrong
;; file's class).
(define (mro-method-keys comp class-name mname methods class-defs comp-supers)
  (define seen (make-hash))
  (define out '())
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (define key (cons c (qualified-name-symbol cls mname)))
      (when (hash-has-key? methods key) (set! out (cons key out)))
      (for ([s (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (resolve-class-component s c class-defs))
        (when c2 (loop c2 s)))))
  (reverse out))

;; `(component . name)` keys a method callee could resolve to (the whole-program
;; counterpart of `method-callee-qualified-names`).
(define (method-callee-keys callee comp owner methods class-defs comp-supers)
  (cond
    [(not (uir-get? callee)) '()]
    [else
     (let* ([mname (get-field-symbol (uir-get-field callee))]
            [base (uir-get-base callee)])
       (cond
         [(and mname (super-call? base))
          ;; super().m() → each parent of the enclosing class, resolved to the
          ;; file that defines it (most-derived first).
          (if owner
              (append-map
               (λ (s)
                 (let ([c2 (resolve-class-component s comp class-defs)])
                   (if c2 (mro-method-keys c2 s mname methods class-defs comp-supers) '())))
               (hash-ref comp-supers (cons comp owner) '()))
              '())]
         [(and mname (uir-var? base))
          (let ([base-name (string->symbol (uir-symbol-name (uir-var-name base)))])
            (cond
              [(memq base-name '(self cls))
               (or (and owner (mro-method-keys comp owner mname methods class-defs comp-supers)) '())]
              [else
               (define c (resolve-class-component base-name comp class-defs))
               (or (and c (mro-method-keys c base-name mname methods class-defs comp-supers)) '())]))]
         [else '()]))]))

;; `(component . name)` key for a bare function call `f(...)`: the local
;; definition wins, else a globally-unique definition, else #f (ambiguous).
(define (bare-function-key nm comp fn-defs known)
  (cond
    [(not nm) #f]
    [else
     (define local (cons comp nm))
     (define defs (remove-duplicates (hash-ref fn-defs nm '())))
     (cond
       [(hash-has-key? known local) local]
       [(and (= (length defs) 1) (pair? defs))
        (define k (cons (car defs) nm))
        (and (hash-has-key? known k) k)]
       [else #f])]))

;; Whole-program counterpart of `classify-call`: edge resolution uses
;; component-qualified keys and the cross-file class/superclass tables; the
;; name-heuristic tail is shared via `classify-nonedge`.
(define (classify-call-all callee known methods class-defs fn-defs comp-supers comp owner)
  (define nm (callee-name callee))
  (define keys (method-callee-keys callee comp owner methods class-defs comp-supers))
  (define key (for/or ([k (in-list keys)] #:when (hash-has-key? known k)) k))
  (cond
    [(and key (hash-has-key? known key)) (cons 'edge key)]
    [(bare-function-key nm comp fn-defs known) => (λ (k) (cons 'edge k))]
    [else (classify-nonedge callee)]))

(define (key<? a b)
  (string<? (format "~a.~a" (car a) (cdr a)) (format "~a.~a" (car b) (cdr b))))

;; Whole-program effect rows: (component . name) → uir-effect-row, computed by
;; a single fixpoint over the union of every program's call graph. Takes a list
;; of `(component . program)` pairs.
(define (infer-effects-all comp-programs)
  (define fns (make-hash))           ;; (cons comp name) → body
  (define methods (make-hash))       ;; (cons comp name) → body
  (define comp-supers (make-hash))   ;; (cons comp class) → (listof super-symbol)
  (define class-defs (make-hash))    ;; class-name → (listof comp)
  (define fn-defs (make-hash))       ;; fn-name → (listof comp)
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define program (cdr cp))
    (for ([(n body) (in-hash (collect-functions program))])
      (hash-set! fns (cons comp n) body)
      (hash-update! fn-defs n (λ (l) (cons comp l)) '()))
    (for ([(n body) (in-hash (collect-methods program))])
      (hash-set! methods (cons comp n) body))
    (for ([(n _) (in-hash (collect-class-names program))])
      (hash-update! class-defs n (λ (l) (cons comp l)) '()))
    (for ([(n chain) (in-hash (collect-supers program))])
      (hash-set! comp-supers (cons comp n) chain)))

  (define known (make-hash))
  (for ([(k _) (in-hash fns)]) (hash-set! known k #t))
  (for ([(k _) (in-hash methods)]) (hash-set! known k #t))

  (define all-keys (append (hash-keys fns) (hash-keys methods)))
  (define direct-rows (make-hash))
  (define edges (make-hash))
  (for ([k (in-list all-keys)])
    (define body (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
    (define comp (car k))
    (define owner (and (hash-has-key? methods k) (class-name-of-name (cdr k))))
    (define effects '())
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
          (let ([cls (classify-call-all (uir-call-callee node) known methods
                                        class-defs fn-defs comp-supers comp owner)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows k (effects->row effects))
    (hash-set! edges k (sort (remove-duplicates callees) key<?)))

  (define rows (make-hash))
  (for ([k (in-list all-keys)]) (hash-set! rows k (hash-ref direct-rows k)))
  (let loop ()
    (define changed? #f)
    (for ([k (in-list all-keys)])
      (define current (hash-ref rows k))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges k)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows k) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin (set! changed? #t) (hash-set! rows k new-row))))
    (if changed? (loop) (void)))
  rows)

;; ── write-back ──────────────────────────────────────────────────────

;; Rewrite a program so every named function carries its inferred effect row
;; in the `effects` field: named uir-fn nodes, plus the JS/TS `set!`-bound
;; function declarations (writing back into the wrapped uir-fn).
(define (annotate-effects program)
  (define rows (infer-effects program))
  ;; Materialize an inferred row for emission: 'unknown is a checker signal
  ;; ("could not verify"), not a runtime effect, and the runtime guards reject
  ;; the 'unknown modality. So it is dropped here; an all-unknown row becomes
  ;; empty and the function stays unannotated.
  (define (concrete-row r)
    (uir-effect-row
     (filter (λ (e) (not (eq? (effect-name-symbol e) 'unknown)))
             (uir-effect-row-entries r))))
  ;; The concrete (runtime-guardable) inferred row for a named function, or #f
  ;; when there is nothing to emit (no inference, an empty row, or an
  ;; all-'unknown row).
  (define (inferred-row nm)
    (define inferred (hash-ref rows nm #f))
    (and inferred
         (pair? (uir-effect-row-entries inferred))
         (let ([c (concrete-row inferred)])
           (and (pair? (uir-effect-row-entries c)) c))))
  (define (with-effects fn nm)
    (define existing (uir-fn-effects fn))
    (cond
      ;; A hand-written row wins: it carries budgets/scopes inference cannot
      ;; recover, so re-inference must not overwrite it.
      [(uir-effect-row? existing) fn]
      [(inferred-row nm) => (λ (c) (struct-copy uir-fn fn [effects c]))]
      [else fn]))
  ;; Arrow and function expressions have no effects slot. A guardable row is
  ;; represented by wrapping the value in the __withEffects(row, fn) call the
  ;; JS emitter renders.
  (define (with-effects-wrap val nm)
    (define c (inferred-row nm))
    (if c
        (uir-call (uir-symbol "__withEffects") (list c val))
        val))
  ;; A hand-written `withEffects(row)(fn)` annotation declares its row
  ;; explicitly. Rewrite it into the internal __withEffects(row, fn) call the
  ;; JS emitter renders, so it is guarded like any other effect row and
  ;; program-has-effects? sees the embedded uir-effect-row.
  (define (rewrite-with-effects val)
    (define row (extract-with-effects-row val))
    (define fn (extract-with-effects-fn val))
    (if row
        (uir-call (uir-symbol "__withEffects") (list row fn))
        val))
  (define (rewrite-value val nm)
    (cond
      [(with-effects-call? val) (rewrite-with-effects val)]
      [(uir-fn? val) (with-effects val nm)]
      [(and (uir-call? val)
            (uir-symbol? (uir-call-callee val))
            (member (uir-symbol-name (uir-call-callee val)) '("async-fn" "gen-fn"))
            (= (length (uir-call-args val)) 1)
            (uir-fn? (first (uir-call-args val))))
       (struct-copy uir-call val
                    [args (list (with-effects (first (uir-call-args val)) nm))])]
      [(or (arrow-call? val) (function-expr-call? val))
       (with-effects-wrap val nm)]
      [else val]))
  (define (rewrite n)
    (cond
      [(uir-fn? n)
       (let ([nm (uir-fn-name n)])
         (if (and nm (uir-symbol? nm))
             (with-effects n (string->symbol (uir-symbol-name nm)))
             n))]
      [(and (uir-set!? n) (uir-symbol? (uir-set!-name n)))
       (struct-copy uir-set! n
                    [value (rewrite-value (uir-set!-value n)
                                          (string->symbol (uir-symbol-name (uir-set!-name n))))])]
      ;; const/let/var declarations wrap their binding one level deep:
      ;; (call "const"/"let"/"var" (list (set! NAME <fn/arrow>))). Descend to
      ;; reach the named binding so its inferred row is written back.
      [(and (uir-call? n) (uir-symbol? (uir-call-callee n))
            (member (uir-symbol-name (uir-call-callee n)) '("var" "let" "const"))
            (= (length (uir-call-args n)) 1))
       (struct-copy uir-call n
                    [args (list (rewrite (first (uir-call-args n))))])]
       [(with-effects-call? n) (rewrite-with-effects n)]
       ;; A bare async/generator function statement ((call "async-fn" (list fn)))
       ;; is a named function (the Python `async def` encoding); write its
       ;; inferred row back into the wrapped uir-fn.
       [(wrapped-fn n)
        => (λ (fn)
             (define nm (and (uir-fn-name fn) (uir-symbol? (uir-fn-name fn))
                             (string->symbol (uir-symbol-name (uir-fn-name fn)))))
             (if nm
                 (struct-copy uir-call n [args (list (with-effects fn nm))])
                 n))]
       [else n]))
  (cond
    [(list? program) (map rewrite program)]
    [(uir-block? program)
     (struct-copy uir-block program
                  [stmts (map rewrite (uir-block-stmts program))])]
    [(uir-module? program)
     (struct-copy uir-module program
                  [body (map rewrite (uir-module-body program))])]
    [else (rewrite program)]))

(module+ main
  (displayln "racklr/effects/infer — effect inference loaded."))
