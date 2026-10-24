#lang racket

;; racklr/effects/pure-roots.rkt
;;
;; Name tables that classify a callee as PURE (no domain effect): front-end
;; constructs, side-effect-free builtins, protocol dunders, exception
;; constructors, gettext lookups, declarative framework constructors, config
;; methods, and string / collection methods. Each predicate is consulted by
;; `classify-nonedge` before a callee falls through to `unknown`.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens)

(provide
  synthetic-callee?
  runtime-primitive-callee?
  pure-builtin-callee?
  pure-stdlib-bare-callee?
  pure-dunder-callee?
  exception-constructor-callee?
  gettext-callee?
  framework-exception-callee?
  declarative-callee?
  pure-config-method-callee?
  str-literal-method-callee?
  pure-container-method-callee?)

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

;; Directly-imported stdlib/ctypes/functools value constructors and decorators
;; that lower to a BARE callee (`from functools import partial`) and therefore
;; skip `pure-stdlib-callee?` (which only inspects member accesses). All build
;; or wrap values with no effect; a call to a C function pointer built by these
;; has a different callee and is classified separately. Names come from the
;; injected token lexicon.
(define (pure-stdlib-bare-callee? c [tokens default-effect-tokens])
  (define nm (callee-name c))
  (and nm (set-member? (effect-tokens-pure-stdlib-bare-names tokens)
                       (symbol->string nm))))

;; Protocol ("dunder") methods on an arbitrary receiver: `x.__init__()`,
;; `x.__repr__()`, `x.__eq__()`, ... implement the object protocol. They
;; initialize / compare / format / index the instance and perform no domain
;; effect (llm/db/network/cache/io/process/time/email) — mutating the
;; instance's own attributes is a local effect, not a domain effect. A dunder
;; call that resolves as an intra-program edge (`self.__str__()`) never reaches
;; this tail; only unresolvable arbitrary-receiver dunders do. `__call__`,
;; `__enter__`, `__exit__`, and `__getattr__` are deliberately absent: they can
;; dispatch to arbitrary (possibly effectful) code and stay `unknown`.
(define pure-dunder-names
  (set "__init__" "__new__" "__repr__" "__str__" "__format__"
       "__eq__" "__ne__" "__lt__" "__le__" "__gt__" "__ge__" "__hash__"
       "__bool__" "__len__" "__contains__" "__iter__" "__next__"
       "__getitem__" "__setitem__" "__delitem__" "__copy__" "__deepcopy__"))

(define (pure-dunder-callee? c)
  (and (member-access-callee? c)
       (let* ([base (cond [(uir-get? c) (uir-get-base c)]
                          [(dot-call? c) (first (uir-call-args c))]
                          [else #f])]
              [op (effect-op-of-callee c)])
         (and op
              ;; `super().__init__()` targets a specific parent method that may
              ;; do real work (and resolves as a whole-program edge); it is not
              ;; an arbitrary receiver's protocol call, so it stays a real edge
              ;; or `unknown` rather than being swept pure.
              (not (super-call? base))
              (set-member? pure-dunder-names (symbol->string op))))))

;; Exception constructors (`ValueError`, `TypeError`, `ValidationError`, ...)
;; build an exception object to raise and perform no effect themselves; the
;; `raise` statement that throws it is already covered by the synthetic "raise"
;; callee. Detected by a bare callee name / final dotted field ending in
;; "Error". Python exceptions are PascalCase with an "Error" suffix while
;; effectful callables are snake_case (`send_mail`), so the case-sensitive
;; suffix is a reliable discriminator.
(define (exception-constructor-callee? c)
  (define op (effect-op-of-callee c))
  (and op
       (or (string-suffix? (symbol->string op) "Error")
           (string-suffix? (symbol->string op) "Exception")
           (string-suffix? (symbol->string op) "Warning"))))

;; Gettext translation lookups are pure string-mapping functions with no effect
;; (no I/O, no network). They are usually imported as a bare alias, most often
;; `_`, so the name must be a bare callee (a uir-var), not only a dotted field.
;; `gettext_noop` is also defined as a function in the Django tree and resolves
;; as a pure edge anyway; the alias `_` and the stdlib `gettext`/`ngettext`
;; imports do not, so this table is what keeps them from `unknown`. Names come
;; from the injected token lexicon.
(define (gettext-callee? c [tokens default-effect-tokens])
  (define op (effect-op-of-callee c))
  (and op (set-member? (effect-tokens-gettext-names tokens) (symbol->string op))))

;; Framework / HTTP exception classes that do NOT end in "Error" (so the
;; case-sensitive "Error" suffix heuristic misses them). Constructing an
;; exception to raise performs no effect; the `raise` statement is the
;; effectful construct. Names come from the injected token lexicon.
(define (framework-exception-callee? c [tokens default-effect-tokens])
  (define op (effect-op-of-callee c))
  (and op (set-member? (effect-tokens-framework-exception-names tokens)
                       (symbol->string op))))

;; Declarative framework constructors that are pure class instantiation, not
;; effectful calls: model fields (`models.CharField`, `ForeignKey`, ...) and
;; query-expression objects (`F`, `Q`, `Value`, `Case`, `When`, `Subquery`, ...).
;; Both the dotted (`models.CharField`) and bare (from `... import CharField`)
;; forms resolve through the final field name. A locally-defined class of the
;; same name is an intra-program edge and never reaches this tail. Names come
;; from the injected token lexicon.
(define (declarative-callee? c [tokens default-effect-tokens])
  (define op (effect-op-of-callee c))
  (and op (set-member? (effect-tokens-declarative-names tokens)
                       (symbol->string op))))

;; Framework config/registration methods on arbitrary receiver objects that
;; mutate no external state of interest: `parser.add_argument(...)`,
;; `parser.set_defaults(...)`, `parser.parse_args(...)` (argparse) and the
;; xml.sax handler builders. These perform no llm/db/network/cache/io/process/
;; time/email effect and would otherwise pollute the algebra as `unknown`.
;; Names come from the injected token lexicon.
(define (pure-config-method-callee? c [tokens default-effect-tokens])
  (and (member-access-callee? c)
       (let ([op (effect-op-of-callee c)])
         (and op (set-member? (effect-tokens-pure-config-method-names tokens)
                              (symbol->string op))))))

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
