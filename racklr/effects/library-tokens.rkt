#lang racket

;; racklr/effects/library-tokens.rkt
;;
;; The effect-token lexicon: the concrete call-chain tokens (module roots,
;; method names, dotted fields) that map to the meta effect categories
;; (llm / db / network / cache / io / process / time / email). This is DATA,
;; kept apart from the classification engine so racklr/effervesce carry no
;; library-specific tokens in code. Swap the whole lexicon by injecting a
;; different `effect-tokens` struct.
;;
;; Each field is a name table consulted by `direct-effect-of-callee` /
;; `pure-stdlib-callee?` in effect-roots.rkt and by the pure-name predicates in
;; pure-roots.rkt. The default instance covers the stdlib and the frameworks
;; racklr is exercised against (Django et al.).

(provide
  effect-tokens? effect-tokens
  effect-tokens-llm-roots effect-tokens-db-roots
  effect-tokens-network-roots effect-tokens-network-names
  effect-tokens-cache-roots effect-tokens-cache-names
  effect-tokens-orm-methods
  effect-tokens-builtin-type-roots effect-tokens-stdlib-module-roots
  effect-tokens-io-builtins effect-tokens-stdlib-root-effects
  effect-tokens-io-methods effect-tokens-email-methods
  effect-tokens-time-methods effect-tokens-root-method-effects
  effect-tokens-pure-stdlib-roots effect-tokens-pure-stdlib-methods
  effect-tokens-framework-exception-names effect-tokens-declarative-names
  effect-tokens-pure-stdlib-bare-names effect-tokens-gettext-names
  effect-tokens-pure-config-method-names
  effect-tokens-python-builtins effect-tokens-pure-dunder-names
  effect-tokens-str-method-names effect-tokens-pure-container-method-names
  default-effect-tokens)

;; The lexicon of tokens the engine classifies. All fields are name sets (or a
;; symbol→effect hash for root-qualified stdlib effects / per-root method sets).
(struct effect-tokens
  (llm-roots db-roots network-roots network-names cache-roots cache-names
   orm-methods
   builtin-type-roots stdlib-module-roots
   io-builtins stdlib-root-effects io-methods email-methods
   time-methods root-method-effects
   pure-stdlib-roots pure-stdlib-methods
   framework-exception-names declarative-names
   pure-stdlib-bare-names gettext-names pure-config-method-names
   python-builtins pure-dunder-names str-method-names pure-container-method-names)
  #:transparent)

(define default-effect-tokens
  (effect-tokens
   ;; library roots whose every call is an llm effect
   (set "llm" "openai" "anthropic" "cohere" "ollama" "groq" "agno"
        "langgraph" "langchain" "llamaindex")
   ;; library roots whose every call is a db effect
   (set "db" "database" "sql" "sqlalchemy" "sqlmodel" "asyncpg" "psycopg")
   ;; library roots / bare names whose every call is a network effect
   (set "fetch" "axios" "httpx" "requests" "aiohttp" "urllib")
   (set "fetch" "axios")
   ;; library roots / bare names whose every call is a cache effect
   (set "redis" "memcache" "cache")
   (set "revalidatePath" "revalidate_path" "invalidate" "clearCache" "purgeCache")
   ;; ORM method names: a dotted call whose final field is one of these is a
   ;; QuerySet / Manager / Model DB operation even when the receiver is an
   ;; arbitrary object (`self.queryset.filter`, `obj.save`, `qs.values_list`)
   ;; and therefore cannot be resolved as an intra-program method edge. This is
   ;; a name-based heuristic, so it runs last in `direct-effect-of-callee`.
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
    ;; SQL cursor / connection / router methods
    "execute" "executemany" "fetchone" "fetchall" "fetchmany" "commit"
    "rollback" "cursor" "create_function" "create_aggregate"
    "create_deterministic_function" "db_for_read" "db_for_write")
   ;; builtin type roots whose method namespace collides with the ORM heuristic
   (set "dict" "list" "set" "str" "int" "float" "complex" "bool" "bytes"
        "bytearray" "tuple" "frozenset" "type" "object" "range" "slice"
        "memoryview")
   ;; stdlib module roots whose attribute namespace collides with the ORM
   ;; heuristic (`os.path.exists`, `pathlib.Path.exists`)
   (set "os" "sys" "io" "time" "re" "json" "pathlib" "logging" "email"
        "warnings" "traceback" "inspect" "random" "uuid" "importlib" "pickle"
        "shelve" "sqlite3" "csv" "glob" "fnmatch" "tarfile" "zipfile" "gzip"
        "bz2" "lzma" "codecs" "subprocess" "multiprocessing" "shutil" "tempfile"
        "fileinput" "smtplib" "http" "socket" "urllib3" "math" "cmath"
        "itertools" "functools" "operator" "collections" "string" "textwrap"
        "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy" "decimal"
        "fractions" "ipaddress" "struct" "array" "bisect" "heapq" "hashlib" "hmac"
        "base64" "binascii" "unicodedata" "calendar" "keyword" "datetime")
   ;; bare builtins that perform console/file I/O
   (set "open" "print" "input")
   ;; module roots whose every call is an effect (unambiguous)
   (hash 'subprocess 'process
         'multiprocessing 'process
         'shutil 'io
         'tempfile 'io
         'fileinput 'io
         'smtplib 'email
         'http 'network
         'socket 'network
         'urllib3 'network)
   ;; dotted methods that perform file/stream I/O regardless of receiver
   (set "open" "read" "readline" "readlines" "read1" "readinto" "readinto1"
        "write" "writelines" "flush" "seek" "tell" "truncate" "fileno")
   ;; dotted methods that send email
   (set "send_mail" "send_mass_mail" "mail_admins" "mail_managers")
   ;; dotted methods that perform a time effect regardless of receiver
   (set "sleep")
   ;; root-qualified method effects: `root.method` → effect, where the effect
   ;; depends on the method, not only the root (`os.remove` → io, `os.system` →
   ;; process, `sys.exit` → process, `warnings.warn` → io).
   (hash
    'os (hash
         ;; `os.*` filesystem operations
         "remove" 'io "unlink" 'io "rename" 'io "renames" 'io "replace" 'io
         "mkdir" 'io "makedirs" 'io "rmdir" 'io "removedirs" 'io "chdir" 'io
         "chmod" 'io "chown" 'io "utime" 'io "link" 'io "symlink" 'io
         "readlink" 'io "listdir" 'io "scandir" 'io "stat" 'io "lstat" 'io
         "fsync" 'io "fdatasync" 'io
         ;; `os.*` process-spawning / process-control operations
         "system" 'process "popen" 'process "popen2" 'process "popen3" 'process
         "popen4" 'process "spawnl" 'process "spawnle" 'process "spawnlp" 'process
         "spawnlpe" 'process "spawnv" 'process "spawnve" 'process "spawnvp" 'process
         "spawnvpe" 'process "execl" 'process "execle" 'process "execlp" 'process
         "execlpe" 'process "execv" 'process "execve" 'process "execvp" 'process
         "execvpe" 'process "fork" 'process "forkpty" 'process "kill" 'process
         "killpg" 'process "startfile" 'process "posix_spawn" 'process
         "posix_spawnp" 'process)
    'sys (hash "exit" 'process)
    'warnings (hash "warn" 'io))
   ;; whole-module pure roots
   (set "math" "cmath" "itertools" "functools" "operator" "collections" "string"
        "textwrap" "statistics" "types" "typing" "dataclasses" "enum" "abc" "copy"
        "decimal" "fractions" "ipaddress" "struct" "array" "bisect" "heapq"
        "hashlib" "hmac" "base64" "binascii" "unicodedata" "calendar" "keyword"
        "datetime" "chain")
   ;; modules with a mix of pure and effectful members: only the listed methods
   ;; are pure
   (hash 'json (set "dumps" "loads" "JSONEncoder" "JSONDecoder")
         're   (set "compile" "match" "search" "fullmatch" "findall" "finditer"
                    "split" "sub" "subn" "escape" "template")
         'os   (set "join" "dirname" "basename" "splitext" "split" "splitdrive"
                    "normpath" "normcase" "isabs" "commonprefix" "commonpath"
                    "relpath" "expanduser" "expandvars" "abspath"))
   ;; framework exception classes that don't end in "Error"/"Exception"/
   ;; "Warning" (so the suffix heuristic misses them); constructing one to
   ;; raise performs no effect
   (set "ImproperlyConfigured" "Http404" "PermissionDenied" "SuspiciousOperation"
        "DisallowedHost" "DisallowedRedirect" "FieldDoesNotExist"
        "ObjectDoesNotExist" "MultipleObjectsReturned" "EmptyResultSet"
        "ViewDoesNotExist" "NoReverseMatch" "AppRegistryNotReady" "BadSignature"
        "PageNotAnInteger" "EmptyPage" "DoesNotExist" "SuspiciousMultipartForm"
        "SuspiciousFileOperation" "RequestAborted" "RequestDataTooBig"
        "TooManyFieldsSent" "Resolver404" "MiddlewareNotUsed")
   ;; declarative framework constructors that are pure class instantiation
   ;; (model fields and query-expression objects)
   (set
    "AutoField" "BigAutoField" "SmallAutoField" "BigIntegerField" "BinaryField"
    "BooleanField" "CharField" "DateField" "DateTimeField" "DecimalField"
    "DurationField" "EmailField" "FileField" "FilePathField" "FloatField"
    "ForeignKey" "GenericIPAddressField" "IPAddressField" "ImageField"
    "IntegerField" "JSONField" "ManyToManyField" "ManyToManyRel" "ManyToOneRel"
    "NullBooleanField" "OneToOneField" "OneToOneRel" "PositiveBigIntegerField"
    "PositiveIntegerField" "PositiveSmallIntegerField" "SlugField"
    "SmallIntegerField" "TextField" "TimeField" "URLField" "UUIDField"
    "F" "Q" "Value" "Case" "When" "Subquery" "Exists" "OuterRef" "Func"
    "Coalesce" "Concat" "Count" "Sum" "Avg" "Min" "Max" "StdDev" "Variance"
    "ExpressionWrapper" "CombinedExpression" "DurationValue" "Greatest" "Least")
   ;; bare stdlib/ctypes/functools value constructors and decorators imported
   ;; directly (`from functools import partial`) that lower to a bare callee
   (set "partial" "wraps" "lru_cache" "cached_property" "byref" "POINTER")
   ;; gettext translation-lookup aliases (pure string mapping, no effect)
   (set "_" "gettext" "gettext_lazy" "gettext_noop" "ngettext" "ngettext_lazy"
        "pgettext" "pgettext_lazy" "npgettext" "npgettext_lazy"
        "ugettext" "ugettext_lazy" "ungettext" "ungettext_lazy")
   ;; framework config/registration methods (argparse / xml.sax) that mutate no
   ;; external state of interest
   (set "add_argument" "add_subparsers" "set_defaults" "parse_args"
        "parse_known_args" "addQuickElement" "startElement" "endElement"
        "characters" "ignorableWhitespace")
   ;; ── pure name tables (relocated from pure-roots.rkt) ─────────────
   ;; Side-effect-free Python builtins (lower to uir-var callees). Effectful
   ;; builtins (open/input/print/eval/exec/compile/breakpoint/help) are
   ;; deliberately absent so they stay `unknown` rather than being marked pure.
   (set "abs" "all" "any" "ascii" "bin" "bool" "bytearray" "bytes" "callable"
        "chr" "classmethod" "complex" "delattr" "dict" "dir" "divmod"
        "enumerate" "filter" "float" "format" "frozenset" "getattr" "globals"
        "hasattr" "hash" "hex" "id" "int" "isinstance" "issubclass" "iter"
        "len" "list" "locals" "map" "max" "memoryview" "min" "next" "object"
        "oct" "ord" "pow" "property" "range" "repr" "reversed" "round" "set"
        "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super" "tuple"
        "type" "vars" "zip" "__import__")
   ;; Protocol ("dunder") method names on an arbitrary receiver. `__call__`,
   ;; `__enter__`, `__exit__`, and `__getattr__` are deliberately absent: they
   ;; dispatch to arbitrary (possibly effectful) code and stay `unknown`.
   (set "__init__" "__new__" "__repr__" "__str__" "__format__"
        "__eq__" "__ne__" "__lt__" "__le__" "__gt__" "__ge__" "__hash__"
        "__bool__" "__len__" "__contains__" "__iter__" "__next__"
        "__getitem__" "__setitem__" "__delitem__" "__copy__" "__deepcopy__")
   ;; String-literal method names (`"".join(x)`); the uir-string base is what
   ;; disambiguates them from an arbitrary `obj.join(...)`.
   (set "join" "format" "format_map" "split" "rsplit" "splitlines" "strip"
        "lstrip" "rstrip" "lower" "upper" "capitalize" "title" "swapcase"
        "casefold" "replace" "translate" "zfill" "center" "ljust" "rjust"
        "expandtabs" "count" "find" "rfind" "index" "rindex" "startswith"
        "endswith" "isalnum" "isalpha" "isascii" "isdecimal" "isdigit"
        "isidentifier" "islower" "isnumeric" "isprintable" "isspace" "istitle"
        "isupper" "encode" "partition" "rpartition" "removeprefix" "removesuffix"
        "maketrans")
   ;; Pure collection-method names on an arbitrary receiver. Deliberately omits
   ;; every name that collides with a Django QuerySet/RelatedManager operation
   ;; (`get`, `update`, `filter`, ...): those reach `direct-effect-of-callee`
   ;; first and must classify `db`, so anything still carrying one here is an
   ;; arbitrary object and stays `unknown`.
   (set "append" "extend" "insert" "pop" "popitem" "sort" "index" "keys"
        "items" "setdefault" "copy" "fromkeys" "discard"
        "issubset" "issuperset" "isdisjoint")))
