#lang racket

;; racklr/effects/imports/effect-tag.rkt
;;
;; Generic module-path → effect-domain tagging: a class inherits its module's
;; domain from the config rules, and its method's primitive ops are derived
;; from the method name via a per-domain verb table. The capability model
;; structs live in effect-tag-types.rkt; the library-specific rules live in
;; library-config.rkt.

(require racklr/effects/imports/import-extract
         racklr/effects/imports/handle
         racklr/effects/imports/effect-tag-types
         racklr/effects/imports/library-config)

(provide
  (all-from-out racklr/effects/imports/effect-tag-types)
  effect-tag-config
  resolve-effect-tag
  module-effect-tag
  clear-effect-tag-cache!
  class-effect-tag
  method-ops
  typed-op-parts
  derive-class-method-effects
  derive-class-supers
  builtin-class-methods)

;; An effect is a structured capability, not a bare category atom. It is a
;; pair (domain . ops): `domain` is the effect root the effect-row algebra
;; keys on (db/io/network/cache/email/process/llm/time), and `ops` is the set
;; of primitive operations the capability covers (() = "all ops" = the coarse
;; category). A method's capability is derived from WHERE its class lives
;; (domain, from the config rules) and WHAT the method does (ops, from the
;; method name via a per-domain verb table). `pure` is not an effect — it is
;; the absence of one (the empty effect row), so pure methods get no entry.
;;
;; The lookup is cached per library (nested hash: library-name → module-path →
;; domain), so after the first resolution of a module path the rest are O(1).

;; The capability structs and the library-specific config live in sibling
;; modules (effect-tag-types.rkt / library-config.rkt); both are re-exported
;; above so the public contract is unchanged.

;; Per-library lookup cache: library-name → (hash module-path → domain-or-#f).
(define effect-tag-cache (make-hash))

(define (clear-effect-tag-cache!)
  (set! effect-tag-cache (make-hash)))

;; Generic, uncached: the first matching rule in `config` for `path`, else #f.
(define (resolve-effect-tag config path)
  (define lib-name (first-component path))
  (for/or ([l (in-list config)]
           #:when (string=? (effect-library-name l) lib-name))
    (for/or ([r (in-list (effect-library-rules l))])
      (and (string-prefix? path (effect-rule-prefix r))
           (effect-rule-domain r)))))

;; Cached facade over a config (the default unless one is supplied). A non-default
;; config bypasses the cache, since the cache is keyed by module path alone.
(define (module-effect-tag path [config effect-tag-config])
  (if (equal? config effect-tag-config)
      (let* ([lib-name (first-component path)]
             [lib-cache (hash-ref! effect-tag-cache lib-name make-hash)])
        (hash-ref! lib-cache path (λ () (resolve-effect-tag config path))))
      (resolve-effect-tag config path)))

(define (class-effect-tag path cname [config effect-tag-config])
  ;; Seam for per-class overrides; the default is the module's domain.
  (module-effect-tag path config))

;; ── primitive-operation vocabulary (per domain) ─────────────────────
;;
;; db: the operation is the SQL access kind.
;;   db.read  = SELECT (get/filter/count/exists/fetch*/...)
;;   db.write = DML/DDL (update/delete/create/save/execute/commit/...)
;; io: the operation names a RESOURCE (stdio/file/socket/http) and a verb
;;   (read/write/close/...), e.g. file.read, stdio.write, http.write. Resource
;;   is derived from the class's module path; verb from the method name.
;; Methods whose verb is not classified keep () — sound: the domain is known,
;; the specific operation is not.

(define db-read-methods
  (set "get" "filter" "all" "count" "exists" "first" "last" "iterator"
       "values" "values_list" "dates" "datetimes" "in_bulk" "distinct"
       "order_by" "defer" "only" "select_related" "prefetch_related" "using"
       "alias" "annotate" "aggregate" "fetchone" "fetchall" "fetchmany"
       "latest" "earliest" "none" "explain" "raw" "resolve_expression"))

(define db-write-methods
  (set "create" "update" "delete" "save" "bulk_create" "bulk_update"
       "get_or_create" "update_or_create" "execute" "executemany" "commit"
       "rollback" "insert" "raw_delete" "set" "add" "remove" "clear"
       "create_function" "create_aggregate" "create_deterministic_function"))

(define io-read-verbs  (set "read" "readline" "readlines" "recv" "recvfrom" "readinto" "peek" "readable"))
(define io-write-verbs (set "write" "writelines" "send" "sendall" "sendto"))

;; A typed operation symbol: domain<ReceiverType>.verb (e.g. db<QuerySet>.read,
;; io<File>.write). The receiver TYPE is the concrete class that performs the
;; operation — a db.read on a QuerySet is a different capability from one on a
;; backend cursor, and the type keeps them distinct atoms in the effect graph.
;; An unclassified method keeps () — the coarse "any op in the domain" — never
;; a receiver-less domain.verb: that middle tier is unsound because it conflates
;; every receiver that happens to share a verb.
(define (typed-op domain cname verb)
  (string->symbol (format "~a<~a>.~a" domain cname verb)))

;; Primitive ops for a method of a class in a module, given the domain. Ops are
;; TYPED by the receiver class, so the I/O stream (file/http/stdio/socket) is
;; implicit in the class name — no separate resource table.
(define (method-ops domain path cname mname)
  (case domain
    [(db) (cond [(set-member? db-write-methods mname) (list (typed-op domain cname "write"))]
                [(set-member? db-read-methods mname)  (list (typed-op domain cname "read"))]
                [else '()])]
    [(io) (cond
            [(set-member? io-write-verbs mname) (list (typed-op domain cname "write"))]
            [(set-member? io-read-verbs mname)  (list (typed-op domain cname "read"))]
            [(member mname '("close" "seek" "tell" "truncate" "fileno" "flush" "readable" "writable" "seekable"))
             (list (typed-op domain cname mname))]
            [else '()])]
    [else '()]))

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

;; Derive Class.method → effect-cap for every class in a tagged module. Pure
;; modules (domain 'pure) and untagged modules contribute nothing; methods
;; with no classified op keep () ops (the coarse category within the domain).
(define (derive-class-method-effects mods [config effect-tag-config])
  (define out (make-hash))
  (for ([(path mi) (in-hash mods)])
    (define domain (module-effect-tag path config))
    (when (and domain (not (eq? domain 'pure)))
      (for ([(cname ci) (in-hash (module-info-classes mi))])
        (for ([mname (in-hash-keys (class-info-methods ci))])
          (hash-set! out (format "~a.~a" cname mname)
                     (effect-cap domain (method-ops domain path cname mname)))))))
  out)

;; Parse a typed operation symbol back into its parts: `db<QuerySet>.read` →
;; '(db "QuerySet" "read"); a non-typed symbol → #f. The inverse of `typed-op`.
;; The receiver name is a *string* so callers can look it up in the class/super
;; tables without re-parsing.
(define (typed-op-parts op)
  (define s (symbol->string op))
  (define m (regexp-match #rx"^([a-z]+)<([^>]+)>[.](.+)$" s))
  (and m
       (list (string->symbol (list-ref m 1))
             (list-ref m 2)
             (list-ref m 3))))

;; Class → list of direct super names, harvested from the class-info super
;; field. This is the MRO *surface*: a per-class chain, not a linearized list,
;; because linearization needs cross-module resolution (super names may live in
;; imported modules) which the caller owns. Keys are bare class names — the
;; same granularity as `derive-class-method-effects`.
(define (derive-class-supers mods)
  (define out (make-hash))
  (for ([(path mi) (in-hash mods)])
    (for ([(cname ci) (in-hash (module-info-classes mi))])
      (define supers (class-info-super ci))
      (when (pair? supers) (hash-set! out cname supers))))
  out)
