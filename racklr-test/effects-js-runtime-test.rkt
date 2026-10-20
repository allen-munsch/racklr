#lang racket

;; JS/TS effect-handler runtime enforcement (budget / must-not / ops / must).
;;
;; These tests emit JS via `emit-javascript`, append a small harness that
;; defines stub effect sources, and run the result through node to assert the
;; runtime guard actually throws (or not) against the operations a function
;; performs.

(require rackunit
         racket/file
         racket/system
         racklr/uir
         racklr/emit-javascript)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (jsdot b f) (uir-call (sym "dot") (list b (sym f))))

(define (decl . stmts) (uir-block stmts))

(define (var-decl kind binding)
  (uir-call (sym kind) (list binding)))

(define (record . pairs)
  (uir-record (map (λ (p) (cons (uir-string (car p)) (cadr p))) pairs)))

;; (call (call (var withEffects) (list ROW)) (list FN)) — the UIR shape a
;; hand-written TS/JS `withEffects(row)(fn)` annotation lowers to.
(define (with-effects-wrapper row fn)
  (uir-call (uir-call (uir-var (sym "withEffects")) (list row))
            (list fn)))

(define (arrow body)
  (uir-call (sym "=>") (list (uir-list '()) body)))

(define (llm-op op) (uir-call (jsdot (v "llm") op) '()))
(define (db-op op) (uir-call (jsdot (v "db") op) '()))

(define (async-fn body)
  (uir-call (sym "async-fn") (list (uir-fn #f '() body #f #f))))

(define (await-promise)
  (uir-await (uir-call (jsdot (v "Promise") "resolve") '())))

;; Build a constraint record for one effect entry: {"modality","max","ops"}.
(define (entry #:modality [modality "may"] #:max [max #f] #:ops [ops #f])
  (define base (list (list "modality" (uir-string modality))))
  (define with-ops
    (if ops (cons (list "ops" (uir-list (map (λ (o) (uir-string o)) ops))) base) base))
  (define with-max
    (if max (cons (list "max" (uir-number (number->string max))) with-ops) with-ops))
  (apply record with-max))

;; Build a full row record: ((name entry) ...).
(define (row . name-entries)
  (apply record name-entries))

;; Program: const f = withEffects(ROW)(() => { STMTS });
(define (guarded-program row-record stmts)
  (decl (var-decl "const"
                  (uir-set! (sym "f")
                            (with-effects-wrapper row-record
                                                  (arrow (uir-block stmts)))))))

;; Program: const f = withEffects(ROW)(async () => { STMTS });
(define (guarded-async-program row-record stmts)
  (decl (var-decl "const"
                  (uir-set! (sym "f")
                            (with-effects-wrapper row-record
                                                  (async-fn (uir-block stmts)))))))

;; ── node harness ────────────────────────────────────────────────────

(define (run-node js)
  (define script (make-temporary-file "racklr-effect-~a.js"))
  (define outfile (make-temporary-file "racklr-effect-out-~a.txt"))
  (display-to-file js script #:exists 'replace)
  (define ok? (system (format "node ~a > ~a 2>&1" script outfile)))
  (define output
    (with-handlers ([exn:fail:filesystem? (λ (e) "")])
      (file->string outfile)))
  (delete-file script)
  (delete-file outfile)
  (values ok? output))

(define (call-f-harness)
  (string-append
   "globalThis.llm = { query: function(){ return \"q\"; }, generate: function(){ return \"g\"; } };\n"
   "globalThis.db = { fetch: function(){ return \"d\"; }, write: function(){ return \"w\"; } };\n"
   "try { f(); console.log(\"OK\"); } catch (e) { console.log(\"THREW:\" + e.message); }\n"))

(define (check-throws program message)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

(define (check-runs program)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

(define (call-f-async-harness)
  (string-append
   "globalThis.llm = { query: function(){ return \"q\"; }, generate: function(){ return \"g\"; } };\n"
   "globalThis.db = { fetch: function(){ return \"d\"; }, write: function(){ return \"w\"; } };\n"
   "(async function() { try { await f(); console.log(\"OK\"); } catch (e) { console.log(\"THREW:\" + e.message); } })();\n"))

(define (check-throws-async program message)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-async-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

(define (check-runs-async program)
  (define-values (ok? out)
    (run-node (string-append (emit-javascript program) "\n" (call-f-async-harness))))
  (check-true ok? (format "node exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-budget-exceeded)
  (check-throws (guarded-program (row (list "llm" (entry #:max 1)))
                                 (list (llm-op "query") (llm-op "query")))
                "effect budget exceeded: llm"))

(define (test-must-not-prohibited)
  (check-throws (guarded-program (row (list "db" (entry #:modality "must-not")))
                                 (list (db-op "write")))
                "effect prohibited: db"))

(define (test-absence-prohibits)
  (check-throws (guarded-program (row)
                                 (list (llm-op "query")))
                "effect prohibited: llm"))

(define (test-op-whitelist)
  (check-throws (guarded-program (row (list "llm" (entry #:ops '("query"))))
                                 (list (llm-op "generate")))
                "effect operation prohibited: llm.generate"))

(define (test-must-unmet)
  (check-throws (guarded-program (row (list "llm" (entry #:modality "must")))
                                 '())
                "effect obligation unmet: llm"))

(define (test-must-met)
  (check-runs (guarded-program (row (list "llm" (entry #:modality "must")))
                               (list (llm-op "query")))))

(define (test-must-ops-met)
  (check-runs (guarded-program (row (list "llm" (entry #:modality "must" #:ops '("query"))))
                               (list (llm-op "query")))))

(define (test-may-within-budget-runs)
  (check-runs (guarded-program (row (list "llm" (entry #:max 5)))
                               (list (llm-op "query")))))

(define (test-async-budget-after-await)
  ;; Effects performed after an await must still count against the ambient
  ;; frame: the context stays pushed until the returned promise settles.
  (check-throws-async
   (guarded-async-program (row (list "llm" (entry #:max 1)))
                          (list (await-promise) (llm-op "query") (llm-op "query")))
   "effect budget exceeded: llm"))

(define (test-async-must-met)
  (check-runs-async
   (guarded-async-program (row (list "llm" (entry #:modality "must")))
                          (list (await-promise) (llm-op "query")))))

(define (test-async-must-unmet)
  (check-throws-async
   (guarded-async-program (row (list "llm" (entry #:modality "must")))
                          (list (await-promise)))
   "effect obligation unmet: llm"))

(module+ main
  (test-budget-exceeded)
  (test-must-not-prohibited)
  (test-absence-prohibits)
  (test-op-whitelist)
  (test-must-unmet)
  (test-must-met)
  (test-must-ops-met)
  (test-may-within-budget-runs)
  (test-async-budget-after-await)
  (test-async-must-met)
  (test-async-must-unmet)
  (displayln "All JS effect-runtime enforcement tests passed."))
