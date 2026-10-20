#lang racket

;; Python effect-handler runtime enforcement (budget / must-not / ops / must).
;;
;; These tests emit Python via `emit-python`, append a harness that defines
;; stub effect sources, and run the result through `python3` to assert the
;; runtime guard actually raises (or not) against the operations a function
;; performs.

(require rackunit
         racket/file
         racket/system
         racklr/uir
         racklr/emit-python)

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
;; Python attribute access `a.b` lowers to (uir-get a (uir-string "b")).
(define (pydot b f) (uir-get b (uir-string f)))

(define (prog . stmts) (uir-block stmts))

(define (fn name body #:params [params '()] #:effects [effects #f])
  (uir-fn (sym name) params body #f effects))

(define (llm-op op) (uir-call (pydot (v "llm") op) '()))
(define (db-op op) (uir-call (pydot (v "db") op) '()))

;; Build one effect entry: (name modality ops budget scope).
(define (entry name #:modality [modality 'may] #:max [max #f] #:ops [ops '()])
  (uir-effect-entry (sym name) modality (map sym ops) max #f))

(define (row . entries)
  (uir-effect-row entries))

;; Program: @__withEffects(ROW)\ndef f(): STMTS
(define (guarded-program row-uir stmts)
  ;; An empty body must still emit a valid Python function (a `pass`).
  (prog (fn "f"
            (uir-block (if (null? stmts) (list (uir-null)) stmts))
            #:effects row-uir)))

;; ── python harness ──────────────────────────────────────────────────

(define (run-python py)
  (define script (make-temporary-file "racklr-effect-~a.py"))
  (define outfile (make-temporary-file "racklr-effect-out-~a.txt"))
  (display-to-file py script #:exists 'replace)
  (define ok? (system (format "python3 ~a > ~a 2>&1" script outfile)))
  (define output
    (with-handlers ([exn:fail:filesystem? (λ (e) "")])
      (file->string outfile)))
  (delete-file script)
  (delete-file outfile)
  (values ok? output))

(define (call-f-harness)
  (string-append
   "class _LLM:\n"
   "    def query(self): return \"q\"\n"
   "    def generate(self): return \"g\"\n"
   "llm = _LLM()\n"
   "class _DB:\n"
   "    def fetch(self): return \"d\"\n"
   "    def write(self): return \"w\"\n"
   "db = _DB()\n"
   "try:\n"
   "    f()\n"
   "    print(\"OK\")\n"
   "except Exception as e:\n"
   "    print(\"THREW:\" + str(e))\n"))

(define (check-throws program message)
  (define-values (ok? out)
    (run-python (string-append (emit-python program) "\n" (call-f-harness))))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out (format "THREW:~a" message))
              (format "expected THREW:~a, got: ~a" message out)))

(define (check-runs program)
  (define-values (ok? out)
    (run-python (string-append (emit-python program) "\n" (call-f-harness))))
  (check-true ok? (format "python3 exited nonzero: ~a" out))
  (check-true (string-contains? out "OK")
              (format "expected OK, got: ~a" out)))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-budget-exceeded)
  (check-throws (guarded-program (row (entry "llm" #:max 1))
                                 (list (llm-op "query") (llm-op "query")))
                "effect budget exceeded: llm"))

(define (test-must-not-prohibited)
  (check-throws (guarded-program (row (entry "db" #:modality 'must-not))
                                 (list (db-op "write")))
                "effect prohibited: db"))

(define (test-absence-prohibits)
  (check-throws (guarded-program (row)
                                 (list (llm-op "query")))
                "effect prohibited: llm"))

(define (test-op-whitelist)
  (check-throws (guarded-program (row (entry "llm" #:ops '("query")))
                                 (list (llm-op "generate")))
                "effect operation prohibited: llm.generate"))

(define (test-must-unmet)
  (check-throws (guarded-program (row (entry "llm" #:modality 'must))
                                 '())
                "effect obligation unmet: llm"))

(define (test-must-met)
  (check-runs (guarded-program (row (entry "llm" #:modality 'must))
                               (list (llm-op "query")))))

(define (test-must-ops-met)
  (check-runs (guarded-program (row (entry "llm" #:modality 'must #:ops '("query")))
                               (list (llm-op "query")))))

(define (test-may-within-budget-runs)
  (check-runs (guarded-program (row (entry "llm" #:max 5))
                               (list (llm-op "query")))))

(module+ main
  (test-budget-exceeded)
  (test-must-not-prohibited)
  (test-absence-prohibits)
  (test-op-whitelist)
  (test-must-unmet)
  (test-must-met)
  (test-must-ops-met)
  (test-may-within-budget-runs)
  (displayln "All Python effect-runtime enforcement tests passed."))
