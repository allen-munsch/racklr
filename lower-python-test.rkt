#lang racket

(require rackunit
         "tree.rkt"
         "uir.rkt"
         "gen-test.rkt"
         "lower-python.rkt"
         "emit-python.rkt")

;; ── Load the Python3 parser ──────────────────────────────────────────

(define-values (py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "grammars-v4/python/python3/Python3Parser.g4"))

;; ── Round-trip test helper ──────────────────────────────────────────

(define (check-py input-str expected-emitted)
  (define cst (py-parse input-str))
  (check-true (any-tree? cst)
              (format "parse ~s" input-str))
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir? uir)
              (format "lower ~s" input-str))
  (define emitted (emit-python uir))
  (check-true (string? emitted)
              (format "emit ~s" input-str))
  (check-equal? emitted expected-emitted
                (format "round-trip ~s" input-str)))

;; ── Simple statements ────────────────────────────────────────────────

(check-py "pass\n" "pass")
(check-py "return\n" "return None")
(check-py "return 42\n" "return 42")
(check-py "x = 1\n" "x = 1")
(check-py "import os\n" "import os")
(check-py "from os import path\n" "import os.path")
(check-py "y = x\n" "y = x")

;; ── UIR inspection tests ────────────────────────────────────────────

;; pass lowers to uir-null
(let ([cst (py-parse "pass\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-null? uir) "pass lowers to uir-null"))

;; return 42 lowers to (uir-return (uir-number "42"))
(let ([cst (py-parse "return 42\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-return? uir) "return lowers to uir-return")
  (check-true (uir-number? (uir-return-value uir)) "return value is number")
  (check-equal? (uir-number-value (uir-return-value uir)) "42" "return value is 42"))

;; x = 1 lowers to (uir-set! (uir-var ...) (uir-number "1"))
(let ([cst (py-parse "x = 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-set!? uir) "assignment lowers to uir-set!")
  (check-true (uir-var? (uir-set!-name uir)) "LHS is uir-var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-set!-name uir))) "x" "LHS name is x")
  (check-true (uir-number? (uir-set!-value uir)) "RHS is number")
  (check-equal? (uir-number-value (uir-set!-value uir)) "1" "RHS value is 1"))

;; import os lowers to (uir-import (uir-symbol "os") '())
(let ([cst (py-parse "import os\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-import? uir) "import lowers to uir-import")
  (check-equal? (uir-symbol-name (uir-import-source uir)) "os" "module name"))

;; ── For loops ─────────────────────────────────────────────────────

;; round-trip: for x in y: pass
(check-py "for x in y: pass\n\n" "for x in y:\n    pass")

;; UIR inspection: for x in y: pass lowers to uir-for-each
(let ([cst (py-parse "for x in y: pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-for-each? uir) "for loop lowers to uir-for-each")
  (check-equal? (uir-symbol-name (uir-for-each-var uir)) "x" "loop var is x")
  (check-true (uir-var? (uir-for-each-iterable uir)) "iterable is uir-var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-for-each-iterable uir))) "y" "iterable is y")
  (check-true (uir-null? (uir-for-each-body uir)) "body is pass (uir-null)")
  (check-true (uir-null? (uir-for-each-else-body uir)) "no else clause"))

;; ── Try/except/finally ────────────────────────────────────────────

;; round-trip: try/except bare
(check-py "try: pass\nexcept: pass\n\n" "try:\n    pass\nexcept:\n    pass")

;; round-trip: try/except with exception type and as
(check-py "try: pass\nexcept Exception as e: pass\n\n"
          "try:\n    pass\nexcept Exception as e:\n    pass")

;; round-trip: try/except/else
(check-py "try: pass\nexcept: pass\nelse: pass\n\n"
          "try:\n    pass\nexcept:\n    pass\nelse:\n    pass")

;; round-trip: try/finally (no except)
(check-py "try: pass\nfinally: pass\n\n"
          "try:\n    pass\nfinally:\n    pass")

;; UIR inspection: try/except structure
(let ([cst (py-parse "try: pass\nexcept ValueError as e: return e\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-try? uir) "try lowers to uir-try")
  (check-true (uir-null? (uir-try-body uir)) "try body is pass (uir-null)")
  (check-equal? (length (uir-try-catches uir)) 1 "one catch clause")
  (let ([cat (car (uir-try-catches uir))])
    (check-true (uir-var? (car cat)) "exception type is var")
    (check-equal? (uir-symbol-name (uir-var-name (car cat))) "ValueError" "exception type is ValueError")
    (check-equal? (uir-symbol-name (uir-var-name (cadr cat))) "e" "exception name is e")
    (check-true (uir-return? (caddr cat)) "handler has return"))
  (check-false (uir-try-else-body uir) "no else clause")
  (check-false (uir-try-finally-body uir) "no finally clause"))

;; ── With statements ───────────────────────────────────────────────

;; round-trip: with x as y: pass
(check-py "with x as y: pass\n\n"
          "with x as y:\n    pass")

;; round-trip: with x: pass (no as)
(check-py "with x: pass\n\n"
          "with x:\n    pass")

;; UIR inspection: with with_items
(let ([cst (py-parse "with x as y: pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-with? uir) "with lowers to uir-with")
  (check-equal? (length (uir-with-items uir)) 1 "one with item")
  (let ([item (car (uir-with-items uir))])
    (check-true (uir-var? (car item)) "context expr is var")
    (check-true (uir-var? (cadr item)) "as-name is var")
    (check-equal? (uir-symbol-name (uir-var-name (cadr item))) "y" "as-name is y"))
  (check-true (uir-null? (uir-with-body uir)) "body is pass"))

;; ── Yield ──────────────────────────────────────────────────────────

;; round-trip: yield x
(check-py "yield x\n" "yield x")

;; round-trip: yield from x
(check-py "yield from x\n" "yield from x")

;; UIR inspection: yield x
(let ([cst (py-parse "yield x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-yield? uir) "yield lowers to uir-yield")
  (check-false (uir-yield-from? uir) "not yield from")
  (check-true (uir-var? (uir-yield-value uir)) "yield value is var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-yield-value uir))) "x" "yield value is x"))

;; UIR inspection: yield from x
(let ([cst (py-parse "yield from x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-yield-from? uir) "is yield from"))

;; ── Await ──────────────────────────────────────────────────────────

;; round-trip: await foo()
(check-py "await foo()\n" "await foo()")

;; UIR inspection: await foo()
(let ([cst (py-parse "await foo()\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-await? uir) "await lowers to uir-await")
  (check-true (uir-call? (uir-await-expr uir)) "await wraps a call"))

;; ── Async def ─────────────────────────────────────────────────────

;; UIR inspection: async def foo(): pass
(let ([cst (py-parse "async def foo(): pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-fn? uir) "async def lowers to uir-fn"))

;; ── Lambda ────────────────────────────────────────────────────────

;; UIR inspection: lambda x: x + 1
(let ([cst (py-parse "lambda x: x + 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-fn? uir) "lambda lowers to uir-fn")
  (check-equal? (length (uir-fn-params uir)) 1 "lambda has 1 param")
  (check-equal? (uir-symbol-name (first (uir-fn-params uir))) "x" "param name is x"))

;; UIR inspection: lambda: 42
(let ([cst (py-parse "lambda: 42\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-fn? uir) "no-arg lambda lowers to uir-fn")
  (check-equal? (length (uir-fn-params uir)) 0 "no params"))

;; ── Decorators ─────────────────────────────────────────────────────

;; round-trip: @deco\ndef foo(): pass
(check-py "@deco\ndef foo(): pass\n\n"
          "@deco\ndef foo():\n    pass")

;; UIR inspection: @deco\ndef foo(): pass
(let ([cst (py-parse "@deco\ndef foo(): pass\n\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-decorated? uir) "decorated lowers to uir-decorated")
  (check-equal? (length (uir-decorated-decorators uir)) 1 "one decorator")
  (check-true (uir-call? (first (uir-decorated-decorators uir))) "decorator is a call")
  (check-true (uir-fn? (uir-decorated-inner uir)) "inner is uir-fn"))

;; ── Data structure literals ─────────────────────────────────────────

;; list round-trip
(check-py "[1, 2, 3]\n" "[1, 2, 3]")

;; empty list
(check-py "[]\n" "[]")

;; dict round-trip
(check-py "{\"a\": 1, \"b\": 2}\n" "{\"a\": 1, \"b\": 2}")

;; empty dict
(check-py "{}\n" "{}")

;; UIR inspection: list
(let ([cst (py-parse "[1, 2, 3]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-list? uir) "list literal lowers to uir-list")
  (check-equal? (length (uir-list-items uir)) 3 "list has 3 items")
  (check-equal? (uir-number-value (first (uir-list-items uir))) "1" "first item is 1"))

;; UIR inspection: dict
(let ([cst (py-parse "{\"a\": 1}\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-record? uir) "dict literal lowers to uir-record")
  (check-equal? (length (uir-record-entries uir)) 1 "dict has 1 entry")
  (let ([entry (car (uir-record-entries uir))])
    (check-equal? (uir-string-value (car entry)) "a" "key is a")
    (check-equal? (uir-number-value (cdr entry)) "1" "value is 1")))

;; ── Comprehensions ──────────────────────────────────────────────────

;; list comp round-trip
(check-py "[x for x in y]\n" "[x for x in y]")

;; list comp with filter
(check-py "[x for x in y if True]\n" "[x for x in y if True]")

;; list comp with expression
(check-py "[x+1 for x in y]\n" "[x + 1 for x in y]")

;; set comp
(check-py "{x for x in y}\n" "{x for x in y}")

;; UIR inspection: list comp
(let ([cst (py-parse "[x for x in y]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "list comp lowers to uir-call")
  (let ([callee (uir-call-callee uir)])
    (check-true (uir-symbol? callee) "callee is symbol")
    (check-equal? (uir-symbol-name callee) "list-comp" "callee is list-comp")))

;; UIR inspection: set comp
(let ([cst (py-parse "{x for x in y}\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "set comp lowers to uir-call")
  (let ([callee (uir-call-callee uir)])
    (check-equal? (uir-symbol-name callee) "set-comp" "callee is set-comp")))

;; ── Boolean operators ───────────────────────────────────────────────

;; and round-trip
(check-py "x and y\n" "x and y")

;; or round-trip
(check-py "x or y\n" "x or y")

;; not round-trip
(check-py "not x\n" "not x")

;; UIR inspection: not x
(let ([cst (py-parse "not x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "not lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "not" "callee is not"))

;; ── Augmented assignment ────────────────────────────────────────────

;; += round-trip
(check-py "x += 1\n" "x = x + 1")

;; -= round-trip
(check-py "x -= 2\n" "x = x - 2")

;; UIR inspection: x += 1
(let ([cst (py-parse "x += 1\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-set!? uir) "augassign lowers to uir-set!")
  (check-true (uir-call? (uir-set!-value uir)) "RHS is a call")
  (check-equal? (uir-symbol-name (uir-call-callee (uir-set!-value uir))) "+" "operator is +"))

;; ── Attribute access and subscript ──────────────────────────────────

;; round-trip: x.attr
(check-py "x.attr\n" "x.attr")

;; round-trip: x[y]
(check-py "x[y]\n" "x[y]")

;; round-trip: x[0]
(check-py "x[0]\n" "x[0]")

;; round-trip: chained: x.attr[y]
(check-py "x.attr[y]\n" "x.attr[y]")

;; UIR inspection: x.attr lowers to uir-get with string field
(let ([cst (py-parse "x.attr\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-get? uir) "x.attr lowers to uir-get")
  (check-true (uir-var? (uir-get-base uir)) "base is var")
  (check-equal? (uir-symbol-name (uir-var-name (uir-get-base uir))) "x" "base name is x")
  (check-true (uir-string? (uir-get-field uir)) "field is string")
  (check-equal? (uir-string-value (uir-get-field uir)) "attr" "field is 'attr'"))

;; UIR inspection: x[0] lowers to uir-get with number field
(let ([cst (py-parse "x[0]\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-get? uir) "x[0] lowers to uir-get")
  (check-true (uir-var? (uir-get-base uir)) "base is var")
  (check-true (uir-number? (uir-get-field uir)) "field is number")
  (check-equal? (uir-number-value (uir-get-field uir)) "0" "field is 0"))

;; round-trip: x[a, b]
(check-py "x[a, b]\n" "x[[a, b]]")

;; round-trip: f-string
(check-py "f\"hello {name}\"\n" "f\"hello {name}\"")

;; ── del, global, nonlocal ──────────────────────────────────────────

;; round-trip: del x
(check-py "del x\n" "del x")

;; round-trip: del x, y
(check-py "del x, y\n" "del x, y")

;; round-trip: global x
(check-py "global x\n" "global x")

;; round-trip: global x, y
(check-py "global x, y\n" "global x, y")

;; round-trip: nonlocal x
(check-py "nonlocal x\n" "nonlocal x")

;; round-trip: nonlocal x, y
(check-py "nonlocal x, y\n" "nonlocal x, y")

;; UIR inspection: del x
(let ([cst (py-parse "del x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "del lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "del" "callee is del")
  (check-equal? (length (uir-call-args uir)) 1 "one arg")
  (check-true (uir-var? (first (uir-call-args uir))) "arg is var"))

;; UIR inspection: global x, y
(let ([cst (py-parse "global x, y\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "global lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "global" "callee is global")
  (check-equal? (length (uir-call-args uir)) 2 "two names")
  (check-equal? (uir-symbol-name (first (uir-call-args uir))) "x" "first name is x"))

;; ── Raise ───────────────────────────────────────────────────────────

;; round-trip: raise
(check-py "raise\n" "raise")

;; round-trip: raise expr
(check-py "raise ValueError\n" "raise ValueError")

;; round-trip: raise expr from cause
(check-py "raise ValueError from e\n" "raise ValueError from e")

;; UIR inspection: raise ValueError
(let ([cst (py-parse "raise ValueError\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "raise lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "raise" "callee is raise")
  (check-equal? (length (uir-call-args uir)) 1 "one arg")
  (check-true (uir-var? (first (uir-call-args uir))) "arg is var"))

;; UIR inspection: raise ValueError from e
(let ([cst (py-parse "raise ValueError from e\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "raise from lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "raise" "callee is raise")
  (check-equal? (length (uir-call-args uir)) 2 "two args")
  (check-true (uir-var? (second (uir-call-args uir))) "cause is var"))

;; ── Assert ──────────────────────────────────────────────────────────

;; round-trip: assert expr
(check-py "assert True\n" "assert True")

;; round-trip: assert expr, msg
(check-py "assert x, \"msg\"\n" "assert x, \"msg\"")

;; UIR inspection: assert x
(let ([cst (py-parse "assert x\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "assert lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "assert" "callee is assert")
  (check-equal? (length (uir-call-args uir)) 1 "one arg")
  (check-true (uir-var? (first (uir-call-args uir))) "arg is var"))

;; ── Ternary expressions ─────────────────────────────────────────────

;; round-trip: ternary x if cond else y
(check-py "x if cond else y\n" "x if cond else y")

;; round-trip: ternary in assignment
(check-py "z = x if cond else y\n" "z = x if cond else y")

;; UIR inspection: ternary
(let ([cst (py-parse "x if cond else y\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-if? uir) "ternary lowers to uir-if")
  (check-true (uir-var? (uir-if-then uir)) "then is var")
  (check-true (uir-var? (uir-if-test uir)) "test is var")
  (check-true (uir-var? (uir-if-else uir)) "else is var"))

;; ── Identity and membership operators ───────────────────────────────

;; round-trip: is
(check-py "1 is None\n" "1 is None")

;; round-trip: in
(check-py "x in y\n" "x in y")

;; UIR inspection: is
(let ([cst (py-parse "1 is None\n")])
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir-call? uir) "is lowers to uir-call")
  (check-equal? (uir-symbol-name (uir-call-callee uir)) "is" "callee is is")
  (check-equal? (length (uir-call-args uir)) 2 "two args"))

;; ── Cleanup ─────────────────────────────────────────────────────────

(cleanup)
(displayln "~nAll lower-python tests passed.")
