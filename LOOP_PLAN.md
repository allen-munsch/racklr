# LOOP_PLAN — effect typing & handlers for Python and TypeScript

Goal: reach non-trivial use cases of effect typing and effect handlers in the
Python and TypeScript front ends.

## Status of the feature

Done:

- UIR effect nodes: `uir-effect-row`, `uir-effect-entry` (+ Koka
  `uir-effect-decl/op/handler` and Effervesce `uir-eff-*`).
- Contract algebra (`racklr/effects/contract.rkt`): union/intersect/difference,
  subtyping, `mutator-ok?`.
- Effect inference (`racklr/effects/infer.rkt`): fixpoint over the call graph
  with direct-effect heuristics.
- Python: `@effects(...)` annotation extraction + `@__withEffects` runtime
  enforcement emit (`__perform` instrumentation + ambient context stack).
- Clojure: `^{:effects ...}` annotation + emit.
- Koka: effect decl / row / handler lower+emit.

## Gaps (toward Python + TypeScript)

1. ~~Effect inference only understands the Python `uir-get` member-access
   encoding. JS/TS/JSX lower `a.b` to `(call (symbol "dot") (list a b))`.~~
   DONE (iteration 1).
2. ~~Synthetic operator/statement calls (`+`, `dot`, `=>`, `not`, …) are
   classified as `unknown`, polluting every row.~~ DONE (iteration 1).
3. ~~TypeScript function declarations lower to `(set! NAME (fn #f …))` and
   arrows lower to `(call "=>" (params body))`, so `collect-functions` (which
   keys on *named* `uir-fn` nodes) misses all TS functions — inference is a
   no-op on real TS source. `collect-functions`, the fixpoint walker, and
   `annotate-effects` need to recognize the `set!`+`fn` and `=>`+`async-fn`/
   `gen-fn` encodings (and write effects back to the wrapped fn).~~
   DONE (iteration 2): `set!`+`fn`, `set!`+`async-fn`/`gen-fn`, and
   `set!`+`=>` now collect and infer; `annotate-effects` writes the row back
   into the wrapped fn for the fn-based encodings; arrow bodies are walk
   boundaries so they no longer leak into the enclosing function.
4. TypeScript has no effect annotation extraction (`withEffects(...)` /
   JSDoc `@effects`) and no `__withEffects` emit.
   - `__withEffects` emit DONE (iteration 3): `emit-javascript.rkt` now renders
     a `uir-effect-row` as a JS object literal, wraps any `uir-fn` (or
     async/gen-wrapped declaration) carrying a row in `__withEffects(row, fn)`,
     and prepends the `__withEffects` runtime preamble once when the program
     uses effects.
   - `withEffects(row)(fn)` extraction DONE (iteration 4):
     `racklr/effects/ts-annot.rkt` recognizes the curried wrapper's UIR shape
     `(call (call (var withEffects) (list ROW)) (list FN))`, extracts the row
     (`extract-with-effects-row`) and the wrapped fn
     (`extract-with-effects-fn`), using the same row-literal convention as
     Python's `@effects(...)`. Unit-tested on hand-built UIR; the row feeds
     the iteration-3 emit guard. JSDoc `@effects` extraction still open (needs
     comment preservation, not available in the CST).
5. ~~Inference/annotation is not wired into any pipeline entry point today
   (unit tests only).~~ DONE (iteration 6): `emit-python` and `emit-javascript`
   now run `annotate-effects` on their input before emitting, so unannotated
   programs get inferred effect rows emitted as runtime guards. To make that
   correct, `annotate-effects` now (a) preserves hand-written rows, (b) skips
   empty inferred rows, and (c) drops `unknown` entries when materializing a
   row for emission (the runtime guards reject the `unknown` modality).
   Also fixed the Python guard preamble's multi-line docstring, which the
   generated Python3 lexer cannot re-parse.

## Unrelated bugs found (do not fix here)

- `lower-javascript-test.rkt` fails pre-existing (confirmed via `git stash`):
  `cst-node-children: contract violation, expected: cst-node?`. A JS lowering
  bug, not effects-related.
- **Async arrows drop the `async` keyword** (found iteration 8):
  `lower-arrow-fn-impl` in `lower-typescript/stmt.rkt` always emits
  `(call "=>" ...)`, never checking the `Async` token, so
  `const load = async () => fetch('/api')` lowers identically to a sync arrow
  and emits `() => fetch("/api")`. Pre-existing lowering bug, unrelated to
  effects; async function *declarations* are unaffected (they use
  `set!`+`async-fn`). Affects all arrows, bound or not.
- `tsx-integration-test.rkt` now passes (verified iteration 6). The 4
  ternary/conditional-rendering failures documented in earlier iterations were
  fixed by the iteration-5 curried-call lowering change.
- **Curried-call lowering drops the RHS** — FIXED (iteration 5):
  `lower-single-expression`'s 2-kid call branch now routes through a recursive
  `lower-call-args` helper that handles the `singleExpression[innerSuffix,
  arguments]` shape the grammar produces for `f(1)(2)` /
  `withEffects(row)(fn)`, plus the `arguments` and `OpenParen…CloseParen`
  suffix shapes (previously separate branches). Verified:
  `lower-typescript-test` green (3 new curried-call regressions) and
  `examples-roundtrip-test` 44/44. End-to-end `withEffects({…})(fn)` now
  lowers to the `(call (call (var withEffects) (list ROW)) (list FN))` shape
  that `effects/ts-annot.rkt` extracts.

## Remaining adjacent encodings (follow-ups, not done)

- ~~Anonymous `function` expressions lower to `(call "function" (params body))`,
  a sibling of `=>`. It is classified pure but its body is not yet a walk
  boundary, so a function expression's effects leak into the enclosing
  function.~~ DONE (iteration 7): added `function-expr-call?`; it is now a
  `walk-body` boundary (no leak into the enclosing function) and
  `named-function` collects `set!`+`function` (bound function expressions are
  separate call-graph nodes). Verified: `effects-infer-test` green (2 new
  tests), all effect suites + `lower-typescript-test` + `lower-python-test` +
  `examples-roundtrip-test` 44/44.
- ~~`const`/`let`/`var`-bound arrows lower to
  `(call "const"/"let"/"var" (list (set! NAME (call "=>" ...))))`; the `set!`
  sits one nesting level deeper than a bare function declaration, so those
  named arrows are not yet collected.~~ DONE (iteration 8): collection already
  worked — `collect-functions` uses the full-descent `walk-uir`, which visits
  the nested `set!`, so `named-function` matched it all along (verified with a
  real-TS probe before touching code). The actual gap was write-back + emit:
  `annotate-effects` did not descend into the var-decl wrapper, and arrows /
  function expressions have no `uir-fn` effects slot to write into.
  `annotate-effects` now (a) descends into `(call "var"/"let"/"const"
  (list (set! ...)))` and (b) wraps an inferred arrow / function expression in
  `(call "__withEffects" (list row fn))`; `emit-javascript` renders that call
  as `__withEffects(row, fn)` and `program-has-effects?` detects the embedded
  row so the guard preamble is emitted. End-to-end:
  `const f = () => llm.query('hi')` → `const f = __withEffects({"llm": ...},
  () => llm.query('hi'))`.

## Iterations

- Iteration 1 (DONE): gaps 1 and 2 — teach `infer.rkt` the `dot` member-access
  encoding; classify front-end-encoded operator/statement/special-form calls as
  pure instead of `unknown`. Verified: `effects-infer-test` (new dot-encoding +
  purity tests) green; full effect + Python suites green.
- Iteration 2 (DONE): gap 3 — recognize `set!`+`fn`, `set!`+`async-fn`/
  `gen-fn`, and `set!`+`=>` in `collect-functions`/`infer-effects`, write rows
  back into the wrapped fn in `annotate-effects`, and make arrow bodies walk
  boundaries. Verified: `effects-infer-test` green (7 new tests); full effect +
  Python + TS suites green.
- Iteration 3 (DONE): gap 4 emit half — `__withEffects` runtime-guard emission
  in `emit-javascript.rkt`. Renamed the recursive worker to `emit-js` and added
  a `emit-javascript` wrapper that prepends the guard preamble when
  `program-has-effects?`. Verified: `effects-js-emit-test` green (new suite);
  effects + TS + examples-roundtrip (44/44) + esbuild green.
- Iteration 4 (DONE): gap 4 extraction half — `racklr/effects/ts-annot.rkt`
  extracts a `uir-effect-row` (and the wrapped fn) from the
  `withEffects(row)(fn)` UIR shape. Verified: `effects-ts-annot-test` green
  (new suite); `effects-js-emit-test`, `effects-python-annot-test`,
  `effects-infer-test`, and `lower-typescript-test` all green. JSDoc `@effects`
  extraction remains open (comments not in CST); end-to-end `withEffects` on
  real TS source is blocked by the curried-call lowering bug documented above.
- Iteration 5 (DONE): unblocked end-to-end `withEffects` by fixing
  curried-call lowering (`f(1)(2)` dropped its RHS) in
  `lower-typescript/expr.rkt`. Added a recursive `lower-call-args` helper
  covering the `arguments`, `OpenParen…CloseParen`, and
  `singleExpression[innerSuffix, arguments]` suffix shapes. Verified:
  `lower-typescript-test` green (3 new regressions), `examples-roundtrip-test`
  44/44, and all four effects suites green.
- Iteration 6 (DONE): gap 5 — wired inference into the emitters.
  `emit-python`/`emit-javascript` now call `annotate-effects` first; `infer.rkt`
  preserves explicit rows, skips empty inferred rows, and drops `unknown`
  entries before materializing a row for emission. Fixed the Python preamble's
  multi-line docstring (generated Python3 lexer cannot re-parse it). Verified:
  `effects-infer-test` (3 new tests), `effects-js-emit-test` (1 new),
  `effects-python-emit-test` (new suite), all other effects suites,
  `lower-python-test`, `lower-typescript-test`, `esbuild-integration-test`,
  `emit-html-test`, and `examples-roundtrip-test` 44/44. End-to-end Python
  `llm.query` → `@__validate_effects` emit + re-parse confirmed.
- Iteration 7 (DONE): anonymous `function` expressions. Added
  `function-expr-call?` in `infer.rkt`; it is a `walk-body` boundary (no leak
  into the enclosing function) and `named-function` collects `set!`+`function`
  (bound function expressions are separate call-graph nodes). Verified:
  `effects-infer-test` green (2 new tests), all effect suites,
  `lower-typescript-test`, `lower-python-test`, and `examples-roundtrip-test`
  44/44.
- Iteration 8 (DONE): const/let/var-bound arrows & function expressions.
  Discovered collection already worked (`walk-uir` descends into the var-decl
  wrapper). Fixed the real gap: write-back + emit. `annotate-effects` descends
  into `(call "var"/"let"/"const" (list (set! ...)))` and wraps an inferred
  arrow/function expression in `(call "__withEffects" (list row fn))`;
  `emit-javascript` renders that as `__withEffects(row, fn)` and
  `program-has-effects?` detects the embedded row. Verified: `effects-infer-test`
  (5 new infer tests + 2 annotate tests), `effects-js-emit-test` (5 new), all
  effect suites, `lower-typescript-test`, `lower-python-test`,
  `esbuild-integration-test`, `emit-html-test`, `examples-roundtrip-test` 44/44.
- Iteration 9 (DONE): wired the TS/JS `withEffects(row)(fn)` annotation into
  the pipeline. The iteration-4 extraction (`effects/ts-annot.rkt`) was only
  unit-tested on hand-built UIR; a real `withEffects({...})(fn)` in TS source
  lowered to `(call (call (var withEffects) (list ROW)) (list FN))` and then
  round-tripped as a call to an undefined `withEffects` — no guard, no
  preamble. `annotate-effects` now rewrites that shape (both as a `set!` value
  and as a bare expression statement) into the internal
  `(call "__withEffects" (list row fn))`, so `program-has-effects?` sees the
  embedded `uir-effect-row` and `emit-javascript` renders the guard. Verified:
  `effects-js-emit-test` (2 new tests), all effect suites,
  `lower-typescript-test`, `lower-python-test`, `examples-roundtrip-test`
  44/44, `esbuild-integration-test`, `emit-html-test`. End-to-end:
  `const f = withEffects({llm:{max:5}})(() => llm.query("hi"))` →
  `const f = __withEffects({"llm":{"modality":"may","max":5}}, () => llm.query("hi"))`
  with the preamble.
- Iteration 11 (DONE): Python runtime enforcement. Ported the JS/TS
  `__perform` call-site instrumentation and ambient effect-context stack to
  `emit-python.rkt`. `emit-expr` now wraps a directly-performed effect call
  (via `direct-effect-of-callee`/`effect-op-of-callee`) as
  `__perform("llm", "query", lambda: llm.query(...))`; the
  `@__validate_effects` guard was replaced with `@__withEffects`, whose preamble
  grows the same context stack and enforces absence/prohibition, `must-not`,
  the `ops` whitelist, `max` budgets, and sync `must` obligations on completion
  (checked in `finally` so the frame always pops). `program-has-effects?` now
  also detects top-level direct-effect calls. Verified with a new
  python3-executed suite (`effects-python-runtime-test.rkt`, 8 tests) plus
  `effects-python-emit-test` (rewritten: 6 tests incl. `__perform` instrumentation
  and top-level-call preamble), `lower-python-test` (2 assertions renamed), all
  eight effect suites, `lower-typescript-test`, `examples-roundtrip-test` 44/44,
  `esbuild-integration-test`, `emit-html-test`; emitted preamble + guarded
  function re-parse cleanly through the generated Python3 lexer.
- Iteration 10 (DONE): JS/TS runtime enforcement (the "effect handler" half).
  Direct-effect call sites (`llm.query`, `db.write`, `fetch`, …) are now
  instrumented at emit time as
  `__perform("llm", "query", function() { return llm.query(...); })`; the
  `__withEffects` preamble grew an ambient effect-context stack and enforces,
  against the operations a function actually performs: absence/prohibition and
  `must-not` (throw "effect prohibited"), the `ops` whitelist (throw "effect
  operation prohibited"), `max` budgets per invocation (throw "effect budget
  exceeded"), and `must` obligations on completion (throw "effect obligation
  unmet", checked after promise settlement for async fns). Added
  `effect-op-of-callee` to `infer.rkt` (last field of a dotted chain, or the
  bare callee name) and `program-has-effects?` now also detects top-level
  direct-effect calls so the preamble is emitted when a bare call is
  instrumented. Verified with a new node-executed suite
  (`effects-js-runtime-test.rkt`, 11 tests: budget, must-not, absence, ops,
  must met/unmet, and post-`await` budget/must) plus `effects-js-emit-test`
  (3 new + 2 updated), `effects-infer-test` (5 new), all eight effect suites,
  `lower-typescript-test`, `lower-python-test`, `examples-roundtrip-test`,
  `esbuild-integration-test`, `emit-html-test`.
- Iteration 13 (DONE): strip the Python `@effects(...)` annotation decorator at
  emit time. `emit-decorated` in `emit-python.rkt` now filters out
  `effects-decorator?` decorators (requiring `effects/python-annot`) so a
  guarded function emits only `@__withEffects(...)`, not the undefined
  `@effects` name (which raised `NameError` at import). Verified:
  `effects-python-emit-test` (1 new), `effects-python-runtime-test` (1 new
  python3-executed), all eight effect suites, `lower-python-test`.
- Iteration 12 (DONE): Python async `must` obligations + async round-trip.
  `async def` previously lowered via `lower-funcdef` with the `async` keyword
  dropped, so a guarded function with an `await` emitted as an invalid sync
  `def ... await` and the `__withEffects` wrapper popped its context frame
  before the coroutine body ran — async budget/must-not/ops/must were all
  unenforced. `lower-python/stmt.rkt` now wraps `async def` (plain and
  decorated) in the existing JS/TS `(call "async-fn" (list fn))` convention
  (`async-fn-wrap`); `emit-python.rkt` renders that as `async def` (with the
  guard prefix) in both statement and decorated positions, and the
  `__withEffects` preamble now imports `inspect` and returns an
  `async def wrapped` that `await`s `func(*args, **kwargs)` before `_check`
  when `inspect.iscoroutinefunction(func)`. `annotate-effects` descends into a
  bare async-fn statement so inferred rows reach the wrapped fn. Verified:
  `lower-python-test` (async UIR-shape + round-trip assertions),
  `effects-python-runtime-test` (3 new async tests: must met, must unmet,
  budget exceeded), `effects-python-emit-test` (2 new: async guard emission +
  async-aware preamble), all effect suites, `lower-typescript-test`,
  `examples-roundtrip-test` 44/44, `esbuild-integration-test`, `emit-html-test`;
   emitted async guarded preamble re-parses cleanly through the generated
   Python3 lexer. Only `lower-javascript-test` fails, pre-existing (documented).
 - Iteration 14 (DONE): runtime `scope` enforcement (`reads=`/`writes=`).
   The `__perform` preamble in both `emit-javascript.rkt` and `emit-python.rkt`
   now enforces the direction half of a `reads=`/`writes=` scope: a `reads=X`
   entry throws `effect scope violation: <name>.<op> outside reads=X` when the
   performed op is a known write verb, and a `writes=X` entry throws the mirror
   when the op is a known read verb. Bare (non-directional) qualifiers stay
   opaque and are emitted as data only. Resource matching (the `X` side) is
   deliberately deferred — the call-site instrumentation exposes only the op
   name (`effect-op-of-callee`), not the resource path. Verified:
   `effects-js-runtime-test` (4 new tests), `effects-python-runtime-test`
   (4 new tests), all nine effect suites, `lower-python-test`,
   `lower-typescript-test`, `tsx-integration-test`, `esbuild-integration-test`,
   `emit-html-test`, and `examples-roundtrip-test` 44/44. Only
   `lower-javascript-test` fails, pre-existing (documented).

## Still open (next iterations)

- **JSDoc `@effects` extraction** — still blocked: comments are dropped by the
  generated lexer (`channel(HIDDEN)` is treated like `skip`), so the CST has
  no comment tokens. Needs comment preservation in `gend-parser/lexer.rkt`.
- ~~**`scope` enforcement** — `scope` (`reads=`/`writes=`) prohibitions are not
  enforced at runtime in either target, only emitted as data.~~ DONE
  (iteration 14): both `__perform` preambles now enforce the direction half of
  a `reads=`/`writes=` scope against the performed operation name — a `reads=X`
  entry throws when the op is a known write verb, a `writes=X` entry throws when
  the op is a known read verb; bare (non-directional) qualifiers stay opaque and
  are still emitted as data. Resource matching (the `X` side) is deferred — the
  call site currently exposes only the op name, not the resource path.
- ~~**`@effects(...)` annotation decorator is re-emitted literally**~~ DONE
  (iteration 13): `emit-decorated` in `emit-python.rkt` now filters out
  `effects-decorator?` decorators before rendering, so a guarded function
  carries only `@__withEffects(...)` (the row travels in the inner fn's effects
  field). Verified with a new emit-level regression
  (`effects-python-emit-test`) plus a python3-executed runtime regression
  (`effects-python-runtime-test`, no `NameError`).
