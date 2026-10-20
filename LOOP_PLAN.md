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
- Python: `@effects(...)` annotation extraction + `__validate_effects` emit.
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
5. Inference/annotation is not wired into any pipeline entry point today
   (unit tests only).

## Unrelated bugs found (do not fix here)

- `lower-javascript-test.rkt` fails pre-existing (confirmed via `git stash`):
  `cst-node-children: contract violation, expected: cst-node?`. A JS lowering
  bug, not effects-related.
- `tsx-integration-test.rkt` fails pre-existing (4 failures, confirmed via
  `git stash` of the emit change): ternary/conditional-rendering assertions at
  `tsx-integration-test.rkt:148-150,166` (`? (function()`, `: null`,
  `createTextNode(cond)`). Unrelated to effect emission.
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

- Anonymous `function` expressions lower to `(call "function" (params body))`,
  a sibling of `=>`. It is classified pure but its body is not yet a walk
  boundary, so a function expression's effects leak into the enclosing
  function. Add `"function"` to the arrow boundary check (and named
  `set!`+`function` collection) when needed.
- `const`/`let`/`var`-bound arrows lower to
  `(call "const"/"let"/"var" (list (set! NAME (call "=>" ...))))`; the `set!`
  sits one nesting level deeper than a bare function declaration, so those
  named arrows are not yet collected. Arrow annotation has no UIR slot anyway
  (it needs gap 4's `__withEffects` emit).

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
