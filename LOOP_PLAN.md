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
3. TypeScript function declarations lower to `(set! NAME (fn #f …))` and
   arrows lower to `(call "=>" (params body))`, so `collect-functions` (which
   keys on *named* `uir-fn` nodes) misses all TS functions — inference is a
   no-op on real TS source. `collect-functions`, the fixpoint walker, and
   `annotate-effects` need to recognize the `set!`+`fn` and `=>`+`async-fn`/
   `gen-fn` encodings (and write effects back to the wrapped fn).
4. TypeScript has no effect annotation extraction (`withEffects(...)` /
   JSDoc `@effects`) and no `__withEffects` emit.
5. Inference/annotation is not wired into any pipeline entry point today
   (unit tests only).

## Unrelated bugs found (do not fix here)

- `lower-javascript-test.rkt` fails pre-existing (confirmed via `git stash`):
  `cst-node-children: contract violation, expected: cst-node?`. A JS lowering
  bug, not effects-related.

## Iterations

- Iteration 1 (DONE): gaps 1 and 2 — teach `infer.rkt` the `dot` member-access
  encoding; classify front-end-encoded operator/statement/special-form calls as
  pure instead of `unknown`. Verified: `effects-infer-test` (new dot-encoding +
  purity tests) green; full effect + Python suites green.
