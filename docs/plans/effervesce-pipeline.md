# Plan: Effervesce Lowering & Raising Pipeline

## Overview

Add Effervesce as a source and target language: `lower-effervesce.rkt` (Effervesce CST to UIR) and `emit-effervesce.rkt` (UIR to Effervesce source), with round-trip tests. This is the first step toward making Effervesce "work": today it is a grammar, worked examples, and design notes only — there is no lowerer, emitter, or checker.

Effervesce is a check-only language for describing a system as typed effects (capabilities), how they are provided (`given` / `via`), who consumes them (components), and the rules that must hold (`always` / `never` / `must`). Its purpose is to drive three checkers from one description. This plan adds the lowering/raising path that those checkers will consume; the checkers themselves are a later phase and out of scope here.

## Effect-System Status (read this first)

Effervesce's effect model is **declarative and check-only**, distinct from Koka's executable handlers. There is no `perform`, `resume`, or effect stack. A `capability` is a typed effect declaration. `given E` declares a terminal capability the environment provides. `E via R { ... }` interprets one capability in terms of a row R. `component C needs R { ... }` declares a consumer. The type checker (not built here) will ask whether every needed capability is reachable through `via` edges down to a `given`.

This is *not* the Koka model, so it does not reuse `uir-effect-handler`'s `with`/`resume` semantics. It gets its own nodes.

## Current State

- **Grammar**: Exists and verified. `effervesce/grammar/Effervesce.g4` (173 lines, top rule `system`), authored from scratch earlier. It loads through `gen-and-load` and parses all five examples in `effervesce/examples/*.fizz`. It is brace-delimited (not layout-sensitive), so it uses `gen-and-load`, not `gen-and-load-layout`.
- **Lowering**: None. `lower-effervesce.rkt` does not exist.
- **Emission**: None. `emit-effervesce.rkt` does not exist.
- **UIR**: `uir-effect-decl` / `uir-effect-op` / `uir-effect-handler` exist from the Koka work, but they model Koka's executable handlers, not Effervesce's declarative capability graph. `uir-effect-row` / `uir-effect-entry` exist for the contract-algebra layer and are orthogonal. No UIR node represents a `system`, `capability` (with type parameters), `given`, `via`, `component`, `state`, `action`, `always`, or `never`/`must` policy.

## UIR Gaps

Effervesce needs dedicated nodes. None of these exist today:

- `uir-system` — `(name decls)`. Top-level `system Name { ... }`.
- `uir-capability` — `(name type-params ops)`. A typed effect declaration. `type-params: (listof uir-symbol)`, `ops: (listof uir-capability-op)`.
- `uir-capability-op` — `(name params result-type)`. An operation declared inside a `capability` block.
- `uir-effect-ref` — `(name type-args)`. A reference to a capability, e.g. `Read<Config>` or `Fs`. `type-args: (listof uir-symbol)`.
- `uir-given` — `(row)`. `given Fs, Log`. `row: (listof uir-effect-ref)`.
- `uir-handler` — `(effect row ops)`. `E via R { ... }`. `effect: uir-effect-ref`, `row: (listof uir-effect-ref)`, `ops: (listof (list name params result-type body))`.
- `uir-component` — `(name needs ops)`. `component C needs R { ... }`. `needs: (listof uir-effect-ref)`, ops same quad shape as handler.
- `uir-state` — `(name type init)`. `state cache: Config = expr`.
- `uir-action` — `(name steps)`. `action A { ... }`. `steps: (listof uir)` (each step is `uir-set!` or an effect-invocation expr).
- `uir-invariant` — `(expr)`. The `always expr` rule.
- `uir-policy` — `(kind predicate)`. `kind: 'never | 'must`, `predicate: uir`.

Policy predicates reuse existing expression nodes rather than a new predicate IR: `component needs X` lowers to `uir-call` with symbol callee `"component-needs"` and one `uir-effect-ref` argument; `not`/`and`/`or` lower to `uir-call` with symbol callee `"not"`/`"and"`/`"or"`, exactly like the value-expression operators. The emitter re-sugars those call names back to surface syntax.

## Scope: What We Handle (Pass 1)

- The full decl surface: `system`, `capability`, `given`, `via`, `component`, `state`, `action`, `always`, `never`/`must`.
- Capability type parameters and effect-ref type arguments, round-tripped verbatim.
- Value expressions: literals (`INT`, `STRING`, `true`, `false`, `none`), variables, calls, field access, parenthesization, `not`, and the arithmetic/comparison/logical operators.
- Policy predicates over the description (`component needs X`, `not`, `and`, `or`).
- Op bodies as either a single expression or a `{ ... }` block; op params with optional types.

## Scope: What We Skip (Pass 1)

- The three checkers (type check, model check, policy evaluation). This plan only produces the UIR they will read.
- Execution / code emission to another target (effect-ts, Koka, Rego). Effervesce is check-only.
- Operator precedence reconstruction on emission: the grammar's `expr` rule is flat left-recursive with no precedence tiers, so the emitter prints operators in the order the accumulator produced them, and `and`/`or` print as a binary chain without extra parentheses. Fixtures use simple shapes that round-trip under this flat model.
- Generic `type_` beyond one level of nesting in type arguments (rare in the examples; `Read<Config>` is the common case).

## Module Split Plan

Effervesce's lowerer and emitter are small enough to stay flat in pass 1, matching `lower-koka.rkt`/`emit-koka.rkt` rather than the split `lower-typescript/` tree. Split only when a checker lands and needs shared helpers.

```
racklr/
  lower-effervesce.rkt        # CST → UIR (single file)
  emit-effervesce.rkt         # UIR → text (single file)
racklr-test/
  gen-effervesce-test.rkt     # grammar/tokenizer smoke tests
  lower-effervesce-test.rkt   # round-trip + UIR-shape tests
```

## Phased Implementation

### Phase 1: UIR structs (done next)

Add the eleven structs to `racklr/uir/types.rkt` and wire them in `racklr/uir.rkt` (provide, `uir?`, `uir-tag`, `uir->sexp`, `sexp->uir`, `uir-tag-set`). Re-run `uir-test.rkt`.

### Phase 2: Lowering

Implement `lower-effervesce.rkt` against the dumped CST shapes. Dispatch on the decl node tag; read keywords by token value (all keywords lex as `ID`).

### Phase 3: Emission

Implement `emit-effervesce.rkt` as the inverse. Re-sugar `component-needs` / `not` / `and` / `or` calls and print effect refs with their type args.

### Phase 4: Round-trip tests

Add `gen-effervesce-test.rkt` and `lower-effervesce-test.rkt`. The round-trip harness parses each `examples/*.fizz` fixture, lowers to UIR, emits, re-parses the emitted text, re-lowers, and asserts structural equality of the two UIRs.

## Files Created/Modified

- `docs/plans/effervesce-pipeline.md` — Create (this file)
- `racklr/uir/types.rkt` — Modify (add the eleven Effervesce structs)
- `racklr/uir.rkt` — Modify (wire the structs)
- `racklr/lower-effervesce.rkt` — Create
- `racklr/emit-effervesce.rkt` — Create
- `racklr-test/gen-effervesce-test.rkt` — Create
- `racklr-test/lower-effervesce-test.rkt` — Create

## Design Decisions

1. **Effervesce gets its own UIR nodes, not Koka's.** `uir-effect-handler` means an executable `with`/`resume` handler. Effervesce's `via` is a declarative interpretation edge, and its `component`/`state`/`action`/`policy` decls have no Koka analogue. Conflating them would make the three checkers read Koka-shaped IR.
2. **Effect references are a node, not a string.** `Read<Config>` must be matchable against `Read<A>` by the type checker, so `uir-effect-ref` keeps `name` and `type-args` separate instead of flattening to `"Read<Config>"`.
3. **Policy predicates reuse `uir-call`.** The Clojure/Koka convention: keep surface sugar as call data in UIR, re-sugar only at emit. `component needs X` is `(uir-call "component-needs" ...)`, not a bespoke predicate node.
4. **Op impls are `(name params result-type body)` quads in both handler and component.** A handler op and a component op differ only in what the checker does with them, so one shape serves both.
5. **Flat emission, no precedence table.** Pass 1 emits operators in accumulator order. The grammar has no precedence tiers, so this round-trips the current fixtures; a precedence table is added only if real examples expose a need for it.
6. **The lowerer dispatches on token value for keywords.** Every keyword (`system`, `capability`, `given`, `via`, `component`, `needs`, `always`, `never`, `must`) lexes as an `ID` token, so `tk-type` cannot distinguish them.
