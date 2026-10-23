# Plan: The Dark Factory Verification Library

## Overview

Turn the "dark factory" into a small library that an agent loop (or a human)
can use to verify system design requirements and do black-box programming.

The goal is not a bigger Effervesce. The goal is one stupid-simple surface
that answers questions about a software system's capabilities and side effects
with a confidence contract, fast. Fast enough that System 1 of an agent loop
can call it in a hot path, and precise enough that System 2 can escalate the
same question to a proof when the stakes demand it.

## Working definition of terms (assumptions, correct me if wrong)

- **Dark factory**: the autonomous, lights-out loop that drives the work. It
  has two reasoning modes. System 1 is fast, closed-form, and approximate.
  System 2 is slow, exact, and on demand. The library's job is to give both
  modes the same substrate and the same questions.
- **System design requirements**: declarative statements of what a component
  needs, provides, and must not do. Today these are `.fizz` specs checked by
  Effervesce. The library makes them queryable without running a checker.
- **Black-box programming**: verifying an artifact by its observable interface
  and recovered effects, not by reading its internals. Source is one artifact;
  a compiled binary is the same kind of artifact through a different front end
  (BAP). Both project into the same graph, so both answer the same questions.

## The library shape

Three verbs, nothing else in the public surface:

- `(analyze artifact)` -> graph. Parse a `.fizz` spec, host source code, or a
  binary into the shared typed-hypergraph projection.
- `(ask graph question)` -> answer plus confidence. Closed-form graph queries
  with a confidence tag: `certain`, `unknown`, or (later) `probable`.
- `(verify spec artifact)` -> verdict. Run the exact checkers and report
  structured findings, the existing Effervesce `check()` promoted to the same
  surface.

Everything downstream - agents, CLIs, future model-checking - talks through
these three. The internals stay free to change.

## Architecture

```
.fizz spec ──► Effervesce lower ──► uir-eff-system ──┐
host source ─► racklr front end ─► host UIR ────────┼──► hypergraph ──► index
binary ──────► BAP adapter ─────► recovered facts ──┘        │
                                                              │
                          System 1: ask (index + memoized closures)
                          System 2: verify (type/policy/model/bridge checkers)
```

The graph is a derived, queryable view. It does not replace the UIR structs or
the checkers; the checkers keep working on structs. The graph exists for query
speed and to give every front end one common vocabulary.

Nodes: component, capability, effect, state, action, resource (scope).
Links: component-needs, component-provides, capability-interprets,
action-writes, fn-effects, effect-touches.

The capability-interpretation table in the bridge checker is promoted from a
plain hash to `capability-interprets` edges, so it can be queried and
traversed instead of only consulted.

## The confidence contract

- System 1 answers are `certain` when they come from a closed-form index lookup
  or a memoized transitive closure, and `unknown` otherwise.
- System 1 never guesses. It says `unknown` and lets the caller escalate.
- System 2 (`verify`) answers are exact: a finding or a discharged invariant.
- The escalation rule is the System 1 / System 2 split, stated literally: the
  loop calls fast first and escalates only when the answer is `unknown` or the
  stakes require a proof.

## Phases

### Phase 1: Materialize the graph

New `effervesce/graph.rkt`: project `uir-eff-system` plus host UIR into
`racklr/hypergraph.rkt` nodes and links, using the checker's existing
`ref-key` for capability identity so graph and checkers agree. Add an inverted
index (`hg-index`) over `hg-match?` so name and type lookups are O(1), not
linear scans. Pure data over the existing `.fizz` examples.

Acceptance: a `graph-test.rkt` that loads the examples and asserts the node and
link sets, no checker required.

### Phase 2: System 1 ask

New `effervesce/query.rkt`: closed-form questions over the index plus memoized
transitive closures - `capabilities-of`, `effects-of`, `side-effects-of`,
`touches` (who reads or writes a scope, the AtomSpace DualLink lesson), and
`reachable?`. Every answer carries confidence. Extend `main.rkt` with an `ask`
subcommand so the loop gets `effervesce ask <spec> <question>` in milliseconds.

Acceptance: `query-test.rkt` answers the example questions correctly and tags
confidence; `effervesce ask` runs end to end.

### Phase 3: System 2 verify

Expose the four checkers behind `verify` with an escalation flag. Fast mode
returns the index answer; exact mode runs type/policy/model/bridge and returns
structured findings. The flagship exact query is proof-shaped: no path from
untrusted input reaches a write, expressed as model-checking the invariant
(exists today) plus scope reachability over the Phase 1 edges (new).

Acceptance: `verify` returns the same findings as `check()` today, and the
escalation path is one function call, not a rewrite of the checkers.

### Phase 4: BAP as a binary front end

Consume BAP as an external process (`bap ... --dump json`), not an FFI. Map
recovered facts into the existing vocabulary, starting with the syscall set
that maps cleanly: network (socket/connect/sendto), fs (open/read/write),
process (fork/exec), env, time. Source-only effects (llm/db/cache) stay
source-only, and the graph is honest about coverage. Two products: `analyze`
on a binary producing the same graph, and a drift check that asks whether a
shipped binary's recovered effects violate its declared capabilities.

Acceptance: a spike proves BAP installs and emits parseable JSON for one tiny
binary before any adapter code is committed.

### Phase 5: Package as a library

One entry module with the three verbs, a README that is a worked example (analyze
a system, ask its side effects, verify a requirement), and the CLI as a thin
wrapper. No new dependencies beyond what already ships.

Acceptance: a fresh caller can `(require effervesce)` and run analyze/ask/verify
reading only the README.

## Non-goals

- Do not replace the UIR structs or the checkers with the hypergraph. It is a
  derived view, not a new primary representation.
- Do not FFI BAP from Racket. JSON over CLI is the seam.
- Do not promise one unbounded query over "the entire system". Scope per
  artifact, compose through the capability graph.

## Open questions

- Is `probable` worth modeling now (weights, BAP confidence), or do we ship
  with a strict certain/unknown contract first?
- Should `touches` scope be per-resource-string, or do scopes need a lattice of
  their own before BAP lands?
- Where does the drift check live: inside the library, or as a separate
  verifier that consumes the library?

## Files created

- `docs/plans/dark-factory-library.md` - this file
- `effervesce/graph.rkt` - Phase 1
- `effervesce/query.rkt` - Phase 2
- `effervesce/bap.rkt` - Phase 4 (after the spike)
- `effervesce/verify.rkt` - Phase 3/5 entry surface
- `effervesce-test/graph-test.rkt`, `query-test.rkt` - tests
