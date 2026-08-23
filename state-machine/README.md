# Glassbox — a machine is a file

Phase 1 of [`docs/kb/plans/the-same-machine.md`](../../../docs/kb/plans/the-same-machine.md).

A state machine here is **data loaded at runtime**, not code. `public/machines/`
holds the artifacts; nothing in `src/` knows what any of them are.

## The acceptance test

> A new machine requires zero lines of PureScript.

`public/machines/car-radio.json` is the proof. No constructor names its states,
no module was recompiled to add it, and nothing in this package knows what a car
radio is — yet it loads, runs, refuses, keeps a deadline, and draws. If that ever
stops being true, the thesis has failed and everything downstream is decoration.

## Modules

| Module | What it is |
|---|---|
| `Glassbox.Spec` | the decoded machine — all data, no functions |
| `Glassbox.Codec.JSON` | the wire format: one bidirectional codec, round-trip tested |
| `Glassbox.Run` | the interpreter: a pure function of (world, state, event) |
| `Glassbox.Describe` | derives the drawing by resolving rules under a world |
| `Glassbox.Tree` | the shortest-path tree a user is forced to build, and its edge classes |
| `Glassbox.Layout.Tree` | a tree strategy for `DataViz.Layout.StateMachine` |
| `Glassbox.Export.StateDiagram` | one-way text export for a free eyeball check. Emits Mermaid's `stateDiagram-v2` dialect because things render it for free — but imports nothing and draws nothing; the picture is ours. |
| `Glassbox.Demo.*` | the inspector — generic; it names no machine |

## The boundary

The codec is treated exactly as an FFI boundary: narrow, explicit, versioned,
checked rather than trusted, loud on failure. Making the machine data costs the
compile-time exhaustiveness a `case` expression gave us; that is bought back by
lints at the load boundary (phase 2 — not yet written, which is the honest state
of this package today).

## Running it

```sh
spago test   -p hylograph-demo-state-machine    # 21 acceptance tests
spago bundle -p hylograph-demo-state-machine
cd state-machine/public && npx http-server . -p 3117 -c-1
```
