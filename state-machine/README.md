# Glassbox — steps 0 and 2

Demo for Marginalia #274 (`echo-charlie-papa-mike`). Spec:
`afc-work/docs/kb/plans/state-machines-that-can-be-drawn.md`.

Yokes the two halves that already existed and had never been joined:

- **`purescript-machines`** (Kmett's, ported) runs the machine — but
  `Mealy i o = Mealy (i -> Tuple (Mealy i o) o)`, so its state is a closure and
  it can never say what states it has.
- **`DataViz.Layout.StateMachine`** in `purescript-hylograph-layout` lays out
  states and transitions — and had nothing to lay out.

Between them sits a **describable transition function**: a plain value that is
*interpreted* into a Mealy and *derived* into a diagram.

## The acceptance test

**The highlighted state is read from the running Mealy.** `State.step` in
`Glassbox.Demo.Component` is only ever written by `stepMealy`; nothing else
assigns it. If it were possible to change what the machine does and leave the
drawing right — or the reverse — the premise of the whole project would be
false.

## What the demo shows

| Claim | Where to see it |
|---|---|
| One value, run and drawn | Press a footswitch; the live state lights up |
| Configuration *reshapes* the machine | Switch between 0 / 2 / 4 beat count-in and watch Empty→Armed→Recording collapse to Empty→Recording, leaving Armed unreachable |
| A state that resolves by itself | Record with a count-in, then tick: "starts in 4 beats" counts down and Recording arrives with no user event |
| Refusal with a reason | Press play from Empty, or tick "loop 3 holds the converter" and try to record |
| Faithfulness cross-check | The Mermaid pane is the same description for an independent renderer |
| **The tree the user is forced to build** | "as the user's tree" — the spine is heavy, the ways back to Empty are faint, and the lateral jumps between Stopped/Playing/Overdub are gold and dotted |
| **A lint, from the same value** | Set a 0-beat count-in: Armed is stranded in the band below the tree, drawn pale, and the Shape panel reports `unreachable — armed` |

## Run it

```bash
# from purescript-hylograph-demos/ — NOT from this directory: spago resolves
# extraPackages paths relative to CWD
spago bundle -p hylograph-demo-state-machine
cd state-machine/public && npx http-server . -p 3071 -c-1 --cors
```

Not registered with Bosun yet; that is a `:3022` step if it ever wants a fleet port.

## Modules

| Module | Role |
|---|---|
| `Glassbox.Machine` | the describable value: `Machine env cfg state event deadline`, `Outcome`, `Pending` |
| `Glassbox.Interpret` | reading one — `toMealy` via `unfoldMealy` |
| `Glassbox.Describe` | reading two — the diagram, **derived** by running `transition` over states × events |
| `Glassbox.Codec.Mermaid` | reading two, again, for an independent renderer |
| `Glassbox.Tree` | directed BFS from the initial state; the induced tree, and every other edge classified as back / forward / cross / self / from-unreachable |
| `Glassbox.Layout.Tree` | a third positioning strategy for `layoutWithConfig`, built on `Data.Graph.Layout.treeLayout` |
| `Glassbox.Demo.Loop` | one loop of the six-loop looper |
| `Glassbox.Demo.Render` | HATS SVG |
| `Glassbox.Demo.Component` | Halogen |

## Things learned building it

**`describe` is derived, not declared.** The spec sketched a `describe` field on
`Machine` sitting alongside `transition`. That would have been two sources for
one truth. Enumerating `states × events` and asking `transition` is the stronger
form, and it is why `Machine` carries `states` and `events` at all.

**`Pending` names the event, not the destination.** `{ fires, at }`, not
`{ becomes, at }`. If pending named the target state, it and `transition` could
disagree about what a deadline does, and the diagram would draw one answer while
the runtime took the other. Naming the event keeps `transition` the only
authority on where anything goes.

**`env` and `cfg` are per-step inputs, not construction-time captures.** The
grid and the converter's occupant genuinely change between presses, and a
Mealy built this way carries state and nothing else — which is the point.

**`enteredAt` belongs in `env`.** A deadline is a function of when the current
state was entered. Folding that into the state would make Armed-at-beat-4 and
Armed-at-beat-8 distinct states and destroy the enumerability the drawing
depends on.

**Halogen and HATS must not own the same DOM node.** An earlier version rendered
the diagram into a Halogen-declared `div` and called `clearContainer` on it
before each redraw; after that the component silently stopped updating — no
console error, just a dead UI. The diagram container is now declared in
`index.html` and Halogen mounts only into `#glassbox-controls`.

**HATS joins by position, so child counts must be constant.** Emitting the
live-state halo only for the live state shifted every subsequent element and
painted one state's attributes onto its neighbour — a diagram disagreeing with
the machine for reasons that had nothing to do with the machine. The halo is
always emitted and merely invisible when idle.

## Upstream change

`DataViz.Layout.StateMachine.Types.Transition` gained an `extra` parameter,
mirroring `State extra`, so a renderer can style an edge by what it *is* (event,
deadline, refusal) without a side table keyed by `from`/`to`/`label`.
`StateMachine`, `LayoutTransition` and `StateMachineLayout` are now parameterised
by both payloads. All 19 golden suites pass unchanged — the field is purely
additive to the geometry.

`publish.version` there is deliberately still `0.3.0`: bumping it to `0.4.0`
would break every sibling demo's `>=0.3.0 <0.4.0` range. **Bump it and update
those ranges at publish time.**


## Step 2 — the tree, and what it revealed

### `Data.Graph.Pathfinding.bfs` could not be used, and the reason matters

It looked like exactly the right function — its `SearchResult` returns
`parents`, which is the search tree. But `Data.Graph.Types.buildAdjacency`
inserts every edge in **both** directions, so that `Graph` is undirected and so
is any search over it. A navigation graph is directed, and the most useful
question about one — *in by one press, out by four* — is meaningless without
direction.

So `Glassbox.Tree` carries its own directed BFS. **A directed BFS that returns
its search tree is a real gap in `hylograph-graph`**, and a good candidate to
move there once Site Explorer's route graph wants it — a website's link graph is
directed too.

### `TidyDag` has the right idea and the wrong policy

`DataViz.Layout.TidyDag` already classifies edges as `TreeLink | CrossLink`,
which is the shape this needs. Two things stop it being usable as-is, and both
are semantic rather than plumbing:

- **Root selection.** `effectiveRoots` is *nodes with no incoming edges*, and the
  `roots` argument only guarantees isolated nodes join `allIds`. Every state in a
  looper machine has an inbound edge, so `effectiveRoots` comes out **empty**.
  There is no way to say "root this at Empty", which is the one thing a state
  machine needs.
- **Depth metric.** Longest path from the roots — correct for a build DAG, where
  a target's depth is its critical path; wrong for navigation, where the user's
  depth is the *fewest* presses.

Its live callers (Levantine's views and tests, minard-for-nix) are genuine build
DAGs where the current policy is right, so this was left alone rather than
changed underneath them.

`Data.Graph.Layout.treeLayout` takes an explicit root and is polymorphic in the
node type, so it does the positioning; the classification is done here, and goes
four ways rather than two because that is where the signal is:

| class | meaning |
|---|---|
| **tree** | how you first arrive — the spine of the mental model |
| **back** | the way out; its *absence* is a trap |
| **forward** | skips a level — a shortcut, directness bought against taxonomy |
| **cross** | a jump into another branch — the interlevel transition that makes where-you-are impossible to reconstruct from how-you-got-here |
| **self** | goes nowhere |
| **from-unreachable** | leaves a state home cannot reach |

### `layoutWithConfig` was the right extension point

`layoutWithConfig :: LayoutConfig -> (LayoutConfig -> Array (State se) -> Array (LayoutState se)) -> ...`
takes the positioning pass as an argument. `circularLayout` and `gridLayout` are
the two shipped strategies; the tree is a third, and no fork of the layout module
was needed to add it — only the curvature change below.

### Second upstream change

`LayoutConfig` gained `edgeCurvature :: Number` (default `0.15`, preserving
current behaviour). A ring needs its parallel chords bowed apart; a tidy tree
wants its parent-child links nearly straight, and the ring's bow was throwing the
labels on top of each other. Tree mode uses `0.04`. All 19 golden suites pass.

### Unreachable states are parked, not dropped

`treeLayout` only returns what the root reaches. A state home cannot reach is a
*finding*, so `Glassbox.Layout.Tree` puts the leftovers in a band below the tree
rather than omitting them — a layout that silently dropped them would be hiding
the most useful thing on the page.

### The classification is already the first lint

The Shape panel is not decoration: counts by edge class, the named unreachable
states, and the worst return asymmetry (from a second BFS over the reversed
edges) all come from the same value the diagram is drawn from. That was supposed
to be step 3.

## Arrows and labels — two upstream bugs, and what still needs work

The node layout landed first; the edges needed the layout module's own
unfinished machinery finishing.

**`countParallelTransitions` was stubbed and `arcPath` ignored its result.**
The signature took an `Int` offset and bound it as `_offset`; the counter
returned `0` unconditionally with the comment "Simplified for now". So nothing
ever separated two edges sharing a pair of states, and every collision in the
first tree drawing was an antiparallel pair — Empty↔Armed, Stopped↔Playing,
Playing↔Overdub — collapsed onto one line with their labels on top of each
other. It had been invisible in circular layouts because a generous bow
(`edgeCurvature` 0.15) hid it; flattening the curve for the tree exposed it.

Now `parallelInfo` computes, per transition, its index among same-direction
siblings and whether the reverse edge exists. The two cases need opposite
treatment, which is probably why it was left undone: same-direction siblings fan
out to *different* offsets, while an antiparallel pair already bows to opposite
sides (the perpendicular is derived from the travel direction, which is
reversed) and needs the same positive magnitude, merely *floored* so the gap
survives a flat base curve. Hence `parallelSeparation` and
`minPairedCurvature`.

**`computeBounds` measured only the state ellipses.** The viewBox was pinned to
`0 0 width height` with `width = maxX + margin`, so anything left of or above
the leftmost state was clipped — which is exactly where self-loops bulge and
where their labels sit. `StateMachineLayout` now carries `originX`/`originY` and
the bounds are computed over states, arc endpoints, control points, label
anchors and the initial arrow.

Both changes were verified against the goldens: **every state position and every
transition path is byte-identical**; only the bounding box moved, and it grew —
most in the self-loop fixture (480 → 562 wide), which is the one that was
clipping. Goldens regenerated deliberately via `UPDATE_GOLDEN=1`.

Labels also now sit off their arc along the perpendicular (`labelOffset`)
instead of a fixed 8px upward nudge, so a label no longer lies on the line it
names.

### Still to do

The remaining crowding is a different problem from parallel edges: **edges with
*different* endpoints whose paths converge on the same region**. Three "clear"
back edges arrive at Empty from the right, and near Armed the Recording→Empty
label collides with the deadline's "after count-in". Nothing currently knows
these edges are crowding, because they share no endpoints.

Candidate fixes, roughly in order of value:

- **Stagger label position along the arc.** Labels are pinned at the control
  point, i.e. the midpoint. Placing them at a fraction that varies per edge
  would decorrelate converging bundles cheaply.
- **Label collision detection.** A post-pass that measures label boxes and
  nudges them apart is the honest fix and the one that generalises.
- **Route back edges as a bundle.** hylograph-layout already has an edge-bundle
  layout; the back edges to home are a natural bundle and drawing them as one
  would remove most of the clutter at the root.
- **Arrowheads stack** where several edges converge on one node from similar
  angles.

## Hover: sidestepping the crowding instead of solving it

Edge and label crowding is a hard layout problem, and this library is probably
not where it should be solved. Hover is a cheaper answer that changes the
requirement rather than meeting it.

**Crowding is a problem of *simultaneous* legibility.** If the reader can pull
one state's story out of the tangle on demand, the static drawing only has to be
*navigable*, not perfect — and navigable is far cheaper to achieve.

Hovering a state lights two things and dims everything else to 6%:

1. **Its route from home**, straight out of the induced tree. This is the
   distinctive half: it answers the question a user actually has — *how do I get
   here* — and no amount of edge routing would have made that legible in a
   crowded picture, because the route is a property of the tree rather than of
   the drawing.
2. **Its incident edges**, in and out: what you can do once you arrive, and what
   else leads here.

The Shape panel follows the pointer too, so hovering reports that state's cost
in and out rather than the current one's.

### Mechanics

HATS handlers are plain `Effect Unit` (`onMouseEnter :: Effect Unit ->
ThunkedBehavior`), so hover is posted back into Halogen through an
`HS.create` emitter subscribed at `Initialize`, with the listener kept in
component state. `SetHover` ignores same-target notifications, so moving the
mouse within one node does not repaint the diagram.

Each state is wrapped in a `Group` carrying the fade, with the behaviours
attached to the group, so the whole node is the hover target rather than just
its outline.

### What hover does not fix

It is an *interactive* affordance. A printed or exported diagram has none, and
some of the eventual audience for this — an MC6 layout on a card by the
pedalboard — is exactly that. Hover makes the **tool** legible; it does not make
the **artifact** legible. Static layout quality still matters for anything that
leaves the screen.
