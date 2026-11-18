# Hylograph Library Demos

Interactive demos for the [Hylograph](https://hylograph.net) visualization ecosystem.

**Live:** [afcondon.github.io/purescript-hylograph-demos](https://afcondon.github.io/purescript-hylograph-demos/)

| Demo | Library | Description |
|------|---------|-------------|
| [graph](graph/) | hylograph-d3-kernel | Honeycomb Puzzle — breadth-first path search on hexagonal graphs, laid out by force simulation. It carries its own BFS and does not call hylograph-graph. |
| [graph-decomposition](graph-decomposition/) | hylograph-graph | Graph Decomposition Explorer — biconnected components, articulation points, chimera visualization |
| [layout](layout/) | hylograph-layout | Layout Gallery — layout algorithms (sankey, treemap, bin-pack, masonry, swimlane, etc.) |
| [layout-components](layout-components/) | hylograph-layout-components | Chart components whose geometry comes from a layout algorithm (Sankey so far) |
| [components](components/) | hylograph-components | Component Gallery — ready-made chart components |
| [music](music/) | hylograph-music | Anscombe's String Quartet — data sonification, through the library's Web Audio bindings; drawn with Halogen SVG, not HATS |
| [selection](selection/) | hylograph-selection | The Hylographic Fold — 7-chapter interactive guide to declarative DOM tree construction |
| [simulation](simulation/) | hylograph-simulation | Force Playground — interactive force-directed graph simulation (force calculations by d3-force) |
| [onion](onion/) | hylograph-onion | Watercolour and pen-plotter finishes — catalogue, recipes, Les Mis |
| [sigil](sigil/) | sigil | Sigil demo — typeset PureScript type signatures |
| [sigil-hats](sigil-hats/) | sigil-hats | HATS ClassGrid — type class hierarchy visualization |
| [state-machine](state-machine/) | glassbox | Glassbox — a state machine as a data artifact, drawn with HATS |
| [baskerville-make](baskerville-make/) | baskerville × hylograph | `make`, deconstructed — a real Makefile as a Sankey |
| [baskerville-sudoku](baskerville-sudoku/) | baskerville × hylograph | Explainable Sudoku |

## Prerequisites

- [Node.js](https://nodejs.org/) >= 18
- [PureScript](https://github.com/purescript/purescript/releases) >= 0.15.15
- [Spago](https://github.com/purescript/spago) >= 0.93

The force-simulation demos (graph, simulation, sigil-hats, onion) bundle
`d3-force`, `d3-selection` and `d3-drag`, which the simulation libraries import.
Install them once, before bundling:

```bash
npm install
```

Several demos depend on sibling checkouts by path (see `spago.yaml`), so this
repository does not build from a fresh clone on its own.

## Build

```bash
# Build all demos
spago build

# Bundle all demos (creates public/bundle.js in each)
make all

# Build and bundle a single demo
spago bundle -p hylograph-demo-graph
```

## Run locally

After bundling, open any demo's `public/index.html` in a browser, or serve with:

```bash
cd graph/public && python3 -m http.server 8080
```

## Deploy to GitHub Pages

```bash
make all    # bundle all demos
make site   # assemble docs/ directory
git add docs/ && git commit -m "Update site" && git push
```

GitHub Pages serves from `docs/` on `main`.
