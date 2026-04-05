# Hylograph Library Demos

Interactive demos for the [Hylograph](https://hylograph.net) visualization ecosystem.

**Live:** [afcondon.github.io/purescript-hylograph-demos](https://afcondon.github.io/purescript-hylograph-demos/)

| Demo | Library | Description |
|------|---------|-------------|
| [graph](graph/) | hylograph-graph | Honeycomb Puzzle — A* pathfinding on hexagonal graphs |
| [graph-decomposition](graph-decomposition/) | hylograph-graph | Graph Decomposition Explorer — biconnected components, articulation points, chimera visualization |
| [layout](layout/) | hylograph-layout | Layout Gallery — 15+ layout algorithms (sankey, treemap, bin-pack, masonry, swimlane, etc.) |
| [music](music/) | hylograph-music | Anscombe's String Quartet — data sonification |
| [selection](selection/) | hylograph-selection | HATS Explorer — 7-chapter interactive guide to declarative DOM tree construction |
| [simulation](simulation/) | hylograph-simulation | Force Playground — interactive force-directed graph simulation |
| [sigil](sigil/) | sigil | Sigil demo — type-safe DOM elements |
| [sigil-hats](sigil-hats/) | sigil-hats | HATS ClassGrid — type class hierarchy visualization |

## Prerequisites

- [Node.js](https://nodejs.org/) >= 18
- [PureScript](https://github.com/purescript/purescript/releases) >= 0.15.15
- [Spago](https://github.com/purescript/spago) >= 0.93

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
