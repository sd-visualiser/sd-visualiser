# Architecture

SD Visualiser is split into three components (Rust crates):
* `sd-core` - contains the main data structures that represent hypergraphs and string diagrams, and parsing logic;
* `sd-graphics` - contains the layout and rendering code, as well as the interface with the LP solver;
* `sd-gui` - [egui](https://egui.rs) UI code.

## `sd-core`

Expressions in `sd-lang` correspond to hypergraphs, for which a data structure is provided.
Operations on these hypergraphs are provided, such as the translation between `sd-lang` expressions and hypergraphs, and some basic graph-like operations (e.g. reachability) are implemented.

Hypergraphs, by nature, quotient out some topological structure of string diagrams, which is convenient when using them to model logical structure (like in `sd-lang`), but inconvenient when trying to render them.
For this reason, in order to render our string diagrams, we first (non-uniquely) translate hypergraphs into terms in some appropriate syntactic monoidal category (which we refer to as a 'monoidal graph'), via an intermediate form of a hypergraph equipped with some ordered layering determined by rank ('monoidal wired graph').
This allows us to explicit represent things like layers, permutations, and caps/cups, which our layout algorithm understands.
Many design decisions made to target 'aesthetically pleasing' string diagram rendering are made at these stages.

## `sd-graphics`

Layout is the process of assigning coordinates to each piece of string diagram, and is obtained by encoding the connectivity and topology of the string diagram as a system of linear equations, and solving it using an LP solver.
Various constraints are used to ensure that the layout is aesthetically pleasing, and that the resulting diagram is easy to read.

TODO(@calintat): add more details and a citation to paper.

From the layout, we can then render each piece of the string diagram in its appropriate position as `egui` shapes, which are to be added to a [`Painter`](https://docs.rs/egui/latest/egui/struct.Painter.html).
Internally, we use our own notion of shape which then gets translated into `egui` [`Shape`](https://docs.rs/egui/latest/egui/enum.Shape.html); our notion has more information, which allows for other output methods (e.g. rendering an SVG instead of painting to `egui`).

This crate also contains some logic which handles interactivity with the string diagram (e.g. toggling thunk visibility on click events).

### LP backends

The crate currently supports two LP backends:
* [`cbc`](https://www.coin-or.org/Cbc/)
* [`minilp`](https://github.com/ztlpn/minilp/)

`cbc` is a much faster solver, but as it is a C++ library, it is difficult to integrate with Rust, particularly when targeting WebAssembly.
`minilp` is a pure Rust solver, and can be compiled straightforwardly with the `wasm32-unknown-unknown` target.

SD Visualiser defaults to using `cbc` when compiling for native targets, and `minilp` when compiling for WebAssembly.

## `sd-gui`

This contains all the ancillary UI code which is not internal to the graph view.
