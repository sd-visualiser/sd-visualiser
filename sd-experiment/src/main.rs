use std::{collections::HashMap, fmt::Display};

use anyhow::anyhow;
use csv::Writer;
use sd_core::{
    hypergraph::{
        self,
        adapter::collapse::CollapseGraph,
        generic::{Operation, Weight},
        mapping::thunk_map,
        traits::Graph,
    },
    language::llvm_ir,
    lp::Solver,
    monoidal::{graph::MonoidalGraph, wired_graph::from_graph},
};
use sd_graphics::layout::{self, AtomType, Layout, layout};

fn map_helper<G>(
    layout: &Layout<CollapseGraph<G>>,
    node_map: &mut HashMap<Operation<G::Ctx>, (f32, f32)>,
) where
    G: Graph,
{
    for row in &layout.nodes {
        for node in row {
            match &node.node {
                layout::Node::Atom {
                    h_pos,
                    v_pos,
                    atype: AtomType::Op(n),
                    ..
                } => {
                    if let hypergraph::generic::Node::Operation(op) = n.inner() {
                        node_map.insert(op.clone(), (*h_pos, *v_pos));
                    }
                }
                layout::Node::Thunk { layout, .. } => map_helper(layout, node_map),
                _ => {}
            }
        }
    }
}

fn calc_node_map<G>(graph: &CollapseGraph<G>) -> HashMap<Operation<G::Ctx>, (f32, f32)>
where
    G: Graph,
    Weight<Operation<CollapseGraph<G>>>: Display,
{
    let solver = Solver::Clarabel;
    let monoidal_term = from_graph(graph, solver);
    let monoidal_graph = MonoidalGraph::from(&monoidal_term);
    let layout = layout(&monoidal_graph, solver).unwrap();

    let mut node_map = HashMap::new();
    map_helper(&layout, &mut node_map);
    node_map
}

fn main() -> anyhow::Result<()> {
    let filename = "../examples/llvm-ir/pow.ll";
    let code = std::fs::read_to_string(filename)?;

    let parsed = llvm_ir::parse(&code).map_err(|s| anyhow!(s))?;

    let graph = parsed.to_graph(false)?;
    let expanded = thunk_map(&graph, true);
    let base_graph = CollapseGraph::new(graph, expanded);

    let node_map = calc_node_map(&base_graph);

    let thunk_count = base_graph.expanded().keys().count();

    for (i, thunk) in base_graph.expanded().keys().enumerate() {
        println!("Collapsing thunk {}/{thunk_count}", i + 1);
        let mut new_graph = base_graph.clone();
        new_graph.toggle(thunk);
        let new_node_map = calc_node_map(&new_graph);

        // Generate CSV
        let mut wtr = Writer::from_path(format!("collapsed-{i}.csv"))?;
        wtr.write_record(["old_x", "old_y", "new_x", "new_y"])?;
        for (op, (new_x, new_y)) in new_node_map {
            let (old_x, old_y) = node_map[&op];
            wtr.write_record(&[
                format!("{old_x:.3}"),
                format!("{old_y:.3}"),
                format!("{new_x:.3}"),
                format!("{new_y:.3}"),
            ])?;
        }
        wtr.flush()?;
    }
    Ok(())
}
