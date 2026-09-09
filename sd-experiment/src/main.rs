use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};

use anyhow::anyhow;
use csv::Writer;
use itertools::Itertools;
use sd_core::{
    hypergraph::{
        self,
        adapter::collapse::CollapseGraph,
        generic::{Endpoint, Node, Operation, Thunk, Weight},
        mapping::thunk_map,
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
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

fn all_nodes<G: Graph>(graph: &G) -> Vec<Node<G::Ctx>> {
    graph
        .nodes()
        .flat_map(|node| {
            let subtree = match &node {
                Node::Thunk(thunk) => all_nodes(thunk),
                Node::Operation(_) => vec![],
            };
            std::iter::once(node).chain(subtree)
        })
        .collect()
}

fn collect_edges<G: Graph>(
    graph: &G,
    op_names: &HashMap<Operation<G::Ctx>, String>,
    thunk_names: &HashMap<Thunk<G::Ctx>, String>,
) -> BTreeSet<(String, String)> {
    let name = |node: &Node<G::Ctx>| match node {
        Node::Operation(op) => op_names.get(op),
        Node::Thunk(thunk) => thunk_names.get(thunk),
    };
    let endpoint = |endpoint: &Endpoint<G::Ctx>| match endpoint {
        Endpoint::Node(node) => name(node),
        Endpoint::Boundary(Some(thunk)) => thunk_names.get(thunk),
        Endpoint::Boundary(None) => None,
    };
    let nodes = all_nodes(graph);
    nodes
        .iter()
        .flat_map(|node| {
            let outputs = node.outputs().flat_map(|edge| {
                edge.targets()
                    .map(|target| (name(node), endpoint(&target)))
                    .collect_vec()
            });
            let inputs = node
                .inputs()
                .map(|edge| (endpoint(&edge.source()), name(node)));
            // also make virtual edges which represent 'node x inside thunk y'
            let containment = match node {
                Node::Thunk(thunk) => thunk
                    .nodes()
                    .map(|child| (thunk_names.get(thunk), name(&child)))
                    .collect_vec(),
                Node::Operation(_) => vec![],
            };
            outputs.chain(inputs).chain(containment).collect_vec()
        })
        .filter_map(|(source, target)| Some((source?.clone(), target?.clone())))
        .collect()
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
    let filename = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "../examples/llvm-ir/pow.ll".to_owned());
    let code = std::fs::read_to_string(&filename)?;

    let parsed = llvm_ir::parse(&code).map_err(|s| anyhow!(s))?;

    let graph = parsed.to_graph(false)?;

    let mut op_names = HashMap::new();
    let mut thunk_names = HashMap::new();
    for node in all_nodes(&graph) {
        match node {
            Node::Operation(op) => {
                let name = format!("op{}", op_names.len());
                op_names.insert(op, name);
            }
            Node::Thunk(thunk) => {
                let name = format!("thunk{}", thunk_names.len());
                thunk_names.insert(thunk, name);
            }
        }
    }

    let mut nodes_wtr = Writer::from_path("nodes.csv")?;
    nodes_wtr.write_record(["node", "weight"])?;
    for (name, weight) in op_names
        .iter()
        .map(|(op, name)| (name.clone(), op.weight().to_string()))
        .sorted()
    {
        nodes_wtr.write_record([name, weight])?;
    }
    nodes_wtr.flush()?;

    let mut edges_wtr = Writer::from_path("edges.csv")?;
    edges_wtr.write_record(["source", "target"])?;
    for (source, target) in collect_edges(&graph, &op_names, &thunk_names) {
        edges_wtr.write_record([source, target])?;
    }
    edges_wtr.flush()?;

    let expanded = thunk_map(&graph, true);
    let base_graph = CollapseGraph::new(graph, expanded);

    let node_map = calc_node_map(&base_graph);

    let thunk_count = base_graph.expanded().keys().count();
    let mut membership_wtr = Writer::from_path("membership.csv")?;
    membership_wtr.write_record(["event", "node"])?;

    for (i, thunk) in base_graph.expanded().keys().enumerate() {
        println!("Collapsing thunk {}/{thunk_count}", i + 1);
        let mut new_graph = base_graph.clone();
        let thunk_size = thunk.nodes().count();
        let edge_cut_size =
            thunk.number_of_free_graph_inputs() + thunk.number_of_free_graph_outputs();

        new_graph.toggle(thunk);
        let new_node_map = calc_node_map(&new_graph);

        for member in all_nodes(thunk)
            .into_iter()
            .filter_map(Node::into_operation)
            .filter_map(|op| op_names.get(&op))
        {
            membership_wtr.write_record([&i.to_string(), member])?;
        }

        let mut wtr = Writer::from_path(format!("pow-{i}-n={thunk_size}-c={edge_cut_size}.csv"))?;
        wtr.write_record(["old_x", "old_y", "new_x", "new_y", "node"])?;
        for (name, old_x, old_y, new_x, new_y) in new_node_map
            .iter()
            .filter_map(|(op, (new_x, new_y))| {
                let (old_x, old_y) = node_map.get(op)?;
                Some((
                    op_names.get(op)?.clone(),
                    format!("{old_x:.3}"),
                    format!("{old_y:.3}"),
                    format!("{new_x:.3}"),
                    format!("{new_y:.3}"),
                ))
            })
            // make output deterministic
            .sorted()
        {
            wtr.write_record([old_x, old_y, new_x, new_y, name])?;
        }
        wtr.flush()?;
    }
    membership_wtr.flush()?;
    Ok(())
}
