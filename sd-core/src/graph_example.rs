use itertools::Itertools;

use crate::{
    graph::SyntaxHyperGraph,
    hypergraph::{Fragment, Graph, HyperGraph},
    language::spartan::Op,
};

#[must_use]
pub fn cyclic() -> SyntaxHyperGraph {
    let mut builder = HyperGraph::new(vec![None], 1);

    let node = builder.add_operation(2, vec![None], Op::Plus);
    let (input_1, input_2) = node.inputs().collect_tuple().unwrap();
    let output = node.outputs().next().unwrap();

    let graph_input = builder.graph_inputs().next().unwrap();
    builder.link(graph_input, input_1).unwrap();

    let graph_output = builder.graph_outputs().next().unwrap();
    builder.link(output.clone(), graph_output).unwrap();

    builder.link(output, input_2).unwrap();

    builder.build().unwrap()
}

#[must_use]
pub fn cyclic_fat() -> SyntaxHyperGraph {
    let mut builder = HyperGraph::new(vec![None], 1);

    let node = builder.add_operation(4, vec![None], Op::Plus);
    let (input_1, input_2, input_3, input_4) = node.inputs().collect_tuple().unwrap();
    let output = node.outputs().next().unwrap();

    let graph_input = builder.graph_inputs().next().unwrap();
    builder.link(graph_input, input_1).unwrap();

    let graph_input = builder.graph_inputs().next().unwrap();
    builder.link(graph_input, input_3).unwrap();

    let graph_input = builder.graph_inputs().next().unwrap();
    builder.link(graph_input, input_4).unwrap();

    let graph_output = builder.graph_outputs().next().unwrap();
    builder.link(output.clone(), graph_output).unwrap();

    builder.link(output, input_2).unwrap();

    builder.build().unwrap()
}
