use std::{
    fmt::{Debug, Display},
    ops::{Bound, Range},
};

use derivative::Derivative;
use egui::Vec2;
use good_lp::{variable, Expression, ResolutionError, Solution, Variable};
use itertools::Itertools;
use ordered_float::OrderedFloat;
use sd_core::{
    common::{InOut, InOutIter},
    hypergraph::{
        generic::{Ctx, Weight},
        traits::{Graph, NodeLike, WithWeight},
    },
    lp::LpProblem,
    monoidal::graph::{MonoidalGraph, MonoidalOp},
};
#[cfg(test)]
use serde::Serialize;
use store_interval_tree::{Interval, IntervalTree};
use thiserror::Error;
use tracing::debug;

use crate::common::RADIUS_OPERATION;

#[derive(Clone, Debug, Error)]
pub enum LayoutError {
    #[error("An error occurred when solving the problem: {0}")]
    ResolutionError(#[from] ResolutionError),
}

#[derive(Clone, Derivative)]
#[derivative(Debug(bound = "T::Operation: Debug, T::Thunk: Debug, H: Debug, V: Debug"))]
#[cfg_attr(test, derive(Serialize), serde(bound = "H: Serialize, V: Serialize"))]
pub struct NodeOffset<T: Ctx, H, V> {
    pub(crate) node: Node<T, H, V>,
    pub(crate) inputs: Range<usize>,
    pub(crate) outputs: Range<usize>,
}

#[derive(Clone, Derivative)]
#[derivative(Debug(bound = "T::Operation: Debug, T::Thunk: Debug"))]
pub enum AtomType<T: Ctx> {
    Cup,
    Cap,
    Op(T::Operation),
    Copy,
    Id,
}

#[derive(Clone, Derivative)]
#[derivative(Debug(bound = "T::Operation: Debug, T::Thunk: Debug, H: Debug, V: Debug"))]
#[cfg_attr(test, derive(Serialize), serde(bound = "H: Serialize, V: Serialize"))]
pub enum Node<T: Ctx, H, V> {
    Atom {
        h_pos: H,
        v_pos: V,
        extra_size: f32,
        #[cfg_attr(test, serde(skip_serializing))]
        atype: AtomType<T>,
    },
    Swap {
        h_pos: H,
        v_top: V,
        v_bot: V,
        out_to_in: Vec<usize>,
    },
    Thunk {
        #[cfg_attr(test, serde(skip_serializing))]
        addr: T::Thunk,
        layout: LayoutInternal<T, H, V>,
        inputs: Vec<H>,
        outputs: Vec<H>,
    },
}

impl<T: Ctx, V> Node<T, Variable, V> {
    #[must_use]
    pub fn h_min(&self) -> Expression {
        match self {
            Self::Atom {
                h_pos: pos,
                extra_size,
                ..
            } => *pos - *extra_size,
            Self::Swap { h_pos: pos, .. } => (*pos).into(),
            Self::Thunk { layout, .. } => layout.h_min.into(),
        }
    }

    #[must_use]
    pub fn h_max(&self) -> Expression {
        match self {
            Self::Atom {
                h_pos: pos,
                extra_size,
                ..
            } => *pos + *extra_size,
            Self::Swap { h_pos: pos, .. } => (*pos).into(),
            Self::Thunk { layout, .. } => layout.h_max.into(),
        }
    }
}

impl<T: Ctx, H, V> Node<T, H, V> {
    pub fn unwrap_atom(&self) -> &H {
        match self {
            Self::Atom { h_pos, .. } | Self::Swap { h_pos, .. } => h_pos,
            Self::Thunk { .. } => panic!(),
        }
    }

    pub fn unwrap_thunk(&self) -> &LayoutInternal<T, H, V> {
        match self {
            Self::Atom { .. } | Self::Swap { .. } => panic!(),
            Self::Thunk { layout, .. } => layout,
        }
    }
}

impl<T: Ctx, H, V> InOut for NodeOffset<T, H, V> {
    fn number_of_inputs(&self) -> usize {
        self.inputs.len()
    }

    fn number_of_outputs(&self) -> usize {
        self.outputs.len()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = "H: Clone, V: Clone"),
    Debug(bound = "T::Edge: Debug, H: Debug, V: Debug")
)]
#[cfg_attr(test, derive(Serialize), serde(bound = "H: Serialize, V: Serialize"))]
pub struct WireData<T: Ctx, H, V> {
    pub h: H,
    pub v_min: V,
    pub v_max: V,
    #[cfg_attr(test, serde(skip_serializing))]
    pub addr: T::Edge,
}

#[derive(Clone, Derivative)]
#[derivative(Debug(
    bound = "T::Operation: Debug, T::Thunk: Debug, T::Edge: Debug, H: Debug, V: Debug"
))]
#[cfg_attr(test, derive(Serialize), serde(bound = "H: Serialize, V: Serialize"))]
pub struct LayoutInternal<T: Ctx, H, V> {
    pub h_min: H,
    pub h_max: H,
    pub v_min: V,
    pub v_max: V,
    pub nodes: Vec<Vec<NodeOffset<T, H, V>>>,
    pub wires: Vec<Vec<WireData<T, H, V>>>,
}

impl<T: Ctx, H, V> LayoutInternal<T, H, V> {
    pub fn input_wires(&self) -> impl DoubleEndedIterator<Item = &WireData<T, H, V>> {
        self.wires.first().unwrap().iter()
    }

    pub fn output_wires(&self) -> impl DoubleEndedIterator<Item = &WireData<T, H, V>> {
        self.wires.last().unwrap().iter()
    }

    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = &H> {
        self.input_wires().map(|x| &x.h)
    }

    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = &H> {
        self.output_wires().map(|x| &x.h)
    }
}

pub type HLayout<T, S> = LayoutInternal<T, f32, S>;
pub type Layout<T> = LayoutInternal<T, f32, f32>;

impl<T: Ctx, S> HLayout<T, S> {
    #[must_use]
    pub fn width(&self) -> f32 {
        self.h_max - self.h_min
    }

    #[allow(clippy::cast_possible_truncation)]
    fn from_solution_h(layout: LayoutInternal<T, Variable, S>, solution: &impl Solution) -> Self {
        HLayout {
            h_min: solution.value(layout.h_min) as f32,
            h_max: solution.value(layout.h_max) as f32,
            v_min: layout.v_min,
            v_max: layout.v_max,
            nodes: layout
                .nodes
                .into_iter()
                .map(|ns| {
                    ns.into_iter()
                        .map(|n| NodeOffset {
                            node: match n.node {
                                Node::Atom {
                                    h_pos,
                                    v_pos,
                                    extra_size,
                                    atype,
                                } => Node::Atom {
                                    h_pos: (solution.value(h_pos) as f32),
                                    v_pos,
                                    extra_size,
                                    atype,
                                },
                                Node::Swap {
                                    h_pos,
                                    v_top,
                                    v_bot,
                                    out_to_in,
                                } => Node::Swap {
                                    h_pos: solution.value(h_pos) as f32,
                                    v_top,
                                    v_bot,
                                    out_to_in,
                                },
                                Node::Thunk {
                                    addr,
                                    layout,
                                    inputs,
                                    outputs,
                                } => Node::Thunk {
                                    addr,
                                    layout: Self::from_solution_h(layout, solution),
                                    inputs: inputs
                                        .into_iter()
                                        .map(|x| solution.value(x) as f32)
                                        .collect(),
                                    outputs: outputs
                                        .into_iter()
                                        .map(|x| solution.value(x) as f32)
                                        .collect(),
                                },
                            },
                            inputs: n.inputs,
                            outputs: n.outputs,
                        })
                        .collect()
                })
                .collect(),
            wires: layout
                .wires
                .into_iter()
                .map(|vs| {
                    vs.into_iter()
                        .map(
                            |WireData {
                                 h,
                                 v_min,
                                 v_max,
                                 addr,
                             }| WireData {
                                h: solution.value(h) as f32,
                                v_min,
                                v_max,
                                addr,
                            },
                        )
                        .collect()
                })
                .collect(),
        }
    }
}

impl<T: Ctx> Layout<T> {
    #[must_use]
    pub fn height(&self) -> f32 {
        self.v_max - self.v_min
    }

    #[must_use]
    pub fn size(&self) -> Vec2 {
        Vec2::new(self.width(), self.height())
    }

    #[allow(clippy::cast_possible_truncation)]
    fn from_solution_v(layout: LayoutInternal<T, f32, Variable>, solution: &impl Solution) -> Self {
        Layout {
            h_min: layout.h_min,
            h_max: layout.h_max,
            v_min: solution.value(layout.v_min) as f32,
            v_max: solution.value(layout.v_max) as f32,
            nodes: layout
                .nodes
                .into_iter()
                .map(|ns| {
                    ns.into_iter()
                        .map(|n| NodeOffset {
                            node: match n.node {
                                Node::Atom {
                                    h_pos,
                                    v_pos,
                                    extra_size,
                                    atype,
                                } => Node::Atom {
                                    h_pos,
                                    v_pos: solution.value(v_pos) as f32,
                                    extra_size,
                                    atype,
                                },
                                Node::Swap {
                                    h_pos,
                                    v_top,
                                    v_bot,
                                    out_to_in,
                                } => Node::Swap {
                                    h_pos,
                                    v_top: solution.value(v_top) as f32,
                                    v_bot: solution.value(v_bot) as f32,
                                    out_to_in,
                                },
                                Node::Thunk {
                                    addr,
                                    layout,
                                    inputs,
                                    outputs,
                                } => Node::Thunk {
                                    addr,
                                    layout: Layout::from_solution_v(layout, solution),
                                    inputs,
                                    outputs,
                                },
                            },
                            inputs: n.inputs,
                            outputs: n.outputs,
                        })
                        .collect()
                })
                .collect(),
            wires: layout
                .wires
                .into_iter()
                .map(|vs| {
                    vs.into_iter()
                        .map(
                            |WireData {
                                 h,
                                 v_min,
                                 v_max,
                                 addr,
                             }| WireData {
                                h,
                                v_min: solution.value(v_min) as f32,
                                v_max: solution.value(v_max) as f32,
                                addr,
                            },
                        )
                        .collect()
                })
                .collect(),
        }
    }
}

#[allow(clippy::too_many_lines)]
fn h_layout_internal<T: Ctx>(
    graph: &MonoidalGraph<T>,
    problem: &mut LpProblem,
) -> LayoutInternal<T, Variable, ()>
where
    Weight<T::Operation>: Display,
{
    // STEP 1. Generate variables for each layer.
    let min = problem.add_variable(variable().min(0.0));
    let max = problem.add_variable(variable().min(0.0));

    let mut nodes = Vec::default();
    let mut wires: Vec<Vec<WireData<T, Variable, ()>>> = Vec::default();

    let add_constraints_wires =
        |problem: &mut LpProblem, vs: &Vec<Variable>, min: Variable, max: Variable| {
            if let Some(x) = vs.first().copied() {
                problem.add_constraint((x - min).geq(0.5));
            }
            if let Some(x) = vs.last().copied() {
                problem.add_constraint((max - x).geq(0.5));
            }
            for (x, y) in vs.iter().copied().tuple_windows() {
                problem.add_constraint((y - x).geq(1.0));
            }
        };

    let add_constraints_nodes = |problem: &mut LpProblem, ns: &Vec<NodeOffset<T, Variable, ()>>| {
        if let Some(x) = ns.first() {
            problem.add_constraint((x.node.h_min() - min).geq(0.5));
        }
        if let Some(x) = ns.last() {
            problem.add_constraint((max - x.node.h_max()).geq(0.5));
        }
        for (x, y) in ns.iter().tuple_windows() {
            problem.add_constraint((y.node.h_min() - x.node.h_max()).geq(1.0));
        }
    };

    let inputs = problem.add_variables(
        variable().min(0.0),
        graph.free_inputs.len() + graph.bound_inputs.len(),
    );
    add_constraints_wires(problem, &inputs, min, max);
    wires.push(
        inputs
            .into_iter()
            .zip(graph.free_inputs.iter().chain(&graph.bound_inputs))
            .map(|(h, addr)| WireData {
                h,
                v_min: (),
                v_max: (),
                addr: addr.clone(),
            })
            .collect(),
    );
    for slice in &graph.slices {
        let outputs = problem.add_variables(variable().min(0.0), slice.number_of_outputs());
        add_constraints_wires(problem, &outputs, min, max);
        wires.push(
            outputs
                .into_iter()
                .zip(slice.output_links())
                .map(|(h, link)| WireData {
                    h,
                    v_min: (),
                    v_max: (),
                    addr: link.0,
                })
                .collect(),
        );

        let mut input_offset = 0;
        let mut output_offset = 0;

        let ns = slice
            .ops
            .iter()
            .map(|op| {
                let node = match op {
                    MonoidalOp::Thunk { body, addr, .. } => Node::Thunk {
                        addr: addr.clone(),
                        layout: h_layout_internal(body, problem),
                        inputs: problem.add_variables(variable().min(0.0), addr.number_of_inputs()),
                        outputs: problem
                            .add_variables(variable().min(0.0), addr.number_of_outputs()),
                    },
                    MonoidalOp::Swap { out_to_in, .. } => Node::Swap {
                        h_pos: problem.add_variable(variable().min(0.0)),
                        v_top: (),
                        v_bot: (),
                        out_to_in: out_to_in.clone(),
                    },
                    MonoidalOp::Cup { .. } => Node::Atom {
                        h_pos: problem.add_variable(variable().min(0.0)),
                        v_pos: (),
                        extra_size: 0.0,
                        atype: AtomType::Cup,
                    },
                    MonoidalOp::Cap { .. } => Node::Atom {
                        h_pos: problem.add_variable(variable().min(0.0)),
                        v_pos: (),
                        extra_size: 0.0,
                        atype: AtomType::Cap,
                    },
                    MonoidalOp::Operation { addr } => Node::Atom {
                        h_pos: problem.add_variable(variable().min(0.0)),
                        v_pos: (),
                        extra_size: (addr.weight().to_string().chars().count().saturating_sub(1)
                            as f32
                            / 2.0)
                            * RADIUS_OPERATION,
                        atype: AtomType::Op(addr.clone()),
                    },
                    MonoidalOp::Copy { copies, .. } if *copies != 1 => Node::Atom {
                        h_pos: problem.add_variable(variable().min(0.0)),
                        v_pos: (),
                        extra_size: 0.0,
                        atype: AtomType::Copy,
                    },
                    _ => Node::Atom {
                        h_pos: problem.add_variable(variable().min(0.0)),
                        v_pos: (),
                        extra_size: 0.0,
                        atype: AtomType::Id,
                    },
                };
                let node_offset = NodeOffset {
                    node,
                    inputs: input_offset..input_offset + op.number_of_inputs(),
                    outputs: output_offset..output_offset + op.number_of_outputs(),
                };
                input_offset += op.number_of_inputs();
                output_offset += op.number_of_outputs();
                node_offset
            })
            .collect_vec();
        add_constraints_nodes(problem, &ns);
        nodes.push(ns);
    }

    // STEP 2. Add constraints between layers.
    for (nodes, (wires_i, wires_o)) in nodes.iter().zip(wires.iter().tuple_windows()) {
        let mut prev_op = None;
        for node in nodes {
            let ni = node.number_of_inputs();
            let no = node.number_of_outputs();

            let ins = &wires_i[node.inputs.clone()];
            let outs = &wires_o[node.outputs.clone()];

            let prev_in: Option<Expression> = if node.inputs.start == 0 {
                None
            } else {
                wires_i.get(node.inputs.start - 1).map(|x| x.h.into())
            };
            let prev_out: Option<Expression> = if node.outputs.start == 0 {
                None
            } else {
                wires_o.get(node.outputs.start - 1).map(|x| x.h.into())
            };

            // Distance constraints
            let constraints = [
                (prev_in.clone(), Some(node.node.h_min())),
                (prev_out.clone(), Some(node.node.h_min())),
                (prev_op.clone(), ins.first().map(|x| x.h.into())),
                (prev_op.clone(), outs.first().map(|x| x.h.into())),
                (prev_in, outs.first().map(|x| x.h.into())),
                (prev_out, ins.first().map(|x| x.h.into())),
                (prev_op, Some(node.node.h_min())),
            ];
            for (x, y) in constraints.into_iter().filter_map(|(x, y)| x.zip(y)) {
                problem.add_constraint((y - x).geq(1.0));
            }

            match &node.node {
                Node::Atom {
                    h_pos: pos, atype, ..
                } => {
                    match atype {
                        AtomType::Cup => {
                            for (x, y) in ins[1..].iter().zip(outs) {
                                problem.add_constraint(Expression::eq(x.h.into(), y.h));
                            }
                            problem.add_constraint((*pos * 2.0).eq(ins[ni - 1].h + ins[0].h));
                            problem.add_objective(ins[ni - 1].h - ins[0].h);
                        }
                        AtomType::Cap => {
                            for (x, y) in outs[1..].iter().zip(ins) {
                                problem.add_constraint(Expression::eq(x.h.into(), y.h));
                            }
                            problem.add_constraint((*pos * 2.0).eq(outs[no - 1].h + outs[0].h));
                            problem.add_objective(outs[no - 1].h - outs[0].h);
                        }
                        _ => {
                            // Try to "squish" inputs and outputs
                            if ni >= 2 {
                                problem.add_objective(ins[ni - 1].h - ins[0].h);
                            }

                            if no >= 2 {
                                problem.add_objective(outs[no - 1].h - outs[0].h);
                            }

                            // Fair averaging constraints
                            if ni > 0 {
                                let sum_ins: Expression = ins.iter().map(|x| x.h).sum();
                                problem.add_constraint((*pos * ni as f64).eq(sum_ins));
                            }
                            if no > 0 {
                                let sum_outs: Expression = outs.iter().map(|x| x.h).sum();
                                problem.add_constraint((*pos * no as f64).eq(sum_outs));
                            }
                        }
                    }
                }
                Node::Swap {
                    h_pos: pos,
                    out_to_in,
                    ..
                } => {
                    let in_outs: Expression = ins.iter().chain(outs.iter()).map(|x| x.h).sum();
                    problem.add_constraint((*pos * (ni + no) as f64).eq(in_outs));

                    for (i, j) in out_to_in.iter().copied().enumerate() {
                        let distance = problem.add_variable(variable().min(0.0));
                        problem.add_constraint((ins[j].h - outs[i].h).leq(distance));
                        problem.add_constraint((outs[i].h - ins[j].h).leq(distance));
                        problem.add_objective(distance);
                    }
                }
                Node::Thunk {
                    addr,
                    layout,
                    inputs,
                    outputs,
                } => {
                    // Distance constraints for the ports.
                    for x in inputs.iter().chain(outputs) {
                        problem.add_constraint((*x - layout.h_min).geq(0.5));
                        problem.add_constraint((layout.h_max - *x).geq(0.5));
                    }

                    // Align inner wires with ports.
                    for (&inner, &port) in layout
                        .inputs()
                        .take(addr.number_of_free_graph_inputs())
                        .zip(inputs)
                    {
                        problem.add_constraint(Expression::eq(inner.into(), port));
                    }
                    for (&inner, &port) in layout
                        .outputs()
                        .take(addr.number_of_free_graph_outputs())
                        .zip(outputs)
                    {
                        problem.add_constraint(Expression::eq(inner.into(), port));
                    }

                    // Align outer wires with ports.
                    for (outer, &port) in ins.iter().zip(inputs) {
                        let distance = problem.add_variable(variable().min(0.0));
                        problem.add_constraint((outer.h - port).leq(distance));
                        problem.add_constraint((port - outer.h).leq(distance));
                        problem.add_objective(distance * 1.5);
                    }
                    for (outer, &port) in outs.iter().zip(outputs) {
                        let distance = problem.add_variable(variable().min(0.0));
                        problem.add_constraint((outer.h - port).leq(distance));
                        problem.add_constraint((port - outer.h).leq(distance));
                        problem.add_objective(distance * 1.5);
                    }

                    problem.add_objective((layout.h_max - layout.h_min) * 2.0);
                }
            }

            prev_op = Some(node.node.h_max());
        }
    }

    LayoutInternal {
        h_min: min,
        h_max: max,
        v_min: (),
        v_max: (),
        nodes,
        wires,
    }
}

#[allow(clippy::too_many_lines)]
fn v_layout_internal<T: Ctx>(
    problem: &mut LpProblem,
    h_layout: HLayout<T, ()>,
) -> LayoutInternal<T, f32, Variable> {
    // Set up wires

    let wires: Vec<Vec<WireData<T, f32, Variable>>> = h_layout
        .wires
        .into_iter()
        .map(|vs| {
            vs.into_iter()
                .map(|v| {
                    let v_top = problem.add_variable(variable().min(0.0));
                    let v_bot = problem.add_variable(variable().min(0.0));

                    problem.add_constraint(Expression::leq(v_top.into(), v_bot));
                    problem.add_objective(v_bot - v_top);

                    WireData {
                        h: v.h,
                        v_min: v_top,
                        v_max: v_bot,
                        addr: v.addr,
                    }
                })
                .collect()
        })
        .collect();

    // Set up min

    let v_min = problem.add_variable(variable().min(0.0));

    for x in wires.first().unwrap() {
        problem.add_constraint(Expression::eq(v_min.into(), x.v_min));
        problem.add_constraint(Expression::leq(x.v_min + 0.5, x.v_max));
    }

    // Set up max

    let v_max = problem.add_variable(variable().min(0.0));

    for x in wires.last().unwrap() {
        problem.add_constraint(Expression::eq(v_max.into(), x.v_max));
        problem.add_constraint(Expression::leq(x.v_min + 0.5, x.v_max));
    }

    // Set up nodes

    let mut interval_tree: IntervalTree<OrderedFloat<f32>, Variable> = IntervalTree::new();

    let nodes = h_layout
        .nodes
        .into_iter()
        .zip(wires.iter().tuple_windows())
        .map(|(ns, (before, after))| {
            // let thunk_height = problem.add_variable(variable().min(0.0));
            ns.into_iter()
                .map(|n| {
                    let ins = &before[n.inputs.clone()];
                    let outs = &after[n.outputs.clone()];
                    let (node, top, bottom, interval) = match n.node {
                        Node::Atom {
                            h_pos,
                            extra_size,
                            atype,
                            ..
                        } => {
                            let v_pos = problem.add_variable(variable().min(0.0));

                            let in_gap = if n.inputs.len() < 2 {
                                1.0
                            } else {
                                f32::sqrt(ins.last().unwrap().h - ins[0].h)
                            } / 2.0;

                            let interval = Interval::new(
                                Bound::Included(OrderedFloat(h_pos - extra_size)),
                                Bound::Included(OrderedFloat(h_pos + extra_size)),
                            );

                            let out_gap = if n.outputs.len() < 2 {
                                1.0
                            } else {
                                f32::sqrt(outs.last().unwrap().h - outs[0].h)
                            } / 2.0;

                            let start = problem.add_variable(variable().min(0.0));
                            problem.add_constraint(Expression::eq(v_pos - in_gap, start));
                            let end = problem.add_variable(variable().min(0.0));
                            problem.add_constraint(Expression::eq(v_pos + out_gap, end));

                            (
                                Node::Atom {
                                    h_pos,
                                    v_pos,
                                    extra_size,
                                    atype,
                                },
                                start,
                                end,
                                interval,
                            )
                        }
                        Node::Swap {
                            h_pos, out_to_in, ..
                        } => {
                            let height = out_to_in
                                .iter()
                                .enumerate()
                                .map(|(i, x)| f32::sqrt((outs[i].h - ins[*x].h).abs()))
                                .max_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap_or_default();
                            let v_top = problem.add_variable(variable().min(0.0));
                            let v_bot = problem.add_variable(variable().min(0.0));

                            problem.add_constraint(Expression::eq(v_top + height, v_bot));

                            let (low, high) = ins
                                .iter()
                                .chain(outs.iter())
                                .map(|x| x.h)
                                .minmax()
                                .into_option()
                                .unwrap();

                            let interval = Interval::new(
                                Bound::Included(OrderedFloat(low)),
                                Bound::Included(OrderedFloat(high)),
                            );

                            (
                                Node::Swap {
                                    h_pos,
                                    v_top,
                                    v_bot,
                                    out_to_in,
                                },
                                v_top,
                                v_bot,
                                interval,
                            )
                        }
                        Node::Thunk {
                            addr,
                            layout,
                            inputs,
                            outputs,
                        } => {
                            let layout = v_layout_internal(problem, layout);

                            let x = ins
                                .iter()
                                .zip(&inputs)
                                .map(|(x, y)| f32::sqrt((x.h - y).abs()))
                                .max_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap_or_default();

                            let height_above = if x < 0.5 { 0.5 } else { x };

                            let y = outs
                                .iter()
                                .zip(&outputs)
                                .map(|(x, y)| f32::sqrt((x.h - y).abs()))
                                .max_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap_or_default();

                            let height_below = if y < 0.5 { 0.5 } else { y };

                            let start = problem.add_variable(variable().min(0.0));
                            problem
                                .add_constraint(Expression::eq(layout.v_min - height_above, start));
                            let end = problem.add_variable(variable().min(0.0));
                            problem
                                .add_constraint(Expression::eq(layout.v_max + height_below, end));

                            let interval = Interval::new(
                                Bound::Included(OrderedFloat(layout.h_min)),
                                Bound::Included(OrderedFloat(layout.h_max)),
                            );

                            (
                                Node::Thunk {
                                    addr,
                                    layout,
                                    inputs,
                                    outputs,
                                },
                                start,
                                end,
                                interval,
                            )
                        }
                    };

                    problem.add_constraint(Expression::leq(v_min.into(), top));
                    problem.add_constraint(Expression::leq(bottom.into(), v_max));

                    for x in &before[n.inputs.clone()] {
                        problem.add_constraint(Expression::eq(top.into(), x.v_max));
                    }

                    for x in &after[n.outputs.clone()] {
                        problem.add_constraint(Expression::eq(bottom.into(), x.v_min));
                    }

                    for x in interval_tree.query(&interval) {
                        problem.add_constraint(Expression::leq((*x.value()).into(), top));
                    }

                    interval_tree.insert(interval, bottom);

                    NodeOffset {
                        node,
                        inputs: n.inputs,
                        outputs: n.outputs,
                    }
                })
                .collect()
        })
        .collect();

    // Minimise entire graph
    problem.add_constraint(Expression::leq(v_min + 1.0, v_max));
    problem.add_objective((v_max - v_min) * 5.0);

    LayoutInternal {
        h_min: h_layout.h_min,
        h_max: h_layout.h_max,
        v_min,
        v_max,
        nodes,
        wires,
    }
}

pub fn layout<T: Ctx>(graph: &MonoidalGraph<T>) -> Result<Layout<T>, LayoutError>
where
    Weight<T::Operation>: Display,
{
    let mut problem = LpProblem::default();

    let layout = h_layout_internal(graph, &mut problem);
    problem.add_objective(layout.h_max);
    let h_solution = problem.minimise(good_lp::default_solver)?;

    problem = LpProblem::default();

    let v_layout = v_layout_internal(&mut problem, HLayout::from_solution_h(layout, &h_solution));
    problem.add_objective(v_layout.v_max);
    let v_solution = problem.minimise(good_lp::default_solver)?;

    let layout_complete = Layout::from_solution_v(v_layout, &v_solution);
    debug!("Layout complete: {:?}", layout_complete);
    Ok(layout_complete)
}

#[cfg(test)]
mod tests {
    use sd_core::examples;

    use super::layout;

    #[test]
    fn int() {
        insta::with_settings!({sort_maps => true}, {
            insta::assert_ron_snapshot!(layout(&examples::int()).expect("Layout failed"));
        });
    }

    #[test]
    fn copy() {
        insta::with_settings!({sort_maps => true}, {
            insta::assert_ron_snapshot!(layout(&examples::copy()).expect("Layout failed"));
        });
    }

    #[test]
    fn thunk() {
        insta::with_settings!({sort_maps => true}, {
            insta::assert_ron_snapshot!(layout(&examples::thunk()).expect("Layout failed"));
        });
    }
}
