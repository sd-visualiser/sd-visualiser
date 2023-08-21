use std::fmt::Display;

use egui::{emath::RectTransform, show_tooltip_at_pointer, Pos2, Rect, Response};
use indexmap::IndexSet;
use sd_core::{
    common::{InOut, InOutIter},
    decompile::{decompile, Fresh},
    graph::Name,
    hypergraph::{
        generic::{Ctx, Edge, Node, NodeWeight, Operation, Thunk},
        subgraph::{ExtensibleEdge, ModifiableGraph},
        traits::{Graph, NodeLike, WithWeight},
    },
    language::{Expr, Language},
    monoidal::graph::{MonoidalGraph, MonoidalOp},
    prettyprinter::PrettyPrint,
    selection::SelectionMap,
    weak_map::WeakMap,
};

use crate::{
    common::{EdgeLabel, BOX_SIZE, RADIUS_ARG, RADIUS_COPY, RADIUS_OPERATION},
    layout::Layout,
    shape::Shape,
};

#[allow(clippy::needless_collect)]
#[allow(clippy::type_complexity)]
pub fn render<T, G>(
    graph: &mut G,
    ui: &egui::Ui,
    shapes: &[Shape<G::Ctx>],
    response: &Response,
    expanded: &mut WeakMap<Thunk<G::Ctx>, bool>,
    mut selection: Option<&mut SelectionMap<G::Ctx>>,
    to_screen: RectTransform,
) -> Vec<egui::Shape>
where
    T: Language,
    T::Op: PrettyPrint,
    T::Var: PrettyPrint + Fresh,
    T::Addr: Display,
    T::VarDef: PrettyPrint,
    Expr<T>: PrettyPrint,
    G: ModifiableGraph,
    Edge<G::Ctx>: WithWeight<Weight = Name<T>>,
    Operation<G::Ctx>: WithWeight<Weight = T::Op>,
{
    let viewport = *to_screen.from();

    let mut highlight_node = None;
    let mut highlight_edges = IndexSet::default();

    let shapes_vec: Vec<_> = shapes
        .iter()
        .filter(|shape| viewport.intersects(shape.bounding_box()))
        .map(|shape| {
            let mut s = shape.clone();
            s.apply_transform(&to_screen);
            s.collect_highlights(
                graph,
                ui,
                response,
                &to_screen,
                &mut highlight_node,
                &mut highlight_edges,
                expanded,
                selection.as_deref_mut(),
            );
            s
        })
        .collect();

    // Show hover tooltips.
    let labels = match highlight_node {
        Some(node) => {
            highlight_edges.extend(node.inputs().chain(node.outputs()));
            match &node {
                Node::Operation(op) => {
                    vec![op.weight().to_pretty()]
                }
                Node::Thunk(thunk) => {
                    vec![decompile(thunk)
                        .map_or_else(|_| "thunk".to_owned(), |body| body.to_pretty())]
                }
            }
        }
        None => highlight_edges
            .iter()
            .map(|edge| EdgeLabel::from_edge::<G::Ctx>(edge).to_pretty())
            .collect(),
    };
    for label in labels {
        show_tooltip_at_pointer(ui.ctx(), egui::Id::new("hover_tooltip"), |ui| {
            ui.label(label)
        });
    }

    shapes_vec
        .into_iter()
        .map(|shape| shape.into_egui_shape(ui, &to_screen, &highlight_edges))
        .collect()
}

#[allow(clippy::too_many_lines)]
pub fn generate_shapes<T>(
    shapes: &mut Vec<Shape<T>>,
    mut y_offset: f32,
    layout: &Layout,
    graph: &MonoidalGraph<T>,
    expanded: &WeakMap<T::Thunk, bool>,
    arrows: bool,
) where
    T: Ctx,
    T::Edge: ExtensibleEdge<Ctx = T>,
    NodeWeight<T>: Display,
{
    // Source
    for (&x, addr) in layout
        .inputs()
        .iter()
        .zip(graph.free_inputs.iter().chain(graph.bound_inputs.iter()))
    {
        let start = Pos2::new(x, y_offset);
        let end = Pos2::new(x, y_offset + 0.5);
        shapes.push(Shape::Line {
            start,
            end,
            addr: addr.clone(),
        });

        if arrows {
            if let Some(node) = addr.extend_source() {
                shapes.push(Shape::Arrow {
                    addr: addr.clone(),
                    to_add: vec![node],
                    center: Pos2::new(x, y_offset - 0.5),
                    upwards: true,
                    stroke: None,
                    height: 0.1,
                });
            }
        }
    }

    y_offset += 0.5;

    for (j, slice) in graph.slices.iter().enumerate() {
        let slice_height = layout.slice_height(j);
        let y_input = y_offset;
        let y_output = y_offset + slice_height;

        y_offset = y_output;

        let mut offset_i = 0;
        let mut offset_o = 0;
        for (i, op) in slice.ops.iter().enumerate() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            let x_op = &layout.nodes[j][i];
            let x_ins = &layout.wires[j][offset_i..offset_i + ni];
            let x_outs = &layout.wires[j + 1][offset_o..offset_o + no];

            match op {
                MonoidalOp::Swap { addrs, out_to_in } => {
                    for (out_idx, in_idx) in out_to_in.iter().enumerate() {
                        let in_wire = Pos2::new(x_ins[*in_idx], y_input);
                        let out_wire = Pos2::new(x_outs[out_idx], y_output);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_vertical_in(in_wire, out_wire),
                            addr: addrs[*in_idx].0.clone(),
                        });
                    }
                }
                MonoidalOp::Thunk { addr, body, .. } if expanded[addr] => {
                    let x_op = x_op.node.unwrap_thunk();
                    let diff = (slice_height - x_op.height()) / 2.0;
                    let y_min = y_input + diff;
                    let y_max = y_output - diff;
                    for ((&x, &x_body), edge) in x_ins
                        .iter()
                        .zip(x_op.inputs().iter())
                        .zip(&body.free_inputs)
                    {
                        let start = Pos2::new(x, y_input);
                        let end = Pos2::new(x_body, y_min);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_vertical_in(start, end),
                            addr: edge.clone(),
                        });
                    }
                    for ((&x, &x_body), edge) in
                        x_outs.iter().zip(x_op.outputs().iter()).zip(addr.outputs())
                    {
                        let start = Pos2::new(x_body, y_max);
                        let end = Pos2::new(x, y_output);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_vertical_in(start, end),
                            addr: edge,
                        });
                    }
                    let thunk_rect =
                        Rect::from_min_max(Pos2::new(x_op.min, y_min), Pos2::new(x_op.max, y_max));
                    shapes.push(Shape::Rectangle {
                        rect: thunk_rect,
                        addr: addr.clone(),
                        fill: None,
                        stroke: None,
                    });
                    for (edge, &x) in addr
                        .bound_graph_inputs()
                        .rev()
                        .zip(x_op.inputs().iter().rev())
                    {
                        let center = Pos2::new(x, y_min);
                        shapes.push(Shape::CircleFilled {
                            center,
                            radius: RADIUS_ARG,
                            addr: edge,
                        });
                    }
                    generate_shapes(shapes, y_min, x_op, body, expanded, false);
                }
                _ => {
                    let x_op = *x_op.node.unwrap_atom();
                    let y_op = (y_input + y_output) / 2.0;
                    let center = Pos2::new(x_op, y_op);

                    let (x_ins_rem, x_outs_rem) = match op {
                        MonoidalOp::Cap { addr, intermediate } => {
                            for (&x, (edge, _)) in x_ins.iter().zip(intermediate) {
                                let start = Pos2::new(x, y_input);
                                let end = Pos2::new(x, y_output);
                                shapes.push(Shape::Line {
                                    start,
                                    end,
                                    addr: edge.clone(),
                                });
                            }
                            (
                                vec![],
                                vec![
                                    (x_outs[0], addr.0.clone()),
                                    (*x_outs.last().unwrap(), addr.0.clone()),
                                ],
                            )
                        }
                        MonoidalOp::Cup { addr, intermediate } => {
                            for (&x, (edge, _)) in x_outs.iter().zip(intermediate) {
                                let start = Pos2::new(x, y_input);
                                let end = Pos2::new(x, y_output);
                                shapes.push(Shape::Line {
                                    start,
                                    end,
                                    addr: edge.clone(),
                                });
                            }
                            (
                                vec![
                                    (x_ins[0], addr.0.clone()),
                                    (*x_ins.last().unwrap(), addr.0.clone()),
                                ],
                                vec![],
                            )
                        }
                        _ => (
                            x_ins
                                .iter()
                                .copied()
                                .zip(op.input_links().map(|(edge, _)| edge))
                                .collect::<Vec<_>>(),
                            x_outs
                                .iter()
                                .copied()
                                .zip(op.output_links().map(|(edge, _)| edge))
                                .collect::<Vec<_>>(),
                        ),
                    };

                    for (x, edge) in x_ins_rem {
                        let input = Pos2::new(x, y_input);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_horizontal_in(input, center),
                            addr: edge,
                        });
                    }

                    for (x, edge) in x_outs_rem {
                        let output = Pos2::new(x, y_output);
                        shapes.push(Shape::CubicBezier {
                            points: horizontal_out_vertical_in(center, output),
                            addr: edge,
                        });
                    }

                    match op {
                        MonoidalOp::Copy { copies, addr, .. } if *copies != 1 => {
                            shapes.push(Shape::CircleFilled {
                                center,
                                radius: RADIUS_COPY,
                                addr: addr.clone(),
                            });
                        }
                        MonoidalOp::Operation { addr } => {
                            shapes.push(Shape::Operation {
                                center,
                                addr: addr.clone(),
                                label: addr.weight().to_string(),
                                radius: RADIUS_OPERATION,
                                fill: None,
                                stroke: None,
                            });
                        }
                        MonoidalOp::Thunk { addr, .. } => {
                            let thunk_rect = Rect::from_center_size(center, BOX_SIZE);
                            shapes.push(Shape::Rectangle {
                                rect: thunk_rect,
                                addr: addr.clone(),
                                fill: None,
                                stroke: None,
                            });
                        }
                        _ => (),
                    }
                }
            }

            offset_i += ni;
            offset_o += no;
        }
    }

    // Target
    for (&x, edge) in layout.outputs().iter().zip(&graph.outputs) {
        let start = Pos2::new(x, y_offset);
        let end = Pos2::new(x, y_offset + 0.5);
        shapes.push(Shape::Line {
            start,
            end,
            addr: edge.clone(),
        });

        if arrows {
            let targets = edge.extend_targets().collect::<Vec<_>>();
            if !targets.is_empty() {
                shapes.push(Shape::Arrow {
                    addr: edge.clone(),
                    to_add: targets,
                    center: Pos2::new(x, y_offset + 1.0),
                    upwards: false,
                    stroke: None,
                    height: 0.1,
                });
            }
        }
    }
}

fn vertical_out_horizontal_in(start: Pos2, end: Pos2) -> [Pos2; 4] {
    [
        start,
        Pos2::new(start.x, 0.2 * start.y + 0.8 * end.y),
        Pos2::new(0.6 * start.x + 0.4 * end.x, end.y),
        end,
    ]
}

fn horizontal_out_vertical_in(start: Pos2, end: Pos2) -> [Pos2; 4] {
    [
        start,
        Pos2::new(0.4 * start.x + 0.6 * end.x, start.y),
        Pos2::new(end.x, 0.8 * start.y + 0.2 * end.y),
        end,
    ]
}

fn vertical_out_vertical_in(start: Pos2, end: Pos2) -> [Pos2; 4] {
    [
        start,
        Pos2::new(start.x, 0.5 * start.y + 0.5 * end.y),
        Pos2::new(end.x, 0.5 * start.y + 0.5 * end.y),
        end,
    ]
}
