use std::fmt::Display;

use egui::{emath::RectTransform, show_tooltip_at_pointer, Id, Pos2, Rect, Response};
use indexmap::IndexSet;
use itertools::Itertools;
use sd_core::{
    codeable::Codeable,
    common::Matchable,
    hypergraph::{
        generic::{Ctx, Edge, Operation, OperationWeight, Thunk},
        subgraph::ExtensibleEdge,
        traits::{Graph, NodeLike, WithWeight},
    },
    prettyprinter::PrettyPrint,
};

use crate::{
    common::{Shapeable, RADIUS_ARG, RADIUS_COPY, RADIUS_OPERATION},
    layout::{AtomType, Layout, NodeOffset},
    renderable::RenderableGraph,
    shape::Shape,
};

#[allow(clippy::needless_collect)]
pub fn render<G>(
    graph: &mut G,
    ui: &egui::Ui,
    shapes: &[Shape<G::Ctx>],
    response: &Response,
    to_screen: RectTransform,
    search: Option<&str>,
) -> Vec<egui::Shape>
where
    G: RenderableGraph,
    Edge<G::Ctx>: Codeable,
    Operation<G::Ctx>: Codeable + Matchable,
    Thunk<G::Ctx>: Matchable,
{
    let viewport = *to_screen.from();

    let mut highlight_op = None;
    let mut highlight_edges = IndexSet::default();

    let id = Id::new(graph.key());
    let shapes_vec: Vec<_> = shapes
        .iter()
        .filter(|shape| viewport.intersects(shape.bounding_box()))
        .map(|shape| {
            let mut s = shape.clone();
            s.apply_transform(&to_screen);
            s.collect_highlights(
                graph,
                id,
                ui,
                response,
                &to_screen,
                search,
                &mut highlight_op,
                &mut highlight_edges,
            );
            s
        })
        .collect();

    // Show hover tooltips.
    let labels = match highlight_op {
        Some(op) => {
            highlight_edges.extend(op.inputs().chain(op.outputs()));
            vec![op.code().to_pretty()]
        }
        None => highlight_edges
            .iter()
            .map(|edge| edge.code().to_pretty())
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
pub fn generate_shapes<T>(shapes: &mut Vec<Shape<T>>, layout: &Layout<T>, arrows: bool)
where
    T: Ctx,
    T::Edge: ExtensibleEdge,
    T::Operation: Shapeable,
    OperationWeight<T>: Display,
{
    if arrows {
        // Source
        for wire in layout.input_wires() {
            if let Some(node) = wire.addr.extend_source() {
                shapes.push(Shape::Arrow {
                    addr: wire.addr.clone(),
                    to_add: vec![node],
                    center: Pos2::new(wire.h, layout.v_min - 0.5),
                    upwards: true,
                    stroke: None,
                    height: 0.1,
                });
            }
        }

        // Target
        for wire in layout.output_wires() {
            let targets = wire.addr.extend_targets().collect::<Vec<_>>();
            if !targets.is_empty() {
                shapes.push(Shape::Arrow {
                    addr: wire.addr.clone(),
                    to_add: targets,
                    center: Pos2::new(wire.h, layout.v_max + 0.5),
                    upwards: false,
                    stroke: None,
                    height: 0.1,
                });
            }
        }
    }

    // Wires
    for wire in layout.wires.iter().flat_map(|x| x.iter()) {
        shapes.push(Shape::Line {
            start: Pos2::new(wire.h, wire.v_min),
            end: Pos2::new(wire.h, wire.v_max),
            addr: wire.addr.clone(),
        });
    }

    // Nodes
    for (i, (slice, (before, after))) in layout
        .nodes
        .iter()
        .zip(layout.wires.iter().tuple_windows())
        .enumerate()
    {
        for (
            j,
            NodeOffset {
                node,
                inputs,
                outputs,
            },
        ) in slice.iter().enumerate()
        {
            let x_ins = &before[inputs.clone()];
            let x_outs = &after[outputs.clone()];

            match node {
                crate::layout::Node::Atom {
                    h_pos,
                    v_pos,
                    atype,
                    ..
                } => {
                    let center = Pos2::new(*h_pos, *v_pos);
                    let (x_ins_rem, x_outs_rem) = match atype {
                        AtomType::Cap => {
                            for (wire_in, wire_out) in x_ins.iter().zip(&x_outs[1..]) {
                                let start = Pos2::new(wire_in.h, wire_in.v_max);
                                let end = Pos2::new(wire_out.h, wire_out.v_min);
                                shapes.push(Shape::Line {
                                    start,
                                    end,
                                    addr: wire_in.addr.clone(),
                                });
                            }
                            (
                                vec![],
                                vec![x_outs[0].clone(), x_outs.last().unwrap().clone()],
                            )
                        }
                        AtomType::Cup => {
                            for (wire_out, wire_in) in x_outs.iter().zip(&x_ins[1..]) {
                                let start = Pos2::new(wire_in.h, wire_in.v_max);
                                let end = Pos2::new(wire_out.h, wire_out.v_min);
                                shapes.push(Shape::Line {
                                    start,
                                    end,
                                    addr: wire_in.addr.clone(),
                                });
                            }
                            (
                                vec![x_ins[0].clone(), x_ins.last().unwrap().clone()],
                                vec![],
                            )
                        }
                        _ => (x_ins.to_vec(), x_outs.to_vec()),
                    };

                    for wire in x_ins_rem {
                        let input = Pos2::new(wire.h, wire.v_max);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_horizontal_in(input, center),
                            addr: wire.addr.clone(),
                        });
                    }

                    for wire in x_outs_rem {
                        let output = Pos2::new(wire.h, wire.v_min);
                        shapes.push(Shape::CubicBezier {
                            points: horizontal_out_vertical_in(center, output),
                            addr: wire.addr.clone(),
                        });
                    }

                    match atype {
                        AtomType::Copy => {
                            shapes.push(Shape::CircleFilled {
                                center,
                                radius: RADIUS_COPY,
                                addr: x_ins[0].addr.clone(),
                                coord: [j, i],
                            });
                        }
                        AtomType::Op(addr) => {
                            shapes.push(Shape::Operation {
                                center,
                                addr: addr.clone(),
                                label: addr.weight().to_string(),
                                kind: addr.to_shape(),
                                radius: RADIUS_OPERATION,
                                fill: None,
                                stroke: None,
                            });
                        }
                        _ => (),
                    }
                }
                crate::layout::Node::Swap {
                    v_top,
                    v_bot,
                    out_to_in,
                    ..
                } => {
                    for (out_idx, in_idx) in out_to_in.iter().enumerate() {
                        let in_wire = Pos2::new(x_ins[*in_idx].h, *v_top);
                        let out_wire = Pos2::new(x_outs[out_idx].h, *v_bot);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_vertical_in(in_wire, out_wire),
                            addr: x_ins[*in_idx].addr.clone(),
                        });
                    }
                }
                crate::layout::Node::Thunk {
                    addr,
                    layout,
                    inputs,
                    outputs,
                } => {
                    for (outer, inner) in x_ins.iter().zip(inputs) {
                        let start = Pos2::new(outer.h, outer.v_max);
                        let end = Pos2::new(*inner, layout.v_min);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_vertical_in(start, end),
                            addr: outer.addr.clone(),
                        });
                    }

                    for (outer, inner) in x_outs.iter().zip(outputs) {
                        let start = Pos2::new(*inner, layout.v_max);
                        let end = Pos2::new(outer.h, outer.v_min);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_vertical_in(start, end),
                            addr: outer.addr.clone(),
                        });
                    }

                    let thunk_rect = Rect::from_min_max(
                        Pos2::new(layout.h_min, layout.v_min),
                        Pos2::new(layout.h_max, layout.v_max),
                    );
                    shapes.push(Shape::Rectangle {
                        rect: thunk_rect,
                        addr: addr.clone(),
                        stroke: None,
                    });

                    for (edge, &x) in addr.bound_graph_inputs().rev().zip(layout.inputs().rev()) {
                        let center = Pos2::new(x, layout.v_min);
                        shapes.push(Shape::CircleFilled {
                            center,
                            radius: RADIUS_ARG,
                            addr: edge,
                            coord: [j, i],
                        });
                    }
                    for (edge, &x) in addr.bound_graph_outputs().rev().zip(layout.outputs().rev()) {
                        shapes.push(Shape::CircleFilled {
                            center: Pos2::new(x, layout.v_max),
                            radius: RADIUS_ARG,
                            addr: edge,
                            coord: [j, i],
                        });
                    }

                    generate_shapes(shapes, layout, false);
                }
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
