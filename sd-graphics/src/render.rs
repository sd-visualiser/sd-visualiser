use std::{collections::HashSet, fmt::Display, hash::BuildHasher};

use egui::{emath::RectTransform, show_tooltip_at_pointer, Pos2, Rect, Response};
use indexmap::IndexSet;
use sd_core::{
    common::{InOut, InOutIter},
    graph::{Name, Op},
    hypergraph::{Graph, Operation, Thunk},
    language::{Expr, Language},
    monoidal::{MonoidalGraph, MonoidalOp},
    prettyprinter::PrettyPrint,
};

use crate::{
    common::{BOX_SIZE, RADIUS_ARG, RADIUS_COPY, RADIUS_OPERATION},
    expanded::Expanded,
    layout::Layout,
    shape::Shape,
};

pub fn render<T, S>(
    ui: &egui::Ui,
    shapes: &[Shape<(Op<T>, Name<T>)>],
    response: &Response,
    expanded: &mut Expanded<Thunk<Op<T>, Name<T>>>,
    selections: &mut HashSet<Operation<Op<T>, Name<T>>, S>,
    to_screen: RectTransform,
) -> Vec<egui::Shape>
where
    T: Language,
    T::Op: PrettyPrint,
    T::Var: PrettyPrint,
    T::Addr: Display,
    T::VarDef: PrettyPrint,
    Expr<T>: PrettyPrint,
    S: BuildHasher,
{
    let viewport = *to_screen.from();

    let mut hover_points = IndexSet::default();
    let mut highlight_ports = HashSet::default();
    let mut operation_hovered = false;

    let shapes_vec: Vec<_> = shapes
        .iter()
        .filter(|shape| viewport.intersects(shape.bounding_box()))
        .map(|shape| {
            let mut s = shape.clone();
            s.apply_transform(&to_screen);
            s.collect_hovers(
                ui,
                response,
                &to_screen,
                &mut hover_points,
                &mut highlight_ports,
                expanded,
                selections,
                &mut operation_hovered,
            );
            s
        })
        .collect();

    let final_shapes: Vec<egui::Shape> = shapes_vec
        .into_iter()
        .map(|shape| shape.into_egui_shape(ui, &to_screen, &highlight_ports))
        .collect();

    // Show hover tooltips if not hovering on operation
    if !operation_hovered {
        for e in hover_points {
            show_tooltip_at_pointer(ui.ctx(), egui::Id::new("hover_tooltip"), |ui| {
                ui.label(e.to_pretty())
            });
        }
    }

    final_shapes
}

#[allow(clippy::too_many_lines)]
pub fn generate_shapes<V, E>(
    shapes: &mut Vec<Shape<(V, E)>>,
    mut y_offset: f32,
    layout: &Layout,
    graph: &MonoidalGraph<(V, E)>,
    expanded: &Expanded<Thunk<V, E>>,
) where
    V: Display,
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
                    let x_op = x_op.unwrap_thunk();
                    let diff = (slice_height - x_op.height()) / 2.0;
                    let y_min = y_input + diff;
                    let y_max = y_output - diff;
                    for (&x, port) in x_ins.iter().zip(&body.free_inputs) {
                        let start = Pos2::new(x, y_min);
                        let end = Pos2::new(x, y_input);
                        shapes.push(Shape::Line {
                            start,
                            end,
                            addr: port.clone(),
                        });
                    }
                    for (&x, port) in x_outs.iter().zip(addr.outputs()) {
                        let start = Pos2::new(x, y_max);
                        let end = Pos2::new(x, y_output);
                        shapes.push(Shape::Line {
                            start,
                            end,
                            addr: port,
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
                    for (port, &x) in addr
                        .bound_graph_inputs()
                        .rev()
                        .zip(x_op.inputs().iter().rev())
                    {
                        let center = Pos2::new(x, y_min);
                        shapes.push(Shape::CircleFilled {
                            center,
                            radius: RADIUS_ARG,
                            addr: port,
                        });
                    }
                    generate_shapes(shapes, y_min, x_op, body, expanded);
                }
                _ => {
                    let x_op = *x_op.unwrap_atom();
                    let y_op = (y_input + y_output) / 2.0;
                    let center = Pos2::new(x_op, y_op);

                    let (x_ins_rem, x_outs_rem) = match op {
                        MonoidalOp::Cap { addr, intermediate } => {
                            for (&x, (port, _)) in x_ins.iter().zip(intermediate) {
                                let start = Pos2::new(x, y_input);
                                let end = Pos2::new(x, y_output);
                                shapes.push(Shape::Line {
                                    start,
                                    end,
                                    addr: port.clone(),
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
                            for (&x, (port, _)) in x_outs.iter().zip(intermediate) {
                                let start = Pos2::new(x, y_input);
                                let end = Pos2::new(x, y_output);
                                shapes.push(Shape::Line {
                                    start,
                                    end,
                                    addr: port.clone(),
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
                                .zip(op.inputs().map(|(port, _)| port))
                                .collect::<Vec<_>>(),
                            x_outs
                                .iter()
                                .copied()
                                .zip(op.outputs().map(|(port, _)| port))
                                .collect::<Vec<_>>(),
                        ),
                    };

                    for (x, port) in x_ins_rem {
                        let input = Pos2::new(x, y_input);
                        shapes.push(Shape::CubicBezier {
                            points: vertical_out_horizontal_in(input, center),
                            addr: port,
                        });
                    }

                    for (x, port) in x_outs_rem {
                        let output = Pos2::new(x, y_output);
                        shapes.push(Shape::CubicBezier {
                            points: horizontal_out_vertical_in(center, output),
                            addr: port,
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
                            shapes.push(Shape::Circle {
                                center,
                                addr: addr.clone(),
                                radius: RADIUS_OPERATION,
                                fill: None,
                                stroke: None,
                            });
                            shapes.push(Shape::Text {
                                text: addr.weight().to_string(),
                                center,
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
    for (&x, port) in layout.outputs().iter().zip(&graph.outputs) {
        let start = Pos2::new(x, y_offset);
        let end = Pos2::new(x, y_offset + 0.5);
        shapes.push(Shape::Line {
            start,
            end,
            addr: port.link(),
        });
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
