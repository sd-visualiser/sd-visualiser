use epaint::{
    emath::{Align2, RectTransform},
    vec2, CircleShape, Color32, CubicBezierShape, Fonts, Pos2, Rect, RectShape, Rounding, Shape,
    Stroke, Vec2,
};
use sd_core::monoidal::{MonoidalGraph, MonoidalOp};

use crate::layout::Layout;

pub const SCALE: f32 = 50.0;
pub const STROKE_WIDTH: f32 = 1.0;

pub const BOX_SIZE: Vec2 = vec2(20.0, 20.0);
pub const RADIUS_UNIT: f32 = 2.5;
pub const RADIUS_COPY: f32 = 5.0;
pub const RADIUS_OPERATION: f32 = 10.0;

pub fn default_stroke() -> Stroke {
    Stroke::new(STROKE_WIDTH, Color32::BLACK)
}

pub fn render(
    layout: &Layout,
    graph: &MonoidalGraph,
    fonts: &Fonts,
    bounds: Vec2,
    to_screen: RectTransform,
) -> Vec<Shape> {
    let n = graph.slices.len();

    let min_x = layout.min;
    let max_x = layout.max;
    let height = n as f32 + 1.0;

    // Scale by a constant and translate to the centre of the bounding box.
    let pos2 = |x: f32, y: f32| {
        to_screen.transform_pos(Pos2::new(
            (x - min_x) * SCALE + (bounds.x - (max_x - min_x) * SCALE) / 2.0,
            y * SCALE + (bounds.y - height * SCALE) / 2.0,
        ))
    };

    let mut shapes: Vec<Shape> = Vec::new();

    // Source
    for &x in layout.inputs() {
        let start = pos2(x, 0.0);
        let end = pos2(x, 0.5);
        shapes.push(Shape::line_segment([start, end], default_stroke()));
    }

    // Target
    for &x in layout.outputs() {
        let start = pos2(x, n as f32 + 0.5);
        let end = pos2(x, n as f32 + 1.0);
        shapes.push(Shape::line_segment([start, end], default_stroke()));
    }

    for (j, slice) in graph.slices.iter().enumerate() {
        let mut offset_i = 0;
        let mut offset_o = 0;
        for (i, (op, _)) in slice.ops.iter().enumerate() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            let x_op = &layout.nodes[j][i];
            let x_ins = &layout.wires[j][offset_i..offset_i + ni];
            let x_outs = &layout.wires[j + 1][offset_o..offset_o + no];

            let y_op = j as f32 + 1.0;
            let y_in = j as f32 + 0.5;
            let y_out = j as f32 + 1.5;

            match op {
                MonoidalOp::Swap => {
                    let in1 = pos2(x_ins[0], y_in);
                    let in2 = pos2(x_ins[1], y_in);
                    let out1 = pos2(x_outs[0], y_out);
                    let out2 = pos2(x_outs[1], y_out);

                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(in1, out2),
                        false,
                        Color32::TRANSPARENT,
                        default_stroke(),
                    )));
                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(in2, out1),
                        false,
                        Color32::TRANSPARENT,
                        default_stroke(),
                    )));
                }
                MonoidalOp::Thunk { .. } => {
                    let x_op = x_op.unwrap_thunk();
                    for &x in x_ins {
                        let thunk = pos2(x, y_op);
                        let input = pos2(x, y_in);
                        shapes.push(Shape::line_segment([input, thunk], default_stroke()));
                    }
                    for &x in x_outs {
                        let thunk = pos2(x, y_op);
                        let output = pos2(x, y_out);
                        shapes.push(Shape::line_segment([thunk, output], default_stroke()));
                    }
                    shapes.push(Shape::Rect(RectShape {
                        rect: Rect::from_min_max(
                            pos2(x_op.min, y_op) - BOX_SIZE / 2.0,
                            pos2(x_op.max, y_op) + BOX_SIZE / 2.0,
                        ),
                        rounding: Rounding::none(),
                        fill: Color32::WHITE,
                        stroke: default_stroke(),
                    }));
                }
                _ => {
                    let x_op = *x_op.unwrap_atom();
                    let center = pos2(x_op, y_op);

                    for &x in x_ins {
                        let input = pos2(x, y_in);
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            vertical_out_horizontal_in(input, center),
                            false,
                            Color32::TRANSPARENT,
                            default_stroke(),
                        )));
                    }

                    for &x in x_outs {
                        let output = pos2(x, y_out);
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            horizontal_out_vertical_in(center, output),
                            false,
                            Color32::TRANSPARENT,
                            default_stroke(),
                        )));
                    }

                    match op {
                        MonoidalOp::Copy { copies } if *copies != 1 => {
                            shapes.push(Shape::circle_filled(center, RADIUS_COPY, Color32::BLACK))
                        }
                        MonoidalOp::Unit => {
                            shapes.push(Shape::circle_filled(center, RADIUS_UNIT, Color32::BLACK))
                        }
                        MonoidalOp::Operation { op_name, .. } => {
                            shapes.push(Shape::Circle(CircleShape {
                                center,
                                radius: RADIUS_OPERATION,
                                fill: Color32::WHITE,
                                stroke: default_stroke(),
                            }));
                            shapes.push(Shape::text(
                                fonts,
                                center,
                                Align2::CENTER_CENTER,
                                op_name,
                                Default::default(),
                                Color32::BLACK,
                            ))
                        }
                        _ => (),
                    }
                }
            }

            offset_i += ni;
            offset_o += no;
        }
    }

    shapes
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
