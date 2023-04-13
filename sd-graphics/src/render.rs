use egui::{
    emath::RectTransform,
    epaint::{CircleShape, CubicBezierShape, RectShape},
    vec2, Align2, Color32, Pos2, Rect, Response, Rounding, Sense, Shape, Vec2,
};
use sd_core::monoidal::{MonoidalGraph, MonoidalOp};

use crate::layout::Layout;

pub const SCALE: f32 = 50.0;
pub const STROKE_WIDTH: f32 = 1.0;

pub const BOX_SIZE: Vec2 = vec2(20.0, 20.0);
pub const RADIUS_ARG: f32 = 2.5;
pub const RADIUS_COPY: f32 = 5.0;
pub const RADIUS_OPERATION: f32 = 10.0;

// Specifies how to transform a layout position to a screen position.
struct Transform {
    layout_bounds: Vec2,
    bounds: Vec2,
    to_screen: RectTransform,
}

impl Transform {
    fn apply(&self, x: f32, y: f32) -> Pos2 {
        // Scale by a constant and translate to the centre of the bounding box.
        self.to_screen.transform_pos(
            Pos2::new(x * SCALE, y * SCALE) + (self.bounds - self.layout_bounds * SCALE) / 2.0,
        )
    }
}

pub fn render(
    ui: &egui::Ui,
    response: &Response,
    layout: &Layout,
    graph: &mut MonoidalGraph,
    bounds: Vec2,
    to_screen: RectTransform,
) -> Vec<Shape> {
    let transform = Transform {
        bounds,
        to_screen,
        layout_bounds: vec2(layout.width(), layout.height()),
    };

    let mut shapes = Vec::default();
    generate_shapes(ui, response, &mut shapes, 0.0, layout, graph, &transform);
    shapes
}

fn generate_shapes(
    ui: &egui::Ui,
    response: &Response,
    shapes: &mut Vec<Shape>,
    mut y_offset: f32,
    layout: &Layout,
    graph: &mut MonoidalGraph,
    transform: &Transform,
) {
    let default_stroke = ui.visuals().noninteractive().fg_stroke;
    let default_color = default_stroke.color;

    // Source
    for &x in layout.inputs() {
        let start = transform.apply(x, y_offset);
        let end = transform.apply(x, y_offset + 0.5);
        shapes.push(Shape::line_segment([start, end], default_stroke));
    }

    y_offset += 0.5;

    for (j, slice) in graph.slices.iter_mut().enumerate() {
        let slice_height = layout.slice_height(j);
        let y_in = y_offset;
        let y_out = y_offset + slice_height;

        let mut offset_i = 0;
        let mut offset_o = 0;
        for (i, (op, _)) in slice.ops.iter_mut().enumerate() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            let x_op = &layout.nodes[j][i];
            let x_ins = &layout.wires[j][offset_i..offset_i + ni];
            let x_outs = &layout.wires[j + 1][offset_o..offset_o + no];

            let id = response.id.with((j, i));

            match op {
                MonoidalOp::Swap => {
                    let in1 = transform.apply(x_ins[0], y_in);
                    let in2 = transform.apply(x_ins[1], y_in);
                    let out1 = transform.apply(x_outs[0], y_out);
                    let out2 = transform.apply(x_outs[1], y_out);

                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(in1, out2),
                        false,
                        Color32::TRANSPARENT,
                        default_stroke,
                    )));
                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(in2, out1),
                        false,
                        Color32::TRANSPARENT,
                        default_stroke,
                    )));
                }
                MonoidalOp::Thunk {
                    args,
                    body,
                    expanded,
                } if *expanded => {
                    let x_op = x_op.unwrap_thunk();
                    let diff = (slice_height - x_op.height()) / 2.0;
                    let y_min = y_in + diff;
                    let y_max = y_out - diff;
                    for &x in x_ins {
                        let thunk = transform.apply(x, y_min);
                        let input = transform.apply(x, y_in);
                        shapes.push(Shape::line_segment([input, thunk], default_stroke));
                    }
                    for &x in x_outs {
                        let thunk = transform.apply(x, y_max);
                        let output = transform.apply(x, y_out);
                        shapes.push(Shape::line_segment([thunk, output], default_stroke));
                    }
                    let thunk_rect = Rect::from_min_max(
                        transform.apply(x_op.min, y_min),
                        transform.apply(x_op.max, y_max),
                    );
                    let thunk_response = ui.interact(thunk_rect, id, Sense::click());
                    if thunk_response.clicked() {
                        *expanded = false;
                    }
                    shapes.push(Shape::rect_stroke(
                        thunk_rect,
                        Rounding::none(),
                        ui.style().interact(&thunk_response).fg_stroke,
                    ));
                    for &x in x_op.inputs().iter().rev().take(*args) {
                        let dot = transform.apply(x, y_min);
                        shapes.push(Shape::circle_filled(dot, RADIUS_ARG, default_color))
                    }
                    generate_shapes(ui, &thunk_response, shapes, y_min, x_op, body, transform);
                }
                _ => {
                    let x_op = *x_op.unwrap_atom();
                    let y_op = (y_in + y_out) / 2.0;
                    let center = transform.apply(x_op, y_op);

                    for &x in x_ins {
                        let input = transform.apply(x, y_in);
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            vertical_out_horizontal_in(input, center),
                            false,
                            Color32::TRANSPARENT,
                            default_stroke,
                        )));
                    }

                    for &x in x_outs {
                        let output = transform.apply(x, y_out);
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            horizontal_out_vertical_in(center, output),
                            false,
                            Color32::TRANSPARENT,
                            default_stroke,
                        )));
                    }

                    match op {
                        MonoidalOp::Copy { copies } if *copies != 1 => {
                            shapes.push(Shape::circle_filled(center, RADIUS_COPY, default_color))
                        }
                        MonoidalOp::Operation { op_name, .. } => {
                            let op_rect = Rect::from_center_size(center, BOX_SIZE);
                            let op_response = ui.interact(op_rect, id, Sense::click());
                            shapes.push(Shape::Circle(CircleShape {
                                center,
                                radius: RADIUS_OPERATION,
                                fill: ui.style().interact(&op_response).bg_fill,
                                stroke: ui.style().interact(&op_response).fg_stroke,
                            }));
                            ui.fonts(|fonts| {
                                shapes.push(Shape::text(
                                    fonts,
                                    center,
                                    Align2::CENTER_CENTER,
                                    op_name,
                                    Default::default(),
                                    ui.visuals().strong_text_color(),
                                ));
                            });
                        }
                        MonoidalOp::Thunk { expanded, .. } => {
                            let thunk_rect = Rect::from_center_size(center, BOX_SIZE);
                            let thunk_response = ui.interact(thunk_rect, id, Sense::click());
                            if thunk_response.clicked() {
                                *expanded = true;
                            }
                            shapes.push(Shape::Rect(RectShape {
                                rect: thunk_rect,
                                rounding: Rounding::none(),
                                fill: ui.style().interact(&thunk_response).bg_fill,
                                stroke: ui.style().interact(&thunk_response).fg_stroke,
                            }));
                        }
                        _ => (),
                    }
                }
            }

            offset_i += ni;
            offset_o += no;
        }

        y_offset = y_out;
    }

    // Target
    for &x in layout.outputs() {
        let start = transform.apply(x, y_offset);
        let end = transform.apply(x, y_offset + 0.5);
        shapes.push(Shape::line_segment([start, end], default_stroke));
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
