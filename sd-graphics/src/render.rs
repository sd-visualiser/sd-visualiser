use epaint::{
    emath::{Align2, RectTransform},
    vec2, CircleShape, Color32, CubicBezierShape, Fonts, Pos2, Rect, RectShape, Rounding, Shape,
    Stroke, Vec2,
};
use itertools::Itertools;
use sd_core::monoidal::{MonoidalGraph, MonoidalOp};
use thiserror::Error;

use crate::layout::{layout, LayoutError};

pub const SCALE: f32 = 50.0;
pub const STROKE_WIDTH: f32 = 1.0;

pub const BOX_SIZE: Vec2 = vec2(20.0, 20.0);
pub const RADIUS_UNIT: f32 = 2.5;
pub const RADIUS_COPY: f32 = 5.0;
pub const RADIUS_OPERATION: f32 = 10.0;

pub fn default_stroke() -> Stroke {
    Stroke::new(STROKE_WIDTH, Color32::BLACK)
}

#[derive(Debug, Error)]
pub enum RenderError {
    #[error(transparent)]
    LayoutError(#[from] LayoutError),
}

pub fn render(
    graph: &MonoidalGraph,
    fonts: &Fonts,
    bounds: Vec2,
    to_screen: RectTransform,
) -> Result<Vec<Shape>, RenderError> {
    let len = graph.slices.len();
    let layout = layout(graph)?;

    let width = layout.width as f32;
    let height = len as f32 + 1.0;

    // Scale by a constant and translate to the centre of the bounding box.
    let pos2 = |x: f32, y: f32| {
        to_screen.transform_pos(Pos2::new(
            x * SCALE + (bounds.x - width * SCALE) / 2.0,
            y * SCALE + (bounds.y - height * SCALE) / 2.0,
        ))
    };

    let mut shapes: Vec<Shape> = Vec::new();

    // Source
    for &x in layout.slices.first().unwrap() {
        let start = pos2(x as f32, 0.0);
        let end = pos2(x as f32, 0.5);
        shapes.push(Shape::line_segment([start, end], default_stroke()));
    }

    // Target
    for &x in layout.slices.last().unwrap() {
        let start = pos2(x as f32, len as f32 + 0.5);
        let end = pos2(x as f32, len as f32 + 1.0);
        shapes.push(Shape::line_segment([start, end], default_stroke()));
    }

    for (j, slice) in graph.slices.iter().enumerate() {
        let mut offset_i = 0;
        let mut offset_o = 0;
        for (op, _) in slice.ops.iter() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            let input_wires = &layout.slices[j][offset_i..offset_i + ni];
            let output_wires = &layout.slices[j + 1][offset_o..offset_o + no];

            let op_y = j as f32 + 1.0;
            let input_y = j as f32 + 0.5;
            let output_y = j as f32 + 1.5;

            match op {
                MonoidalOp::Swap => {
                    let start_l = pos2(input_wires[0] as f32, input_y);
                    let start_r = pos2(input_wires[1] as f32, input_y);
                    let end_l = pos2(output_wires[0] as f32, output_y);
                    let end_r = pos2(output_wires[1] as f32, output_y);

                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(start_l, end_r),
                        false,
                        Color32::TRANSPARENT,
                        default_stroke(),
                    )));
                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(start_r, end_l),
                        false,
                        Color32::TRANSPARENT,
                        default_stroke(),
                    )));
                }
                _ => {
                    // Find the horizontal range that this operation covers.
                    let (&min_x, &max_x) = input_wires
                        .iter()
                        .chain(output_wires)
                        .minmax_by(|x, y| x.partial_cmp(y).unwrap())
                        .into_option()
                        .expect("Scalars are not allowed!");

                    let center = pos2((min_x + max_x) as f32 / 2.0, op_y);

                    for &x in input_wires {
                        let input = pos2(x as f32, input_y);
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            vertical_out_horizontal_in(input, center),
                            false,
                            Color32::TRANSPARENT,
                            default_stroke(),
                        )));
                    }

                    for &x in output_wires {
                        let output = pos2(x as f32, output_y);
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
                        MonoidalOp::Thunk { .. } => shapes.push(Shape::Rect(RectShape {
                            rect: Rect::from_center_size(center, BOX_SIZE),
                            rounding: Rounding::none(),
                            fill: Color32::WHITE,
                            stroke: default_stroke(),
                        })),
                        _ => (),
                    }
                }
            }

            offset_i += ni;
            offset_o += no;
        }
    }

    Ok(shapes)
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
