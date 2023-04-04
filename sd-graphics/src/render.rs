use epaint::{
    emath::RectTransform, vec2, CircleShape, Color32, CubicBezierShape, Pos2, Rect, RectShape,
    Rounding, Shape, Stroke, Vec2,
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

#[derive(Debug, Error)]
pub enum RenderError {
    #[error(transparent)]
    LayoutError(#[from] LayoutError),
}

pub fn render(
    graph: MonoidalGraph,
    bounds: Vec2,
    transform: RectTransform,
) -> Result<Vec<Shape>, RenderError> {
    let layout = layout(&graph)?;

    let width = layout.width as f32;
    let height = layout.slices.len() as f32;

    // Scale by a constant and translate to the centre of the bounding box.
    let transform = |pos: Pos2| {
        transform.transform_pos(Pos2 {
            x: pos.x * SCALE + (bounds.x - width * SCALE) / 2.0,
            y: pos.y * SCALE + (bounds.y - height * SCALE) / 2.0,
        })
    };

    let mut shapes: Vec<Shape> = Vec::new();

    for (y, slice) in graph.slices.iter().enumerate() {
        let mut offset_i = 0;
        let mut offset_o = 0;
        for op in slice.ops.iter() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            let input_wires = &layout.slices[y][offset_i..offset_i + ni];
            let output_wires = &layout.slices[y + 1][offset_o..offset_o + no];

            // Find the horizontal range that this operation covers.
            let (&min_x, &max_x) = input_wires
                .iter()
                .chain(output_wires)
                .minmax_by(|x, y| x.partial_cmp(y).unwrap())
                .into_option()
                .expect("Scalars are not allowed!");

            match op {
                MonoidalOp::Swap => {
                    let start_l = transform(Pos2::new(input_wires[0] as f32, 2.0 * y as f32));
                    let start_r = transform(Pos2::new(input_wires[1] as f32, 2.0 * y as f32));
                    let end_l = transform(Pos2::new(output_wires[0] as f32, 2.0 * y as f32 + 2.0));
                    let end_r = transform(Pos2::new(output_wires[1] as f32, 2.0 * y as f32 + 2.0));

                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(start_l, end_r),
                        false,
                        Color32::TRANSPARENT,
                        Stroke::new(STROKE_WIDTH, Color32::BLACK),
                    )));
                    shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                        vertical_out_vertical_in(start_r, end_l),
                        false,
                        Color32::TRANSPARENT,
                        Stroke::new(STROKE_WIDTH, Color32::BLACK),
                    )));
                }
                _ => {
                    let center = transform(Pos2 {
                        x: (min_x + max_x) as f32 / 2.0,
                        y: 2.0 * y as f32 + 1.0,
                    });

                    for &x in input_wires {
                        let input = transform(Pos2 {
                            x: x as f32,
                            y: 2.0 * y as f32,
                        });
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            vertical_out_horizontal_in(input, center),
                            false,
                            Color32::TRANSPARENT,
                            Stroke::new(STROKE_WIDTH, Color32::BLACK),
                        )));
                    }

                    for &x in output_wires {
                        let output = transform(Pos2 {
                            x: x as f32,
                            y: 2.0 * y as f32 + 2.0,
                        });
                        shapes.push(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                            horizontal_out_vertical_in(center, output),
                            false,
                            Color32::TRANSPARENT,
                            Stroke::new(STROKE_WIDTH, Color32::BLACK),
                        )));
                    }

                    match op {
                        MonoidalOp::Copy { copies } if *copies != 1 => {
                            shapes.push(Shape::circle_filled(center, RADIUS_COPY, Color32::BLACK))
                        }
                        MonoidalOp::Unit => {
                            shapes.push(Shape::circle_filled(center, RADIUS_UNIT, Color32::BLACK))
                        }
                        MonoidalOp::Operation { .. } => shapes.push(Shape::Circle(CircleShape {
                            center,
                            radius: RADIUS_OPERATION,
                            fill: Color32::WHITE,
                            stroke: Stroke::new(STROKE_WIDTH, Color32::BLACK),
                        })),
                        MonoidalOp::Thunk { .. } => shapes.push(Shape::Rect(RectShape {
                            rect: Rect::from_center_size(center, BOX_SIZE),
                            rounding: Rounding::none(),
                            fill: Color32::WHITE,
                            stroke: Stroke::new(STROKE_WIDTH, Color32::BLACK),
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
        Pos2::new(start.x, 0.2 * start.y + 0.8 * end.y),
        Pos2::new(end.x, 0.8 * start.y + 0.2 * end.y),
        end,
    ]
}
