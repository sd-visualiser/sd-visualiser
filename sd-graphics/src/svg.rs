use egui::{Color32, emath::TSTransform};
use sd_core::hypergraph::{
    generic::Ctx,
    traits::{WireType, WithType, WithWeight},
};
use svg::{
    Document, Node,
    node::element::{Circle, Group, Line, Path, Rectangle, Text, path::Data},
};

use crate::{
    common::SCALE,
    shape::{Shape, Shapes},
};

impl<T: Ctx> Shape<T>
where
    <<T as Ctx>::Edge as WithWeight>::Weight: WithType,
{
    pub(crate) fn to_svg(&self) -> Box<dyn Node> {
        match self {
            Self::Operation {
                center,
                radius,
                label,
                ..
            } => {
                let x_size =
                    f32::from(*radius) / 20.0 * f32::from(label.chars().count().max(1) as u16 + 1);
                Box::new(
                    Group::new()
                        .add(
                            Rectangle::new()
                                .set("x", center.x - x_size / 2.0)
                                .set("y", center.y - f32::from(*radius) / 20.0)
                                .set("width", x_size)
                                .set("height", f32::from(*radius) / 10.0)
                                .set("rx", f32::from(*radius) / 20.0)
                                .set("ry", f32::from(*radius) / 20.0)
                                .set("fill", "white")
                                .set("stroke", "black")
                                .set("stroke-width", 1),
                        )
                        .add(
                            Text::new(html_escape::encode_text(label))
                                .set("x", center.x)
                                .set("y", center.y)
                                .set("font-size", 16)
                                .set("font-family", "monospace")
                                .set("text-anchor", "middle")
                                .set("dominant-baseline", "middle"),
                        ),
                )
            }
            Self::CircleFilled {
                center,
                radius,
                addr,
                ..
            } => Box::new(
                Circle::new()
                    .set("cx", center.x)
                    .set("cy", center.y)
                    .set("r", f32::from(*radius) / 20.0)
                    .set(
                        "fill",
                        wire_type_to_svg_colour(addr.weight().get_type()).to_hex(),
                    ),
            ),
            Self::Rectangle { rect, .. } => Box::new(
                Rectangle::new()
                    .set("x", rect.min.x)
                    .set("y", rect.min.y)
                    .set("width", rect.width())
                    .set("height", rect.height())
                    .set("fill", "none")
                    .set("stroke", "gray")
                    .set("stroke-width", 1),
            ),
            Self::Line {
                start, end, addr, ..
            } => Box::new({
                Line::new()
                    .set("x1", start.x)
                    .set("y1", start.y)
                    .set("x2", end.x)
                    .set("y2", end.y)
                    .set(
                        "stroke",
                        wire_type_to_svg_colour(addr.weight().get_type()).to_hex(),
                    )
                    .set("stroke-width", "1")
            }),
            Self::CubicBezier { points, addr, .. } => Box::new({
                let data = Data::new()
                    .move_to((points[0].x, points[0].y))
                    .cubic_curve_to((
                        points[1].x,
                        points[1].y,
                        points[2].x,
                        points[2].y,
                        points[3].x,
                        points[3].y,
                    ));
                Path::new()
                    .set("d", data)
                    .set("fill", "none")
                    .set(
                        "stroke",
                        wire_type_to_svg_colour(addr.weight().get_type()).to_hex(),
                    )
                    .set("stroke-width", 1)
            }),
            Self::Arrow { .. } => {
                panic!("Arrows should not be in svgs")
            }
        }
    }
}

impl<T: Ctx> Shapes<T>
where
    <<T as Ctx>::Edge as WithWeight>::Weight: WithType,
{
    #[must_use]
    pub fn to_svg(&self) -> Document {
        let mut document = Document::new()
            .set("width", self.size.x * SCALE)
            .set("height", self.size.y * SCALE);

        let scale = TSTransform::from_scaling(SCALE);

        for shape in &self.shapes {
            let mut shape = shape.clone();
            shape.apply_tst_transform(&scale);
            document = document.add(shape.to_svg());
        }

        document
    }
}

fn wire_type_to_svg_colour(wire_type: WireType) -> Color32 {
    match wire_type {
        WireType::Data => Color32::BLACK,
        WireType::ControlFlow => Color32::GOLD,
        WireType::SymName => Color32::DARK_GREEN,
        WireType::Colour(colour) => colour,
    }
}
