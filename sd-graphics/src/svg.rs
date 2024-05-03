use egui::{emath::RectTransform, Pos2, Rect};
use sd_core::hypergraph::generic::Ctx;
use svg::{
    node::element::{path::Data, Anchor, Circle, Group, Line, Path, Rectangle, Text},
    Document, Node,
};

use crate::shape::{Shape, Shapes};

impl<T: Ctx> Shape<T> {
    pub(crate) fn to_svg(&self) -> Box<dyn Node> {
        match self {
            Self::Operation {
                center,
                radius,
                label,
                ..
            } => {
                let x_size = radius * (label.chars().count().max(1) as f32 + 1.0);
                Box::new(
                    Group::new()
                        .add(
                            Rectangle::new()
                                .set("x", center.x - x_size / 2.0)
                                .set("y", center.y - radius)
                                .set("width", x_size)
                                .set("height", radius * 2.0)
                                .set("rx", *radius)
                                .set("ry", *radius)
                                .set("fill", "white")
                                .set("stroke", "black")
                                .set("stroke-width", 1),
                        )
                        .add(
                            Anchor::new()
                                .set(
                                    "href",
                                    format!("https://alexarice.github.io/catt-agda/{label}.html"),
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
                        ),
                )
            }
            Self::CircleFilled { center, radius, .. } => Box::new(
                Circle::new()
                    .set("cx", center.x)
                    .set("cy", center.y)
                    .set("r", *radius)
                    .set("fill", "black"),
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
            Self::Line { start, end, .. } => Box::new(
                Line::new()
                    .set("x1", start.x)
                    .set("y1", start.y)
                    .set("x2", end.x)
                    .set("y2", end.y)
                    .set("stroke", "black")
                    .set("stroke-width", "1"),
            ),
            Self::CubicBezier { points, .. } => Box::new({
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
                    .set("stroke", "black")
                    .set("stroke-width", 1)
            }),
            Self::Arrow { .. } => {
                panic!("Arrows should not be in svgs")
            }
        }
    }
}

impl<T: Ctx> Shapes<T> {
    const SCALE: f32 = 50.0;

    #[must_use]
    pub fn to_svg(&self) -> Document {
        let mut document = Document::new()
            .set("width", self.size.x * Self::SCALE)
            .set("height", self.size.y * Self::SCALE);

        let scale = RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, self.size / Self::SCALE),
            Rect::from_min_size(Pos2::ZERO, self.size),
        );

        for shape in &self.shapes {
            let mut shape = shape.clone();
            shape.apply_transform(&scale);
            document = document.add(shape.to_svg());
        }

        document
    }
}
