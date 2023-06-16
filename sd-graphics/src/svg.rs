use egui::{emath::RectTransform, Pos2, Rect};
use sd_core::common::Addr;
use svg::{
    node::element::{path::Data, Circle, Line, Path, Rectangle, Text},
    Document, Node,
};

use crate::shape::{Shape, Shapes};

impl<T: Addr> Shape<T> {
    pub(crate) fn to_svg(&self) -> Box<dyn Node> {
        match self {
            Self::Circle { center, radius, .. } => Box::new(
                Circle::new()
                    .set("cx", center.x)
                    .set("cy", center.y)
                    .set("r", *radius)
                    .set("fill", "white")
                    .set("stroke", "black")
                    .set("stroke-width", 1),
            ),
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
            Self::Text { text: str, center } => Box::new({
                Text::new()
                    .set("x", center.x)
                    .set("y", center.y)
                    .set("font-size", 28)
                    .set("font-family", "monospace")
                    .set("text-anchor", "middle")
                    .set("dominant-baseline", "middle")
                    .add(svg::node::Text::new(str))
            }),
        }
    }
}

impl<T: Addr> Shapes<T> {
    const SCALE: f32 = 100.0;

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
