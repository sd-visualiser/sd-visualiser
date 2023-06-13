use std::{
    collections::HashSet,
    hash::{BuildHasher, Hash},
};

use derivative::Derivative;
use egui::{
    epaint::{CircleShape, CubicBezierShape, RectShape},
    Align2, Color32, Id, Pos2, Rect, Response, Rounding, Sense, Stroke, Vec2,
};
use indexmap::IndexSet;
use sd_core::{
    common::Addr,
    hypergraph::{Operation, Thunk},
};

use crate::{
    common::{
        ContainsPoint, DummyValue, Transform, BOX_SIZE, RADIUS_OPERATION, TEXT_SIZE, TOLERANCE,
    },
    expanded::Expanded,
};

#[derive(Derivative)]
#[derivative(Clone(bound = "T::OutPort: Clone, T::Thunk: Clone, T::Operation: Clone"))]
pub enum Shape<T: Addr> {
    Line {
        start: Pos2,
        end: Pos2,
        addr: T::OutPort,
    },
    CubicBezier {
        points: [Pos2; 4],
        addr: T::OutPort,
    },
    Rectangle {
        rect: Rect,
        addr: T::Thunk,
    },
    CircleFilled {
        center: Pos2,
        radius: f32,
    },
    Circle {
        center: Pos2,
        addr: T::Operation,
    },
    Text {
        text: String,
        center: Pos2,
    },
}

pub struct Shapes<T: Addr> {
    pub shapes: Vec<Shape<T>>,
    pub width: f32,
    pub height: f32,
}

impl<V, E> Shape<(V, Option<E>)> {
    pub(crate) fn apply_transform(&mut self, transform: &Transform) {
        match self {
            Shape::Line { start, end, .. } => {
                *start = transform.apply(*start);
                *end = transform.apply(*end);
            }
            Shape::CubicBezier { points, .. } => {
                points[0] = transform.apply(points[0]);
                points[1] = transform.apply(points[1]);
                points[2] = transform.apply(points[2]);
                points[3] = transform.apply(points[3]);
            }
            Shape::Rectangle { rect, .. } => {
                rect.min = transform.apply(rect.min);
                rect.max = transform.apply(rect.max);
            }
            Shape::CircleFilled { center, radius } => {
                *center = transform.apply(*center);
                *radius *= transform.scale;
            }
            Shape::Circle { center, .. } | Shape::Text { center, .. } => {
                *center = transform.apply(*center);
            }
        }
    }

    pub(crate) fn collect_hovers(
        &self,
        response: &Response,
        transform: &Transform,
        hover_points: &mut IndexSet<DummyValue<V, E>>,
    ) where
        V: Clone + PartialEq + Eq + Hash,
        E: Clone + PartialEq + Eq + Hash,
    {
        macro_rules! check_hover {
            ($path:expr, $port:expr) => {
                if let Some(hover_pos) = response.ctx.input(|i| i.pointer.hover_pos()) {
                    if response.rect.contains(hover_pos) {
                        if $path.contains_point(hover_pos, TOLERANCE * transform.scale) {
                            hover_points.insert(DummyValue::from_port($port));
                        }
                    }
                }
            };
        }

        match self {
            Shape::Line { start, end, addr } => {
                check_hover!([*start, *end], addr);
            }
            Shape::CubicBezier { points, addr } => {
                let bezier = CubicBezierShape::from_points_stroke(
                    *points,
                    false,
                    Color32::TRANSPARENT,
                    Stroke::default(),
                );
                check_hover!(bezier, addr);
            }
            _ => {}
        }
    }

    pub(crate) fn into_egui_shape<S>(
        self,
        ui: &egui::Ui,
        transform: &Transform,
        expanded: &mut Expanded<Thunk<V, Option<E>>>,
        selections: &mut HashSet<Operation<V, Option<E>>, S>,
    ) -> egui::Shape
    where
        V: Clone + PartialEq + Eq + Hash,
        E: Clone + PartialEq + Eq + Hash,
        S: BuildHasher,
    {
        let default_stroke = ui.visuals().noninteractive().fg_stroke;
        let default_color = default_stroke.color;

        match self {
            Shape::Line { start, end, .. } => {
                egui::Shape::line_segment([start, end], default_stroke)
            }
            Shape::CubicBezier { points, .. } => {
                let bezier = CubicBezierShape::from_points_stroke(
                    points,
                    false,
                    Color32::TRANSPARENT,
                    default_stroke,
                );
                egui::Shape::CubicBezier(bezier)
            }
            Shape::Rectangle { rect, addr } => {
                let thunk_response = ui.interact(
                    rect.intersect(transform.bounds),
                    Id::new(&addr),
                    Sense::click(),
                );
                if thunk_response.clicked() {
                    expanded[&addr] = !expanded[&addr];
                }
                let mut stroke = ui.style().interact(&thunk_response).fg_stroke;
                if expanded[&addr] {
                    stroke.color = stroke.color.gamma_multiply(0.35);
                }
                egui::Shape::Rect(RectShape {
                    rect,
                    rounding: Rounding::none(),
                    fill: if expanded[&addr] {
                        Color32::default()
                    } else {
                        ui.style().interact(&thunk_response).bg_fill
                    },
                    stroke,
                })
            }
            Shape::CircleFilled { center, radius } => {
                egui::Shape::circle_filled(center, radius, default_color)
            }
            Shape::Circle { center, addr } => {
                let selected = selections.contains(&addr);
                let op_rect = Rect::from_center_size(center, BOX_SIZE * transform.scale);
                let op_response = ui.interact(
                    op_rect.intersect(transform.bounds),
                    Id::new(&addr),
                    Sense::click(),
                );
                if op_response.clicked() && !selections.remove(&addr) {
                    selections.insert(addr.clone());
                }
                egui::Shape::Circle(CircleShape {
                    center,
                    radius: RADIUS_OPERATION * transform.scale,
                    fill: ui
                        .style()
                        .interact_selectable(&op_response, selected)
                        .bg_fill,
                    stroke: ui
                        .style()
                        .interact_selectable(&op_response, selected)
                        .fg_stroke,
                })
            }
            Shape::Text { text, center } => {
                if transform.scale > 10.0 {
                    ui.fonts(|fonts| {
                        egui::Shape::text(
                            fonts,
                            center,
                            Align2::CENTER_CENTER,
                            text,
                            egui::FontId::monospace(TEXT_SIZE * transform.scale),
                            ui.visuals().strong_text_color(),
                        )
                    })
                } else {
                    egui::Shape::Noop
                }
            }
        }
    }

    pub(crate) fn bounding_box(&self) -> Rect {
        match self {
            Shape::Line { start, end, .. } => Rect::from_two_pos(*start, *end),
            Shape::CubicBezier { points, .. } => Rect::from_points(points),
            Shape::Rectangle { rect, .. } => *rect,
            Shape::CircleFilled { center, radius } => {
                Rect::from_center_size(*center, Vec2::new(*radius, *radius))
            }
            Shape::Circle { center, .. } => {
                Rect::from_center_size(*center, Vec2::new(RADIUS_OPERATION, RADIUS_OPERATION))
            }
            Shape::Text { center, .. } => {
                Rect::from_center_size(*center, Vec2::new(f32::INFINITY, 1.0))
            }
        }
    }
}
