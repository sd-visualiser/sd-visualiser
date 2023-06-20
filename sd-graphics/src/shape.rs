use std::{collections::HashSet, hash::BuildHasher};

use derivative::Derivative;
use egui::{
    emath::RectTransform,
    epaint::{CircleShape, CubicBezierShape, RectShape},
    Align2, Color32, Id, Pos2, Rect, Response, Rounding, Sense, Stroke, Vec2,
};
use indexmap::IndexSet;
use sd_core::common::Addr;

use crate::{
    common::{ContainsPoint, TEXT_SIZE, TOLERANCE},
    expanded::Expanded,
};

#[derive(Derivative)]
#[derivative(Clone(bound = "T::Edge: Clone, T::Thunk: Clone, T::Operation: Clone"))]
pub enum Shape<T: Addr> {
    Line {
        start: Pos2,
        end: Pos2,
        addr: T::Edge,
    },
    CubicBezier {
        points: [Pos2; 4],
        addr: T::Edge,
    },
    Rectangle {
        rect: Rect,
        addr: T::Thunk,
        fill: Option<Color32>,
        stroke: Option<Stroke>,
    },
    CircleFilled {
        center: Pos2,
        radius: f32,
        addr: T::Edge,
    },
    Circle {
        center: Pos2,
        radius: f32,
        addr: T::Operation,
        fill: Option<Color32>,
        stroke: Option<Stroke>,
    },
    Text {
        text: String,
        center: Pos2,
    },
}

pub struct Shapes<T: Addr> {
    pub shapes: Vec<Shape<T>>,
    pub size: Vec2,
}

impl<T: Addr> Shape<T> {
    pub(crate) fn apply_transform(&mut self, transform: &RectTransform) {
        match self {
            Shape::Line { start, end, .. } => {
                *start = transform.transform_pos(*start);
                *end = transform.transform_pos(*end);
            }
            Shape::CubicBezier { points, .. } => {
                for point in points {
                    *point = transform.transform_pos(*point);
                }
            }
            Shape::Rectangle { rect, .. } => {
                *rect = transform.transform_rect(*rect);
            }
            Shape::CircleFilled { center, radius, .. } | Shape::Circle { center, radius, .. } => {
                *center = transform.transform_pos(*center);
                *radius *= transform.scale().min_elem(); // NOTE(calintat): should this be length?
            }
            Shape::Text { center, .. } => {
                *center = transform.transform_pos(*center);
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn collect_highlights<S>(
        &mut self,
        ui: &egui::Ui,
        response: &Response,
        transform: &RectTransform,
        highlight_op: &mut Option<T::Operation>,
        highlight_thunk: &mut Option<T::Thunk>,
        highlight_edges: &mut IndexSet<T::Edge>,
        expanded: &mut Expanded<T::Thunk>,
        selection: &mut Option<&mut HashSet<T::Operation, S>>,
    ) where
        S: BuildHasher,
    {
        let bounds = *transform.to();
        let tolerance = TOLERANCE * transform.scale().min_elem();

        let bounding_box = self.bounding_box();
        match self {
            Shape::Line { start, end, addr } => {
                if let Some(hover_pos) = response.hover_pos() {
                    if [*start, *end].contains_point(hover_pos, tolerance) {
                        highlight_edges.insert(addr.clone());
                    }
                }
            }
            Shape::CubicBezier { points, addr } => {
                if let Some(hover_pos) = response.hover_pos() {
                    let bezier = CubicBezierShape::from_points_stroke(
                        *points,
                        false,
                        Color32::TRANSPARENT,
                        Stroke::default(),
                    );
                    if bezier.contains_point(hover_pos, tolerance) {
                        highlight_edges.insert(addr.clone());
                    }
                }
            }
            Shape::Rectangle {
                addr, fill, stroke, ..
            } => {
                let addr: &_ = addr;
                let thunk_response = ui.interact(
                    bounding_box.intersect(bounds),
                    Id::new(addr),
                    Sense::click(),
                );
                let mut new_stroke = ui.style().interact(&thunk_response).fg_stroke;
                if expanded[addr] {
                    new_stroke.color = new_stroke.color.gamma_multiply(0.35);
                }
                *stroke = Some(new_stroke);
                if !expanded[addr] {
                    *fill = Some(ui.style().interact(&thunk_response).bg_fill);
                    if thunk_response.hovered() {
                        *highlight_thunk = Some(addr.clone());
                    }
                }
                if thunk_response.clicked() {
                    expanded[addr] = !expanded[addr];
                }
            }
            Shape::Circle {
                addr, fill, stroke, ..
            } => {
                let selected = selection.as_ref().map_or(false, |s| s.contains(addr));
                let op_response = ui.interact(
                    bounding_box.intersect(bounds),
                    Id::new(&addr),
                    Sense::click().union(Sense::hover()),
                );
                if let Some(s) = selection.as_mut() {
                    if op_response.clicked() && !s.remove(addr) {
                        s.insert(addr.clone());
                    }
                }
                *fill = Some(
                    ui.style()
                        .interact_selectable(&op_response, selected)
                        .bg_fill,
                );
                *stroke = Some(
                    ui.style()
                        .interact_selectable(&op_response, selected)
                        .fg_stroke,
                );
                if op_response.hovered() {
                    *highlight_op = Some(addr.clone());
                }
            }
            _ => {}
        }
    }

    pub(crate) fn into_egui_shape(
        self,
        ui: &egui::Ui,
        transform: &RectTransform,
        highlight_edges: &IndexSet<T::Edge>,
    ) -> egui::Shape {
        let default_stroke = ui.visuals().noninteractive().fg_stroke;

        match self {
            Shape::Line { start, end, addr } => {
                let stroke = if highlight_edges.contains(&addr) {
                    ui.style().visuals.widgets.hovered.fg_stroke
                } else {
                    default_stroke
                };
                egui::Shape::line_segment([start, end], stroke)
            }
            Shape::CubicBezier { points, addr } => {
                let stroke = if highlight_edges.contains(&addr) {
                    ui.style().visuals.widgets.hovered.fg_stroke
                } else {
                    default_stroke
                };
                let bezier = CubicBezierShape::from_points_stroke(
                    points,
                    false,
                    Color32::TRANSPARENT,
                    stroke,
                );
                egui::Shape::CubicBezier(bezier)
            }
            Shape::Rectangle {
                rect, fill, stroke, ..
            } => egui::Shape::Rect(RectShape {
                rect,
                rounding: Rounding::none(),
                fill: fill.unwrap_or(Color32::default()),
                stroke: stroke.unwrap_or(default_stroke),
            }),
            Shape::CircleFilled {
                center,
                radius,
                addr,
            } => {
                let stroke = if highlight_edges.contains(&addr) {
                    ui.style().visuals.widgets.hovered.fg_stroke
                } else {
                    default_stroke
                };
                egui::Shape::circle_filled(center, radius, stroke.color)
            }
            Shape::Circle {
                center,
                radius,
                fill,
                stroke,
                ..
            } => egui::Shape::Circle(CircleShape {
                center,
                radius,
                fill: fill.unwrap_or(Color32::default()),
                stroke: stroke.unwrap_or(default_stroke),
            }),
            Shape::Text { text, center } => {
                let size = TEXT_SIZE * transform.scale().min_elem();
                if size > 5.0 {
                    ui.fonts(|fonts| {
                        egui::Shape::text(
                            fonts,
                            center,
                            Align2::CENTER_CENTER,
                            text,
                            egui::FontId::monospace(size),
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
            Shape::CircleFilled { center, radius, .. } | Shape::Circle { center, radius, .. } => {
                Rect::from_center_size(*center, Vec2::splat(*radius * 2.0))
            }
            Shape::Text { center, .. } => {
                Rect::from_center_size(*center, Vec2::new(f32::INFINITY, 1.0))
            }
        }
    }
}
