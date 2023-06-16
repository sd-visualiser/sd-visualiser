use std::{collections::HashSet, hash::BuildHasher};

use derivative::Derivative;
use egui::{
    emath::RectTransform,
    epaint::{CircleShape, CubicBezierShape, RectShape},
    show_tooltip_at_pointer, Align2, Color32, Id, Pos2, Rect, Response, Rounding, Sense, Stroke,
    Vec2,
};
use indexmap::IndexSet;
use sd_core::{
    common::Addr,
    decompile::decompile,
    graph::{Name, Op},
    hypergraph::{Operation, OutPort, Thunk},
    language::{Expr, Language},
    prettyprinter::PrettyPrint,
};

use crate::{
    common::{ContainsPoint, DummyValue, TEXT_SIZE, TOLERANCE},
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
        fill: Option<Color32>,
        stroke: Option<Stroke>,
    },
    CircleFilled {
        center: Pos2,
        radius: f32,
        addr: T::OutPort,
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
}

impl<T: Language> Shape<(Op<T>, Name<T>)> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn collect_hovers<S>(
        &mut self,
        ui: &egui::Ui,
        response: &Response,
        transform: &RectTransform,
        hover_points: &mut IndexSet<DummyValue<T>>,
        highlight_ports: &mut HashSet<OutPort<Op<T>, Name<T>>>,
        expanded: &mut Expanded<Thunk<Op<T>, Name<T>>>,
        selections: &mut HashSet<Operation<Op<T>, Name<T>>, S>,
        operation_hovered: &mut bool,
    ) where
        S: BuildHasher,
        Expr<T>: PrettyPrint,
    {
        let bounds = *transform.to();
        let tolerance = TOLERANCE * transform.scale().min_elem();

        let bounding_box = self.bounding_box();
        let hover_pos = response
            .ctx
            .input(|i| i.pointer.hover_pos())
            .filter(|x| response.rect.contains(*x));

        match self {
            Shape::Line { start, end, addr } => {
                if let Some(hover_pos) = hover_pos {
                    if [*start, *end].contains_point(hover_pos, tolerance) {
                        hover_points.insert(DummyValue::from_port(addr));
                        highlight_ports.insert(addr.clone());
                    }
                }
            }
            Shape::CubicBezier { points, addr } => {
                if let Some(hover_pos) = hover_pos {
                    let bezier = CubicBezierShape::from_points_stroke(
                        *points,
                        false,
                        Color32::TRANSPARENT,
                        Stroke::default(),
                    );
                    if bezier.contains_point(hover_pos, tolerance) {
                        hover_points.insert(DummyValue::from_port(addr));
                        highlight_ports.insert(addr.clone());
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
                        *operation_hovered = true;
                        highlight_ports
                            .extend(addr.inputs().map(|port| port.link()).chain(addr.outputs()));
                        show_tooltip_at_pointer(ui.ctx(), egui::Id::new("hover_tooltip"), |ui| {
                            ui.label(
                                decompile(addr)
                                    .map(|x| x.to_pretty())
                                    .unwrap_or("<Thunk>".to_owned()),
                            );
                        });
                    }
                }
                if thunk_response.clicked() {
                    expanded[addr] = !expanded[addr];
                }
            }
            Shape::Circle {
                addr, fill, stroke, ..
            } => {
                let selected = selections.contains(addr);
                let op_response = ui.interact(
                    bounding_box.intersect(bounds),
                    Id::new(&addr),
                    Sense::click().union(Sense::hover()),
                );
                if op_response.clicked() && !selections.remove(addr) {
                    selections.insert(addr.clone());
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
                    *operation_hovered = true;
                    highlight_ports
                        .extend(addr.inputs().map(|port| port.link()).chain(addr.outputs()));
                }
            }
            _ => {}
        }
    }
}

impl<T: Addr> Shape<T> {
    pub(crate) fn into_egui_shape(
        self,
        ui: &egui::Ui,
        transform: &RectTransform,
        highlight_ports: &HashSet<T::OutPort>,
    ) -> egui::Shape {
        let default_stroke = ui.visuals().noninteractive().fg_stroke;

        match self {
            Shape::Line { start, end, addr } => {
                let stroke = if highlight_ports.contains(&addr) {
                    ui.style().visuals.widgets.hovered.fg_stroke
                } else {
                    default_stroke
                };
                egui::Shape::line_segment([start, end], stroke)
            }
            Shape::CubicBezier { points, addr } => {
                let stroke = if highlight_ports.contains(&addr) {
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
                let stroke = if highlight_ports.contains(&addr) {
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
