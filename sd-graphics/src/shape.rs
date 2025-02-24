use derivative::Derivative;
use egui::{
    Align2, Color32, CornerRadius, Id, Pos2, Rect, Response, Sense, Stroke, Vec2,
    emath::RectTransform,
    epaint::{CubicBezierShape, PathShape, RectShape},
    vec2,
};
use flo_curves::bezier::{Curve, solve_curve_for_t_along_axis};
use indexmap::IndexSet;
use sd_core::{
    common::Matchable,
    hypergraph::{
        generic::{Ctx, Node, Weight},
        traits::{Keyable, WireType, WithType, WithWeight},
    },
};

use crate::{
    common::{ShapeKind, TEXT_SIZE, TOLERANCE, to_coord2},
    renderable::RenderableGraph,
};

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum Shape<T: Ctx> {
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
        stroke: Option<Stroke>,
    },
    CircleFilled {
        center: Pos2,
        radius: u8,
        addr: T::Edge,
        coord: [usize; 2],
    },
    Operation {
        center: Pos2,
        radius: u8,
        addr: T::Operation,
        label: String,
        kind: ShapeKind,
        fill: Option<Color32>,
        stroke: Option<Stroke>,
    },
    Arrow {
        addr: T::Edge,
        to_add: Vec<Node<T>>,
        center: Pos2,
        upwards: bool,
        stroke: Option<Stroke>,
        height: f32,
    },
}

pub struct Shapes<T: Ctx> {
    pub shapes: Vec<Shape<T>>,
    pub size: Vec2,
}

impl<T: Ctx> Shape<T> {
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
            Shape::CircleFilled { center, radius, .. }
            | Shape::Operation { center, radius, .. } => {
                *center = transform.transform_pos(*center);
                *radius = (f32::from(*radius) * transform.scale().min_elem()) as u8 // NOTE(calintat): should this be length?
            }
            Shape::Arrow { center, height, .. } => {
                *center = transform.transform_pos(*center);
                *height *= transform.scale().min_elem();
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::too_many_lines)]
    pub(crate) fn collect_highlights<G>(
        &mut self,
        graph: &mut G,
        id: Id,
        ui: &egui::Ui,
        response: &Response,
        transform: &RectTransform,
        search: Option<&str>,
        highlight_op: &mut Option<T::Operation>,
        highlight_edges: &mut IndexSet<T::Edge>,
    ) where
        G: RenderableGraph<Ctx = T>,
        T::Operation: Matchable,
        T::Thunk: Matchable,
    {
        let bounds = *transform.to();
        let tolerance = TOLERANCE * transform.scale().min_elem();

        let bounding_box = self.bounding_box();

        if let Some(hover_pos) = response.hover_pos() {
            if self.contains_point(hover_pos, tolerance) {
                match self {
                    Shape::Line { addr, .. } | Shape::CubicBezier { addr, .. } => {
                        highlight_edges.insert(addr.clone());
                    }
                    _ => {}
                }
            }
        }
        match self {
            Shape::Line { .. } | Shape::CubicBezier { .. } => {}
            Shape::CircleFilled { addr, coord, .. } => {
                let circle_response = ui.interact(
                    bounding_box.intersect(bounds),
                    id.with((addr.key(), coord)),
                    Sense::click(),
                );
                if circle_response.clicked() {
                    graph.clicked_edge(addr.clone());
                }
            }
            Shape::Rectangle { addr, stroke, .. } => {
                let search_match = search.map(|x| addr.is_match(x)).unwrap_or_default();
                let addr: &_ = addr;
                let selected = graph.selected(Node::Thunk(addr.clone()));
                let thunk_response = ui.interact(
                    bounding_box.intersect(bounds),
                    id.with(addr.key()),
                    Sense::click(),
                );
                let mut new_stroke = ui
                    .style()
                    .interact_selectable(&thunk_response, selected)
                    .fg_stroke;
                if !selected {
                    new_stroke.color = new_stroke.color.gamma_multiply(0.35);
                }
                if search_match {
                    new_stroke.color = Color32::LIGHT_RED;
                }
                *stroke = Some(new_stroke);

                if thunk_response.clicked() {
                    graph.clicked_thunk(addr.clone(), true);
                }
                if thunk_response.secondary_clicked() {
                    graph.clicked_thunk(addr.clone(), false);
                }
            }
            Shape::Operation {
                addr, fill, stroke, ..
            } => {
                let search_match = search.map(|x| addr.is_match(x)).unwrap_or_default();
                let selected = graph.selected(Node::Operation(addr.clone()));
                let op_response = ui.interact(
                    bounding_box.intersect(bounds),
                    id.with(addr.key()),
                    Sense::click(),
                );
                if op_response.clicked() {
                    graph.clicked_operation(addr.clone(), true);
                }
                if op_response.secondary_clicked() {
                    graph.clicked_operation(addr.clone(), false);
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
                if search_match {
                    *fill = Some(Color32::DARK_RED);
                    stroke.as_mut().unwrap().color = Color32::LIGHT_RED;
                }

                if op_response.hovered() {
                    *highlight_op = Some(addr.clone());
                }
            }
            Shape::Arrow {
                addr,
                to_add,
                stroke,
                ..
            } => {
                let arrow_response = ui.interact(
                    bounding_box.intersect(bounds),
                    id.with(addr.key()),
                    Sense::click(),
                );

                if arrow_response.hovered() {
                    *stroke = Some(ui.style().interact(&arrow_response).fg_stroke);
                }

                if arrow_response.clicked() {
                    graph.extend(to_add.iter().cloned());
                }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn into_egui_shape(
        self,
        ui: &egui::Ui,
        transform: &RectTransform,
        highlight_edges: &IndexSet<T::Edge>,
    ) -> egui::Shape
    where
        Weight<T::Edge>: WithType,
    {
        let default_stroke = ui.visuals().noninteractive().fg_stroke;
        let fg_stroke = ui.visuals().widgets.hovered.fg_stroke;

        let wire_stroke = |highlighted: bool, wire_type: WireType| -> Stroke {
            let mut stroke = if highlighted {
                fg_stroke
            } else {
                default_stroke
            };
            match wire_type {
                WireType::Data => {}
                WireType::ControlFlow => {
                    if highlighted {
                        stroke.color = Color32::YELLOW;
                    } else {
                        stroke.color = Color32::GOLD;
                    }
                }
                WireType::SymName => {
                    if highlighted {
                        stroke.color = Color32::GREEN;
                    } else {
                        stroke.color = Color32::DARK_GREEN;
                    }
                }
            }
            stroke
        };

        match self {
            Shape::Line { start, end, addr } => {
                let stroke = wire_stroke(highlight_edges.contains(&addr), addr.weight().get_type());
                egui::Shape::line_segment([start, end], stroke)
            }
            Shape::CubicBezier { points, addr } => {
                let stroke = wire_stroke(highlight_edges.contains(&addr), addr.weight().get_type());

                let bezier = CubicBezierShape::from_points_stroke(
                    points,
                    false,
                    Color32::TRANSPARENT,
                    stroke,
                );
                egui::Shape::CubicBezier(bezier)
            }
            Shape::Rectangle { rect, stroke, .. } => egui::Shape::Rect(RectShape::new(
                rect,
                CornerRadius::ZERO,
                Color32::default(),
                stroke.unwrap_or(default_stroke),
                egui::StrokeKind::Outside,
            )),
            Shape::CircleFilled {
                center,
                radius,
                addr,
                ..
            } => {
                let stroke = wire_stroke(highlight_edges.contains(&addr), addr.weight().get_type());
                egui::Shape::circle_filled(center, f32::from(radius) / 20.0, stroke.color)
            }
            Shape::Operation {
                center,
                radius,
                label,
                kind,
                fill,
                stroke,
                ..
            } => {
                let rect = egui::Shape::Rect(RectShape::new(
                    Rect::from_center_size(
                        center,
                        (f32::from(radius) / 20.0)
                            * vec2(label.chars().count().max(1) as f32 + 1.0, 2.0),
                    ),
                    kind.into_rounding(radius),
                    fill.unwrap_or_default(),
                    stroke.unwrap_or(default_stroke),
                    egui::StrokeKind::Outside,
                ));
                let text_size: f32 = TEXT_SIZE * transform.scale().min_elem();
                if text_size <= 5.0 {
                    return rect;
                }
                let text = ui.fonts(|fonts| {
                    egui::Shape::text(
                        fonts,
                        center,
                        Align2::CENTER_CENTER,
                        label,
                        egui::FontId::monospace(text_size),
                        ui.visuals().strong_text_color(),
                    )
                });
                egui::Shape::Vec(vec![rect, text])
            }
            Shape::Arrow {
                center,
                upwards,
                height,
                stroke,
                ..
            } => {
                let stroke = stroke.unwrap_or(default_stroke);

                let fill = stroke.color;

                let vertical_offset = if upwards { height } else { -height };

                let left = center + Vec2::new(-height / 2.0, vertical_offset);
                let right = center + Vec2::new(height / 2.0, vertical_offset);

                // Paths should be clockwise
                let points = if upwards {
                    vec![left, center, right]
                } else {
                    vec![left, right, center]
                };

                egui::Shape::Path(PathShape {
                    points,
                    closed: true,
                    fill,
                    stroke: stroke.into(),
                })
            }
        }
    }

    pub fn center(&self) -> Pos2 {
        match self {
            Shape::Line { start, end, .. } => *start + (*end - *start) / 2.0,
            Shape::CubicBezier { points, .. } => points[0] + (points[3] - points[0]) / 2.0,
            Shape::Rectangle { rect, .. } => rect.center(),
            Shape::CircleFilled { center, .. }
            | Shape::Operation { center, .. }
            | Shape::Arrow { center, .. } => *center,
        }
    }

    pub(crate) fn bounding_box(&self) -> Rect {
        match self {
            Shape::Line { start, end, .. } => Rect::from_two_pos(*start, *end),
            Shape::CubicBezier { points, .. } => Rect::from_points(points),
            Shape::Rectangle { rect, .. } => *rect,
            Shape::CircleFilled { center, radius, .. } => {
                Rect::from_center_size(*center, Vec2::splat(f32::from(*radius) / 10.0))
            }
            Shape::Operation {
                center,
                radius,
                label,
                ..
            } => Rect::from_center_size(
                *center,
                (f32::from(*radius) / 20.0) * vec2(label.chars().count() as f32 + 1.0, 2.0),
            ),
            Shape::Arrow { center, height, .. } => {
                Rect::from_center_size(*center, Vec2::splat(*height * 5.0))
            }
        }
    }

    pub fn contains_point(&self, point: Pos2, tolerance: f32) -> bool {
        match self {
            Shape::Line { start, end, .. } => {
                let start = *start;
                let end = *end;
                let distance = if start == end {
                    (start - point).length()
                } else {
                    let vec = end - start;
                    let t = (point - start).dot(vec) / vec.length_sq();
                    let t = t.clamp(0.0, 1.0);
                    let projected = start + vec * t;
                    (projected - point).length()
                };
                distance < tolerance
            }
            Shape::CubicBezier { points, .. } => solve_curve_for_t_along_axis(
                &Curve {
                    start_point: to_coord2(points[0]),
                    end_point: to_coord2(points[3]),
                    control_points: (to_coord2(points[1]), to_coord2(points[2])),
                },
                &to_coord2(point),
                f64::from(tolerance),
            )
            .is_some(),
            _ => false,
        }
    }
}

impl<T: Ctx> Matchable for Shape<T>
where
    T::Operation: Matchable,
    T::Thunk: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        match self {
            Self::Operation { addr, .. } => addr.is_match(query),
            Self::Rectangle { addr, .. } => addr.is_match(query),
            _ => false,
        }
    }
}
