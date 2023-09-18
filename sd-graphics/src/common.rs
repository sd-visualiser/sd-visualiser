use egui::{vec2, Pos2, Vec2};
use flo_curves::Coord2;

pub const RADIUS_ARG: f32 = 0.05;
pub const RADIUS_COPY: f32 = 0.1;
pub const BOX_SIZE: Vec2 = vec2(0.4, 0.4);
pub const TOLERANCE: f32 = 0.3;
pub const TEXT_SIZE: f32 = 0.28;
pub const RADIUS_OPERATION: f32 = 0.2;

// pub trait ContainsPoint {
//     // Check if a point lies on a line or curve (with the given tolerance).
//     fn contains_point(self, point: Pos2, tolerance: f32) -> bool;
// }

// impl ContainsPoint for [Pos2; 2] {
//     fn contains_point(self, point: Pos2, tolerance: f32) -> bool {

//     }
// }

// const SAMPLES: u8 = 100;

// impl ContainsPoint for CubicBezierShape {
//     fn contains_point(self, point: Pos2, tolerance: f32) -> bool {
//         (0..=SAMPLES).any(|t| {
//             let t = f32::from(t) / f32::from(SAMPLES);
//             let p = self.sample(t);
//             p.distance(point) < tolerance
//         })
//     }
// }

pub(crate) fn to_coord2(pos2: Pos2) -> Coord2 {
    Coord2(f64::from(pos2.x), f64::from(pos2.y))
}
