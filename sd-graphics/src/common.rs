use egui::{Pos2, Rounding};
use flo_curves::Coord2;
use sd_core::hypergraph::{
    self,
    adapter::{collapse::CollapseOperation, cut::CutOperation},
    generic::{Ctx, Node, Operation},
    subgraph::SubOperation,
    traits::Graph,
    Weight,
};

pub const RADIUS_ARG: f32 = 0.05;
pub const RADIUS_COPY: f32 = 0.1;
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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ShapeKind {
    Squircle,
    Square,
    BulletUp,
    BulletDown,
}

impl ShapeKind {
    #[must_use]
    pub fn into_rounding(self, radius: f32) -> Rounding {
        match self {
            ShapeKind::Square => Rounding::ZERO,
            ShapeKind::Squircle => Rounding::same(radius),
            ShapeKind::BulletUp => Rounding {
                nw: radius,
                ne: radius,
                sw: 0.0,
                se: 0.0,
            },
            ShapeKind::BulletDown => Rounding {
                nw: 0.0,
                ne: 0.0,
                sw: radius,
                se: radius,
            },
        }
    }
}

pub trait Shapeable {
    fn to_shape(&self) -> ShapeKind;
}

impl<G: Graph> Shapeable for CollapseOperation<G>
where
    Operation<G::Ctx>: Shapeable,
{
    fn to_shape(&self) -> ShapeKind {
        match self.inner() {
            Node::Operation(op) => op.to_shape(),
            Node::Thunk(_) => ShapeKind::Square,
        }
    }
}

impl<G: Graph> Shapeable for CutOperation<G>
where
    Operation<G::Ctx>: Shapeable,
{
    fn to_shape(&self) -> ShapeKind {
        match self {
            Self::Inner { op, .. } => op.to_shape(),
            Self::Reuse { .. } => ShapeKind::BulletUp,
            Self::Store { .. } => ShapeKind::BulletDown,
        }
    }
}

impl<T: Ctx> Shapeable for SubOperation<T>
where
    T::Operation: Shapeable,
{
    fn to_shape(&self) -> ShapeKind {
        self.inner().to_shape()
    }
}

impl<W: Weight> Shapeable for hypergraph::Operation<W> {
    fn to_shape(&self) -> ShapeKind {
        ShapeKind::Squircle
    }
}
