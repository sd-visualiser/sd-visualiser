use derivative::Derivative;
use egui::{vec2, Pos2, Vec2};
use flo_curves::Coord2;
use pretty::RcDoc;
use sd_core::{
    graph::{Elem, Name},
    hypergraph::{
        generic::{Ctx, Node},
        traits::{EdgeLike, NodeLike, WithWeight},
    },
    language::Language,
    prettyprinter::{paran_list, PrettyPrint},
};

pub const RADIUS_ARG: f32 = 0.05;
pub const RADIUS_COPY: f32 = 0.1;
pub const BOX_SIZE: Vec2 = vec2(0.4, 0.4);
pub const TOLERANCE: f32 = 0.3;
pub const TEXT_SIZE: f32 = 0.28;
pub const RADIUS_OPERATION: f32 = 0.2;

/// Edge label is like `Name` but records the arguments to operations.
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum EdgeLabel<T: Language> {
    Fresh,
    Thunk(T::Addr),
    FreeVar(T::Var),
    BoundVar(T::VarDef),
    Operation(T::Op, Vec<EdgeLabel<T>>),
}

impl<T: Language> EdgeLabel<T> {
    pub(crate) fn from_edge<U>(edge: &U::Edge) -> Self
    where
        U: Ctx,
        U::Edge: WithWeight<Weight = Name<T>>,
        U::Operation: WithWeight<Weight = Elem<T>>,
        U::Thunk: WithWeight<Weight = Elem<T>>,
    {
        match edge.weight() {
            Name::Nil => match edge.source() {
                None => Self::Fresh,
                Some(Node::Operation(op)) => Self::Operation(
                    op.weight().into_op(),
                    op.inputs()
                        .map(|edge| Self::from_edge::<U>(&edge))
                        .collect(),
                ),
                Some(Node::Thunk(thunk)) => Self::Thunk(thunk.weight().into_addr()),
            },
            Name::FreeVar(var) => Self::FreeVar(var),
            Name::BoundVar(def) => Self::BoundVar(def),
        }
    }
}

impl<T: Language> PrettyPrint for EdgeLabel<T> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Fresh => RcDoc::text("?"),
            Self::Thunk(addr) => {
                let addr = addr.to_string();
                if addr.is_empty() {
                    RcDoc::text("thunk")
                } else {
                    RcDoc::text("thunk")
                        .append(RcDoc::space())
                        .append(RcDoc::text(addr))
                }
            }
            Self::FreeVar(var) => var.to_doc(),
            Self::BoundVar(def) => def.to_doc(),
            Self::Operation(op, vs) => {
                if vs.is_empty() {
                    op.to_doc()
                } else {
                    op.to_doc().append(paran_list(vs))
                }
            }
        }
    }
}

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
