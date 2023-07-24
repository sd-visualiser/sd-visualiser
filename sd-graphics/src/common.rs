use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

use derivative::Derivative;
use egui::{vec2, Pos2, Vec2};
use flo_curves::Coord2;
use pretty::RcDoc;
use sd_core::{
    common::Addr,
    graph::{Name, Op},
    hypergraph::{
        subgraph::Mapping,
        traits::{EdgeLike, NodeLike, WithWeight},
    },
    language::Language,
    prettyprinter::{paran_list, PrettyPrint},
    weak_map::WeakMap,
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
    Thunk(T::Addr),
    FreeVar(T::Var),
    BoundVar(T::VarDef),
    Operation(T::Op, Vec<EdgeLabel<T>>),
}

impl<T: Language> EdgeLabel<T> {
    pub(crate) fn from_edge<U>(edge: &U::Edge) -> Self
    where
        U: Addr,
        U::Edge: EdgeLike<T = U> + WithWeight<Weight = Name<T>>,
        U::Operation: NodeLike<T = U> + WithWeight<Weight = Op<T>>,
    {
        match edge.weight() {
            Name::Op => {
                let op: U::Operation = edge.source().unwrap().try_into().ok().unwrap();
                Self::Operation(
                    op.weight().0.clone(),
                    op.inputs()
                        .map(|edge| Self::from_edge::<U>(&edge))
                        .collect(),
                )
            }
            Name::Thunk(addr) => Self::Thunk(addr.clone()),
            Name::FreeVar(var) => Self::FreeVar(var.clone()),
            Name::BoundVar(def) => Self::BoundVar(def.clone()),
        }
    }
}

impl<T: Language> PrettyPrint for EdgeLabel<T>
where
    T::Op: PrettyPrint,
    T::Var: PrettyPrint,
    T::Addr: Display,
    T::VarDef: PrettyPrint,
{
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
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

#[derive(Derivative)]
#[derivative(
    Clone(bound = "T::Thunk: Clone, T::Edge: Clone"),
    Hash(bound = ""),
    Default(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
pub struct GraphMetadata<T: Addr> {
    pub expanded: WeakMap<T::Thunk, bool>,
    pub mapping: Option<Mapping<T>>,
}

impl<T: Addr> Index<&T::Thunk> for GraphMetadata<T> {
    type Output = bool;

    fn index(&self, index: &T::Thunk) -> &Self::Output {
        if let Some(map) = &self.mapping {
            &self.expanded[&map.thunk_mapping[index]]
        } else {
            &self.expanded[index]
        }
    }
}

impl<T: Addr> IndexMut<&T::Thunk> for GraphMetadata<T> {
    fn index_mut(&mut self, index: &T::Thunk) -> &mut Self::Output {
        if let Some(map) = &self.mapping {
            &mut self.expanded[&map.thunk_mapping[index]]
        } else {
            &mut self.expanded[index]
        }
    }
}
