use std::fmt::Display;

use derivative::Derivative;
use egui::{epaint::CubicBezierShape, vec2, Pos2, Vec2};
use pretty::RcDoc;
use sd_core::{
    graph::{Name, Op},
    hypergraph::{Edge, Node},
    language::Language,
    prettyprinter::{paran_list, PrettyPrint},
};

pub(crate) const RADIUS_ARG: f32 = 0.05;
pub(crate) const RADIUS_COPY: f32 = 0.1;
pub(crate) const BOX_SIZE: Vec2 = vec2(0.4, 0.4);
pub(crate) const TOLERANCE: f32 = 0.1;
pub(crate) const TEXT_SIZE: f32 = 0.28;
pub(crate) const RADIUS_OPERATION: f32 = 0.2;

/// A dummy value is like a `spartan::Value` but with anonymous thunks and (possibly) free variables.
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum DummyValue<T: Language> {
    Thunk(T::Addr),
    FreeVar(T::Var),
    BoundVar(T::VarDef),
    Operation(T::Op, Vec<DummyValue<T>>),
}

impl<T: Language> DummyValue<T> {
    pub(crate) fn from_port(out_port: &Edge<Op<T>, Name<T>>) -> Self {
        match out_port.weight() {
            Name::Op => match out_port.node() {
                Some(Node::Operation(op)) => Self::Operation(
                    op.weight().0.clone(),
                    op.inputs()
                        .map(|out_port| Self::from_port(&out_port))
                        .collect(),
                ),
                _ => unreachable!(),
            },
            Name::Thunk(addr) => Self::Thunk(addr.clone()),
            Name::FreeVar(var) => Self::FreeVar(var.clone()),
            Name::BoundVar(def) => Self::BoundVar(def.clone()),
        }
    }
}

impl<T: Language> PrettyPrint for DummyValue<T>
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

pub(crate) trait ContainsPoint {
    // Check if a point lies on a line or curve (with the given tolerance).
    fn contains_point(self, point: Pos2, tolerance: f32) -> bool;
}

impl ContainsPoint for [Pos2; 2] {
    fn contains_point(self, point: Pos2, tolerance: f32) -> bool {
        let [from, to] = self;
        let distance = if from == to {
            (from - point).length()
        } else {
            let vec = to - from;
            let t = (point - from).dot(vec) / vec.length_sq();
            let t = t.clamp(0.0, 1.0);
            let projected = from + vec * t;
            (projected - point).length()
        };
        distance < tolerance
    }
}

const SAMPLES: u8 = 100;

impl ContainsPoint for CubicBezierShape {
    fn contains_point(self, point: Pos2, tolerance: f32) -> bool {
        (0..=SAMPLES).any(|t| {
            let t = f32::from(t) / f32::from(SAMPLES);
            let p = self.sample(t);
            p.distance(point) < tolerance
        })
    }
}
