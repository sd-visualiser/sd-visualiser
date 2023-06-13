use derivative::Derivative;
use egui::{emath::RectTransform, epaint::CubicBezierShape, vec2, Pos2, Rect, Vec2};
use pretty::RcDoc;
use sd_core::{
    graph::{Name, Op},
    hypergraph::{Node, OutPort},
    language::Language,
    prettyprinter::PrettyPrint,
};

pub(crate) const RADIUS_ARG: f32 = 0.05;
pub(crate) const RADIUS_COPY: f32 = 0.1;
pub(crate) const BOX_SIZE: Vec2 = vec2(0.4, 0.4);
pub(crate) const TOLERANCE: f32 = 0.1;
pub(crate) const TEXT_SIZE: f32 = 0.28;
pub(crate) const RADIUS_OPERATION: f32 = 0.2;

// Specifies how to transform a layout position to a screen position.
pub(crate) struct Transform {
    pub scale: f32,
    pub bounds: Rect,
    pub to_screen: RectTransform,
}

impl Transform {
    pub(crate) fn apply(&self, pos: Pos2) -> Pos2 {
        // Scale by a constant and translate to the centre of the bounding box.
        self.to_screen.transform_pos(pos)
    }
}

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
    BoundVar(T::Var),
    Operation(T::Op, Vec<DummyValue<T>>),
}

impl<T: Language> DummyValue<T> {
    pub(crate) fn from_port(out_port: &OutPort<Op<T>, Name<T>>) -> Self {
        match out_port.weight() {
            Name::Variable(var) => Self::BoundVar(var.clone()),
            Name::Thunk(addr) => Self::Thunk(addr.clone()),
            Name::Null => match out_port.node() {
                Some(Node::Operation(op)) => Self::Operation(
                    op.weight().0.clone(),
                    op.inputs()
                        .map(|in_port| Self::from_port(&in_port.link()))
                        .collect(),
                ),
                _ => unreachable!(),
            },
        }
    }
}

impl<T: Language> PrettyPrint for DummyValue<T> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Thunk(addr) => {
                let addr = addr.to_string();
                if addr.is_empty() {
                    RcDoc::text("<thunk>")
                } else {
                    RcDoc::text("thunk")
                        .append(RcDoc::space())
                        .append(RcDoc::as_string(addr))
                }
            }
            Self::BoundVar(var) => RcDoc::as_string(var),
            Self::Operation(op, vs) => {
                if vs.is_empty() {
                    RcDoc::as_string(op)
                } else {
                    RcDoc::as_string(op)
                        .append(RcDoc::text("("))
                        .append(RcDoc::intersperse(
                            vs.iter().map(PrettyPrint::to_doc),
                            RcDoc::text(",").append(RcDoc::space()),
                        ))
                        .append(RcDoc::text(")"))
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
