use egui::{emath::RectTransform, epaint::CubicBezierShape, vec2, Pos2, Rect, Vec2};
use pretty::RcDoc;
use sd_core::{
    hypergraph::{Node, OutPort},
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
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum DummyValue<Op, Var> {
    Thunk,
    FreeVar,
    BoundVar(Var),
    Operation(Op, Vec<DummyValue<Op, Var>>),
}

impl<Op: Clone, Var: Clone> DummyValue<Op, Var> {
    pub(crate) fn from_port(out_port: &OutPort<Op, Option<Var>>) -> Self {
        match out_port.weight() {
            Some(var) => Self::BoundVar(var.clone()),
            None => match out_port.node() {
                None => Self::FreeVar, // technically should be unreachable
                Some(Node::Thunk(_)) => Self::Thunk,
                Some(Node::Operation(op)) => Self::Operation(
                    op.weight().clone(),
                    op.inputs()
                        .map(|in_port| Self::from_port(&in_port.link()))
                        .collect(),
                ),
            },
        }
    }
}

impl<Op: PrettyPrint, Var: PrettyPrint> PrettyPrint for DummyValue<Op, Var> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Thunk => RcDoc::text("<thunk>"),
            Self::FreeVar => RcDoc::text("<free var>"),
            Self::BoundVar(var) => var.to_doc(),
            Self::Operation(op, vs) => {
                if vs.is_empty() {
                    op.to_doc()
                } else {
                    op.to_doc()
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
