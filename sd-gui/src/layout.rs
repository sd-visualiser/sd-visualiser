use std::sync::Arc;

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use sd_core::monoidal::MonoidalGraph;
use sd_graphics::layout::{layout, Layout, LayoutError};
use tracing::{event, Level};

#[derive(Default)]
pub struct Layouter {}

impl ComputerMut<&MonoidalGraph, Result<Arc<Layout>, LayoutError>> for Layouter {
    fn compute(&mut self, graph: &MonoidalGraph) -> Result<Arc<Layout>, LayoutError> {
        event!(Level::DEBUG, "Generating Layout");
        Ok(Arc::new(layout(graph)?))
    }
}

type LayoutCache<'a> = FrameCache<Result<Arc<Layout>, LayoutError>, Layouter>;

impl Layouter {
    pub fn layout(ctx: &Context, graph: &MonoidalGraph) -> Result<Arc<Layout>, LayoutError> {
        ctx.memory_mut(|mem| mem.caches.cache::<LayoutCache<'_>>().get(graph))
    }
}
