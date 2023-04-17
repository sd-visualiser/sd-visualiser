use std::{hash::Hash, marker::PhantomData, sync::Arc};

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use sd_core::monoidal::MonoidalGraph;
use sd_graphics::layout::{layout, Layout, LayoutError};
use tracing::{event, Level};

pub struct Layouter<O> {
    _pd: PhantomData<O>,
}

impl<O> Default for Layouter<O> {
    fn default() -> Self {
        Layouter { _pd: PhantomData }
    }
}

impl<O: 'static + Send + Sync> ComputerMut<&MonoidalGraph<O>, Result<Arc<Layout>, LayoutError>>
    for Layouter<O>
{
    fn compute(&mut self, graph: &MonoidalGraph<O>) -> Result<Arc<Layout>, LayoutError> {
        event!(Level::DEBUG, "Generating Layout");
        Ok(Arc::new(layout(graph)?))
    }
}

type LayoutCache<'a, O> = FrameCache<Result<Arc<Layout>, LayoutError>, Layouter<O>>;

impl<O: 'static + Send + Sync + Hash> Layouter<O> {
    pub fn layout(ctx: &Context, graph: &MonoidalGraph<O>) -> Result<Arc<Layout>, LayoutError> {
        ctx.memory_mut(|mem| mem.caches.cache::<LayoutCache<'_, O>>().get(graph))
    }
}
