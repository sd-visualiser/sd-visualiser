use std::{hash::Hash, marker::PhantomData, sync::Arc};

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use sd_core::monoidal::MonoidalGraph;
use sd_graphics::layout::{layout, Layout, LayoutError};
use tracing::{event, Level};

pub struct Layouter<V, E> {
    _pd: PhantomData<(V, E)>,
}

impl<V, E> Default for Layouter<V, E> {
    fn default() -> Self {
        Layouter { _pd: PhantomData }
    }
}

impl<V: 'static + Send + Sync, E: 'static + Send + Sync>
    ComputerMut<&MonoidalGraph<(V, E)>, Result<Arc<Layout>, LayoutError>> for Layouter<V, E>
{
    fn compute(&mut self, graph: &MonoidalGraph<(V, E)>) -> Result<Arc<Layout>, LayoutError> {
        event!(Level::DEBUG, "Generating Layout");
        Ok(Arc::new(layout(graph)?))
    }
}

type LayoutCache<'a, V, E> = FrameCache<Result<Arc<Layout>, LayoutError>, Layouter<V, E>>;

impl<V: 'static + Send + Sync + Hash, E: 'static + Send + Sync + Hash> Layouter<V, E> {
    pub fn layout(
        ctx: &Context,
        graph: &MonoidalGraph<(V, E)>,
    ) -> Result<Arc<Layout>, LayoutError> {
        ctx.memory_mut(|mem| mem.caches.cache::<LayoutCache<'_, V, E>>().get(graph))
    }
}
