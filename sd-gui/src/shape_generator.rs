use std::{fmt::Display, marker::PhantomData, sync::Arc};

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use sd_core::{hypergraph::Thunk, monoidal::MonoidalGraph};
use sd_graphics::{expanded::Expanded, layout::layout, render::generate_shapes, shape::Shapes};
use tracing::debug;

pub struct ShapeGenerator<V, E> {
    _pd: PhantomData<(V, E)>,
}

impl<V, E> Default for ShapeGenerator<V, E> {
    fn default() -> Self {
        ShapeGenerator { _pd: PhantomData }
    }
}

impl<V: 'static + Send + Sync + Display, E: 'static + Send + Sync>
    ComputerMut<(&MonoidalGraph<(V, E)>, &Expanded<Thunk<V, E>>), Arc<Shapes<(V, E)>>>
    for ShapeGenerator<V, E>
{
    fn compute(
        &mut self,
        (graph, expanded): (&MonoidalGraph<(V, E)>, &Expanded<Thunk<V, E>>),
    ) -> Arc<Shapes<(V, E)>> {
        debug!("Calculating layout...");
        let layout = layout(graph, expanded).unwrap();
        debug!("Calculating shapes...");
        let mut shapes = Vec::new();
        generate_shapes(&mut shapes, 0.0, &layout, graph, expanded);
        debug!("Generated {} shapes...", shapes.len());
        Arc::new(Shapes {
            shapes,
            width: layout.width(),
            height: layout.height(),
        })
    }
}

type ShapeCache<'a, V, E> = FrameCache<Arc<Shapes<(V, E)>>, ShapeGenerator<V, E>>;

impl<V: 'static + Send + Sync + Display, E: 'static + Send + Sync> ShapeGenerator<V, E> {
    pub fn generate_shapes(
        ctx: &Context,
        graph: &MonoidalGraph<(V, E)>,
        expanded: &Expanded<Thunk<V, E>>,
    ) -> Arc<Shapes<(V, E)>> {
        ctx.memory_mut(|mem| {
            mem.caches
                .cache::<ShapeCache<'_, V, E>>()
                .get((graph, expanded))
        })
    }
}
