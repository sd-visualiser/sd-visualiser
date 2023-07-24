use std::{
    fmt::Display,
    sync::{Arc, Mutex, OnceLock},
};

use eframe::egui::{util::IdTypeMap, Id};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{
    common::{Addr, InOutIter},
    hypergraph::traits::{EdgeLike, Graph, WithWeight},
    monoidal::graph::MonoidalGraph,
    selection::SelectionMap,
};
use sd_graphics::{common::GraphMetadata, layout::layout, render, shape::Shapes};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<T> = LruCache<
    (
        Arc<MonoidalGraph<T>>,
        GraphMetadata<T>,
        Option<SelectionMap<T>>,
    ),
    Arc<Mutex<Promise<Shapes<T>>>>,
>;

fn shape_cache<T>() -> Arc<Mutex<Cache<T>>>
where
    T: Addr,
    T::Node: Send + Sync,
    T::Edge: Send + Sync,
    T::Operation: Send + Sync,
    T::Thunk: Send + Sync,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .unwrap()
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<T>>>>(Id::null(), || {
            tracing::trace!("initialise shape cache");
            Arc::new(Mutex::new(LruCache::unbounded()))
        })
        .clone()
}

pub fn clear_shape_cache() {
    if let Some(cache) = CACHE.get() {
        cache.lock().unwrap().clear();
    }
}

#[allow(clippy::type_complexity)]
pub fn generate_shapes<T>(
    graph: &Arc<MonoidalGraph<T>>,
    metadata: &GraphMetadata<T>,
    subgraph_selection: Option<&SelectionMap<T>>,
) -> Arc<Mutex<Promise<Shapes<T>>>>
where
    T: Addr,
    T::Node: Send + Sync,
    T::Edge: Send + Sync + EdgeLike<T = T>,
    T::Operation: Send + Sync + WithWeight + InOutIter<T = T>,
    T::Thunk: Send + Sync + Graph<T = T> + InOutIter<T = T>,
    <T::Operation as WithWeight>::Weight: Display,
{
    let cache = shape_cache();
    let mut guard = cache.lock().unwrap();
    guard
        .get_or_insert(
            (graph.clone(), metadata.clone(), subgraph_selection.cloned()),
            || {
                let graph = graph.clone();
                let metadata = metadata.clone();
                let subgraph_selection = subgraph_selection.cloned();
                Arc::new(Mutex::new(crate::spawn!("shape", {
                    tracing::debug!("Calculating layout...");
                    let layout = layout(&graph, &metadata).unwrap();
                    tracing::debug!("Calculating shapes...");
                    let mut shapes = Vec::new();
                    render::generate_shapes(
                        &mut shapes,
                        0.0,
                        &layout,
                        &graph,
                        &metadata,
                        subgraph_selection.as_ref(),
                    );
                    tracing::debug!("Generated {} shapes...", shapes.len());
                    Shapes {
                        shapes,
                        size: layout.size(),
                    }
                })))
            },
        )
        .clone()
}
