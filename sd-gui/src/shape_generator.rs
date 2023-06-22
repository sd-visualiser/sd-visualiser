use std::{
    fmt::Display,
    sync::{Arc, Mutex, OnceLock},
};

use eframe::egui::{util::IdTypeMap, Id};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{monoidal::MonoidalGraph, selection::SelectionMap};
use sd_graphics::{common::GraphMetadata, layout::layout, render, shape::Shapes};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<V, E> = LruCache<
    (
        Arc<MonoidalGraph<(V, E)>>,
        GraphMetadata<(V, E)>,
        Option<SelectionMap<(V, E)>>,
    ),
    Arc<Mutex<Promise<Shapes<(V, E)>>>>,
>;

fn shape_cache<V, E>() -> Arc<Mutex<Cache<V, E>>>
where
    V: 'static + Send + Sync,
    E: 'static + Send + Sync,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .unwrap()
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<V, E>>>>(Id::null(), || {
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
pub fn generate_shapes<V, E>(
    graph: &Arc<MonoidalGraph<(V, E)>>,
    metadata: &GraphMetadata<(V, E)>,
    subgraph_selection: Option<&SelectionMap<(V, E)>>,
) -> Arc<Mutex<Promise<Shapes<(V, E)>>>>
where
    V: 'static + Send + Sync + Display,
    E: 'static + Send + Sync,
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
