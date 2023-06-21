use std::{
    fmt::Display,
    sync::{Arc, OnceLock},
};

use eframe::{
    egui::{util::IdTypeMap, Id},
    epaint::mutex::Mutex,
};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{hypergraph::Thunk, monoidal::MonoidalGraph, weak_map::WeakMap};
use sd_graphics::{layout::layout, render, shape::Shapes};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<V, E> = LruCache<
    (Arc<MonoidalGraph<(V, E)>>, WeakMap<Thunk<V, E>, bool>),
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
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<V, E>>>>(Id::null(), || {
            tracing::trace!("initialise shape cache");
            Arc::new(Mutex::new(LruCache::unbounded()))
        })
        .clone()
}

pub fn clear_shape_cache() {
    if let Some(cache) = CACHE.get() {
        cache.lock().clear();
    }
}

#[allow(clippy::type_complexity)]
pub fn generate_shapes<V, E>(
    graph: &Arc<MonoidalGraph<(V, E)>>,
    expanded: &WeakMap<Thunk<V, E>, bool>,
) -> Arc<Mutex<Promise<Shapes<(V, E)>>>>
where
    V: 'static + Send + Sync + Display,
    E: 'static + Send + Sync,
{
    let cache = shape_cache();
    let mut guard = cache.lock();
    guard
        .get_or_insert((graph.clone(), expanded.clone()), || {
            let graph = graph.clone();
            let expanded = expanded.clone();
            Arc::new(Mutex::new(Promise::spawn_thread("shape", move || {
                tracing::debug!("Calculating layout...");
                let layout = layout(&graph, &expanded).unwrap();
                tracing::debug!("Calculating shapes...");
                let mut shapes = Vec::new();
                render::generate_shapes(&mut shapes, 0.0, &layout, &graph, &expanded);
                tracing::debug!("Generated {} shapes...", shapes.len());
                Shapes {
                    shapes,
                    size: layout.size(),
                }
            })))
        })
        .clone()
}
