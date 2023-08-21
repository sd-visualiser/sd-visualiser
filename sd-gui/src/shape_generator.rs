use std::{
    fmt::{Debug, Display},
    sync::{Arc, Mutex, OnceLock},
};

use eframe::egui::{util::IdTypeMap, Id};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{
    hypergraph::{
        generic::{Edge, Operation, Thunk},
        subgraph::ExtensibleEdge,
        traits::{Graph, WithWeight},
    },
    monoidal::{graph::MonoidalGraph, wired_graph::MonoidalWiredGraph},
    weak_map::WeakMap,
};
use sd_graphics::{layout::layout, render, shape::Shapes};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<G, T> = LruCache<(G, WeakMap<Thunk<T>, bool>), Arc<Mutex<Promise<Shapes<T>>>>>;

#[allow(clippy::type_complexity)]
fn shape_cache<G>() -> Arc<Mutex<Cache<G, G::Ctx>>>
where
    G: Graph + Send + Sync + 'static,
    Edge<G::Ctx>: Send + Sync,
    Operation<G::Ctx>: Send + Sync,
    Thunk<G::Ctx>: Send + Sync,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .unwrap()
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<G, G::Ctx>>>>(Id::null(), || {
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
pub fn generate_shapes<G>(
    graph: &G,
    expanded: &WeakMap<Thunk<G::Ctx>, bool>,
) -> Arc<Mutex<Promise<Shapes<G::Ctx>>>>
where
    G: Graph + Send + Sync + 'static,
    Edge<G::Ctx>: ExtensibleEdge + Debug + Send + Sync,
    Operation<G::Ctx>: WithWeight + Debug + Send + Sync,
    Thunk<G::Ctx>: Debug + Send + Sync,
    <Operation<G::Ctx> as WithWeight>::Weight: Display,
{
    let cache = shape_cache::<G>();
    let mut guard = cache.lock().unwrap();
    guard
        .get_or_insert((graph.clone(), expanded.clone()), || {
            let graph = graph.clone();
            let expanded = expanded.clone();
            Arc::new(Mutex::new(crate::spawn!("shape", {
                tracing::debug!("Converting to monoidal term");
                let monoidal_term = MonoidalWiredGraph::from(&graph);
                tracing::debug!("Got term {:#?}", monoidal_term);

                tracing::debug!("Inserting swaps and copies");
                let monoidal_graph = Arc::new(MonoidalGraph::from(&monoidal_term));
                tracing::debug!("Got graph {:#?}", monoidal_graph);

                tracing::debug!("Calculating layout...");
                let layout = layout(&monoidal_graph, &expanded).unwrap();
                tracing::debug!("Calculating shapes...");
                let mut shapes = Vec::new();
                render::generate_shapes(
                    &mut shapes,
                    0.0,
                    &layout,
                    &monoidal_graph,
                    &expanded,
                    true,
                );
                tracing::debug!("Generated {} shapes...", shapes.len());
                Shapes {
                    shapes,
                    size: layout.size(),
                }
            })))
        })
        .clone()
}
