use std::{
    fmt::{Debug, Display},
    sync::{Arc, Mutex, OnceLock},
};

use eframe::egui::{util::IdTypeMap, Id};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{
    common::Addr,
    hypergraph::{
        subgraph::ExtensibleEdge,
        traits::{Graph, NodeLike, WithWeight},
    },
    monoidal::{graph::MonoidalGraph, wired_graph::MonoidalWiredGraph},
    weak_map::WeakMap,
};
use sd_graphics::{layout::layout, render, shape::Shapes};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<G, T, Thunk> = LruCache<(G, WeakMap<Thunk, bool>), Arc<Mutex<Promise<Shapes<T>>>>>;

#[allow(clippy::type_complexity)]
fn shape_cache<G>() -> Arc<Mutex<Cache<G, G::T, <G::T as Addr>::Thunk>>>
where
    G: Graph + Send + Sync + 'static,
    <G::T as Addr>::Node: Send + Sync,
    <G::T as Addr>::Edge: Send + Sync,
    <G::T as Addr>::Operation: Send + Sync,
    <G::T as Addr>::Thunk: Send + Sync,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .unwrap()
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<G, G::T, <G::T as Addr>::Thunk>>>>(
            Id::null(),
            || {
                tracing::trace!("initialise shape cache");
                Arc::new(Mutex::new(LruCache::unbounded()))
            },
        )
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
    expanded: &WeakMap<<G::T as Addr>::Thunk, bool>,
) -> Arc<Mutex<Promise<Shapes<G::T>>>>
where
    G: Graph + Send + Sync + 'static,
    <G::T as Addr>::Node: NodeLike<T = G::T> + Debug + Send + Sync,
    <G::T as Addr>::Edge: ExtensibleEdge<T = G::T> + WithWeight + Debug + Send + Sync,
    <G::T as Addr>::Operation: NodeLike<T = G::T> + WithWeight + Debug + Send + Sync,
    <G::T as Addr>::Thunk: NodeLike<T = G::T> + Graph<T = G::T> + Debug + Send + Sync,
    <<G::T as Addr>::Operation as WithWeight>::Weight: Display,
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
