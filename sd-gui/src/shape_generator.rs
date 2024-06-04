use std::{
    fmt::Display,
    sync::{Arc, Mutex, OnceLock},
};

use eframe::egui::{util::IdTypeMap, Id};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{
    hypergraph::{
        generic::{Edge, Key, Operation, Weight},
        subgraph::ExtensibleEdge,
        traits::Graph,
    },
    lp::Solver,
    monoidal::{graph::MonoidalGraph, wired_graph::from_graph},
};
use sd_graphics::{common::Shapeable, layout::layout, render, shape::Shapes};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<G> = LruCache<Key<G>, Arc<Mutex<Promise<Shapes<<G as Graph>::Ctx>>>>>;

fn shape_cache<G>() -> Arc<Mutex<Cache<G>>>
where
    G: Graph + 'static,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .unwrap()
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<G>>>>(Id::NULL, || {
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

pub fn generate_shapes<G>(graph: &G, solver: Solver) -> Arc<Mutex<Promise<Shapes<G::Ctx>>>>
where
    G: Graph + 'static,
    Edge<G::Ctx>: ExtensibleEdge,
    Operation<G::Ctx>: Shapeable,
    Weight<Operation<G::Ctx>>: Display,
{
    let cache = shape_cache::<G>();
    let mut guard = cache.lock().unwrap();
    guard
        .get_or_insert(graph.key(), || {
            let graph = graph.clone();
            Arc::new(Mutex::new(crate::spawn!("shape", {
                tracing::info!("Converting to monoidal term");
                let monoidal_term = from_graph(&graph, solver);
                tracing::debug!("Got term {:#?}", monoidal_term);

                tracing::info!("Inserting swaps and copies");
                let monoidal_graph = Arc::new(MonoidalGraph::from(&monoidal_term));
                tracing::debug!("Got graph {:#?}", monoidal_graph);

                tracing::info!("Calculating layout...");
                let layout = layout(&monoidal_graph, solver).unwrap();
                tracing::info!("Calculating shapes...");
                let mut shapes = Vec::new();
                render::generate_shapes(&mut shapes, &layout, true);
                tracing::debug!("Generated {} shapes...", shapes.len());
                Shapes {
                    shapes,
                    size: layout.size(),
                }
            })))
        })
        .clone()
}
