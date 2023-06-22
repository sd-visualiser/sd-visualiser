use std::{
    fmt::Display,
    sync::{Arc, OnceLock},
};

use eframe::{
    egui::{util::IdTypeMap, Id},
    epaint::mutex::Mutex,
};
use lru::LruCache;
use sd_core::{
    graph::{Name, Op},
    hypergraph::{
        subgraph::{Free, Subgraph},
        Thunk,
    },
    language::Language,
    selection::SelectionMap,
    weak_map::WeakMap,
};

use crate::graph_ui::GraphUiInternal;

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<V, E, T> = Arc<Mutex<LruCache<SelectionMap<(V, E)>, Arc<Mutex<GraphUiInternal<T>>>>>>;

fn subgraph_cache<T: Language>() -> Cache<Op<T>, Name<T>, T>
where
    Op<T>: 'static + Send + Sync,
    Name<T>: 'static + Send + Sync,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .get_temp_mut_or_insert_with::<Cache<Op<T>, Name<T>, T>>(Id::null(), || {
            tracing::trace!("initialise hypergraph cache");
            Arc::new(Mutex::new(LruCache::unbounded()))
        })
        .clone()
}

pub fn clear_subgraph_cache() {
    if let Some(cache) = CACHE.get() {
        cache.lock().clear();
    }
}

pub fn generate_subgraph<T: Language + 'static>(
    selection: &SelectionMap<(Op<T>, Name<T>)>,
    expanded: WeakMap<Thunk<Op<T>, Name<T>>, bool>,
) -> Arc<Mutex<GraphUiInternal<T>>>
where
    T::Op: Display,
    T::Var: Free,
{
    let cache = subgraph_cache();
    let mut guard = cache.lock();
    guard
        .get_or_insert(selection.clone(), || {
            let subgraph = Subgraph::generate_subgraph(selection);
            let graph_ui = GraphUiInternal::from_subgraph(subgraph, expanded);
            Arc::new(Mutex::new(graph_ui))
        })
        .clone()
}
