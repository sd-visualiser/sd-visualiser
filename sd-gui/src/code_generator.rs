use std::sync::{Arc, Mutex, OnceLock};

use eframe::egui::{Id, util::IdTypeMap};
use lru::LruCache;
use poll_promise::Promise;
use sd_core::{
    codeable::Codeable,
    hypergraph::{generic::Key, traits::Graph},
    prettyprinter::PrettyPrint,
};

static CACHE: OnceLock<Mutex<IdTypeMap>> = OnceLock::new();

type Cache<G> = LruCache<Key<G>, Arc<Mutex<Promise<String>>>>;

fn code_cache<G>() -> Arc<Mutex<Cache<G>>>
where
    G: Graph + 'static,
{
    CACHE
        .get_or_init(Mutex::default)
        .lock()
        .unwrap()
        .get_temp_mut_or_insert_with::<Arc<Mutex<Cache<G>>>>(Id::NULL, || {
            tracing::trace!("initialise code cache");
            Arc::new(Mutex::new(LruCache::unbounded()))
        })
        .clone()
}

pub fn clear_code_cache() {
    if let Some(cache) = CACHE.get() {
        cache.lock().unwrap().clear();
    }
}

pub fn generate_code<G>(graph: &G) -> Arc<Mutex<Promise<String>>>
where
    G: Graph + Codeable + 'static,
{
    let cache = code_cache::<G>();
    let mut guard = cache.lock().unwrap();
    guard
        .get_or_insert(graph.key(), || {
            let graph = graph.clone();
            Arc::new(Mutex::new(crate::spawn!("code", {
                graph.code().to_pretty()
            })))
        })
        .clone()
}
