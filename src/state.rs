use std::sync::Arc;

use parking_lot::RwLock;
use tera::Tera;
use tokio::sync::watch::Receiver;

#[derive(Debug, Clone)]
pub struct AppState {
    pub template: Arc<RwLock<Tera>>,
    pub refresh_chan: Receiver<()>,
}
