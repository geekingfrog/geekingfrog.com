use std::sync::Arc;

use parking_lot::RwLock;
use tera::Tera;
use tokio::sync::watch::Receiver;

use crate::post::Post;

#[derive(Debug, Clone)]
pub struct AppState {
    /// handler to the template engine
    pub template: Arc<RwLock<Tera>>,

    /// in dev mode, a way to know if a template has changed
    pub refresh_chan: Receiver<()>,

    // /// all posts, sorted with the most recent first
    pub posts: Arc<Vec<Post>>
}
