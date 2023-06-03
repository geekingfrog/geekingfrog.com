use std::{env, net::SocketAddr, sync::Arc};

use axum::BoxError;
use parking_lot::RwLock;
use tera::Tera;
use tokio::sync::watch;
use website::{app, post::read_all_posts_sync, state::AppState};

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    tracing_subscriber::fmt::init();

    if cfg!(debug_assertions) {
        tracing::info!("debug mode");
    } else {
        tracing::info!("release mode");
    };

    let port = env::args()
        .next()
        .as_ref()
        .and_then(|p| p.parse::<u16>().ok())
        .unwrap_or(8088);

    let tera = Arc::new(RwLock::new(Tera::new("templates/**/*.html")?));
    let (refresh_tx, refresh_rx) = watch::channel(());

    // force a new value from the initial one so that calling rx.watch
    // is sure to return something.
    refresh_tx.send(())?;

    let mut posts = read_all_posts_sync()?;
    tracing::info!("read {} posts", posts.len());
    posts.sort_unstable_by(|a, b| a.date.cmp(&b.date).reverse());

    let app_state = AppState {
        template: tera.clone(),
        refresh_chan: refresh_rx,
        posts: Arc::new(posts),
    };

    let app = app::build(app_state);
    let addr = SocketAddr::from(([127, 0, 0, 1], port));

    tokio::try_join!(
        async {
            tokio::task::spawn_blocking(move || app::watch_templates_change(tera, refresh_tx))
                .await?
        },
        async {
            tracing::info!("listening on {addr}");
            axum::Server::bind(&addr)
                .serve(app.into_make_service())
                .await?;
            Ok(())
        }
    )?;

    Ok(())
}
