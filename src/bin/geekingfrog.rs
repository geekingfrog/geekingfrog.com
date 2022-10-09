use std::{net::SocketAddr, sync::Arc};

use axum::BoxError;
use parking_lot::RwLock;
use tera::Tera;
use tokio::sync::watch;
use website::{app, post::read_all_posts_sync, state::AppState};

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    tracing_subscriber::fmt::init();

    let tera = Arc::new(RwLock::new(Tera::new("templates/**/*.html")?));
    let (refresh_tx, refresh_rx) = watch::channel(());

    // force a new value from the initial one so that calling rx.watch
    // is sure to return something.
    refresh_tx.send(())?;

    // let mut posts = vec![];
    // let start = std::time::Instant::now();
    // let mut posts1 = read_all_posts().await?;
    // let async_duration = start.elapsed();
    //
    // let start = std::time::Instant::now();
    // let mut posts2 = website::post::read_all_posts_sync()?;
    // let sync_duration = start.elapsed();
    //
    // posts.append(&mut posts1);
    // posts.append(&mut posts2);
    // println!("Don't elide code: {}", posts.len());
    //
    // println!("async time: {}ms", async_duration.as_millis());
    // println!(" sync time: {}ms",  sync_duration.as_millis());
    //
    // return Ok(());

    let mut posts = read_all_posts_sync()?;
    posts.sort_unstable_by(|a, b| a.date.cmp(&b.date).reverse());

    // println!("{}", website::feed::build(&posts));
    // return Ok(());

    let app_state = AppState {
        template: tera.clone(),
        refresh_chan: refresh_rx,
        posts: Arc::new(posts),
    };

    let app = app::build(app_state);
    let addr = SocketAddr::from(([127, 0, 0, 1], 8088));

    tokio::try_join!(
        async {
            tokio::task::spawn_blocking(move || app::watch_templates_change(tera, refresh_tx))
                .await?
        },
        async {
            tracing::info!("Listening on {addr}");
            axum::Server::bind(&addr)
                .serve(app.into_make_service())
                .await?;
            Ok(())
        }
    )?;

    Ok(())
}
