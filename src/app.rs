use std::sync::Arc;
use std::time::Duration;

use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::{IntoResponse, Redirect};
use axum::{routing, BoxError, Router};
use hyper::StatusCode;
use notify::Watcher;
use parking_lot::RwLock;
use tera::Tera;
use tokio::sync::watch::{Receiver, Sender};
use tower::ServiceBuilder;
use tower_http::services::ServeDir;
use tower_http::trace::TraceLayer;

use crate::handlers;
use crate::state::AppState;

pub fn build(app_state: AppState) -> Router<AppState> {
    let service = ServiceBuilder::new().layer(TraceLayer::new_for_http());
    Router::with_state(app_state)
        .layer(service)
        .route("/", routing::get(handlers::root::get))
        .route("/blog", routing::get(handlers::blog::get_all_posts))
        .route("/blog/post/:slug", routing::get(handlers::blog::get_post))
        .route("/gpg", routing::get(handlers::gpg::get))
        .route("/feed.atom", routing::get(handlers::feed::get_feed))
        .route("/feed/:page", routing::get(handlers::feed::get_feed_page))
        .route(
            "/rss",
            routing::get(|| async { Redirect::permanent("/feed.atom") }),
        )
        .route("/robots.txt", routing::get(handlers::robots::get_robots))
        .route("/sitemap.txt", routing::get(handlers::robots::get_sitemap))
        .route("/ws/autorefresh", routing::get(autorefresh_handler))
        .nest(
            "/static",
            routing::get_service(ServeDir::new("static")).handle_error(
                |err: std::io::Error| async move {
                    tracing::info!("Error serving static stuff: {err:?}");
                    (StatusCode::INTERNAL_SERVER_ERROR, format!("{err:?}"))
                },
            ),
        )
        .fallback(handlers::not_found::not_found)
}

#[cfg(debug_assertions)]
async fn autorefresh_handler(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
) -> impl IntoResponse {
    tracing::debug!("got a websocket upgrade request");
    ws.on_upgrade(|socket| handle_socket(socket, state.refresh_chan))
}

#[cfg(not(debug_assertions))]
async fn autorefresh_handler(
    _ws: WebSocketUpgrade,
    State(_state): State<AppState>,
) -> impl IntoResponse {
    axum::http::StatusCode::NOT_FOUND
}

async fn handle_socket(mut socket: WebSocket, mut refresh_tx: Receiver<()>) {
    // There's this weird problem, if a watched file has changed at some point
    // there will be a new value on the refresh_rx channel, and calling
    // `changed` on it will return immediately, even if the change has happened
    // before this call. So always ignore the first change on the channel.
    // The sender will always send a new value after channel creation to avoid
    // a different behavior between pages loaded before and after a change
    // to a watched file.
    let mut has_seen_one_change = false;
    loop {
        tokio::select! {
            x = refresh_tx.changed() => {
                tracing::debug!("refresh event!");
                if !has_seen_one_change {
                    has_seen_one_change = true;
                    continue
                }

                match x {
                    Ok(_) => if socket.send(Message::Text("refresh".to_string())).await.is_err() {
                        tracing::debug!("cannot send stuff, socket probably disconnected");
                        break;
                    },
                    Err(err) => {
                        tracing::error!("Cannot read refresh chan??? {err:?}");
                        break
                    },
                }
            }
            msg = socket.recv() => {
                match msg {
                    Some(_) => {
                        tracing::debug!("received a websocket message, don't care");
                    },
                    None => {
                        tracing::debug!("websocket disconnected");
                        break
                    },
                }
            }
            else => break
        }
    }
}

#[cfg(debug_assertions)]
pub fn watch_templates_change(
    tera: Arc<RwLock<Tera>>,
    refresh_tx: Sender<()>,
) -> Result<(), BoxError> {
    let (tx, rx) = std::sync::mpsc::channel();
    let rx = Debounced {
        rx,
        d: Duration::from_millis(300),
    };

    // the default watcher is debounced, but will always send the first event
    // emmediately, and then debounce any further events. But that means a regular
    // update actually triggers many events, which are debounced to 2 events.
    // So use the raw_watcher and manually debounce
    let mut watcher = notify::raw_watcher(tx)?;
    watcher.watch("templates", notify::RecursiveMode::Recursive)?;
    // truly, only interested in static/styles.css but only watching one file
    // has unexpected behaviour so watch the parent instead
    watcher.watch("static", notify::RecursiveMode::NonRecursive)?;
    loop {
        match rx.recv() {
            Ok(ev) => {
                tracing::info!("debounced event {ev:?}");
                match tera.write().full_reload() {
                    Ok(_) => refresh_tx.send(())?,
                    Err(err) => tracing::error!("failed to reload templates: {err:?}"),
                }
            }
            Err(_timeout_error) => (),
        }
    }
}

#[cfg(not(debug_assertions))]
pub fn watch_templates_change(
    _tera: Arc<RwLock<Tera>>,
    _refresh_tx: Sender<()>,
) -> Result<(), BoxError> {
    Ok(())
}


/// wrap a Receiver<T> such that if many T are received between the given Duration
/// then only the latest one will be kept and returned when calling recv
struct Debounced<T> {
    rx: std::sync::mpsc::Receiver<T>,
    d: Duration,
}

impl<T> Debounced<T> {
    fn recv(&self) -> Result<T, std::sync::mpsc::RecvError> {
        let mut prev = None;

        loop {
            match prev {
                Some(v) => match self.rx.recv_timeout(self.d) {
                    Ok(newval) => {
                        prev = Some(newval);
                        continue;
                    }
                    Err(_) => break Ok(v),
                },
                None => match self.rx.recv() {
                    Ok(val) => {
                        prev = Some(val);
                        continue;
                    }
                    Err(err) => break Err(err),
                },
            }
        }
    }
}
