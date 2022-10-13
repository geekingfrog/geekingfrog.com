use axum::{
    extract::{Host, Path, State},
    http::HeaderValue,
};
use hyper::{header, StatusCode};

use crate::{feed, state::AppState, post::PostStatus};

pub(crate) struct AtomResponse {
    // None means 404
    resp: Option<String>,
}

impl axum::response::IntoResponse for AtomResponse {
    fn into_response(self) -> axum::response::Response {
        match self.resp {
            Some(feed) => (
                [
                    (
                        header::CONTENT_TYPE,
                        HeaderValue::from_static("application/atom+xml"),
                    ),
                    (
                        header::CACHE_CONTROL,
                        HeaderValue::from_static("max-age=3600"),
                    ),
                ],
                feed,
            )
                .into_response(),

            None => (StatusCode::NOT_FOUND, ()).into_response(),
        }
    }
}

#[tracing::instrument(skip(state))]
pub(crate) async fn get_feed(Host(hostname): Host, State(state): State<AppState>) -> AtomResponse {
    let posts = state
        .posts
        .iter()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .collect::<Vec<_>>();
    AtomResponse {
        resp: feed::build_page(&hostname, &posts, 0),
    }
}

#[tracing::instrument(skip(state))]
pub(crate) async fn get_feed_page(
    Host(hostname): Host,
    State(state): State<AppState>,
    Path(page): Path<usize>,
) -> AtomResponse {
    let posts = state
        .posts
        .iter()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .collect::<Vec<_>>();
    AtomResponse {
        resp: feed::build_page(&hostname, &posts, page),
    }
}
