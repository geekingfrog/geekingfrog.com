use axum::{
    extract::{Path, State},
    http::HeaderValue,
};
use hyper::{header, StatusCode};

use crate::{feed, state::AppState};

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
pub(crate) async fn get_feed(State(state): State<AppState>) -> AtomResponse {
    AtomResponse {
        resp: feed::build_page(&state.posts, 0),
    }
}

#[tracing::instrument(skip(state))]
pub(crate) async fn get_feed_page(
    State(state): State<AppState>,
    Path(page): Path<usize>,
) -> AtomResponse {
    AtomResponse {
        resp: feed::build_page(&state.posts, page),
    }
}
