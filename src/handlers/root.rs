use axum::extract::Host;
use axum::{extract::State, response::Html};

use crate::error::Result;
use crate::handlers::blog::PostHeader;
use crate::post::PostStatus;
use crate::state::AppState;
use crate::template::{self, Section};

#[tracing::instrument(skip(state))]
pub async fn get(State(state): State<AppState>, Host(hostname): Host) -> Result<Html<String>> {
    let top_headers = state
        .posts
        .iter()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .take(5)
        .map(|p| p.into())
        .collect::<Vec<PostHeader>>();

    tracing::debug!("hostname is {hostname}");
    let mut tpl_context = template::base_ctx(Some(Section::Home));
    tpl_context.insert("top_posts_headers", &top_headers);

    // use starts_with instead of an exact match because the port is part of the
    // host header, and so that avoid hardcoding the local port here.
    let target_template = if hostname.starts_with("geekinfrog.com") {
        "ltd.html"
    } else {
        "index.html"
    };

    Ok(state
        .template
        .read()
        .render(target_template, &tpl_context)?
        .into())
}
