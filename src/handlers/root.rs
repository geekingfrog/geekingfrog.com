use axum::{extract::State, response::Html};

use crate::error::Result;
use crate::handlers::blog::PostHeader;
use crate::post::PostStatus;
use crate::state::AppState;
use crate::template::{self, Section};

#[tracing::instrument]
pub async fn get(State(state): State<AppState>) -> Result<Html<String>> {
    let top_headers = state
        .posts
        .iter()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .take(5)
        .map(|p| p.into())
        .collect::<Vec<PostHeader>>();

    let mut tpl_context = template::base_ctx(Some(Section::Home));
    tpl_context.insert("top_posts_headers", &top_headers);

    Ok(state
        .template
        .read()
        .render("index.html", &tpl_context)?
        .into())
}
