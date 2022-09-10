use axum::{extract::State, response::Html};

use crate::error::Result;
use crate::post::{read_all_posts, PostStatus};
use crate::state::AppState;
use crate::handlers::blog::PostHeader;

#[tracing::instrument]
pub async fn get(State(state): State<AppState>) -> Result<Html<String>> {
    let mut posts = read_all_posts().await?;
    posts.sort_unstable_by_key(|p| p.date);
    let posts = posts;

    let top_headers = posts
        .into_iter()
        .rev()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .take(5)
        .map(|p| p.into())
        .collect::<Vec<PostHeader>>();

    let mut tpl_context = tera::Context::new();
    tpl_context.insert("nav_target", "HOME");
    tpl_context.insert("top_posts_headers", &top_headers);

    Ok(state
        .template
        .read()
        .render("index.html", &tpl_context)?
        .into())
}
