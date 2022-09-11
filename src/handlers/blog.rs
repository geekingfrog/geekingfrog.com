use axum::extract::State;
use axum::response::Html;

use crate::error::Result;
use crate::post::{read_all_posts, Post, PostStatus};
use crate::state::AppState;

#[derive(serde::Serialize)]
pub struct PostHeader {
    date: String,
    title: String,
    tags: Vec<String>,
}

impl From<Post> for PostHeader {
    fn from(p: Post) -> Self {
        let format = time::macros::format_description!("[month]/[year]");
        Self {
            date: p.date.format(&format).unwrap(),
            title: p.title,
            tags: p.tags,
        }
    }
}

#[tracing::instrument(skip(state))]
pub async fn get_all_posts(State(state): State<AppState>) -> Result<Html<String>> {
    let mut posts = read_all_posts().await?;
    posts.sort_unstable_by_key(|p| p.date);
    let post_headers = posts
        .into_iter()
        .rev()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .map(|p| p.into())
        .collect::<Vec<PostHeader>>();

    let mut tpl_context = tera::Context::new();
    tpl_context.insert("nav_target", "BLOG");
    tpl_context.insert("post_headers", &post_headers);

    Ok(state
        .template
        .read()
        .render("blog.html", &tpl_context)?
        .into())
}
