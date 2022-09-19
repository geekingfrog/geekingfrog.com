use axum::extract::{Path, State};
use axum::response::Html;

use crate::error::Result;
use crate::post::{Post, PostStatus};
use crate::state::AppState;

#[derive(serde::Serialize)]
pub struct PostHeader {
    date: String,
    title: String,
    slug: String,
    tags: Vec<String>,
}

impl From<Post> for PostHeader {
    fn from(p: Post) -> Self {
        (&p).into()
    }
}

/// This is better than cloning a full Post then calling into()
/// to convert to a PostHeader, since it avoid cloning the entire content
/// of the post.
impl<'a> From<&'a Post> for PostHeader {
    fn from(p: &'a Post) -> Self {
        let format = time::macros::format_description!("[month]/[year]");
        Self {
            date: p.date.format(&format).unwrap(),
            title: p.title.clone(),
            slug: p.slug.clone(),
            tags: p.tags.clone(),
        }
    }
}

#[tracing::instrument(skip(state))]
pub async fn get_all_posts(State(state): State<AppState>) -> Result<Html<String>> {
    let post_headers = state
        .posts
        .iter()
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

#[derive(serde::Serialize)]
struct RenderedPost {
    date: String,
    title: String,
    tags: Vec<String>,

    /// the formatted hmtl to be dumped verbatim into the template
    html: String,
}

impl From<Post> for RenderedPost {
    fn from(p: Post) -> Self {
        let format = time::macros::format_description!("[day] [month repr:short] [year]");

        RenderedPost {
            date: p.date.format(&format).unwrap(),
            title: p.title.to_string(),
            tags: p.tags,
            html: p.html_content,
        }
    }
}

#[tracing::instrument(skip(state))]
pub async fn get_post(
    State(state): State<AppState>,
    Path(slug): Path<String>,
) -> Result<Html<String>> {
    tracing::debug!("getting post for slug {slug}");
    let mut tpl_context = tera::Context::new();
    tpl_context.insert("nav_target", "BLOG");

    let post = state.posts.iter().find(|p| p.slug == slug);
    match post {
        Some(p) => {
            tracing::debug!("found the post");
            let rp: RenderedPost = p.clone().into();
            tpl_context.insert("post", &rp);
            Ok(state
                .template
                .read()
                .render("post.html", &tpl_context)?
                .into())
        }
        None => Ok(state
            .template
            .read()
            .render("404.html", &tpl_context)?
            .into()),
    }
}
