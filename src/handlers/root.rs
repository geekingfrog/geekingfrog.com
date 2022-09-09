use axum::{extract::State, response::Html};

use crate::error::{AppError, IOContext, Result};
use crate::post::{Post, PostStatus};
use crate::state::AppState;
use tokio::fs;

#[tracing::instrument]
pub async fn get(State(state): State<AppState>) -> Result<Html<String>> {
    let mut posts = read_all_posts().await?;
    posts.sort_unstable_by_key(|p| p.date);
    let posts = posts;

    for p in posts
        .iter()
        .rev()
        .filter(|p| matches!(p.status, PostStatus::Published))
        .take(5)
    {
        println!("{} - {}", p.date, p.title);
    }

    Ok(state
        .template
        .read()
        .render("index.html", &tera::Context::new())?
        .into())
}

#[tracing::instrument]
async fn read_all_posts() -> Result<Vec<Post>> {
    let mut read_dir = fs::read_dir("./blog/posts/")
        .await
        .io_context("./blog/posts")?;

    let mut res = Vec::new();
    while let Some(entry) = read_dir
        .next_entry()
        .await
        .map_err(|e| AppError::IOError(e, "reading post entry"))?
    {
        if !entry.file_type().await.io_context("coucou")?.is_file() {
            continue;
        };
        tracing::debug!("reading {:?}", entry.path());
        let post = Post::parse(
            &entry
                .path()
                .file_name()
                .expect("valid utf-8 filename")
                .to_string_lossy(),
            &fs::read_to_string(entry.path()).await.unwrap(),
        )
        .map_err(|e| AppError::ParseError(e, entry.path().to_string_lossy().to_string()))?;
        res.push(post);
    }
    Ok(res)
}
