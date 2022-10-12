use crate::{
    error::{AppError, IOContext},
    html::HtmlRenderer,
};
use rayon::prelude::*;

use futures::stream::{Stream, StreamExt};

use nom::{
    bytes::complete::{take, take_until},
    sequence::separated_pair,
    Finish, IResult,
};
use time::Date;
use tokio::fs;

#[derive(PartialEq, Eq, Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum PostStatus {
    #[serde(rename = "draft")]
    Draft,
    #[serde(rename = "published")]
    Published,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct PostHeader {
    title: String,
    status: PostStatus,
    tags: Vec<String>,
}

// a simple struct to hold a post, without parsing the markdown
// basically, simply parse the header before feeding it to content pipeline
// originally everything was tied to the lifetime of the original input
// but it complicated things a great deal having to keep around the raw
// strings alongside the Post (tried self ref structs, didn't work).
// So do some good old cloning when constructing from nom parsed results.
#[derive(Debug, Clone)]
pub struct Post {
    pub date: Date,
    pub title: String,
    pub slug: String,
    pub tags: Vec<String>,
    pub status: PostStatus,
    pub raw_content: String,
    pub html_content: String,
}

/// extract the url friendly slug and the date of the post based on the filename
/// 2022-06-29-clojure-context-logging.md will give
/// (2022-06-29, clojure-context-logging)
fn post_metadata(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(
        take("yyyy-mm-dd".len()),
        nom::character::complete::char('-'),
        take_until("."),
    )(input)
}

impl Post {
    pub fn parse(filename: &str, input: &str) -> Result<Post, String> {
        let (_remaining, (raw_date, slug)) = post_metadata(filename)
            .map_err(|e| e.to_owned())
            .finish()
            .map_err(|e| format!("{:?}", e))?;

        let format = time::macros::format_description!("[year]-[month]-[day]");
        let date = Date::parse(raw_date, &format)
            .map_err(|e| format!("Invalid date: {} - {:?}", raw_date, e))?;

        let mut segments = input.split("---");
        segments
            .next()
            .ok_or_else(|| "Missing first segment".to_string())?;

        let header: PostHeader = serde_yaml::from_str(
            segments
                .next()
                .ok_or_else(|| "Missing header".to_string())?,
        )
        .map_err(|e| format!("Invalid header: {:?}", e))?;

        let raw_content = segments.next().ok_or_else(|| "Empty content".to_string())?;

        Ok(Self {
            date,
            title: header.title,
            slug: slug.to_string(),
            tags: header.tags,
            status: header.status,
            raw_content: raw_content.to_string(),
            html_content: HtmlRenderer::new().render_content(raw_content),
        })
    }
}

struct ReadDirStream {
    inner: tokio::fs::ReadDir,
}

impl ReadDirStream {
    fn new(inner: tokio::fs::ReadDir) -> Self {
        Self { inner }
    }
}

impl Stream for ReadDirStream {
    type Item = tokio::io::Result<tokio::fs::DirEntry>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        self.inner.poll_next_entry(cx).map(Result::transpose)
    }
}

pub fn read_all_posts_sync() -> Result<Vec<Post>, AppError> {
    std::fs::read_dir("./blog/posts")
        .unwrap()
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
        .into_par_iter()
        .map(|entry| {
            tracing::debug!("reading entry {:?}", entry.path());
            let content = std::fs::read_to_string(entry.path()).io_context("cannot read entry")?;
            let filename = entry
                .path()
                .file_name()
                .expect("valid utf-8 filename")
                .to_string_lossy()
                .to_string();
            let post = Post::parse(&filename, &content).map_err(|e| {
                AppError::ParseError(format!("Parse error for file {} - {:?}", filename, e))
            })?;

            Result::<_, AppError>::Ok(post)
        })
        .collect()
}

// left here for posterity, but it's at least two times slower than the sync version
// powered by rayon. I'm probably doing something stupid though.
#[tracing::instrument]
pub async fn read_all_posts() -> Result<Vec<Post>, AppError> {
    let read_dir = fs::read_dir("./blog/posts/")
        .await
        .io_context("./blog/posts")?;

    let posts = ReadDirStream::new(read_dir)
        .map(|x| async {
            let entry = x.io_context("cannot read entry")?;
            tracing::debug!("reading entry {:?}", entry.path());
            let filename: String = entry
                .path()
                .file_name()
                .expect("valid utf-8 filename")
                .to_string_lossy()
                .to_string();
            let content = fs::read_to_string(entry.path())
                .await
                .io_context("cannot read entry")?;
            let post = Post::parse(&filename, &content).map_err(|e| {
                AppError::ParseError(format!("Parse error for file {} - {:?}", filename, e))
            })?;

            Result::<_, AppError>::Ok(post)
        })
        .buffer_unordered(100)
        .collect::<Vec<_>>()
        .await;

    let posts: Vec<Post> = posts.into_iter().collect::<Result<Vec<_>, _>>()?;

    Ok(posts)
}

#[cfg(test)]
mod test {
    type BoxResult<T> = Result<T, Box<dyn std::error::Error>>;
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_metadata() -> BoxResult<()> {
        assert_eq!(
            post_metadata("2022-06-29-clojure-context-logging.md"),
            Ok((".md", ("2022-06-29", "clojure-context-logging")))
        );
        Ok(())
    }
}
