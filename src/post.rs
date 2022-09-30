use crate::{
    error::{AppError, IOContext},
    html::HtmlRenderer,
};
use rayon::prelude::*;
use std::ops::RangeFrom;

use futures::stream::{Stream, StreamExt};

use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_until},
    character::complete::{newline, space0},
    combinator::map,
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    AsChar, Compare, Finish, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
    Slice,
};
use time::Date;
use tokio::fs;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum PostStatus {
    Draft,
    Published,
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

fn header_key<T, Input, E: ParseError<Input>>(k: T) -> impl FnMut(Input) -> IResult<Input, (), E>
where
    Input:
        InputTake + InputTakeAtPosition + Compare<T> + Clone + InputIter + Slice<RangeFrom<usize>>,
    <Input as InputTakeAtPosition>::Item: AsChar + Clone,
    <Input as InputIter>::Item: AsChar,
    T: InputLength + Clone,
    // ^ 😱 I was just following orders from rust-analyzer
{
    map(
        tuple((tag(k), space0, nom::character::complete::char(':'), space0)),
        |_| (),
    )
}

fn post_title(input: &str) -> IResult<&str, &str> {
    delimited(header_key("title"), take_until("\n"), newline)(input)
}

fn post_tags(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(
        header_key("tags"),
        separated_list0(
            tuple((space0, tag(","), space0)),
            take_till(|c| c == ',' || c == '\n'),
        ),
        newline,
    )(input)
}

fn post_status(input: &str) -> IResult<&str, PostStatus> {
    delimited(
        header_key("status"),
        terminated(
            alt((
                map(tag("published"), |_| PostStatus::Published),
                map(tag("draft"), |_| PostStatus::Draft),
            )),
            space0,
        ),
        newline,
    )(input)
}

fn post_header(input: &str) -> IResult<&str, (&str, Vec<&str>, PostStatus)> {
    delimited(
        pair(tag("---"), newline),
        tuple((post_title, post_tags, post_status)),
        pair(tag("---"), newline),
    )(input)
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
    pub fn parse(filename: &str, input: &str) -> Result<Post, nom::error::Error<String>> {
        let (_remaining, (raw_date, slug)) =
            post_metadata(filename).map_err(|e| e.to_owned()).finish()?;

        let format = time::macros::format_description!("[year]-[month]-[day]");
        let date = Date::parse(raw_date, &format)
            .map_err(|e| nom::error::make_error(format!("{e:?}"), nom::error::ErrorKind::Fail))?;

        // let date = Date::parse
        let (remaining, (title, tags, status)) =
            post_header(input).map_err(|e| e.to_owned()).finish()?;

        Ok(Self {
            date,
            title: title.to_string(),
            slug: slug.to_string(),
            tags: tags.into_iter().map(|s| s.to_string()).collect(),
            status,
            raw_content: remaining.to_string(),
            html_content: HtmlRenderer::new().render_content(remaining),
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
            let post = Post::parse(
                &entry
                    .path()
                    .file_name()
                    .expect("valid utf-8 filename")
                    .to_string_lossy(),
                &content,
            )
            .map_err(|e| AppError::ParseError(e, entry.path().to_string_lossy().to_string()))?;

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
            let content = fs::read_to_string(entry.path())
                .await
                .io_context("cannot read entry")?;
            let post = Post::parse(
                &entry
                    .path()
                    .file_name()
                    .expect("valid utf-8 filename")
                    .to_string_lossy(),
                &content,
            )
            .map_err(|e| AppError::ParseError(e, entry.path().to_string_lossy().to_string()))?;

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
    fn test_parse_title() -> BoxResult<()> {
        assert_eq!(post_title("title: coucou\n"), Ok(("", "coucou")));
        assert_eq!(post_title("title  : coucou\n"), Ok(("", "coucou")));
        assert_eq!(post_title("title:coucou\n"), Ok(("", "coucou")));
        Ok(())
    }

    #[test]
    fn test_parse_status() -> BoxResult<()> {
        assert_eq!(post_tags("tags: one, two\n"), Ok(("", vec!["one", "two"])));
        assert_eq!(
            post_tags("tags  : one, two\n"),
            Ok(("", vec!["one", "two"]))
        );
        assert_eq!(post_tags("tags:one, two\n"), Ok(("", vec!["one", "two"])));
        Ok(())
    }

    #[test]
    fn test_parse_tags() -> BoxResult<()> {
        assert_eq!(
            post_status("status: published\n"),
            Ok(("", PostStatus::Published))
        );
        assert_eq!(
            post_status("status  : published\n"),
            Ok(("", PostStatus::Published))
        );
        assert_eq!(
            post_status("status:published\n"),
            Ok(("", PostStatus::Published))
        );
        assert_eq!(post_status("status: draft\n"), Ok(("", PostStatus::Draft)));
        Ok(())
    }

    #[test]
    fn test_parse_metadata() -> BoxResult<()> {
        assert_eq!(
            post_metadata("2022-06-29-clojure-context-logging.md"),
            Ok((".md", ("2022-06-29", "clojure-context-logging")))
        );
        Ok(())
    }
}
