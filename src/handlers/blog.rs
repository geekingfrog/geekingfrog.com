use axum::extract::{Path, State};
use axum::response::Html;
use pulldown_cmark::{html, CodeBlockKind, Event, Parser, Tag};
use syntect::{
    highlighting::ThemeSet,
    html::highlighted_html_for_string,
    parsing::SyntaxSet,
};

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
        let parser = Parser::new(&p.raw_content);
        let events = SyntectEvent::new(parser);
        let mut html_output = String::new();
        html::push_html(&mut html_output, events);

        RenderedPost {
            date: p.date.format(&format).unwrap(),
            title: p.title.to_string(),
            tags: p.tags,
            html: html_output,
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

struct SyntectEvent<I> {
    inner: I,
    tok: Option<String>,
    syntax_set: SyntaxSet,
}

impl<I> SyntectEvent<I> {
    fn new(inner: I) -> Self {
        Self {
            inner,
            tok: None,
            syntax_set: SyntaxSet::load_defaults_newlines(),
        }
    }
}

impl<'a, I> Iterator for SyntectEvent<I>
where
    I: Iterator<Item = Event<'a>>,
{
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            None => None,
            Some(ev) => match ev {
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(ref tok))) => {
                    self.tok = Some(tok.to_string());
                    // Some(ev)
                    self.next() // TODO check that, it's fishy, used to strip the <code> block
                }
                Event::Text(ref content) => {
                    if let Some(tok) = &self.tok {
                        let ts = ThemeSet::load_defaults();
                        let theme = &ts.themes["Solarized (light)"];
                        let s = self
                            .syntax_set
                            .find_syntax_by_token(tok)
                            .unwrap_or_else(|| self.syntax_set.find_syntax_plain_text());
                        match highlighted_html_for_string(content, &self.syntax_set, s, theme) {
                            Ok(res) => Some(Event::Html(res.into())),
                            Err(err) => {
                                tracing::error!("error during html conversion: {err:?}");
                                Some(ev)
                            }
                        }
                    } else {
                        Some(ev)
                    }
                }
                Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(_))) => {
                    self.tok = None;
                    // Some(ev)
                    self.next() // skip the closing </code> since the opening was also skipped
                }
                _ => Some(ev),
            },
        }
    }
}
