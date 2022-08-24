//****************************************
// tracing test
//****************************************
// use axum::{routing::get, Router};
// use std::error::Error;
//
// use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Registry};
// use tracing_tree::HierarchicalLayer;
// use website::handlers;
//
// #[tokio::main]
// async fn main() -> Result<(), Box<dyn Error>> {
//     Registry::default()
//         .with(EnvFilter::from_default_env())
//         .with(
//             HierarchicalLayer::new(2)
//                 .with_targets(true)
//                 .with_bracketed_fields(true),
//         )
//         .init();
//
//     let app = Router::new().route("/", get(handlers::root::get));
//
//     axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
//         .serve(app.into_make_service())
//         .await?;
//
//     Ok(())
// }

//****************************************
// parsing and rendering test
//****************************************
// use nom::{
//     branch::alt,
//     bytes::complete::{tag, take_till, take_until},
//     character::complete::{newline, space0, space1},
//     combinator::map,
//     multi::separated_list0,
//     sequence::{delimited, pair, tuple},
//     IResult,
// };
// use std::error::Error;
//
// use pulldown_cmark::{html, CodeBlockKind, Event, Parser, Tag};
// use syntect::{
//     highlighting::ThemeSet,
//     html::{highlighted_html_for_file, highlighted_html_for_string},
//     parsing::SyntaxSet,
// };
//
// #[derive(Debug)]
// enum PostStatus {
//     Draft,
//     Published,
// }
//
// // a simple struct to hold a post, without parsing the markdown
// // basically, simply parse the header before feeding it to content pipeline
// struct Post<'input> {
//     title: &'input str,
//     tags: Vec<&'input str>,
//     status: PostStatus,
//     raw_content: &'input str,
// }
//
// fn post_title(input: &str) -> IResult<&str, &str> {
//     delimited(tag("title"), take_until("\n"), newline)(input)
// }
//
// fn post_tags(input: &str) -> IResult<&str, Vec<&str>> {
//     delimited(
//         pair(tag("tags:"), space0),
//         separated_list0(
//             tuple((space0, tag(","), space0)),
//             take_till(|c| c == ',' || c == '\n'),
//         ),
//         newline,
//     )(input)
// }
//
// fn post_status(input: &str) -> IResult<&str, PostStatus> {
//     delimited(
//         tag("status:"),
//         delimited(
//             space1,
//             alt((
//                 map(tag("published"), |_| PostStatus::Published),
//                 map(tag("draft"), |_| PostStatus::Draft),
//             )),
//             space0,
//         ),
//         newline,
//     )(input)
// }
//
// fn post_header(input: &str) -> IResult<&str, (&str, Vec<&str>, PostStatus)> {
//     delimited(
//         pair(tag("---"), newline),
//         tuple((post_title, post_tags, post_status)),
//         pair(tag("---"), newline),
//     )(input)
// }
//
// impl<'a> Post<'a> {
//     fn from_str(input: &'a str) -> Result<Post<'a>, Box<dyn Error + 'a>> {
//         let (remaining, (title, tags, status)) = post_header(input)?;
//         Ok(Self {
//             title,
//             tags,
//             status,
//             raw_content: remaining,
//         })
//     }
// }
//
// struct SyntectEvent<I> {
//     inner: I,
//     tok: Option<String>,
//     syntax_set: SyntaxSet,
// }
//
// impl<'a, I> SyntectEvent<I> {
//     fn new(inner: I) -> Self {
//         Self {
//             inner,
//             tok: None,
//             syntax_set: SyntaxSet::load_defaults_newlines(),
//         }
//     }
// }
//
// impl<'a, I> Iterator for SyntectEvent<I>
// where
//     I: Iterator<Item = Event<'a>>,
// {
//     type Item = Event<'a>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         match self.inner.next() {
//             None => None,
//             Some(ev) => match ev {
//                 Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(ref tok))) => {
//                     self.tok = Some(tok.to_string());
//                     Some(ev)
//                     // self.next()  // TODO check that, it's fishy, used to strip the <code> block
//                 }
//                 Event::Text(ref content) => {
//                     if let Some(tok) = &self.tok {
//                         let ts = ThemeSet::load_defaults();
//                         let theme = &ts.themes["Solarized (light)"];
//                         let s = self
//                             .syntax_set
//                             .find_syntax_by_token(&tok)
//                             .unwrap_or_else(|| self.syntax_set.find_syntax_plain_text());
//                         eprintln!("syntax found: {}", s.name);
//                         match highlighted_html_for_string(content, &self.syntax_set, &s, &theme) {
//                             Ok(res) => Some(Event::Html(res.into())),
//                             Err(err) => {
//                                 eprintln!("error during html conversion: {:?}", err);
//                                 Some(ev)
//                             }
//                         }
//                     } else {
//                         Some(ev)
//                     }
//                 },
//                 Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(_))) => {
//                     self.tok = None;
//                     Some(ev)
//                 },
//                 _ => Some(ev),
//             },
//         }
//     }
// }
//
// fn main() -> Result<(), Box<dyn Error>> {
//     let raw = include_str!("../../test.md");
//     let post = Post::from_str(raw)?;
//     let parser = Parser::new(post.raw_content);
//
//     // let ts = ThemeSet::load_defaults();
//     // let ss = SyntaxSet::load_defaults_newlines();
//     // let theme = &ts.themes["Solarized (light)"];
//
//     // for event in &parser {
//     //     println!("{:?}", event);
//     //     // if let Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(tok))) = event {
//     //     //     println!("!!!!!!!!!!!!!! got a token {tok:?}");
//     //     //     let syn = ss.find_syntax_by_token(&tok).as_ref().map(|x| &*x.name);
//     //     //     println!("syntax? {:?}", syn);
//     //     // }
//     // }
//
//     let events = SyntectEvent::new(parser);
//     let mut html_output = String::new();
//     html::push_html(&mut html_output, events);
//     println!("{}", html_output);
//
//     // println!("title: {}", post.title);
//     // println!("tags: {:?}", post.tags);
//     // println!("status: {:?}", post.status);
//
//     // for sr in ss.syntaxes() {
//     //     println!("{} - {:?}", sr.name, sr.file_extensions);
//     // }
//
//     // let ts = ThemeSet::load_defaults();
//     // let theme = &ts.themes["Solarized (light)"];
//     // let html = highlighted_html_for_file("src/bin/geekingfrog.rs", &ss, theme).unwrap();
//     // println!("{}", html);
//
//     Ok(())
// }

//****************************************
// Axum test
//****************************************
use axum::{
    extract::State,
    http::StatusCode,
    response::{Html, IntoResponse, Response},
    routing::get,
    BoxError, Router,
};
use notify::{watcher, RecursiveMode, Watcher};
use parking_lot::RwLock;
use std::{net::SocketAddr, sync::Arc, time::Duration};
use tera::Tera;
use tower::ServiceBuilder;
use tower_http::trace::TraceLayer;

#[derive(Clone)]
struct AppState {
    template: Arc<RwLock<Tera>>,
}

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    tracing_subscriber::fmt::init();

    let tera = Arc::new(RwLock::new(
        Tera::new("templates/**/*.html").expect("can get tera"),
    ));
    let app_state = AppState {
        template: tera.clone(),
    };

    let service = ServiceBuilder::new().layer(TraceLayer::new_for_http());

    let app = Router::with_state(app_state)
        .route("/", get(root))
        .layer(service);

    let addr = SocketAddr::from(([127, 0, 0, 1], 8888));

    tokio::try_join!(
        async { tokio::task::spawn_blocking(move || watch_templates_change(tera)).await? },
        async {
            tracing::debug!("Listening on {addr}");
            axum::Server::bind(&addr)
                .serve(app.into_make_service())
                .await?;
            Ok(())
        }
    )?;

    Ok(())
}

async fn root(State(state): State<AppState>) -> Result<Html<String>, AppError> {
    Ok(state
        .template
        .read()
        .render("index.html", &tera::Context::new())?
        .into())
}

#[derive(thiserror::Error, Debug)]
enum AppError {
    #[error("Template error")]
    TemplateError(#[from] tera::Error),
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let res = match self {
            AppError::TemplateError(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Templating error: {err}"),
            ),
        };
        res.into_response()
    }
}

fn watch_templates_change(tera: Arc<RwLock<Tera>>) -> Result<(), BoxError> {
    let (tx, rx) = std::sync::mpsc::channel();
    let mut watcher = watcher(tx, Duration::from_secs(1))?;
    watcher.watch("templates", RecursiveMode::Recursive)?;
    loop {
        match rx.recv_timeout(Duration::from_millis(200)) {
            Ok(ev) => {
                tracing::info!("debounced event {ev:?}");
                tera.write().full_reload()?;
            }
            Err(_timeout_error) => (),
        }
    }
}
