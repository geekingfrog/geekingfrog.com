#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_imports)]
#![allow(clippy::all)]

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

// ****************************************
// parsing and rendering test
// ****************************************
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until},
    character::complete::{newline, space0, space1},
    combinator::map,
    multi::separated_list0,
    sequence::{delimited, pair, tuple},
    IResult,
};
use std::{error::Error, fs};

use pulldown_cmark::{html, CodeBlockKind, Event, Parser, Tag};
use syntect::{
    highlighting::ThemeSet,
    html::{highlighted_html_for_file, highlighted_html_for_string},
    parsing::SyntaxSet,
};

type BoxError = Box<dyn Error>;
type BoxResult<T> = Result<T, BoxError>;

#[derive(Debug)]
enum PostStatus {
    Draft,
    Published,
}

// a simple struct to hold a post, without parsing the markdown
// basically, simply parse the header before feeding it to content pipeline
struct Post<'input> {
    title: &'input str,
    tags: Vec<&'input str>,
    status: PostStatus,
    raw_content: &'input str,
}

fn post_title(input: &str) -> IResult<&str, &str> {
    delimited(tag("title"), take_until("\n"), newline)(input)
}

fn post_tags(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(
        pair(tag("tags:"), space0),
        separated_list0(
            tuple((space0, tag(","), space0)),
            take_till(|c| c == ',' || c == '\n'),
        ),
        newline,
    )(input)
}

fn post_status(input: &str) -> IResult<&str, PostStatus> {
    delimited(
        tag("status:"),
        delimited(
            space1,
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

impl<'a> Post<'a> {
    fn from_str(input: &'a str) -> Result<Post<'a>, Box<dyn Error>> {
        let (remaining, (title, tags, status)) = post_header(input).map_err(|e| e.to_owned())?;
        Ok(Self {
            title,
            tags,
            status,
            raw_content: remaining,
        })
    }
}

struct SyntectEvent<I> {
    inner: I,
    tok: Option<String>,
    syntax_set: SyntaxSet,
}

impl<'a, I> SyntectEvent<I> {
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
                    Some(ev)
                    // self.next()  // TODO check that, it's fishy, used to strip the <code> block
                }
                Event::Text(ref content) => {
                    if let Some(tok) = &self.tok {
                        let ts = ThemeSet::load_defaults();
                        let theme = &ts.themes["Solarized (light)"];
                        let s = self
                            .syntax_set
                            .find_syntax_by_token(&tok)
                            .unwrap_or_else(|| self.syntax_set.find_syntax_plain_text());
                        eprintln!("syntax found: {}", s.name);
                        match highlighted_html_for_string(content, &self.syntax_set, &s, &theme) {
                            Ok(res) => Some(Event::Html(res.into())),
                            Err(err) => {
                                eprintln!("error during html conversion: {:?}", err);
                                Some(ev)
                            }
                        }
                    } else {
                        Some(ev)
                    }
                }
                Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(_))) => {
                    self.tok = None;
                    Some(ev)
                }
                _ => Some(ev),
            },
        }
    }
}

fn main() -> BoxResult<()> {
    let fmt = time::format_description::parse("[year]-[month]-[day]")?;
    let t = time::Date::parse("2022-08-01-coucou", &fmt)?;
    println!("{}", t.format(&fmt)?);
    return Ok(());

    let raws = fs::read_dir("./blog/posts/")?
        .into_iter()
        .map(|d| d.and_then(|d| fs::read_to_string(d.path())))
        .collect::<Result<Vec<_>, _>>()?;

    let posts: Vec<Post> = raws
        // .map(|d| d.and_then(|d| fs::read_to_string(d.path()).map_err(|e|
        //             Box::new(e) as Box<dyn Error>
        //             )))
        .iter()
        .map(|s| Post::from_str(s))
        .collect::<BoxResult<Vec<_>>>()?;

    // let posts2: Vec<Post> = fs::read_dir("./blog/posts/")?
    //     .into_iter()
    //     .map(|d| {
    //         d.and_then(|d| fs::read_to_string(d.path()))
    //             .map_err(|e| Box::new(e) as Box<dyn Error>)
    //             .as_ref().map(|x| Post::from_str(x).unwrap())
    //             .map_err(|e| todo!())
    //     })
    //     .collect::<BoxResult<Vec<_>>>()?;

    let blah = vec![fs::read_to_string(
        "./blog/posts/2020-03-31-quick-static-hosting.md",
    )?];
    let _p = blah
        .iter()
        .map(|s| Post::from_str(s).unwrap())
        .collect::<Vec<_>>();

    let raw = include_str!("../../test.md");
    let post = Post::from_str(raw)?;
    let parser = Parser::new(post.raw_content);

    // let ts = ThemeSet::load_defaults();
    // let ss = SyntaxSet::load_defaults_newlines();
    // let theme = &ts.themes["Solarized (light)"];

    // for event in &parser {
    //     println!("{:?}", event);
    //     // if let Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(tok))) = event {
    //     //     println!("!!!!!!!!!!!!!! got a token {tok:?}");
    //     //     let syn = ss.find_syntax_by_token(&tok).as_ref().map(|x| &*x.name);
    //     //     println!("syntax? {:?}", syn);
    //     // }
    // }

    let events = SyntectEvent::new(parser);
    let mut html_output = String::new();
    html::push_html(&mut html_output, events);
    println!("{}", html_output);

    // println!("title: {}", post.title);
    // println!("tags: {:?}", post.tags);
    // println!("status: {:?}", post.status);

    // for sr in ss.syntaxes() {
    //     println!("{} - {:?}", sr.name, sr.file_extensions);
    // }

    // let ts = ThemeSet::load_defaults();
    // let theme = &ts.themes["Solarized (light)"];
    // let html = highlighted_html_for_file("src/bin/geekingfrog.rs", &ss, theme).unwrap();
    // println!("{}", html);

    Ok(())
}

// //****************************************
// // Axum test
// //****************************************
// use axum::{
//     extract::{
//         ws::{Message, WebSocket},
//         State, WebSocketUpgrade,
//     },
//     http::StatusCode,
//     response::{Html, IntoResponse, Response},
//     routing::{get, get_service},
//     BoxError, Router,
// };
// use notify::{watcher, RecursiveMode, Watcher, raw_watcher};
// use parking_lot::RwLock;
// use std::{
//     net::SocketAddr,
//     sync::{mpsc, Arc},
//     time::Duration,
// };
// use tera::Tera;
// use tokio::sync::watch::{self, Receiver, Sender};
// use tower::ServiceBuilder;
// use tower_http::{services::ServeDir, trace::TraceLayer};
//
// #[derive(Clone)]
// struct AppState {
//     template: Arc<RwLock<Tera>>,
//     refresh_chan: Receiver<()>,
// }
//
// #[tokio::main]
// async fn main() -> Result<(), BoxError> {
//     tracing_subscriber::fmt::init();
//
//     let tera = Arc::new(RwLock::new(
//         Tera::new("templates/**/*.html").expect("can get tera"),
//     ));
//     let (refresh_tx, refresh_rx) = watch::channel(());
//
//     // force a new value from the initial one so that calling rx.watch
//     // is sure to return something.
//     refresh_tx.send(())?;
//
//     let app_state = AppState {
//         template: tera.clone(),
//         refresh_chan: refresh_rx,
//     };
//
//     let service = ServiceBuilder::new().layer(TraceLayer::new_for_http());
//     // .layer(ServeDir::new("templates"));
//
//     let app = Router::with_state(app_state)
//         .layer(service)
//         .route("/", get(root))
//         .route("/ws/autorefresh", get(autorefresh_handler))
//         .nest(
//             "/static",
//             get_service(ServeDir::new("static")).handle_error(|err: std::io::Error| async move {
//                 tracing::info!("Error serving static staff: {err:?}");
//                 (StatusCode::INTERNAL_SERVER_ERROR, format!("{err:?}"))
//             }),
//         );
//
//     let addr = SocketAddr::from(([127, 0, 0, 1], 8888));
//
//     tokio::try_join!(
//         async {
//             tokio::task::spawn_blocking(move || watch_templates_change(tera, refresh_tx)).await?
//         },
//         async {
//             tracing::debug!("Listening on {addr}");
//             axum::Server::bind(&addr)
//                 .serve(app.into_make_service())
//                 .await?;
//             Ok(())
//         }
//     )?;
//
//     Ok(())
// }
//
// async fn root(State(state): State<AppState>) -> Result<Html<String>, AppError> {
//     Ok(state
//         .template
//         .read()
//         .render("index.html", &tera::Context::new())?
//         .into())
// }
//
// async fn autorefresh_handler(
//     ws: WebSocketUpgrade,
//     State(state): State<AppState>,
// ) -> impl IntoResponse {
//     tracing::debug!("got a websocket upgrade request");
//     ws.on_upgrade(|socket| handle_socket(socket, state.refresh_chan))
// }
//
// async fn handle_socket(mut socket: WebSocket, mut refresh_tx: Receiver<()>) {
//     // There's this weird problem, if a watched file has changed at some point
//     // there will be a new value on the refresh_rx channel, and calling
//     // `changed` on it will return immediately, even if the change has happened
//     // before this call. So always ignore the first change on the channel.
//     // The sender will always send a new value after channel creation to avoid
//     // a different behavior between pages loaded before and after a change
//     // to a watched file.
//     let mut has_seen_one_change = false;
//     loop {
//         tokio::select! {
//             x = refresh_tx.changed() => {
//                 tracing::debug!("refresh event!");
//                 if !has_seen_one_change {
//                     has_seen_one_change = true;
//                     continue
//                 }
//
//                 match x {
//                     Ok(_) => if socket.send(Message::Text("refresh".to_string())).await.is_err() {
//                         tracing::debug!("cannot send stuff, socket probably disconnected");
//                         break;
//                     },
//                     Err(err) => {
//                         tracing::error!("Cannot read refresh chan??? {err:?}");
//                         break
//                     },
//                 }
//             }
//             msg = socket.recv() => {
//                 match msg {
//                     Some(_) => {
//                         tracing::debug!("received a websocket message, don't care");
//                     },
//                     None => {
//                         tracing::debug!("websocket disconnected");
//                         break
//                     },
//                 }
//             }
//             else => break
//         }
//     }
// }
//
// #[derive(thiserror::Error, Debug)]
// enum AppError {
//     #[error("Template error")]
//     TemplateError(#[from] tera::Error),
// }
//
// impl IntoResponse for AppError {
//     fn into_response(self) -> Response {
//         let res = match self {
//             AppError::TemplateError(err) => (
//                 StatusCode::INTERNAL_SERVER_ERROR,
//                 format!("Templating error: {err}"),
//             ),
//         };
//         res.into_response()
//     }
// }
//
// fn watch_templates_change(tera: Arc<RwLock<Tera>>, refresh_tx: Sender<()>) -> Result<(), BoxError> {
//     let (tx, rx) = std::sync::mpsc::channel();
//     let rx = Debounced {
//         rx,
//         d: Duration::from_millis(300),
//     };
//
//     // the default watcher is debounced, but will always send the first event
//     // emmediately, and then debounce any further events. But that means a regular
//     // update actually triggers many events, which are debounced to 2 events.
//     // So use the raw_watcher and manually debounce
//     let mut watcher = raw_watcher(tx)?;
//     watcher.watch("templates", RecursiveMode::Recursive)?;
//     loop {
//         match rx.recv() {
//             Ok(ev) => {
//                 tracing::info!("debounced event {ev:?}");
//                 tera.write().full_reload()?;
//                 refresh_tx.send(())?;
//             }
//             Err(_timeout_error) => (),
//         }
//     }
// }
//
// /// wrap a Receiver<T> such that if many T are received between the given Duration
// /// then only the latest one will be kept and returned when calling recv
// struct Debounced<T> {
//     rx: mpsc::Receiver<T>,
//     d: Duration,
// }
//
// impl<T> Debounced<T> {
//     fn recv(&self) -> Result<T, mpsc::RecvError> {
//         let mut prev = None;
//
//         loop {
//             match prev {
//                 Some(v) => match self.rx.recv_timeout(self.d) {
//                     Ok(newval) => {
//                         prev = Some(newval);
//                         continue;
//                     }
//                     Err(_) => break Ok(v),
//                 },
//                 None => match self.rx.recv() {
//                     Ok(val) => {
//                         prev = Some(val);
//                         continue;
//                     }
//                     Err(err) => break Err(err),
//                 },
//             }
//         }
//     }
// }
