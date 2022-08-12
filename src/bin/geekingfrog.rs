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

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until},
    character::complete::{newline, space0, space1},
    combinator::map,
    multi::separated_list0,
    sequence::{delimited, pair, tuple},
    IResult,
};
use std::error::Error;

use pulldown_cmark::{html, Parser};

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
    fn from_str(input: &'a str) -> Result<Post<'a>, Box<dyn Error + 'a>> {
        let (remaining, (title, tags, status)) = post_header(input)?;
        Ok(Self {
            title,
            tags,
            status,
            raw_content: remaining,
        })
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // post_status("status: published\n")?;
    // post_title("title: coucou\n")?;
    // println!("{:?}", post_tags("tags: coucou, blah, moo\n")?);

    let raw = include_str!("../../blog/posts/2011-05-17-notes-on-ssh-agent.md");
    let post = Post::from_str(raw)?;
    let parser = Parser::new(post.raw_content);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    println!("title: {}", post.title);
    println!("tags: {:?}", post.tags);
    println!("status: {:?}", post.status);
    println!("{}", html_output);

    Ok(())
}
