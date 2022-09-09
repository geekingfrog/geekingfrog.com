use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until},
    character::complete::{newline, space0, space1},
    combinator::map,
    multi::separated_list0,
    sequence::{delimited, pair, tuple},
    Finish, IResult,
};
use time::Date;

#[derive(Debug)]
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
pub struct Post {
    pub date: Date,
    pub title: String,
    pub tags: Vec<String>,
    pub status: PostStatus,
    pub raw_content: String,
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

impl Post {
    pub fn parse(filename: &str, input: &str) -> Result<Post, nom::error::Error<String>> {
        // bit meh to convert the parse error for date into a nom error
        if filename.len() < 10 {
            let err =
                nom::error::make_error(filename.to_string(), nom::error::ErrorKind::LengthValue);
            return Err(err);
        };

        let format = time::macros::format_description!("[year]-[month]-[day]");
        let sub_str = filename.chars().take("YYYY-MM-DD".len()).collect::<String>();
        let date = Date::parse(&sub_str, &format)
            .map_err(|e| nom::error::make_error(format!("{e:?}"), nom::error::ErrorKind::Fail))?;

        // let date = Date::parse
        let (remaining, (title, tags, status)) =
            post_header(input).map_err(|e| e.to_owned()).finish()?;
        Ok(Self {
            date,
            title: title.to_string(),
            tags: tags.into_iter().map(|s| s.to_string()).collect(),
            status,
            raw_content: remaining.to_string(),
        })
    }
}
