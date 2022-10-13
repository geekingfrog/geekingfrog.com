use atom_syndication::{
    CategoryBuilder, ContentBuilder, Entry, EntryBuilder, FeedBuilder, LinkBuilder, Person,
    PersonBuilder,
};
use chrono::naive::{NaiveDate, NaiveDateTime};
use chrono::DateTime;

use crate::post::Post;

/// content: sorted list of *published* posts, newest first.
/// Ref:
/// https://kevincox.ca/2022/05/06/rss-feed-best-practices/
pub fn build_page(hostname: &str, posts: &[&Post], page: usize) -> Option<String> {
    let author = PersonBuilder::default()
        .name("Greg Charvet 黑瓜".to_string())
        .email(Some(format!("greg@{hostname}")))
        .build();

    let page_size = 10;

    let entries = build_entries(
        hostname,
        &author,
        posts.iter().skip(page * page_size).take(page_size).copied(),
    );

    if entries.is_empty() {
        return None;
    }

    let self_url = if page == 0 {
        format!("https://{hostname}/feed.atom")
    } else {
        format!("https://{hostname}/feed/{page}")
    };

    let last_url = format!("https://{hostname}/feed/{}", posts.len() / page_size);

    let mut links = vec![
        LinkBuilder::default()
            .rel("alternate".to_string())
            .href(format!("https://{hostname}/"))
            .build(),
        LinkBuilder::default()
            .rel("self".to_string())
            .href(self_url)
            .build(),
        LinkBuilder::default()
            .rel("first".to_string())
            .href(format!("https://{hostname}/feed.atom"))
            .build(),
        LinkBuilder::default()
            .rel("next".to_string())
            .href(format!("https://{hostname}/feed/{}", page + 1))
            .build(),
        LinkBuilder::default()
            .rel("last".to_string())
            .href(last_url)
            .build(),
    ];

    match page {
        0 => (),
        1 => links.push(
            LinkBuilder::default()
                .rel("previous".to_string())
                .href("https://{hostname}/feed.atom".to_string())
                .build(),
        ),
        _ => links.push(
            LinkBuilder::default()
                .rel("previous".to_string())
                .href(format!("https://{hostname}/feed/{}", page - 1))
                .build(),
        ),
    }

    let feed = FeedBuilder::default()
        .title("geekingfrog feed")
        .id(format!("https://{hostname}/"))
        .links(links)
        .updated(convert_date(posts[0].date))
        .author(author)
        .entries(entries)
        .build();

    Some(feed.to_string())
}

fn build_entries<'a, P>(hostname: &str, author: &Person, posts: P) -> Vec<Entry>
where
    P: Iterator<Item = &'a Post>,
{
    posts
        .map(|p| {
            let categories = p
                .tags
                .iter()
                .map(|t| CategoryBuilder::default().term(t.clone()).build())
                .collect::<Vec<_>>();

            let content = ContentBuilder::default()
                .value(Some(p.html_content.clone()))
                .base(Some(format!("/blog/post/{}", p.slug)))
                .content_type(Some("html".to_string()))
                .build();

            let entry_link = LinkBuilder::default()
                .rel("alternate".to_string())
                .mime_type(Some("text/html".to_string()))
                .href(format!("https://{hostname}/blog/post/{}", p.slug))
                .build();

            EntryBuilder::default()
                .title(p.title.clone())
                .id(format!("https://{hostname}/blog/post/{}", p.slug))
                .updated(convert_date(p.date))
                .author(author.clone())
                .categories(categories)
                .link(entry_link)
                // TODO summary of the post
                .content(Some(content))
                .build()
        })
        .collect()
}

fn convert_date(d: time::Date) -> atom_syndication::FixedDateTime {
    let mu8: u8 = d.month().into();
    let update_date = NaiveDateTime::new(
        NaiveDate::from_ymd(d.year(), mu8.into(), d.day().into()),
        Default::default(),
    );
    DateTime::from_utc(update_date, chrono::offset::FixedOffset::east(0))
}
