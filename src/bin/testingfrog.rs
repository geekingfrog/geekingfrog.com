#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_imports)]
#![allow(clippy::all)]

use std::{error::Error, fs};
use website::post::Post;

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("./blog/posts/2026-07-12-testing-dir-post/index.md")?;
    let post = Post::parse("./blog/posts/2026-07-12-testing-dir-post", &content).unwrap();
    println!("{}", post.html_content);
    Ok(())
}
