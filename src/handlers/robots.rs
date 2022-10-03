use axum::extract::{Host, State};

use crate::state::AppState;

#[tracing::instrument()]
pub(crate) async fn get_robots(Host(hostname): Host) -> String {
    let sitemap = format!(
        "User-agent: *\n\
        Allow: *\n\
        \n\
        SITEMAP: https://{hostname}/sitemap.txt",
    );
    sitemap
}

#[tracing::instrument(skip(state))]
pub(crate) async fn get_sitemap(Host(hostname): Host, State(state): State<AppState>) -> String {
    let mut urls = vec![
        format!("https://{hostname}.com"),
        format!("https://{hostname}.com/blog"),
        format!("https://{hostname}.com/gpg"),
    ];
    urls.extend(
        state
            .posts
            .iter()
            .map(|p| format!("https://{hostname}.com/blog/post/{}", p.slug)),
    );

    urls.join("\n")
}
