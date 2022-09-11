use axum::extract::State;
use axum::response::Html;

use crate::error::Result;
use crate::state::AppState;

#[tracing::instrument(skip(state))]
pub async fn not_found(State(state): State<AppState>, _uri: hyper::Uri) -> Result<Html<String>> {
    let tpl_context = tera::Context::new();

    Ok(state
        .template
        .read()
        .render("404.html", &tpl_context)?
        .into())
}
