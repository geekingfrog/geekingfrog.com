use axum::extract::State;
use axum::response::Html;

use crate::error::Result;
use crate::state::AppState;

#[tracing::instrument(skip(state))]
pub async fn get(State(state): State<AppState>) -> Result<Html<String>> {
    let mut tpl_context = tera::Context::new();
    tpl_context.insert("nav_target", "GPG");

    Ok(state
        .template
        .read()
        .render("gpg.html", &tpl_context)?
        .into())
}
