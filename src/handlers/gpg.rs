use axum::extract::State;
use axum::response::Html;

use crate::error::Result;
use crate::state::AppState;
use crate::template::{self, Section};

#[tracing::instrument(skip(state))]
pub async fn get(State(state): State<AppState>) -> Result<Html<String>> {
    let tpl_context = template::base_ctx(Some(Section::Gpg));

    Ok(state
        .template
        .read()
        .render("gpg.html", &tpl_context)?
        .into())
}
