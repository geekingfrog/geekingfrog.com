pub(crate) enum Section {
    Home,
    Blog,
    Gpg
}

#[cfg(debug_assertions)]
fn should_autorefresh() -> bool {
    true
}

#[cfg(not(debug_assertions))]
fn should_autorefresh() -> bool {
    false
}

pub(crate) fn base_ctx(header_link: Option<Section>) -> tera::Context {
    let mut ctx = tera::Context::new();
    match header_link {
        Some(Section::Home) => ctx.insert("nav_target", "HOME"),
        Some(Section::Blog) => ctx.insert("nav_target", "BLOG"),
        Some(Section::Gpg) => ctx.insert("nav_target", "GPG"),
        None => (),
    }
    ctx.insert("should_autorefresh", &should_autorefresh());
    ctx
}
