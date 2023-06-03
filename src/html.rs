use pulldown_cmark::{html, CodeBlockKind, Event, Parser, Tag};
use syntect::{highlighting::ThemeSet, html::highlighted_html_for_string, parsing::SyntaxSet};

// create a struct here, so I have the option to put some init code there
pub struct HtmlRenderer {}

impl HtmlRenderer {
    pub(crate) fn new() -> Self {
        Self {}
    }

    pub(crate) fn render_content(&self, raw_content: &str) -> String {
        let parser = Parser::new(raw_content);
        let events = SyntectEvent::new(parser);
        let mut html_output = String::new();
        html::push_html(&mut html_output, events);
        html_output
    }
}

struct SyntectEvent<I> {
    inner: I,
    tok: Option<String>,
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
}

impl<I> SyntectEvent<I> {
    fn new(inner: I) -> Self {
        Self {
            inner,
            tok: None,
            syntax_set: SyntaxSet::load_defaults_newlines(),
            theme_set: ThemeSet::load_defaults(),
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
                // hack to inject a special class for inline code
                Event::Code(code) => {
                    // if the code contains things that look like html tags that's going
                    // to mess up the display. The replace like that seems a bit fishy though
                    let code = code.replace("<", "&lt;").replace(">", "&gt;");
                    let new_str = format!(r#"<code class="inline">{}</code>"#, code);
                    Some(Event::Html(new_str.into()))
                }
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(ref tok))) => {
                    self.tok = Some(tok.to_string());
                    Some(ev)
                }
                Event::Text(ref content) => {
                    if let Some(tok) = &self.tok {
                        let theme = &self.theme_set.themes["Solarized (light)"];
                        let s = self
                            .syntax_set
                            .find_syntax_by_token(tok)
                            .unwrap_or_else(|| self.syntax_set.find_syntax_plain_text());
                        match highlighted_html_for_string(content, &self.syntax_set, s, theme) {
                            Ok(res) => Some(Event::Html(res.into())),
                            Err(err) => {
                                tracing::error!("error during html conversion: {err:?}");
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
