use axum::response::{IntoResponse, Response};
use hyper::StatusCode;

pub type Result<T> = std::result::Result<T, AppError>;

#[derive(thiserror::Error, Debug)]
pub enum AppError {
    #[error("Templating error")]
    TemplateError(#[from] tera::Error),

    #[error("IO error from {1}: {0}")]
    IOError(#[source] std::io::Error, &'static str),

    #[error("Parsing error: {0}")]
    ParseError(String)

}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let res = match self {
            AppError::TemplateError(ref _err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("{self:?}"),
            ),
            AppError::IOError(_, _) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("{self:?}"),
            ),
            AppError::ParseError(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Parse error: {err}")
            )
        };
        res.into_response()
    }
}

pub trait IOContext {
    type Out;
    fn io_context(self, ctx: &'static str) -> Self::Out;
}

impl<T> IOContext for std::result::Result<T, std::io::Error> {
    type Out = std::result::Result<T, AppError>;
    fn io_context(self, ctx: &'static str) -> std::result::Result<T, AppError> {
        self.map_err(|e| AppError::IOError(e, ctx))
    }
}
