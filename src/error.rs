use thiserror::Error;

use crate::lexer::Token;

#[derive(Debug, Error)]
pub enum Error {
    #[error("expected {0} found {1}")]
    UnexpectedToken(String, String)
}

pub type Result<T> = std::result::Result<T, Error>;