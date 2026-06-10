use std::num::{ParseFloatError, ParseIntError};

use thiserror::Error;


#[derive(Debug, Error, PartialEq, Clone)]
pub enum Error {
    #[error("expected {0} found {1}")]
    UnexpectedToken(String, String),
    #[error("unexpected end of input, expected: {0}")]
    UnexpectedEndOfInput(String),
    #[error("invalid float literal {0}")]
    InvalidFloatLiteral(#[from] ParseFloatError),
    #[error("invalid int literal {0}")]
    InvalidIntLiteral(#[from] ParseIntError),
}

pub type Result<T> = std::result::Result<T, Error>;