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

impl Error {
    pub fn unexpected(expected: impl Into<String>, found: impl Into<String>) -> Self {
        Error::UnexpectedToken(expected.into(), found.into())
    }

    pub fn swap_expected(self, expected: impl Into<String>) -> Self {
        match self {
            Error::UnexpectedToken(_, found) => Self::unexpected(expected, found),
            err => err,
        }
    }

    pub fn swap_found(self, found: impl Into<String>) -> Self {
        match self {
            Error::UnexpectedToken(expected, _) => Self::unexpected(expected, found),
            err => err,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;