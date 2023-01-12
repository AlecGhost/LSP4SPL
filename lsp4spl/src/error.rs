use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_repr::{Deserialize_repr, Serialize_repr};
use thiserror::Error;

#[derive(Debug, Serialize_repr, Deserialize_repr, PartialEq)]
#[repr(i64)]
pub enum ErrorCode {
    ServerNotInitialized = -32002,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct ResponseError {
    code: ErrorCode,
    message: String,
    data: Option<Value>,
}

impl ResponseError {
    pub fn new(code: ErrorCode, message: String) -> Self {
        Self {
            code,
            message,
            data: None,
        }
    }

    pub fn new_with_data(code: ErrorCode, message: String, data: Value) -> Self {
        Self {
            code,
            message,
            data: Some(data),
        }
    }
}

#[derive(Debug, Error)]
pub(super) enum CodecError {
    #[error("Invalid headers")]
    InvalidHeaders,
    #[error("IO error")]
    IOError(#[from] std::io::Error),
    #[error("Could not deserialize content")]
    InvalidContent(#[from] serde_json::Error),
}
