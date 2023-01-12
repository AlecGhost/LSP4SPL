use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_repr::{Deserialize_repr, Serialize_repr};

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

#[derive(Debug, PartialEq)]
pub(super) enum CodecError {
    InvalidHeaders,
    NoUTF8,
    IOError(String),
    InvalidContent,
}

impl From<std::io::Error> for CodecError {
    fn from(value: std::io::Error) -> Self {
        CodecError::IOError(value.to_string())
    }
}

impl From<serde_json::Error> for CodecError {
    fn from(_value: serde_json::Error) -> Self {
        CodecError::InvalidContent
    }
}
