use crate::error::{CodecError, ResponseError};
use bytes::{Buf, BytesMut};
use futures::SinkExt;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::mpsc::Receiver;
use tokio_util::codec::{Decoder, Encoder, FramedWrite};

const RPC_VERSION: &str = "2.0";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

pub trait ToValue {
    fn to_value(self) -> serde_json::Value
    where
        Self: Sized + serde::Serialize,
    {
        serde_json::to_value(self).expect("Cannot encode result to json")
    }
}

impl<T> ToValue for T where T: Sized + serde::Serialize {}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Request {
    jsonrpc: String,
    id: i32,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

impl Request {
    #[allow(clippy::missing_const_for_fn)]
    pub fn split(self) -> (Value, PreparedResponse) {
        (self.params, PreparedResponse::new(self.id))
    }
}

pub struct PreparedResponse {
    id: i32,
}

impl PreparedResponse {
    const fn new(id: i32) -> Self {
        Self { id }
    }

    pub fn into_result_response<T: Serialize>(self, value: T) -> Response {
        Response {
            jsonrpc: RPC_VERSION.to_string(),
            id: self.id,
            answer: ResponseAnswer::Result {
                result: value.to_value(),
            },
        }
    }

    pub fn into_error_response(self, error: ResponseError) -> Response {
        Response {
            jsonrpc: RPC_VERSION.to_string(),
            id: self.id,
            answer: ResponseAnswer::Error { error },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Notification {
    jsonrpc: String,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

impl Notification {
    pub fn new(method: String, params: Value) -> Self {
        Self {
            jsonrpc: RPC_VERSION.to_string(),
            method,
            params,
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Response {
    jsonrpc: String,
    id: i32,
    #[serde(flatten)]
    answer: ResponseAnswer,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum ResponseAnswer {
    Result { result: Value },
    Error { error: ResponseError },
}

pub struct LSCodec;

impl Decoder for LSCodec {
    type Item = Message;
    type Error = CodecError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if src.len() < 21 {
            return Ok(None);
        }
        let mut headers = [httparse::EMPTY_HEADER; 2];
        let content_start = match httparse::parse_headers(src, &mut headers)
            .map_err(|_| CodecError::InvalidHeaders)?
        {
            httparse::Status::Complete((content_start, _)) => content_start,
            httparse::Status::Partial => return Ok(None),
        };
        let length_header = headers
            .iter()
            .find(|header| header.name == "Content-Length")
            .ok_or(CodecError::InvalidHeaders)?;
        let length_str =
            std::str::from_utf8(length_header.value).map_err(|_| CodecError::InvalidHeaders)?;
        let content_length: usize = length_str.parse().map_err(|_| CodecError::InvalidHeaders)?;
        let content_end = content_start + content_length;
        if src.len() < content_end {
            return Ok(None);
        }
        let content = &src[content_start..content_end];
        let message = serde_json::from_slice(content).map_err(CodecError::from);
        src.advance(content_end);
        log::info!("Decoded: {:#?}", message);
        message
    }
}

impl Encoder<Message> for LSCodec {
    type Error = CodecError;
    fn encode(&mut self, item: Message, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let content = serde_json::to_string(&item)?;
        let length = content.len();
        let encoded = format!("Content-Length: {}\r\n\r\n{}", length, content);
        dst.reserve(length);
        dst.extend_from_slice(encoded.as_bytes());
        log::info!("Encoded: {:#?}", item);
        Ok(())
    }
}

pub async fn responder(stdout: tokio::io::Stdout, mut rx: Receiver<Message>) {
    let mut framed_write = FramedWrite::new(stdout, LSCodec);
    while let Some(response) = rx.recv().await {
        if let Err(err) = framed_write.send(response).await {
            log::error!("Sending responses failed: {:#?}", err);
            panic!("Sending responses failed: {:#?}", err);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::StreamExt;
    use std::io::Cursor;
    use tokio_util::codec::FramedRead;

    #[tokio::test]
    async fn framed_read_success() {
        let content = r#"{"jsonrpc": "2.0", "id": 1,"method": "initialize","params": null}"#;
        let stdin = Cursor::new(
            format!("Content-Length: {}\r\n\r\n{}", content.len(), content)
                .as_bytes()
                .to_vec(),
        );
        let mut framed_read = FramedRead::new(stdin, LSCodec);
        assert_eq!(
            framed_read.next().await.unwrap().unwrap(),
            Message::Request(Request {
                jsonrpc: "2.0".to_string(),
                id: 1,
                method: "initialize".to_string(),
                params: Value::Null,
            })
        );
        assert!(framed_read.next().await.is_none());
    }
}
