use crate::error::{CodecError, ResponseError};
use bytes::{Buf, BytesMut};
use futures::SinkExt;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::mpsc::Receiver;
use tokio_util::codec::{Decoder, Encoder, FramedWrite};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub(super) enum Message {
    Request(Request),
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) struct Request {
    jsonrpc: String,
    id: i32,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

impl Request {
    pub(super) fn split(self) -> (Value, PreparedResponse) {
        (self.params, PreparedResponse::new(self.id))
    }
}

pub(super) struct PreparedResponse {
    id: i32,
}

impl PreparedResponse {
    fn new(id: i32) -> Self {
        Self { id }
    }

    pub fn into_result_response<T: Serialize>(self, value: T) -> Response {
        Response {
            jsonrpc: "2.0".to_string(),
            id: self.id,
            answer: ResponseAnswer::Result {
                result: value.to_value(),
            },
        }
    }

    pub fn into_error_response(self, error: ResponseError) -> Response {
        Response {
            jsonrpc: "2.0".to_string(),
            id: self.id,
            answer: ResponseAnswer::Error { error },
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) struct Notification {
    jsonrpc: String,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) struct Response {
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

pub(super) struct LSCodec {}

impl LSCodec {
    pub(super) fn new() -> Self {
        Self {}
    }
}

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

impl Encoder<Response> for LSCodec {
    type Error = CodecError;
    fn encode(&mut self, item: Response, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let content = serde_json::to_string(&item)?;
        let length = content.len();
        let encoded = format!("Content-Length: {}\r\n\r\n{}", length, content);
        dst.reserve(length);
        dst.extend_from_slice(encoded.as_bytes());
        log::info!("Encoded: {:#?}", item);
        Ok(())
    }
}

pub(super) async fn responder(stdout: tokio::io::Stdout, mut rx: Receiver<Response>) {
    let mut framed_write = FramedWrite::new(stdout, LSCodec::new());
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
        let mut framed_read = FramedRead::new(stdin, LSCodec::new());
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
