use bytes::{Buf, BytesMut};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio_util::codec::{Decoder, Encoder};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Message {
    jsonrpc: String,
    id: Option<i32>,
    method: String,
    params: Value,
}

impl Message {
    fn new(id: Option<i32>, method: String, params: Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            method,
            params,
        }
    }
}

struct LSCodec {}

impl LSCodec {
    fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, PartialEq)]
enum CodecError {
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

impl Decoder for LSCodec {
    type Item = Message;
    type Error = CodecError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if src.len() < 21 {
            return Ok(None);
        }
        let mut headers = [httparse::EMPTY_HEADER; 2];
        let content_start = match httparse::parse_headers(src, &mut headers) {
            Ok(status) => match status {
                httparse::Status::Complete((content_start, _)) => content_start,
                httparse::Status::Partial => return Ok(None),
            },
            Err(_) => return Err(CodecError::InvalidHeaders),
        };
        let content_end = match headers
            .iter()
            .find(|header| header.name == "Content-Length")
        {
            Some(header) => {
                let length = match std::str::from_utf8(header.value) {
                    Ok(length) => length,
                    Err(_) => return Err(CodecError::InvalidHeaders),
                };
                let length = match usize::from_str_radix(length, 10) {
                    Ok(length) => length,
                    Err(_) => return Err(CodecError::InvalidHeaders),
                };
                length
            }
            None => return Err(CodecError::InvalidHeaders),
        };
        if src.len() < content_end {
            return Ok(None);
        }
        let content = &src[content_start..content_end];
        let message = serde_json::from_slice(content).map_err(|err| CodecError::from(err));
        src.advance(content_end);
        message
    }
}

impl Encoder<Message> for LSCodec {
    type Error = CodecError;
    fn encode(&mut self, item: Message, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let content = serde_json::to_string(&item).map_err(|err| CodecError::from(err))?;
        let header_length = 20;
        let length = header_length + content.len();
        let encoded = format!("Content-Length: {}\r\n\r\n{}", length, content);
        dst.reserve(length);
        dst.extend_from_slice(encoded.as_bytes());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use tokio_stream::StreamExt;
    use tokio_util::codec::FramedRead;

    #[tokio::test]
    async fn framed_read_success() {
        let stdin = Cursor::new(b"Content-Length: 97\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 1,\"method\": \"textDocument/didOpen\",\"params\": null}");
        let mut framed_read = FramedRead::new(stdin, LSCodec::new());
        assert_eq!(
            framed_read.next().await,
            Some(Ok(Message::new(
                Some(1),
                "textDocument/didOpen".to_string(),
                Value::Null
            )))
        );
        assert_eq!(framed_read.next().await, None);
    }
}
