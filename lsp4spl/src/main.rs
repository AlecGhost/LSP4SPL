#![allow(dead_code)]
use io::{LSCodec, Message};
use lsp_types::*;
use tokio_stream::StreamExt;
use tokio_util::codec::FramedRead;

mod io;

#[tokio::main]
async fn main() {
    eprintln!("LSP4SPL: Startup.");
    let mut ls = LanguageServer {};
    ls.run().await;
    eprintln!("LSP4SPL: Shutdown.");
}

struct LanguageServer {}

impl LanguageServer {
    async fn run(&mut self) {
        let stdin = tokio::io::stdin();
        let mut framed_read = FramedRead::new(stdin, LSCodec::new());
        while let Some(frame) = framed_read.next().await {
            match frame {
                Ok(message) => self.rpc(message),
                Err(err) => panic!("Recieved frame with error: {:?}", err),
            };
        }
    }

    fn rpc(&mut self, message: Message) {
        match message.method.as_str() {
            "initialize" => {
                let params = serde_json::from_value(message.params).expect("Valid params");
                self.initialize(params);
            }
            _ => panic!("Unknown function: {}", message.method),
        }
    }

    fn initialize(&self, params: InitializeParams) -> InitializeResult {
        unimplemented!();
    }
}
