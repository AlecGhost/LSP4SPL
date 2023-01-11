#![allow(dead_code)]
use futures::{SinkExt, StreamExt};
use io::{LSCodec, Message, Response};
use lsp_types::notification::*;
use lsp_types::request::*;
use lsp_types::*;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::{self, Sender};
use tokio_util::codec::{FramedRead, FramedWrite};

mod io;

#[tokio::main]
async fn main() {
    eprintln!("LSP4SPL: Startup.");
    let ls = LanguageServer {
        server_info: Some(ServerInfo {
            name: "lsp4spl".to_string(),
            version: Some("0.1".to_string()),
        }),
        server_capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: None,
                },
            )),
            selection_range_provider: None,
            hover_provider: None,
            completion_provider: None,
            signature_help_provider: None,
            definition_provider: None,
            type_definition_provider: None,
            implementation_provider: None,
            references_provider: None,
            document_highlight_provider: None,
            document_symbol_provider: None,
            workspace_symbol_provider: None,
            code_action_provider: None,
            code_lens_provider: None,
            document_formatting_provider: None,
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            rename_provider: None,
            document_link_provider: None,
            color_provider: None,
            folding_range_provider: None,
            declaration_provider: None,
            execute_command_provider: None,
            workspace: None,
            call_hierarchy_provider: None,
            semantic_tokens_provider: None,
            moniker_provider: None,
            linked_editing_range_provider: None,
            experimental: None,
        },
    };
    run(ls).await;
    eprintln!("LSP4SPL: Shutdown.");
}

#[derive(Clone, Debug)]
struct LanguageServer {
    server_info: Option<ServerInfo>,
    server_capabilities: ServerCapabilities,
}

impl LanguageServer {
    fn initialize(&self, _params: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: self.server_capabilities.clone(),
            server_info: self.server_info.clone(),
        }
    }

    fn shutdown(&self) {}
}

async fn run(ls: LanguageServer) {
    // spawn thread which handles sending back messages to the client
    let stdout = tokio::io::stdout();
    let (tx, rx) = mpsc::channel(32);
    tokio::spawn(responder(stdout, rx));

    // Decode messages, while stdin is not closed
    let stdin = tokio::io::stdin();
    let mut framed_read = FramedRead::new(stdin, LSCodec::new());
    while let Some(frame) = framed_read.next().await {
        match frame {
            Ok(message) => rpc(ls.clone(), tx.clone(), message),
            Err(err) => panic!("Recieved frame with error: {:?}", err),
        };
    }
}

fn rpc(ls: LanguageServer, tx: Sender<Response>, message: Message) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        match message {
            Message::Request(request) => {
                let response: Response = match request.method.as_str() {
                    Initialize::METHOD => {
                        let (params, response) = request.split();
                        let params = serde_json::from_value(params).expect("Ivalid params");
                        let result = ls.initialize(params);
                        response.to_result_response(result)
                    }
                    Shutdown::METHOD => {
                        let (_, response) = request.split();
                        ls.shutdown();
                        response.to_result_response(serde_json::Value::Null)
                    }
                    _ => panic!("Unknown request: {}", request.method),
                };
                tx.send(response).await.expect("Cannot send messages");
            }
            Message::Notification(notification) => match notification.method.as_str() {
                Initialized::METHOD => {}
                DidOpenTextDocument::METHOD => {}
                DidChangeTextDocument::METHOD => {}
                DidCloseTextDocument::METHOD => {}
                Exit::METHOD => std::process::exit(0),
                _ => panic!("Unknown notification: {}", notification.method),
            },
        }
    })
}

async fn responder(stdout: tokio::io::Stdout, mut rx: Receiver<Response>) {
    let mut framed_write = FramedWrite::new(stdout, LSCodec::new());
    while let Some(response) = rx.recv().await {
        if let Err(err) = framed_write.send(response).await {
            panic!("Sending messages failed: {:#?}", err);
        }
    }
}
