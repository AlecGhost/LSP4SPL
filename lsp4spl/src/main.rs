#![allow(dead_code)]
use error::ErrorCode;
use error::ResponseError;
use futures::{SinkExt, StreamExt};
use io::{LSCodec, Message, Response};
use lsp_types::notification::*;
use lsp_types::request::*;
use lsp_types::*;
use serde_json::Value;
use tokio::io::Stdin;
use tokio::sync::mpsc;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::Sender;
use tokio_util::codec::{FramedRead, FramedWrite};

mod error;
mod io;

#[tokio::main]
async fn main() {
    eprintln!("LSP4SPL: Startup.");
    let ls = LanguageServer::setup(
        Some(ServerInfo {
            name: "lsp4spl".to_string(),
            version: Some("0.1".to_string()),
        }),
        ServerCapabilities {
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
    );
    ls.run().await;
    eprintln!("LSP4SPL: Shutdown.");
}

#[derive(Clone, Debug)]
struct LanguageServer {
    server_info: Option<ServerInfo>,
    server_capabilities: ServerCapabilities,
}

impl LanguageServer {
    fn setup(server_info: Option<ServerInfo>, server_capabilities: ServerCapabilities) -> Self {
        Self {
            server_info,
            server_capabilities,
        }
    }

    async fn run(self) {
        // spawn thread which handles sending back messages to the client
        let stdout = tokio::io::stdout();
        let (tx, rx) = mpsc::channel(32);
        let responder_handle = tokio::spawn(responder(stdout, rx));

        // Decode messages, while stdin is not closed
        let stdin = tokio::io::stdin();
        let mut framed_read = FramedRead::new(stdin, LSCodec::new());

        // Initialization phase
        self.run_initialization_phase(&mut framed_read, tx.clone())
            .await;

        // Main phase
        while let Some(frame) = framed_read.next().await {
            match frame {
                Ok(message) => match message {
                    Message::Request(request) => {
                        let response: Response = match request.method.as_str() {
                            Initialize::METHOD => {
                                let (_, response) = request.split();
                                response.to_error_response(ResponseError::new(
                                    ErrorCode::InvalidRequest,
                                    "Initialize method shall only be send once".to_string(),
                                ))
                            }
                            Shutdown::METHOD => {
                                // exit main phase
                                let (_, response) = request.split();
                                let response = response.to_result_response(Value::Null);
                                tx.send(response).await.expect("Cannot send messages");
                                break;
                            }
                            unknown_method => {
                                let method_name = unknown_method.to_string();
                                let (_, response) = request.split();
                                response.to_error_response(ResponseError::new(
                                    ErrorCode::MethodNotFound,
                                    format!("Unknown method {}", method_name).to_string(),
                                ))
                            }
                        };
                        tx.send(response).await.expect("Cannot send messages");
                    }
                    Message::Notification(notification) => match notification.method.as_str() {
                        Exit::METHOD => std::process::exit(1), // ungraceful exit
                        _ => { /* drop all other notifications */ }
                    },
                },
                Err(err) => panic!("Recieved frame with error: {:?}", err),
            };
        }

        // Shutdown phase
        self.run_shutdown_phase(&mut framed_read, tx).await;
        responder_handle.await.expect("Should shutdown gracefully");
    }

    async fn run_initialization_phase(
        &self,
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        tx: Sender<Response>,
    ) {
        while let Some(frame) = framed_read.next().await {
            match frame {
                Ok(message) => match message {
                    Message::Request(request) => {
                        let response: Response = match request.method.as_str() {
                            Initialize::METHOD => {
                                let (params, response) = request.split();
                                let params = serde_json::from_value(params).expect("Ivalid params");
                                let result = self.initialize(params);
                                let response = response.to_result_response(result);
                                tx.send(response).await.expect("Cannot send messages");
                                break;
                            }
                            _ => {
                                let (_, response) = request.split();
                                response.to_error_response(ResponseError::new(
                                    error::ErrorCode::ServerNotInitialized,
                                    "Server not initialized".to_string(),
                                ))
                            }
                        };
                        tx.send(response).await.expect("Cannot send messages");
                    }
                    Message::Notification(notification) => match notification.method.as_str() {
                        Exit::METHOD => std::process::exit(1), // ungraceful exit
                        _ => { /* drop all other notifications */ }
                    },
                },
                Err(err) => panic!("Recieved frame with error: {:?}", err),
            };
        }
        while let Some(frame) = framed_read.next().await {
            match frame {
                Ok(message) => match message {
                    Message::Request(request) => {
                        // Answer all incoming requests with an error
                        let (_, response) = request.split();
                        let response = response.to_error_response(ResponseError::new(
                            error::ErrorCode::ServerNotInitialized,
                            "Server not initialized".to_string(),
                        ));
                        tx.send(response).await.expect("Cannot send messages");
                    }
                    Message::Notification(notification) => match notification.method.as_str() {
                        Initialized::METHOD => {
                            // Server is properly initialized and can start working
                            break;
                        }
                        Exit::METHOD => std::process::exit(1), // ungraceful exit
                        _ => { /* drop all other notifications */ }
                    },
                },
                Err(err) => panic!("Recieved frame with error: {:?}", err),
            };
        }
    }

    async fn run_shutdown_phase(
        &self,
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        tx: Sender<Response>,
    ) {
        while let Some(frame) = framed_read.next().await {
            match frame {
                Ok(message) => match message {
                    Message::Request(request) => {
                        // Answer all incoming requests with an error
                        let (_, response) = request.split();
                        let response = response.to_error_response(ResponseError::new(
                            error::ErrorCode::InvalidRequest,
                            "Server shutting down. No further requests allowed".to_string(),
                        ));
                        tx.send(response).await.expect("Cannot send messages");
                    }
                    Message::Notification(notification) => match notification.method.as_str() {
                        // only waiting for exit notification
                        Exit::METHOD => break,
                        _ => { /* drop all other notifications */ }
                    },
                },
                Err(err) => panic!("Recieved frame with error: {:?}", err),
            };
        }
    }

    fn initialize(&self, _params: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: self.server_capabilities.clone(),
            server_info: self.server_info.clone(),
        }
    }
}

async fn responder(stdout: tokio::io::Stdout, mut rx: Receiver<Response>) {
    let mut framed_write = FramedWrite::new(stdout, LSCodec::new());
    while let Some(response) = rx.recv().await {
        if let Err(err) = framed_write.send(response).await {
            panic!("Sending messages failed: {:#?}", err);
        }
    }
}
