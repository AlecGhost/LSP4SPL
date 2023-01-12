use crate::document::{self, DocumentStatus};
use crate::error::ErrorCode;
use crate::error::ResponseError;
use crate::io::{self, LSCodec, Message, Response};
use color_eyre::eyre::Result;
use futures::StreamExt;
use lsp_types::notification::*;
use lsp_types::request::*;
use lsp_types::*;
use serde_json::Value;
use tokio::io::Stdin;
use tokio::sync::mpsc;
use tokio::sync::mpsc::Sender;
use tokio_util::codec::FramedRead;

#[derive(Clone, Debug)]
pub struct LanguageServer {
    server_info: Option<ServerInfo>,
    server_capabilities: ServerCapabilities,
}

impl LanguageServer {
    pub fn setup(server_info: Option<ServerInfo>, server_capabilities: ServerCapabilities) -> Self {
        Self {
            server_info,
            server_capabilities,
        }
    }

    fn initialize(&self, _params: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: self.server_capabilities.clone(),
            server_info: self.server_info.clone(),
        }
    }

    pub async fn run(self) {
        // spawn thread which handles sending back messages to the client
        let stdout = tokio::io::stdout();
        let (iotx, iorx) = mpsc::channel(32);
        let responder_handle = tokio::spawn(io::responder(stdout, iorx));

        // spawn thread which handles document synchronization
        let (doctx, docrx) = mpsc::channel(32);
        let document_broker_handle = tokio::spawn(document::broker(docrx));

        // Decode messages, while stdin is not closed
        let stdin = tokio::io::stdin();
        let mut framed_read = FramedRead::new(stdin, LSCodec::new());

        async fn run_initialization_phase(
            ls: &LanguageServer,
            framed_read: &mut FramedRead<Stdin, LSCodec>,
            iotx: Sender<Response>,
        ) -> Result<()> {
            while let Some(frame) = framed_read.next().await {
                let message = frame?;
                match message {
                    Message::Request(request) => {
                        let response: Response = match request.method.as_str() {
                            Initialize::METHOD => {
                                let (params, response) = request.split();
                                let params = serde_json::from_value(params)?;
                                let result = ls.initialize(params);
                                let response = response.into_result_response(result);
                                iotx.send(response).await?;
                                break;
                            }
                            _ => {
                                let (_, response) = request.split();
                                response.into_error_response(ResponseError::new(
                                    ErrorCode::ServerNotInitialized,
                                    "Server not initialized".to_string(),
                                ))
                            }
                        };
                        iotx.send(response).await?;
                    }
                    Message::Notification(notification) => {
                        if notification.method.as_str() == Exit::METHOD {
                            std::process::exit(1) // ungraceful exit
                        }
                    }
                };
            }
            while let Some(frame) = framed_read.next().await {
                let message = frame?;
                match message {
                    Message::Request(request) => {
                        // Answer all incoming requests with an error
                        let (_, response) = request.split();
                        let response = response.into_error_response(ResponseError::new(
                            ErrorCode::ServerNotInitialized,
                            "Server not initialized".to_string(),
                        ));
                        iotx.send(response).await?;
                    }
                    Message::Notification(notification) => match notification.method.as_str() {
                        Initialized::METHOD => break, // Server is properly initialized and can start working
                        Exit::METHOD => std::process::exit(1), // ungraceful exit
                        _ => { /* drop all other notifications */ }
                    },
                };
            }
            Ok(())
        }

        // Initialization phase
        if let Err(err) = run_initialization_phase(&self, &mut framed_read, iotx.clone()).await {
            panic!("Unexpected error occured during initialization: {:#?}", err);
        }

        // Main phase
        while let Some(frame) = framed_read.next().await {
            match frame {
                Ok(message) => match message {
                    Message::Request(request) => {
                        let response: Response = match request.method.as_str() {
                            Initialize::METHOD => {
                                let (_, response) = request.split();
                                response.into_error_response(ResponseError::new(
                                    ErrorCode::InvalidRequest,
                                    "Initialize method shall only be send once".to_string(),
                                ))
                            }
                            Shutdown::METHOD => {
                                // exit main phase
                                let (_, response) = request.split();
                                let response = response.into_result_response(Value::Null);
                                iotx.send(response).await.expect("Cannot send messages");
                                break;
                            }
                            unknown_method => {
                                let method_name = unknown_method.to_string();
                                let (_, response) = request.split();
                                response.into_error_response(ResponseError::new(
                                    ErrorCode::MethodNotFound,
                                    format!("Unknown method {}", method_name).to_string(),
                                ))
                            }
                        };
                        iotx.send(response).await.expect("Cannot send messages");
                    }
                    Message::Notification(notification) => {
                        async fn match_notification(
                            notification: io::Notification,
                            doctx: Sender<DocumentStatus>,
                        ) -> Result<()> {
                            match notification.method.as_str() {
                                DidOpenTextDocument::METHOD => {
                                    let params = serde_json::from_value(notification.params)?;
                                    document::did_open(doctx.clone(), params).await?;
                                }
                                DidChangeTextDocument::METHOD => {
                                    let params = serde_json::from_value(notification.params)?;
                                    document::did_change(doctx.clone(), params).await?;
                                }
                                DidCloseTextDocument::METHOD => {
                                    let params = serde_json::from_value(notification.params)?;
                                    document::did_close(doctx.clone(), params).await?;
                                }
                                Exit::METHOD => std::process::exit(1), // ungraceful exit
                                _ => { /* drop all other notifications */ }
                            };
                            Ok(())
                        }

                        if let Err(err) = match_notification(notification, doctx.clone()).await {
                            // TODO: proper error logging
                            eprintln!("Error occured during notification handling: {:#?}", err);
                        }
                    }
                },
                Err(err) => panic!("Recieved frame with error: {:?}", err),
            };
        }

        async fn run_shutdown_phase(
            framed_read: &mut FramedRead<Stdin, LSCodec>,
            tx: Sender<Response>,
        ) -> Result<()> {
            while let Some(frame) = framed_read.next().await {
                let message = frame?;
                match message {
                    Message::Request(request) => {
                        // Answer all incoming requests with an error
                        let (_, response) = request.split();
                        let response = response.into_error_response(ResponseError::new(
                            ErrorCode::InvalidRequest,
                            "Server shutting down. No further requests allowed".to_string(),
                        ));
                        tx.send(response).await?;
                    }
                    Message::Notification(notification) => {
                        // only waiting for exit notification
                        if notification.method.as_str() == Exit::METHOD {
                            break;
                        }
                    }
                };
            }
            Ok(())
        }

        // Shutdown phase
        if let Err(err) = run_shutdown_phase(&mut framed_read, iotx).await {
            panic!("Unexpected error occured during shutdown: {:#?}", err);
        }
        drop(doctx);
        responder_handle.await.unwrap();
        document_broker_handle.await.unwrap();
        // tokio::join!(responder_handle, document_broker_handle);
    }
}
