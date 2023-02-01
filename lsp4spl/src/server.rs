use crate::{
    document::{self, DocumentRequest},
    error::{ErrorCode, ResponseError},
    io::{self, LSCodec, Message, Response},
};
use color_eyre::eyre::Result;
use futures::StreamExt;
use lsp_types::{notification::*, request::*, *};
use serde_json::Value;
use tokio::sync::mpsc::{self, Sender};
use tokio_util::codec::FramedRead;

#[derive(Clone, Debug, Default)]
struct ClientDetails {
    diagnostics: bool,
}

#[derive(Clone, Debug)]
pub struct LanguageServer {
    server_info: Option<ServerInfo>,
    server_capabilities: ServerCapabilities,
    client_details: ClientDetails,
}

impl LanguageServer {
    pub fn setup(server_info: Option<ServerInfo>, server_capabilities: ServerCapabilities) -> Self {
        Self {
            server_info,
            server_capabilities,
            client_details: ClientDetails::default(),
        }
    }

    fn initialize(&mut self, params: InitializeParams) -> InitializeResult {
        self.client_details.diagnostics = params
            .capabilities
            .text_document
            .and_then(|caps| caps.publish_diagnostics)
            .is_some();
        InitializeResult {
            capabilities: self.server_capabilities.clone(),
            server_info: self.server_info.clone(),
        }
    }

    pub async fn run(mut self) {
        let mut handles = vec![];
        // spawn thread which handles sending back messages to the client
        let stdout = tokio::io::stdout();
        let (iotx, iorx) = mpsc::channel(32);
        handles.push(tokio::spawn(io::responder(stdout, iorx)));

        // decode messages, while stdin is not closed
        let stdin = tokio::io::stdin();
        let mut framed_read = FramedRead::new(stdin, LSCodec::new());

        // initialization phase
        if let Err(err) = phases::initialization(&mut self, &mut framed_read, iotx.clone()).await {
            log::error!("Unexpected error occured during initialization: {:#?}", err);
            panic!("Unexpected error occured during initialization: {:#?}", err);
        }

        // spawn thread which handles document synchronization
        let (doctx, docrx) = mpsc::channel(32);
        handles.push(tokio::spawn(document::broker(
            docrx,
            iotx.clone(),
            self.client_details.diagnostics,
        )));

        // main phase
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
                                iotx.send(Message::Response(response))
                                    .await
                                    .expect("Cannot send messages");
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
                        iotx.send(Message::Response(response))
                            .await
                            .expect("Cannot send messages");
                    }
                    Message::Notification(notification) => {
                        async fn match_notification(
                            notification: io::Notification,
                            doctx: Sender<DocumentRequest>,
                        ) -> Result<()> {
                            match notification.method.as_str() {
                                DidOpenTextDocument::METHOD => {
                                    let params = serde_json::from_value(notification.params)?;
                                    document::open(doctx.clone(), params).await?;
                                }
                                DidChangeTextDocument::METHOD => {
                                    let params = serde_json::from_value(notification.params)?;
                                    document::change(doctx.clone(), params).await?;
                                }
                                DidCloseTextDocument::METHOD => {
                                    let params = serde_json::from_value(notification.params)?;
                                    document::close(doctx.clone(), params).await?;
                                }
                                Exit::METHOD => std::process::exit(1), // ungraceful exit
                                _ => { /* drop all other notifications */ }
                            };
                            Ok(())
                        }

                        if let Err(err) = match_notification(notification, doctx.clone()).await {
                            log::warn!("Error occured during notification handling: {:#?}", err);
                        }
                    }
                    Message::Response(_) => {
                        panic!("Cannot handle responses")
                    }
                },
                Err(err) => {
                    log::error!("Recieved frame with error: {:?}", err);
                    panic!("Recieved frame with error: {:?}", err);
                }
            };
        }

        // shutdown phase
        if let Err(err) = phases::shutdown(&mut framed_read, iotx).await {
            log::error!("Unexpected error occured during shutdown: {:#?}", err);
            panic!("Unexpected error occured during shutdown: {:#?}", err);
        }
        drop(doctx);
        for handle in handles {
            handle.await.unwrap()
        }
        // tokio::join!(responder_handle, document_broker_handle);
    }
}

mod phases {
    use super::LanguageServer;
    use crate::{
        error::{ErrorCode, ResponseError},
        io::{LSCodec, Message, Response},
    };
    use color_eyre::eyre::Result;
    use futures::StreamExt;
    use lsp_types::{notification::*, request::*};
    use tokio::{io::Stdin, sync::mpsc::Sender};
    use tokio_util::codec::FramedRead;

    pub(super) async fn initialization(
        ls: &mut LanguageServer,
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        iotx: Sender<Message>,
    ) -> Result<()> {
        while let Some(frame) = framed_read.next().await {
            let message = frame?;
            match message {
                Message::Request(request) => {
                    let response: Response = if request.method.as_str() == Initialize::METHOD {
                        let (params, response) = request.split();
                        let params = serde_json::from_value(params)?;
                        let result = ls.initialize(params);
                        let response = response.into_result_response(result);
                        iotx.send(Message::Response(response)).await?;
                        break;
                    } else {
                        let (_, response) = request.split();
                        response.into_error_response(ResponseError::new(
                            ErrorCode::ServerNotInitialized,
                            "Server not initialized".to_string(),
                        ))
                    };
                    iotx.send(Message::Response(response)).await?;
                }
                Message::Notification(notification) => {
                    if notification.method.as_str() == Exit::METHOD {
                        std::process::exit(1) // ungraceful exit
                    }
                }
                Message::Response(_) => {
                    panic!("Cannot handle responses")
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
                    iotx.send(Message::Response(response)).await?;
                }
                Message::Notification(notification) => match notification.method.as_str() {
                    Initialized::METHOD => break, // Server is properly initialized and can start working
                    Exit::METHOD => std::process::exit(1), // ungraceful exit
                    _ => { /* drop all other notifications */ }
                },
                Message::Response(_) => {
                    panic!("Cannot handle responses")
                }
            };
        }
        Ok(())
    }

    pub(super) async fn shutdown(
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        tx: Sender<Message>,
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
                    tx.send(Message::Response(response)).await?;
                }
                Message::Notification(notification) => {
                    // only waiting for exit notification
                    if notification.method.as_str() == Exit::METHOD {
                        break;
                    }
                }
                Message::Response(_) => {
                    panic!("Cannot handle responses")
                }
            };
        }
        Ok(())
    }
}
