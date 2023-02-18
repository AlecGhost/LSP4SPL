use crate::{document, io};
use color_eyre::eyre::{Context, Result};
use lsp_types::*;
use tokio::sync::mpsc;
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

    pub async fn run(mut self) -> Result<()> {
        let mut handles = vec![];
        // spawn thread which handles sending back messages to the client
        let stdout = tokio::io::stdout();
        let (iotx, iorx) = mpsc::channel(32);
        handles.push(tokio::spawn(io::responder(stdout, iorx)));

        // decode messages, while stdin is not closed
        let stdin = tokio::io::stdin();
        let mut framed_read = FramedRead::new(stdin, io::LSCodec);

        phases::initialization(&mut self, &mut framed_read, iotx.clone())
            .await
            .wrap_err("Unexpected error occured during initialization")?;

        // spawn thread which handles document synchronization
        let (doctx, docrx) = mpsc::channel(32);
        handles.push(tokio::spawn(document::broker(
            docrx,
            iotx.clone(),
            self.client_details.diagnostics,
        )));

        phases::main(&mut framed_read, iotx.clone(), doctx.clone())
            .await
            .wrap_err("Unexpected error occured during main phase")?;

        phases::shutdown(&mut framed_read, iotx)
            .await
            .wrap_err("Unexpected error occured during shutdown")?;

        drop(doctx);
        for handle in handles {
            handle.await.expect("Cannot await handle");
        }
        Ok(())
    }
}

mod phases {
    use super::LanguageServer;
    use crate::{
        document::{self, DocumentRequest},
        error::{ErrorCode, ResponseError},
        features,
        io::{LSCodec, Message, Response},
    };
    use color_eyre::eyre::{eyre, Context, Result};
    use futures::StreamExt;
    use lsp_types::{notification::*, request::*};
    use serde_json::Value;
    use tokio::{io::Stdin, sync::mpsc::Sender};
    use tokio_util::codec::FramedRead;

    macro_rules! respond {
        ($request:ident, $func:path, $doctx:expr) => {{
            let (params, response) = $request.split();
            let params = serde_json::from_value(params)?;
            let result = $func($doctx, params)
                .await
                .wrap_err("Cannot process request")?;
            response.into_result_response(result)
        }};
    }

    macro_rules! note {
        ($notification:ident, $func:path, $doctx:expr) => {{
            let params = serde_json::from_value($notification.params)?;
            $func($doctx, params)
                .await
                .wrap_err("Cannot process notification")?;
        }};
    }

    pub(super) async fn initialization(
        ls: &mut LanguageServer,
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        iotx: Sender<Message>,
    ) -> Result<()> {
        while let Some(frame) = framed_read.next().await {
            let message = frame.wrap_err("Recieved frame with error")?;
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
                Message::Response(response) => {
                    return Err(eyre!("Cannot handle responses: {:?}", response));
                }
            };
        }
        while let Some(frame) = framed_read.next().await {
            let message = frame.wrap_err("Recieved frame with error")?;
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
                Message::Response(response) => {
                    return Err(eyre!("Cannot handle responses: {:?}", response));
                }
            };
        }
        Ok(())
    }

    pub(super) async fn main(
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        iotx: Sender<Message>,
        doctx: Sender<DocumentRequest>,
    ) -> Result<()> {
        while let Some(frame) = framed_read.next().await {
            let message = frame.wrap_err("Recieved frame with error")?;
            match message {
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
                            iotx.send(Message::Response(response)).await?;
                            return Ok(());
                        }
                        GotoDeclaration::METHOD => {
                            respond!(request, features::goto::declaration, doctx.clone())
                        }
                        GotoDefinition::METHOD => {
                            respond!(request, features::goto::definition, doctx.clone())
                        }
                        GotoImplementation::METHOD => {
                            respond!(request, features::goto::implementation, doctx.clone())
                        }
                        GotoTypeDefinition::METHOD => {
                            respond!(request, features::goto::type_definition, doctx.clone())
                        }
                        References::METHOD => {
                            respond!(request, features::references::find, doctx.clone())
                        }
                        HoverRequest::METHOD => {
                            respond!(request, features::hover, doctx.clone())
                        }
                        Rename::METHOD => {
                            respond!(request, features::references::rename, doctx.clone())
                        }
                        PrepareRenameRequest::METHOD => {
                            respond!(request, features::references::prepare_rename, doctx.clone())
                        }
                        Completion::METHOD => {
                            respond!(request, features::completion::propose, doctx.clone())
                        }
                        FoldingRangeRequest::METHOD => {
                            respond!(request, features::fold, doctx.clone())
                        }
                        unknown_method => {
                            let method_name = unknown_method.to_string();
                            let (_, response) = request.split();
                            response.into_error_response(ResponseError::new(
                                ErrorCode::MethodNotFound,
                                format!("Unknown method {}", method_name),
                            ))
                        }
                    };
                    iotx.send(Message::Response(response)).await?;
                }
                Message::Notification(notification) => {
                    match notification.method.as_str() {
                        DidOpenTextDocument::METHOD => {
                            note!(notification, document::open, doctx.clone())
                        }
                        DidChangeTextDocument::METHOD => {
                            note!(notification, document::change, doctx.clone())
                        }
                        DidCloseTextDocument::METHOD => {
                            note!(notification, document::close, doctx.clone())
                        }
                        Exit::METHOD => std::process::exit(1), // ungraceful exit
                        _ => { /* drop all other notifications */ }
                    };
                }
                Message::Response(response) => {
                    return Err(eyre!("Cannot handle responses: {:#?}", response));
                }
            }
        }
        Ok(())
    }

    pub(super) async fn shutdown(
        framed_read: &mut FramedRead<Stdin, LSCodec>,
        tx: Sender<Message>,
    ) -> Result<()> {
        while let Some(frame) = framed_read.next().await {
            let message = frame.wrap_err("Recieved frame with error")?;
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
                Message::Response(response) => {
                    return Err(eyre!("Cannot handle responses {:#?}", response));
                }
            };
        }
        Ok(())
    }
}
