import * as path from 'path';
import { ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  let debugExecutable = context.asAbsolutePath(
    path.join('..', '..', 'target', 'debug', 'lsp4spl')
  );
  let logFile = context.asAbsolutePath(
    path.join('..', '..', 'lsp4spl.log')
  )
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions: ServerOptions = {
    command: debugExecutable,
    transport: TransportKind.stdio,
    args: ['--log', logFile],
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'spl' }],
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'lsp4spl',
    'LSP4SPL',
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}