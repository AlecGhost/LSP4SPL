# VS Code Extension for LSP4SPL

To get LSP4SPL up and running in VS Code, you need to install this extension.
It starts the language server and hooks it up with VS Code.

## Features

The following LSP features are supported:

- [x] Go to Declaration
- [x] Go to Definition
- [x] Go to Type Definition
- [x] Go to Implementation
- [x] Find References
- [x] Hover
- [x] Signature Help
- [x] Folding Range
- [x] Completion Proposals
- [x] Rename
- [x] Prepare Rename
- [x] Semantic Tokens
- [x] Formatting

## Quick Start

1. Install the [Language Server](https://github.com/AlecGhost/LSP4SPL).
2. Configure the path to the executable.

## Configuration

You need to tell the extension, where the LSP4SPL executable is located.
To do this, add this to your `settings.json`:

```json
{
  "lsp4spl.executable": "path/to/the/executable"
}
```
