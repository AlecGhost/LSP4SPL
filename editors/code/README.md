# VS Code Extension for LSP4SPL

To get LSP4SPL up and running in VS Code, you need to install this extension.
It starts the language server and hooks it up with VS Code.

## Installation

Currently this extension is not published in the store,
so you need to build it yourself.
You need `npm` and `npx` installed.

First, clone this repository and go into this folder.

```sh
git clone https://github.com/AlecGhost/LSP4SPL.git
cd LSP4SPL/editors/code
```

Then install all the dependencies.

```sh
npm install
```

Afterwards, you can package the extension and manually install it to VS Code.

```sh
npx vsce package
```

## Configuration

You need to tell the extension, where the LSP4SPL binary is located.
To do this, add this to your `settings.json`:

```json
{
   "lsp4spl.executable": "path/to/the/binary"
}
```

If you haven't got the binary yet, consider [building](https://github.com/AlecGhost/LSP4SPL/tree/master#building) it.
