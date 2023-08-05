# LSP4SPL

LSP4SPL is implementing the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
for the programming language SPL.

## Background

SPL is a programming language developed by Prof. Dr. Hellwig Geisse at [THM](https://www.thm.de/).
Have a look at [this script](https://homepages.thm.de/~hg53/cb-ss20/praktikum/compprakt.pdf),
written by Prof. Dr. Michael Jäger,
for a specification of the language.

CS students at THM have to write a compiler as part of the CS1019 class.
This project aims to help with the development of test programs for this compiler.

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

## Repository

This repository consists of the following parts:

- the [language server](https://github.com/AlecGhost/LSP4SPL/tree/master/lsp4spl) itself
- the [spl_frontend](https://github.com/AlecGhost/LSP4SPL/tree/master/spl_frontend) library
- the [VS Code extension](https://github.com/AlecGhost/LSP4SPL/tree/master/editors/code)
- a [Tree-sitter grammar](https://github.com/AlecGhost/LSP4SPL/tree/master/editors/nvim/tree-sitter-spl)

## Building

If you haven't already, install [Rust](https://rustup.rs/).

If you are just interested in the binary of the language server,
install it with this command:

```sh
cargo install lsp4spl
```

Alternatively, you can clone this repository
and then build it.

```sh
git clone https://github.com/AlecGhost/LSP4SPL.git
cd LSP4SPL
cargo build --release
```
