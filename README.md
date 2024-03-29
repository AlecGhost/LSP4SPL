# LSP4SPL

LSP4SPL is implementing the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
for the Simple Programming Language (SPL).

## Background

SPL is a programming language developed by Prof. Dr. Hellwig Geisse at [THM](https://www.thm.de/).
Have a look at [this script](https://homepages.thm.de/~hg53/cb-ss20/praktikum/compprakt.pdf),
written by Prof. Dr. Michael Jäger,
for a specification of the language.

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

## Installation

You can download the latest binary
from the [releases page](https://github.com/AlecGhost/LSP4SPL/releases).

Alternatively, you can install the binary with `cargo`,
if you have [Rust](https://rustup.rs/) installed.

```sh
cargo install lsp4spl
```

Of course, you can also clone this repository
and then build it.

```sh
git clone https://github.com/AlecGhost/LSP4SPL.git
cd LSP4SPL
cargo build --release
```
