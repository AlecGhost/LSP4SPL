# LSP4SPL

LSP4SPL is a Language Server for SPL.
It uses the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) developed by Microsoft.

## Background

SPL is a programming language developed by Prof. Dr. Hellwig Geisse at [THM](https://www.thm.de/).
Have a look at [this script](https://homepages.thm.de/~hg53/cb-ss20/praktikum/compprakt.pdf),
written by Prof. Dr. Michael Jäger,
for a specification of the language.

CS students at THM have to write a compiler as part of the CS1019 class.
This project aims to help with the development of test programs for this compiler.

## Repository

This repository consists of the [language server](https://github.com/AlecGhost/LSP4SPL/tree/master/lsp4spl) itself,
the [spl_frontend](https://github.com/AlecGhost/LSP4SPL/tree/master/spl_frontend) library,
that provides the functionality to parse and analyze SPL source code
and the [extension](https://github.com/AlecGhost/LSP4SPL/tree/master/editors/code)
that is required to use LSP4SPL in VSCode.

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
