# Hazel Menhir Parser
> This directory contains a Menhir parser for textual Hazel `Exp` syntax and a utility to convert the Menhir AST to post-elaboration `Exp`. This is used for Hazel elaborator unit tests
* The Menhir parser is located in the `Parser.mly` file
* The ocamllex lexer can be found in `Lexer.mll`
* `Interface.re` contains functions to interface with the parser and parse text into an `AST` structure
* The parser's AST is defined in `AST.re`
* `Conversion.re` contains utility functions to convert the `AST` structure into the Hazel post-elaboration `Exp` structure
* Examples of the textual syntax can be found in the `Exp` module of the `TermBase.re` file
