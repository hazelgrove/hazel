# High Level Mapper

## Purpose
This folder's main role is to offer utility functions which can lazily build and work with high-level abstraction of the AST.
It takes Hazel's default AST (the info map) and converts it into "nodes" of high-level terms. 

Currently supported high-level terms:
- Let Bindings
- Type Aliases

Future support is planned for the following high-level terms:
- Pattern Matches
- Functions (maybe, probably not, since let bindings typically cover these)

