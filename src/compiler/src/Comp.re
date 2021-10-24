// open Grain_parsing;
// open Grain_typed;
// open Grain_middle_end;
// open Grain_codegen;
// open Grain_linking;
// open Optimize;

[@deriving sexp]
type t =
  | Initial(DHExp.t)
  // | Parsed(Parsetree.parsed_program)
  // | WellFormed(Parsetree.parsed_program)
  // | TypeChecked(Typedtree.typed_program)
  // | TypedWellFormed(Typedtree.typed_program)
  // | Linearized(Anftree.anf_program)
  // | Optimized(Anftree.anf_program)
  // | Mashed(Mashtree.mash_program)
  // | Compiled(Compmod.compiled_program)
  // | ObjectFileEmitted(Compmod.compiled_program)
  // | Linked(Compmod.compiled_program)
  | Assembled;

let compile = (d: DHExp.t): t => Initial(d);