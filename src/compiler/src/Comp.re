open Grain_parsing;
// open Grain_typed;
// open Grain_middle_end;
// open Grain_codegen;
// open Grain_linking;
// open Optimize;

[@deriving sexp]
type t =
  | Initial(DHExp.t)
  | Parsed(Parsetree.parsed_program)
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

type error = NotImplemented;

let trans_top_statement = (d:DHExp.t) : Parsetree.toplevel_stmt => 
  switch(d) {
    | 
  }

let trans_parsed = (d:DHExp.t) : Parsetree.parsed_program => {
  let top_statement : Grain_parsing.Parsetree.toplevel_stmt = {
    ptop_desc: trans_top_statement(d),
    ptop_attributes: [],
    ptop_loc: Grain_parsing.Location.dummy_loc,
  };
  {statements : [], comments : [], prog_loc : Grain_parsing.Location.dummy_loc};
}

