// open Grain_parsing;
open Grain_typed;
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
  | TypedWellFormed(Typedtree.typed_program)
  // | Linearized(Anftree.anf_program)
  // | Optimized(Anftree.anf_program)
  // | Mashed(Mashtree.mash_program)
  // | Compiled(Compmod.compiled_program)
  // | ObjectFileEmitted(Compmod.compiled_program)
  // | Linked(Compmod.compiled_program)
  | Assembled;

let compile = (d: DHExp.t): t => Initial(d);

let trans_typed = (d:DHExp.t) : Typedtree.typed_program => {
  let sourcefile = prog.prog_loc.loc_start.pos_fname;
  /* TODO: Do we maybe need a fallback here? */
  let modulename = Grain_utils.Files.filename_to_module_name(sourcefile);
  Env.set_unit((modulename, sourcefile, get_compilation_mode()));
  let initenv = initial_env();
  let (stritems, sg, finalenv) = type_module(initenv, prog);
  let (statements, env) = stritems;
  let simple_sg = simplify_signature(sg);
  let filename = sourcefile; /* TODO: I think this is okay */
  let coercion =
    Includemod.compunit(
      initenv,
      ~mark=Includemod.Mark_positive,
      sourcefile,
      sg,
      "(inferred signature)",
      simple_sg,
    );

  check_nongen_schemes(finalenv, simple_sg);
  let normalized_sig = normalize_signature(finalenv, simple_sg);
  let signature = Env.build_signature(normalized_sig, modulename, filename);
  ignore(coercion);
  {statements, Env.empty, Cmi.hazel_cmi, comments: []};
}