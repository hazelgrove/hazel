/**
  Compiler entry point.

  The compiler consists a number of passes:

    . Parsing: parsing source into UHExp
    . Elaboration: elaborating into DHExp
    . Transformation: transforming into a high-level IR (hir/Hir.rei); see hir/Transform.rei
    . Linearization: linearizing into a linear, lower-level IR (mir/Anf.rei); see mir/Linearize.rei
    . Optimizeation: optimize the Anf
    . Code generation: generating WebAssembly (currently via Grain); see modules in codegen/grain
 */

[@deriving sexp]
type opts = {
  optimize: Mir.Optimize.opts,
  codegen: Codegen.GrainCodegen.opts,
};

let parse: (~opts: opts, Source.t) => result(UHExp.t, string);
let elaborate: (~opts: opts, UHExp.t) => result((Contexts.t, DHExp.t), unit);
let transform: (~opts: opts, Contexts.t, DHExp.t) => Hir.Expr.expr;
let linearize: (~opts: opts, Hir.Expr.expr) => Mir.Anf.prog;
let optimize: (~opts: opts, Mir.Anf.prog) => Mir.Anf.prog;
let grainize: (~opts: opts, Mir.Anf.prog) => Grainlib.prog;
let print: (~opts: opts, Grainlib.prog) => string;

[@deriving sexp]
type wasm_opts = {
  grain: string,
  includes: list(string),
  wat: bool,
  maximum_memory_pages: int,
  release: bool,
};

let wasmize:
  (~opts: wasm_opts, ~source: string, ~output: string, string) =>
  result(unit, unit);

/*
   Compiler state.
 */
[@deriving sexp]
type state =
  | Source(Source.t)
  | Parsed(UHExp.t)
  | Elaborated(Contexts.t, DHExp.t)
  | Transformed(Hir.Expr.expr)
  | Linearized(Mir.Anf.prog)
  | Optimized(Mir.Anf.prog)
  | Grainized(Grainlib.prog)
  | Printed(string);

/**
  Possible errors from calling `next`.
 */
[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError;

[@deriving sexp]
type next_result = result(option(state), next_error);

/**
  Transition to the next compilation state.
 */
let next: (~opts: opts, state) => next_result;

[@deriving sexp]
type resume_action =
  | Continue(state)
  | Stop;

let stop_after_parsed: state => resume_action;
let stop_after_elaborated: state => resume_action;
let stop_after_transformed: state => resume_action;
let stop_after_linearized: state => resume_action;
let stop_after_optimized: state => resume_action;
let stop_after_grainized: state => resume_action;
let stop_after_printed: state => resume_action;

/**
  Resume from a given state until the last compilation state or `hook` returns
  `Stop`.
 */
let resume:
  (~opts: opts, ~hook: state => resume_action=?, state) => next_result;

/**
  Resume from a given state until DHExp.
 */
let resume_until_elaborated:
  (~opts: opts, state) => result(DHExp.t, next_error);

/**
  Resume from a given state until Hir.
 */
let resume_until_transformed:
  (~opts: opts, state) => result(Hir.Expr.expr, next_error);

/**
  Resume from a given state until Anf.
 */
let resume_until_linearized:
  (~opts: opts, state) => result(Mir.Anf.prog, next_error);

/**
  Resume from a given state until optimized Anf.
 */
let resume_until_optimized:
  (~opts: opts, state) => result(Mir.Anf.prog, next_error);

/**
  Resume from a given state until Grain IR.
 */
let resume_until_grainized:
  (~opts: opts, state) => result(Grainlib.prog, next_error);

/**
  Resume from a given state until textual Grain.
 */
let resume_until_printed: (~opts: opts, state) => result(string, next_error);
