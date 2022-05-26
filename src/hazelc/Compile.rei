/*
   Compiler entry point.
 */

[@deriving sexp]
type opts = {indet_analysis: option(IndetAnalysis.analysis_level)};

[@deriving sexp]
type grain_opts = Grain.opts;

/* FIXME: Make these functions not take grain_opts. */
let parse: (~opts: opts, Source.t) => result(UHExp.t, string);
let elaborate: (~opts: opts, UHExp.t) => result((Contexts.t, DHExp.t), unit);
let transform: (~opts: opts, Contexts.t, DHExp.t) => Hir.expr;
let linearize: (~opts: opts, Hir.expr) => Anf.prog;
let optimize: (~opts: opts, Anf.prog) => Anf.prog;
let grainize: (~opts: opts, Anf.prog) => GrainIR.prog;
let print: (~opts: opts, GrainIR.prog) => string;

let wasmize:
  (~opts: grain_opts, string, string, string) => result(unit, unit);

/*
   Compiler state.
 */
[@deriving sexp]
type state =
  | Source(Source.t)
  | Parsed(UHExp.t)
  | Elaborated(Contexts.t, DHExp.t)
  | Transformed(Hir.expr)
  | Linearized(Anf.prog)
  | Optimized(Anf.prog)
  | Grainized(GrainIR.prog)
  | Printed(string);

/*
    Possible errors from calling `next`.
 */
[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError;

[@deriving sexp]
type next_result = result(option(state), next_error);

/*
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

/*
   Resume from a given state until the last compilation state or `hook` returns
   `Stop`.
 */
let resume:
  (~opts: opts, ~hook: state => resume_action=?, state) => next_result;

/*
   Exception indicative of an error in `resume_until_*`.
 */
exception BadState;

/*
   Resume from a given state until DHExp.
 */
let resume_until_dhexp: (~opts: opts, state) => result(DHExp.t, next_error);

/*
   Resume from a given state until Hir.
 */
let resume_until_hir: (~opts: opts, state) => result(Hir.expr, next_error);

/*
   Resume from a given state until Anf.
 */
let resume_until_anf: (~opts: opts, state) => result(Anf.prog, next_error);

/*
   Resume from a given state until optimized Anf.
 */
let resume_until_optimized:
  (~opts: opts, state) => result(Anf.prog, next_error);

/*
   Resume from a given state until Grain IR.
 */
let resume_until_grain:
  (~opts: opts, state) => result(GrainIR.prog, next_error);

/*
   Resume from a given state until textual Grain.
 */
let resume_until_grain_text:
  (~opts: opts, state) => result(string, next_error);
