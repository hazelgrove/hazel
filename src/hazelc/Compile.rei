exception BadState;

[@deriving sexp]
type grain_opts = Grain.opts;

[@deriving sexp]
type opts = {grain: grain_opts};

let parse_next: (~opts: opts=?, Source.t) => result(UHExp.t, string);
let elaborate_next:
  (~opts: opts=?, UHExp.t) => result((Contexts.t, DHExp.t), unit);
let transform_next: (~opts: opts=?, Contexts.t, DHExp.t) => Hir.expr;
let linearize_next: (~opts: opts=?, Hir.expr) => Anf.prog;
let grainize_next: (~opts: opts=?, Anf.prog) => GrainIR.prog;
let print_next: (~opts: opts=?, GrainIR.prog) => string;
let wasmize_next:
  (~opts: opts=?, string, string, string) => result(unit, unit);

[@deriving sexp]
type state =
  | Source(Source.t)
  | Parsed(UHExp.t)
  | Elaborated(Contexts.t, DHExp.t)
  | Transformed(Hir.expr)
  | Linearized(Anf.prog)
  | Grainized(GrainIR.prog)
  | Printed(string);

[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError;

[@deriving sexp]
type next_result = result(state, next_error);

[@deriving sexp]
type resume_action =
  | Continue(state)
  | Stop;

let next: (~opts: opts=?, state) => next_result;

let stop_after_parsed: state => resume_action;
let stop_after_elaborated: state => resume_action;
let stop_after_transformed: state => resume_action;
let stop_after_linearized: state => resume_action;
let stop_after_grainized: state => resume_action;
let stop_after_printed: state => resume_action;

let resume:
  (~opts: opts=?, ~hook: state => resume_action=?, state) => next_result;

let resume_until_dhexp: (~opts: opts=?, state) => result(DHExp.t, next_error);
let resume_until_hir: (~opts: opts=?, state) => result(Hir.expr, next_error);
let resume_until_anf: (~opts: opts=?, state) => result(Anf.prog, next_error);
let resume_until_grain:
  (~opts: opts=?, state) => result(GrainIR.prog, next_error);
let resume_until_grain_text:
  (~opts: opts=?, state) => result(string, next_error);
