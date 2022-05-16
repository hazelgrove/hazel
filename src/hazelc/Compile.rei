[@deriving sexp]
type grain_opts = Grain.opts;

[@deriving sexp]
type opts = {
  exp_only: bool,
  grain: grain_opts,
};

[@deriving sexp]
type source =
  | SourceString(string)
  | SourceLexbuf(Lexing.lexbuf)
  | SourceChannel(in_channel);

[@deriving sexp]
type state =
  | Source(source)
  | Parsed(UHExp.t)
  | Elaborated(DHExp.t)
  | Transformed(Hir.expr)
  | Linearized(Anf.prog)
  | Grainized(GrainIR.prog)
  | Printed(string)
  | Wasmized(string);

[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError
  | GrainError;

[@deriving sexp]
type next_result = result(state, next_error);

[@deriving sexp]
type resume_action =
  | Continue(state)
  | Stop;

let next: (~opts: opts=?, string, state) => next_result;

let resume:
  (~opts: opts=?, ~hook: state => resume_action=?, string, state) =>
  next_result;

let stop_after_parsed: state => resume_action;
let stop_after_elaborated: state => resume_action;
let stop_after_transformed: state => resume_action;
let stop_after_linearized: state => resume_action;
let stop_after_grainized: state => resume_action;
let stop_after_printed: state => resume_action;
let stop_after_wasmized: state => resume_action;

let compile_dhexp:
  (~opts: opts=?, string, source) => result(DHExp.t, next_error);
let compile_ihexp:
  (~opts: opts=?, string, source) => result(Hir.expr, next_error);
let compile_anf:
  (~opts: opts=?, string, source) => result(Anf.prog, next_error);
let compile_grain:
  (~opts: opts=?, string, source) => result(string, next_error);
let compile_wasm:
  (~opts: opts=?, string, source) => result(string, next_error);
