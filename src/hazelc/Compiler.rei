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
  | Transformed(IHExp.t)
  | Grainized(string)
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
let stop_after_grainized: state => resume_action;
let stop_after_wasmized: state => resume_action;

let compile_grain:
  (~opts: opts=?, string, source) => result(string, next_error);
let compile: (~opts: opts=?, string, source) => result(string, next_error);
