[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(Typ.t) /* the argument is the corresponding ground type */;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

/**
  [evaluate builtins env d] is [(es, r)], where [r] is the result of evaluating [d] and
  [es] is the accumulated state.
 */
let evaluate:
  (Environment.t, DHExp.t) => (EvaluatorState.t, EvaluatorResult.t);

let ground_cases_of: Typ.t => ground_cases;

let matches: (DHPat.t, DHExp.t) => match_result;

let eval_bin_bool_op: (DHExp.BinBoolOp.t, bool, bool) => DHExp.t;

let eval_bin_int_op: (DHExp.BinIntOp.t, int, int) => DHExp.t;

let eval_bin_float_op: (DHExp.BinFloatOp.t, float, float) => DHExp.t;

let eval_bin_string_op: (DHExp.BinStringOp.t, string, string) => DHExp.t;
