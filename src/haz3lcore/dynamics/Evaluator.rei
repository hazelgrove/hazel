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

let evaluate_closure:
  (ClosureEnvironment.t, DHExp.t) => EvaluatorMonad.t(EvaluatorResult.t);

let ground_cases_of: Typ.t => ground_cases;

let matches: (DHPat.t, DHExp.t) => match_result;

let eval_bin_bool_op: (TermBase.UExp.op_bin_bool, bool, bool) => DHExp.t;

let eval_bin_bool_op_short_circuit:
  (TermBase.UExp.op_bin_bool, bool) => option(DHExp.t);

let eval_bin_int_op: (TermBase.UExp.op_bin_int, int, int) => DHExp.t;

let eval_bin_float_op: (TermBase.UExp.op_bin_float, float, float) => DHExp.t;

let eval_bin_string_op:
  (TermBase.UExp.op_bin_string, string, string) => DHExp.t;

let evaluate_extend_env:
  (Environment.t, ClosureEnvironment.t) =>
  EvaluatorMonad.t(ClosureEnvironment.t);

let evaluate_test:
  (ClosureEnvironment.t, KeywordID.t, DHExp.t) =>
  EvaluatorMonad.t(EvaluatorResult.t);

let evaluate_ap_builtin:
  (ClosureEnvironment.t, string, list(DHExp.t)) =>
  EvaluatorMonad.t(EvaluatorResult.t);
