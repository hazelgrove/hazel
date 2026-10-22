// INVARIANT: this evaluate function should never return an expression with closures.

/* MEASURING ONLY (^ticket(540)): evaluate calls and steps by kind */
let step_counts: Hashtbl.t(string, int);

type yielding_evaluation;

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

let evaluate:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

/* evaluate without finishing: closures keep their environments, for a host
   that calls the value again (^ticket(547)). May contain closures. */
let evaluate_unfinished: (~env: Environment.t(Exp.t), Exp.t) => Exp.t;

/* What evaluate does to a final value: the environment written in, ids
   replaced. */
let finish: (~env: Environment.t(Exp.t), Exp.t) => Exp.t;

let evaluate_and_limit:
  (
    ~step_limit: int,
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    ~outbox: ref(IncrEval.outbox(EvaluatorState.t))=?,
    Exp.t
  ) =>
  limited_result;

let start_yielding_evaluation:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    ~reuse_map: IncrEval.reuse_map=?,
    ~reuse_stream: IncrEval.t(EvaluatorState.t)=?,
    Exp.t
  ) =>
  yielding_evaluation;

let run_yielding_slice:
  (~step_budget: int, yielding_evaluation) => yielding_result;

let yielding_step_count: yielding_evaluation => int;

let drain_streaming_outbox:
  yielding_evaluation => IncrEval.outbox(EvaluatorState.t);
