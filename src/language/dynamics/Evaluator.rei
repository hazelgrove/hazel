// INVARIANT: this evaluate function should never return an expression with closures.

type yielding_evaluation;

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

/* Incremental evaluator. Reuses entries from `prev` for non-deferred
 * sub-expressions whose elaboration (per `eval_info`) and co-ctx
 * dependencies are unchanged. The defaults for `prev` and `eval_info`
 * turn reuse off.
 *
 * `eval_info` is a serializable projection of the statics map (see
 * `EvalInfo.of_info_map`); the full map holds closures and can't cross
 * postMessage. Once evaluation is done, the proofs of the theorems it
 * reached are checked, in the order it reached them, with the statics
 * `eval_info` carries for a program with theorems, or with `statics`,
 * the full map, when it is given. Each check is cached in the state's
 * `incr_eval` and reused from `prev`. */
let evaluate:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~statics: Statics.Map.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  (Exp.t, EvaluatorState.t);

/* evaluate, also returning the work done: the steps the trampoline
   actually took, a reused result counting as one. Unlike the state's
   step_count, it drops when results are reused. For Test_PerfBudgets. */
let evaluate_counting_work:
  (
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~env: Environment.t(Exp.t),
    Exp.t
  ) =>
  ((Exp.t, EvaluatorState.t), int);

let evaluate_and_limit:
  (
    ~step_limit: int,
    ~prev: EvaluatorState.incr_eval=?,
    ~eval_info: EvalInfo.t=?,
    ~statics: Statics.Map.t=?,
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

/* Checking theorems' proofs. */
module Proofs: {
  /* The steps a `step` proof step passes through silently from an
   * expression (auto-hidden steps and `VarLookup`s), each as (input,
   * justification, output), and the expression they reach. None if there
   * are more than `max_steps`. */
  let advance_hidden:
    (~max_steps: int=?, ~env: Environment.t(Exp.t), Exp.t) =>
    option((list((Exp.t, string, Exp.t)), Exp.t));

  /* One `step` proof step: the silent steps, one visible step, and the
   * silent steps after it. The checker takes its evaluation steps with
   * this. */
  let step: ProofCheck.step_fn;

  /* The proof map of `proof` checked against `goal` in `ctx`. */
  let check_proof:
    (~statics: ProofStatics.t, ~ctx: SemanticCtx.t, ~goal: Exp.t, Proof.t) =>
    ProofMap.t;

  /* The theorems an evaluation recorded (`Dynamics.theorems`), checked as
   * the evaluation checks them, against `elaborated`, a program whose
   * proofs may have changed since. Nothing is reused. */
  let recheck_theorems:
    (
      ~statics: ProofStatics.t,
      ~elaborated: Exp.t,
      list((Id.t, string, Environment.t(Exp.t), Exp.t))
    ) =>
    ProofMap.t;
};
