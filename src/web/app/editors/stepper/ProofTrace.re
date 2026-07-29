open Language;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type prover_step_origin =
  | ManualRewrite
  | Normalization
  | AutoEvaluation;

[@deriving (show({with_path: false}), sexp, yojson)]
type prover_step = {
  origin: prover_step_origin,
  rule_id: string,
  before_full_exp: Exp.t,
  after_full_exp: Exp.t,
  before_exp: Exp.t,
  after_exp: Exp.t,
  occurrence: int,
  detail: option(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type trace_summary = {
  justification: string,
  group_name: option(string),
  from_normal_exp: Exp.t,
  to_normal_exp: Exp.t,
  from_rule_ids: list(string),
  to_rule_ids: list(string),
  rule_ids: list(string),
  prover_steps: list(prover_step),
  exportable: bool,
};

let trace_summary_label = (summary: trace_summary): string =>
  summary.justification;

let prover_step_at =
    (
      ~origin,
      ~rule_id,
      ~before_full_exp,
      ~after_full_exp,
      ~before_exp,
      ~after_exp,
      ~occurrence,
      ~detail,
    ) => {
  origin,
  rule_id,
  before_full_exp,
  after_full_exp,
  before_exp,
  after_exp,
  occurrence,
  detail: Some(detail),
};

let prover_step =
    (
      ~origin,
      ~rule_id,
      ~before_full_exp,
      ~after_full_exp,
      ~before_exp,
      ~after_exp,
      ~detail,
    ) =>
  prover_step_at(
    ~origin,
    ~rule_id,
    ~before_full_exp,
    ~after_full_exp,
    ~before_exp,
    ~after_exp,
    ~occurrence=1,
    ~detail,
  );
