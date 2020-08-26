/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst_var_rules: (DHExp.t, Var.t, list(DHExp.rule)) => list(DHExp.rule);

let subst_var_env: (DHExp.t, Var.t, VarMap.t_(DHExp.t)) => Environment.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | Indet;

let matches: (DHPat.t, DHExp.t) => match_result;

let matches_cast_Inj:
  (InjSide.t, DHPat.t, DHExp.t, list((HTyp.t, HTyp.t, HTyp.t, HTyp.t))) =>
  match_result;

let matches_cast_Pair:
  (
    DHPat.t,
    DHPat.t,
    DHExp.t,
    list((HTyp.t, HTyp.t)),
    list((HTyp.t, HTyp.t))
  ) =>
  match_result;

let matches_cast_Cons:
  (DHPat.t, DHExp.t, list((HTyp.t, HTyp.t))) => match_result;

type elab_result_lines =
  | LinesExpand(DHExp.t => DHExp.t, Contexts.t, Delta.t)
  | LinesDoNotExpand;

module ElaborationResult: {
  type t =
    | Elaborates(DHExp.t, HTyp.t, Delta.t)
    | DoesNotElaborate;

  let to_option: t => option((DHExp.t, HTyp.t, Delta.t));

  let from_option: option((DHExp.t, HTyp.t, Delta.t)) => t;

  let bind: (t, ~f: ((DHExp.t, HTyp.t, Delta.t)) => t) => t;
};

module Let_syntax = ElaborationResult;

let id_env: VarCtx.t => Environment.t;

let syn_elab: (Contexts.t, Delta.t, UHExp.t) => ElaborationResult.t;

let syn_elab_block: (Contexts.t, Delta.t, UHExp.t) => ElaborationResult.t;

let syn_elab_lines:
  (Contexts.t, Delta.t, list(UHExp.line)) => elab_result_lines;

let syn_elab_line: (Contexts.t, Delta.t, UHExp.line) => elab_result_lines;

let syn_elab_opseq: (Contexts.t, Delta.t, UHExp.opseq) => ElaborationResult.t;

let syn_elab_skel:
  (
    Contexts.t,
    Delta.t,
    OpSeq.skel(UHExp.operator),
    OpSeq.seq(UHExp.operand, UHExp.operator)
  ) =>
  Let_syntax.t;

let syn_elab_operand:
  (Contexts.t, Delta.t, UHExp.operand) => ElaborationResult.t;

let syn_elab_rules:
  (Contexts.t, Delta.t, UHExp.rules, HTyp.t) =>
  option((list(DHExp.rule), HTyp.t, Delta.t));

let syn_elab_rule:
  (Contexts.t, Delta.t, UHExp.rule, HTyp.t, HTyp.t) =>
  option((DHExp.rule, Delta.t));

let ana_elab: (Contexts.t, Delta.t, UHExp.t, HTyp.t) => ElaborationResult.t;

let ana_elab_block:
  (Contexts.t, Delta.t, UHExp.t, HTyp.t) => ElaborationResult.t;

let ana_elab_opseq:
  (Contexts.t, Delta.t, UHExp.opseq, HTyp.t) => ElaborationResult.t;

let ana_elab_skel:
  (Contexts.t, Delta.t, UHExp.skel, UHExp.seq, HTyp.t) => ElaborationResult.t;

let ana_elab_operand:
  (Contexts.t, Delta.t, UHExp.operand, HTyp.t) => ElaborationResult.t;

let ana_elab_rules:
  (Contexts.t, Delta.t, UHExp.rules, HTyp.t, HTyp.t) =>
  option((list(DHExp.rule), Delta.t));

let ana_elab_rule:
  (Contexts.t, Delta.t, UHExp.rule, HTyp.t, HTyp.t) =>
  option((DHExp.rule, Delta.t));

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHExp.t) =>
  (DHExp.t, HoleInstanceInfo.t);

let renumber_result_only_rules:
  (InstancePath.t, HoleInstanceInfo.t, list(DHExp.rule)) =>
  (list(DHExp.rule), HoleInstanceInfo.t);

let renumber_sigmas_only:
  (InstancePath.t, HoleInstanceInfo.t, DHExp.t) =>
  (DHExp.t, HoleInstanceInfo.t);

let renumber_sigmas_only_rules:
  (InstancePath.t, HoleInstanceInfo.t, list(DHExp.rule)) =>
  (list(DHExp.rule), HoleInstanceInfo.t);

let renumber_sigma:
  (
    InstancePath.t,
    MetaVar.t,
    MetaVarInst.t,
    HoleInstanceInfo.t,
    VarMap.t_(DHExp.t)
  ) =>
  (Environment.t, HoleInstanceInfo.t);

let renumber:
  (InstancePath.t, HoleInstanceInfo.t, DHExp.t) =>
  (DHExp.t, HoleInstanceInfo.t);
