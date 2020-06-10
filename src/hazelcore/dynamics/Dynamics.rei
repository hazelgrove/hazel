/* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */

module Pat: {
  module ExpandResult: {
    type t =
      | Expands(DHPat.t, HTyp.t, Contexts.t, Delta.t)
      | DoesNotExpand;

    let to_option: t => option((DHPat.t, HTyp.t, Contexts.t, Delta.t));

    let from_option: option((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t;

    let bind: (t, ~f: ((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t) => t;
  };

  module Let_syntax = ExpandResult;

  let syn_expand: (Contexts.t, Delta.t, UHPat.t) => ExpandResult.t;

  let syn_expand_opseq: (Contexts.t, Delta.t, UHPat.t) => ExpandResult.t;

  let syn_expand_skel:
    (
      Contexts.t,
      Delta.t,
      OpSeq.skel(UHPat.operator),
      OpSeq.seq(UHPat.operand, UHPat.operator)
    ) =>
    Let_syntax.t;

  let syn_expand_operand:
    (Contexts.t, Delta.t, UHPat.operand) => ExpandResult.t;

  let ana_expand: (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ExpandResult.t;

  let ana_expand_opseq:
    (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ExpandResult.t;

  let ana_expand_skel:
    (Contexts.t, Delta.t, UHPat.skel, UHPat.seq, HTyp.t) => ExpandResult.t;

  let ana_expand_operand:
    (Contexts.t, Delta.t, UHPat.operand, HTyp.t) => ExpandResult.t;

  let renumber_result_only:
    (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
    (DHPat.t, HoleInstanceInfo.t);
};

module Exp: {
  /* closed substitution [d1/x]d2*/
  let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

  let subst_var_rules:
    (DHExp.t, Var.t, list(DHExp.rule)) => list(DHExp.rule);

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
    (DHPat.t, DHPat.t, DHExp.t, list((HTyp.t, HTyp.t))) => match_result;

  type expand_result_lines =
    | LinesExpand(DHExp.t => DHExp.t, Contexts.t, Delta.t)
    | LinesDoNotExpand;

  module ExpandResult: {
    type t =
      | Expands(DHExp.t, HTyp.t, Delta.t)
      | DoesNotExpand;

    let to_option: t => option((DHExp.t, HTyp.t, Delta.t));

    let from_option: option((DHExp.t, HTyp.t, Delta.t)) => t;

    let bind: (t, ~f: ((DHExp.t, HTyp.t, Delta.t)) => t) => t;
  };

  module Let_syntax = ExpandResult;

  let id_env: VarCtx.t => Environment.t;

  let syn_expand: (Contexts.t, Delta.t, UHExp.t) => ExpandResult.t;

  let syn_expand_block: (Contexts.t, Delta.t, UHExp.t) => ExpandResult.t;

  let syn_expand_lines:
    (Contexts.t, Delta.t, list(UHExp.line)) => expand_result_lines;

  let syn_expand_line:
    (Contexts.t, Delta.t, UHExp.line) => expand_result_lines;

  let syn_expand_opseq: (Contexts.t, Delta.t, UHExp.opseq) => ExpandResult.t;

  let syn_expand_skel:
    (
      Contexts.t,
      Delta.t,
      OpSeq.skel(UHExp.operator),
      OpSeq.seq(UHExp.operand, UHExp.operator)
    ) =>
    Let_syntax.t;

  let syn_expand_operand:
    (Contexts.t, Delta.t, UHExp.operand) => ExpandResult.t;

  let syn_expand_rules:
    (Contexts.t, Delta.t, UHExp.rules, HTyp.t) =>
    option((list(DHExp.rule), HTyp.t, Delta.t));

  let syn_expand_rule:
    (Contexts.t, Delta.t, UHExp.rule, HTyp.t, HTyp.t) =>
    option((DHExp.rule, Delta.t));

  let ana_expand: (Contexts.t, Delta.t, UHExp.t, HTyp.t) => ExpandResult.t;

  let ana_expand_block:
    (Contexts.t, Delta.t, UHExp.t, HTyp.t) => ExpandResult.t;

  let ana_expand_opseq:
    (Contexts.t, Delta.t, UHExp.opseq, HTyp.t) => ExpandResult.t;

  let ana_expand_skel:
    (Contexts.t, Delta.t, UHExp.skel, UHExp.seq, HTyp.t) => ExpandResult.t;

  let ana_expand_operand:
    (Contexts.t, Delta.t, UHExp.operand, HTyp.t) => ExpandResult.t;

  let ana_expand_rules:
    (Contexts.t, Delta.t, UHExp.rules, HTyp.t, HTyp.t) =>
    option((list(DHExp.rule), Delta.t));

  let ana_expand_rule:
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
};

module Evaluator: {
  [@deriving sexp]
  type result =
    | InvalidInput(int)
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t);

  /*
     0 = out of fuel
     1 = free or invalid variable
     2 = ap invalid boxed function val
     3 = boxed value not a int literal 2
     4 = boxed value not a int literal 1
     5 = bad pattern match
     6 = Cast BV Hole Ground
     7 = boxed value not a float literal 1
     8 = boxed value not a float literal 2
   */

  [@deriving sexp]
  type ground_cases =
    | Hole
    | Ground
    | NotGroundOrHole(HTyp.t);

  let grounded_Arrow: ground_cases;

  let grounded_Sum: ground_cases;

  let grounded_Prod: int => ground_cases;

  let grounded_List: ground_cases;

  let ground_cases_of: HTyp.t => ground_cases;

  let eval_bin_bool_op: (DHExp.BinBoolOp.t, bool, bool) => DHExp.t;

  let eval_bin_int_op: (DHExp.BinIntOp.t, int, int) => DHExp.t;

  let eval_bin_float_op: (DHExp.BinFloatOp.t, float, float) => DHExp.t;

  let evaluate: DHExp.t => result;

  let evaluate_case:
    (
      option((MetaVar.t, MetaVarInst.t, VarMap.t_(DHExp.t))),
      DHExp.t,
      list(DHExp.rule),
      int
    ) =>
    result;
};
