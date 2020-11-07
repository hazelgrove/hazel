/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | Indet;

let matches: (DHPat.t, DHExp.t) => match_result;

type elab_result_lines =
  | LinesExpand(DHExp.t => DHExp.t, Contexts.t'((DHExp.t, HTyp.t)), Delta.t)
  | LinesDoNotExpand;

module ElaborationResult: {
  type t =
    | Elaborates(DHExp.t, HTyp.t, Delta.t)
    | DoesNotElaborate;
};

let id_env: VarCtx.t => Environment.t;

let syn_elab:
  (
    ~livelit_holes: bool=?,
    Contexts.t'((DHExp.t, HTyp.t)),
    Delta.t,
    UHExp.t
  ) =>
  ElaborationResult.t;

let renumber:
  (InstancePath.t, HoleInstanceInfo.t, LivelitInstanceInfo.t, DHExp.t) =>
  (DHExp.t, HoleInstanceInfo.t, LivelitInstanceInfo.t);

type names_to_vars_map = IntMap.t(Var.t);
