module ElaborationResult: {
  type t =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t, MetaVarGen.t, Delta.t)
    | DoesNotElaborate;
};

module Let_syntax = ElaborationResult;

let syn_elab:
  (Contexts.t, MetaVarGen.t, Delta.t, UHPat.t) => ElaborationResult.t;

let ana_elab:
  (Contexts.t, MetaVarGen.t, Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t;

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
