module ElaborationResult: {
  type t =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotElaborate;
};

module Let_syntax = ElaborationResult;

let syn_elab:
  (Contexts.t, Delta.t, UHPat.t, ~moded: Statics_Pat.moded) =>
  ElaborationResult.t;

let ana_elab:
  (Contexts.t, Delta.t, UHPat.t, HTyp.t, ~moded: Statics_Pat.moded) =>
  ElaborationResult.t;

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
