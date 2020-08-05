module ElaborationResult: {
  type t =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotElaborate;
};

let ana_elab: (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t;

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
