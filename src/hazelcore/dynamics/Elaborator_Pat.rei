module ElaborationResult: {
  type t =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotElaborate;

  let to_option: t => option((DHPat.t, HTyp.t, Contexts.t, Delta.t));

  let from_option: option((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t;

  let bind: (t, ~f: ((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t) => t;
};

module Let_syntax = ElaborationResult;

let syn_elab: (Contexts.t, Delta.t, UHPat.t) => ElaborationResult.t;

let ana_elab: (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t;

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
