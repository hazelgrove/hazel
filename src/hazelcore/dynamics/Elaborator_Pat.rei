module ElaborationResult: {
  type t =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotElaborate;
};

module Let_syntax = ElaborationResult;

let syn_elab: (Contexts.t, Delta.t, UHPat.t) => ElaborationResult.t;

let ana_elab: (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t;
