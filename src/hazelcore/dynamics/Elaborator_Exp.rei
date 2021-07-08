type elab_result_lines =
  | LinesExpand(DHExp.t => DHExp.t, Contexts.t, Delta.t)
  | LinesDoNotExpand;

module ElaborationResult: {
  type t =
    | Elaborates(DHExp.t, HTyp.t, Delta.t)
    | DoesNotElaborate;
};

let syn_elab: (Contexts.t, Delta.t, UHExp.t) => ElaborationResult.t;

let ana_elab: (Contexts.t, Delta.t, UHExp.t, HTyp.t) => ElaborationResult.t;
