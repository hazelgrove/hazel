type elab_result_lines =
  | LinesElaborate(DHExp.t => DHExp.t, Contexts.t, MetaVarGen.t, Delta.t)
  | LinesDoNotElaborate;

module ElaborationResult: {
  type t =
    | Elaborates(DHExp.t, HTyp.t, Contexts.t, MetaVarGen.t, Delta.t)
    | DoesNotElaborate;
};

let syn_elab:
  (Contexts.t, MetaVarGen.t, Delta.t, UHExp.t) => ElaborationResult.t;

let ana_elab:
  (Contexts.t, MetaVarGen.t, Delta.t, UHExp.t, HTyp.t) => ElaborationResult.t;
