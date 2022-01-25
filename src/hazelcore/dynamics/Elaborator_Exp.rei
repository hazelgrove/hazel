type elab_result_lines =
  | LinesElaborate(DHExp.t => DHExp.t, Contexts.t, Delta.t, MetaVarGen.t)
  | LinesDoNotElaborate;

module ElaborationResult: {
  type t =
    | Elaborates(DHExp.t, HTyp.t, Delta.t, MetaVarGen.t)
    | DoesNotElaborate;
};

let syn_elab:
  (Contexts.t, Delta.t, UHExp.t, MetaVarGen.t) => ElaborationResult.t;

let ana_elab:
  (Contexts.t, Delta.t, UHExp.t, HTyp.t, MetaVarGen.t) => ElaborationResult.t;
