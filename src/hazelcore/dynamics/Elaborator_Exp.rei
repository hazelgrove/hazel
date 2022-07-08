type elab_result_lines =
  | LinesElaborate(DHExp.t => DHExp.t, Context.t, Delta.t)
  | LinesDoNotElaborate;

module ElaborationResult: {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, HTyp.t, Delta.t)
    | DoesNotElaborate;
};

let syn_elab: (Context.t, Delta.t, UHExp.t) => ElaborationResult.t;

let ana_elab: (Context.t, Delta.t, UHExp.t, HTyp.t) => ElaborationResult.t;

let elab: (Context.t, Delta.t, UHExp.t) => ElaborationResult.t;
