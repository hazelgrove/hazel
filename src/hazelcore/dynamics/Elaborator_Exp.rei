type elab_result_lines =
  | LinesElaborate(DHExp.t => DHExp.t, Contexts.t, Delta.t)
  | LinesDoNotElaborate;

module ElaborationResult: {
  type t =
    | Elaborates(DHExp.t, HTyp.t, Delta.t)
    | DoesNotElaborate;
};

let syn_elab: (Contexts.t, Delta.t, UHExp.t) => ElaborationResult.t;

let ana_elab: (Contexts.t, Delta.t, UHExp.t, HTyp.t) => ElaborationResult.t;

let renumber:
  (InstancePath.t, HoleInstanceInfo.t, DHExp.t) =>
  (DHExp.t, HoleInstanceInfo.t);

let elab: (Contexts.t, Delta.t, UHExp.t) => ElaborationResult.t;
