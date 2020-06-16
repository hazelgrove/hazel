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

let syn_elab_opseq: (Contexts.t, Delta.t, UHPat.t) => ElaborationResult.t;

let syn_elab_skel:
  (
    Contexts.t,
    Delta.t,
    OpSeq.skel(UHPat.operator),
    OpSeq.seq(UHPat.operand, UHPat.operator)
  ) =>
  Let_syntax.t;

let syn_elab_operand:
  (Contexts.t, Delta.t, UHPat.operand) => ElaborationResult.t;

let ana_elab: (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t;

let ana_elab_opseq:
  (Contexts.t, Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t;

let ana_elab_skel:
  (Contexts.t, Delta.t, UHPat.skel, UHPat.seq, HTyp.t) => ElaborationResult.t;

let ana_elab_operand:
  (Contexts.t, Delta.t, UHPat.operand, HTyp.t) => ElaborationResult.t;

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
