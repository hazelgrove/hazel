module ElaborationResult: {
  type t('a) =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t'('a), Delta.t)
    | DoesNotElaborate;

  let to_option:
    t('a) => option((DHPat.t, HTyp.t, Contexts.t'('a), Delta.t));

  let from_option:
    option((DHPat.t, HTyp.t, Contexts.t'('a), Delta.t)) => t('a);

  let bind:
    (t('a), ~f: ((DHPat.t, HTyp.t, Contexts.t'('a), Delta.t)) => t('a)) =>
    t('a);
};

module Let_syntax = ElaborationResult;

let syn_elab: (Contexts.t'('a), Delta.t, UHPat.t) => ElaborationResult.t('a);

let syn_elab_opseq:
  (Contexts.t'('a), Delta.t, UHPat.t) => ElaborationResult.t('a);

let syn_elab_skel:
  (
    Contexts.t'('a),
    Delta.t,
    OpSeq.skel(UHPat.operator),
    OpSeq.seq(UHPat.operand, UHPat.operator)
  ) =>
  ElaborationResult.t('a);

let syn_elab_operand:
  (Contexts.t'('a), Delta.t, UHPat.operand) => ElaborationResult.t('a);

let ana_elab:
  (Contexts.t'('a), Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t('a);

let ana_elab_opseq:
  (Contexts.t'('a), Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t('a);

let ana_elab_skel:
  (Contexts.t'('a), Delta.t, UHPat.skel, UHPat.seq, HTyp.t) =>
  ElaborationResult.t('a);

let ana_elab_operand:
  (Contexts.t'('a), Delta.t, UHPat.operand, HTyp.t) =>
  ElaborationResult.t('a);

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
