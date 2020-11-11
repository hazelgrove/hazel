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

let ana_elab:
  (Contexts.t'('a), Delta.t, UHPat.t, HTyp.t) => ElaborationResult.t('a);

let renumber_result_only:
  (InstancePath.t, HoleInstanceInfo.t, DHPat.t) =>
  (DHPat.t, HoleInstanceInfo.t);
