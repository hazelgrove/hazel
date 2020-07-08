[@deriving sexp]
type zsplice_map('exp, 'zexp) = ZIntMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));

[@deriving sexp]
type t('exp, 'zexp) = {
  next: SpliceName.t,
  zsplice_map: zsplice_map('exp, 'zexp),
  splice_order: list(SpliceName.t),
};

let erase:
  (t('exp, 'zexp), ((HTyp.t, 'zexp)) => (HTyp.t, 'exp)) =>
  SpliceInfo.t('exp);

let select_opt:
  (SpliceInfo.t('exp), int, ((HTyp.t, 'exp)) => option((HTyp.t, 'zexp))) =>
  option(t('exp, 'zexp));

let prj_ze: t('e, 'ze) => 'ze;
let prj_z: t('e, 'ze) => (HTyp.t, 'ze);
let update_z: (t('e, 'ze), (HTyp.t, 'ze)) => t('e, 'ze);
