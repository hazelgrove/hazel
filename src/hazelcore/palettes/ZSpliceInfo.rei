// [@deriving sexp]
type zsplice_map('exp, 'zexp) = ZIntMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));

// [@deriving sexp]
type t('exp, 'zexp) = {
  next: SpliceInfo.splice_name,
  zsplice_map: zsplice_map('exp, 'zexp),
  splice_order: list(SpliceInfo.splice_name),
};

let erase:
  (t('exp, 'zexp), ((HTyp.t, 'zexp)) => (HTyp.t, 'exp)) =>
  SpliceInfo.t('exp);

let select_opt:
  (SpliceInfo.t('exp), int, ((HTyp.t, 'exp)) => option((HTyp.t, 'zexp))) =>
  option(t('exp, 'zexp));
