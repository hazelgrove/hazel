include Monads.MONAD;

let t_of_sexp: ((Sexplib.Sexp.t as 's) => 'a, 's) => t('a);
let sexp_of_t: ('a => (Sexplib.Sexp.t as 's), t('a)) => 's;

let new_splice:
  (~init_uhexp_gen: MetaVarGen.t => (UHExp.t, MetaVarGen.t)=?, HTyp.t) =>
  t(SpliceName.t);
// the return value is the original (htyp, uhexp)
let map_splice:
  (
    SpliceName.t,
    ((HTyp.t, UHExp.t), MetaVarGen.t) => ((HTyp.t, UHExp.t), MetaVarGen.t)
  ) =>
  t((HTyp.t, UHExp.t));
// the return value is the dropped (htyp, uhexp)
let drop_splice: SpliceName.t => t((HTyp.t, UHExp.t));
let exec:
  (t('a), SpliceInfo.t(UHExp.t), MetaVarGen.t) =>
  ('a, SpliceInfo.t(UHExp.t), MetaVarGen.t);

/* Have to add these functions to stub ppx_deriving.show for types that use this type */
let pp: ('a, 'b, 'c) => unit;
let show: t('a) => string;
