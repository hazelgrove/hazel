include Monads.MONAD;

// type esi = SpliceInfo.t(UHExp.t);
// type out('a) = ('a, esi, MetaVarGen.t);
let new_splice: t(SpliceName.t);
let drop_splice: SpliceName.t => t((HTyp.t, UHExp.t));
let exec:
  (t('a), SpliceInfo.t(UHExp.t), MetaVarGen.t) =>
  ('a, SpliceInfo.t(UHExp.t), MetaVarGen.t);

/* Have to add these functions to stub ppx_deriving.show for types that use this type */
let pp: ('a, 'b, 'c) => unit;
let show: t('a) => string;
