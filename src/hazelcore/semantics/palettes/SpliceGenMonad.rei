include Monads.MONAD;

let exec:
  (t('a), SpliceInfo.t(UHExp.block), MetaVarGen.t) =>
  ('a, SpliceInfo.t(UHExp.block), MetaVarGen.t) /* Have to add these functions to stub ppx_deriving.show for types that use this type */;

let pp: ('a, 'b, 'c) => unit;
let show: t('a) => string;
