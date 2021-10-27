[@deriving sexp]
type t('a);

include Monads.MONAD with type t('a) := t('a);

let exec:
  (t('a), SpliceInfo.t(UHExp.t), IDGen.t) =>
  ('a, SpliceInfo.t(UHExp.t), IDGen.t);

/* Have to add these functions to stub ppx_deriving.show for types that use this type */
let pp: ('a, 'b, 'c) => unit;
let show: t('a) => string;
