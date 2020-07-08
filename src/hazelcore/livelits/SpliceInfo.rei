[@deriving sexp]
type splice_map('exp) = IntMap.t((HTyp.t, 'exp));

[@deriving sexp]
type t('exp) = {
  next: SpliceName.t,
  splice_map: splice_map('exp),
  splice_order: list(SpliceName.t),
};

let empty: t('exp);

let splice_map: t('a) => splice_map('a);

let update_splice_map: (t('a), splice_map('b)) => t('b);

let splice_var_prefix: string;
let var_of_splice_name: int => string;
