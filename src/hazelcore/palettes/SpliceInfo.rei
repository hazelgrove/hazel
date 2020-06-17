[@deriving sexp]
type splice_name = int;

[@deriving sexp]
type splice_map('exp) = NatMap.t((HTyp.t, 'exp));

[@deriving sexp]
type t('exp) = {
  next: splice_name,
  splice_map: splice_map('exp),
  splice_order: list(splice_name),
};

let t_of_sexp:
  (Ppx_sexp_conv_lib.Sexp.t => 'exp, Ppx_sexp_conv_lib.Sexp.t) => t('exp);
let sexp_of_t:
  ('exp => Ppx_sexp_conv_lib.Sexp.t, t('exp)) => Ppx_sexp_conv_lib.Sexp.t;

let empty: t('exp);

let splice_map: t('a) => splice_map('a);

let update_splice_map: (t('a), splice_map('b)) => t('b);

let var_of_splice_name: int => string;
