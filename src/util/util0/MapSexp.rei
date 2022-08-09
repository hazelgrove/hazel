module type OrderedSexpType = {
  include Map.OrderedType;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module type S = {
  include Map.S;

  let sexp_of_t: ('v => Sexplib.Sexp.t, t('v)) => Sexplib.Sexp.t;
  let t_of_sexp: (Sexplib.Sexp.t => 'v, Sexplib.Sexp.t) => t('v);
};

module Make: (O: OrderedSexpType) => S with type key = O.t;
