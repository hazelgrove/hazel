include (module type of Ident0);

module Map: {
  [@deriving sexp]
  type binding('v) = (t, 'v);

  include Map.S with type key = t;

  let sexp_of_t: ('v => Sexplib.Sexp.t, t('v)) => Sexplib.Sexp.t;
  let t_of_sexp: (Sexplib.Sexp.t => 'v, Sexplib.Sexp.t) => t('v);
};
