module type Map = {
  include Map.S;

  let sexp_of_t: ('a => Sexplib.Sexp.t, t('a)) => Sexplib.Sexp.t;

  let t_of_sexp: (Sexplib.Sexp.t => 'a, Sexplib.Sexp.t) => t('a);
};

module type Set = {
  include Set.S;

  let sexp_of_t: t => Sexplib.Sexp.t;

  let t_of_sexp: Sexplib.Sexp.t => t;
};
