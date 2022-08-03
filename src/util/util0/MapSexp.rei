module type OrderedSexpType = {
  include Map.OrderedType;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module Make:
  (O: OrderedSexpType) =>
   {
    include (module type of Map.Make(O));

    let sexp_of_t: ('v => Sexplib.Sexp.t, t('v)) => Sexplib.Sexp.t;
    let t_of_sexp: (Sexplib.Sexp.t => 'v, Sexplib.Sexp.t) => t('v);
  };
