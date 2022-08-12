module type OrderedSexpType = {
  include Set.OrderedType;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module type S = {
  include Set.S;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module Make: (O: OrderedSexpType) => S with type elt = O.t;
