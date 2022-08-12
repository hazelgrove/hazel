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

module Make = (O: OrderedSexpType) : (S with type elt = O.t) => {
  open Sexplib.Std;

  include Set.Make(O);

  let sexp_of_t = set => set |> elements |> sexp_of_list(O.sexp_of_t);
  let t_of_sexp = sexp => sexp |> list_of_sexp(O.t_of_sexp) |> of_list;
};
