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

module Make = (O: OrderedSexpType) : (S with type key = O.t) => {
  open Sexplib.Std;

  include Map.Make(O);

  [@deriving sexp]
  type binding('v) = (O.t, 'v);

  let sexp_of_t = (sexp_of_v, map) =>
    map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
  let t_of_sexp = (v_of_sexp, sexp) =>
    sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;
};
