open Sexplib.Std;
module Sexp = Sexplib.Sexp;

module Inner = {
  include Map.Make(Var);

  /* See IntMap */
  [@deriving sexp]
  type binding('v) = (Var.t, 'v);

  let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
    map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
  let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
    sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;
};

[@deriving sexp]
type t_('a) = Inner.t('a);

let empty = Inner.empty;

let is_empty = Inner.is_empty;

let singleton = ((x, a)) => Inner.singleton(x, a);

let extend = (ctx, (x, a)) => Inner.add(x, a, ctx);

let union = (ctx1, ctx2) =>
  Inner.union((_x, a, _a') => Some(a), ctx1, ctx2);

let lookup = (ctx, x) => Inner.find_opt(x, ctx);

let contains = (ctx, x) => Inner.mem(x, ctx);

let map = f => Inner.mapi((x, a) => f((x, a)));

let filter = f => Inner.filter((x, a) => f((x, a)));

let fold = (f, init, ctx) =>
  Inner.fold((x, a, acc) => f((x, a), acc), ctx, init);

let length = Inner.cardinal;

let to_list = ctx => ctx |> Inner.to_seq |> List.of_seq;
