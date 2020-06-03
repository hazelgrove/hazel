include Map.Make(Int);
open Sexplib.Std;
module Sexp = Sexplib.Sexp;

[@deriving sexp]
type binding('v) = (int, 'v);

let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
  map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
  sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;

let rec insert_or_map = (delta, u, a0, f) =>
  switch (delta) {
  | [] =>
    let a0 = a0();
    (a0, [(u, a0), ...delta]);
  | [(u', a), ...delta'] =>
    if (u === u') {
      let a' = f(a);
      (a', [(u', a'), ...delta']);
    } else {
      let (a', delta'') = insert_or_map(delta', u, a0, f);
      (a', [(u', a), ...delta'']);
    }
  };

let rec map = f =>
  fun
  | [] => []
  | [(u, a), ...delta'] => [(u, f(a)), ...map(f, delta')];

let rec update_with = (f, u, delta, u_nil) =>
  switch (delta) {
  | [] => (u_nil, delta)
  | [(u', a), ...delta'] =>
    if (u == u') {
      let a' = f(a);
      (a', [(u', a'), ...delta']);
    } else {
      let (a', delta'') = update_with(f, u, delta', u_nil);
      (a', [(u', a), ...delta'']);
    }
  };

let length = List.length;

let to_list = delta => delta;

