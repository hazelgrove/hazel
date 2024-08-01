include Ptmap;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

module Sexp = Sexplib.Sexp;

[@deriving (sexp, yojson)]
type binding('v) = (int, 'v);

let singleton = (k, v) => Ptmap.add(k, v, Ptmap.empty);

let disj_union = (m: t('a), m': t('a)): t('a) =>
  union(
    (_, _, _) =>
      raise(
        Invalid_argument(
          "IntMap.disj_union expects input maps to have disjoint key sets",
        ),
      ),
    m,
    m',
  );

let pp = (pp_v, f, map) =>
  iter((k, v) => Format.fprintf(f, "%d -> %a@\n", k, pp_v, v), map);

let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
  map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
  sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;

let yojson_of_t =
    (yojson_of_v: 'v => Yojson.Safe.t, map: t('v)): Yojson.Safe.t =>
  map |> bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));
let t_of_yojson =
    (v_of_yojson: Yojson.Safe.t => 'v, yojson: Yojson.Safe.t): t('v) =>
  yojson
  |> list_of_yojson(binding_of_yojson(v_of_yojson))
  |> List.to_seq
  |> of_seq;
