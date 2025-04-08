open Util;
open Sexplib;

[@deriving (show({with_path: false}), sexp, yojson)]
type pair('a, 'b) = ('a, 'b);

type t('a) = OrderedMultiMap.t(string, 'a, Core.String.comparator_witness);

let pp = (pp_v, f, ctx) =>
  ctx
  |> OrderedMultiMap.to_assoc_list
  |> List.iter(((k, v)) => Format.fprintf(f, "%s -> %a@\n", k, pp_v, v));

let sexp_of_t = (sexp_of_v: 'a => Sexp.t, ctx: t('a)): Sexp.t =>
  ctx
  |> OrderedMultiMap.to_assoc_list
  |> sexp_of_list(sexp_of_pair(sexp_of_string, sexp_of_v));

let t_of_sexp = (v_of_sexp: Sexp.t => 'a, sexp: Sexp.t): t('a) =>
  sexp
  |> list_of_sexp(pair_of_sexp(string_of_sexp, v_of_sexp))
  |> OrderedMultiMap.of_assoc_list(_, (module Core.String));

let t_of_yojson =
    (v_of_yojson: Yojson.Safe.t => 'a, yojson: Yojson.Safe.t): t('a) =>
  yojson
  |> list_of_yojson(pair_of_yojson(string_of_yojson, v_of_yojson))
  |> OrderedMultiMap.of_assoc_list(_, (module Core.String));

let yojson_of_t =
    (yojson_of_v: 'a => Yojson.Safe.t, ctx: t('a)): Yojson.Safe.t =>
  ctx
  |> OrderedMultiMap.to_assoc_list
  |> yojson_of_list(yojson_of_pair(yojson_of_string, yojson_of_v));

let equal = (equal_v: ('a, 'a) => bool, ctx1: t('a), ctx2: t('a)): bool => {
  let ctx1' = OrderedMultiMap.to_assoc_list(ctx1);
  let ctx2' = OrderedMultiMap.to_assoc_list(ctx2);
  if (List.length(ctx1') != List.length(ctx2')) {
    false;
  } else {
    List.for_all(
      (((k, v1), (_, v2))) => String.equal(k, k) && equal_v(v1, v2),
      List.combine(ctx1', ctx2'),
    );
  };
};

let empty = OrderedMultiMap.empty((module Core.String));

let extend = OrderedMultiMap.extend;
let concat = OrderedMultiMap.concat;
let lookup = OrderedMultiMap.lookup;
let lookup_n = OrderedMultiMap.lookup_n;
let contains = OrderedMultiMap.contains;
let mapo = (f, m) => OrderedMultiMap.mapo((module Core.String), f, m);
let filter = (f, m) => OrderedMultiMap.filter((module Core.String), f, m);
let filter_map = (f, m) =>
  OrderedMultiMap.filter_map((module Core.String), f, m);
let singleton = (x, y): t('a) =>
  OrderedMultiMap.singleton((x, y), (module Core.String));
let filter_find_map = OrderedMultiMap.filter_find_map;
let to_assoc_list = OrderedMultiMap.to_assoc_list;
let of_assoc_list = OrderedMultiMap.of_assoc_list(_, (module Core.String));
let map2 = OrderedMultiMap.map2((module Core.String), _); // [Matt]: I have no idea why Ocaml needs this _
