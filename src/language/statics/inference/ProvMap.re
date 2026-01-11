open Util;

[@deriving (sexp, yojson)]
type binding('v) = (StringProv.t, 'v);

include Map.Make(StringProv);

let sexp_of_t = (sexp_of_v, map) =>
  map |> bindings |> Sexplib.Std.sexp_of_list(sexp_of_binding(sexp_of_v));

let t_of_sexp = (v_of_sexp, sexp) =>
  sexp
  |> Sexplib.Std.list_of_sexp(binding_of_sexp(v_of_sexp))
  |> List.to_seq
  |> of_seq;

let yojson_of_t = (yojson_of_v, map) =>
  map |> bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));

let t_of_yojson = (v_of_yojson, json) =>
  json
  |> list_of_yojson(binding_of_yojson(v_of_yojson))
  |> List.to_seq
  |> of_seq;

let pp = (pp_v, fmt, map) =>
  bindings(map)
  |> List.iter(((k, v)) =>
       Format.fprintf(fmt, "%a -> %a\n", StringProv.pp, k, pp_v, v)
     ) /* }*/;
