module type OrderedShowType = {
  include Map.OrderedType;

  let pp: (Format.formatter, t) => unit;
};

module type OrderedSexpType = {
  include Map.OrderedType;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module type OrderedYojsonType = {
  include Map.OrderedType;

  let yojson_of_t: t => Yojson.Safe.t;
  let t_of_yojson: Yojson.Safe.t => t;
};

module type OrderedType = {
  include OrderedShowType;
  include OrderedSexpType with type t := t;
  include OrderedYojsonType with type t := t;
};

module type ShowS = {
  include Map.S;

  let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit;
};

module type SexpS = {
  include Map.S;

  let sexp_of_t: ('v => Sexplib.Sexp.t, t('v)) => Sexplib.Sexp.t;
  let t_of_sexp: (Sexplib.Sexp.t => 'v, Sexplib.Sexp.t) => t('v);
};

module type YojsonS = {
  include Map.S;

  let yojson_of_t: ('v => Yojson.Safe.t, t('v)) => Yojson.Safe.t;
  let t_of_yojson: (Yojson.Safe.t => 'v, Yojson.Safe.t) => t('v);
};

module type S = {
  include ShowS;
  include SexpS with type t('a) := t('a) and type key := key;
  include YojsonS with type t('a) := t('a) and type key := key;
};

module MakeShowFor = (O: OrderedShowType, S: Map.S with type key = O.t) => {
  let pp = (pp_v, f, map) =>
    S.iter(
      (k, v) => Format.fprintf(f, "%a -> %a@\n", O.pp, k, pp_v, v),
      map,
    );
};

module MakeShow = (O: OrderedShowType) : (ShowS with type key = O.t) => {
  module M = Map.Make(O);

  include M;
  include MakeShowFor(O, M);
};

module MakeSexpFor = (O: OrderedSexpType, S: Map.S with type key = O.t) => {
  open Sexplib.Std;

  [@deriving sexp]
  type binding('v) = (O.t, 'v);

  let sexp_of_t = (sexp_of_v, map) =>
    map |> S.bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
  let t_of_sexp = (v_of_sexp, sexp) =>
    sexp
    |> list_of_sexp(binding_of_sexp(v_of_sexp))
    |> List.to_seq
    |> S.of_seq;
};

module MakeSexp = (O: OrderedSexpType) : (SexpS with type key = O.t) => {
  module M = Map.Make(O);

  include M;
  include MakeSexpFor(O, M);
};

module MakeYojsonFor = (O: OrderedYojsonType, S: Map.S with type key = O.t) => {
  [@deriving yojson]
  type binding('v) = (O.t, 'v);

  let yojson_of_t = (yojson_of_v, map) =>
    map |> S.bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));
  let t_of_yojson = (v_of_yojson, yojson) =>
    yojson
    |> list_of_yojson(binding_of_yojson(v_of_yojson))
    |> List.to_seq
    |> S.of_seq;
};

module MakeYojson = (O: OrderedYojsonType) : (YojsonS with type key = O.t) => {
  module M = Map.Make(O);

  include M;
  include MakeYojsonFor(O, M);
};

module MakeFor = (O: OrderedType, M: Map.S with type key = O.t) => {
  include MakeShowFor(O, M);
  include MakeSexpFor(O, M);
  include MakeYojsonFor(O, M);
};

module Make = (O: OrderedType) : (S with type key = O.t) => {
  module M = Map.Make(O);

  include M;
  include MakeFor(O, M);
};
