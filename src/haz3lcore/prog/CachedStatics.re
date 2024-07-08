open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type statics = {
  term: Term.UExp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
};

let empty_statics: statics = {
  term: Term.UExp.{ids: [Id.invalid], term: Triv},
  info_map: Id.Map.empty,
  error_ids: [],
};

module Key = {
  include String;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
};

module M = Util.MapUtil.Make(Key);
include M;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = M.t(statics);

let mk = (ds: list((Key.t, statics))): t =>
  ds |> List.to_seq |> of_seq |> map(Fun.id);

let lookup = (results: t, key: Key.t) =>
  switch (find_opt(key, results)) {
  | None => empty_statics
  | Some(statics) => statics
  };
