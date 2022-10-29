open Sexplib.Std;
open Haz3lcore;

module Key = {
  include String;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
};

module M = Util.MapUtil.Make(Key);
include M;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = M.t(ModelResult.t);

let init = (ds: list((Key.t, DHExp.t))): t =>
  ds
  |> List.map(((key, d)) =>
       (key, ModelResult.init(Interface.step(d, -1)))
     )
  |> List.to_seq
  |> of_seq;

let lookup = (results: t, key: Key.t) => find_opt(key, results);
