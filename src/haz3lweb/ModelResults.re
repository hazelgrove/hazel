open Sexplib.Std;
open Haz3lcore;

module Key = {
  include String;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
};

module M = Util.MapUtil.Make(Key);
include M;

type t = M.t(ModelResult.t);

let init = (ds: list((Key.t, DHExp.t))): t =>
  ds
  |> List.map(((key, d)) =>
       (key, ModelResult.init(Interface.evaluate(d)))
     )
  |> List.to_seq
  |> of_seq;
