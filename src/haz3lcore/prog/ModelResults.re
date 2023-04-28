open Sexplib.Std;

module Key = {
  include String;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
};

module M = Util.MapUtil.Make(Key);
include M;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = M.t(ModelResult.t);

let init = (~step=false, ds: list((Key.t, DHExp.t))): t => {
  ds
  |> List.map(((key, d)) => {
       let result =
         if (step) {
           Interface.init(d);
         } else {
           Interface.evaluate(d);
         };
       (key, ModelResult.init(result));
     })
  |> List.to_seq
  |> of_seq;
};

let lookup = (results: t, key: Key.t) => find_opt(key, results);
