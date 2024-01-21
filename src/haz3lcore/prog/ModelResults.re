open Sexplib.Std;

/*
 ModelResults is used to store the results of
 evaluations requested by the current editor mode,
 with the key distinguishing these requests.

 See the SchoolExercise module for an example.
 */
module Key = {
  include String;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
};

module M = Util.MapUtil.Make(Key);
include M;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = M.t(ModelResult.t);

let init = (~settings, ds: list((Key.t, DHExp.t))): t =>
  ds
  |> List.map(((key, d)) =>
       (
         key,
         ModelResult.init(
           Interface.evaluate(~settings, ~env=Builtins.env_init, d),
         ),
       )
     )
  |> List.to_seq
  |> of_seq;

let lookup = (results: t, key: Key.t) => find_opt(key, results);

let get = (results: t, key: Key.t) =>
  ModelResult.unwrap(lookup(results, key));
