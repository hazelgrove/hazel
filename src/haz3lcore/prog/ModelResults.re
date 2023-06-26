open Sexplib.Std;

/* This module is a map used to store ModelResult.t objects
      where the key is used to distiguish between editors on
      a particular page.

      It is used in haz3lweb/Update.re
      The keys for school mode editors are in haz3lschool/SchoolExercise.re
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

let init = (ds: list((Key.t, DHExp.t))): t =>
  ds
  |> List.map(((key, d)) =>
       (key, ModelResult.init(Interface.evaluate(d)))
     )
  |> List.to_seq
  |> of_seq;

let lookup = (results: t, key: Key.t) => find_opt(key, results);
