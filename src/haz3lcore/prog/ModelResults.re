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

let init_eval = (ds: list((Key.t, DHExp.t))): t =>
  ds |> List.to_seq |> of_seq |> map(ModelResult.init_eval);

let update_elabs =
  List.fold_right(((k, elab), acc) =>
    update(
      k,
      v =>
        Some(
          v
          |> Option.value(~default=ModelResult.NoElab)
          |> ModelResult.update_elab(elab),
        ),
      acc,
    )
  );

let lookup = (results: t, key: Key.t) => find_opt(key, results);

let get = (results: t, key: Key.t) => lookup(results, key);
let run_pending = (~settings) => M.map(ModelResult.run_pending(~settings));
