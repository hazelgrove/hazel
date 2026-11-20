open Alcotest;
open Web;
open Test_CellEditorPersistence;

/* A saved theorem exercise may carry a `theorem` field, the evaluation
   state of its theorem cell, which is rebuilt from the exercise's spec
   instead. Its lemmas cell may carry a `result` field too. It loads, with
   the lemmas, from either encoding. */

let saved: TheoremExerciseMode.Model.persistent = {lemmas: cell};

let loads_from_sexp = () => {
  let lemmas =
    with_sexp_field(
      "result",
      result_sexp,
      CellEditor.Model.sexp_of_persistent(cell),
    );
  check(
    string,
    "the lemmas load",
    Sexplib.Sexp.to_string(
      TheoremExerciseMode.Model.sexp_of_persistent(saved),
    ),
    List([List([Atom("lemmas"), lemmas])])
    |> with_sexp_field("theorem", result_sexp)
    |> TheoremExerciseMode.Model.persistent_of_sexp
    |> TheoremExerciseMode.Model.sexp_of_persistent
    |> Sexplib.Sexp.to_string,
  );
};

let loads_from_yojson = () => {
  let lemmas =
    with_yojson_field(
      "result",
      result_yojson,
      CellEditor.Model.yojson_of_persistent(cell),
    );
  check(
    string,
    "the lemmas load",
    Yojson.Safe.to_string(
      TheoremExerciseMode.Model.yojson_of_persistent(saved),
    ),
    `Assoc([("lemmas", lemmas)])
    |> with_yojson_field("theorem", result_yojson)
    |> TheoremExerciseMode.Model.persistent_of_yojson
    |> TheoremExerciseMode.Model.yojson_of_persistent
    |> Yojson.Safe.to_string,
  );
};

let tests = (
  "TheoremExercisePersistence",
  [
    test_case(
      "a theorem exercise saved with its theorem's result loads from sexp",
      `Quick,
      loads_from_sexp,
    ),
    test_case(
      "a theorem exercise saved with its theorem's result loads from json",
      `Quick,
      loads_from_yojson,
    ),
  ],
);
