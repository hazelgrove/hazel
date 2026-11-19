open Alcotest;
open Web;

/* A saved cell may carry a `result` field, the stepper and theorem state
   of its evaluation, which a cell doesn't persist. It loads, with its
   code, from either encoding. */

/* What a `result` field holds for a cell whose stepper isn't open. */
let result_sexp =
  Sexplib.Sexp.of_string("((stepper ()) (theorems ((thm_map ()))))");

let result_yojson =
  Yojson.Safe.from_string({|{"stepper": null, "theorems": {"thm_map": []}}|});

let with_sexp_field =
    (name: string, value: Sexplib.Sexp.t, saved: Sexplib.Sexp.t)
    : Sexplib.Sexp.t =>
  switch (saved) {
  | List(fields) => List(fields @ [List([Atom(name), value])])
  | Atom(_) => fail("a record saves as a list of fields")
  };

let with_yojson_field =
    (name: string, value: Yojson.Safe.t, saved: Yojson.Safe.t): Yojson.Safe.t =>
  switch (saved) {
  | `Assoc(fields) => `Assoc(fields @ [(name, value)])
  | _ => fail("a record saves as an object")
  };

let cell =
  CellEditor.Model.from_persistent_zipper(
    ~root=Haz3lcore.Sort.Exp,
    Haz3lcore.PersistentZipper.of_text("let x = 1 in x"),
  );

let loads_from_sexp = () => {
  let saved = CellEditor.Model.sexp_of_persistent(cell);
  check(
    string,
    "the code loads",
    Sexplib.Sexp.to_string(saved),
    with_sexp_field("result", result_sexp, saved)
    |> CellEditor.Model.persistent_of_sexp
    |> CellEditor.Model.sexp_of_persistent
    |> Sexplib.Sexp.to_string,
  );
};

let loads_from_yojson = () => {
  let saved = CellEditor.Model.yojson_of_persistent(cell);
  check(
    string,
    "the code loads",
    Yojson.Safe.to_string(saved),
    with_yojson_field("result", result_yojson, saved)
    |> CellEditor.Model.persistent_of_yojson
    |> CellEditor.Model.yojson_of_persistent
    |> Yojson.Safe.to_string,
  );
};

let tests = (
  "CellEditorPersistence",
  [
    test_case(
      "a cell saved with its result loads from sexp",
      `Quick,
      loads_from_sexp,
    ),
    test_case(
      "a cell saved with its result loads from json",
      `Quick,
      loads_from_yojson,
    ),
  ],
);
