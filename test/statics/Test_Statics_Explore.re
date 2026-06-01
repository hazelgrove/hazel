open Alcotest;
open Language;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let all_marks = serialized => {
  parse_exp(serialized) |> statics |> errors |> List.concat_map(snd);
};

let has_free = (name, marks) =>
  List.exists(
    fun
    | Mark.Free(v) => v == name
    | _ => false,
    marks,
  );

let tests = (
  "Statics.Explore",
  [
    fully_consistent_typecheck(
      "Explore assumes free variables in explored expression",
      "explore 1 + 2 + x in 0",
      Some(int()),
    ),
    test_case(
      "Explore assumptions do not bind the body",
      `Quick,
      () => {
        let marks = all_marks("explore 1 + x in y");
        check(Alcotest.bool, "x is assumed", false, has_free("x", marks));
        check(Alcotest.bool, "y remains free", true, has_free("y", marks));
      },
    ),
  ],
);
