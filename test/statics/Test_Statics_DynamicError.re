open Alcotest;
open Test_Statics_Prelude;
open Language;

/* Statics attaches a `Mark.DynamicError` for a `DynamicErrorHole` to the
 *inner* expression's info entry: printing strips the DynamicErrorHole
 wrapper, so only inner ids appear in rendered syntax and can carry the
 error decoration (shards, cursor inspector, problem sidebar). */

let is_dynamic_error =
  fun
  | Mark.DynamicError(_) => true
  | _ => false;

let test_mark_attaches_to_inner_expression = () => {
  let inner = parse_exp("1 + 1");
  let e = Exp.fresh(DynamicErrorHole(inner, DivideByZero));
  let map = statics(e);
  let inner_marks =
    switch (Statics.Map.lookup(Exp.rep_id(inner), map)) {
    | Some(info) => Info.marks_of(info)
    | None => Alcotest.fail("no info entry for the inner expression")
    };
  Alcotest.check(
    bool,
    "inner expression carries the DynamicError mark",
    true,
    List.exists(is_dynamic_error, inner_marks),
  );
};

let test_no_mark_without_error_hole = () => {
  let e = parse_exp("1 + 1");
  let map = statics(e);
  Alcotest.check(
    bool,
    "plain expression carries no DynamicError mark",
    true,
    !
      Id.Map.exists(
        (_, info: Info.t) =>
          List.exists(is_dynamic_error, Info.marks_of(info)),
        map,
      ),
  );
};

let tests = (
  "Statics_DynamicError",
  [
    test_case(
      "DynamicErrorHole marks its inner expression",
      `Quick,
      test_mark_attaches_to_inner_expression,
    ),
    test_case(
      "no DynamicError mark on plain expressions",
      `Quick,
      test_no_mark_without_error_hole,
    ),
  ],
);
