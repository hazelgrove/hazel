// Exceptions for invalid queries
open Alcotest;
open Language;
open Test_Statics_Slicing_Prelude;

let expect_exn = (name, pred, thunk): test_case(unit) =>
  test_case(name, `Quick, _ =>
    switch (thunk()) {
    | _ => Alcotest.fail("expected an invalid-query exception")
    | exception e =>
      check(bool, "raised expected invalid-query exception", true, pred(e))
    }
  );

let cases = [
  expect_exn(
    "focus-not-found",
    fun
    | S.Focus_not_found(_) => true
    | _ => false,
    () =>
    Statics.slice(
      ~ctx=base_ctx(),
      ~focus=Some(Id.mk()),
      ~direction=`Syn,
      parse_exp("1"),
      parse_typ("Int"),
    )
  ),
  expect_exn(
    "wrong-focus-sort",
    fun
    | S.Wrong_focus_sort => true
    | _ => false,
    () => {
      let e = parse_exp("fun (x : Int) -> x");
      Statics.slice(
        ~ctx=base_ctx(),
        ~focus=Some(pat_var(e, "x")),
        ~direction=`Syn,
        e,
        parse_typ("Int"),
      );
    },
  ),
  expect_exn(
    "incompatible-query-shape",
    fun
    | S.Incompatible_query(_) => true
    | _ => false,
    () =>
    Statics.slice(
      ~ctx=base_ctx(),
      ~focus=Some(whole(parse_exp("1"))),
      ~direction=`Syn,
      parse_exp("1"),
      parse_typ("Int -> Bool"),
    )
  ),
];

let tests = ("Statics.Slicing.InvalidQuery", cases);
