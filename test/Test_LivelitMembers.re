open Alcotest;

/* Livelits as module members: `module Lib = { let ^f = ... } in` makes
   `^f` usable outside the module as `Lib.^f`, keyed `Lib.f` in the body's
   context and reaching its definition at run time as the member
   `Lib."^f"` (UserLivelit.requalify, head_key). A library of livelits is
   then just a module a slide defines, with a livelit per member. */

let run = Test_UserLivelits.run;
let statics = Test_UserLivelits.statics;
let dbl = Test_UserLivelits.dbl_def;

let marked = text =>
  Test_UserLivelits.has_mark(_ => true, fst(statics(text)));

let int_is = (msg, expected, program) =>
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    msg,
    run(string_of_int(expected)),
    run(program),
  );

/* ^pick: one of a list of values, its type a parameter, as the
   configuration's ^choice is. */
let pick = "typfun A -> fun cases : [(String, A)] -> {
type Model = A;
type Action = A;
type Expansion = A;
let init = let (_, v) = head(cases) in Pure(v);
let update = fun _m : Model -> fun a : Action -> Pure(a);
let view = fun m : Model -> Pure(Html.text(\"\"));
let expand = Functional(fun m : Model -> m)
}";

/* ^span: an Int between bounds given as value parameters. */
let span = "fun (lo, hi) : (Int, Int) -> {
type Model = Int;
type Action = Int;
type Expansion = Int;
let init = Pure(lo);
let update = fun _m : Model -> fun a : Action -> Pure(a);
let view = fun m : Model -> Pure(Html.text(\"\"));
let expand = Functional(fun m : Model -> m)
}";

let tests = (
  "UserLivelits.Members",
  [
    test_case(
      "a member is used as Lib.^f",
      `Quick,
      () => {
        let program =
          "module Lib = { let ^dbl = " ++ dbl ++ " } in Lib.^dbl(4)";
        check(bool, "no static errors", false, marked(program));
        int_is("its expansion", 8, program);
      },
    ),
    test_case("a projected member use expands as a bare one does", `Quick, () =>
      int_is(
        "^^livelit(Lib.^dbl(4))",
        8,
        "module Lib = { let ^dbl = " ++ dbl ++ " } in ^^livelit(Lib.^dbl(4))",
      )
    ),
    test_case("a member is still usable inside its module", `Quick, () =>
      int_is(
        "inside and out",
        6,
        "module Lib = { let ^dbl = "
        ++ dbl
        ++ "; let one = ^dbl(1) } in Lib.one + Lib.^dbl(2)",
      )
    ),
    test_case(
      "a nested module's member is A.B.^f",
      `Quick,
      () => {
        let program =
          "module A = { module B = { let ^dbl = "
          ++ dbl
          ++ " } } in A.B.^dbl(5)";
        check(bool, "no static errors", false, marked(program));
        int_is("its expansion", 10, program);
      },
    ),
    test_case(
      "a type-parameterized member abbreviates as Lib.^f@<T>",
      `Quick,
      () => {
        let program =
          "module Lib = { let ^pick = "
          ++ pick
          ++ " } in let ^num = Lib.^pick@<Int>([(\"one\", 1), (\"two\", 2)]) in ^num(2)";
        check(bool, "no static errors", false, marked(program));
        int_is("its expansion", 2, program);
      },
    ),
    test_case(
      "a member with value parameters, used directly and abbreviated",
      `Quick,
      () => {
        let direct =
          "module Lib = { let ^span = " ++ span ++ " } in Lib.^span(0, 9)(3)";
        let abbreviated =
          "module Lib = { let ^span = "
          ++ span
          ++ " } in let ^digit = Lib.^span(0, 9) in ^digit(4)";
        check(bool, "direct: no static errors", false, marked(direct));
        check(
          bool,
          "abbreviated: no static errors",
          false,
          marked(abbreviated),
        );
        int_is("direct", 3, direct);
        int_is("abbreviated", 4, abbreviated);
      },
    ),
    test_case("a model of the wrong type is reported", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        marked("module Lib = { let ^dbl = " ++ dbl ++ " } in Lib.^dbl(true)"),
      )
    ),
    test_case("a member the module does not have is reported", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        marked("module Lib = { let ^dbl = " ++ dbl ++ " } in Lib.^nope(1)"),
      )
    ),
    test_case("a signature that leaves the member out hides it", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        marked(
          "module Lib : {let x : Int} = { let x = 1; let ^dbl = "
          ++ dbl
          ++ " } in Lib.^dbl(1)",
        ),
      )
    ),
    Test_TextRoundtrip.text_fixed_point_case(
      ~name="Lib.^f(model) prints back as written",
      "module Lib = { let ^dbl = " ++ dbl ++ " } in A.B.^dbl(Lib.^dbl(4))",
    ),
  ],
);
