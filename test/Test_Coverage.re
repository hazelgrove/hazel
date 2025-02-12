open Alcotest;
open Haz3lcore;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);
let testable_info_error_pat =
  testable(Fmt.using(Info.show_error_pat, Fmt.string), Info.equal_error_pat);

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let info_error_of_pat_id = (f: Exp.t, id: Id.t): option(Info.error_pat) => {
  Statics.get_pat_error_at(statics(f), id);
};

let alco_check = Alcotest.option(testable_typ) |> Alcotest.check;

let reusable_id = Id.mk();
let reusable_pat: TermBase.pat_term => TermBase.pat_t =
  p => {
    {ids: [reusable_id], term: p, copied: false};
  };

let bare_let =
  test_case("Bare let has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let x = 1 in x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(let_(reusable_pat(Var("x")), int(1), var("x"))),
        reusable_id,
      ),
    )
  );

let bare_fun =
  test_case("Bare fun has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun x -> x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(fun_(reusable_pat(Var("x")), var("x"), None, None)),
        reusable_id,
      ),
    )
  );

let annotated_let =
  test_case("Annotated let has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let x : Int = 1 in x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(
          let_(
            reusable_pat(
              Pat.Fresh.(
                Cast(var("x"), Typ.Fresh.int(), Typ.Fresh.unknown(Internal))
              ),
            ),
            int(1),
            var("x"),
          )
        ),
        reusable_id,
      ),
    )
  );

let annotated_fun =
  test_case("Annotated fun has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun x : Int -> x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(
          fun_(
            reusable_pat(
              Pat.Fresh.(
                Cast(var("x"), Typ.Fresh.int(), Typ.Fresh.unknown(Internal))
              ),
            ),
            var("x"),
            None,
            None,
          )
        ),
        reusable_id,
      ),
    )
  );

let let_tuple =
  test_case("Let binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let (x, y, z) = 1 in x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(
          let_(
            reusable_pat(
              Pat.Fresh.(Tuple([var("x"), var("y"), var("z")])),
            ),
            tuple([int(1), int(2), int(3)]),
            var("x"),
          )
        ),
        reusable_id,
      ),
    )
  );

let fun_tuple =
  test_case("Fun binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun (x, y, z) => x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(
          fun_(
            reusable_pat(
              Pat.Fresh.(Tuple([var("x"), var("y"), var("z")])),
            ),
            var("x"),
            None,
            None,
          )
        ),
        reusable_id,
      ),
    )
  );

let annotated_let_tuple =
  test_case(
    "Annotated let binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let (x, y, z): (Int, Int, Int) = 1 in x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(
          let_(
            reusable_pat(
              Pat.Fresh.(
                Cast(
                  tuple([var("x"), var("y"), var("z")]),
                  Typ.Fresh.(prod([int(), int(), int()])),
                  Typ.Fresh.unknown(Internal),
                )
              ),
            ),
            tuple([int(1), int(2), int(3)]),
            var("x"),
          )
        ),
        reusable_id,
      ),
    )
  );

let annotated_fun_tuple =
  test_case(
    "Annotated fun binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun (x, y, z) : (Int, Int, Int) -> x",
      None,
      info_error_of_pat_id(
        Exp.Fresh.(
          fun_(
            reusable_pat(
              Pat.Fresh.(
                Cast(
                  tuple([var("x"), var("y"), var("z")]),
                  Typ.Fresh.(prod([int(), int(), int()])),
                  Typ.Fresh.unknown(Internal),
                )
              ),
            ),
            var("x"),
            None,
            None,
          )
        ),
        reusable_id,
      ),
    )
  );

// TODO: list examples from paper
// TODO: first example from paper
// TODO: recursive type
// TODO: integers
// TODO: floats
// TODO: strings
// TODO: Andrew's card example
// TODO: test that exercises the default case
// TODO: unknown scrutinee
// TODO: partially unknown scrutinee - still check exhaustiveness at outer level?

let tests = (
  "Pattern Coverage Checker",
  [
    bare_let,
    bare_fun,
    annotated_let,
    annotated_fun,
    let_tuple,
    fun_tuple,
    annotated_let_tuple,
    annotated_fun_tuple,
  ],
);
