open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* `!!e` evaluates e, then steps on the shape of its value. */

let is_true = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Atom(Bool(true)) => true
  | _ => false
  };

/* `forall (x : T) -> !!f(x)`, with its binder's name and type. */
let forall_binder = (e: Exp.t): option((Var.t, Typ.t)) =>
  switch (Exp.term_of(e)) {
  | Forall(p, body) =>
    switch (Pat.term_of(p), Exp.term_of(body)) {
    | (Asc(x, t), Standard(app)) =>
      switch (Pat.term_of(x), Exp.term_of(app)) {
      | (Var(x), Ap(_)) => Some((x, t))
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };

let standard = (src: string) =>
  check(bool, src ++ " is true", true, is_true(parse_and_evaluate(src)));

let not_shown_standard = (src: string) =>
  check(
    bool,
    src ++ " is not true",
    false,
    is_true(parse_and_evaluate(src)),
  );

let steps_to_forall_at = (cls: Atom.cls, src: string) =>
  switch (forall_binder(parse_and_evaluate(src))) {
  | Some((_, t)) =>
    check(
      bool,
      src ++ ": binder type",
      true,
      Poly.equal(Typ.term_of(t), Atom(cls)),
    )
  | None =>
    Alcotest.fail(
      src
      ++ " did not step to a forall: "
      ++ Exp.show(parse_and_evaluate(src)),
    )
  };

let tests = (
  "Evaluator.Standard",
  [
    test_case("data built from atoms is standard", `Quick, () =>
      List.iter(
        ~f=standard,
        [
          "!!3",
          {|!!"s"|},
          "!!true",
          "!!(1, [2, 3])",
          "!![]",
          "!!(1 :: [2])",
          "!!(a=1, b=2)",
        ],
      )
    ),
    test_case("constructors are standard when their arguments are", `Quick, () =>
      List.iter(
        ~f=standard,
        [
          "type Shape = + Dot + Box(Int) in !!Dot",
          "type Shape = + Dot + Box(Int) in !!Box(3)",
        ],
      )
    ),
    test_case("an error is not standard", `Quick, () =>
      not_shown_standard("!!(1 / 0)")
    ),
    test_case(
      "a function steps to a forall over its argument type", `Quick, () =>
      steps_to_forall_at(Int, "!!(fun (x : Int) -> x + 1)")
    ),
    test_case("a recursive function steps to a forall too", `Quick, () =>
      steps_to_forall_at(
        Int,
        "let count = fun (n : Int) -> if n <= 0 then 0 else count(n - 1) in !!count",
      )
    ),
    test_case(
      "the forall's binder doesn't capture the function's names", `Quick, () =>
      switch (
        forall_binder(
          parse_and_evaluate("let x = 5 in !!(fun (y : Int) -> x + y)"),
        )
      ) {
      | Some((name, _)) =>
        check(bool, "binder isn't x", false, String.equal(name, "x"))
      | None => Alcotest.fail("did not step to a forall")
      }
    ),
    test_case("a function with no argument type is stuck", `Quick, () =>
      check(
        bool,
        "stuck",
        true,
        switch (Exp.term_of(parse_and_evaluate("!!(fun x -> x)"))) {
        | Standard(_) => true
        | _ => false
        },
      )
    ),
    test_case("data holding a function is not shown standard", `Quick, () =>
      not_shown_standard("!!(1, fun (x : Int) -> x)")
    ),
  ],
);
