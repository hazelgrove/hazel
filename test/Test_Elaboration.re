open Alcotest;
open Haz3lcore;

let dhexp_eq = (d1: option(DHExp.t), d2: option(DHExp.t)): bool =>
  switch (d1, d2) {
  | (Some(d1), Some(d2)) => DHExp.fast_equal(d1, d2)
  | _ => false
  };

let dhexp_print = (d: option(DHExp.t)): string =>
  switch (d) {
  | None => "None"
  | Some(d) => DHExp.show(d)
  };

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(dhexp_print, Fmt.string), dhexp_eq);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let mk_map = CoreSettings.on |> Interface.Statics.mk_map;
let dhexp_of_uexp = u => Elaborator.dhexp_of_uexp(mk_map(u), u, false);
let alco_check = dhexp_typ |> Alcotest.check;

let u1: Term.UExp.t = {ids: [id_at(0)], term: Int(8)};
let single_integer = () =>
  alco_check("Integer literal 8", Some(IntLit(8)), dhexp_of_uexp(u1));

let u2: Term.UExp.t = {ids: [id_at(0)], term: EmptyHole};
let empty_hole = () =>
  alco_check(
    "Empty hole",
    Some(EmptyHole(id_at(0), 0)),
    dhexp_of_uexp(u2),
  );

let u3: Term.UExp.t = {
  ids: [id_at(0)],
  term: Parens({ids: [id_at(1)], term: Var("y")}),
};
let d3: DHExp.t =
  NonEmptyHole(TypeInconsistent, id_at(1), 0, FreeVar(id_at(1), 0, "y"));
let free_var = () =>
  alco_check(
    "Nonempty hole with free variable",
    Some(d3),
    dhexp_of_uexp(u3),
  );

let u4: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Let(
      {
        ids: [id_at(1)],
        term:
          Tuple([
            {ids: [id_at(2)], term: Var("a")},
            {ids: [id_at(3)], term: Var("b")},
          ]),
      },
      {
        ids: [id_at(4)],
        term:
          Tuple([
            {ids: [id_at(5)], term: Int(4)},
            {ids: [id_at(6)], term: Int(6)},
          ]),
      },
      {
        ids: [id_at(7)],
        term:
          BinOp(
            Int(Minus),
            {ids: [id_at(8)], term: Var("a")},
            {ids: [id_at(9)], term: Var("b")},
          ),
      },
    ),
};
let d4: DHExp.t =
  Let(
    Tuple([Var("a"), Var("b")]),
    Tuple([IntLit(4), IntLit(6)]),
    BinIntOp(Minus, BoundVar("a"), BoundVar("b")),
  );
let let_exp = () =>
  alco_check(
    "Let expression for tuple (a, b)",
    Some(d4),
    dhexp_of_uexp(u4),
  );

let u5: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    BinOp(
      Int(Plus),
      {ids: [id_at(1)], term: Bool(false)},
      {ids: [id_at(2)], term: Var("y")},
    ),
};
let d5: DHExp.t =
  BinIntOp(
    Plus,
    NonEmptyHole(TypeInconsistent, id_at(1), 0, BoolLit(false)),
    NonEmptyHole(TypeInconsistent, id_at(2), 0, FreeVar(id_at(2), 0, "y")),
  );
let bin_op = () =>
  alco_check(
    "Inconsistent binary integer operation (plus)",
    Some(d5),
    dhexp_of_uexp(u5),
  );

let u6: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    If(
      {ids: [id_at(1)], term: Bool(false)},
      {ids: [id_at(2)], term: Int(8)},
      {ids: [id_at(3)], term: Int(6)},
    ),
};
let d6: DHExp.t =
  IfThenElse(DH.ConsistentIf, BoolLit(false), IntLit(8), IntLit(6));
let consistent_if = () =>
  alco_check(
    "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
    Some(d6),
    dhexp_of_uexp(u6),
  );

let u7: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Ap(
      {
        ids: [id_at(1)],
        term:
          Fun(
            {ids: [id_at(2)], term: Var("x")},
            {
              ids: [id_at(3)],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [id_at(4)], term: Int(4)},
                  {ids: [id_at(5)], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [id_at(6)], term: Var("y")},
    ),
};
let d7: DHExp.t =
  Ap(
    Fun(
      Var("x"),
      Unknown(Internal),
      BinIntOp(
        Plus,
        IntLit(4),
        Cast(BoundVar("x"), Unknown(Internal), Int),
      ),
      None,
    ),
    NonEmptyHole(TypeInconsistent, id_at(6), 0, FreeVar(id_at(6), 0, "y")),
  );
let ap_fun = () =>
  alco_check(
    "Application of a function of a free variable wrapped inside a nonempty hole constructor",
    Some(d7),
    dhexp_of_uexp(u7),
  );

let u8: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Match(
      {
        ids: [id_at(1)],
        term:
          BinOp(
            Int(Equals),
            {ids: [id_at(2)], term: Int(4)},
            {ids: [id_at(3)], term: Int(3)},
          ),
      },
      [
        (
          {ids: [id_at(6)], term: Bool(true)},
          {ids: [id_at(4)], term: Int(24)},
        ),
        (
          {ids: [id_at(7)], term: Bool(false)},
          {ids: [id_at(5)], term: Bool(false)},
        ),
      ],
    ),
};
let d8scrut: DHExp.t = BinIntOp(Equals, IntLit(4), IntLit(3));
let d8rules =
  DHExp.[
    Rule(BoolLit(true), IntLit(24)),
    Rule(BoolLit(false), BoolLit(false)),
  ];
let d8a: DHExp.t =
  InconsistentBranches(id_at(0), 0, Case(d8scrut, d8rules, 0));
let d8: DHExp.t = NonEmptyHole(TypeInconsistent, id_at(0), 0, d8a);
let inconsistent_case = () =>
  alco_check(
    "Inconsistent branches where the first branch is an integer and second branch is a boolean",
    Some(d8),
    dhexp_of_uexp(u8),
  );

let u9: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Let(
      {
        ids: [id_at(1)],
        term:
          TypeAnn(
            {ids: [id_at(2)], term: Var("f")},
            {
              ids: [id_at(3)],
              term:
                Arrow(
                  {ids: [id_at(4)], term: Int},
                  {ids: [id_at(5)], term: Int},
                ),
            },
          ),
      },
      {
        ids: [id_at(6)],
        term:
          Fun(
            {ids: [id_at(7)], term: Var("x")},
            {
              ids: [id_at(8)],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [id_at(9)], term: Int(1)},
                  {ids: [id_at(10)], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [id_at(11)], term: Int(55)},
    ),
};
let d9: DHExp.t =
  Let(
    Var("f"),
    FixF(
      "f",
      Arrow(Int, Int),
      Fun(
        Var("x"),
        Int,
        BinIntOp(Plus, IntLit(1), BoundVar("x")),
        Some("f"),
      ),
    ),
    IntLit(55),
  );
let let_fun = () =>
  alco_check(
    "Let expression for function which wraps a fix point constructor around the function",
    Some(d9),
    dhexp_of_uexp(u9),
  );

let elaboration_tests = [
  test_case("Single integer", `Quick, single_integer),
  test_case("Empty hole", `Quick, empty_hole),
  test_case("Free variable", `Quick, free_var),
  test_case("Let expression", `Quick, let_exp),
  test_case("Inconsistent binary operation", `Quick, bin_op),
  test_case("Consistent if statement", `Quick, consistent_if),
  test_case("Application of function on free variable", `Quick, ap_fun),
  test_case("Inconsistent case statement", `Quick, inconsistent_case),
  test_case("Let expression for a function", `Quick, let_fun),
  test_case("Sample failure", `Quick, Alcotest.fail("Sample failure")),

];
