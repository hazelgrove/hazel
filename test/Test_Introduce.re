open Language;
open Alcotest;

let exp = testable(Fmt.using(DHExp.show, Fmt.string), DHExp.fast_equal);

let find_hole_id = (e: Exp.t): option(Id.t) => {
  exception Found(Id.t);
  switch (
    Exp.map_term(
      ~f_pat=
        (continue, p) =>
          switch (p.term) {
          | EmptyHole => raise(Found(IdTagged.rep_id(p)))
          | _ => continue(p)
          },
      ~f_exp=
        (continue, e) =>
          switch (e.term) {
          | EmptyHole => raise(Found(IdTagged.rep_id(e)))
          | _ => continue(e)
          },
      e,
    )
  ) {
  | exception (Found(id)) => Some(id)
  | _ => None
  };
};

let introduction_test = (before: string, expected: string) => {
  open Util.OptUtil.Syntax;

  let serialized = {
    open Haz3lcore;
    let* zip = Parser.to_zipper(before);
    let MakeTerm.{term: exp, term_data, _} = MakeTerm.from_zip_for_sem(zip);
    let* hole_id = find_hole_id(exp);
    let* zip = Move.jump_to_side_of_id(Left, zip, hole_id);
    let* zip = Move.local(ByToken, Right, zip); // To get on the hole itself
    let* zip =
      Select.current_term(
        term_data,
        ~defs_exclude_bodies=false,
        ~case_rules=false,
        zip,
      );
    let statics =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        exp,
      );
    let+ zip = Introduce.introduce(Indicated.ci_of(zip, statics), zip);
    Printer.of_zipper(~holes="?", zip);
  };

  check(option(string), "Introduce", Some(expected), serialized);
};

let introduce_expression = (x: Typ.t): option(Exp.t) =>
  Haz3lcore.Introduce.IntroduceExp.introduce(x)
  |> Option.map(((a, _b, _c)) => a);

let tests =
  IdTagged.FreshGrammar.[
    (
      "Introduce.introduce_expression",
      [
        test_case("Arrow type", `Quick, () => {
          check(
            option(exp),
            "Function",
            Some(Exp.(fn(Pat.empty_hole(), empty_hole()))),
            introduce_expression(Typ.(arrow(int(), int()))),
          )
        }),
        test_case(
          "Product types",
          `Quick,
          () => {
            check(
              option(exp),
              "Cardinality 0",
              Some(Exp.(tuple([]))),
              introduce_expression(Typ.(prod([]))),
            );
            check(
              option(exp),
              "Cardinality 2",
              Some(Exp.(tuple([empty_hole(), empty_hole()]))),
              introduce_expression(Typ.(prod([int(), int()]))),
            );
            check(
              option(exp),
              "Cardinality 3",
              Some(Exp.(tuple([empty_hole(), empty_hole(), empty_hole()]))),
              introduce_expression(Typ.(prod([int(), int(), int()]))),
            );
            check(
              option(exp),
              "Cardinality 4",
              Some(
                Exp.(
                  tuple([
                    empty_hole(),
                    empty_hole(),
                    empty_hole(),
                    empty_hole(),
                  ])
                ),
              ),
              introduce_expression(Typ.(prod([int(), int(), int(), int()]))),
            );
            check(
              option(exp),
              "Cardinality 5",
              Some(
                Exp.(
                  tuple([
                    empty_hole(),
                    empty_hole(),
                    empty_hole(),
                    empty_hole(),
                    empty_hole(),
                  ])
                ),
              ),
              introduce_expression(
                Typ.(prod([int(), int(), int(), int(), int()])),
              ),
            );
          },
        ),
        test_case(
          "Labeled tuples",
          `Quick,
          () => {
            check(
              option(exp),
              "Singleton",
              Some(Exp.(tuple([tup_label(label("l"), empty_hole())]))),
              introduce_expression(
                Typ.(prod([tup_label(label("l"), int())])),
              ),
            );
            check(
              option(exp),
              "Multiple",
              Some(
                Exp.(
                  tuple([
                    tup_label(label("l"), empty_hole()),
                    tup_label(label("l2"), empty_hole()),
                  ])
                ),
              ),
              introduce_expression(
                Typ.(
                  prod([
                    tup_label(label("l"), int()),
                    tup_label(label("l2"), string()),
                  ])
                ),
              ),
            );
          },
        ),
        test_case("Singleton Variant", `Quick, () => {
          check(
            option(exp),
            "Function",
            Some(Exp.(constructor("A", None))),
            introduce_expression(Typ.(sum([Variant("A", [], None)]))),
          )
        }),
        test_case("Type fun", `Quick, () => {
          check(
            option(exp),
            "Function",
            Some(Exp.(typ_fun(TPat.empty_hole(), empty_hole(), None))),
            introduce_expression(Typ.(forall(TPat.var("a"), var("a")))),
          )
        }),
        test_case("String", `Quick, () => {
          check(
            option(exp),
            "String",
            Some(Exp.(string(""))),
            introduce_expression(Typ.string()),
          )
        }),
        test_case("List", `Quick, () => {
          check(
            option(exp),
            "List",
            Some(Exp.(list_lit([]))),
            introduce_expression(Typ.(list(int()))),
          )
        }),
      ],
    ),
    (
      "Introduce.introduce.expression",
      [
        test_case("Tuple", `Quick, () => {
          introduction_test(
            "let x : (Int, Int) =in x",
            "let x : (Int, Int) =(?, ?)in x",
          )
        }),
        test_case("Function", `Quick, () => {
          introduction_test(
            "let x : Int -> Int =in x",
            "let x : Int -> Int =fun ? -> ?in x",
          )
        }),
        test_case("Already parenthesized tuple", `Quick, () => {
          introduction_test(
            "let x : (Int, Int) = ( ) in x",
            "let x : (Int, Int) = (?, ? ) in x",
          )
        }),
        test_case("Nested tuple", `Quick, () => {
          introduction_test(
            "let x : (Int, (Int, Int)) = (1,  ) in x",
            "let x : (Int, (Int, Int)) = (1,(?, ?)  ) in x",
          )
        }),
        test_case("Explicit hole", `Quick, () => {
          introduction_test(
            "let x : (Int, Int) = ? in x",
            "let x : (Int, Int) = (?, ?) in x",
          )
        }),
        test_case(
          "Singleton variant",
          `Quick,
          () => {
            introduction_test("let x : +A = ? in x", "let x : +A = A in x");
            introduction_test(
              "let x : +B(Int) = ? in x",
              "let x : +B(Int) = B(?) in x",
            );
          },
        ),
      ],
    ),
    (
      "Introduce.introduce.pattern",
      [
        test_case(
          "Tuple Pattern",
          `Quick,
          () => {
            introduction_test(
              "let ? : () = () in 1",
              "let () : () = () in 1",
            );
            introduction_test(
              "let x : (Int, Int) -> Int = fun ? -> 1 in x",
              "let x : (Int, Int) -> Int = fun (?, ?) -> 1 in x",
            );
            introduction_test(
              "let x : (Int, (Int, Int)) -> Int = fun (a, ?) -> 1 in x",
              "let x : (Int, (Int, Int)) -> Int = fun (a, (?, ?)) -> 1 in x",
            );
          },
        ),
        test_case("Tuple Pattern already parenthesized", `Quick, () => {
          introduction_test(
            "let x : (Int, Int) -> Int = fun (?) -> 1 in x",
            "let x : (Int, Int) -> Int = fun (?, ?) -> 1 in x",
          )
        }),
        test_case(
          "Singleton Variant",
          `Quick,
          () => {
            introduction_test("let ? : +A = A in 1", "let A : +A = A in 1");
            introduction_test(
              "let ? : +A(Int) = A(1) in 1",
              "let A(?) : +A(Int) = A(1) in 1",
            );
          },
        ),
      ],
    ),
  ];
