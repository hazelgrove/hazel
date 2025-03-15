open Haz3lcore;
open Alcotest;
open Introduce;

let exp = testable(Fmt.using(DHExp.show, Fmt.string), DHExp.fast_equal);

let find_id = (p: Exp.t => bool, e: Exp.t): option(Id.t) => {
  exception Found(Id.t);
  switch (
    Exp.map_term(
      ~f_exp=
        (continue, e) =>
          if (p(e)) {
            raise(Found(IdTagged.rep_id(e)));
          } else {
            continue(e);
          },
      e,
    )
  ) {
  | exception (Found(id)) => Some(id)
  | _ => None
  };
};

let tests =
  IdTagged.FreshGrammar.[
    (
      "Introduce.introduce_expression",
      [
        test_case("Arrow type", `Quick, () => {
          check(
            option(exp),
            "Function",
            Some(Exp.(fn(Pat.empty_hole(), empty_hole(), None, None))),
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
      ],
    ),
    (
      "Introduce.introduce",
      [
        test_case(
          "Tuple",
          `Quick,
          () => {
            open Util.OptUtil.Syntax; // TODO Figure out a way to make this work with whitespace. We probably need a token for the empty hole
            // We could use ? but then we need to make sure introduce removes the ? when it introduces
            let serialized = {
              let* zip = Printer.zipper_of_string("let x : (Int, Int) =in x");
              let exp = MakeTerm.from_zip_for_sem(zip).term;
              let* hole_id =
                find_id(
                  e =>
                    switch (e.term) {
                    | EmptyHole => true
                    | _ => false
                    },
                  exp,
                );
              let x = Editor.Model.mk(zip);
              module Move = Move.Make((val Editor.Model.to_move_s(x)));
              let* zip = Move.jump_to_id(zip, hole_id);
              let* zip = Move.go(Local(Right(ByChar)), zip); // To get on the hole itself
              let statics =
                Statics.mk(CoreSettings.on, Builtins.ctx_init, exp);
              let+ zip = Introduce.introduce(statics, zip);
              Printer.zipper_to_string(~holes=Some("?"), zip);
            };

            check(
              option(string),
              "Introduce",
              Some("let x : (Int, Int) =(?, ?)in x"),
              serialized,
            );
          },
        ),
        test_case(
          "Function",
          `Quick,
          () => {
            open Util.OptUtil.Syntax; // TODO Figure out a way to make this work with whitespace. We probably need a token for the empty hole
            // We could use ? but then we need to make sure introduce removes the ? when it introduces
            let serialized = {
              let* zip = Printer.zipper_of_string("let x : Int -> Int =in x");
              let exp = MakeTerm.from_zip_for_sem(zip).term;
              let* hole_id =
                find_id(
                  e =>
                    switch (e.term) {
                    | EmptyHole => true
                    | _ => false
                    },
                  exp,
                );
              let x = Editor.Model.mk(zip);
              module Move = Move.Make((val Editor.Model.to_move_s(x)));
              let* zip = Move.jump_to_id(zip, hole_id);
              let* zip = Move.go(Local(Right(ByChar)), zip); // To get on the hole itself
              let statics =
                Statics.mk(CoreSettings.on, Builtins.ctx_init, exp);
              let+ zip = Introduce.introduce(statics, zip);
              Printer.zipper_to_string(~holes=Some("?"), zip);
            };

            check(
              option(string),
              "Introduce",
              Some("let x : Int -> Int =fun ? -> ?in x"),
              serialized,
            );
          },
        ),
      ],
    ),
  ];
