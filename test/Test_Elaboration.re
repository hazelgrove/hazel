open Alcotest;
open Language;

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let mk_map = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));
let dhexp_of_uexp = u =>
  Elaborator.elaborate(
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), u),
    u,
  )
  |> fst;
let alco_check = dhexp_typ |> Alcotest.check;

module PlainTests = {
  open IdTagged.FreshGrammar;

  let parse_exp = (s: string) => {
    switch (Parse.parse_exp(s)) {
    | Some(e) => e
    | None => Alcotest.fail("Failed to parse expression: " ++ s)
    };
  };
  let u1: Exp.t = Exp.int(8);
  let single_integer = () =>
    alco_check("Integer literal 8", u1, dhexp_of_uexp(u1));

  let u2: Exp.t = Exp.empty_hole();
  let empty_hole = () => alco_check("Empty hole", u2, dhexp_of_uexp(u2));

  let u3: Exp.t = Exp.(parens(var("y")));

  let free_var = () => alco_check("free variable", u3, dhexp_of_uexp(u3));

  let u4: Exp.t =
    Exp.(
      let_(
        Pat.(tuple([var("a"), var("b")])),
        tuple([int(4), int(6)]),
        bin_op(Int(Minus), var("a"), var("b")),
      )
    );

  let let_exp = () =>
    alco_check("Let expression for tuple (a, b)", u4, dhexp_of_uexp(u4));

  let u5 = Exp.(bin_op(Int(Plus), bool(false), var("y")));

  let d5 =
    Exp.(
      bin_op(
        Int(Plus),
        failed_cast(bool(false), Typ.(bool()), Typ.int()),
        cast(var("y"), Typ.unknown(Internal), Typ.int()),
      )
    );

  let u6: Exp.t = Exp.(if_(bool(false), int(8), int(6)));

  let consistent_if = () =>
    alco_check(
      "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
      u6,
      dhexp_of_uexp(u6),
    );

  // x => 4 + 5
  let f =
    Exp.(
      fn(Pat.var("x"), bin_op(Int(Plus), int(4), int(5)), None, None)
    );

  let f' =
    Exp.(
      fn(
        Pat.var("x"),
        bin_op(Int(Plus), int(4), int(5)),
        Some(Typ.unknown(Hole(EmptyHole))),
        None,
      )
    );
  let unapplied_function = () =>
    alco_check("A function", f', dhexp_of_uexp(f));

  let u7: Exp.t = Exp.(ap(Forward, f, var("y")));

  let d7: Exp.t = Exp.(ap(Forward, f', var("y")));

  let ap_fun = () =>
    alco_check("Application of a function", d7, dhexp_of_uexp(u7));

  let u8: Exp.t =
    Exp.(
      match(
        bin_op(Poly(Equals), int(4), int(3)),
        [(Pat.bool(true), int(24)), (Pat.bool(false), bool(false))],
      )
    );

  let d8: Exp.t =
    Exp.(
      match(
        bin_op(Poly(Equals), int(4), int(3)),
        [
          (
            Pat.(bool(true)),
            cast(int(24), Typ.int(), Typ.unknown(Internal)),
          ),
          (
            Pat.bool(false),
            cast(bool(false), Typ.bool(), Typ.unknown(Internal)),
          ),
        ],
      )
    );

  let inconsistent_case = () =>
    alco_check(
      "Inconsistent branches where the first branch is an integer and second branch is a boolean",
      d8,
      dhexp_of_uexp(u8),
    );

  let u9: Exp.t =
    Exp.(
      let_(
        Pat.(
          cast(
            var("f"),
            Typ.arrow(Typ.int(), Typ.int()),
            Typ.unknown(Internal),
          )
        ),
        fn(
          Pat.var("x"),
          bin_op(Int(Plus), int(1), var("x")),
          None,
          None,
        ),
        int(55),
      )
    );

  let d9: Exp.t =
    Exp.(
      let_(
        Pat.(
          cast(
            var("f"),
            Typ.arrow(Typ.int(), Typ.int()),
            Typ.unknown(Internal),
          )
        ),
        fn(
          Pat.var("x"),
          bin_op(Int(Plus), int(1), var("x")),
          Some(Typ.int()),
          Some("f"),
        ),
        int(55),
      )
    );

  let let_fun = () =>
    alco_check(
      "Let expression for function which is not recursive",
      d9,
      dhexp_of_uexp(u9),
    );

  let deferral = () =>
    alco_check(
      "string_sub(\"hello\", 1, _)",
      Exp.(
        deferred_ap(
          var("string_sub"),
          [string("hello"), int(1), deferral(InAp)],
        )
      ),
      dhexp_of_uexp(
        Exp.(
          deferred_ap(
            var("string_sub"),
            [string("hello"), int(1), deferral(InAp)],
          )
        ),
      ),
    );

  let ap_deferral_single_argument = () =>
    alco_check(
      "string_sub(\"hello\", 1, _)(2)",
      Exp.(
        ap(
          Forward,
          deferred_ap(
            var("string_sub"),
            [string("hello"), int(1), deferral(InAp)],
          ),
          int(2),
        )
      ),
      dhexp_of_uexp(
        Exp.(
          ap(
            Forward,
            deferred_ap(
              var("string_sub"),
              [string("hello"), int(1), deferral(InAp)],
            ),
            int(2),
          )
        ),
      ),
    );

  let ap_of_deferral_of_hole = () =>
    alco_check(
      "?(_, _, 3)(1., true)",
      Exp.(
        ap(
          Forward,
          deferred_ap(
            empty_hole(),
            [deferral(InAp), deferral(InAp), int(3)],
          ),
          tuple([float(1.), bool(true)]),
        )
      ),
      Exp.(
        dhexp_of_uexp(
          ap(
            Forward,
            deferred_ap(
              empty_hole(),
              [deferral(InAp), deferral(InAp), int(3)],
            ),
            tuple([float(1.), bool(true)]),
          ),
        )
      ),
    );

  /*
     Labeled Tuple Elaboration Test
     ```hazel
     let add : (street=String, city=String, state=String, zipcode=Int) = (
       "123 Maple St",
       "Ann Arbor",
       "MI",
       48103
     ) in add
     ```
     elaborates to
     ```hazel
     let add : (street=String, city=String, state=String, zipcode=Int) =
     (street="123 Maple St", city="Ann Arbor", state="MI", zipcode=48103) in add
     ```
   */
  let elaborated_labeled_tuple = () => {
    let typ =
      Typ.(
        parens(
          prod([
            tup_label(label("street"), string()),
            tup_label(label("city"), string()),
            tup_label(label("state"), string()),
            tup_label(label("zipcode"), int()),
          ]),
        )
      );

    let full_labeled_tuple_program: Exp.t =
      Exp.(
        let_(
          Pat.(cast(var("add"), typ, Typ.unknown(Internal))),
          parens(
            tuple([
              string("123 Maple St"),
              string("Ann Arbor"),
              string("MI"),
              int(48103),
            ]),
          ),
          var("add"),
        )
      );

    alco_check(
      "Labeled Tuple label introduction",
      Exp.(
        let_(
          Pat.(cast(var("add"), typ, Typ.unknown(Internal))),
          tuple([
            tup_label(label("street"), string("123 Maple St")),
            tup_label(label("city"), string("Ann Arbor")),
            tup_label(label("state"), string("MI")),
            tup_label(label("zipcode"), int(48103)),
          ]),
          var("add"),
        )
      ),
      dhexp_of_uexp(full_labeled_tuple_program),
    );
  };

  let singleton_labeled_tuple = () =>
    Exp.(
      alco_check(
        "Singleton Labeled Tuple",
        tuple([tup_label(label("label"), string("a string value"))]),
        dhexp_of_uexp(
          tuple([tup_label(label("label"), string("a string value"))]),
        ),
      )
    );

  let singleton_labeled_tuple_elaborates_labels = () =>
    alco_check(
      "let x : (l=String) = \"a\" in x",
      Exp.(
        let_(
          Pat.(
            cast(
              var("x"),
              Typ.(prod([tup_label(label("l"), string())])),
              Typ.unknown(Internal),
            )
          ),
          tuple([tup_label(label("l"), string("a"))]),
          var("x"),
        )
      ),
      dhexp_of_uexp(parse_exp("let x : (l=String) = \"a\" in x")),
    );

  /* Labeled Tuple Rearranging
       ```hazel
      let val : (a=Int, b=String, Float, c=Bool)= (1,
        1.0,
        c=true,
        b="a") in val ```
       elaborates to
       (a=1, b="a", 1.0, c=true)
     */
  let rearranged_labeled_tuple = () => {
    let typ =
      Typ.(
        parens(
          prod([
            tup_label(label("a"), Typ.int()),
            tup_label(label("b"), Typ.string()),
            float(),
            tup_label(label("c"), Typ.bool()),
          ]),
        )
      );
    let rearranged_labeled_tuple_program: Exp.t =
      Exp.(
        let_(
          Pat.(cast(var("val"), typ, Typ.unknown(Internal))),
          parens(
            tuple([
              int(1),
              float(1.0),
              tup_label(label("c"), bool(true)),
              tup_label(label("b"), string("a")),
            ]),
          ),
          var("val"),
        )
      );

    alco_check(
      "Labeled Tuple rearrangement",
      Exp.(
        let_(
          Pat.(cast(var("val"), typ, Typ.unknown(Internal))),
          tuple([
            tup_label(label("a"), int(1)),
            tup_label(label("b"), string("a")),
            float(1.0),
            tup_label(label("c"), bool(true)),
          ]),
          var("val"),
        )
      ),
      dhexp_of_uexp(rearranged_labeled_tuple_program),
    );
  };

  let skip_known_bug = (message: string, expression: string) =>
    test_case("Known Bug: " ++ message, `Quick, () => {
      [@warning "-21"]
      {
        let uexp = parse_exp(expression);
        let statics = mk_map(uexp);
        Alcotest.skip();
        let _ = Elaborator.elaborate(statics, uexp);
        ();
      }
    });
  let tests = [
    test_case("Single integer", `Quick, single_integer),
    test_case("Empty hole", `Quick, empty_hole),
    test_case("Free variable", `Quick, free_var),
    test_case("Let expression", `Quick, let_exp),
    test_case("Consistent if statement", `Quick, consistent_if),
    test_case("An unapplied function", `Quick, unapplied_function),
    test_case("Application of function on free variable", `Quick, ap_fun),
    /* test_case("Inconsistent case statement", `Quick, inconsistent_case), */
    test_case("Let expression for a function", `Quick, let_fun),
    test_case(
      "Function application with a deferred argument",
      `Quick,
      deferral,
    ),
    test_case(
      "Function application with a single remaining argument after deferral",
      `Quick,
      ap_deferral_single_argument,
    ),
    test_case("Inconsistent type ascription", `Quick, () =>
      alco_check(
        {|4 : String|},
        parse_exp({|4 : String|}),
        dhexp_of_uexp(parse_exp({|4 : String|})) // Ignoring casts for now
      )
    ),
    test_case("Inconsistent let ascription", `Quick, () =>
      alco_check(
        {|let x : String = 4  in x|},
        Exp.(
          let_(
            Pat.(cast(var("x"), Typ.string(), Typ.unknown(Internal))),
            int(4),
            var("x"),
          )
        ),
        dhexp_of_uexp(parse_exp({|let x : String = 4 in x|})),
      )
    ),
    test_case("Inconsistent list ascription", `Quick, () =>
      alco_check(
        {|[1,2,3] : [String]|},
        parse_exp({|[1,2,3] : [String]|}),
        dhexp_of_uexp(parse_exp({|[1,2,3] : [String]|})),
      )
    ),
    test_case("Inlines type aliases", `Quick, () =>
      alco_check(
        {|type T = [String] in [1,2,3] : T|},
        parse_exp({|[1,2,3] : [String]|}),
        dhexp_of_uexp(parse_exp({|type T = [String] in [1,2,3] : T|})),
      )
    ),
    test_case(
      "Function application with a deferral of a hole",
      `Quick,
      ap_of_deferral_of_hole,
    ),
    test_case("Labeled tuple elaboration", `Quick, elaborated_labeled_tuple),
    test_case("Rearranged labeled tuple", `Quick, rearranged_labeled_tuple),
    test_case(
      "Singleton labeled tuple adds labels",
      `Quick,
      singleton_labeled_tuple_elaborates_labels,
    ),
    test_case("Singleton labeled tuple", `Quick, singleton_labeled_tuple),
    test_case(
      "Singleton labeled tuple analysis adds label",
      `Quick,
      () => {
        let typ =
          Typ.(parens(prod([tup_label(label("l"), Typ.string())])));
        alco_check(
          "Singleton labeled tuple analysis adds label",
          Exp.(
            let_(
              Pat.(cast(var("x"), typ, Typ.unknown(Internal))),
              tuple([tup_label(label("l"), string("a"))]),
              var("x"),
            )
          ),
          dhexp_of_uexp(
            Exp.(
              let_(
                Pat.(cast(var("x"), typ, Typ.unknown(Internal))),
                parens(string("a")),
                var("x"),
              )
            ),
          ),
        );
      },
    ),
    test_case(
      "Singleton labeled tuple analysis adds label with type alias", `Quick, () =>
      alco_check(
        {|type T = (a=String) in
        let x : T = "hello" in x|},
        Exp.(
          let_(
            Pat.(
              cast(
                var("x"),
                Typ.(prod([tup_label(label("a"), string())])),
                Typ.unknown(Internal),
              )
            ),
            tuple([tup_label(label("a"), string("hello"))]),
            var("x"),
          )
        ),
        dhexp_of_uexp(
          parse_exp({|type T = (a=String) in let x : T = "hello" in x|}),
        ),
      )
    ),
    test_case(
      "Singleton labeled tuple analysis adds label with type alias", `Quick, () =>
      alco_check(
        {|let zip_only : (zip=Int) = (zip=12345) in zip_only|},
        Exp.(
          let_(
            Pat.(
              cast(
                var("zip_only"),
                Typ.(prod([tup_label(label("zip"), int())])),
                Typ.unknown(Internal),
              )
            ),
            tuple([tup_label(label("zip"), int(12345))]),
            var("zip_only"),
          )
        ),
        dhexp_of_uexp(
          parse_exp({|let zip_only : (zip=Int) = (zip=12345) in zip_only|}),
        ),
      )
    ),
    test_case(
      "Singleton labeled argument function application with known type",
      `Quick,
      () =>
      alco_check(
        {|(fun a=(x:Int) -> x)(a=1)|},
        Exp.(
          ap(
            Forward,
            fn(
              Pat.(
                tuple([tup_label(label("a"), asc(var("x"), Typ.int()))])
              ),
              var("x"),
              Some(Typ.(prod([tup_label(label("a"), int())]))),
              None,
            ),
            tuple([tup_label(label("a"), int(1))]),
          )
        ),
        dhexp_of_uexp(parse_exp({|(fun a=(x:Int) -> x)(a=1)|})) // Ignoring casts for now
      )
    ),
    test_case(
      "Singleton labeled argument function application with no label in ap",
      `Quick,
      () =>
      alco_check(
        {|(fun a=(x:Int) -> x)(1)|},
        Exp.(
          ap(
            Forward,
            fn(
              Pat.(
                tuple([tup_label(label("a"), asc(var("x"), Typ.int()))])
              ),
              var("x"),
              Some(Typ.(prod([tup_label(label("a"), Typ.int())]))),
              None,
            ),
            tuple([tup_label(label("a"), int(1))]),
          )
        ),
        dhexp_of_uexp(parse_exp({|(fun a=(x:Int) -> x)(1)|})),
      )
    ),
    test_case("nested different singleton labeled arguments", `Quick, () =>
      alco_check(
        {|let x : (b=c=String) = b="" in x|},
        Exp.(
          let_(
            Pat.(
              asc(
                var("x"),
                Typ.(
                  prod([
                    tup_label(
                      label("b"),
                      prod([tup_label(label("c"), string())]),
                    ),
                  ])
                ),
              )
            ),
            tuple([
              tup_label(
                label("b"),
                tuple([tup_label(label("c"), string(""))]),
              ),
            ]),
            var("x"),
          )
        ),
        dhexp_of_uexp(parse_exp({|let x : (b=c=String) = b="" in x|})),
      )
    ),
    test_case(
      "Singleton labeled argument function application with unknown type",
      `Quick,
      () =>
      alco_check(
        {|(fun a=x->x)(a=1)|},
        Exp.(
          ap(
            Forward,
            fn(
              Pat.(tuple([tup_label(label("a"), var("x"))])),
              var("x"),
              Some(
                Typ.(prod([tup_label(label("a"), unknown(Internal))])),
              ),
              None,
            ),
            tuple([tup_label(label("a"), int(1))]),
          )
        ),
        DHExp.strip_casts(dhexp_of_uexp(parse_exp({|(fun a=x->x)(a=1)|}))),
      )
    ),
    test_case("Singleton labeled argument let with unknown type", `Quick, () =>
      alco_check(
        {|let x : (a=?) = (a=1) in x|},
        Exp.(
          let_(
            Pat.var("x"),
            tuple([tup_label(label("a"), int(1))]),
            var("x"),
          )
        ),
        DHExp.strip_casts(
          dhexp_of_uexp(parse_exp({|let x : (a=?) = (a=1) in x|})),
        ) // Ignoring casts for now
      )
    ),
    test_case(
      "Automatically add label in pattern inside type annotation", `Quick, () => {
      alco_check(
        "Adds label",
        dhexp_of_uexp(
          parse_exp(
            {|let fn : (a=String) -> Int =
  fun (a=a : String) -> 1
in 1|},
          ),
        ),
        dhexp_of_uexp(
          parse_exp(
            {|let fn : (a=String) -> Int =
  fun (a : String) -> 1
in 1|},
          ),
        ),
      )
    }),
    test_case("Does not add labels with different cardinality", `Quick, () =>
      alco_check(
        "Does not add label",
        parse_exp({|(1, 2)|}),
        DHExp.strip_casts(
          dhexp_of_uexp(parse_exp({|(1, 2) : (a= ,b= ,  )|})),
        ),
      )
    ),
    skip_known_bug(
      "Nontermination in typ normalization",
      {|type x = x in (([] @ false) @ [] @< Float >) @< x([(())]) > @ case test 0.000006 end:: "f":: ? | B => (())| x => (())| (()) => ?| [] => ?| ? => 12 end|},
    ),
    skip_known_bug(
      "Invalid typ ap", // TODO https://github.com/hazelgrove/hazel/issues/1625
      "let [(A: (Bool(Bool))), (_: (String))] = 0 in ()",
    ),
    skip_known_bug(
      "Type join of ap", // TODO https://github.com/hazelgrove/hazel/issues/1625
      "type x = + B((forall x -> ?)(?)) in case a | B => 0| B => 0 end",
    ),
    QCheck_alcotest.to_alcotest(
      QCheck.Test.make(
        ~name="Elaboration does not crash",
        ~count=10000,
        QCheck_Util.arb_exp(~minimal_idents=true, 50),
        exp => {
        switch (mk_map(exp)) {
        | statics =>
          switch (Elaborator.elaborate(statics, exp)) {
          | _ => true
          | exception (Failure(msg) as e) =>
            switch (msg) {
            | _
                when
                  List.exists(
                    (==)(msg),
                    [
                      "type application in dynamics", // https://github.com/hazelgrove/hazel/issues/1459?issue=hazelgrove%7Chazel%7C1625
                      "normalize exceeded 1000 recursive calls", // https://github.com/hazelgrove/hazel/issues/1627
                      "Type join of ap" // https://github.com/hazelgrove/hazel/issues/1459?issue=hazelgrove%7Chazel%7C1625
                    ],
                  ) =>
              print_endline("Known failure: " ++ Printexc.to_string(e));
              true;
            | _ => raise(e)
            }
          }
        | exception e =>
          print_endline("Skipping statics: " ++ Printexc.to_string(e));
          true;
        }
      }),
    ),
  ];
};
module MenhirElaborationTests = {
  //dhexp = expected
  //uexp = tested
  open IdTagged.FreshGrammar;

  let alco_check_menhir = (name: string, dhexp: string, uexp: Term.Exp.t) =>
    alco_check(
      name,
      Grammar.map_exp_annotation(
        _ => IdTagged.IdTag.fresh(),
        MenhirParser.Conversion.Exp.of_menhir_ast(
          MenhirParser.Interface.parse_program(dhexp),
        ),
      ),
      dhexp_of_uexp(uexp),
    );

  //Test for an empty hole
  let empty_hole_str = "?";
  let empty_hole_uexp: Exp.t = Exp.empty_hole();
  let empty_hole_menhir = () =>
    alco_check_menhir("Empty hole (menhir)", empty_hole_str, empty_hole_uexp);

  //Test for a free variable
  let free_var_uexp: Exp.t = Exp.(parens(var("y")));
  let free_var_menhir = () =>
    alco_check_menhir(
      "Nonempty hole with free variable (menhir)",
      "y",
      dhexp_of_uexp(free_var_uexp),
    );

  //Menhir test for a binary operation
  let bin_op_uexp: Exp.t = Exp.(bin_op(Int(Plus), bool(false), var("y")));

  let bin_op_str = "false + y";

  let bin_op_menhir = () =>
    alco_check_menhir(
      "Inconsistent binary integer operation (plus)",
      bin_op_str,
      dhexp_of_uexp(bin_op_uexp),
    );

  //Inconsistent branches menhir test
  let inconsistent_case_menhir_str = "
    case 4 == 3
    | true => 24
    | false => false
    end
";
  /* let inconsistent_case_uexp: Exp.t =
     Exp.(
       match(
         bin_op(Poly(Equals), int(4), int(3)),
         [(Pat.bool(true), int(24)), (Pat.bool(false), bool(false))],
       )
     ); */

  /* let inconsistent_case_menhir = () =>
     alco_check_menhir(
       "Inconsistent branches where the first branch is an integer and second branch is a boolean (menhir)",
       inconsistent_case_menhir_str,
       inconsistent_case_uexp,
     ); */

  //Consistent if statement menhir test
  let consistent_if_uexp: Exp.t = Exp.(if_(bool(false), int(8), int(6)));

  let consistent_if_str = "
    if false then 8 else 6
";
  let consistent_if_menhir = () =>
    alco_check_menhir(
      "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
      consistent_if_str,
      dhexp_of_uexp(consistent_if_uexp),
    );

  //Single integer menhir test
  let single_int_str = "8";
  let single_int_uexp: Exp.t = Exp.int(8);
  let single_integer_menhir = () =>
    alco_check_menhir(
      "Single integer test (menhir)",
      single_int_str,
      single_int_uexp,
    );

  //Menhir let expression test
  let let_exp_str = "let (a, b) = (4, 6) in a - b";
  let let_exp_uexp: Exp.t =
    Exp.(
      let_(
        Pat.(tuple([var("a"), var("b")])),
        tuple([int(4), int(6)]),
        bin_op(Int(Minus), var("a"), var("b")),
      )
    );

  let let_exp_menhir = () =>
    alco_check_menhir(
      "Let expression for tuple (a, b) (menhir)",
      let_exp_str,
      let_exp_uexp,
    );

  let typ_ap_str = "(typfun x -> 4)@<Int>";
  let typ_ap_uexp: Exp.t =
    Exp.(typ_ap(typ_fun(TPat.var("x"), int(4), None), Typ.int()));

  let typ_ap_menhir = () =>
    alco_check_menhir("Type ap test (menhir)", typ_ap_str, typ_ap_uexp);

  let constructor_str = "X/~";
  let constructor_uexp: Exp.t = Exp.constructor("X", None);
  let constructor_menhir = () =>
    alco_check_menhir(
      "Constructor test (menhir)",
      constructor_str,
      constructor_uexp,
    );

  /*
   <<1 / 2 ? `a`>>
       */
  let dynamic_error_hole_str = "<<(1/0) ? `DivideByZero`>>";
  let dynamic_error_hole_uexp: Exp.t =
    Exp.(
      dynamic_error_hole(
        bin_op(Int(Divide), int(1), int(0)),
        InvalidOperationError.DivideByZero,
      )
    );

  let dynamic_error_hole_menhir = () =>
    alco_check_menhir(
      "Dynamic error hole (menhir)",
      dynamic_error_hole_str,
      dynamic_error_hole_uexp,
    );

  let builtin_fun_str = "infinity";
  let builtin_fun_uexp: Exp.t = Exp.builtin_fun("infinity");
  let builtin_fun_menhir = () =>
    alco_check_menhir(
      "Builtin function test (menhir)",
      builtin_fun_str,
      builtin_fun_uexp,
    );

  let undef_str = "undef";
  let undef_uexp: Exp.t = Exp.undefined();
  let undef_menhir = () =>
    alco_check_menhir("Undef test (menhir)", undef_str, undef_uexp);

  let test_str = "test 1 end";
  let test_uexp: Exp.t = Exp.(test(int(1)));
  let test_menhir = () =>
    alco_check_menhir("Test failed (menhir)", test_str, test_uexp);

  let filter_str = "eval 1 in 0";
  let stepper_filter_kind: TermBase.stepper_filter_kind_t =
    StepperFilter.(
      filter({
        pat: Exp.int(1),
        act: (FilterAction.Eval, FilterAction.All),
      })
    );
  let filter_uexp: Exp.t = Exp.(filter(stepper_filter_kind, int(0)));
  let filter_menhir = () =>
    alco_check_menhir("Filter test (menhir)", filter_str, filter_uexp);

  let undefined_str = "
undef
";
  let undefined_uexp: Exp.t = Exp.undefined();
  let undefined_menhir = () =>
    alco_check_menhir(
      "Undefined test (menhir)",
      undefined_str,
      undefined_uexp,
    );

  let list_exp_str = "[1, 2, 3]";
  let list_exp_uexp: Exp.t = Exp.(list_lit([int(1), int(2), int(3)]));
  let list_exp_menhir = () =>
    alco_check_menhir("List exp (menhir)", list_exp_str, list_exp_uexp);

  let invalid_str = "
?e \"x\"
";
  let invalid_uexp: Exp.t = Exp.invalid("x");
  let invalid_menhir = () =>
    alco_check_menhir("Invalid test (menhir)", invalid_str, invalid_uexp);

  let ty_alias_str = "
x
";
  let ty_alias_uexp: Exp.t =
    Exp.(ty_alias(TPat.var("x"), Typ.int(), Exp.var("x")));
  let ty_alias_menhir = () =>
    alco_check_menhir(
      "Type alias test (menhir)",
      ty_alias_str,
      ty_alias_uexp,
    );

  let list_concat_str = "[1, 2] @ [3, 4]";
  let list_concat_uexp: Exp.t =
    Exp.(
      list_concat(
        list_lit([int(1), int(2)]),
        list_lit([int(3), int(4)]),
      )
    );
  let list_concat_menhir = () =>
    alco_check_menhir(
      "List concat test (menhir)",
      list_concat_str,
      list_concat_uexp,
    );

  let unop_str = "-1";
  let unop_uexp: Exp.t = Exp.(un_op(Int(Minus), int(1)));
  let unop_menhir = () =>
    alco_check_menhir("Unary operation test (menhir)", unop_str, unop_uexp);

  let seq_str = "1; 2";
  let seq_uexp: Exp.t = Exp.(seq(int(1), int(2)));
  let seq_menhir = () =>
    alco_check_menhir("Sequence test (menhir)", seq_str, seq_uexp);

  let fixf_str = "fix x -> 1";
  let fixf_uexp: Exp.t = Exp.(fix_f(Pat.var("x"), int(1), None));
  let fixf_menhir = () =>
    alco_check_menhir("FixF test (menhir)", fixf_str, fixf_uexp);

  let tests = [
    test_case("Filter test (menhir)", `Quick, filter_menhir),
    test_case("Test failed (menhir)", `Quick, test_menhir),
    test_case("Built-in function (menhir)", `Quick, builtin_fun_menhir),
    test_case(
      "Dynamic error hole (menhir)",
      `Quick,
      dynamic_error_hole_menhir,
    ),
    test_case("Constructor test (menhir)", `Quick, constructor_menhir),
    test_case("Type ap test (menhir)", `Quick, typ_ap_menhir),
    test_case("Let expression for a tuple (menhir)", `Quick, let_exp_menhir),
    test_case("Single integer (menhir)", `Quick, single_integer_menhir),
    test_case("Empty hole (menhir)", `Quick, empty_hole_menhir),
    test_case("Free var (menhir)", `Quick, free_var_menhir),
    test_case("Bin op (menhir)", `Quick, bin_op_menhir),
    /* test_case("Inconsistent case (menhir)", `Quick, inconsistent_case_menhir), */
    test_case("Consistent if (menhir)", `Quick, consistent_if_menhir),
    test_case("Undefined test (menhir)", `Quick, undefined_menhir),
    test_case("List exp (menhir)", `Quick, list_exp_menhir),
    test_case("Invalid test (menhir)", `Quick, invalid_menhir),
    test_case("Type alias test (menhir)", `Quick, ty_alias_menhir),
    test_case("List concat test (menhir)", `Quick, list_concat_menhir),
    test_case("Unary operation test (menhir)", `Quick, unop_menhir),
    test_case("Sequence test (menhir)", `Quick, seq_menhir),
    test_case("FixF test (menhir)", `Quick, fixf_menhir),
  ];
};

let tests = [
  ("Elaboration tests", PlainTests.tests),
  ("Menhir elaboration tests", MenhirElaborationTests.tests),
];
