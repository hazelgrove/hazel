open Alcotest;
open Haz3lcore;

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);

let mk_map = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));
let dhexp_of_uexp = u => Elaborator.elaborate(mk_map(u), u) |> fst;
let alco_check = dhexp_typ |> Alcotest.check;
let parse_exp = (s: string) => {
  switch (MakeTerm.parse_exp(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

module PlainTests = {
  let u1: Exp.t = {
    term: Atom(Int(Bigint.of_int(8))),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let single_integer = () =>
    alco_check("Integer literal 8", u1, dhexp_of_uexp(u1));

  let u2: Exp.t = {
    term: EmptyHole,
    annotation: {
      ids: [id_at(0)],
    },
  };
  let empty_hole = () => alco_check("Empty hole", u2, dhexp_of_uexp(u2));

  let u3: Exp.t = {
    term:
      Parens({
        term: Var("y"),
        annotation: {
          ids: [id_at(1)],
        },
      }),
    annotation: {
      ids: [id_at(0)],
    },
  };

  let free_var = () => alco_check("free variable", u3, dhexp_of_uexp(u3));

  let u4: Exp.t =
    Let(
      Tuple([Var("a") |> Pat.fresh, Var("b") |> Pat.fresh]) |> Pat.fresh,
      Tuple([
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(6))) |> Exp.fresh,
      ])
      |> Exp.fresh,
      BinOp(Int(Minus), Var("a") |> Exp.fresh, Var("b") |> Exp.fresh)
      |> Exp.fresh,
    )
    |> Exp.fresh;

  let let_exp = () =>
    alco_check("Let expression for tuple (a, b)", u4, dhexp_of_uexp(u4));

  let u5 =
    BinOp(
      Int(Plus),
      Atom(Bool(false)) |> Exp.fresh,
      Var("y") |> Exp.fresh,
    )
    |> Exp.fresh;

  let d5 =
    BinOp(
      Int(Plus),
      FailedCast(
        Atom(Bool(false)) |> Exp.fresh,
        Atom(Bool) |> Typ.fresh,
        Atom(Int) |> Typ.fresh,
      )
      |> Exp.fresh,
      Cast(
        Var("y") |> Exp.fresh,
        Unknown(Internal) |> Typ.fresh,
        Atom(Int) |> Typ.fresh,
      )
      |> Exp.fresh,
    )
    |> Exp.fresh;

  let bin_op = () =>
    alco_check(
      "Inconsistent binary integer operation (plus)",
      d5,
      dhexp_of_uexp(u5),
    );

  let u6: Exp.t =
    If(
      Atom(Bool(false)) |> Exp.fresh,
      Atom(Int(Bigint.of_int(8))) |> Exp.fresh,
      Atom(Int(Bigint.of_int(6))) |> Exp.fresh,
    )
    |> Exp.fresh;

  let consistent_if = () =>
    alco_check(
      "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
      u6,
      dhexp_of_uexp(u6),
    );

  // x => 4 + 5
  let f =
    Fun(
      Var("x") |> Pat.fresh,
      BinOp(
        Int(Plus),
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(5))) |> Exp.fresh,
      )
      |> Exp.fresh,
      None,
      None,
    )
    |> Exp.fresh;

  let f' =
    Fun(
      Var("x") |> Pat.fresh,
      BinOp(
        Int(Plus),
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(5))) |> Exp.fresh,
      )
      |> Exp.fresh,
      Some(Unknown(Hole(EmptyHole)) |> Typ.fresh),
      None,
    )
    |> Exp.fresh;
  let unapplied_function = () =>
    alco_check("A function", f', dhexp_of_uexp(f));

  let u7: Exp.t = Ap(Forward, f, Var("y") |> Exp.fresh) |> Exp.fresh;

  let d7: Exp.t = Ap(Forward, f', Var("y") |> Exp.fresh) |> Exp.fresh;

  let ap_fun = () =>
    alco_check("Application of a function", d7, dhexp_of_uexp(u7));

  let u8: Exp.t =
    Match(
      BinOp(
        Int(Equals),
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
      )
      |> Exp.fresh,
      [
        (
          Atom(Bool(true)) |> Pat.fresh,
          Atom(Int(Bigint.of_int(24))) |> Exp.fresh,
        ),
        (Atom(Bool(false)) |> Pat.fresh, Atom(Bool(false)) |> Exp.fresh),
      ],
    )
    |> Exp.fresh;

  let d8: Exp.t =
    Match(
      BinOp(
        Int(Equals),
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
      )
      |> Exp.fresh,
      [
        (
          Atom(Bool(true)) |> Pat.fresh,
          Cast(
            Atom(Int(Bigint.of_int(24))) |> Exp.fresh,
            Atom(Int) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Exp.fresh,
        ),
        (
          Atom(Bool(false)) |> Pat.fresh,
          Cast(
            Atom(Bool(false)) |> Exp.fresh,
            Atom(Bool) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Exp.fresh,
        ),
      ],
    )
    |> Exp.fresh;

  let inconsistent_case = () =>
    alco_check(
      "Inconsistent branches where the first branch is an integer and second branch is a boolean",
      d8,
      dhexp_of_uexp(u8),
    );

  let u9: Exp.t =
    Let(
      Cast(
        Var("f") |> Pat.fresh,
        Arrow(Atom(Int) |> Typ.fresh, Atom(Int) |> Typ.fresh) |> Typ.fresh,
        Unknown(Internal) |> Typ.fresh,
      )
      |> Pat.fresh,
      Fun(
        Var("x") |> Pat.fresh,
        BinOp(
          Int(Plus),
          Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
      Atom(Int(Bigint.of_int(55))) |> Exp.fresh,
    )
    |> Exp.fresh;

  let d9: Exp.t =
    Let(
      Var("f") |> Pat.fresh,
      Fun(
        Var("x") |> Pat.fresh,
        BinOp(
          Int(Plus),
          Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        Some(Atom(Int) |> Typ.fresh),
        Some("f"),
      )
      |> Exp.fresh,
      Atom(Int(Bigint.of_int(55))) |> Exp.fresh,
    )
    |> Exp.fresh;

  let let_fun = () =>
    alco_check(
      "Let expression for function which is not recursive",
      d9,
      dhexp_of_uexp(u9),
    );

  let deferral = () =>
    alco_check(
      "string_sub(\"hello\", 1, _)",
      DeferredAp(
        Var("string_sub") |> Exp.fresh,
        [
          Atom(String("hello")) |> Exp.fresh,
          Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
          Deferral(InAp) |> Exp.fresh,
        ],
      )
      |> Exp.fresh,
      dhexp_of_uexp(
        DeferredAp(
          Var("string_sub") |> Exp.fresh,
          [
            Atom(String("hello")) |> Exp.fresh,
            Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            Deferral(InAp) |> Exp.fresh,
          ],
        )
        |> Exp.fresh,
      ),
    );

  let ap_deferral_single_argument = () =>
    alco_check(
      "string_sub(\"hello\", 1, _)(2)",
      Ap(
        Forward,
        DeferredAp(
          Var("string_sub") |> Exp.fresh,
          [
            Atom(String("hello")) |> Exp.fresh,
            Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            Deferral(InAp) |> Exp.fresh,
          ],
        )
        |> Exp.fresh,
        Atom(Int(Bigint.of_int(2))) |> Exp.fresh,
      )
      |> Exp.fresh,
      dhexp_of_uexp(
        Ap(
          Forward,
          DeferredAp(
            Var("string_sub") |> Exp.fresh,
            [
              Atom(String("hello")) |> Exp.fresh,
              Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
              Deferral(InAp) |> Exp.fresh,
            ],
          )
          |> Exp.fresh,
          Atom(Int(Bigint.of_int(2))) |> Exp.fresh,
        )
        |> Exp.fresh,
      ),
    );

  let ap_of_deferral_of_hole = () =>
    alco_check(
      "?(_, _, 3)(1., true)",
      Ap(
        Forward,
        DeferredAp(
          Cast(
            Cast(
              EmptyHole |> Exp.fresh,
              Unknown(Internal) |> Typ.fresh,
              Arrow(
                Unknown(Internal) |> Typ.fresh,
                Unknown(Internal) |> Typ.fresh,
              )
              |> Typ.fresh,
            )
            |> Exp.fresh,
            Arrow(
              Unknown(Internal) |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            )
            |> Typ.fresh,
            Arrow(
              Prod([
                Unknown(Internal) |> Typ.fresh,
                Unknown(Internal) |> Typ.fresh,
                Unknown(Internal) |> Typ.fresh,
              ])
              |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            )
            |> Typ.fresh,
          )
          |> Exp.fresh,
          [
            Deferral(InAp) |> Exp.fresh,
            Deferral(InAp) |> Exp.fresh,
            Cast(
              Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
              Atom(Int) |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            )
            |> Exp.fresh,
          ],
        )
        |> Exp.fresh,
        Cast(
          Tuple([
            Atom(Float(1.)) |> Exp.fresh,
            Atom(Bool(true)) |> Exp.fresh,
          ])
          |> Exp.fresh,
          Prod([Atom(Float) |> Typ.fresh, Atom(Bool) |> Typ.fresh])
          |> Typ.fresh,
          Prod([
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          ])
          |> Typ.fresh,
        )
        |> Exp.fresh,
      )
      |> Exp.fresh,
      dhexp_of_uexp(
        Ap(
          Forward,
          DeferredAp(
            EmptyHole |> Exp.fresh,
            [
              Deferral(InAp) |> Exp.fresh,
              Deferral(InAp) |> Exp.fresh,
              Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
            ],
          )
          |> Exp.fresh,
          Tuple([
            Atom(Float(1.)) |> Exp.fresh,
            Atom(Bool(true)) |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
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
    let full_labeled_tuple_program: Exp.t =
      Let(
        Cast(
          Var("add") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(
                Label("street") |> Typ.fresh,
                Atom(String) |> Typ.fresh,
              )
              |> Typ.fresh,
              TupLabel(
                Label("city") |> Typ.fresh,
                Atom(String) |> Typ.fresh,
              )
              |> Typ.fresh,
              TupLabel(
                Label("state") |> Typ.fresh,
                Atom(String) |> Typ.fresh,
              )
              |> Typ.fresh,
              TupLabel(
                Label("zipcode") |> Typ.fresh,
                Atom(Int) |> Typ.fresh,
              )
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(
          Tuple([
            Atom(String("123 Maple St")) |> Exp.fresh,
            Atom(String("Ann Arbor")) |> Exp.fresh,
            Atom(String("MI")) |> Exp.fresh,
            Atom(Int(Bigint.of_int(48103))) |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("add") |> Exp.fresh,
      )
      |> Exp.fresh;
    alco_check(
      "Labeled Tuple label introduction",
      Let(
        Var("add") |> Pat.fresh,
        Tuple([
          TupLabel(
            Label("street") |> Exp.fresh,
            Atom(String("123 Maple St")) |> Exp.fresh,
          )
          |> Exp.fresh,
          TupLabel(
            Label("city") |> Exp.fresh,
            Atom(String("Ann Arbor")) |> Exp.fresh,
          )
          |> Exp.fresh,
          TupLabel(
            Label("state") |> Exp.fresh,
            Atom(String("MI")) |> Exp.fresh,
          )
          |> Exp.fresh,
          TupLabel(
            Label("zipcode") |> Exp.fresh,
            Atom(Int(Bigint.of_int(48103))) |> Exp.fresh,
          )
          |> Exp.fresh,
        ])
        |> Exp.fresh,
        Var("add") |> Exp.fresh,
      )
      |> Exp.fresh,
      dhexp_of_uexp(full_labeled_tuple_program),
    );
  };

  let singleton_labeled_tuple = () =>
    alco_check(
      "Singleton Labeled Tuple",
      Tuple([
        TupLabel(
          Label("label") |> Exp.fresh,
          Atom(String("a string value")) |> Exp.fresh,
        )
        |> Exp.fresh,
      ])
      |> Exp.fresh,
      dhexp_of_uexp(
        Tuple([
          TupLabel(
            Label("label") |> Exp.fresh,
            Atom(String("a string value")) |> Exp.fresh,
          )
          |> Exp.fresh,
        ])
        |> Exp.fresh,
      ),
    );

  let singleton_labeled_tuple_elaborates_labels = () =>
    alco_check(
      "let x : (l=String) = \"a\" in x",
      Let(
        Var("x") |> Pat.fresh,
        Tuple([
          TupLabel(Label("l") |> Exp.fresh, Atom(String("a")) |> Exp.fresh)
          |> Exp.fresh,
        ])
        |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
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
    let rearranged_labeled_tuple_program: Exp.t =
      Let(
        Cast(
          Var("val") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("a") |> Typ.fresh, Atom(Int) |> Typ.fresh)
              |> Typ.fresh,
              TupLabel(Label("b") |> Typ.fresh, Atom(String) |> Typ.fresh)
              |> Typ.fresh,
              Atom(Float) |> Typ.fresh,
              TupLabel(Label("c") |> Typ.fresh, Atom(Bool) |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(
          Tuple([
            Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            Atom(Float(1.0)) |> Exp.fresh,
            TupLabel(
              Label("c") |> Exp.fresh,
              Atom(Bool(true)) |> Exp.fresh,
            )
            |> Exp.fresh,
            TupLabel(
              Label("b") |> Exp.fresh,
              Atom(String("a")) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("val") |> Exp.fresh,
      )
      |> Exp.fresh;
    alco_check(
      "Labeled Tuple rearrangement",
      Let(
        Var("val") |> Pat.fresh,
        Tuple([
          TupLabel(
            Label("a") |> Exp.fresh,
            Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
          )
          |> Exp.fresh,
          TupLabel(Label("b") |> Exp.fresh, Atom(String("a")) |> Exp.fresh)
          |> Exp.fresh,
          Atom(Float(1.0)) |> Exp.fresh,
          TupLabel(Label("c") |> Exp.fresh, Atom(Bool(true)) |> Exp.fresh)
          |> Exp.fresh,
        ])
        |> Exp.fresh,
        Var("val") |> Exp.fresh,
      )
      |> Exp.fresh,
      dhexp_of_uexp(rearranged_labeled_tuple_program),
    );
  };

  let tests = [
    test_case("Single integer", `Quick, single_integer),
    test_case("Empty hole", `Quick, empty_hole),
    test_case("Free variable", `Quick, free_var),
    test_case("Let expression", `Quick, let_exp),
    test_case("Inconsistent binary operation", `Quick, bin_op),
    test_case("Consistent if statement", `Quick, consistent_if),
    test_case("An unapplied function", `Quick, unapplied_function),
    test_case("Application of function on free variable", `Quick, ap_fun),
    test_case("Inconsistent case statement", `Quick, inconsistent_case),
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
    test_case("Singleton labeled tuple analysis adds label", `Quick, () =>
      alco_check(
        "Singleton labeled tuple analysis adds label",
        Let(
          Var("x") |> Pat.fresh,
          Tuple([
            TupLabel(
              Label("l") |> Exp.fresh,
              Atom(String("a")) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        dhexp_of_uexp(
          Let(
            Cast(
              Var("x") |> Pat.fresh,
              Parens(
                Prod([
                  TupLabel(
                    Label("l") |> Typ.fresh,
                    Atom(String) |> Typ.fresh,
                  )
                  |> Typ.fresh,
                ])
                |> Typ.fresh,
              )
              |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            )
            |> Pat.fresh,
            Parens(Atom(String("a")) |> Exp.fresh) |> Exp.fresh,
            Var("x") |> Exp.fresh,
          )
          |> Exp.fresh,
        ),
      )
    ),
    test_case(
      "Singleton labeled tuple analysis adds label with type alias", `Quick, () =>
      alco_check(
        {|type T = (a=String) in
        let x : T = "hello" in x|},
        Let(
          Var("x") |> Pat.fresh,
          Tuple([
            TupLabel(
              Label("a") |> Exp.fresh,
              Atom(String("hello")) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        dhexp_of_uexp(
          parse_exp({|type T = (a=String) in let x : T = "hello" in x|}),
        ),
      )
    ),
    test_case(
      "Singleton labeled tuple analysis adds label with type alias", `Quick, () =>
      alco_check(
        {|let zip_only : (zip=Int) = (zip=12345) in zip_only|},
        Let(
          Var("zip_only") |> Pat.fresh,
          Tuple([
            TupLabel(
              Label("zip") |> Exp.fresh,
              Atom(Int(Bigint.of_int(12345))) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
          Var("zip_only") |> Exp.fresh,
        )
        |> Exp.fresh,
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
        Ap(
          Forward,
          Fun(
            Tuple([
              TupLabel(Label("a") |> Pat.fresh, Var("x") |> Pat.fresh)
              |> Pat.fresh,
            ])
            |> Pat.fresh,
            Var("x") |> Exp.fresh,
            Some(
              Prod([
                TupLabel(Label("a") |> Typ.fresh, Atom(Int) |> Typ.fresh)
                |> Typ.fresh,
              ])
              |> Typ.fresh,
            ),
            None,
          )
          |> Exp.fresh,
          Tuple([
            TupLabel(
              Label("a") |> Exp.fresh,
              Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        dhexp_of_uexp(parse_exp({|(fun a=(x:Int) -> x)(a=1)|})) // Ignoring casts for now
      )
    ),
    test_case(
      "Singleton labeled argument function application with no label in ap",
      `Quick,
      () =>
      alco_check(
        {|(fun a=(x:Int) -> x)(1)|},
        Ap(
          Forward,
          Fun(
            Tuple([
              TupLabel(Label("a") |> Pat.fresh, Var("x") |> Pat.fresh)
              |> Pat.fresh,
            ])
            |> Pat.fresh,
            Var("x") |> Exp.fresh,
            Some(
              Prod([
                TupLabel(Label("a") |> Typ.fresh, Atom(Int) |> Typ.fresh)
                |> Typ.fresh,
              ])
              |> Typ.fresh,
            ),
            None,
          )
          |> Exp.fresh,
          Tuple([
            TupLabel(
              Label("a") |> Exp.fresh,
              Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        dhexp_of_uexp(parse_exp({|(fun a=(x:Int) -> x)(1)|})),
      )
    ),
    test_case("Failed cast inside labeled tuple", `Quick, () =>
      alco_check(
        {|let x : (c=String) = c=1 in x|},
        Let(
          Var("x") |> Pat.fresh,
          Tuple([
            TupLabel(
              Label("c") |> Exp.fresh,
              FailedCast(
                Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
                Atom(Int) |> Typ.fresh,
                Atom(String) |> Typ.fresh,
              )
              |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        dhexp_of_uexp(parse_exp({|let x : (c=String) = c=1 in x|})),
      )
    ),
    test_case("nested different singleton labeled arguments", `Quick, () =>
      alco_check(
        {|let x : (b=c=String) = b="" in x|},
        Let(
          Var("x") |> Pat.fresh,
          Tuple([
            TupLabel(
              Label("b") |> Exp.fresh,
              Tuple([
                TupLabel(
                  Label("c") |> Exp.fresh,
                  Atom(String("")) |> Exp.fresh,
                )
                |> Exp.fresh,
              ])
              |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        dhexp_of_uexp(parse_exp({|let x : (b=c=String) = b="" in x|})),
      )
    ),
    test_case(
      "Singleton labeled argument function application with unknown type",
      `Quick,
      () =>
      alco_check(
        {|(fun a=x->x)(a=1)|},
        Ap(
          Forward,
          Fun(
            Tuple([
              TupLabel(Label("a") |> Pat.fresh, Var("x") |> Pat.fresh)
              |> Pat.fresh,
            ])
            |> Pat.fresh,
            Var("x") |> Exp.fresh,
            Some(
              Prod([
                TupLabel(
                  Label("a") |> Typ.fresh,
                  Unknown(Internal) |> Typ.fresh,
                )
                |> Typ.fresh,
              ])
              |> Typ.fresh,
            ),
            None,
          )
          |> Exp.fresh,
          Tuple([
            TupLabel(
              Label("a") |> Exp.fresh,
              Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        DHExp.strip_casts(dhexp_of_uexp(parse_exp({|(fun a=x->x)(a=1)|}))),
      )
    ),
    test_case("Singleton labeled argument let with unknown type", `Quick, () =>
      alco_check(
        {|let x : (a=?) = (a=1) in x|},
        Let(
          Var("x") |> Pat.fresh,
          Tuple([
            TupLabel(
              Label("a") |> Exp.fresh,
              Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
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
    test_case("Does not add labels with different cardinality", `Quick, () => {
      alco_check(
        "Does not add label",
        FailedCast(
          DHExp.strip_casts(parse_exp({|(1, 2) : (a= ,b= ,  )|})),
          Prod([
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          ])
          |> Typ.fresh,
          Prod([
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          ])
          |> Typ.fresh,
        )
        |> Exp.fresh,
        DHExp.strip_casts(
          dhexp_of_uexp(parse_exp({|(1, 2) : (a= ,b= ,  )|})),
        ),
      )
    }),
    QCheck_alcotest.to_alcotest(
      QCheck.Test.make(
        ~name="Elaboration does not crash",
        ~count=1000,
        QCheck_Util.arb_exp(~minimal_idents=true, 50),
        exp => {
        switch (mk_map(exp)) {
        | statics =>
          switch (Elaborator.elaborate(statics, exp)) {
          | _ => true
          | exception (Failure(msg) as e) =>
            switch (msg) {
            | "type application in dynamics" =>
              print_endline("Known failure: " ++ Printexc.to_string(e));
              true; // https://github.com/hazelgrove/hazel/issues/1459?issue=hazelgrove%7Chazel%7C1625
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
  let alco_check_menhir = (name: string, dhexp: string, uexp: Term.Exp.t) =>
    alco_check(
      name,
      Grammar.map_exp_annotation(
        _ => IdTagged.IdTag.fresh(),
        Haz3lmenhir.Conversion.Exp.of_menhir_ast(
          Haz3lmenhir.Interface.parse_program(dhexp),
        ),
      ),
      dhexp_of_uexp(uexp),
    );

  //Test for an empty hole
  let empty_hole_str = "?";
  let empty_hole_uexp: Exp.t = {
    term: EmptyHole,
    annotation: {
      ids: [id_at(0)],
    },
  };
  let empty_hole_menhir = () =>
    alco_check_menhir("Empty hole (menhir)", empty_hole_str, empty_hole_uexp);

  //Test for a free variable
  let free_var_uexp: Exp.t = {
    term:
      Parens({
        term: Var("y"),
        annotation: {
          ids: [id_at(1)],
        },
      }),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let free_var_menhir = () =>
    alco_check_menhir(
      "Nonempty hole with free variable (menhir)",
      "y",
      dhexp_of_uexp(free_var_uexp),
    );

  //Menhir test for a binary operation
  let bin_op_uexp: Exp.t =
    BinOp(
      Int(Plus),
      Atom(Bool(false)) |> Exp.fresh,
      Var("y") |> Exp.fresh,
    )
    |> Exp.fresh;

  let bin_op_str = "false?{Bool => Int} + y{Unknown Internal => Int}";

  let bin_op_menhir = () =>
    alco_check_menhir(
      "Inconsistent binary integer operation (plus)",
      bin_op_str,
      dhexp_of_uexp(bin_op_uexp),
    );

  //Inconsistent branches menhir test
  let inconsistent_case_menhir_str = "
    case 4 == 3
    | true => 24{Int => Unknown Internal}
    | false => false{Bool => Unknown Internal}
    end
";
  let inconsistent_case_uexp: Exp.t =
    Match(
      BinOp(
        Int(Equals),
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
      )
      |> Exp.fresh,
      [
        (
          Atom(Bool(true)) |> Pat.fresh,
          Atom(Int(Bigint.of_int(24))) |> Exp.fresh,
        ),
        (Atom(Bool(false)) |> Pat.fresh, Atom(Bool(false)) |> Exp.fresh),
      ],
    )
    |> Exp.fresh;
  let inconsistent_case_menhir = () =>
    alco_check_menhir(
      "Inconsistent branches where the first branch is an integer and second branch is a boolean (menhir)",
      inconsistent_case_menhir_str,
      inconsistent_case_uexp,
    );

  //Consistent if statement menhir test
  let consistent_if_uexp: Exp.t =
    If(
      Atom(Bool(false)) |> Exp.fresh,
      Atom(Int(Bigint.of_int(8))) |> Exp.fresh,
      Atom(Int(Bigint.of_int(6))) |> Exp.fresh,
    )
    |> Exp.fresh;

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
  let single_int_uexp: Exp.t = {
    term: Atom(Int(Bigint.of_int(8))),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let single_integer_menhir = () =>
    alco_check_menhir(
      "Single integer test (menhir)",
      single_int_str,
      single_int_uexp,
    );

  //Menhir let expression test
  let let_exp_str = "let (a, b) = (4, 6) in a - b";
  let let_exp_uexp: Exp.t =
    Let(
      Tuple([Var("a") |> Pat.fresh, Var("b") |> Pat.fresh]) |> Pat.fresh,
      Tuple([
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(6))) |> Exp.fresh,
      ])
      |> Exp.fresh,
      BinOp(Int(Minus), Var("a") |> Exp.fresh, Var("b") |> Exp.fresh)
      |> Exp.fresh,
    )
    |> Exp.fresh;
  let let_exp_menhir = () =>
    alco_check_menhir(
      "Let expression for tuple (a, b) (menhir)",
      let_exp_str,
      let_exp_uexp,
    );

  let typ_ap_str = "(typfun x -> 4)@<Int>";
  let typ_ap_uexp: Exp.t =
    TypAp(
      TypFun(
        Var("x") |> TPat.fresh,
        Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        None,
      )
      |> Exp.fresh,
      Atom(Int) |> Typ.fresh,
    )
    |> Exp.fresh;
  let typ_ap_menhir = () =>
    alco_check_menhir("Type ap test (menhir)", typ_ap_str, typ_ap_uexp);

  let failed_cast_str = "1 ?{Int => String}";
  let failed_cast_uexp: Exp.t =
    FailedCast(
      Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
      Atom(Int) |> Typ.fresh,
      Atom(String) |> Typ.fresh,
    )
    |> Exp.fresh;
  let failed_cast_menhir = () =>
    alco_check_menhir(
      "Failed cast test (menhir)",
      failed_cast_str,
      failed_cast_uexp,
    );

  let constructor_str = "X/~";
  let constructor_uexp: Exp.t = Constructor("X", None) |> Exp.fresh;
  let constructor_menhir = () =>
    alco_check_menhir(
      "Constructor test (menhir)",
      constructor_str,
      constructor_uexp,
    );

  /*
   <<1 / 2 ? `a`>>
       */
  let dynamic_error_hole_str = "<<(1/0) ? `DivideByZero`>> {Unknown Internal => Int}";
  let dynamic_error_hole_uexp: Exp.t = {
    term:
      DynamicErrorHole(
        BinOp(
          Int(Divide),
          Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
          Atom(Int(Bigint.of_int(0))) |> Exp.fresh,
        )
        |> Exp.fresh,
        InvalidOperationError.DivideByZero,
      ),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let dynamic_error_hole_menhir = () =>
    alco_check_menhir(
      "Dynamic error hole (menhir)",
      dynamic_error_hole_str,
      dynamic_error_hole_uexp,
    );

  let builtin_fun_str = "infinity";
  let builtin_fun_uexp: Exp.t = {
    term: BuiltinFun("infinity"),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let builtin_fun_menhir = () =>
    alco_check_menhir(
      "Builtin function test (menhir)",
      builtin_fun_str,
      builtin_fun_uexp,
    );

  let undef_str = "undef";
  let undef_uexp: Exp.t = {
    term: Undefined,
    annotation: {
      ids: [id_at(0)],
    },
  };
  let undef_menhir = () =>
    alco_check_menhir("Undef test (menhir)", undef_str, undef_uexp);

  let test_str = "test 1 ?{Int => Bool} end";
  let test_uexp: Exp.t = {
    term: Test(Atom(Int(Bigint.of_int(1))) |> Exp.fresh),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let test_menhir = () =>
    alco_check_menhir("Test failed (menhir)", test_str, test_uexp);

  let filter_str = "eval 1 in 0";
  let stepper_filter_kind: TermBase.stepper_filter_kind_t =
    Filter({
      pat: Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
      act: (FilterAction.Eval, FilterAction.All),
    });
  let filter_uexp: Exp.t = {
    term:
      Filter(
        stepper_filter_kind,
        Atom(Int(Bigint.of_int(0))) |> Exp.fresh,
      ),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let filter_menhir = () =>
    alco_check_menhir("Filter test (menhir)", filter_str, filter_uexp);

  let undefined_str = "
undef
";
  let undefined_uexp: Exp.t = Undefined |> Exp.fresh;
  let undefined_menhir = () =>
    alco_check_menhir(
      "Undefined test (menhir)",
      undefined_str,
      undefined_uexp,
    );

  let list_exp_str = "[1, 2, 3]";
  let list_exp_uexp: Exp.t = {
    term:
      ListLit([
        Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(2))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
      ]),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let list_exp_menhir = () =>
    alco_check_menhir("List exp (menhir)", list_exp_str, list_exp_uexp);

  let invalid_str = "
?e \"x\"
";
  let invalid_uexp: Exp.t = Invalid("x") |> Exp.fresh;
  let invalid_menhir = () =>
    alco_check_menhir("Invalid test (menhir)", invalid_str, invalid_uexp);

  let ty_alias_str = "
x
";
  let ty_alias_uexp: Exp.t = {
    term:
      TyAlias(
        Var("x") |> TPat.fresh,
        Atom(Int) |> Typ.fresh,
        Var("x") |> Exp.fresh,
      ),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let ty_alias_menhir = () =>
    alco_check_menhir(
      "Type alias test (menhir)",
      ty_alias_str,
      ty_alias_uexp,
    );

  let list_concat_str = "[1, 2] @ [3, 4]";
  let list_concat_uexp: Exp.t = {
    term:
      ListConcat(
        ListLit([
          Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
          Atom(Int(Bigint.of_int(2))) |> Exp.fresh,
        ])
        |> Exp.fresh,
        ListLit([
          Atom(Int(Bigint.of_int(3))) |> Exp.fresh,
          Atom(Int(Bigint.of_int(4))) |> Exp.fresh,
        ])
        |> Exp.fresh,
      ),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let list_concat_menhir = () =>
    alco_check_menhir(
      "List concat test (menhir)",
      list_concat_str,
      list_concat_uexp,
    );

  let unop_str = "-1";
  let unop_uexp: Exp.t = {
    term: UnOp(Int(Minus), Atom(Int(Bigint.of_int(1))) |> Exp.fresh),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let unop_menhir = () =>
    alco_check_menhir("Unary operation test (menhir)", unop_str, unop_uexp);

  let seq_str = "1; 2";
  let seq_uexp: Exp.t = {
    term:
      Seq(
        Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
        Atom(Int(Bigint.of_int(2))) |> Exp.fresh,
      ),
    annotation: {
      ids: [id_at(0)],
    },
  };
  let seq_menhir = () =>
    alco_check_menhir("Sequence test (menhir)", seq_str, seq_uexp);

  let fixf_str = "fix x -> 1{Int => Unknown Internal}";
  let fixf_uexp: Exp.t = {
    term:
      FixF(
        Var("x") |> Pat.fresh,
        Atom(Int(Bigint.of_int(1))) |> Exp.fresh,
        None,
      ),
    annotation: {
      ids: [id_at(0)],
    },
  };
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
    test_case("Failed cast test (menhir)", `Quick, failed_cast_menhir),
    test_case("Constructor test (menhir)", `Quick, constructor_menhir),
    test_case("Type ap test (menhir)", `Quick, typ_ap_menhir),
    test_case("Let expression for a tuple (menhir)", `Quick, let_exp_menhir),
    test_case("Single integer (menhir)", `Quick, single_integer_menhir),
    test_case("Empty hole (menhir)", `Quick, empty_hole_menhir),
    test_case("Free var (menhir)", `Quick, free_var_menhir),
    test_case("Bin op (menhir)", `Quick, bin_op_menhir),
    test_case("Inconsistent case (menhir)", `Quick, inconsistent_case_menhir),
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
