open Test_Statics_Prelude;
open Alcotest;
open Language;
open TypExpectation;

/* Direct-API helper for exercising `utyp_to_info_map` against a given
   `TypExpectation.t`. Relocated from the old test/Test_Info.re; these tests
   are still the most direct way to check per-typ-position status without
   threading the specific expectation through concrete syntax. */
module DeriveTypStatus = {
  let show_typ_status =
      ((marks, ok): (list(Mark.t), option(Message.ok_typ))): string =>
    "{"
    ++ [%derive.show: list(Mark.t)](marks)
    ++ ", "
    ++ [%derive.show: option(Message.ok_typ)](ok)
    ++ "}";

  let equal_typ_status =
      (
        (m1, o1): (list(Mark.t), option(Message.ok_typ)),
        (m2, o2): (list(Mark.t), option(Message.ok_typ)),
      )
      : bool =>
    m1 == m2 && o1 == o2;

  let derive = (ctx: Ctx.t, expects: TypExpectation.t, ty: Typ.t) => {
    let (_, m) =
      Statics.utyp_to_info_map(
        ~ctx,
        ~expects,
        ~ancestors=[],
        ty,
        Statics.Map.empty,
      );
    switch (Statics.Map.lookup_typ(Typ.rep_id(ty), m)) {
    | Some(info) =>
      let ok =
        switch (info.message) {
        | Some(Message.TypOk(ok)) => Some(ok)
        | _ => None
        };
      (info.marks, ok);
    | None => ([], None)
    };
  };
};

let typ_status_testable =
  testable(
    Fmt.using(DeriveTypStatus.show_typ_status, Fmt.string),
    DeriveTypStatus.equal_typ_status,
  );

let derive_typ_tests = [
  test_case(
    "Typ meet on polymorphic types",
    `Quick,
    () => {
      let status =
        DeriveTypStatus.derive(
          Builtins.ctx_init(Some(Int)),
          TypeExpected,
          Unknown(Hole(Invalid("x"))) |> Typ.temp,
        );

      check(
        typ_status_testable,
        "Bad token",
        ([Mark.BadToken("x")], None),
        status,
      );
    },
  ),
  /* Unknown(EmptyHole) */
  test_case(
    "EmptyHole under LabelExpected => EmptyLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Unknown(Hole(EmptyHole)) |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "Empty label",
        ([], Some(Message.EmptyLabel)),
        status,
      );
    },
  ),
  test_case(
    "EmptyHole under TypeExpected => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Unknown(Hole(EmptyHole)) |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Type of EmptyHole",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  /* Unknown(MultiHole) */
  test_case(
    "MultiHole => ParseFailure",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Unknown(Hole(MultiHole([]))) |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Parse failure",
        ([Mark.TypParseFailure], None),
        status,
      );
    },
  ),
  /* Var(name) with TypeExpected cases */
  test_case(
    "Var alias under TypeExpected => TypeAlias",
    `Quick,
    () => {
      let ctx0 = Builtins.ctx_init(Some(Int));
      let ctx =
        Ctx.extend_alias(ctx0, "A", Id.invalid, Atom(Int) |> Typ.temp);
      let ty = Var("A") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Alias is a type",
        (
          [],
          Some(Message.TypeAlias("A", Typ.weak_head_normalize(ctx, ty))),
        ),
        status,
      );
    },
  ),
  test_case(
    "Free type var under TypeExpected => FreeTypeVariable",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Var("X") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Free type variable",
        ([Mark.TypFreeTypeVariable("X")], None),
        status,
      );
    },
  ),
  test_case(
    "Abstract tvar under TypeExpected => Type(Var)",
    `Quick,
    () => {
      let ctx0 = Builtins.ctx_init(Some(Int));
      let ctx =
        Ctx.extend_tvar(
          ctx0,
          {
            name: "T",
            id: Id.invalid,
            kind: Abstract,
          },
        );
      let ty = Var("T") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Abstract type variable",
        ([], Some(Message.Type(Var("T") |> Typ.temp))),
        status,
      );
    },
  ),
  test_case(
    "Var non-label under LabelExpected => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Var("A") |> Typ.temp; /* not an alias in this ctx */
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "WantLabel",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  /* Var(name) with Constructor/VariantExpected */
  test_case(
    "Var under ConstructorExpected Unique => Variant(name, sum)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let ty = Var("Some") |> Typ.temp;
      check(
        typ_status_testable,
        "Variant unique",
        ([], Some(Message.Variant("Some", sum_ty))),
        DeriveTypStatus.derive(ctx, ConstructorExpected(Unique, sum_ty), ty),
      );
      check(
        typ_status_testable,
        "Variant unique",
        ([], Some(Message.Variant("Some", sum_ty))),
        DeriveTypStatus.derive(ctx, VariantExpected(Unique, sum_ty), ty),
      );
    },
  ),
  test_case(
    "Var under ConstructorExpected Duplicate => DuplicateConstructor",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let ty = Var("Some") |> Typ.temp;
      check(
        typ_status_testable,
        "Duplicate constructor",
        ([Mark.TypDuplicateConstructor("Some")], None),
        DeriveTypStatus.derive(
          ctx,
          ConstructorExpected(Duplicate, sum_ty),
          ty,
        ),
      );
      check(
        typ_status_testable,
        "Duplicate constructor variant",
        ([Mark.TypDuplicateConstructor("Some")], None),
        DeriveTypStatus.derive(ctx, VariantExpected(Duplicate, sum_ty), ty),
      );
    },
  ),
  /* Label(name) term cases */
  test_case(
    "Label under TypeExpected => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Label as type",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelExpected Unique => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "Label ok",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelExpected Duplicate (present) => Duplicate",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let dupes = ["a", "b"];
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Duplicate, dupes), ty);
      check(
        typ_status_testable,
        "Duplicate label",
        ([Mark.DuplicateLabel("a", ty)], None),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelExpected Duplicate (absent) => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Duplicate, ["x"]), ty);
      check(
        typ_status_testable,
        "WantLabel when not expected duplicate",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelProjectionExpected (included) => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("p") |> Typ.temp;
      let status =
        DeriveTypStatus.derive(
          ctx,
          LabelProjectionExpected(Some(["p", "q"])),
          ty,
        );
      check(
        typ_status_testable,
        "Projection ok",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelProjectionExpected (not included) => InvalidLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("p") |> Typ.temp;
      let labels = ["x", "y"];
      let status =
        DeriveTypStatus.derive(
          ctx,
          LabelProjectionExpected(Some(labels)),
          ty,
        );
      check(
        typ_status_testable,
        "Invalid projection label",
        ([Mark.InvalidLabel("p", labels)], None),
        status,
      );
    },
  ),
  test_case(
    "Label under ConstructorExpected => WantConstructorFoundType",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let status =
        DeriveTypStatus.derive(ctx, ConstructorExpected(Unique, sum_ty), ty);
      check(
        typ_status_testable,
        "Label where constructor expected",
        ([Mark.TypWantConstructorFoundType(ty)], None),
        status,
      );
    },
  ),
  test_case(
    "Label under VariantExpected => WantConstructorFoundType",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let status =
        DeriveTypStatus.derive(ctx, VariantExpected(Unique, sum_ty), ty);
      check(
        typ_status_testable,
        "Label where variant expected",
        ([Mark.TypWantConstructorFoundType(ty)], None),
        status,
      );
    },
  ),
  /* Default branch on other types */
  test_case(
    "Other type under TypeExpected => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Atom type ok",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Other type under LabelExpected => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "WantLabel for non-label",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  test_case(
    "Other type under LabelProjectionExpected => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let status =
        DeriveTypStatus.derive(
          ctx,
          LabelProjectionExpected(Some(["a"])),
          ty,
        );
      check(
        typ_status_testable,
        "WantLabel for non-label (projection)",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  test_case(
    "Other type under ConstructorExpected => WantConstructorFoundType",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let status =
        DeriveTypStatus.derive(ctx, ConstructorExpected(Unique, sum_ty), ty);
      check(
        typ_status_testable,
        "Constructor expected elsewhere",
        ([Mark.TypWantConstructorFoundType(ty)], None),
        status,
      );
    },
  ),
];

let tests = (
  "Statics.Types",
  [
    fully_consistent_typecheck(
      "Type alias works for typfun variable",
      {|typfun a -> fun y ->
  let x :a =  ? in
  type F = a in
  x : F|},
      Some(
        FTemp.(
          Typ.(poly(TPat.var("a"), arrow(unknown(Internal), var("a"))))
        ),
      ),
    ),
    skip_known_bug(
      "Typ.weak_head_normalize infinite recursion", // https://github.com/hazelgrove/hazel/issues/1621
      "type y = y in type ? = y in ?",
    ),
    test_case(
      // https://github.com/hazelgrove/hazel/issues/1624
      "Coverage.all_ctrs_of_typ no infinite recursion on nested non-productive Rec",
      `Quick,
      () => {
        let rec_type =
          FTemp.Typ.(
            rec_(
              FTemp.TPat.var("x"),
              rec_(FTemp.TPat.var("y"), var("x")),
            )
          );
        let mismatch =
          Marks([
            ExpectationMismatch({ana: rec_type, syn: FTemp.Typ.prod([])}),
          ]);
        annotated_tree_test(
          "fun (() : rec x -> rec y -> x) -> []",
          FTemp.Typ.(arrow(rec_type, list(unknown(Internal)))),
          FIError.Exp.(
            fn(
              FIError.Pat.(
                asc(
                  tuple(~ann=Some(mismatch), []),
                  FIError.Typ.(
                    rec_(
                      FIError.TPat.var("x"),
                      rec_(FIError.TPat.var("y"), var("x")),
                    )
                  ),
                )
              ),
              list_lit([]),
              None,
              None,
            )
          ),
        );
      },
    ),
    test_case(
      // https://github.com/hazelgrove/hazel/issues/2235
      "Coverage.all_ctrs_of_typ no infinite recursion on Rec with hole binder",
      `Quick,
      () => {
        let rec_type =
          FTemp.Typ.(
            rec_(
              FTemp.TPat.var("x"),
              rec_(FTemp.TPat.empty_hole(), var("x")),
            )
          );
        let mismatch =
          Marks([
            ExpectationMismatch({ana: rec_type, syn: FTemp.Typ.prod([])}),
          ]);
        annotated_tree_test(
          "fun (() : rec x -> rec ? -> x) -> 132032.832758",
          FTemp.Typ.(arrow(rec_type, float())),
          FIError.Exp.(
            fn(
              FIError.Pat.(
                asc(
                  tuple(~ann=Some(mismatch), []),
                  FIError.Typ.(
                    rec_(
                      FIError.TPat.var("x"),
                      rec_(FIError.TPat.empty_hole(), var("x")),
                    )
                  ),
                )
              ),
              float(132032.832758),
              None,
              None,
            )
          ),
        );
      },
    ),
    skip_known_bug(
      "all_ctrs_of_type called with a non-normalized type", // https://github.com/hazelgrove/hazel/issues/1626
      {|fun (?: (Float((+ A(Bool))))) -> ""|},
    ),
    test_case(
      "Type parse failure",
      `Quick,
      () => {
        // This was https://github.com/hazelgrove/hazel/issues/1459 which used to crash statics
        let exp = parse_exp("type x = Int(Float) in let y : x =  1");
        let s = statics(exp);

        let errors = errors(s) |> List.map(((_, ms)) => Marks(ms));

        check(
          list(testable_issue),
          "Has parse failure error",
          [Marks([Mark.TypParseFailure])],
          errors,
        );
      },
    ),
  ]
  @ derive_typ_tests,
);
