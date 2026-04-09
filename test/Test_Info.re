open Alcotest;
open Language;
open TypExpectation;

let typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let typ_status_testable =
  testable(
    Fmt.using(Statics.show_typ_status, Fmt.string),
    Statics.equal_typ_status,
  );

let derive_typ_tests = (
  "Statics.derive_typ_status",
  [
    test_case(
      "Typ meet on polymorphic types",
      `Quick,
      () => {
        let status =
          Statics.derive_typ_status(
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
        let status = Statics.derive_typ_status(ctx, LabelExpected(Unique, []), ty);
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
        check(
          typ_status_testable,
          "Alias is a type",
          ([], Some(
                Message.TypeAlias("A", Typ.weak_head_normalize(ctx, ty)),
              ),
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
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
        let status = Statics.derive_typ_status(ctx, LabelExpected(Unique, []), ty);
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
        /* Use built-in Option sum type for expects arg */
        let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
        let ty = Var("Some") |> Typ.temp;
        check(
          typ_status_testable,
          "Variant unique",
          ([], Some(Message.Variant("Some", sum_ty))),
          Statics.derive_typ_status(ctx, ConstructorExpected(Unique, sum_ty), ty),
        );
        check(
          typ_status_testable,
          "Variant unique",
          ([], Some(Message.Variant("Some", sum_ty))),
          Statics.derive_typ_status(ctx, VariantExpected(Unique, sum_ty), ty),
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
          Statics.derive_typ_status(ctx, ConstructorExpected(Duplicate, sum_ty), ty),
        );
        check(
          typ_status_testable,
          "Duplicate constructor variant",
          ([Mark.TypDuplicateConstructor("Some")], None),
          Statics.derive_typ_status(ctx, VariantExpected(Duplicate, sum_ty), ty),
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
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
        let status = Statics.derive_typ_status(ctx, LabelExpected(Unique, []), ty);
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
          Statics.derive_typ_status(ctx, LabelExpected(Duplicate, dupes), ty);
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
          Statics.derive_typ_status(ctx, LabelExpected(Duplicate, ["x"]), ty);
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
          Statics.derive_typ_status(
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
          Statics.derive_typ_status(ctx, LabelProjectionExpected(Some(labels)), ty);
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
          Statics.derive_typ_status(ctx, ConstructorExpected(Unique, sum_ty), ty);
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
          Statics.derive_typ_status(ctx, VariantExpected(Unique, sum_ty), ty);
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
        let status = Statics.derive_typ_status(ctx, TypeExpected, ty);
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
        let status = Statics.derive_typ_status(ctx, LabelExpected(Unique, []), ty);
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
          Statics.derive_typ_status(ctx, LabelProjectionExpected(Some(["a"])), ty);
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
          Statics.derive_typ_status(ctx, ConstructorExpected(Unique, sum_ty), ty);
        check(
          typ_status_testable,
          "Constructor expected elsewhere",
          ([Mark.TypWantConstructorFoundType(ty)], None),
          status,
        );
      },
    ),
  ],
);

let tests = [derive_typ_tests];
