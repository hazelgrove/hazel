open Alcotest;
open Language;

let typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let status_typ_tests = (
  "Info.status_typ",
  [
    test_case(
      "Typ join on polymorphic types",
      `Quick,
      () => {
        let status =
          Info.status_typ(
            Builtins.ctx_init(Some(Int)),
            TypeExpected,
            Unknown(Hole(Invalid("x"))) |> Typ.temp,
          );

        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Bad token",
          Info.InHole(Info.BadToken("x")),
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
        let status = Info.status_typ(ctx, LabelExpected(Unique, []), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Empty label",
          Info.NotInHole(Info.EmptyLabel),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Type of EmptyHole",
          Info.NotInHole(Info.Type(ty)),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Parse failure",
          Info.InHole(Info.ParseFailure),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Alias is a type",
          Info.NotInHole(
            Info.TypeAlias("A", Typ.weak_head_normalize(ctx, ty)),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Free type variable",
          Info.InHole(Info.FreeTypeVariable("X")),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Abstract type variable",
          Info.NotInHole(Info.Type(Var("T") |> Typ.temp)),
          status,
        );
      },
    ),
    /* Var(name) with LabelExpected */
    test_case(
      "Var label-alias under LabelExpected => TypeAlias",
      `Quick,
      () => {
        let ctx0 = Builtins.ctx_init(Some(Int));
        let ctx =
          Ctx.extend_alias(ctx0, "Lab", Id.invalid, Label("a") |> Typ.temp);
        let ty = Var("Lab") |> Typ.temp;
        let status = Info.status_typ(ctx, LabelExpected(Unique, []), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Label alias resolves",
          Info.NotInHole(
            Info.TypeAlias("Lab", Typ.weak_head_normalize(ctx, ty)),
          ),
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
        let status = Info.status_typ(ctx, LabelExpected(Unique, []), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "WantLabel",
          Info.InHole(Info.WantLabel),
          status,
        );
      },
    ),
    /* Var(name) with LabelProjectionExpected */
    test_case(
      "Var label-alias included in LabelProjectionExpected => TypeAlias",
      `Quick,
      () => {
        let ctx0 = Builtins.ctx_init(Some(Int));
        let ctx =
          Ctx.extend_alias(ctx0, "Lab", Id.invalid, Label("x") |> Typ.temp);
        let ty = Var("Lab") |> Typ.temp;
        let status =
          Info.status_typ(ctx, LabelProjectionExpected(["x", "y"]), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "TypeAlias for projected label",
          Info.NotInHole(
            Info.TypeAlias("Lab", Typ.weak_head_normalize(ctx, ty)),
          ),
          status,
        );
      },
    ),
    test_case(
      "Var label-alias not included in LabelProjectionExpected => InvalidLabel",
      `Quick,
      () => {
        let ctx0 = Builtins.ctx_init(Some(Int));
        let ctx =
          Ctx.extend_alias(ctx0, "Lab", Id.invalid, Label("x") |> Typ.temp);
        let ty = Var("Lab") |> Typ.temp;
        let labels = ["a", "b"];
        let status =
          Info.status_typ(ctx, LabelProjectionExpected(labels), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "InvalidLabel",
          Info.InHole(Info.InvalidLabel("x", labels)),
          status,
        );
      },
    ),
    test_case(
      "Var not alias under LabelProjectionExpected => WantLabel",
      `Quick,
      () => {
        let ctx = Builtins.ctx_init(Some(Int));
        let ty = Var("ZZ") |> Typ.temp;
        let status =
          Info.status_typ(ctx, LabelProjectionExpected(["x"]), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "WantLabel (no alias)",
          Info.InHole(Info.WantLabel),
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
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Variant unique",
          Info.NotInHole(Info.Variant("Some", sum_ty)),
          Info.status_typ(ctx, ConstructorExpected(Unique, sum_ty), ty),
        );
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Variant unique",
          Info.NotInHole(Info.Variant("Some", sum_ty)),
          Info.status_typ(ctx, VariantExpected(Unique, sum_ty), ty),
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
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Duplicate constructor",
          Info.InHole(Info.DuplicateConstructor("Some")),
          Info.status_typ(ctx, ConstructorExpected(Duplicate, sum_ty), ty),
        );
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Duplicate constructor variant",
          Info.InHole(Info.DuplicateConstructor("Some")),
          Info.status_typ(ctx, VariantExpected(Duplicate, sum_ty), ty),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Label as type",
          Info.NotInHole(Info.Type(ty)),
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
        let status = Info.status_typ(ctx, LabelExpected(Unique, []), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Label ok",
          Info.NotInHole(Info.Type(ty)),
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
          Info.status_typ(ctx, LabelExpected(Duplicate, dupes), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Duplicate label",
          Info.InHole(Info.Duplicate("a", ty)),
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
          Info.status_typ(ctx, LabelExpected(Duplicate, ["x"]), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "WantLabel when not expected duplicate",
          Info.InHole(Info.WantLabel),
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
          Info.status_typ(ctx, LabelProjectionExpected(["p", "q"]), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Projection ok",
          Info.NotInHole(Info.Type(ty)),
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
          Info.status_typ(ctx, LabelProjectionExpected(labels), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Invalid projection label",
          Info.InHole(Info.InvalidLabel("p", labels)),
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
          Info.status_typ(ctx, ConstructorExpected(Unique, sum_ty), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Label where constructor expected",
          Info.InHole(Info.WantConstructorFoundType(ty)),
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
          Info.status_typ(ctx, VariantExpected(Unique, sum_ty), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Label where variant expected",
          Info.InHole(Info.WantConstructorFoundType(ty)),
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
        let status = Info.status_typ(ctx, TypeExpected, ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Atom type ok",
          Info.NotInHole(Info.Type(ty)),
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
        let status = Info.status_typ(ctx, LabelExpected(Unique, []), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "WantLabel for non-label",
          Info.InHole(Info.WantLabel),
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
          Info.status_typ(ctx, LabelProjectionExpected(["a"]), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "WantLabel for non-label (projection)",
          Info.InHole(Info.WantLabel),
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
          Info.status_typ(ctx, ConstructorExpected(Unique, sum_ty), ty);
        check(
          testable(
            Fmt.using(Info.show_status_typ, Fmt.string),
            Info.equal_status_typ,
          ),
          "Constructor expected elsewhere",
          Info.InHole(Info.WantConstructorFoundType(ty)),
          status,
        );
      },
    ),
  ],
);

let tests = [status_typ_tests];
