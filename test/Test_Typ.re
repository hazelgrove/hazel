open Alcotest;
open Language;

let typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let meet_tests = (
  "Typ.meet",
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "Typ meet on polymorphic types",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(Some(Int)),
            Poly(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Poly(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          );
        check(
          option(testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal)),
          "Type all alpha equivalent",
          Some(
            Poly(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
          ),
          t,
        );
      },
    ),
    test_case(
      "Typ meet on product projection with fully known types",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(None),
            int(),
            prod_projection(
              prod([
                tup_label(label("a"), int()),
                tup_label(label("b"), bool()),
              ]),
              label("a"),
            ),
          );
        check(option(typ), "Meet product projections", Some(int()), t);
      },
    ),
    test_case(
      "Typ meet on product projection with unknown types",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(None),
            int(),
            prod_projection(unknown(Internal), label("a")),
          );
        check(
          option(typ),
          "Meet product projections with unknown",
          Some(int()),
          t,
        );
      },
    ),
    test_case(
      "Typ meet on product projection with unknown label",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(None),
            int(),
            prod_projection(
              prod([
                tup_label(label("a"), int()),
                tup_label(label("b"), bool()),
              ]),
              unknown(Internal),
            ),
          );
        check(
          option(typ),
          "Meet product projections with unknown label",
          Some(int()),
          t,
        );
      },
    ),
    test_case(
      "Typ meet on product extension with fully known extension types",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(None),
            prod_extension(
              prod([
                tup_label(label("a"), int()),
                bool(),
                tup_label(label("b"), float()),
              ]),
              prod([
                tup_label(label("c"), string()),
                tup_label(label("b"), bool()),
                nat(),
              ]),
            ),
            prod([
              tup_label(unknown(Internal), int()),
              unknown(Internal),
              tup_label(label("b"), unknown(Internal)),
              unknown(Internal),
              nat(),
            ]),
          );
        check(
          option(typ),
          "Meet product extensions",
          Some(
            prod([
              tup_label(label("a"), int()),
              bool(),
              tup_label(label("b"), bool()),
              tup_label(label("c"), string()),
              nat(),
            ]),
          ),
          t,
        );
      },
    ),
    test_case(
      "Typ meet on two product extensions with known extension types",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(None),
            prod_extension(
              prod([
                tup_label(label("a"), int()),
                tup_label(label("b"), bool()),
              ]),
              prod([string(), tup_label(label("b"), float())]),
            ),
            prod_extension(
              prod([tup_label(label("a"), int())]),
              prod([tup_label(label("b"), float()), string()]),
            ),
          );
        check(
          option(typ),
          "Meet product extensions",
          Some(
            prod([
              tup_label(label("a"), int()),
              tup_label(label("b"), float()),
              string(),
            ]),
          ),
          t,
        );
      },
    ),
  ],
);

let fast_equal_tests = (
  "Typ.fast_equal",
  [
    test_case(
      "Equality alpha equivalent",
      `Quick,
      () => {
        check(
          bool,
          "Poly alpha equivalent",
          true,
          Typ.fast_equal(
            Poly(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Poly(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          ),
        );
        check(
          bool,
          "Poly non alpha equivalent",
          false,
          Equality.syntactic.typ(
            Poly(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Poly(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          ),
        );
      },
    ),
  ],
);

let normalize_tests = (
  "Typ.normalize",
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "Type-level beta reduction",
      `Quick,
      () => {
        let ctx = Builtins.ctx_init(None);
        let ty =
          typ_param_ap(
            typ_fun(Var("a") |> TPat.temp, list(var("a"))),
            int(),
          );
        check(
          typ,
          "beta-normalized type application",
          list(int()),
          Typ.normalize(ctx, ty),
        );
      },
    ),
    test_case(
      "Parameterized alias application normalizes",
      `Quick,
      () => {
        let option_ty =
          typ_fun(
            Var("a") |> TPat.temp,
            sum([
              ConstructorMap.Variant(
                "None",
                ConstructorMap.empty_variant_ann,
                None,
              ),
              Variant(
                "Some",
                ConstructorMap.empty_variant_ann,
                Some(var("a")),
              ),
            ]),
          );
        let ctx =
          Ctx.extend_alias(
            Builtins.ctx_init(None),
            "Option",
            Id.invalid,
            ~typ_kind=TypKind.of_param_count(1),
            option_ty,
          );
        let normalized =
          Typ.normalize(ctx, typ_param_ap(var("Option"), int()));
        check(
          typ,
          "instantiated option",
          sum([
            ConstructorMap.Variant(
              "None",
              ConstructorMap.empty_variant_ann,
              None,
            ),
            Variant("Some", ConstructorMap.empty_variant_ann, Some(int())),
          ]),
          normalized,
        );
      },
    ),
    test_case(
      "Recursive parameterized alias exposes sum constructors",
      `Quick,
      () => {
        let list_name = "List";
        let param = Var("a") |> TPat.temp;
        let recursive_list_a = typ_param_ap(var(list_name), var("a"));
        let list_body =
          typ_fun(
            param,
            sum([
              ConstructorMap.Variant(
                "Nil",
                ConstructorMap.empty_variant_ann,
                None,
              ),
              Variant(
                "Cons",
                ConstructorMap.empty_variant_ann,
                Some(prod([var("a"), recursive_list_a])),
              ),
            ]),
          );
        let list_ty =
          Rec(Var(list_name) |> TPat.temp, list_body) |> Typ.temp;
        let ctx =
          Ctx.extend_alias(
            Builtins.ctx_init(None),
            list_name,
            Id.invalid,
            ~typ_kind=TypKind.of_param_count(1),
            list_ty,
          );
        let constructors =
          Typ.get_sum_constructors(
            ctx,
            typ_param_ap(var(list_name), int()),
          );
        check(
          Alcotest.bool,
          "recursive list constructors found",
          true,
          Option.is_some(constructors),
        );
      },
    ),
  ],
);

let structural_traversal_tests = (
  "Typ structural traversal",
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "collects only the first type layer",
      `Quick,
      () => {
        let shape = arrow(list(int()), bool());
        check(
          Alcotest.list(typ),
          "arrow children",
          [list(int()), bool()],
          Statics.Slice.typ_children(shape),
        );
      },
    ),
    test_case(
      "sum traversal includes constructors, payloads, and bad entries",
      `Quick,
      () => {
        let ann_a = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_b = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let shape =
          sum([
            ConstructorMap.Variant("A", ann_a, Some(int())),
            ConstructorMap.Variant("B", ann_b, None),
            ConstructorMap.BadEntry(bool()),
          ]);
        check(
          Alcotest.list(typ),
          "sum children",
          [var("A"), int(), var("B"), bool()],
          Statics.Slice.typ_children(shape),
        );
      },
    ),
    test_case(
      "sum rebuilding consumes a stable traversal",
      `Quick,
      () => {
        let ann_a = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_b = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let shape =
          sum([
            ConstructorMap.Variant("A", ann_a, Some(int())),
            ConstructorMap.Variant("B", ann_b, None),
            ConstructorMap.BadEntry(bool()),
          ]);
        let rebuilt =
          Statics.Slice.typ_rebuild(
            shape,
            [var("X"), string(), var("Y"), nat()],
          );
        check(
          option(typ),
          "rebuilt sum",
          Some(
            sum([
              ConstructorMap.Variant("X", ann_a, Some(string())),
              ConstructorMap.Variant("Y", ann_b, None),
              ConstructorMap.BadEntry(nat()),
            ]),
          ),
          rebuilt,
        );
      },
    ),
    test_case(
      "constructor replacement continues through the bad entry",
      `Quick,
      () => {
        let ann_a = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_b = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let shape =
          sum([
            ConstructorMap.Variant("A", ann_a, Some(int())),
            ConstructorMap.Variant("B", ann_b, None),
          ]);
        check(
          option(typ),
          "constructor replaced by gap",
          Some(
            sum([
              ConstructorMap.BadEntry(bool()),
              ConstructorMap.Variant("B", ann_b, None),
            ]),
          ),
          Statics.Slice.typ_rebuild(
            shape,
            [Statics.Slice.gap, bool(), var("B")],
          ),
        );
      },
    ),
    test_case(
      "nullary collapse preserves the following traversal slot",
      `Quick,
      () => {
        let ann = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let shape =
          sum([
            ConstructorMap.Variant("A", ann, None),
            ConstructorMap.BadEntry(bool()),
          ]);
        check(
          option(typ),
          "nullary constructor replaced by gap",
          Some(
            sum([
              ConstructorMap.BadEntry(Statics.Slice.gap),
              ConstructorMap.BadEntry(nat()),
            ]),
          ),
          Statics.Slice.typ_rebuild(shape, [Statics.Slice.gap, nat()]),
        );
      },
    ),
    test_case(
      "subtracts sum types by constructor",
      `Quick,
      () => {
        let ann_a = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_b = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let query =
          sum([
            ConstructorMap.Variant("A", ann_a, Some(int())),
            ConstructorMap.Variant("B", ann_b, Some(bool())),
          ]);
        let supplied =
          sum([
            ConstructorMap.Variant("A", ann_a, Some(int())),
            ConstructorMap.Variant("B", ann_b, Some(Typ.gap)),
          ]);
        check(
          typ,
          "residual type",
          sum([
            ConstructorMap.BadEntry(Typ.gap),
            ConstructorMap.Variant("B", ann_b, Some(bool())),
          ]),
          Typ.subtract(Builtins.ctx_init(None), query, supplied),
        );
      },
    ),
    test_case(
      "collects constraints from reordered sum variants",
      `Quick,
      () => {
        let ann_a = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_b = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let schema =
          sum([
            ConstructorMap.Variant("A", ann_a, Some(var("X"))),
            ConstructorMap.Variant("B", ann_b, Some(var("Y"))),
          ]);
        let demand =
          sum([
            ConstructorMap.Variant("B", ann_b, Some(bool())),
            ConstructorMap.Variant("A", ann_a, Some(int())),
          ]);
        let (matched, constraints) =
          Typ.collect_constraints(
            Builtins.ctx_init(None),
            ["X", "Y"],
            schema,
            demand,
          );
        check(typ, "matched schema order", schema, matched);
        check(
          Alcotest.list(Alcotest.pair(Alcotest.string, typ)),
          "constraints",
          [("X", int()), ("Y", bool())],
          constraints,
        );
      },
    ),
    test_case(
      "rebuilding rejects arity mismatches",
      `Quick,
      () => {
        let shape = arrow(int(), bool());
        check(
          option(typ),
          "too few children",
          None,
          Statics.Slice.typ_rebuild(shape, [int()]),
        );
        check(
          option(typ),
          "too many children",
          None,
          Statics.Slice.typ_rebuild(shape, [int(), bool(), string()]),
        );
      },
    ),
  ],
);

let tests = [
  meet_tests,
  fast_equal_tests,
  normalize_tests,
  structural_traversal_tests,
];
