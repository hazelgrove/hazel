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

let former_tests = (
  "MatchedTyp formers",
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "matches the components it builds",
      `Quick,
      () => {
        let former: MatchedTyp.former = MatchedTyp.arrow_former;
        let components = [list(int()), bool()];
        check(
          Alcotest.list(typ),
          "arrow components",
          components,
          former.parts(former.whole(components))
          |> Option.value(~default=[]),
        );
      },
    ),
    test_case(
      "label former derives the resulting type",
      `Quick,
      () => {
        let former: MatchedTyp.former = MatchedTyp.label_former;
        check(
          typ,
          "formed label",
          tup_label(label("field"), bool()),
          MatchedTyp.formed_type(
            MatchedTyp.form(former, [label("field"), bool()]),
          ),
        );
      },
    ),
    test_case(
      "sum former preserves variant structure",
      `Quick,
      () => {
        let ann_a = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_b = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let variants = [
          ConstructorMap.Variant("A", ann_a, Some(int())),
          ConstructorMap.Variant("B", ann_b, None),
          ConstructorMap.BadEntry(bool()),
        ];
        let former: MatchedTyp.former = MatchedTyp.sum_former(variants);
        check(
          typ,
          "rebuilt sum",
          sum([
            ConstructorMap.Variant("X", ann_a, Some(string())),
            ConstructorMap.Variant("Y", ann_b, None),
            ConstructorMap.BadEntry(nat()),
          ]),
          former.whole([
            var("X"),
            string(),
            var("Y"),
            Typ.gap,
            nat(),
            Typ.gap,
          ]),
        );
      },
    ),
    test_case(
      "Part requires a former",
      `Quick,
      () => {
        let id = Id.mk();
        check_raises(
          "missing Part former", Statics.Slice.Missing_former(id), () =>
          ignore(
            Statics.Slice.mk(
              ~ctx=Ctx.empty,
              ~id,
              ~ids=Id.Set.singleton(id),
              ~shape=int(),
              ~sub_terms=[(Statics.Slice.Part, Statics.Slice.opaque)],
              (),
            ),
          )
        );
      },
    ),
    test_case(
      "Prune requires a former",
      `Quick,
      () => {
        let id = Id.mk();
        check_raises(
          "missing Prune former", Statics.Slice.Missing_former(id), () =>
          ignore(
            Statics.Slice.mk(
              ~ctx=Ctx.empty,
              ~id,
              ~ids=Id.Set.singleton(id),
              ~shape=int(),
              ~sub_terms=[(Statics.Slice.Prune, Statics.Slice.opaque)],
              (),
            ),
          )
        );
      },
    ),
    test_case(
      "supply comes from the formation, not the checked shape",
      `Quick,
      () => {
        let id = Id.mk();
        let slice =
          Statics.Slice.mk(
            ~ctx=Ctx.empty,
            ~id,
            ~ids=Id.Set.singleton(id),
            ~shape=int(),
            ~formation=MatchedTyp.identity(Typ.gap),
            (),
          );
        check(typ, "unannotated supply", Typ.gap, slice.supplied);
      },
    ),
    test_case(
      "Part supplies are rebuilt by the formation",
      `Quick,
      () => {
        let leaf = ty => {
          let id = Id.mk();
          Statics.Slice.mk(
            ~ctx=Ctx.empty,
            ~id,
            ~ids=Id.Set.singleton(id),
            ~shape=ty,
            ~formation=MatchedTyp.identity(ty),
            (),
          );
        };
        let id = Id.mk();
        let slice =
          Statics.Slice.mk(
            ~ctx=Ctx.empty,
            ~id,
            ~ids=Id.Set.singleton(id),
            ~shape=list(int()),
            ~sub_terms=[
              (Statics.Slice.Part, leaf(int())),
              (Statics.Slice.Part, leaf(int())),
            ],
            ~formation=MatchedTyp.form(MatchedTyp.list_former, [int()]),
            (),
          );
        check(typ, "list supply", list(int()), slice.supplied);
      },
    ),
    test_case(
      "a constructor formation supplies its fixed shell",
      `Quick,
      () => {
        let ann_none = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let ann_some = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let shape =
          sum([
            ConstructorMap.Variant("None", ann_none, None),
            ConstructorMap.Variant("Some", ann_some, Some(int())),
          ]);
        let former =
          MatchedTyp.sum_payload_former(~shape, ~expanded=shape, "Some");
        let id = Id.mk();
        let slice =
          Statics.Slice.mk(
            ~ctx=Ctx.empty,
            ~id,
            ~ids=Id.Set.singleton(id),
            ~shape,
            ~sub_terms=[(Statics.Slice.Part, Statics.Slice.opaque)],
            ~formation=MatchedTyp.form(former, [int()]),
            (),
          );
        check(
          typ,
          "constructor shell",
          sum([
            ConstructorMap.Variant("None", ann_none, None),
            ConstructorMap.Variant("Some", ann_some, Some(Typ.gap)),
          ]),
          slice.supplied,
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
      "does not collect through a shadowing binder",
      `Quick,
      () => {
        let (_, constraints) =
          Typ.collect_constraints(
            Builtins.ctx_init(None),
            ["A"],
            typ_fun(Var("A") |> TPat.temp, var("A")),
            typ_fun(Var("B") |> TPat.temp, int()),
          );
        check(
          Alcotest.list(Alcotest.pair(Alcotest.string, typ)),
          "constraints",
          [],
          constraints,
        );
      },
    ),
  ],
);

let tests = [meet_tests, fast_equal_tests, normalize_tests, former_tests];
