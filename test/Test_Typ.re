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
let testable_id = testable(Fmt.using(Id.show, Fmt.string), (==));
let diff_tests = (
  "Typ.diff",
  [
    QCheck_alcotest.to_alcotest(
      QCheck.Test.make(
        ~name="diff identity",
        ~count=1000,
        QCheck_Util.arb_typ(~minimal_idents=true, 7),
        typ =>
        Typ.diff(typ, typ) == []
      ),
    ),
    test_case(
      "diff root different atom types",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let expected = [Typ.rep_id(float_typ)];
        check(
          list(testable_id),
          "diff on different atom types",
          expected,
          Typ.diff(int_typ, float_typ),
        );
      },
    ),
    test_case(
      "diff Unknown on the dynamic side",
      `Quick,
      () => {
        /* Runtime can know less than statics -- a closure's domain reads as
           `?` -- and what it did not supply is not marked. */
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let unknown = Typ.fresh(Unknown(Internal));
        check(
          list(testable_id),
          "`?` against a concrete type marks nothing",
          [],
          Typ.diff(int_typ, unknown),
        );
        check(
          list(testable_id),
          "and nothing inside an arrow either",
          [],
          Typ.diff(
            Typ.fresh(Arrow(int_typ, int_typ)),
            Typ.fresh(Arrow(unknown, int_typ)),
          ),
        );
      },
    ),
    test_case(
      "diff arrow different codomain",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let arrow1 = Typ.fresh(Arrow(int_typ, int_typ));
        let arrow2 = Typ.fresh(Arrow(int_typ, float_typ));
        let expected = [Typ.rep_id(float_typ)];
        check(
          list(testable_id),
          "diff on arrows with different codomains",
          expected,
          Typ.diff(arrow1, arrow2),
        );
      },
    ),
    test_case(
      "diff list different element",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let list1 = Typ.fresh(List(int_typ));
        let list2 = Typ.fresh(List(float_typ));
        let expected = [Typ.rep_id(float_typ)];
        check(
          list(testable_id),
          "diff on lists with different elements",
          expected,
          Typ.diff(list1, list2),
        );
      },
    ),
    test_case(
      "diff arrow different domain",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let string_typ = Typ.fresh(Atom(Atom.String));
        let arrow1 = Typ.fresh(Arrow(int_typ, string_typ));
        let arrow2 = Typ.fresh(Arrow(float_typ, string_typ));
        let expected = [Typ.rep_id(float_typ)];
        check(
          list(testable_id),
          "diff on arrows with different domains",
          expected,
          Typ.diff(arrow1, arrow2),
        );
      },
    ),
    test_case(
      "(Int, a) ~ (Int, String)",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let string_typ = Typ.fresh(Atom(Atom.String));
        let var_a = Typ.fresh(Var("a"));
        let expected = [Typ.rep_id(string_typ)];
        check(
          list(testable_id),
          "diff on (Int, a) ~ (Int, String)",
          expected,
          Typ.diff(
            Typ.fresh(Prod([int_typ, var_a])),
            Typ.fresh(Prod([int_typ, string_typ])),
          ),
        );
      },
    ),
    test_case(
      "diff var different names",
      `Quick,
      () => {
        let var1 = Typ.fresh(Var("x"));
        let var2 = Typ.fresh(Var("y"));
        let expected = [Typ.rep_id(var2)];
        check(
          list(testable_id),
          "diff on vars with different names",
          expected,
          Typ.diff(var1, var2),
        );
      },
    ),
    test_case(
      "Recursive types with same tpat and type",
      `Quick,
      () => {
        let tpat_x = TPat.fresh(Var("x"));
        let var_x = Typ.fresh(Var("x"));
        let rec1 = Typ.fresh(Rec(tpat_x, var_x));
        let rec2 = Typ.fresh(Rec(tpat_x, var_x));
        let expected = [];
        check(
          list(testable_id),
          "diff on recursive types with same tpats",
          expected,
          Typ.diff(rec1, rec2),
        );
      },
    ),
    test_case(
      "Recursive types with different tpats",
      `Quick,
      () => {
        let rec1 =
          Typ.fresh(Rec(TPat.fresh(Var("x")), Typ.fresh(Var("x"))));
        let tpat_y = TPat.fresh(Var("y"));
        let var_y = Typ.fresh(Var("y"));
        let rec2 = Typ.fresh(Rec(tpat_y, var_y));

        let expected = [
          TPat.rep_id(tpat_y),
          Typ.rep_id(var_y),
          Typ.rep_id(rec2),
        ];
        check(
          list(testable_id),
          "diff on recursive types with different tpats",
          expected,
          Typ.diff(rec1, rec2),
        );
      },
    ),
    test_case(
      "diff Var alias expanded on right side",
      `Quick,
      () => {
        let ctx =
          Ctx.extend_tvar(
            Ctx.empty,
            {
              name: "MyList",
              id: Id.mk(),
              kind: Singleton(Typ.fresh(List(Typ.fresh(Atom(Atom.Int))))),
            },
          );
        let static_typ = Typ.fresh(List(Typ.fresh(Atom(Atom.Int))));
        let dynamic_typ = Typ.fresh(Var("MyList"));
        check(
          list(testable_id),
          "alias on right expands to same type",
          [],
          Typ.diff(~ctx, static_typ, dynamic_typ),
        );
      },
    ),
    test_case(
      "diff Var alias on right with partial diff",
      `Quick,
      () => {
        let ctx =
          Ctx.extend_tvar(
            Ctx.empty,
            {
              name: "Pair",
              id: Id.mk(),
              kind:
                Singleton(
                  Typ.fresh(
                    Prod([
                      Typ.fresh(Atom(Atom.Int)),
                      Typ.fresh(Atom(Atom.Bool)),
                    ]),
                  ),
                ),
            },
          );
        let string_typ = Typ.fresh(Atom(Atom.String));
        let static_typ =
          Typ.fresh(Prod([Typ.fresh(Atom(Atom.Int)), string_typ]));
        let dynamic_typ = Typ.fresh(Var("Pair"));
        /* The Bool in Pair's expansion differs from String, so the alias
           differs -- but it prints as the single token `Pair`, so the ids
           are that node's, not the expansion's, which appear nowhere. */
        check(
          list(testable_id),
          "a differing alias marks its own token",
          [Typ.rep_id(dynamic_typ)],
          Typ.diff(~ctx, static_typ, dynamic_typ),
        );
      },
    ),
    test_case(
      "diff both sides parenthesized",
      `Quick,
      () => {
        /* Preparing for printing parenthesizes both sides. The wrapped node is
           wholly replaced, so the parens go with it. */
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let dynamic_typ = Typ.fresh(Parens(int_typ));
        check(
          list(testable_id),
          "parens around a replaced node are marked with it",
          [Typ.rep_id(dynamic_typ), Typ.rep_id(int_typ)],
          Typ.diff(Typ.fresh(Parens(Typ.fresh(Var("a")))), dynamic_typ),
        );
      },
    ),
    test_case(
      "diff terminates on a cyclic alias chain",
      `Quick,
      () => {
        /* `type A = B in type B = A` -- neither side is self-referential, so
           TyAlias wraps neither in a Rec, and nothing but expanded_aliases
           stops diff following the chain forever. */
        let a_body = Typ.fresh(Var("B"));
        let b_body = Typ.fresh(Var("A"));
        let extend = (ctx, name, kind) =>
          Ctx.extend_tvar(
            ctx,
            {
              name,
              id: Id.mk(),
              kind,
            },
          );
        let ctx =
          Ctx.empty
          |> extend(_, "A", Singleton(a_body))
          |> extend(_, "B", Singleton(b_body));
        let int_typ = Typ.fresh(Atom(Atom.Int));
        check(
          list(testable_id),
          "an unexpandable alias on the left marks the whole right side",
          [Typ.rep_id(int_typ)],
          Typ.diff(~ctx, Typ.fresh(Var("A")), int_typ),
        );
        let var_a = Typ.fresh(Var("A"));
        check(
          list(testable_id),
          "on the right, the alias token's own id",
          [Typ.rep_id(var_a)],
          Typ.diff(~ctx, Typ.fresh(List(int_typ)), var_a),
        );
      },
    ),
    test_case(
      "diff terminates on a cyclic alias chain through parens",
      `Quick,
      () => {
        /* `type A = (B) in type B = (A)` -- the same cycle with a node that
           carries no meaning of its own in the way. */
        let extend = (ctx, name, kind) =>
          Ctx.extend_tvar(
            ctx,
            {
              name,
              id: Id.mk(),
              kind,
            },
          );
        let ctx =
          Ctx.empty
          |> extend(
               _,
               "A",
               Singleton(Typ.fresh(Parens(Typ.fresh(Var("B"))))),
             )
          |> extend(
               _,
               "B",
               Singleton(Typ.fresh(Parens(Typ.fresh(Var("A"))))),
             );
        let int_typ = Typ.fresh(Atom(Atom.Int));
        check(
          list(testable_id),
          "an unexpandable alias on the left marks the whole right side",
          [Typ.rep_id(int_typ)],
          Typ.diff(~ctx, Typ.fresh(Var("A")), int_typ),
        );
      },
    ),
    test_case(
      "diff Sum missing constructor in dynamic",
      `Quick,
      () => {
        let ann = ConstructorMap.empty_variant_ann;
        let static_typ =
          Typ.fresh(
            Sum([
              ConstructorMap.Variant("None", ann, None),
              ConstructorMap.Variant(
                "Some",
                ann,
                Some(Typ.fresh(Atom(Atom.Int))),
              ),
            ]),
          );
        let some_int = Typ.fresh(Atom(Atom.Int));
        let dynamic_typ =
          Typ.fresh(
            Sum([ConstructorMap.Variant("Some", ann, Some(some_int))]),
          );
        let result = Typ.diff(static_typ, dynamic_typ);
        /* A constructor missing on the right makes the whole Sum different. */
        check(
          bool,
          "missing constructor marks all dynamic IDs",
          true,
          List.length(result) > 0,
        );
      },
    ),
    test_case(
      "diff Sum extra constructor in dynamic",
      `Quick,
      () => {
        let ann = ConstructorMap.empty_variant_ann;
        let ann_with_id = ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ());
        let static_typ =
          Typ.fresh(Sum([ConstructorMap.Variant("A", ann, None)]));
        let dynamic_typ =
          Typ.fresh(
            Sum([
              ConstructorMap.Variant("A", ann, None),
              ConstructorMap.Variant("B", ann_with_id, None),
            ]),
          );
        let result = Typ.diff(static_typ, dynamic_typ);
        /* B is extra on the right, so its variant_ann id is in the diff. */
        check(
          bool,
          "extra constructor produces diff",
          true,
          List.length(result) > 0,
        );
      },
    ),
    test_case(
      "diff Sum same constructors no diff",
      `Quick,
      () => {
        let ann = ConstructorMap.empty_variant_ann;
        let static_typ =
          Typ.fresh(
            Sum([
              ConstructorMap.Variant("A", ann, None),
              ConstructorMap.Variant("B", ann, None),
            ]),
          );
        let dynamic_typ =
          Typ.fresh(
            Sum([
              ConstructorMap.Variant("A", ann, None),
              ConstructorMap.Variant("B", ann, None),
            ]),
          );
        check(
          list(testable_id),
          "same constructors produce no diff",
          [],
          Typ.diff(static_typ, dynamic_typ),
        );
      },
    ),
  ],
);

module F = IdTagged.FreshGrammar;
let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
let sg = items => F.Typ.sig_(items);
/* A signature already closed, as Typ.close leaves one. */
let cs = items => Typ.temp(Sig(Closed(ClosedSig.of_items(items))));

/* Signature types: consistency is exact (same member names), normalization
   keeps the Sig constructor, and member projection substitutes the
   signature's own type members. */
let sig_tests = {
  let ti = F.Typ.int();
  let tb = F.Typ.bool();
  let tu = F.Typ.unknown(Internal);
  let ctx = Builtins.ctx_init(None);
  let opt_typ = option(typ);
  (
    "Typ.Sig",
    [
      test_case("meet of identical signatures", `Quick, () =>
        check(
          opt_typ,
          "same",
          Some(cs([sv("x", ti)])),
          Typ.meet(ctx, cs([sv("x", ti)]), cs([sv("x", ti)])),
        )
      ),
      test_case(
        "meet keeps a shadowed type member an earlier item mentions",
        `Quick,
        () => {
          let s =
            Typ.close(
              ctx,
              sg([st("T", ti), sv("x", F.Typ.var("T")), st("T", tb)]),
            );
          check(
            option(typ),
            "x projects to Int",
            Some(ti),
            switch (Option.map(Typ.term_of, Typ.meet(ctx, s, s))) {
            | Some(Sig(Closed(items))) =>
              Typ.sig_project_value(ClosedSig.items(items), "x")
            | _ => None
            },
          );
        },
      ),
      test_case("meet refines an unknown member", `Quick, () =>
        check(
          opt_typ,
          "refined",
          Some(cs([sv("x", ti)])),
          Typ.meet(ctx, cs([sv("x", ti)]), cs([sv("x", tu)])),
        )
      ),
      test_case("meet is exact: no width", `Quick, () =>
        check(
          opt_typ,
          "width rejected",
          None,
          Typ.meet(
            ctx,
            cs([sv("x", ti)]),
            cs([sv("x", ti), sv("y", tb)]),
          ),
        )
      ),
      test_case("meet rejects different member names", `Quick, () =>
        check(
          opt_typ,
          "names differ",
          None,
          Typ.meet(ctx, cs([sv("x", ti)]), cs([sv("y", ti)])),
        )
      ),
      test_case("meet matches members by name, left order", `Quick, () =>
        check(
          opt_typ,
          "reordered",
          Some(cs([sv("x", ti), sv("y", tb)])),
          Typ.meet(
            ctx,
            cs([sv("x", ti), sv("y", tb)]),
            cs([sv("y", tb), sv("x", ti)]),
          ),
        )
      ),
      test_case("meet of signatures with a type member", `Quick, () =>
        check(
          opt_typ,
          "type member",
          Some(cs([st("T", ti), sv("x", F.Typ.var("T"))])),
          Typ.meet(
            ctx,
            cs([st("T", ti), sv("x", F.Typ.var("T"))]),
            cs([st("T", ti), sv("x", F.Typ.var("T"))]),
          ),
        )
      ),
      test_case("meet rejects different manifest type members", `Quick, () =>
        check(
          opt_typ,
          "manifest differ",
          None,
          Typ.meet(ctx, cs([st("T", ti)]), cs([st("T", tb)])),
        )
      ),
      test_case(
        "signatures and labeled tuples are inconsistent",
        `Quick,
        () => {
          let prod = F.Typ.prod([F.Typ.tup_label(F.Typ.label("x"), ti)]);
          check(
            bool,
            "sig vs prod",
            false,
            Typ.is_consistent(ctx, cs([sv("x", ti)]), prod),
          );
          check(
            bool,
            "prod vs sig",
            false,
            Typ.is_consistent(ctx, prod, cs([sv("x", ti)])),
          );
          check(
            bool,
            "empty sig vs unit",
            false,
            Typ.is_consistent(ctx, cs([]), F.Typ.prod([])),
          );
        },
      ),
      test_case(
        "meet with unknown",
        `Quick,
        () => {
          check(
            opt_typ,
            "sig meet ?",
            Some(cs([sv("x", ti)])),
            Typ.meet(ctx, cs([sv("x", ti)]), tu),
          );
          check(
            opt_typ,
            "? meet sig",
            Some(cs([sv("x", ti)])),
            Typ.meet(ctx, tu, cs([sv("x", ti)])),
          );
        },
      ),
      test_case(
        "normalize keeps the Sig and expands aliases",
        `Quick,
        () => {
          let ctx = Ctx.extend_alias(ctx, "A", Id.invalid, ti);
          check(
            typ,
            "alias expanded",
            cs([sv("x", ti)]),
            Typ.normalize(ctx, cs([sv("x", F.Typ.var("A"))])),
          );
        },
      ),
      test_case(
        "close substitutes the earlier type members into later items",
        `Quick,
        () =>
        check(
          typ,
          "closed",
          cs([st("T", ti), sv("x", ti)]),
          Typ.close(ctx, sg([st("T", ti), sv("x", F.Typ.var("T"))])),
        )
      ),
      test_case(
        "close: a member before the type member means the outer alias, expanded",
        `Quick,
        () => {
          let ctx = Ctx.extend_alias(ctx, "T", Id.invalid, tb);
          check(
            typ,
            "closed",
            cs([sv("x", tb), st("T", ti)]),
            Typ.close(ctx, sg([sv("x", F.Typ.var("T")), st("T", ti)])),
          );
        },
      ),
      test_case(
        "close makes a self-mentioning type member recursive",
        `Quick,
        () => {
          let pair_t = F.Typ.prod([ti, F.Typ.var("T")]);
          let rec_t = F.Typ.rec_(F.TPat.var("T"), pair_t);
          check(
            typ,
            "closed",
            cs([st("T", rec_t), sv("x", rec_t)]),
            Typ.close(
              ctx,
              sg([st("T", pair_t), sv("x", F.Typ.var("T"))]),
            ),
          );
        },
      ),
      test_case(
        "close roots a path at an earlier module member",
        `Quick,
        () => {
          let sm = (x, ty) =>
            F.Sig.sig_module(F.MPat.asc(F.MPat.var(x), ty));
          let p_u = F.Typ.prod_projection(F.Typ.var("P"), F.Typ.label("U"));
          check(
            typ,
            "closed",
            cs([sm("P", cs([st("U", ti)])), sv("y", ti)]),
            Typ.close(
              ctx,
              sg([sm("P", sg([st("U", ti)])), sv("y", p_u)]),
            ),
          );
        },
      ),
      test_case(
        "free_vars: a closed signature's members bind nothing",
        `Quick,
        () => {
          check(
            list(string),
            "outer alias free",
            ["A"],
            Typ.free_vars(cs([sv("x", F.Typ.var("A"))])),
          );
          check(
            list(string),
            "a closed member mentioning a sibling's name means the outer one",
            ["T"],
            Typ.free_vars(cs([st("T", ti), sv("x", F.Typ.var("T"))])),
          );
          check(
            list(string),
            "closed: nothing free",
            [],
            Typ.free_vars(
              Typ.close(ctx, sg([st("T", ti), sv("x", F.Typ.var("T"))])),
            ),
          );
          let p_u = F.Typ.prod_projection(F.Typ.var("P"), F.Typ.label("U"));
          check(
            list(string),
            "~binder=`Var: path roots only",
            ["P"],
            Typ.free_vars(~binder=`Var, F.Typ.arrow(p_u, F.Typ.var("Q"))),
          );
        },
      ),
      test_case(
        "member projection: the last member of a name",
        `Quick,
        () => {
          let s = [st("T", ti), sv("x", ti), st("T", tb)];
          check(
            opt_typ,
            "type member",
            Some(tb),
            Typ.sig_project_type(s, "T"),
          );
          check(
            opt_typ,
            "value member",
            Some(ti),
            Typ.sig_project_value(s, "x"),
          );
          check(opt_typ, "missing", None, Typ.sig_project_value(s, "nope"));
        },
      ),
      test_case(
        "close: a nested signature's same-named type member does not make the outer one recursive",
        `Quick,
        () => {
          let inner = sg([st("T", ti), sv("x", F.Typ.var("T"))]);
          check(
            typ,
            "closed",
            cs([st("T", cs([st("T", ti), sv("x", ti)]))]),
            Typ.close(ctx, sg([st("T", inner)])),
          );
        },
      ),
      test_case(
        "close reads a closed signature's members alone, not in order",
        `Quick,
        () => {
          let s = cs([st("T", ti), sv("x", F.Typ.var("T"))]);
          check(typ, "x keeps the outer T", s, Typ.close(ctx, s));
        },
      ),
      test_case(
        "an operation reading an open signature's members raises",
        `Quick,
        () => {
          let s = sg([sv("x", ti)]);
          check_raises("subst", Sig.OpenSignature, () =>
            ignore(Typ.subst(ti, F.TPat.var("a"), s))
          );
          check_raises("normalize", Sig.OpenSignature, () =>
            ignore(Typ.normalize(ctx, s))
          );
          check_raises("meet", Sig.OpenSignature, () =>
            ignore(Typ.meet(ctx, s, cs([sv("x", ti)])))
          );
          check_raises("meet against a non-signature", Sig.OpenSignature, () =>
            ignore(Typ.meet(ctx, s, ti))
          );
          check_raises("has_fun", Sig.OpenSignature, () =>
            ignore(Typ.has_fun(s))
          );
          check_raises("free_vars", Sig.OpenSignature, () =>
            ignore(Typ.free_vars(s))
          );
          check_raises("count_unknowns", Sig.OpenSignature, () =>
            ignore(Typ.count_unknowns(s))
          );
        },
      ),
      test_case(
        "pretty printing",
        `Quick,
        () => {
          check(
            string,
            "as written",
            "{ let x : Int; type T = Int }",
            Typ.pretty_print(sg([sv("x", ti), st("T", ti)])),
          );
          check(
            string,
            "closed members as they are",
            "{ type T = Int; let x : Int }",
            Typ.pretty_print(cs([st("T", ti), sv("x", ti)])),
          );
        },
      ),
    ],
  );
};

/* `m.T` in the inner `m`'s own signature resolves to the inner `m`: the
   path has no weak head normal form. */
let cyclic_path_tests = {
  let m_t = F.Typ.prod_projection(F.Typ.var("m"), F.Typ.label("T"));
  let self_sig = cs([F.Sig.sig_type(F.TPat.var("T"), m_t)]);
  let ctx =
    Ctx.extend(
      Builtins.ctx_init(None),
      VarEntry({
        name: "m",
        id: Id.invalid,
        typ: self_sig,
        custom_statics: None,
      }),
    );
  (
    "Typ.CyclicPath",
    [
      test_case(
        "a member path through a same-named binding normalizes to unknown",
        `Quick,
        () =>
        check(
          typ,
          "unknown",
          F.Typ.unknown(Internal),
          Typ.weak_head_normalize(ctx, m_t),
        )
      ),
    ],
  );
};

/* [fun_in] names the signature members the function is found under. */
let fun_in_tests = {
  let ctx = Builtins.ctx_init(None);
  let ti = F.Typ.int();
  let to_int = F.Typ.arrow(ti, ti);
  let sm = (x, ty) => F.Sig.sig_module(F.MPat.asc(F.MPat.var(x), ty));
  let members = (found: option(Typ.fun_in)) =>
    Option.map((f: Typ.fun_in) => f.members, found);
  (
    "Typ.fun_in",
    [
      test_case("a type without a function has none", `Quick, () =>
        check(
          option(list(string)),
          "none",
          None,
          members(Typ.fun_in(ctx, F.Typ.prod([ti, F.Typ.list(ti)]))),
        )
      ),
      test_case("a function in a tuple is under no member", `Quick, () =>
        check(
          option(list(string)),
          "no members",
          Some([]),
          members(Typ.fun_in(ctx, F.Typ.prod([ti, to_int]))),
        )
      ),
      test_case(
        "a function in a sub-module's member is under both members", `Quick, () =>
        check(
          option(list(string)),
          "Inner.g",
          Some(["Inner", "g"]),
          members(
            Typ.fun_in(
              ctx,
              cs([sm("Inner", cs([sv("g", to_int)])), sv("x", ti)]),
            ),
          ),
        )
      ),
      test_case(
        "an alias to a function is a function",
        `Quick,
        () => {
          let ctx = Ctx.extend_alias(ctx, "F", Id.invalid, to_int);
          check(
            option(list(string)),
            "through the alias",
            Some([]),
            members(Typ.fun_in(ctx, F.Typ.var("F"))),
          );
        },
      ),
    ],
  );
};

let subst_tests = {
  let ti = F.Typ.int();
  let a = F.TPat.var("a");
  let t_var = F.Typ.var("T");
  let a_to_int = F.Typ.arrow(F.Typ.var("a"), ti);
  let t_to_int = F.Typ.arrow(t_var, ti);
  /* A closed signature's members bind nothing: [subst] substitutes into
     each item, and a free name of `s` named like a member is the outer one. */
  let case = (name, ty, expected) =>
    test_case(name, `Quick, () =>
      check(typ, "substituted", expected, Typ.subst(t_var, a, ty))
    );
  (
    "Typ.Subst",
    [
      case(
        "a member named like a free variable of s does not capture it",
        cs([st("T", ti), sv("v", a_to_int)]),
        cs([st("T", ti), sv("v", t_to_int)]),
      ),
      case(
        "a member's own definition is substituted into",
        cs([sv("v", F.Typ.var("a")), st("T", F.Typ.var("a"))]),
        cs([sv("v", t_var), st("T", t_var)]),
      ),
      case(
        "a member named like x does not shadow it",
        cs([st("a", ti), sv("v", a_to_int)]),
        cs([st("a", ti), sv("v", t_to_int)]),
      ),
      case(
        "a type binder named like x shadows it",
        F.Typ.poly(a, cs([st("T", ti), sv("v", a_to_int)])),
        F.Typ.poly(a, cs([st("T", ti), sv("v", a_to_int)])),
      ),
      case(
        "a member's signature type is substituted into",
        cs([sv("m", cs([st("T", ti), sv("v", a_to_int)]))]),
        cs([sv("m", cs([st("T", ti), sv("v", t_to_int)]))]),
      ),
      test_case(
        "subst leaves a signature it touches nothing in equal to itself",
        `Quick,
        () => {
          let s = cs([st("T", F.Typ.prod([ti, t_var]))]);
          check(typ, "unchanged", s, Typ.subst(ti, a, s));
        },
      ),
    ],
  );
};

let tests = [
  meet_tests,
  fun_in_tests,
  fast_equal_tests,
  diff_tests,
  sig_tests,
  cyclic_path_tests,
  subst_tests,
];
