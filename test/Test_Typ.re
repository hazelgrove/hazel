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
/* `type A = B in type B = A` bound raw: B captures A's forward reference
   and the aliases cycle. Statics never binds so (Typ.unshadow renames the
   second binder), but a cycle must still not hang normalization. */
let alias_cycle_tests = {
  let pp = Fmt.using(Typ.show, Fmt.string);
  /* which name the cap stops on depends on its parity */
  let stuck =
    testable(pp, (_, ty) =>
      switch (Typ.term_of(ty)) {
      | Var("A" | "B") => true
      | _ => false
      }
    );
  (
    "Typ alias cycles",
    [
      test_case(
        "a raw alias cycle normalizes to a stuck alias rather than throwing",
        `Quick,
        () => {
          let ctx =
            Ctx.extend_alias(Ctx.empty, "A", Id.mk(), Var("B") |> Typ.temp)
            |> Ctx.extend_alias(_, "B", Id.mk(), Var("A") |> Typ.temp);
          let var_a = Var("A") |> Typ.temp;
          let expected = Var("B") |> Typ.temp;
          check(stuck, "normalize", expected, Typ.normalize(ctx, var_a));
          /* Out of fuel, the weak head is unknown: a stuck alias would be
             re-expanded by every caller, exponentially for a chain of them. */
          check(
            bool,
            "weak_head_normalize",
            true,
            switch (Typ.term_of(Typ.weak_head_normalize(ctx, var_a))) {
            | Unknown(Internal) => true
            | _ => false
            },
          );
        },
      ),
    ],
  );
};

let unshadow_tests = {
  let abstract = (name): Ctx.tvar_entry => {
    name,
    id: Id.mk(),
    kind: Abstract,
  };
  let renamed = option(string);
  let poly = (x, body) =>
    Poly(Var(x) |> TPat.temp, body(Var(x) |> Typ.temp)) |> Typ.temp;
  let arrow = (t1, t2) => Arrow(t1, t2) |> Typ.temp;
  let var = x => Var(x) |> Typ.temp;
  (
    "Typ.unshadow",
    [
      test_case("a name not in scope binds as written", `Quick, () =>
        check(renamed, "T", None, Typ.unshadow(Ctx.empty, "T"))
      ),
      test_case("a name in scope is numbered", `Quick, () =>
        check(
          renamed,
          "T",
          Some("T#1"),
          Typ.unshadow(Ctx.extend_tvar(Ctx.empty, abstract("T")), "T"),
        )
      ),
      test_case("the number passes one in ctx", `Quick, () =>
        check(
          renamed,
          "T",
          Some("T#2"),
          Typ.unshadow(
            Ctx.extend_tvar(Ctx.empty, abstract("T"))
            |> Ctx.extend_tvar(_, abstract("T#1")),
            "T",
          ),
        )
      ),
      test_case(
        "a name an alias mentions while unbound is numbered", `Quick, () =>
        check(
          renamed,
          "T",
          Some("T#1"),
          Typ.unshadow(
            Ctx.extend_alias(Ctx.empty, "S", Id.mk(), var("T")),
            "T",
          ),
        )
      ),
      test_case(
        "Typ.rename_var renames up to a binder of the same name",
        `Quick,
        () => {
          let poly_t = poly("T", t => t);
          check(
            typ,
            "(T, U, poly T -> T)",
            Prod([var("T#1"), var("U"), poly_t]) |> Typ.temp,
            Typ.rename_var(
              "T",
              "T#1",
              Prod([var("T"), var("U"), poly_t]) |> Typ.temp,
            ),
          );
        },
      ),
      test_case("a renamed name is shown as written", `Quick, () =>
        check(
          typ,
          "poly T -> T",
          poly("T", t => t),
          Typ.surface_names(poly("T#1", t => t)),
        )
      ),
      test_case(
        "a binder sharing a written name with another is tagged with its id",
        `Quick,
        () => {
          let inner = Id.mk();
          let ty =
            poly("T", t =>
              arrow(
                t,
                Poly(
                  IdTagged.mk_internal([inner], Var("T#1"): TPat.term),
                  t,
                )
                |> Typ.temp,
              )
            );
          let shown = Typ.surface_name_in([ty]);
          check(string, "T", "T", shown("T"));
          check(string, "T#1", "T~" ++ Id.str3(inner), shown("T#1"));
        },
      ),
      test_case(
        "of two bindings with one written name, the one ctx means shows plain",
        `Quick,
        () => {
          let outer = Id.mk();
          let ctx =
            Ctx.extend_tvar(
              Ctx.empty,
              {
                name: "T",
                id: outer,
                kind: Abstract,
              },
            )
            |> Ctx.extend_tvar(_, abstract("T#1"));
          let shown = Typ.surface_name_in(~ctx, [var("T#1"), var("T")]);
          check(string, "T#1", "T", shown("T#1"));
          check(string, "T", "T~" ++ Id.str3(outer), shown("T"));
        },
      ),
      test_case(
        "a binding its name does not mean in ctx is tagged even shown alone",
        `Quick,
        () => {
          let outer = Id.mk();
          let ctx =
            Ctx.extend_tvar(
              Ctx.empty,
              {
                name: "T",
                id: outer,
                kind: Abstract,
              },
            )
            |> Ctx.extend_tvar(_, abstract("T#1"));
          check(
            string,
            "T",
            "T~" ++ Id.str3(outer),
            Typ.surface_name_in(~ctx, [var("T")], "T"),
          );
        },
      ),
      test_case(
        "a type variable is linked to its binder in the type, else in ctx",
        `Quick,
        () => {
          let outer = Id.mk();
          let inner = Id.mk();
          let ctx =
            Ctx.extend_tvar(
              Ctx.empty,
              {
                name: "T",
                id: outer,
                kind: Abstract,
              },
            )
            |> Ctx.extend_tvar(
                 _,
                 {
                   name: "B",
                   id: Id.invalid,
                   kind: Abstract,
                 },
               );
          let ty =
            Prod([
              var("T"),
              var("B"),
              Poly(
                IdTagged.mk_internal([inner], Var("U"): TPat.term),
                var("U"),
              )
              |> Typ.temp,
            ])
            |> Typ.temp;
          check(
            list(pair(string, string)),
            "name, binder",
            [("T", Id.to_string(outer)), ("U", Id.to_string(inner))],
            Typ.binding_sites_in(~ctx, [ty], ty)
            |> List.map(((name, id)) => (name, Id.to_string(id))),
          );
        },
      ),
      test_case(
        "two bindings of one name are linked apart, by the names they show as",
        `Quick,
        () => {
          let outer = Id.mk();
          let inner = Id.mk();
          let ctx =
            Ctx.extend_tvar(
              Ctx.empty,
              {
                name: "T",
                id: outer,
                kind: Abstract,
              },
            )
            |> Ctx.extend_tvar(
                 _,
                 {
                   name: "T#1",
                   id: inner,
                   kind: Abstract,
                 },
               );
          let ty = Prod([var("T"), var("T#1")]) |> Typ.temp;
          check(
            list(pair(string, string)),
            "name, binder",
            [
              ("T~" ++ Id.str3(outer), Id.to_string(outer)),
              ("T", Id.to_string(inner)),
            ],
            Typ.binding_sites_in(~ctx, [ty], ty)
            |> List.map(((name, id)) => (name, Id.to_string(id))),
          );
        },
      ),
    ],
  );
};

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
            | Some(Sig(s)) => Sig.find_value(Typ.sig_members(s), "x")
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
        "sig_members does not capture a member's free variable",
        `Quick,
        () => {
          /* `T = a` names an outer `a`; resolving `T` under `f`'s own
             binder `a` renames the binder. */
          let a = F.Typ.var("a");
          let poly = (b, body) => F.Typ.poly(F.TPat.var(b), body);
          check(
            option(typ),
            "f",
            Some(poly("b", a)),
            Sig.find_value(
              Typ.sig_members(
                Grammar.Closed(
                  ClosedSig.of_items([
                    st("T", a),
                    sv("f", poly("a", F.Typ.type_member("T"))),
                  ]),
                ),
              ),
              "f",
            ),
          );
        },
      ),
      test_case(
        "close names an earlier type member in later items by its leaf",
        `Quick,
        () => {
          let closed =
            Typ.close(ctx, sg([st("T", ti), sv("x", F.Typ.var("T"))]));
          check(
            typ,
            "closed",
            cs([st("T", ti), sv("x", F.Typ.type_member("T"))]),
            closed,
          );
          check(
            option(typ),
            "read independently, x is Int",
            Some(ti),
            switch (Typ.term_of(closed)) {
            | Sig(s) => Sig.find_value(Typ.sig_members(s), "x")
            | _ => None
            },
          );
        },
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
      /* The same signature with a member naming its sibling and with the
         member spelled out: statics compares them through the members'
         independent types, so they are one type. */
      test_case(
        "a member naming its sibling is the same type as one spelled out",
        `Quick,
        () => {
          let named =
            Typ.close(ctx, sg([st("T", ti), sv("x", F.Typ.var("T"))]));
          let spelled = cs([st("T", ti), sv("x", ti)]);
          check(
            bool,
            "equal up to aliases",
            true,
            Typ.equal_up_to_aliases(ctx, named, spelled)
            && Typ.equal_up_to_aliases(ctx, spelled, named),
          );
          check(
            bool,
            "they meet, both ways",
            true,
            Option.is_some(Typ.meet(ctx, named, spelled))
            && Option.is_some(Typ.meet(ctx, spelled, named)),
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
            cs([st("T", rec_t), sv("x", F.Typ.type_member("T"))]),
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
        "close: a nested signature's same-named type member does not make the outer one recursive",
        `Quick,
        () => {
          let inner = sg([st("T", ti), sv("x", F.Typ.var("T"))]);
          check(
            typ,
            "closed",
            cs([
              st("T", cs([st("T", ti), sv("x", F.Typ.type_member("T"))])),
            ]),
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
          check_raises("count_unknowns", Sig.OpenSignature, () =>
            ignore(Typ.count_unknowns(s))
          );
        },
      ),
      test_case(
        "a hole item is an unknown",
        `Quick,
        () => {
          check(
            bool,
            "with a hole item",
            true,
            Typ.contains_unknown(cs([sv("x", ti), F.Sig.empty_hole()])),
          );
          check(
            bool,
            "without",
            false,
            Typ.contains_unknown(cs([sv("x", ti)])),
          );
        },
      ),
      test_case(
        "free_vars scopes an open signature's items sequentially",
        `Quick,
        () => {
          let p_t = F.Typ.prod_projection(F.Typ.var("P"), F.Typ.label("T"));
          check(
            list(string),
            "an outer path root and an outer alias; sibling-bound names are not free",
            ["P", "Q"],
            Typ.free_vars(
              sg([
                sv("x", p_t),
                F.Sig.sig_module(
                  F.MPat.asc(F.MPat.var("P"), sg([st("T", tb)])),
                ),
                sv("y", p_t),
                st("T", F.Typ.var("Q")),
                sv("z", F.Typ.var("T")),
              ]),
            ),
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
   path has no weak head normal form. Nor has a cyclic alias, which the walk
   gives up on instead of exploring once per branch. */
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
      test_case(
        "a cyclic alias through a tuple extension normalizes to unknown",
        `Quick,
        () => {
          let alias = (name, def): Ctx.entry =>
            TVarEntry({
              name,
              id: Id.invalid,
              kind: Singleton(def),
            });
          let ctx =
            Ctx.extend(
              Ctx.extend(
                Builtins.ctx_init(None),
                alias("A", F.Typ.var("B")),
              ),
              alias(
                "B",
                F.Typ.prod_extension(F.Typ.var("A"), F.Typ.var("A")),
              ),
            );
          check(
            typ,
            "unknown",
            F.Typ.unknown(Internal),
            Typ.weak_head_normalize(ctx, F.Typ.var("A")),
          );
        },
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
      case(
        "a variant's payload is substituted into",
        F.Typ.sum([
          ConstructorMap.Variant(
            "A",
            ConstructorMap.empty_variant_ann,
            Some(F.Typ.var("a")),
          ),
          ConstructorMap.Variant("B", ConstructorMap.empty_variant_ann, None),
        ]),
        F.Typ.sum([
          ConstructorMap.Variant(
            "A",
            ConstructorMap.empty_variant_ann,
            Some(t_var),
          ),
          ConstructorMap.Variant("B", ConstructorMap.empty_variant_ann, None),
        ]),
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

/* coercion: the meet, or a wider signature sealed to the one it is coerced
   to; nothing else is directional. */
let coercion_tests = {
  module F = IdTagged.FreshGrammar;
  let ctx = Builtins.ctx_init(None);
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  /* The relation takes closed signatures, as statics hands it. */
  let sg = items => Typ.close(ctx, F.Typ.sig_(items));
  let ti = F.Typ.int();
  let tb = F.Typ.bool();
  let tu = F.Typ.unknown(Internal);
  let opt_typ = option(typ);
  let coercion = (to_, from) => Typ.coercion(ctx, ~from, ~to_);
  (
    "Typ.Coercion",
    [
      test_case("wider module fits a narrower signature", `Quick, () =>
        check(
          opt_typ,
          "sealed to ana",
          Some(sg([sv("x", ti)])),
          coercion(sg([sv("x", ti)]), sg([sv("x", ti), sv("y", tb)])),
        )
      ),
      test_case("narrower module does not fit a wider signature", `Quick, () =>
        check(
          opt_typ,
          "missing member",
          None,
          coercion(sg([sv("x", ti), sv("y", tb)]), sg([sv("x", ti)])),
        )
      ),
      test_case("member types must fit", `Quick, () =>
        check(
          opt_typ,
          "wrong member type",
          None,
          coercion(sg([sv("x", ti)]), sg([sv("x", tb)])),
        )
      ),
      test_case("unknown member type refines, extras dropped", `Quick, () =>
        check(
          opt_typ,
          "refined",
          Some(sg([sv("x", tu)])),
          coercion(sg([sv("x", tu)]), sg([sv("x", ti), sv("y", tb)])),
        )
      ),
      test_case(
        "manifest type members must agree",
        `Quick,
        () => {
          check(
            opt_typ,
            "same manifest",
            Some(sg([st("T", ti), sv("x", F.Typ.var("T"))])),
            coercion(
              sg([st("T", ti), sv("x", F.Typ.var("T"))]),
              sg([st("T", ti), sv("x", F.Typ.var("T")), sv("y", tb)]),
            ),
          );
          check(
            opt_typ,
            "different manifest",
            None,
            coercion(sg([st("T", ti)]), sg([st("T", tb), sv("y", tb)])),
          );
        },
      ),
      test_case(
        "signatures never fit labeled tuples",
        `Quick,
        () => {
          let prod = F.Typ.prod([F.Typ.tup_label(F.Typ.label("x"), ti)]);
          check(
            opt_typ,
            "sig vs prod",
            None,
            coercion(sg([sv("x", ti)]), prod),
          );
          check(
            opt_typ,
            "prod vs sig",
            None,
            coercion(prod, sg([sv("x", ti)])),
          );
        },
      ),
      test_case(
        "unknown on either side",
        `Quick,
        () => {
          check(
            opt_typ,
            "? ana",
            Some(sg([sv("x", ti)])),
            coercion(tu, sg([sv("x", ti)])),
          );
          check(
            opt_typ,
            "? syn",
            Some(sg([sv("x", ti)])),
            coercion(sg([sv("x", ti)]), tu),
          );
        },
      ),
      test_case(
        "seals through tuple components",
        `Quick,
        () => {
          let narrow = sg([sv("x", ti)]);
          let wide = sg([sv("x", ti), sv("y", ti)]);
          check(
            opt_typ,
            "componentwise, holes refined",
            Some(F.Typ.prod([ti, narrow])),
            coercion(F.Typ.prod([tu, narrow]), F.Typ.prod([ti, wide])),
          );
          check(
            opt_typ,
            "arity must agree",
            None,
            coercion(F.Typ.prod([narrow]), F.Typ.prod([ti, wide])),
          );
        },
      ),
      test_case(
        "agrees with meet unless the expected type is a signature",
        `Quick,
        () => {
          let narrow = sg([sv("x", ti)]);
          let wide = sg([sv("x", ti), sv("y", ti)]);
          let pairs = [
            (ti, ti),
            (ti, tb),
            (F.Typ.list(ti), F.Typ.list(tu)),
            (F.Typ.arrow(ti, tb), F.Typ.arrow(ti, tb)),
            /* no contravariance: function types match exactly */
            (F.Typ.arrow(wide, ti), F.Typ.arrow(narrow, ti)),
            (F.Typ.arrow(narrow, ti), F.Typ.arrow(wide, ti)),
            /* no depth through lists */
            (F.Typ.list(narrow), F.Typ.list(wide)),
            (
              F.Typ.prod([F.Typ.tup_label(F.Typ.label("x"), ti)]),
              F.Typ.prod([
                F.Typ.tup_label(F.Typ.label("x"), ti),
                F.Typ.tup_label(F.Typ.label("y"), ti),
              ]),
            ),
          ];
          List.iter(
            ((a, b)) =>
              check(
                opt_typ,
                "same as meet",
                Typ.meet(ctx, a, b),
                coercion(a, b),
              ),
            pairs,
          );
        },
      ),
    ],
  );
};

/* Abstract type members and the paths that name them: `M.T` is a stuck
   normal form equal only to itself, a member read through a module path is
   typed by such a path, and sealing at a coercion site realizes an abstract
   member by whatever the module provides. */
let sig_paths_tests = {
  module F = IdTagged.FreshGrammar;
  let base_ctx = Builtins.ctx_init(None);
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sa = t => F.Sig.sig_type_abstract(F.TPat.var(t));
  let sg = items => Typ.close(base_ctx, F.Typ.sig_(items));
  let ti = F.Typ.int();
  let tu = F.Typ.unknown(Internal);
  let tv = F.Typ.var;
  let path = (m, t) => F.Typ.prod_projection(F.Typ.var(m), F.Typ.label(t));
  let abstract_sig = sg([sa("T"), sv("x", tv("T"))]);
  let manifest_sig = sg([st("T", ti), sv("x", tv("T"))]);
  let var_entry = (name, typ) =>
    Ctx.VarEntry({
      name,
      id: Id.invalid,
      typ,
      custom_statics: None,
    });
  /* M is sealed, N is transparent, S is a signature alias. */
  let ctx =
    base_ctx
    |> Ctx.extend(_, var_entry("M", abstract_sig))
    |> Ctx.extend(_, var_entry("N", manifest_sig))
    |> Ctx.extend_alias(_, "S", Id.invalid, abstract_sig);
  let opt_typ = option(typ);
  let meet = (a, b) => Typ.meet(ctx, a, b);
  let coercion = (to_, from) => Typ.coercion(ctx, ~from, ~to_);
  (
    "Typ.SigPaths",
    [
      test_case("abstract members meet exactly", `Quick, () =>
        check(
          opt_typ,
          "same",
          Some(abstract_sig),
          meet(abstract_sig, abstract_sig),
        )
      ),
      test_case(
        "abstract and manifest members do not meet",
        `Quick,
        () => {
          check(
            opt_typ,
            "abstract/manifest",
            None,
            meet(abstract_sig, manifest_sig),
          );
          check(
            opt_typ,
            "manifest/abstract",
            None,
            meet(manifest_sig, abstract_sig),
          );
        },
      ),
      test_case(
        "a path meets itself and unknown only",
        `Quick,
        () => {
          check(
            opt_typ,
            "self",
            Some(path("M", "T")),
            meet(path("M", "T"), path("M", "T")),
          );
          check(
            opt_typ,
            "unknown",
            Some(path("M", "T")),
            meet(path("M", "T"), tu),
          );
          check(opt_typ, "int", None, meet(path("M", "T"), ti));
          check(
            opt_typ,
            "other path",
            None,
            meet(path("M", "T"), path("N", "T")),
          );
          check(
            opt_typ,
            "manifest path reduces",
            Some(ti),
            meet(path("N", "T"), ti),
          );
        },
      ),
      test_case(
        "weak head normalization of paths",
        `Quick,
        () => {
          check(
            typ,
            "abstract member is stuck",
            path("M", "T"),
            Typ.weak_head_normalize(ctx, path("M", "T")),
          );
          check(
            typ,
            "manifest member reduces",
            ti,
            Typ.weak_head_normalize(ctx, path("N", "T")),
          );
          check(
            typ,
            "alias route has no path to name the member",
            tu,
            Typ.weak_head_normalize(ctx, path("S", "T")),
          );
        },
      ),
      test_case(
        "a member read through a path is typed by the path",
        `Quick,
        () => {
          let s =
            switch (Typ.term_of(abstract_sig)) {
            | Sig(s) => s
            | _ => failwith("a signature")
            };
          let x = (~self=?, ()) =>
            Sig.find_value(Typ.sig_members(~self?, s), "x");
          check(
            opt_typ,
            "through M",
            Some(path("M", "T")),
            x(~self=tv("M"), ()),
          );
          check(opt_typ, "no path", Some(tu), x());
        },
      ),
      test_case(
        "sealing at a coercion site",
        `Quick,
        () => {
          check(
            opt_typ,
            "manifest fits abstract",
            Some(abstract_sig),
            coercion(abstract_sig, manifest_sig),
          );
          check(
            opt_typ,
            "abstract does not fit manifest",
            None,
            coercion(manifest_sig, abstract_sig),
          );
          check(
            opt_typ,
            "abstract fits abstract",
            Some(abstract_sig),
            coercion(abstract_sig, abstract_sig),
          );
        },
      ),
      test_case("normalize keeps abstract members", `Quick, () =>
        check(
          typ,
          "normalized",
          abstract_sig,
          Typ.normalize(ctx, abstract_sig),
        )
      ),
      test_case(
        "sibling paths survive normalize and meet",
        `Quick,
        () => {
          let inner =
            F.Sig.sig_module(F.MPat.asc(F.MPat.var("Inner"), abstract_sig));
          let s = sg([inner, sv("y", path("Inner", "T"))]);
          check(typ, "normalized", s, Typ.normalize(ctx, s));
          check(opt_typ, "met", Some(s), Typ.meet(ctx, s, s));
          check(
            opt_typ,
            "distinct roots do not meet",
            None,
            Typ.meet(
              ctx,
              s,
              Typ.close(ctx, F.Typ.sig_([inner, sv("y", path("M", "T"))])),
            ),
          );
        },
      ),
      test_case("pretty printing", `Quick, () =>
        check(
          string,
          "printed",
          "{ type T; let x : T }",
          Typ.pretty_print(abstract_sig),
        )
      ),
    ],
  );
};

/* Avoidance: a path rooted at a binder that goes out of scope is reduced,
   then, if still abstract, named by an enclosing signature member or
   replaced by `?`. */
let avoid_tests = {
  module F = IdTagged.FreshGrammar;
  let base_ctx = Builtins.ctx_init(None);
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sa = t => F.Sig.sig_type_abstract(F.TPat.var(t));
  let sg = items => Typ.close(base_ctx, F.Typ.sig_(items));
  let ti = F.Typ.int();
  let tu = F.Typ.unknown(Internal);
  let tv = F.Typ.var;
  let path = (m, t) => F.Typ.prod_projection(F.Typ.var(m), F.Typ.label(t));
  let abstract_sig = sg([sa("T"), sv("x", tv("T"))]);
  let manifest_sig = sg([st("T", ti), sv("x", tv("T"))]);
  let var_entry = (name, typ) =>
    Ctx.VarEntry({
      name,
      id: Id.invalid,
      typ,
      custom_statics: None,
    });
  let ctx =
    base_ctx
    |> Ctx.extend(_, var_entry("M", abstract_sig))
    |> Ctx.extend(_, var_entry("N", manifest_sig));
  let avoid = (escaping, ty) => Typ.avoid(ctx, ~escaping, ty);
  /* A signature mentioning M or N is closed where they are bound. */
  let sgc = items => Typ.close(ctx, F.Typ.sig_(items));
  (
    "Typ.Avoid",
    [
      test_case(
        "identity without escaping paths",
        `Quick,
        () => {
          check(typ, "int", ti, avoid(["M"], ti));
          check(
            typ,
            "other root",
            path("M", "T"),
            avoid(["N"], path("M", "T")),
          );
          check(
            typ,
            "abstract signature",
            abstract_sig,
            avoid(["M"], abstract_sig),
          );
        },
      ),
      test_case(
        "an escaping abstract path becomes unknown",
        `Quick,
        () => {
          check(typ, "bare", tu, avoid(["M"], path("M", "T")));
          check(
            typ,
            "nested",
            F.Typ.arrow(tu, ti),
            avoid(["M"], F.Typ.arrow(path("M", "T"), ti)),
          );
        },
      ),
      test_case("an escaping manifest path reduces", `Quick, () =>
        check(typ, "reduced", ti, avoid(["N"], path("N", "T")))
      ),
      test_case(
        "a member defined as an escaping path becomes abstract",
        `Quick,
        () => {
          check(
            typ,
            "strengthened signature",
            abstract_sig,
            avoid(
              ["M"],
              sgc([st("T", path("M", "T")), sv("x", tv("T"))]),
            ),
          );
          check(
            typ,
            "later mentions use the member",
            sgc([sa("V"), sv("w", tv("V"))]),
            avoid(
              ["M"],
              sgc([st("V", path("M", "T")), sv("w", path("M", "T"))]),
            ),
          );
        },
      ),
      test_case("path roots", `Quick, () =>
        check(
          list(string),
          "roots",
          ["M", "N"],
          Typ.path_roots(
            F.Typ.arrow(
              path("M", "T"),
              F.Typ.sig_([sv("x", path("N", "U"))]),
            ),
          ),
        )
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
  alias_cycle_tests,
  unshadow_tests,
  coercion_tests,
  sig_paths_tests,
  avoid_tests,
];
