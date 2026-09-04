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

/* Signature types: consistency is exact (same member names), normalization
   keeps the Sig constructor, and member projection substitutes the
   signature's own type members. */
let sig_tests = {
  module F = IdTagged.FreshGrammar;
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sg = items => F.Typ.sig_(items);
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
          Some(sg([sv("x", ti)])),
          Typ.meet(ctx, sg([sv("x", ti)]), sg([sv("x", ti)])),
        )
      ),
      test_case("meet refines an unknown member", `Quick, () =>
        check(
          opt_typ,
          "refined",
          Some(sg([sv("x", ti)])),
          Typ.meet(ctx, sg([sv("x", ti)]), sg([sv("x", tu)])),
        )
      ),
      test_case("meet is exact: no width", `Quick, () =>
        check(
          opt_typ,
          "width rejected",
          None,
          Typ.meet(
            ctx,
            sg([sv("x", ti)]),
            sg([sv("x", ti), sv("y", tb)]),
          ),
        )
      ),
      test_case("meet rejects different member names", `Quick, () =>
        check(
          opt_typ,
          "names differ",
          None,
          Typ.meet(ctx, sg([sv("x", ti)]), sg([sv("y", ti)])),
        )
      ),
      test_case("meet matches members by name, left order", `Quick, () =>
        check(
          opt_typ,
          "reordered",
          Some(sg([sv("x", ti), sv("y", tb)])),
          Typ.meet(
            ctx,
            sg([sv("x", ti), sv("y", tb)]),
            sg([sv("y", tb), sv("x", ti)]),
          ),
        )
      ),
      test_case("meet of signatures with a type member", `Quick, () =>
        check(
          opt_typ,
          "type member",
          Some(sg([st("T", ti), sv("x", F.Typ.var("T"))])),
          Typ.meet(
            ctx,
            sg([st("T", ti), sv("x", F.Typ.var("T"))]),
            sg([st("T", ti), sv("x", F.Typ.var("T"))]),
          ),
        )
      ),
      test_case("meet rejects different manifest type members", `Quick, () =>
        check(
          opt_typ,
          "manifest differ",
          None,
          Typ.meet(ctx, sg([st("T", ti)]), sg([st("T", tb)])),
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
            Typ.is_consistent(ctx, sg([sv("x", ti)]), prod),
          );
          check(
            bool,
            "prod vs sig",
            false,
            Typ.is_consistent(ctx, prod, sg([sv("x", ti)])),
          );
          check(
            bool,
            "empty sig vs unit",
            false,
            Typ.is_consistent(ctx, sg([]), F.Typ.prod([])),
          );
        },
      ),
      test_case(
        "meet with tu",
        `Quick,
        () => {
          check(
            opt_typ,
            "sig meet ?",
            Some(sg([sv("x", ti)])),
            Typ.meet(ctx, sg([sv("x", ti)]), tu),
          );
          check(
            opt_typ,
            "? meet sig",
            Some(sg([sv("x", ti)])),
            Typ.meet(ctx, tu, sg([sv("x", ti)])),
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
            sg([sv("x", ti)]),
            Typ.normalize(ctx, sg([sv("x", F.Typ.var("A"))])),
          );
          check(
            typ,
            "member alias expanded",
            sg([st("T", ti), sv("x", ti)]),
            Typ.normalize(
              ctx,
              sg([st("T", ti), sv("x", F.Typ.var("T"))]),
            ),
          );
        },
      ),
      test_case(
        "free_vars respects type member binders",
        `Quick,
        () => {
          check(
            list(string),
            "member bound",
            [],
            Typ.free_vars(sg([st("T", ti), sv("x", F.Typ.var("T"))])),
          );
          check(
            list(string),
            "outer alias free",
            ["A"],
            Typ.free_vars(sg([sv("x", F.Typ.var("A"))])),
          );
        },
      ),
      test_case(
        "member projection substitutes type members",
        `Quick,
        () => {
          let s = [st("T", ti), sv("x", F.Typ.var("T"))];
          check(
            opt_typ,
            "type member",
            Some(ti),
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
      test_case("pretty printing", `Quick, () =>
        check(
          string,
          "printed",
          "{ let x : Int; type T = Int }",
          Typ.pretty_print(sg([sv("x", ti), st("T", ti)])),
        )
      ),
    ],
  );
};

/* ana_meet: exact meet first, then signature width subtyping and
   contravariant function domains, only at analysis positions. */
let ana_meet_tests = {
  module F = IdTagged.FreshGrammar;
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sg = items => F.Typ.sig_(items);
  let ti = F.Typ.int();
  let tb = F.Typ.bool();
  let tu = F.Typ.unknown(Internal);
  let ctx = Builtins.ctx_init(None);
  let opt_typ = option(typ);
  let ana_meet = (ana, syn) => Typ.ana_meet(ctx, ~ana, ~syn);
  (
    "Typ.AnaMeet",
    [
      test_case("wider module fits a narrower signature", `Quick, () =>
        check(
          opt_typ,
          "sealed to ana",
          Some(sg([sv("x", ti)])),
          ana_meet(sg([sv("x", ti)]), sg([sv("x", ti), sv("y", tb)])),
        )
      ),
      test_case("narrower module does not fit a wider signature", `Quick, () =>
        check(
          opt_typ,
          "missing member",
          None,
          ana_meet(sg([sv("x", ti), sv("y", tb)]), sg([sv("x", ti)])),
        )
      ),
      test_case("member types must fit", `Quick, () =>
        check(
          opt_typ,
          "wrong member type",
          None,
          ana_meet(sg([sv("x", ti)]), sg([sv("x", tb)])),
        )
      ),
      test_case("unknown member type refines, extras dropped", `Quick, () =>
        check(
          opt_typ,
          "refined",
          Some(sg([sv("x", tu)])),
          ana_meet(sg([sv("x", tu)]), sg([sv("x", ti), sv("y", tb)])),
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
            ana_meet(
              sg([st("T", ti), sv("x", F.Typ.var("T"))]),
              sg([st("T", ti), sv("x", F.Typ.var("T")), sv("y", tb)]),
            ),
          );
          check(
            opt_typ,
            "different manifest",
            None,
            ana_meet(sg([st("T", ti)]), sg([st("T", tb), sv("y", tb)])),
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
            ana_meet(sg([sv("x", ti)]), prod),
          );
          check(
            opt_typ,
            "prod vs sig",
            None,
            ana_meet(prod, sg([sv("x", ti)])),
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
            ana_meet(tu, sg([sv("x", ti)])),
          );
          check(
            opt_typ,
            "? syn",
            Some(sg([sv("x", ti)])),
            ana_meet(sg([sv("x", ti)]), tu),
          );
        },
      ),
      test_case(
        "functions are contravariant in their domain",
        `Quick,
        () => {
          let narrow = sg([sv("x", ti)]);
          let wide = sg([sv("x", ti), sv("y", ti)]);
          check(
            opt_typ,
            "wide -> Int expected, narrow -> Int given",
            Some(F.Typ.arrow(wide, ti)),
            ana_meet(F.Typ.arrow(wide, ti), F.Typ.arrow(narrow, ti)),
          );
          check(
            opt_typ,
            "narrow -> Int expected, wide -> Int given",
            None,
            ana_meet(F.Typ.arrow(narrow, ti), F.Typ.arrow(wide, ti)),
          );
        },
      ),
      test_case(
        "agrees with meet away from signatures",
        `Quick,
        () => {
          let pairs = [
            (ti, ti),
            (ti, tb),
            (F.Typ.list(ti), F.Typ.list(tu)),
            (F.Typ.arrow(ti, tb), F.Typ.arrow(ti, tb)),
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
                ana_meet(a, b),
              ),
            pairs,
          );
        },
      ),
    ],
  );
};

let tests = [meet_tests, fast_equal_tests, sig_tests, ana_meet_tests];
