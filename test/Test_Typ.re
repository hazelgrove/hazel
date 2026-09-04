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
      test_case("meet refines an tu member", `Quick, () =>
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

let tests = [meet_tests, fast_equal_tests, sig_tests];
