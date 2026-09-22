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

/* A signature that names a same-named outer binding,
     let m = { type T = Int } in let m : { type T = m.T } = ... in ... m.T ...
   has no weak head normal form: normalizing `m.T` asks path_sig what `m`
   denotes, finds the inner `m`, whose signature defines `T` as `m.T`, and is
   back where it started. Out of fuel the type has to come back unreduced so
   statics reports a type error, instead of the whole analysis dying on
   Failure("weak_head_normalize exceeded 1000 recursive calls"). */
let cyclic_path_tests = {
  module F = IdTagged.FreshGrammar;
  let m_t = F.Typ.prod_projection(F.Typ.var("m"), F.Typ.label("T"));
  let self_sig = F.Typ.sig_([F.Sig.sig_type(F.TPat.var("T"), m_t)]);
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
        "a member path through a same-named binding normalizes without raising",
        `Quick,
        () => {
          ignore(Typ.weak_head_normalize(ctx, m_t));
          check(bool, "returned", true, true);
        },
      ),
    ],
  );
};

/* coercion: the meet, or a wider signature sealed to the one it is coerced
   to; nothing else is directional. */
let coercion_tests = {
  module F = IdTagged.FreshGrammar;
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sg = items => F.Typ.sig_(items);
  let ti = F.Typ.int();
  let tb = F.Typ.bool();
  let tu = F.Typ.unknown(Internal);
  let ctx = Builtins.ctx_init(None);
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
   normal form equal only to itself, strengthening exposes a module's own
   abstract members as such paths, and sealing at analysis positions realizes
   an abstract member by whatever the module provides. */
let sig_paths_tests = {
  module F = IdTagged.FreshGrammar;
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sa = t => F.Sig.sig_type_abstract(F.TPat.var(t));
  let sg = items => F.Typ.sig_(items);
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
    Builtins.ctx_init(None)
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
        "member projection through a path",
        `Quick,
        () => {
          let items = [sa("T"), sv("x", tv("T"))];
          check(
            opt_typ,
            "through M",
            Some(path("M", "T")),
            Typ.sig_project_value(~self=F.Typ.var("M"), items, "x"),
          );
          check(
            opt_typ,
            "no path",
            Some(tu),
            Typ.sig_project_value(items, "x"),
          );
          check(
            opt_typ,
            "local",
            Some(tv("T")),
            Typ.sig_project_value(~keep_local=_ => true, items, "x"),
          );
        },
      ),
      test_case(
        "strengthening",
        `Quick,
        () => {
          let m = F.Typ.var("M");
          let strengthened =
            sg([st("T", path("M", "T")), sv("x", tv("T"))]);
          check(
            typ,
            "exposes abstract members as paths",
            strengthened,
            Typ.strengthen(ctx, abstract_sig, ~path=m),
          );
          check(
            typ,
            "idempotent",
            strengthened,
            Typ.strengthen(
              ctx,
              Typ.strengthen(ctx, abstract_sig, ~path=m),
              ~path=m,
            ),
          );
          check(
            typ,
            "identity without abstract members",
            manifest_sig,
            Typ.strengthen(ctx, manifest_sig, ~path=m),
          );
        },
      ),
      test_case(
        "sealing at analysis positions",
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
            Typ.meet(ctx, s, sg([inner, sv("y", path("M", "T"))])),
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

let tests = [
  meet_tests,
  fast_equal_tests,
  sig_tests,
  cyclic_path_tests,
  coercion_tests,
  sig_paths_tests,
  diff_tests,
];
