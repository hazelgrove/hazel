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

/* Avoidance: a path rooted at a binder that goes out of scope is reduced,
   then, if still abstract, named by an enclosing signature member or
   replaced by `?`. */
let avoid_tests = {
  module F = IdTagged.FreshGrammar;
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sa = t => F.Sig.sig_type_abstract(F.TPat.var(t));
  let sg = items => F.Typ.sig_(items);
  let ti = F.Typ.int();
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
    Builtins.ctx_init(None)
    |> Ctx.extend(_, var_entry("M", abstract_sig))
    |> Ctx.extend(_, var_entry("N", manifest_sig));
  let site = Id.mk_str("avoid-tests");
  let avoid = (escaping, ty) =>
    Typ.avoid(ctx, ~escape_to=EscapesAt(site), ~escaping, ty);
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
        "an escaping abstract path becomes an escaped abstract type",
        `Quick,
        () => {
          let esc = F.Typ.escaped("M.T");
          check(typ, "bare", esc, avoid(["M"], path("M", "T")));
          check(
            typ,
            "nested",
            F.Typ.arrow(esc, ti),
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
              sg([st("T", path("M", "T")), sv("x", tv("T"))]),
            ),
          );
          check(
            typ,
            "later mentions use the member",
            sg([sa("V"), sv("w", tv("V"))]),
            avoid(
              ["M"],
              sg([st("V", path("M", "T")), sv("w", path("M", "T"))]),
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
            F.Typ.arrow(path("M", "T"), sg([sv("x", path("N", "U"))])),
          ),
        )
      ),
    ],
  );
};

/* An escaped abstract type: what avoidance leaves where a path outlived the
   binder it was rooted at. Its identity is its id, it is consistent with `?`
   and with itself and with nothing else, and it is closed. */
let escaped_tests = {
  module F = IdTagged.FreshGrammar;
  let ctx = Builtins.ctx_init(None);
  let esc = (~id, label) => F.Typ.escaped(~id, label);
  let a = esc(~id=Id.mk_str("a"), "m.T");
  let a' = esc(~id=Id.mk_str("a"), "m.T");
  let b = esc(~id=Id.mk_str("b"), "m.T");
  let ti = F.Typ.int();
  let tu = F.Typ.unknown(Internal);
  let meet = (t1, t2) => Typ.meet(ctx, t1, t2);
  let coercion = (~to_, ~from) => Typ.coercion(ctx, ~from, ~to_);
  let opt_typ = option(typ);
  (
    "Typ.Escaped",
    [
      test_case(
        "consistent with itself",
        `Quick,
        () => {
          check(opt_typ, "same id", Some(a), meet(a, a'));
          check(opt_typ, "idempotent", Some(a), meet(a, a));
        },
      ),
      test_case(
        "inconsistent with a different escape of the same path",
        `Quick,
        () => {
          check(opt_typ, "different ids", None, meet(a, b));
          check(opt_typ, "symmetric", None, meet(b, a));
        },
      ),
      test_case(
        "inconsistent with everything concrete",
        `Quick,
        () => {
          check(opt_typ, "int", None, meet(a, ti));
          check(opt_typ, "int flipped", None, meet(ti, a));
          check(
            opt_typ,
            "at an analysis position",
            None,
            coercion(~to_=ti, ~from=a),
          );
          check(
            opt_typ,
            "analysis flipped",
            None,
            coercion(~to_=a, ~from=ti),
          );
        },
      ),
      test_case(
        "consistent with unknown, which it refines",
        `Quick,
        () => {
          check(opt_typ, "unknown on the right", Some(a), meet(a, tu));
          check(opt_typ, "unknown on the left", Some(a), meet(tu, a));
          check(
            opt_typ,
            "analyzed against unknown",
            Some(a),
            coercion(~to_=tu, ~from=a),
          );
        },
      ),
      test_case(
        "closed: it names no binder",
        `Quick,
        () => {
          check(list(string), "no path roots", [], Typ.path_roots(a));
          check(list(string), "no free variables", [], Typ.free_vars(a));
          check(
            list(string),
            "nor inside an arrow",
            [],
            Typ.path_roots(F.Typ.arrow(a, b)),
          );
        },
      ),
      test_case(
        "normalization and avoidance leave it alone",
        `Quick,
        () => {
          check(typ, "normalize", a, Typ.normalize(ctx, a));
          check(typ, "whnf", a, Typ.weak_head_normalize(ctx, a));
          check(
            typ,
            "avoid",
            a,
            Typ.avoid(
              ctx,
              ~escape_to=EscapesAt(Id.mk_str("s")),
              ~escaping=["m"],
              a,
            ),
          );
        },
      ),
      test_case("prints as the path it came from, plus a tag", `Quick, () =>
        check(
          bool,
          "printed",
          true,
          String.starts_with(~prefix="m.T~", Typ.pretty_print(a)),
        )
      ),
      test_case(
        "freshening derives a new identity per site, stable per site",
        `Quick,
        () => {
          let site1 = Id.mk_str("site1");
          let site2 = Id.mk_str("site2");
          let f1 = Typ.freshen_escaped(~site=site1, a);
          let f2 = Typ.freshen_escaped(~site=site2, a);
          check(opt_typ, "not the original", None, meet(a, f1));
          check(opt_typ, "not another site", None, meet(f1, f2));
          check(
            opt_typ,
            "stable for the same site",
            Some(f1),
            meet(f1, Typ.freshen_escaped(~site=site1, a)),
          );
          check(
            bool,
            "keeps its path",
            true,
            String.starts_with(~prefix="m.T~", Typ.pretty_print(f1)),
          );
          check(
            opt_typ,
            "reaches inside a type",
            None,
            meet(
              F.Typ.arrow(ti, a),
              Typ.freshen_escaped(~site=site1, F.Typ.arrow(ti, a)),
            ),
          );
        },
      ),
      test_case(
        "a hand-written escape matches by label",
        `Quick,
        () => {
          /* Id.invalid is the wildcard a test writes, since real ids come from
             the term whose scope closed. */
          let any = F.Typ.escaped("m.T");
          check(
            opt_typ,
            "matches an id-carrying escape",
            Some(any),
            meet(any, a),
          );
          check(
            opt_typ,
            "but not a different label",
            None,
            meet(F.Typ.escaped("m.U"), a),
          );
        },
      ),
    ],
  );
};

/* Implicit module binders: `implicit S : SIG` as a component of an arrow
   domain binds S in the later components and in the codomain. */
let implicit_tests = {
  module F = IdTagged.FreshGrammar;
  let sv = (x, ty) => F.Sig.sig_let(F.Pat.asc(F.Pat.var(x), ty));
  let st = (t, ty) => F.Sig.sig_type(F.TPat.var(t), ty);
  let sa = t => F.Sig.sig_type_abstract(F.TPat.var(t));
  let sg = items => F.Typ.sig_(items);
  let ti = F.Typ.int();
  let tv = F.Typ.var;
  let path = (m, t) => F.Typ.prod_projection(F.Typ.var(m), F.Typ.label(t));
  let imp = (s, sig_) => F.Typ.implicit_(F.MPat.asc(F.MPat.var(s), sig_));
  let abstract_sig = sg([sa("T")]);
  /* (implicit s : { type T }, s.T) -> s.T */
  let shower = s =>
    F.Typ.arrow(
      F.Typ.prod([imp(s, abstract_sig), path(s, "T")]),
      path(s, "T"),
    );
  let ctx = Builtins.ctx_init(None);
  let ctx_x = Ctx.extend_alias(ctx, "X", Id.invalid, ti);
  (
    "Typ.Implicit",
    [
      test_case("meet renames the right binder to the left", `Quick, () =>
        check(
          option(typ),
          "S vs R",
          Some(shower("S")),
          Typ.meet(ctx, shower("S"), shower("R")),
        )
      ),
      test_case("meet rejects differing implicit positions", `Quick, () =>
        check(
          option(typ),
          "positions",
          None,
          Typ.meet(
            ctx,
            F.Typ.arrow(F.Typ.prod([imp("S", abstract_sig), ti]), ti),
            F.Typ.arrow(F.Typ.prod([ti, imp("S", abstract_sig)]), ti),
          ),
        )
      ),
      test_case(
        "an implicit component meets a plain type as its signature", `Quick, () =>
        check(
          option(typ),
          "sig",
          Some(sg([sv("x", ti)])),
          Typ.meet(ctx, imp("S", sg([sv("x", ti)])), sg([sv("x", ti)])),
        )
      ),
      test_case(
        "coercion: function types with implicit binders match exactly",
        `Quick,
        () => {
          /* No contravariance: a function taking a narrower binder is not
             coerced to a wider expectation, nor the reverse; only equal
             binder signatures fit (eta-expand to adapt). */
          let wide =
            F.Typ.arrow(imp("S", sg([sv("x", ti), sv("y", ti)])), ti);
          let narrow = F.Typ.arrow(imp("S", sg([sv("x", ti)])), ti);
          check(
            option(typ),
            "narrow does not fit wide",
            None,
            Typ.coercion(ctx, ~from=narrow, ~to_=wide),
          );
          check(
            option(typ),
            "wide does not fit narrow",
            None,
            Typ.coercion(ctx, ~from=wide, ~to_=narrow),
          );
          check(
            option(typ),
            "equal fits",
            Some(narrow),
            Typ.coercion(ctx, ~from=narrow, ~to_=narrow),
          );
        },
      ),
      test_case(
        "normalize resolves a manifest member through the binder", `Quick, () =>
        check(
          typ,
          "S.T becomes Int",
          F.Typ.arrow(F.Typ.prod([imp("S", sg([st("T", ti)])), ti]), ti),
          Typ.normalize(
            ctx_x,
            F.Typ.arrow(
              F.Typ.prod([
                imp("S", sg([st("T", tv("X"))])),
                path("S", "T"),
              ]),
              path("S", "T"),
            ),
          ),
        )
      ),
      test_case(
        "subst_path_root renames a root and stops at a shadowing binder",
        `Quick,
        () =>
        check(
          typ,
          "renamed",
          F.Typ.arrow(path("M", "T"), shower("S")),
          Typ.subst_path_root(
            ~from="S",
            ~to_=F.Typ.var("M"),
            F.Typ.arrow(path("S", "T"), shower("S")),
          ),
        )
      ),
      test_case(
        "avoid keeps a path rooted at the arrow's own binder", `Quick, () =>
        check(
          typ,
          "kept",
          shower("S"),
          Typ.avoid(
            ctx,
            ~escape_to=EscapesAt(Id.mk_str("implicit-tests")),
            ~escaping=["S"],
            shower("S"),
          ),
        )
      ),
      test_case(
        "path_roots and free_vars do not report the binder",
        `Quick,
        () => {
          check(list(string), "roots", [], Typ.path_roots(shower("S")));
          check(list(string), "free", [], Typ.free_vars(shower("S")));
        },
      ),
      test_case("pretty_print", `Quick, () =>
        check(
          string,
          "printed",
          "(implicit S : { type T }) -> S.T",
          Typ.pretty_print(
            F.Typ.arrow(imp("S", abstract_sig), path("S", "T")),
          ),
        )
      ),
    ],
  );
};

/* `type x = x.a in ?` used to abort the whole analysis with
   Failure("weak_head_normalize exceeded 1000 recursive calls"): TyAlias wraps a
   self-referential alias in Rec, but as_sig unrolls that Rec to look for a
   signature, which hands the projection straight back to path_sig. A cyclic
   alias has no weak head normal form, so the type must come back stuck --
   the QCheck property only catches this on seeds that happen to generate one. */
let cyclic_alias_tests = {
  module F = IdTagged.FreshGrammar;
  let cyclic =
    F.Typ.rec_(
      F.TPat.var("x"),
      F.Typ.prod_projection(F.Typ.var("x"), F.Typ.label("a")),
    );
  let ctx =
    Builtins.ctx_init(None) |> Ctx.extend_alias(_, "x", Id.invalid, cyclic);
  (
    "Typ.CyclicAlias",
    [
      test_case(
        "a cyclic alias normalizes to a stuck type instead of crashing",
        `Quick,
        () =>
        check(
          typ,
          "type x = x.a is stuck",
          cyclic,
          Typ.weak_head_normalize(ctx, F.Typ.var("x")),
        )
      ),
    ],
  );
};

let tests = [
  cyclic_alias_tests,
  escaped_tests,
  meet_tests,
  fast_equal_tests,
  sig_tests,
  cyclic_path_tests,
  coercion_tests,
  sig_paths_tests,
  avoid_tests,
  implicit_tests,
];
