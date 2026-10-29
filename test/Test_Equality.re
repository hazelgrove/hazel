open Alcotest;

open Language;
open IdTagged.FreshGrammar;

/* + A(Int) + A(String): ill-formed but reachable from surface syntax, and the
   repeated name is what made ConstructorMap.venn_regions pair the variants up
   in reverse (Hashtbl.add/find_opt are LIFO), so the sum compared unequal to a
   copy of itself. */
let dup_ctr_sum = () =>
  Typ.sum([
    ConstructorMap.Variant(
      "A",
      ConstructorMap.empty_variant_ann,
      Some(Typ.int()),
    ),
    ConstructorMap.Variant(
      "A",
      ConstructorMap.empty_variant_ann,
      Some(Typ.string()),
    ),
  ]);

/* A structurally identical but physically distinct value, via a sexp round
   trip. Equality bugs of the venn_regions kind only show up against a copy:
   comparing a value with itself can short-circuit on physical equality. */
let copy_of = (t: Language.Typ.t): Language.Typ.t =>
  t
  |> Language.Typ.sexp_of_t
  |> Sexplib.Sexp.to_string
  |> Sexplib.Sexp.of_string
  |> Language.Typ.t_of_sexp;

let ctx = Builtins.ctx_init(None);

/* Type equality and normalization had no property coverage: Test_Equality was
   unit tests only, and Statics.Properties either checks for absence of crashes
   or gathers statistics without asserting. The venn_regions bug — a sum
   comparing unequal to a copy of itself, which surfaced as spurious type
   errors while typing a variant — reached the editor and was caught only
   incidentally, by the evaluator/stepper confluence property. */
let qcheck_equal_copy =
  QCheck.Test.make(
    ~name="a type equals a distinct copy of itself",
    ~count=2000,
    QCheck_Util.arb_typ(~minimal_idents=true, 15),
    t =>
    Language.Typ.fast_equal(t, copy_of(t))
  );

/* The user-visible face of the same property: a type must meet itself. */
let qcheck_meet_self =
  QCheck.Test.make(
    ~name="a type meets a distinct copy of itself",
    ~count=2000,
    QCheck_Util.arb_typ(~minimal_idents=true, 15),
    t =>
    Option.is_some(Language.Typ.meet(ctx, t, copy_of(t)))
  );

/* venn_regions paired by hashtable order, so asymmetry is the shape of bug
   worth pinning, not just the one instance of it. */
let qcheck_symmetric =
  QCheck.Test.make(
    ~name="type equality is symmetric",
    ~count=2000,
    QCheck.pair(
      QCheck_Util.arb_typ(~minimal_idents=true, 12),
      QCheck_Util.arb_typ(~minimal_idents=true, 12),
    ),
    ((a, b)) =>
    Language.Typ.fast_equal(a, b) == Language.Typ.fast_equal(b, a)
  );

/* StaticsBase.fresh_ascription skips normalization when the two types are
   already equal. That is only sound if equal types normalize equally. */
let qcheck_normalize_preserves_equality =
  QCheck.Test.make(
    ~name="equal types normalize equally",
    ~count=2000,
    QCheck_Util.arb_typ(~minimal_idents=true, 12),
    t => {
      let t' = copy_of(t);
      !Language.Typ.fast_equal(t, t')
      || Language.Typ.fast_equal(
           Language.Typ.normalize(ctx, t),
           Language.Typ.normalize(ctx, t'),
         );
    },
  );

let qcheck_normalize_idempotent =
  QCheck.Test.make(
    ~name="normalize is idempotent",
    ~count=2000,
    QCheck_Util.arb_typ(~minimal_idents=true, 12),
    t => {
      let n = Language.Typ.normalize(ctx, t);
      Language.Typ.fast_equal(n, Language.Typ.normalize(ctx, n));
    },
  );

let tests = (
  "Equality",
  [
    test_case(
      "sum with a repeated constructor name equals a copy of itself",
      `Quick,
      () => {
        check(
          bool,
          "+ A(Int) + A(String) === + A(Int) + A(String)",
          true,
          Equality.semantic.typ(dup_ctr_sum(), dup_ctr_sum()),
        );
        check(
          bool,
          "+ A(Int) + A(String) !== + A(Int)",
          false,
          Equality.semantic.typ(
            dup_ctr_sum(),
            Typ.sum([
              ConstructorMap.Variant(
                "A",
                ConstructorMap.empty_variant_ann,
                Some(Typ.int()),
              ),
            ]),
          ),
        );
      },
    ),
    test_case(
      "let alpha equivalence",
      `Quick,
      () => {
        let x1 = Exp.let_(Pat.var("x"), Exp.int(1), Exp.var("x"));
        let x2 = Exp.let_(Pat.var("x'"), Exp.int(1), Exp.var("x'"));
        check(
          bool,
          "let x = 1 in x === let x' = 1 in x'",
          true,
          Equality.semantic.exp(x1, x2),
        );
      },
    ),
    test_case(
      "forall type inequality",
      `Quick,
      () => {
        let forall_string =
          Exp.forall(
            Pat.asc(Pat.var("x"), Typ.string()),
            Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var("x"),
              Exp.var("x"),
            ),
          );
        let forall_int =
          Exp.forall(
            Pat.asc(Pat.var("x"), Typ.int()),
            Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var("x"),
              Exp.var("x"),
            ),
          );
        check(
          bool,
          "forall x : String -> x == x !== forall x : Int -> x == x",
          false,
          Equality.semantic.exp(forall_string, forall_int),
        );
      },
    ),
    test_case(
      "module item alpha equivalence (pat bindings are alpha-renamed)",
      `Quick,
      () => {
        let m1 =
          Exp.module_([
            Mod.mod_let(Pat.var("x"), Exp.int(1)),
            Mod.mod_let(Pat.var("y"), Exp.int(2)),
          ]);
        let m2 =
          Exp.module_([
            Mod.mod_let(Pat.var("a"), Exp.int(1)),
            Mod.mod_let(Pat.var("b"), Exp.int(2)),
          ]);
        /* ModLet pattern names become labels, so different names
           means different modules — no alpha-renaming. */
        check(
          bool,
          "{let x=1; let y=2} !== {let a=1; let b=2}",
          false,
          Equality.semantic.exp(m1, m2),
        );
      },
    ),
    test_case(
      "module structural equality",
      `Quick,
      () => {
        let m1 = Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]);
        let m2 = Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]);
        check(
          bool,
          "{let x=1} === {let x=1}",
          true,
          Equality.semantic.exp(m1, m2),
        );
      },
    ),
    test_case(
      "module keyword - MPat uses literal name comparison",
      `Quick,
      () => {
        let e1 =
          Exp.module_exp(
            MPat.var("M"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("M"), Exp.label("x")),
          );
        let e2 =
          Exp.module_exp(
            MPat.var("N"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("N"), Exp.label("x")),
          );
        /* MPat supports alpha-equivalence: M and N are just binders,
           so module M = ... in M.x === module N = ... in N.x */
        check(
          bool,
          "module M = {let x=1} in M.x === module N = {let x=1} in N.x",
          true,
          Equality.semantic.exp(e1, e2),
        );
      },
    ),
    test_case(
      "module keyword structural equality",
      `Quick,
      () => {
        let e1 =
          Exp.module_exp(
            MPat.var("M"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("M"), Exp.label("x")),
          );
        let e2 =
          Exp.module_exp(
            MPat.var("M"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("M"), Exp.label("x")),
          );
        check(
          bool,
          "module M = {let x=1} in M.x === module M = {let x=1} in M.x",
          true,
          Equality.semantic.exp(e1, e2),
        );
      },
    ),
    QCheck_alcotest.to_alcotest(qcheck_equal_copy),
    QCheck_alcotest.to_alcotest(qcheck_meet_self),
    QCheck_alcotest.to_alcotest(qcheck_symmetric),
    QCheck_alcotest.to_alcotest(qcheck_normalize_preserves_equality),
    QCheck_alcotest.to_alcotest(qcheck_normalize_idempotent),
  ],
);
