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
  ],
);
