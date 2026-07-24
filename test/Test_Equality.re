open Alcotest;

open Language;
open IdTagged.FreshGrammar;

let tests = (
  "Equality",
  [
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
    // Fixpoint unrolling tests
    test_case(
      "fixpoint constant body unrolling",
      `Quick,
      () => {
        // fix f. 42 === 42
        let fix_const = Exp.fix_f(Pat.var("f"), Exp.int(42), None);
        let just_const = Exp.int(42);
        check(
          bool,
          "fix f. 42 === 42 (with unrolling)",
          true,
          Equality.semantic.exp(fix_const, just_const),
        );
        // Should NOT be equal without unrolling enabled
        check(
          bool,
          "fix f. 42 !== 42 (without unrolling)",
          false,
          Equality.syntactic.exp(fix_const, just_const),
        );
      },
    ),
    test_case(
      "fixpoint free variable body unrolling",
      `Quick,
      () => {
        // fix f. x === x (where x is free)
        let fix_x = Exp.fix_f(Pat.var("f"), Exp.var("x"), None);
        let just_x = Exp.var("x");
        check(
          bool,
          "fix f. x === x",
          true,
          Equality.semantic.exp(fix_x, just_x),
        );
      },
    ),
    test_case(
      "fixpoint recursive function structural equality",
      `Quick,
      () => {
        // fix fact. fun n -> if n == 0 then 1 else n * fact(n-1)
        // === fix fact. fun n -> if n == 0 then 1 else n * fact(n-1)
        let factorial_body = n =>
          Exp.if_(
            Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var(n),
              Exp.int(0),
            ),
            Exp.int(1),
            Exp.bin_op(
              Operators.Int(Operators.Times),
              Exp.var(n),
              Exp.ap(
                Forward,
                Exp.var("fact"),
                Exp.bin_op(
                  Operators.Int(Operators.Minus),
                  Exp.var(n),
                  Exp.int(1),
                ),
              ),
            ),
          );
        let factorial1 =
          Exp.fix_f(
            Pat.var("fact"),
            Exp.fn(Pat.var("n"), factorial_body("n"), None, None),
            None,
          );
        let factorial2 =
          Exp.fix_f(
            Pat.var("fact"),
            Exp.fn(Pat.var("n"), factorial_body("n"), None, None),
            None,
          );
        check(
          bool,
          "factorial === factorial (structural)",
          true,
          Equality.semantic.exp(factorial1, factorial2),
        );
      },
    ),
    test_case(
      "fixpoint function one level unrolling",
      `Quick,
      () => {
        // fix sum. fun n -> if n == 0 then 0 else n + sum(n-1)
        // === fun n -> if n == 0 then 0 else n + (fix sum. ...)(n-1)
        let sum_body = sum_name =>
          Exp.fn(
            Pat.var("n"),
            Exp.if_(
              Exp.bin_op(
                Operators.Poly(Operators.Equals),
                Exp.var("n"),
                Exp.int(0),
              ),
              Exp.int(0),
              Exp.bin_op(
                Operators.Int(Operators.Plus),
                Exp.var("n"),
                Exp.ap(
                  Forward,
                  Exp.var(sum_name),
                  Exp.bin_op(
                    Operators.Int(Operators.Minus),
                    Exp.var("n"),
                    Exp.int(1),
                  ),
                ),
              ),
            ),
            None,
            None,
          );
        let fix_sum = Exp.fix_f(Pat.var("sum"), sum_body("sum"), None);
        // Unrolled version: body with sum replaced by fix_sum
        let unrolled_body =
          Exp.fn(
            Pat.var("n"),
            Exp.if_(
              Exp.bin_op(
                Operators.Poly(Operators.Equals),
                Exp.var("n"),
                Exp.int(0),
              ),
              Exp.int(0),
              Exp.bin_op(
                Operators.Int(Operators.Plus),
                Exp.var("n"),
                Exp.ap(
                  Forward,
                  fix_sum,
                  Exp.bin_op(
                    Operators.Int(Operators.Minus),
                    Exp.var("n"),
                    Exp.int(1),
                  ),
                ),
              ),
            ),
            None,
            None,
          );
        check(
          bool,
          "fix sum. (fun n -> ...) === (fun n -> ... with fix sum. ... inlined)",
          true,
          Equality.semantic.exp(fix_sum, unrolled_body),
        );
      },
    ),
    test_case(
      "fixpoint function application unrolling",
      `Quick,
      () => {
        // (fix f. fun x -> f(x))(42) === (fun x -> (fix f. fun x -> f(x))(x))(42)
        let fix_body =
          Exp.fn(
            Pat.var("x"),
            Exp.ap(Forward, Exp.var("f"), Exp.var("x")),
            None,
            None,
          );
        let fix_expr = Exp.fix_f(Pat.var("f"), fix_body, None);
        let applied = Exp.ap(Forward, fix_expr, Exp.int(42));
        let unrolled_fun =
          Exp.fn(
            Pat.var("x"),
            Exp.ap(Forward, fix_expr, Exp.var("x")),
            None,
            None,
          );
        let unrolled_applied = Exp.ap(Forward, unrolled_fun, Exp.int(42));
        check(
          bool,
          "(fix f. fun x -> f(x))(42) === (fun x -> (fix f. ...)(x))(42)",
          true,
          Equality.semantic.exp(applied, unrolled_applied),
        );
      },
    ),
    test_case(
      "fixpoint cycle detection",
      `Quick,
      () => {
        // fix f. f should not infinitely loop
        // and should not equal a free variable f
        let fix_recursive = Exp.fix_f(Pat.var("f"), Exp.var("f"), None);
        let just_f = Exp.var("f");
        check(
          bool,
          "fix f. f !== f (cycle detection prevents infinite loop)",
          false,
          Equality.semantic.exp(fix_recursive, just_f),
        );
      },
    ),
    test_case(
      "fixpoint bidirectional unrolling",
      `Quick,
      () => {
        // 42 === fix f. 42 (unroll on right side)
        let just_const = Exp.int(42);
        let fix_const = Exp.fix_f(Pat.var("f"), Exp.int(42), None);
        check(
          bool,
          "42 === fix f. 42 (bidirectional)",
          true,
          Equality.semantic.exp(just_const, fix_const),
        );
      },
    ),
    test_case(
      "fixpoint multiple unrollings",
      `Quick,
      () => {
        // fix f. g(f) === g(fix f. g(f)) === g(g(fix f. g(f)))
        let fix_body = Exp.ap(Forward, Exp.var("g"), Exp.var("f"));
        let fix_expr = Exp.fix_f(Pat.var("f"), fix_body, None);
        let once_unrolled = Exp.ap(Forward, Exp.var("g"), fix_expr);
        let twice_unrolled = Exp.ap(Forward, Exp.var("g"), once_unrolled);
        check(
          bool,
          "fix f. g(f) === g(fix f. g(f))",
          true,
          Equality.semantic.exp(fix_expr, once_unrolled),
        );
        check(
          bool,
          "fix f. g(f) === g(g(fix f. g(f)))",
          true,
          Equality.semantic.exp(fix_expr, twice_unrolled),
        );
      },
    ),
    test_case(
      "fixpoint mis-aligned nesting",
      `Quick,
      () => {
        // fix x. [[x]] should NOT equal [fix x. [[x]]]
        // This tests that fixpoint unrolling doesn't cause false positives
        let inner_list_x = Exp.list_lit([Exp.var("x")]);
        let outer_list_inner = Exp.list_lit([inner_list_x]);
        let fix_nested = Exp.fix_f(Pat.var("x"), outer_list_inner, None);
        let fix_in_list = Exp.list_lit([fix_nested]);
        check(
          bool,
          "fix x. [[x]] !== [fix x. [[x]]]",
          false,
          Equality.semantic.exp(fix_nested, fix_in_list),
        );
      },
    ),
    test_case(
      "fixpoint tuple unrolling",
      `Quick,
      () => {
        // ((fix x. 5), (fix y. 7)) === (5, 7) with unrolling
        // This checks that fixpoint unrolling works inside tuples
        let fix1 = Exp.fix_f(Pat.var("x"), Exp.int(5), None);
        let fix2 = Exp.fix_f(Pat.var("y"), Exp.int(7), None);
        let tuple_with_fixes = Exp.tuple([fix1, fix2]);
        let tuple_without_fixes = Exp.tuple([Exp.int(5), Exp.int(7)]);
        check(
          bool,
          "((fix x. 5), (fix y. 7)) === (5, 7) with unrolling",
          true,
          Equality.semantic.exp(tuple_with_fixes, tuple_without_fixes),
        );
      },
    ),
    test_case(
      "var_eq_named_fun is asymmetric",
      `Quick,
      () => {
        let id_fun = Exp.fn(Pat.var("x"), Exp.var("x"), None, Some("id"));
        let var = Exp.var("id");
        let eq =
          Equality.equality({
            ...Equality.semantic_settings,
            var_eq_named_fun: true,
          });
        check(bool, "Var(id) === fun id (named)", true, eq.exp(var, id_fun));
        check(
          bool,
          "fun id (named) !== Var(id)",
          false,
          eq.exp(id_fun, var),
        );
      },
    ),
    test_case(
      "var_eq_named_fun matches fixpoint name with + suffix",
      `Quick,
      () => {
        let fac_fun = Exp.fn(Pat.var("n"), Exp.int(0), None, Some("fac+"));
        let fix = Exp.fix_f(Pat.var("fac"), fac_fun, None);
        let var = Exp.var("fac");
        let eq =
          Equality.equality({
            ...Equality.semantic_settings,
            var_eq_named_fun: true,
          });
        check(
          bool,
          "Var(fac) === fix fac. ... (named fac+)",
          true,
          eq.exp(var, fix),
        );
      },
    ),
    test_case(
      "var_eq_named_fun matches named fn in application",
      `Quick,
      () => {
        let fac_fun = Exp.fn(Pat.var("n"), Exp.int(0), None, Some("fac+"));
        let fix = Exp.fix_f(Pat.var("fac"), fac_fun, None);
        let user_ap = Exp.ap(Forward, Exp.var("fac"), Exp.int(3));
        let eval_ap = Exp.ap(Forward, fix, Exp.int(3));
        let eq =
          Equality.equality({
            ...Equality.semantic_settings,
            var_eq_named_fun: true,
          });
        check(bool, "fac(3) === <fac>(3)", true, eq.exp(user_ap, eval_ap));
      },
    ),
    test_case(
      "fixpoint with proof_of traverses all sorts",
      `Quick,
      () => {
        // fix x. (? : proof_of x end) === (? : proof_of (fix x. ...) end)
        // This ensures fixpoint unrolling traverses all sorts including proof types
        let proof_typ_with_var = Typ.proof_of(Exp.var("x"));
        let hole_with_proof_var =
          Exp.asc(Exp.multi_hole([]), proof_typ_with_var);
        let fix_with_proof =
          Exp.fix_f(Pat.var("x"), hole_with_proof_var, None);

        // After unrolling: the proof type should contain the fixpoint
        let proof_typ_with_fix = Typ.proof_of(fix_with_proof);
        let hole_with_proof_fix =
          Exp.asc(Exp.multi_hole([]), proof_typ_with_fix);

        check(
          bool,
          "fix x. (? : proof_of x end) === (? : proof_of (fix x. ...) end)",
          true,
          Equality.semantic.exp(fix_with_proof, hole_with_proof_fix),
        );
      },
    ),
  ],
);
