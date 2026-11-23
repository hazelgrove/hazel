open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* `algebra` steps, verified by Z3. It reads Int, Nat, Bool and String
 * expressions built from literals, variables known to be of those types,
 * the operators and some builtins (`ProofCheck.Algebra`), and refuses a
 * step it finds wrong. Any other step's equation is left as an
 * obligation, so the proof can't read plain Proven. */

let status = (name, src) => {
  let (pm, proof) = run_named(name, src);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | Proven => "Proven"
  | ProvenModulo(obs) =>
    "ProvenModulo "
    ++ String.concat(
         ~sep=", ",
         List.map(
           ~f=(ob: Obligation.t) => Obligation.show_kind(ob.kind),
           obs,
         ),
       )
  | Refuted => "Refuted"
  | Incomplete => "Incomplete"
  };
};

let differs = (name, src) => {
  let (pm, proof) = run_named(name, src);
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.AlgebraDiffers(_) => true
    | _ => false,
  );
};

/* The first theorem's proof, whose only step is `algebra true end`. */
let claim = (~prefix="", goal: string): string =>
  prefix ++ "theorem t = " ++ goal ++ " proof algebra true end in t";

let proven = (~prefix="", goal: string): bool =>
  String.equal(status("t", claim(~prefix, goal)), "Proven");

let refused = (~prefix="", goal: string): bool =>
  differs("t", claim(~prefix, goal));

/* Z3 has to read an operation as the evaluator computes it. Of
 * `expressions`, written in `mode`, the ones on which the two disagree:
 *   - where the evaluator gives a literal, Z3 has to find the expression
 *     equal to it and different from another literal (which it wouldn't,
 *     if its reading were contradictory);
 *   - where the evaluator gives none (a domain error), no step may prove
 *     the expression equal to a number.
 * With them, how many of `expressions` the evaluator gives no value. */
let disagreements =
    (~mode=Operators.default_mode, expressions: list(string))
    : (list(string), int) => {
  let ctx = Builtins.ctx_init(Some(mode));
  let verify =
    ProofCheck.Algebra.verify(
      ~statics=ProofStatics.empty,
      ~ctx=SemanticCtx.of_ctx_and_env(ctx, Builtins.env_init),
    );
  let other: Atom.t => Atom.t =
    fun
    | Int(n) => Int(Bigint.(n + one))
    | Nat(n) => Nat(Bigint.(n + one))
    | Bool(b) => Bool(!b)
    | String(s) => String(s ++ "x")
    | (SInt(_) | Float(_)) as v => v;
  let prefix =
    switch (mode) {
    | Nat => "use Nat in "
    | Int
    | SInt
    | Float => ""
    };
  let errors = ref(0);
  let disagreements =
    List.filter(
      expressions,
      ~f=src => {
        let (statics, e) =
          Statics.mk(
            CoreSettings.on,
            ctx,
            Test_Evaluator_Prelude.parse_exp(src),
          );
        switch (Evaluator.evaluate(~statics, ~env=Builtins.env_init, e)) {
        | ({term: Atom(v), _} as value, _) =>
          switch (verify(e, value), verify(e, Exp.fresh(Atom(other(v))))) {
          | (Equal, Differ(_)) => false
          | _ => true
          }
        | _
        | exception _ =>
          incr(errors);
          proven(~prefix, "(" ++ src ++ ") == 0");
        };
      },
    );
  (disagreements, errors^);
};

/* `errors` is how many of `expressions` have no value. */
let agrees = (~mode=?, ~errors=0, name: string, expressions: list(string)) =>
  test_case(name, `Quick, () =>
    check(
      pair(list(string), int),
      "disagreements, and expressions without a value",
      ([], errors),
      disagreements(~mode?, expressions),
    )
  );

/* Both signs, a zero, and quotients that are and aren't whole. */
let ints = ["(-7)", "(-6)", "(-2)", "0", "2", "3", "7"];
let few_ints = ["(-2)", "0", "3"];
let nats = ["0", "2", "3", "7"];
let bools = ["true", "false"];

/* `template` with each `A` and `B` in it replaced, over all pairs. */
let over = (values: list(string), template: string): list(string) =>
  List.concat_map(values, ~f=a =>
    List.map(values, ~f=b =>
      String.concat_map(
        template,
        ~f=
          fun
          | 'A' => a
          | 'B' => b
          | c => String.of_char(c),
      )
    )
  )
  |> List.dedup_and_sort(~compare=String.compare);

/* The values a refused step names, as source text. */
let point = (src: string): list((string, string)) => {
  let (pm, proof) = run_named("t", src);
  find_marked_sub(pm, proof)
  |> Option.bind(~f=((_, marks)) =>
       List.find_map(
         marks,
         ~f=
           fun
           | ProofMark.AlgebraDiffers(point) => Some(point)
           | _ => None,
       )
     )
  |> Option.value(~default=[])
  |> List.map(~f=((x, v)) => (x, Test_ProofMap.print_exp(v)));
};

let tests = (
  "AlgebraSteps",
  [
    test_case(
      "a polynomial identity over Int variables is verified", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "theorem t = forall a: Int -> forall b: Int -> (a + b) * (a + b) == a * a + 2 * a * b + b ** 2 proof from (a + b) * (a + b) do algebra a * a + 2 * a * b + b ** 2 end end; from $e == $e do rewrite refl end end in t",
        ),
      )
    ),
    test_case(
      "a polynomial identity over Nat variables is verified", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "use Nat in theorem t = forall a: Nat -> forall b: Nat -> (a + b) * (a + b) == a * a + 2 * a * b + b ** 2 proof from (a + b) * (a + b) do algebra a * a + 2 * a * b + b ** 2 end end; from $e == $e do rewrite refl end end in t",
        ),
      )
    ),
    test_case("a wrong Nat step is refused", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs(
          "t",
          "use Nat in theorem t = forall x: Nat -> x + x == 3 * x proof from x + x do algebra 3 * x end end in t",
        ),
      )
    ),
    test_case("Nat subtraction is verified", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "use Nat in theorem t = forall x: Nat -> monus(x + 1, 1) == x proof from monus(x + 1, 1) do algebra x end end; rewrite refl end in t",
        ),
      )
    ),
    test_case("a wrong step is refused", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs(
          "t",
          "theorem t = forall x: Int -> x + x == 3 * x proof from x + x do algebra 3 * x end end in t",
        ),
      )
    ),
    test_case(
      "a wrong step names values at which the sides differ",
      `Quick,
      () => {
        let (pm, proof) =
          run_named(
            "t",
            "theorem t = forall x: Int -> forall y: Int -> x * y == x + y proof from x * y do algebra x + y end end in t",
          );
        let point =
          find_marked_sub(pm, proof)
          |> Option.bind(~f=((_, marks)) =>
               List.find_map(
                 marks,
                 ~f=
                   fun
                   | ProofMark.AlgebraDiffers(point) => Some(point)
                   | _ => None,
               )
             )
          |> Option.value(~default=[]);
        let value = name =>
          List.Assoc.find(point, name, ~equal=String.equal)
          |> Option.map(~f=Test_ProofMap.print_exp)
          |> Option.bind(~f=Int.of_string_opt);
        switch (value("x"), value("y")) {
        | (Some(x), Some(y)) =>
          check(bool, "the sides differ there", true, x * y != x + y)
        | _ =>
          fail(
            "no value for x or for y among: "
            ++ String.concat(
                 ~sep=", ",
                 List.map(point, ~f=((x, v)) =>
                   x ++ " = " ++ Test_ProofMap.print_exp(v)
                 ),
               ),
          )
        };
      },
    ),
    test_case("`i` is a variable, not the imaginary unit", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs(
          "t",
          "theorem t = forall i: Int -> i * i + 1 == 0 proof from i * i do algebra 0 - 1 end end in t",
        ),
      )
    ),
    test_case("a definition with a literal value counts as it", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "let n = 3 in theorem t = n + n == 6 proof from n + n do algebra 6 end end; from 6 == 6 do rewrite refl end end in t",
        ),
      )
    ),
    test_case(
      "a definition whose value is an error leaves the step unverified",
      `Quick,
      () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status(
          "t",
          "let x : Int = 1 / 0 in theorem t = 0 * x == 0 proof from 0 * x do algebra 0 end end; from 0 == 0 do rewrite refl end end in t",
        ),
      )
    ),
    test_case("a variable not known to be Int is unverified", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status(
          "t",
          "theorem t = forall x -> x + 0 == x proof from x + 0 do algebra x end end; from x == x do rewrite refl end end in t",
        ),
      )
    ),
    test_case("a false equation between literals is refused", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs("bad", "theorem bad = 1 == 2 proof algebra true end in bad"),
      )
    ),
    /* Machine integers overflow at a width that depends on the machine. */
    test_case("a step outside what Z3 reads doesn't prove", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status(
          "t",
          "theorem t = forall x: SInt -> sint_plus(x, x) == sint_times(2, x) proof algebra true end in t",
        ),
      )
    ),
    /* False at n = 1: Int division rounds. */
    test_case("Int division is not exact", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs(
          "half",
          "theorem half = forall n: Int -> n / 2 * 2 == n proof from n / 2 * 2 do algebra n end end; rewrite refl end in half",
        ),
      )
    ),
    test_case("Nat division is not exact", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs(
          "half",
          "use Nat in theorem half = forall n: Nat -> n / 2 * 2 == n proof from n / 2 * 2 do algebra n end end; rewrite refl end in half",
        ),
      )
    ),
    test_case("a boolean identity is verified", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven(
          "forall a: Bool -> forall b: Bool -> (a && (a || b)) == a && (!(a && b)) == (!a || !b) && (a ==> b) == (!a || b)",
        ),
      )
    ),
    test_case("a wrong boolean step names values", `Quick, () =>
      check(
        list(pair(string, string)),
        "point",
        [("a", "false"), ("b", "true")],
        point(claim("forall a: Bool -> forall b: Bool -> (a || b) == a")),
      )
    ),
    test_case("an order fact is verified", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven(
          "forall x: Int -> forall y: Int -> x < x + 1 && (x <= y) == !(y < x) && (x >= y || y > x)",
        ),
      )
    ),
    /* Z3 is given no facts in scope, so a step that holds only under a
       hypothesis is refused (D91, D94). */
    test_case("a step that needs a hypothesis is refused", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        differs(
          "t",
          "theorem t = forall x: Int -> forall y: Int -> x > 0 && y > 0 ==> x * y > 0 proof assume h = x > 0 && y > 0 => algebra true end in t",
        ),
      )
    ),
    /* With the hypothesis in the goal, the goal is one boolean expression
       and Z3 reads all of it. */
    test_case("an implication is verified whole", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven(
          "forall x: Int -> forall y: Int -> x > 0 && y > 0 ==> x * y > 0",
        ),
      )
    ),
    test_case("`if` and `abs` are verified", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven(
          "forall x: Int -> (if x >= 0 then x else -x) == abs(x) && abs(x) >= 0 && abs(x * x) == x * x",
        ),
      )
    ),
    test_case("`monus` stops at zero", `Quick, () =>
      check(
        list(pair(string, string)),
        "point",
        [("n", "0")],
        point(
          claim(
            ~prefix="use Nat in ",
            "forall n: Nat -> monus(n, 1) + 1 == n",
          ),
        ),
      )
    ),
    /* The quotient and the remainder of two Nats fit together. Those of
       two Ints don't: the quotient rounds toward zero and the remainder
       is never negative. */
    test_case("Nat division and `nat_mod` fit together", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          claim(
            ~prefix="use Nat in ",
            "forall n: Nat -> n / 3 * 3 + nat_mod(n, 3) == n && nat_mod(n, 3) < 3",
          ),
        ),
      )
    ),
    test_case("Int division and `int_mod` don't fit together", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        refused("forall n: Int -> n / 2 * 2 + int_mod(n, 2) == n"),
      )
    ),
    test_case("`int_mod` by a positive literal is in range", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven("forall n: Int -> int_mod(n, 5) >= 0 && int_mod(n, 5) < 5"),
      )
    ),
    test_case("`int_mod` by a negative literal is in range", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven("forall n: Int -> int_mod(n, -5) >= 0 && int_mod(n, -5) < 5"),
      )
    ),
    test_case("a division's condition is the step's obligation", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Definedness",
        status("t", claim("forall x: Int -> x / x == 1")),
      )
    ),
    test_case(
      "a division's condition is discharged by a fact in scope", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "theorem t = forall x: Int -> x != 0 ==> x / x == 1 proof assume h = x != 0 => algebra true end in t",
        ),
      )
    ),
    /* At x = 0 the division is guarded, but the domain scan doesn't
       follow the guard: the step holds where `x != 0`, and that is its
       obligation. */
    test_case("a guarded division still incurs its condition", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Definedness",
        status(
          "t",
          "theorem t = forall x: Int -> (if x == 0 then 1 else x / x) == 1 proof from if x == 0 then 1 else x / x do algebra 1 end end; rewrite refl end in t",
        ),
      )
    ),
    /* The domain scan doesn't cover the function form of `/`, so Z3 may
       not assume its divisor is nonzero. */
    test_case(
      "an operation whose condition isn't incurred is unverified", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status("t", claim("forall x: Int -> int_divide(x, x) == 1")),
      )
    ),
    test_case("conversions among Int, Nat and Bool", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven("forall n: Nat -> int_of_nat(n) >= 0")
        && proven(
             ~prefix="use Nat in ",
             "forall n: Nat -> forall b: Bool -> bool_of_nat(n) == (n != 0) && nat_of_bool(b) * nat_of_bool(b) == nat_of_bool(b) && bool_of_int(int_of_bool(b)) == b",
           ),
      )
    ),
    test_case(
      "`nat_of_int` incurs that its argument isn't negative", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Definedness",
        status(
          "t",
          claim("forall x: Int -> int_of_nat(nat_of_int(x)) == x"),
        ),
      )
    ),
    test_case("string concatenation", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven(
          "forall a: String -> forall b: String -> forall c: String -> a ++ (b ++ c) == (a ++ b) ++ c && a ++ \"\" == a && \"ab\" ++ \"c\" == \"abc\"",
        ),
      )
    ),
    test_case("string concatenation doesn't commute", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        refused("forall a: String -> forall b: String -> a ++ b == b ++ a"),
      )
    ),
    test_case("an operator under its function name", `Quick, () =>
      check(
        bool,
        "proven",
        true,
        proven(
          "forall x: Int -> forall y: Int -> int_plus(x, y) == y + x && int_lt(x, y) == (y > x) && fst((x, y)) + snd((x, y)) == x + y",
        ),
      )
    ),
    /* No value: a zero divisor. */
    agrees(
      ~errors=14,
      "Z3 rounds Int division and `int_mod` as the evaluator does",
      List.concat_map(["A / B", "int_mod(A, B)"], ~f=over(ints)),
    ),
    /* No value: `nat_of_int` of a negative. */
    agrees(
      ~errors=1,
      "Z3 reads the other Int operators and builtins as the evaluator does",
      List.concat_map(
        [
          "A + B",
          "A - B",
          "A * B",
          "A < B",
          "A <= B",
          "A > B",
          "A >= B",
          "A == B",
          "A != B",
          "-A",
          "A ** 0",
          "A ** 3",
          "if A < B then A else B",
          "abs(A)",
          "bool_of_int(A)",
          "int_of_nat(nat_of_int(A))",
          "fst((A, B)) - snd((A, B))",
          "int_times(A, B)",
          "int_gte(A, B)",
        ],
        ~f=over(few_ints),
      ),
    ),
    /* No value: a zero divisor. */
    agrees(
      ~mode=Nat,
      ~errors=8,
      "Z3 rounds Nat division, `nat_mod` and `monus` as the evaluator does",
      List.concat_map(
        ["A / B", "nat_mod(A, B)", "monus(A, B)"],
        ~f=over(nats),
      ),
    ),
    agrees(
      ~mode=Nat,
      "Z3 reads the other Nat operators and builtins as the evaluator does",
      List.concat_map(
        [
          "A + B",
          "A * B",
          "A < B",
          "A <= B",
          "A > B",
          "A >= B",
          "A == B",
          "A != B",
          "A ** 0",
          "A ** 3",
          "bool_of_nat(A)",
          "int_of_nat(A)",
        ],
        ~f=over(["0", "3"]),
      ),
    ),
    agrees(
      "Z3 reads Bool operators and builtins as the evaluator does",
      List.concat_map(
        [
          "A && B",
          "A || B",
          "A ==> B",
          "!A",
          "A == B",
          "A != B",
          "if A then B else !B",
          "int_of_bool(A) + int_of_bool(B)",
          "bool_and(A, B)",
        ],
        ~f=over(bools),
      ),
    ),
    agrees(
      "Z3 reads string operators as the evaluator does",
      List.concat_map(
        ["A ++ B", "A ++ B == B ++ A", "A != B"],
        ~f=over(["\"\"", "\"a\"", "\"b a\""]),
      ),
    ),
    test_case("nat_succ_ne_zero decides a Nat test against 0", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "use Nat in theorem t = forall m: Nat -> (if m + 1 == 0 then 0 else 5) == 5 proof from m + 1 == 0 do rewrite nat_succ_ne_zero end end; from if false then 0 else 5 do step end; rewrite refl end in t",
        ),
      )
    ),
    /* `m + 1 == 0` is true at m = -1. */
    test_case("nat_succ_ne_zero doesn't apply to an Int", `Quick, () =>
      check(
        bool,
        "refused",
        true,
        {
          let (pm, proof) =
            run_named(
              "t",
              "theorem t = forall m: Int -> (m + 1 == 0) == false proof from m + 1 == 0 do rewrite nat_succ_ne_zero end end; rewrite refl end in t",
            );
          has_mark_kind(
            pm,
            proof,
            fun
            | ProofMark.RuleDoesNotApply(_) => true
            | _ => false,
          );
        },
      )
    ),
    test_case("nat_add_sub takes off what was added", `Quick, () =>
      check(
        string,
        "Proven",
        "Proven",
        status(
          "t",
          "use Nat in theorem t = forall m: Nat -> monus(m + 3, 3) == m proof from monus(m + 3, 3) do rewrite nat_add_sub end end; rewrite refl end in t",
        ),
      )
    ),
    /* Both rules apply, so the sum unfolds. It recurses through `monus`,
       which the totality check doesn't follow, so `refl` at a goal that
       mentions it is refused: the first mark is on that last step. */
    test_case("a sum written with if unfolds at m + 1", `Quick, () =>
      check(
        bool,
        "only refl is refused",
        true,
        {
          let (pm, proof) =
            run_named(
              "t",
              "use Nat in let sum = fun (n : Nat) -> if n == 0 then 0 else n + sum(monus(n, 1)) in theorem t = forall m: Nat -> sum(m + 1) == m + 1 + sum(m) proof from sum(m + 1) do step end; from m + 1 == 0 do rewrite nat_succ_ne_zero end end; from if false then 0 else m + 1 + sum(monus(m + 1, 1)) do step end; from monus(m + 1, 1) do rewrite nat_add_sub end end; rewrite refl end in t",
            );
          has_mark_kind(
            pm,
            proof,
            fun
            | ProofMark.PossiblyDivergentInstantiation({equality: "refl", _}) =>
              true
            | _ => false,
          );
        },
      )
    ),
  ],
);
