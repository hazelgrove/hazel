open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* `algebra` steps, verified by Algebrite. It verifies Int arithmetic
 * (literals, +, −, ×, unary −, ** with a literal exponent) over variables
 * known to be Int, and Nat arithmetic (literals, +, ×, **) over variables
 * known to be Nat, and refuses a step it finds wrong. Any other step's
 * equation is left as an obligation, so the proof can't read plain
 * Proven. */

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
    | ProofMark.AlgebraDiffers => true
    | _ => false,
  );
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
    /* True, but `monus` truncates, so it isn't a polynomial and the step
       stays an obligation. */
    test_case("Nat subtraction is unverified", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
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
    test_case("a step outside Int arithmetic doesn't prove", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status("bad", "theorem bad = 1 == 2 proof algebra true end in bad"),
      )
    ),
    /* False at n = 1: Int division rounds. */
    test_case("Int division is not exact", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status(
          "half",
          "theorem half = forall n: Int -> n / 2 * 2 == n proof from n / 2 * 2 do algebra n end end; rewrite refl end in half",
        ),
      )
    ),
    test_case("Nat division is not exact", `Quick, () =>
      check(
        string,
        "ProvenModulo",
        "ProvenModulo Algebra",
        status(
          "half",
          "use Nat in theorem half = forall n: Nat -> n / 2 * 2 == n proof from n / 2 * 2 do algebra n end end; rewrite refl end in half",
        ),
      )
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
