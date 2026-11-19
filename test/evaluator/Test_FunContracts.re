open Alcotest;
open Language;
open Test_ProofMap.Harness;
open Poly;

/* The closure lemmas of Axioms.re, guarded axioms used as ordinary
 * conditional rewrite rules. */

let check_count = (msg, expected, obs) =>
  Alcotest.check(Alcotest.int, msg, expected, List.length(obs));

let is_remote: Obligation.discharge => bool =
  fun
  | Obligation.Remote(_) => true
  | _ => false;

/* --- Closure lemmas -------------------------------------------------- */

/* Nothing covers the antecedents: ProvenModulo with the two Pending. */
let test_nonzero_mul_pending_without_binders = () => {
  let src = {|theorem t = forall a -> forall b -> a * b != 0 proof from a * b != 0 do rewrite nonzero_mul end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("the two antecedent obligations", 2, obs);
  Alcotest.check(
    Alcotest.bool,
    "both are Pending",
    true,
    List.for_all(~f=Obligation.is_pending, obs),
  );
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([_, _]) => ()
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with two pending, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* Bridging lemma: `nonzero_of_pos` turns a `> 0` fact into the `!= 0`
 * an obligation needs, here as an axiom step rewriting the goal
 * itself. */
let test_nonzero_of_pos_bridge = () => {
  let src = {|theorem t = forall a -> a > 0 ==> a != 0 proof assume a > 0 => from a != 0 do rewrite nonzero_of_pos end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("one antecedent obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the antecedent is a > 0", "a > 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the assumption",
      true,
      is_remote(ob.discharge),
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* Closed antecedents just run: `pos_mul` at literals. */
let test_pos_mul_closed_evaluated = () => {
  let src = {|theorem t = 2 * 3 > 0 proof from 2 * 3 > 0 do rewrite pos_mul end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("the two antecedent obligations", 2, obs);
  Alcotest.check(
    Alcotest.bool,
    "both discharged by closed evaluation",
    true,
    List.for_all(
      ~f=(ob: Obligation.t) => ob.discharge == Obligation.Evaluated,
      obs,
    ),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let tests = (
  "Evaluator.FunContracts",
  [
    test_case(
      "nonzero_mul pending without binders (ProvenModulo)",
      `Quick,
      test_nonzero_mul_pending_without_binders,
    ),
    test_case(
      "nonzero_of_pos bridges > 0 to != 0",
      `Quick,
      test_nonzero_of_pos_bridge,
    ),
    test_case(
      "pos_mul closed antecedents evaluate",
      `Quick,
      test_pos_mul_closed_evaluated,
    ),
  ],
);
