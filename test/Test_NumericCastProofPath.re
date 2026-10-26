open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Reset-numeric-cast-proof-path (private, test-only, on fa558f23).
 *
 * The EXISTING library definition `int_to_rat` (Test_NumericCastLibrary
 * .library, unchanged) goes through parser, statics, the evaluator WITH
 * statics, and ProofCheck. Exercised: its quantified defining equation
 * over Int, closed negative/zero/positive instances in the normal and the
 * `use Rational` context, and citation of the defining equation (explicit
 * `with` and automatic). No universal ring or homomorphism law is stated
 * as a fact; the one homomorphism statement is an open control expected
 * to stay unproved.
 *
 * Known baseline gap (Reset-disproven-citation-reference-oracle, f9890f03):
 * `ProofMap.full_status` carries no dependency, so a theorem citing an
 * unproved statement can read as plain Proven. Where that applies the
 * status is printed as BASELINE GAP and NOT asserted as a proof.
 *
 * Every observation is printed before any assertion. Helpers reused from
 * Test_ProofMap and Test_Instantiation. */

module I = Test_Instantiation;

let library = Test_NumericCastLibrary.library;

/* The defining equation and its definitional proof: unfold the call, then
   close `rat(n, 1) == rat(n, 1)` with the built-in refl_eq. */
let def_eq = "forall n: Int -> int_to_rat(n) == rat(n, 1)";
let def_proof =
  "eval int_to_rat(n) at 0 end; axiom refl_eq at 0 on rat(n, 1) == rat(n, 1) end";

type thm = {
  name: string,
  status: string,
  marks: string,
  obligations: list(string),
};

let observe = (~label, rest: string, names: list(string)) => {
  let src = library ++ rest;
  let term = parse_exp(src);
  let (state, statics, elab) = Test_ProofMap.eval_with_proof(term);
  let pm = EvaluatorState.get_proof_map(state);
  let errs = List.length(Statics.Map.error_ids(statics));
  let ty = Typ.pretty_print(elaborated_type(statics, term));
  let thms =
    List.map(
      n => {
        let proof = Test_ProofMap.proof_of_named(n, elab);
        let t = {
          name: n,
          status:
            I.one_line(
              ProofMap.show_full_status(
                ProofMap.full_status_of_proof(pm, proof),
              ),
            ),
          marks:
            switch (Test_ProofMap.find_marked_sub(pm, proof)) {
            | Some((_, ms)) => String.concat("; ", List.map(ProofMark.show, ms))
            | None => ""
            },
          obligations: I.obligation_goals(pm, proof),
        };
        print_endline(
          "THEOREM "
          ++ n
          ++ ": status="
          ++ t.status
          ++ " marks=["
          ++ t.marks
          ++ "] obligations=["
          ++ String.concat("; ", t.obligations)
          ++ "]\n"
          ++ I.dump(pm, proof, "  "),
        );
        t;
      },
      names,
    );
  print_endline(
    "OBSERVE "
    ++ label
    ++ "\nsource: "
    ++ rest
    ++ "\nstatic errors: "
    ++ string_of_int(errs)
    ++ "\nprogram type: "
    ++ ty
    ++ "\nEND OBSERVE",
  );
  (errs, thms);
};

let find = (thms, n) => List.find(t => t.name == n, thms);

let check_proven = (t: thm) => {
  check(string, t.name ++ " marks", "", t.marks);
  check(string, t.name ++ " status", "Proven", t.status);
};

let check_not_proven = (t: thm) =>
  check(bool, t.name ++ " is not Proven", false, t.status == "Proven");

let check_valid = errs => check(int, "valid program: no static errors", 0, errs);

let tests = (
  "NumericCastProofPath",
  [
    test_case(
      "defining equation over Int n, proved by unfolding int_to_rat",
      `Quick,
      () => {
        let (errs, thms) =
          observe(
            ~label="defining equation",
            "theorem def = "
            ++ def_eq
            ++ " proof "
            ++ def_proof
            ++ " in 0",
            ["def"],
          );
        check_valid(errs);
        check_proven(find(thms, "def"));
      },
    ),
    test_case(
      "closed instances (negative, zero, positive), normal context",
      `Quick,
      () => {
        let inst = (nm, k) =>
          "theorem "
          ++ nm
          ++ " = int_to_rat("
          ++ k
          ++ ") == rat("
          ++ k
          ++ ", 1) proof eval int_to_rat("
          ++ k
          ++ ") == rat("
          ++ k
          ++ ", 1) at 0 end in ";
        let (errs, thms) =
          observe(
            ~label="closed normal",
            inst("cn", "-7") ++ inst("cz", "0") ++ inst("cp", "12") ++ "0",
            ["cn", "cz", "cp"],
          );
        check_valid(errs);
        List.iter(check_proven, thms);
      },
    ),
    test_case(
      "closed instances (negative, zero, positive) under use Rational",
      `Quick,
      () => {
        let inst = (nm, k) =>
          "theorem "
          ++ nm
          ++ " = int_to_rat("
          ++ k
          ++ ") == "
          ++ k
          ++ " proof eval int_to_rat("
          ++ k
          ++ ") == "
          ++ k
          ++ " at 0 end in ";
        let (errs, thms) =
          observe(
            ~label="closed use Rational",
            "use Rational in "
            ++ inst("un", "-7")
            ++ inst("uz", "0")
            ++ inst("up", "12")
            ++ "0",
            ["un", "uz", "up"],
          );
        check_valid(errs);
        List.iter(check_proven, thms);
      },
    ),
    test_case(
      "citing the proved defining equation: explicit with, and automatic",
      `Quick,
      () => {
        let (errs, thms) =
          observe(
            ~label="citation",
            "theorem def = "
            ++ def_eq
            ++ " proof "
            ++ def_proof
            ++ " in "
            ++ "theorem gw = int_to_rat(-7) == rat(-7, 1) proof axiom def with n = -7 at 0 on int_to_rat(-7) end; axiom refl_eq at 0 on rat(-7, 1) == rat(-7, 1) end in "
            ++ "theorem ga = int_to_rat(12) == rat(12, 1) proof axiom def at 0 on int_to_rat(12) end; axiom refl_eq at 0 on rat(12, 1) == rat(12, 1) end in "
            ++ "theorem ds = "
            ++ def_eq
            ++ " proof ? in "
            ++ "theorem gs = int_to_rat(0) == rat(0, 1) proof axiom ds with n = 0 at 0 on int_to_rat(0) end; axiom refl_eq at 0 on rat(0, 1) == rat(0, 1) end in 0",
            ["def", "gw", "ga", "ds", "gs"],
          );
        let gs = find(thms, "gs");
        print_endline(
          "BASELINE GAP (unasserted): gs cites ds (status "
          ++ find(thms, "ds").status
          ++ "); gs status="
          ++ gs.status
          ++ ". full_status has no dependency carrier, so this is not a proof of gs.",
        );
        check_valid(errs);
        check_proven(find(thms, "def"));
        check_proven(find(thms, "gw"));
        check_proven(find(thms, "ga"));
        check(string, "ds is Incomplete (stated)", "Incomplete", find(thms, "ds").status);
        check(string, "gs citation machinery: no marks", "", gs.marks);
      },
    ),
    test_case(
      "open frontier: Int-to-Rational additivity is not proved by unfolding",
      `Quick,
      () => {
        let hom =
          "forall a: Int -> forall b: Int -> int_to_rat(a + b) == (use Rational in int_to_rat(a) + int_to_rat(b))";
        let (errs, thms) =
          observe(
            ~label="frontier",
            "theorem hs = "
            ++ hom
            ++ " proof ? in theorem hu = "
            ++ hom
            ++ " proof eval int_to_rat(a + b) at 0 end; axiom refl_eq at 0 on rat(a + b, 1) == rat(a + b, 1) end in 0",
            ["hs", "hu"],
          );
        check_valid(errs);
        check(string, "hs is Incomplete (stated only)", "Incomplete", find(thms, "hs").status);
        check_not_proven(find(thms, "hu"));
      },
    ),
    test_case(
      "negatives: a wrong defining equation and a false closed instance",
      `Quick,
      () => {
        let (errs, thms) =
          observe(
            ~label="negatives",
            "theorem wd = forall n: Int -> int_to_rat(n) == rat(n, 2) proof eval int_to_rat(n) at 0 end; axiom refl_eq at 0 on rat(n, 2) == rat(n, 2) end in "
            ++ "theorem wc = int_to_rat(3) == rat(3, 2) proof eval int_to_rat(3) == rat(3, 2) at 0 end in 0",
            ["wd", "wc"],
          );
        check_valid(errs);
        check_not_proven(find(thms, "wd"));
        check_not_proven(find(thms, "wc"));
      },
    ),
  ],
);
