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


/* --- Bounded adaptive discovery (Reset-numeric-cast-proof-script-correction)
 *
 * Old-style `eval E at i end` takes ONE stepper step (ProofCheck
 * eval_step_outgoing_result), so the number of steps a closed control
 * needs is not known in advance. Root approved a bounded search: for
 * n = 1..32, the script is n copies of the existing whole-goal pattern
 * step `eval $lhs == $rhs at 0 end` (distinct named captures, so the two
 * sides are never constrained equal), run through the REAL parser,
 * statics, Evaluator and ProofCheck. Every attempt is printed. The search
 * stops at the first mark, the first literal true/false, or -- for a
 * positive -- the first goal whose sides are the same canonical Rational
 * value, where `axiom refl_eq at 0 on $x == $x end` (linear `$x`) closes
 * it. This is discovery, not a fixture run once; the FINAL script is
 * printed literally and then asserted. */

let step_text = "eval $lhs == $rhs at 0 end";
let refl_text = "axiom refl_eq at 0 on $x == $x end";
let max_steps = 32;

type attempt = {
  script: string,
  status: string,
  marks: string,
  outgoing: string,
  canonical_equal: bool,
};

/* `p / q` as printed for a Rational value. */
let is_rat_value = (s: string): bool => {
  let digits = d => {
    let ok = ref(d != "");
    String.iter(c => if (c < '0' || c > '9') {ok := false}, d);
    ok^;
  };
  switch (String.split_on_char(' ', s)) {
  | [n, "/", d] =>
    let n =
      String.length(n) > 1 && n.[0] == '-'
        ? String.sub(n, 1, String.length(n) - 1) : n;
    digits(n) && digits(d);
  | _ => false
  };
};

let run_attempt = (~prefix, name, stmt, script): attempt => {
  let src =
    library
    ++ prefix
    ++ "theorem "
    ++ name
    ++ " = "
    ++ stmt
    ++ " proof "
    ++ script
    ++ " in 0";
  let (state, statics, elab) = Test_ProofMap.eval_with_proof(parse_exp(src));
  let pm = EvaluatorState.get_proof_map(state);
  let proof = Test_ProofMap.proof_of_named(name, elab);
  let out =
    switch (ProofMap.lookup(Proof.rep_id(proof), pm)) {
    | Some({outgoing: Some(o), _}) => Some(o)
    | _ => None
    };
  let canonical_equal =
    switch (out) {
    | Some(o) =>
      switch (Exp.term_of(o)) {
      | BinOp(_, a, b) =>
        let (sa, sb) = (
          I.one_line(Test_ProofMap.print_exp(a)),
          I.one_line(Test_ProofMap.print_exp(b)),
        );
        sa == sb && is_rat_value(sa);
      | _ => false
      }
    | None => false
    };
  let a = {
    script,
    status:
      I.one_line(
        ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof)),
      ),
    marks:
      switch (Test_ProofMap.find_marked_sub(pm, proof)) {
      | Some((_, ms)) => I.one_line(String.concat("; ", List.map(ProofMark.show, ms)))
      | None => ""
      },
    outgoing: I.opt_exp_str(out),
    canonical_equal,
  };
  print_endline(
    "ATTEMPT "
    ++ name
    ++ " static_errors="
    ++ string_of_int(List.length(Statics.Map.error_ids(statics)))
    ++ " status="
    ++ a.status
    ++ " outgoing=["
    ++ a.outgoing
    ++ "] canonical_equal="
    ++ string_of_bool(a.canonical_equal)
    ++ " marks=["
    ++ a.marks
    ++ "]\n  script: "
    ++ script,
  );
  a;
};

type discovery =
  | Final(int, attempt)
  | Blocked(string, attempt);

/* Runtime value of the closed statement in the same context. */
let runtime = (~prefix, stmt: string): string => {
  let src = library ++ prefix ++ stmt;
  let (statics, elab) = Test_ProofMap.statics_and_elab(parse_exp(src));
  let (v, _) = Evaluator.evaluate(~statics, ~env=Builtins.env_init, elab);
  I.one_line(Test_ProofMap.print_exp(v));
};

let discover = (~prefix, ~positive, name, stmt): discovery => {
  let rec go = n => {
    let script = String.concat("; ", List.init(n, _ => step_text));
    let a = run_attempt(~prefix, name, stmt, script);
    if (a.marks != "") {
      Blocked("mark after " ++ string_of_int(n) ++ " steps", a);
    } else if (a.outgoing == "true" || a.outgoing == "false") {
      Final(n, a);
    } else if (positive && a.canonical_equal) {
      Final(n, run_attempt(~prefix, name, stmt, script ++ "; " ++ refl_text));
    } else if (n >= max_steps) {
      Blocked("32-step bound", a);
    } else {
      go(n + 1);
    };
  };
  let d = go(1);
  print_endline(
    switch (d) {
    | Final(n, a) =>
      "FINAL "
      ++ name
      ++ " eval_steps="
      ++ string_of_int(n)
      ++ " status="
      ++ a.status
      ++ " outgoing="
      ++ a.outgoing
      ++ " runtime="
      ++ runtime(~prefix, stmt)
      ++ "\n  literal script: "
      ++ a.script
    | Blocked(why, a) =>
      "BLOCKED "
      ++ name
      ++ " ("
      ++ why
      ++ ") last status="
      ++ a.status
      ++ " outgoing="
      ++ a.outgoing
      ++ " marks=["
      ++ a.marks
      ++ "]"
    },
  );
  d;
};

let check_positive = (~prefix, name, stmt) =>
  switch (discover(~prefix, ~positive=true, name, stmt)) {
  | Blocked(why, _) => Alcotest.fail(name ++ " blocked: " ++ why)
  | Final(n, a) =>
    check(bool, name ++ " used at least one eval step", true, n >= 1);
    check(string, name ++ " marks", "", a.marks);
    check(string, name ++ " outgoing", "true", a.outgoing);
    check(string, name ++ " status", "Proven", a.status);
    check(string, name ++ " runtime", "true", runtime(~prefix, stmt));
  };

let check_negative = (~prefix, name, stmt) =>
  switch (discover(~prefix, ~positive=false, name, stmt)) {
  | Blocked(why, _) => Alcotest.fail(name ++ " blocked control: " ++ why)
  | Final(_, a) =>
    check(string, name ++ " marks", "", a.marks);
    check(string, name ++ " outgoing is literal false", "false", a.outgoing);
    check(string, name ++ " status", "Refuted", a.status);
    check(string, name ++ " runtime", "false", runtime(~prefix, stmt));
  };

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
      "closed instances (negative, zero, positive), normal context: adaptive stepwise eval",
      `Quick,
      () =>
        List.iter(
          ((nm, k)) =>
            check_positive(
              ~prefix="",
              nm,
              "int_to_rat(" ++ k ++ ") == rat(" ++ k ++ ", 1)",
            ),
          [("cn", "-7"), ("cz", "0"), ("cp", "12")],
        ),
    ),
    test_case(
      "closed instances (negative, zero, positive) under use Rational: adaptive stepwise eval",
      `Quick,
      () =>
        List.iter(
          ((nm, k)) =>
            check_positive(
              ~prefix="use Rational in ",
              nm,
              "int_to_rat(" ++ k ++ ") == " ++ k,
            ),
          [("un", "-7"), ("uz", "0"), ("up", "12")],
        ),
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
      "negatives: a wrong defining equation; false closed instances reach literal false",
      `Quick,
      () => {
        let (errs, thms) =
          observe(
            ~label="negatives",
            "theorem wd = forall n: Int -> int_to_rat(n) == rat(n, 2) proof eval int_to_rat(n) at 0 end; axiom refl_eq at 0 on rat(n, 2) == rat(n, 2) end in 0",
            ["wd"],
          );
        check_valid(errs);
        check_not_proven(find(thms, "wd"));
        /* a1a9ea44's wc (one whole-statement eval) was vacuous: Incomplete
           from an unfinished one-step script. Replaced by discovery to an
           actual literal false. */
        check_negative(~prefix="", "wc", "int_to_rat(3) == rat(3, 2)");
        check_negative(~prefix="use Rational in ", "wu", "int_to_rat(3) == 3 / 2");
      },
    ),
  ],
);
