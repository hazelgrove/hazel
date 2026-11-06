open Alcotest;
open Test_Statics_Prelude;
open Language;

/* Non-computable forms: `forall` / `forall ... where ...` are
   propositions, and `==` / `!=` at a type containing a function has no
   evaluation semantics. Inside a theorem's statement or proof they are
   legal; outside they are warnings, not marks. */

let warnings = (map: Statics.Map.t): list(Warning.list_item) =>
  Id.Map.fold(
    (_, info: Info.t, acc) => Info.warnings_of(info) @ acc,
    map,
    [],
  );

let is_forall_warning =
  fun
  | Warning.Exp(ForallOutsideTheorem) => true
  | _ => false;

let compare_fun_errors = (map: Statics.Map.t): int =>
  errors(map)
  |> List.concat_map(snd)
  |> List.filter(
       fun
       | Mark.CompareFun(_) => true
       | _ => false,
     )
  |> List.length;

let count = (pred, map) => List.length(List.filter(pred, warnings(map)));

let expects_warning =
    (name: string, source: string, pred: Warning.list_item => bool) =>
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(parse_exp(source));
      Alcotest.check(bool, name, true, count(pred, s) > 0);
    },
  );

let expects_no_warning =
    (name: string, source: string, pred: Warning.list_item => bool) =>
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(parse_exp(source));
      Alcotest.check(int, name, 0, count(pred, s));
    },
  );

/* --- stepper path -------------------------------------------------
   A stepper / eval-result editor re-runs statics on an already-evaluated
   term, which may have come out of a theorem's statement or proof. Those
   editors pass `is_dynamic_term=true`, which threads through to
   `Statics.mk(~in_theorem=true, ...)`, so the non-computable warnings must
   stay quiet there. The cell editor (`is_dynamic_term=false`) keeps warning. */

let statics_stepper = term =>
  fst(
    Statics.mk(
      ~in_theorem=true,
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      term,
    ),
  );

let statics_cached_stepper = term => {
  let s =
    Haz3lcore.CachedStatics.init_from_term(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=true,
      term,
    );
  s.info_map;
};

let expects_no_warning_via =
    (
      statics_of: Exp.t => Statics.Map.t,
      name: string,
      source: string,
      pred: Warning.list_item => bool,
    ) =>
  test_case(
    name,
    `Quick,
    () => {
      let s = statics_of(parse_exp(source));
      Alcotest.check(int, name, 0, count(pred, s));
    },
  );

/* Whether comparing functions is reported as a type error. */
let expects_compare_fun_via =
    (
      statics_of: Exp.t => Statics.Map.t,
      name: string,
      source: string,
      expected: bool,
    ) =>
  test_case(
    name,
    `Quick,
    () => {
      let s = statics_of(parse_exp(source));
      Alcotest.check(bool, name, expected, compare_fun_errors(s) > 0);
    },
  );

/* No expression anywhere in the program carries a mark. */
let expects_no_errors = (name: string, source: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(parse_exp(source));
      let marks = List.map(snd, errors(s));
      Alcotest.check(
        list(testable_issue),
        name,
        [],
        List.map(ms => Marks(ms), marks),
      );
    },
  );

let tests = (
  "Statics.Noncomputable",
  [
    /* --- `forall` outside a theorem ---------------------------------- */
    expects_warning(
      "bare forall warns",
      {|forall x -> x == x|},
      is_forall_warning,
    ),
    /* Int `==` is computable: no function-comparison error. */
    expects_compare_fun_via(
      statics,
      "bare forall over Int == is not a function comparison",
      {|forall x -> x == x|},
      false,
    ),
    expects_no_errors("bare forall is not an error", {|forall x -> x == x|}),
    expects_warning(
      "forall in a let body warns",
      {|let p = forall x -> x == x in p|},
      is_forall_warning,
    ),
    /* --- `forall` inside a theorem ------------------------------------ */
    expects_no_warning(
      "forall in a theorem statement is quiet",
      {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on x == x end in 0|},
      is_forall_warning,
    ),
    expects_no_errors(
      "forall in a theorem statement is not an error",
      {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on x == x end in 0|},
    ),
    /* The continuation after `in` is ordinary code, so a `forall` there
       still warns even though it sits inside a `theorem` term. */
    expects_warning(
      "forall in the theorem continuation warns",
      {|theorem t = true proof ? in forall x -> true|},
      is_forall_warning,
    ),
    /* --- function equality outside a theorem -------------------------- */
    expects_compare_fun_via(
      statics,
      "function == outside a theorem is a type error",
      {|(fun x -> x) == (fun x -> x)|},
      true,
    ),
    /* `!=` shares the DefinedPoly path with `==`. */
    expects_compare_fun_via(
      statics,
      "function != outside a theorem is a type error",
      {|(fun x -> x) != (fun x -> x)|},
      true,
    ),
    /* --- function equality inside a theorem (the funext pin) ---------- */
    expects_no_errors(
      "function == in a theorem statement is not an error",
      {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in 0|},
    ),
    /* --- `forall ... where ...` --------------------------------------- */
    expects_warning(
      "forall-where outside a theorem warns",
      {|forall x where x != 0 -> x / x == 1|},
      is_forall_warning,
    ),
    expects_no_warning(
      "forall-where in a theorem statement is quiet",
      {|theorem t = forall x where x != 0 -> x / x == 1 proof ? in 0|},
      is_forall_warning,
    ),
    /* --- nesting ------------------------------------------------------- */
    /* The inner theorem's own statement is quiet; the program-level
       `forall` in the outer let-binding still warns. Exactly one warning. */
    test_case(
      "nested theorem: only the outer forall warns",
      `Quick,
      () => {
        let src = {|let p = forall x -> x == x in theorem t = forall y -> y == y proof ? in 0|};
        let s = statics(parse_exp(src));
        Alcotest.check(int, src, 1, count(is_forall_warning, s));
      },
    ),
    /* A `forall` in a proof (here, an `on` target inside an axiom step) is
       inside the theorem block and must not warn. */
    expects_no_warning(
      "forall inside a proof is quiet",
      {|theorem t = true proof axiom refl_eq at 0 on (forall x -> x == x) end in 0|},
      is_forall_warning,
    ),
    /* --- stepper editors count as inside a theorem -------------------- */
    expects_no_warning_via(
      statics_stepper,
      "stepper: bare forall is quiet",
      {|forall x -> x == x|},
      is_forall_warning,
    ),
    expects_compare_fun_via(
      statics_stepper,
      "stepper: function == is not an error",
      {|(fun x -> x) == (fun x -> x)|},
      false,
    ),
    expects_compare_fun_via(
      statics_stepper,
      "stepper: function != is not an error",
      {|(fun x -> x) != (fun x -> x)|},
      false,
    ),
    expects_no_warning_via(
      statics_stepper,
      "stepper: forall-where is quiet",
      {|forall x where x != 0 -> x / x == 1|},
      is_forall_warning,
    ),
    /* Same, through the editor-facing entry point the stepper actually
       uses (`CachedStatics.init_from_term(~is_dynamic_term=true)`). */
    expects_no_warning_via(
      statics_cached_stepper,
      "stepper (cached): bare forall is quiet",
      {|forall x -> x == x|},
      is_forall_warning,
    ),
    expects_compare_fun_via(
      statics_cached_stepper,
      "stepper (cached): function == is not an error",
      {|(fun x -> x) == (fun x -> x)|},
      false,
    ),
    /* The memo cache key includes `in_theorem`, so a stepper result must
       not silence the cell editor for the same term (or vice versa). */
    test_case(
      "cell path still warns after the stepper path is cached",
      `Quick,
      () => {
        let src = {|forall x -> x == x|};
        let e = parse_exp(src);
        let quiet = statics_stepper(e);
        Alcotest.check(
          int,
          "stepper quiet",
          0,
          count(is_forall_warning, quiet),
        );
        let loud = statics(e);
        Alcotest.check(
          bool,
          "cell warns",
          true,
          count(is_forall_warning, loud) > 0,
        );
      },
    ),
  ],
);
