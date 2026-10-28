/* G1-prime (reviewer ADDENDUM-5): standardness does not establish an
   Int/Bool class. Typed operations (Int `+ - *`, unary `-`, comparisons,
   Bool `&& || ==>`, `!`, `==`/`!=`, an `if` condition) and list
   operations (`@`, a cons tail) need ACTUAL operand class evidence: a
   literal, a variable whose context type is that class (typed binders
   keep their declared type), or an operation of that result class over
   evidenced operands. Unknown is never class evidence; there is no
   inference of application / `if` / `case` result classes.
   Assertions state the target (fail closed); every case is its own test,
   and prints the judgement, the program's static errors and its runtime
   result at a mismatched argument. */
open Alcotest;
open Language;

module RE = Test_ResetExistentials;
module RS = Test_ResetRecursiveStandardness;

let judge = src =>
  ProofCheck.std_structural(
    SemanticCtx.of_ctx_and_env(
      Builtins.ctx_init(Some(Operators.default_mode)),
      Builtins.env_init,
    ),
    Test_Evaluator_Prelude.parse_exp(src),
  );

/* A lambda judged standard must be standard at EVERY standard argument;
   `bad` is a statics-valid standard argument of the wrong class. */
let negative = (label, lam, bad) =>
  test_case("negative " ++ label ++ ": not std", `Quick, () => {
    let j = judge(lam);
    let o = RE.observe("opc_" ++ label, "let f = " ++ lam ++ " in f(" ++ bad ++ ")");
    print_endline(
      "OPC "
      ++ label
      ++ " | judgement "
      ++ string_of_bool(j)
      ++ " | static errors "
      ++ string_of_int(List.length(o.errors))
      ++ " | runtime f("
      ++ bad
      ++ ") ~> "
      ++ RE.print_exp(o.runtime),
    );
    check(bool, label, false, j);
  });

/* Judgement in a context with `h : Int -> Int` and `x : Int` standard,
   `k : Int -> Int` NOT standard, `g : ? -> ?` standard, and `u` standard
   but untyped (no Ctx type). */
let judge_in_fn_ctx = src => {
  let int_t = Typ.temp(Atom(Int));
  let arrow = Typ.temp(Arrow(int_t, int_t));
  let unknown_arrow =
    Typ.temp(Arrow(Typ.temp(Unknown(Internal)), Typ.temp(Unknown(Internal))));
  let ctx =
    SemanticCtx.of_ctx_and_env(
      Builtins.ctx_init(Some(Operators.default_mode)),
      Builtins.env_init,
    );
  let ctx =
    List.fold_left(
      (c, (x, t)) => SemanticCtx.add_entry(c, Id.mk(), x, t, None),
      ctx,
      [("h", arrow), ("k", arrow), ("g", unknown_arrow), ("x", int_t)],
    )
    |> SemanticCtx.mark_standard(_, ["h", "g", "x", "u"]);
  ProofCheck.std_structural(ctx, Test_Evaluator_Prelude.parse_exp(src));
};

let fn_unit = (label, src, expected) =>
  test_case("fn-ctx judgement " ++ label, `Quick, () => {
    let j = judge_in_fn_ctx(src);
    print_endline("OPC fn-ctx " ++ label ++ " | " ++ src ++ " | judgement " ++ string_of_bool(j));
    check(bool, label, expected, j);
  });

/* A witness route whose gate is reported raw; a refused witness is NOT a
   gate observation. */
let route_gate = (label, src, expected) =>
  test_case("route " ++ label ++ ": gate " ++ expected, `Quick, () => {
    let (o, d) = RS.run(label, src);
    let full = RE.full(o, "w");
    print_endline(
      "OPC route "
      ++ label
      ++ " | gate "
      ++ d
      ++ (d == "NO !!v OBLIGATION" ? " (witness refused: no gate observed)" : "")
      ++ " | static errors "
      ++ string_of_int(List.length(o.errors))
      ++ " | full "
      ++ full,
    );
    RE.valid(label, o);
    check(string, label, expected, d);
    if (expected != "Structural") {
      check(bool, label ++ " not Proven", false, full == "Proven");
    };
  });

/* Generalize: the re-quantified binder keeps x's precise Ctx type. The
   witness gate `!!(acc + 1)` reads acc's class AFTER generalize + forall. */
let gen_typed =
  "theorem w = forall acc: Int -> exists (n : Int) -> n == acc + 1 proof generalize acc => forall acc => == true by witness acc + 1 end; axiom refl_eq at 0 on acc + 1 == acc + 1 end in 0";
let gen_typed_before =
  "theorem w = forall acc: Int -> exists (n : Int) -> n == acc + 1 proof == true by witness acc + 1 end; axiom refl_eq at 0 on acc + 1 == acc + 1 end in 0";
let gen_unknown =
  "theorem w = forall acc -> exists (n : Int) -> n == acc + 1 proof generalize acc => forall acc => == true by witness acc + 1 end; axiom refl_eq at 0 on acc + 1 == acc + 1 end in 0";
/* HISTORICAL (g1pC, fa1cbafb): this fixture was MALFORMED -- a stray
   third `end` gives the static error "Variable end is not bound", so its
   Pending gate was not a shadow observation. Kept verbatim as evidence. */
let gen_shadow_malformed =
  "theorem w = forall acc: Int -> exists (n : Int) -> n == acc + 1 proof generalize acc => forall acc => == true by witness case [acc] | acc :: _ => acc + 1 | _ => 0 end end; ? end in 0";
/* Corrected shadow pair (statics-clean): after `generalize acc => forall
   acc` the outer `acc : Int` is evidence; a CASE binder named `acc`
   shadows it with no class evidence (case binders are Unknown to the
   judgement), so `acc + 1` under the shadow fails closed. The companion
   differs only in the binder's name. */
let gen_shadow =
  "theorem w = forall acc: Int -> exists (n : Int) -> n == acc + 1 proof generalize acc => forall acc => == true by witness (case [acc] | acc :: _ => acc + 1 | _ => 0 end) end; ? in 0";
let gen_no_shadow =
  "theorem w = forall acc: Int -> exists (n : Int) -> n == acc + 1 proof generalize acc => forall acc => == true by witness (case [acc] | a :: _ => acc + 1 | _ => 0 end) end; ? in 0";
let gen_capture =
  "theorem w = forall acc: Int -> acc == 1 ==> acc == 1 proof assume acc == 1 as h => generalize acc => forall acc => == true by h end in 0";

/* ADDENDUM-6 (reviewer): application of a standard typed function
   variable needs evidence of its precise domain at the actual argument. */
let fn_untyped_arg =
  "theorem w = forall (f : Int -> Int) -> forall y -> exists (n : Int) -> n == f(y) proof == true by witness f(y) end; axiom refl_eq at 0 on f(y) == f(y) end in 0";
let fn_typed_arg =
  "theorem w = forall (f : Int -> Int) -> forall (y : Int) -> exists (n : Int) -> n == f(y) proof == true by witness f(y) end; axiom refl_eq at 0 on f(y) == f(y) end in 0";
let fn_literal_arg =
  "theorem w = forall (f : Int -> Int) -> exists (n : Int) -> n == f(0) proof == true by witness f(0) end; axiom refl_eq at 0 on f(0) == f(0) end in 0";
let fn_result_arith =
  "theorem w = forall (f : Int -> Int) -> exists (n : Int) -> n == f(0) + 1 proof == true by witness f(0) + 1 end; axiom refl_eq at 0 on f(0) + 1 == f(0) + 1 end in 0";

let positive = (label, src) =>
  test_case("positive " ++ label ++ ": std", `Quick, () => {
    let j = judge(src);
    print_endline("OPC " ++ label ++ " | judgement " ++ string_of_bool(j));
    check(bool, label, true, j);
  });

let sumh = "let sumh = fun xs -> case xs | [] => 0 | h :: t => h + sumh(t) end in\n";

let sumh_bool =
  sumh
  ++ "theorem w = exists (n : Int) -> n == sumh([true]) proof == true by witness sumh([true]) end; axiom refl_eq at 0 on sumh([true]) == sumh([true]) end in 0";

let lam_unknown =
  "theorem w = exists (g : Int -> Int) -> 0 == 0 proof == true by witness fun (y : ?) -> y + 1 end; axiom refl_eq at 0 on 0 == 0 end in 0";

let lam_typed =
  "theorem w = exists (g : Int -> Int) -> 0 == 0 proof == true by witness fun (x : Int) -> x + 1 end; axiom refl_eq at 0 on 0 == 0 end in 0";

/* Witness route: gate kind, statics, runtime, full status. A refused
   witness (no `!!v` obligation) is reported as "not reproduced". */
let route = (label, src, runtime_src, ~expect_structural) =>
  test_case(
    "route " ++ label ++ (expect_structural ? ": Structural" : ": gate not Structural, not Proven"),
    `Quick,
    () => {
      let (o, d) = RS.run(label, src);
      let full = RE.full(o, "w");
      print_endline(
        "OPC route "
        ++ label
        ++ " | gate "
        ++ d
        ++ (d == "NO !!v OBLIGATION" ? " (not reproduced: witness refused)" : "")
        ++ " | static errors "
        ++ string_of_int(List.length(o.errors))
        ++ " | full "
        ++ full,
      );
      Option.iter(s => ignore(RE.eval_print(s)), runtime_src);
      if (expect_structural) {
        RE.valid(label, o);
        check(string, label, "Structural", d);
      } else {
        check(bool, label ++ " gate not Structural", false, d == "Structural");
        check(bool, label ++ " not Proven", false, full == "Proven");
      };
    },
  );

let tests = (
  "ResetOperandClass",
  [
    route("sumh_bool_list", sumh_bool, Some(sumh ++ "sumh([true])"), ~expect_structural=false),
    route("unknown_param_arith_lambda", lam_unknown, Some("(fun (y : ?) -> y + 1)(true)"), ~expect_structural=false),
    route("typed_param_arith_lambda", lam_typed, None, ~expect_structural=true),
    negative("unknown_plus", "fun (y : ?) -> y + 1", "true"),
    negative("unknown_unary_minus", "fun (y : ?) -> -y", "true"),
    negative("unknown_not", "fun (y : ?) -> !y", "3"),
    negative("unknown_less", "fun (y : ?) -> y < 1", "true"),
    negative("unknown_and", "fun (y : ?) -> y && true", "3"),
    negative("unknown_eq_int_literal", "fun (y : ?) -> y == 1", "true"),
    negative("unknown_if_condition", "fun (y : ?) -> if y then 1 else 2", "3"),
    negative("unknown_concat_left", "fun (y : ?) -> y @ [1]", "2"),
    negative("unknown_cons_tail", "fun (y : ?) -> 1 :: y", "2"),
    /* Declared-type mismatches (statics-invalid programs; the judgement
       alone must still refuse them). */
    negative("typed_bool_in_int_plus", "fun (b : Bool) -> b + 1", "true"),
    negative("typed_int_not", "fun (x : Int) -> !x", "0"),
    negative("typed_int_if_condition", "fun (x : Int) -> if x then 1 else 2", "0"),
    negative("typed_int_cons_tail", "fun (x : Int) -> 1 :: x", "0"),
    /* LIMITATION control (fail-closed; NOT recursion-proof evidence): an
       application operand (`h + sumh(t)`) has no class evidence, so even a
       well-typed list argument stays Pending. */
    test_case("LIMITATION sumh([1, 2]): application operand gives no class evidence, Pending", `Quick, () => {
      let (o, d) =
        RS.run(
          "sumh_int_list_limitation",
          sumh
          ++ "theorem w = exists (n : Int) -> n == sumh([1, 2]) proof == true by witness sumh([1, 2]) end; axiom refl_eq at 0 on sumh([1, 2]) == sumh([1, 2]) end in 0",
        );
      RE.valid("sumh_int_list_limitation", o);
      check(string, "fail-closed limitation", "Pending", d);
    }),
    /* Generalize type preservation (final closure). */
    route_gate("generalize_typed_acc_before", gen_typed_before, "Structural"),
    route_gate("generalize_typed_acc_after", gen_typed, "Structural"),
    route_gate("generalize_unknown_acc_stays_unknown", gen_unknown, "Pending"),
    test_case("HISTORICAL malformed shadow fixture: static error, not a shadow observation", `Quick, () => {
      let o = RE.observe("generalize_shadow_malformed", gen_shadow_malformed);
      print_endline("OPC generalize_shadow_malformed | static errors " ++ string_of_int(List.length(o.errors)));
      check(bool, "statics reject it", true, o.errors != []);
    }),
    route_gate("generalize_case_binder_shadow", gen_shadow, "Pending"),
    route_gate("generalize_case_binder_no_shadow", gen_no_shadow, "Structural"),
    test_case("generalize capture: the assumed fact about the old acc is unavailable", `Quick, () => {
      let o = RE.observe("generalize_capture", gen_capture);
      print_endline("OPC generalize_capture | full " ++ RE.full(o, "w") ++ " | static errors " ++ string_of_int(List.length(o.errors)));
      check(bool, "not Proven", false, RE.full(o, "w") == "Proven");
    }),
    /* Standard typed function variable application (final closure +
       ADDENDUM-6). */
    route_gate("fn_var_untyped_arg", fn_untyped_arg, "Pending"),
    route_gate("fn_var_typed_arg", fn_typed_arg, "Structural"),
    route_gate("fn_var_literal_arg", fn_literal_arg, "Structural"),
    route_gate("fn_var_result_arith", fn_result_arith, "Structural"),
    fn_unit("h(0) + 1", "h(0) + 1", true),
    fn_unit("h(h(0)) + 1", "h(h(0)) + 1", true),
    fn_unit("h(x) == 0", "h(x) == 0", true),
    fn_unit("h(true) + 1 (literal domain mismatch)", "h(true) + 1", false),
    fn_unit("h(true) alone (domain mismatch)", "h(true)", false),
    fn_unit("fun (y : ?) -> h(y) (Unknown actual)", "fun (y : ?) -> h(y)", false),
    fn_unit("h(0) && true (codomain mismatch)", "h(0) && true", false),
    fn_unit("g(0) + 1 (imprecise arrow)", "g(0) + 1", false),
    fn_unit("g(0) alone (imprecise domain)", "g(0)", false),
    fn_unit("k(0) + 1 (function not standard)", "k(0) + 1", false),
    fn_unit("(fun z -> z)(0) + 1 (lambda result: no inference)", "(fun z -> z)(0) + 1", false),
    /* Core 21 companion: a STANDARD Int-typed variable is evidence; the
       same mark on an untyped `u` is not (core 21's original assertion
       stays as recorded). */
    fn_unit("x + 1 (x : Int, standard)", "x + 1", true),
    fn_unit("u + 1 (u standard, untyped)", "u + 1", false),
    positive("literal_arith", "1 + 2 * 3"),
    positive("literal_unary_minus", "-(4)"),
    positive("literal_not", "!true"),
    positive("literal_concat", "[1] @ [2]"),
    positive("typed_int_plus", "fun (x : Int) -> x + 1"),
    positive("typed_int_compare_unary", "fun (x : Int) -> -x < 3"),
    positive("typed_bool_not_and", "fun (b : Bool) -> !b && true"),
    positive("typed_int_eq", "fun (x : Int) -> x == 1"),
    positive("typed_int_if", "fun (x : Int) -> if x < 1 then 1 else 2"),
    positive("typed_list_concat", "fun (xs : [Int]) -> xs @ [1]"),
    positive("typed_list_cons_tail", "fun (xs : [Int]) -> 1 :: xs"),
    positive("exists_typed_binder_eq", "exists (n : Int) -> n == 5"),
  ],
);
