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
