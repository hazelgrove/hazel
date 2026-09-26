open Test_Statics_Prelude;
open FTemp;
open Typ;

/* fun x : Int -> let f1 = fun x : Int -> ... in f1(x):
   n let-bound functions, each holding the next. Every let used to analyze
   its definition up to four times, so this took 4^n passes. */
let nested_function_lets = n => {
  let rec go = i =>
    i > n
      ? "x"
      : Printf.sprintf(
          "let f%d = fun x : Int -> %s in f%d(x)",
          i,
          go(i + 1),
          i,
        );
  "let f0 = fun x : Int -> " ++ go(1) ++ " in f0(1)";
};

let tests = (
  "Statics.Let",
  [
    synthesizes(
      "inexhaustive let synthesizes its body type (#1671)",
      {|let [x] = [1] in 1|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "exhaustive let synthesizes its body type",
      {|let x = [1] in 1|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "a let-bound function",
      {|let f = fun x -> x + 1 in f(2)|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "a let-bound function that calls itself is still recursive",
      {|let f = fun n -> if n == 0 then 0 else f(n - 1) in f(3)|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "a let-bound function shadowing an earlier one",
      {|let f = fun x -> x in let f = fun y -> y + 1 in f(2)|},
      Some(int()),
    ),
    Alcotest.test_case(
      "twelve nested let-bound functions check in a few seconds",
      `Quick,
      () => {
        let code = nested_function_lets(12);
        let exp = parse_exp(code);
        let t0 = Sys.time();
        let s = statics(exp);
        let dt = Sys.time() -. t0;
        Alcotest.check(
          Alcotest.int,
          "no static errors",
          0,
          List.length(errors(s)),
        );
        Alcotest.check(
          Alcotest.option(testable_typ),
          "type",
          Some(int()),
          synthesized_type(s, exp),
        );
        /* It took over 300 s when each let analyzed its definition up to
           four times. */
        Alcotest.check(
          Alcotest.bool,
          Printf.sprintf("took %.1f s", dt),
          true,
          dt < 10.,
        );
      },
    ),
  ],
);
