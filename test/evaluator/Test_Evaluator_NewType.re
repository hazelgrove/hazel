open Alcotest;
open Test_Evaluator_Prelude;

let evaluates = (name, expected, src) =>
  test_case(name, `Quick, () =>
    parse_and_evaluate_test(
      ~msg=name,
      ~ignore_constructor_types=true,
      expected,
      src,
    )
  );

let tests = (
  "Evaluator.NewType",
  [
    evaluates(
      "wrap then unwrap",
      "3",
      {|
type CustomerId = + CustomerId(Int) in
let c : CustomerId = CustomerId(3) in
case c | CustomerId(n) => n end
|},
    ),
    evaluates(
      "phantom parameter carries no data",
      {|"total"|},
      {|
type Col(R, A) = + Col(String) in
type Orders = + Orders in
let total : Col(Orders, Float) = Col("total") in
case total | Col(s) => s end
|},
    ),
    evaluates(
      "brand-indexed interpreter",
      "3",
      {|
type App(F, A) = + App(?) in
type Eval = + Eval in
type Sym(F) = (int = Int -> App(F, Int), add = App(F, Int) -> App(F, Int) -> App(F, Int)) in
let ev : Sym(Eval) = (
  int = fun n -> App(n),
  add = fun a -> fun b -> case (a, b) | (App(x), App(y)) => App(x + y) end
) in
let three : poly F -> Sym(F) -> App(F, Int) =
  abs F -> fun s -> s.add(s.int(1))(s.int(2)) in
let prj : App(Eval, Int) -> Int = fun a -> case a | App(x) => x end in
prj(three@<Eval>(ev))
|},
    ),
    evaluates(
      "parameterised nominal record payload",
      "true",
      {|
type Expr(R, A) = + Expr((sql = String, ev = R -> A)) in
let run : poly R -> poly A -> Expr(R, A) -> R -> A =
  abs R -> abs A -> fun e -> fun r ->
    case e | Expr(x) => x.ev(r) end in
run@<Int>@<Bool>(Expr((sql = "x > 0", ev = fun n -> n > 0)))(3)
|},
    ),
  ],
);
