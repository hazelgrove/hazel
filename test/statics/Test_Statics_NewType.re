open Alcotest;
open Language;

let parse_menhir_exp = (src: string): TermBase.Exp.t =>
  MenhirParser.Interface.parse_program(src)
  |> MenhirParser.Conversion.Exp.of_menhir_ast
  |> Grammar.map_exp_annotation(_ => IdTagged.IdTag.mk_internal([Id.mk()]));

let error_count = (src: string): int =>
  Test_Statics_Prelude.(
    parse_menhir_exp(src)
    |> statics
    |> errors
    |> List.map(snd)
    |> List.flatten
  )
  |> List.length;

let accepts = (name, src) =>
  test_case(name, `Quick, () =>
    check(int, "static errors", 0, error_count(src))
  );

let rejects = (name, src) =>
  test_case(name, `Quick, () =>
    check(bool, "has static errors", true, error_count(src) > 0)
  );

let keys = {|
type CustomerId = + CustomerId(Int) in
type OrderId = + OrderId(Int) in
|};

let tests = (
  "Statics.NewType",
  [
    accepts(
      "constructor wraps and pattern unwraps",
      keys
      ++ {|
let c : CustomerId = CustomerId(3) in
let raw : Int = case c | CustomerId(n) => n end in
raw
|},
    ),
    rejects(
      "same representation, different nominal types",
      keys ++ {|
let o : OrderId = CustomerId(3) in o
|},
    ),
    rejects(
      "nominal type is not its representation",
      keys ++ {|
let n : Int = CustomerId(3) in n
|},
    ),
    rejects(
      "representation is not the nominal type",
      keys ++ {|
let c : CustomerId = 3 in c
|},
    ),
    accepts(
      "unknown is consistent with a nominal type",
      keys
      ++ {|
let c : CustomerId = ? in
let d : ? = c in
let e : CustomerId = d in e
|},
    ),
    accepts(
      "transparent alias of a nominal type",
      keys
      ++ {|
type Key = CustomerId in
let k : Key = CustomerId(1) in
let c : CustomerId = k in c
|},
    ),
    accepts(
      "structural aliases with equal bodies stay interchangeable",
      {|
type A = Int in
type B = Int in
let a : A = 1 in
let b : B = a in b
|},
    ),
    rejects(
      "phantom parameter survives",
      {|
type Col(R, A) = + Col(String) in
type Orders = + Orders in
let total : Col(Orders, Float) = Col("total") in
let wrong : Col(Orders, Int) = total in wrong
|},
    ),
    accepts(
      "phantom parameter at equal arguments",
      {|
type Col(R, A) = + Col(String) in
type Orders = + Orders in
let total : Col(Orders, Float) = Col("total") in
let same : Col(Orders, Float) = total in same
|},
    ),
    accepts(
      "phantom parameter meets unknown argument",
      {|
type Col(R, A) = + Col(String) in
type Orders = + Orders in
let total : Col(Orders, Float) = Col("total") in
let partial : Col(Orders, ?) = total in partial
|},
    ),
    rejects(
      "brands distinguish higher-kinded applications",
      {|
type App(F, A) = + App(?) in
type Eval = + Eval in
type Pretty = + Pretty in
let x : App(Eval, Int) = App(1) in
let y : App(Pretty, Int) = x in y
|},
    ),
    accepts(
      "brand polymorphism at kind Type",
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
three@<Eval>(ev)
|},
    ),
    accepts(
      "case over a parameterised nominal type",
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
