open Test_Statics_Slicing_Prelude;
open Language;
open Util;

let customer = "type CustomerId = + CustomerId(Int) in ";

let col = "type Col(R, A) = + Col(String) in type Orders = + Orders in ";

let app = "type App(F, A) = + App(?) in type Eval = + Eval in ";

let ctor_ap = (name: string, e: Exp.t): Id.t =>
  first(
    "application of " ++ name,
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Ap(_, fn, _) =>
          switch (Exp.term_of(fn)) {
          | Constructor(c, _) => c == name
          | _ => false
          }
        | _ => false
        },
      e,
    ),
  );

let type_ap = (e: Exp.t): Id.t =>
  first(
    "type application",
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | TypAp(_) => true
        | _ => false
        },
      e,
    ),
  );

let use_of = (name: string, e: Exp.t): Id.t => exp_var(e, name);

let total = col ++ "let total : Col(Orders, Float) = Col(\"total\") in total";

let three =
  app ++ "let three : poly F -> App(F, Int) = abs F -> App(3) in three@<Eval>";

let tests = (
  "Statics.Slicing.NewType",
  [
    synthesis_case(
      ~focus=ctor_ap("CustomerId"),
      "nominal-wrap",
      customer ++ "CustomerId(3)",
      "CustomerId",
      {|type CustomerId = + CustomerId(?) in CustomerId(?)|},
    ),
    synthesis_case(
      "nominal-unwrap-payload",
      customer ++ "case CustomerId(3) | CustomerId(n) => n end",
      "Int",
      {|type CustomerId = + CustomerId(Int) in case ? | CustomerId(n) => n end|},
    ),
    synthesis_case(
      ~focus=use_of("total"),
      "nominal-phantom-full",
      total,
      "Col(Orders, Float)",
      {|type Col(?, ?) = ? in type Orders = ? in let total:(Col(Orders, Float)) = ? in total|},
    ),
    synthesis_case(
      ~focus=use_of("total"),
      "nominal-phantom-arg-query",
      total,
      "Col(?, Float)",
      {|type Col(?, ?) = ? in type ? = ? in let total:(Col(?, Float)) = ? in total|},
    ),
    synthesis_case(
      ~focus=use_of("total"),
      "nominal-phantom-head-query",
      total,
      "Col(?, ?)",
      {|type Col(?, ?) = ? in type ? = ? in let total:(Col(?, ?)) = ? in total|},
    ),
    synthesis_case(
      ~focus=type_ap,
      "nominal-brand-hk-query",
      three,
      "App(?, Int)",
      {|type App(?, ?) = ? in type ? = ? in let three:(poly F -> App(?, Int)) = ? in three @< ? >|},
    ),
    synthesis_case(
      ~focus=type_ap,
      "nominal-brand-only",
      three,
      "App(Eval, ?)",
      {|type App(?, ?) = ? in type Eval = ? in let three:(poly F -> App(F, ?)) = ? in three @< Eval >|},
    ),
    synthesis_case(
      ~focus=use_of("m"),
      "nominal-multi-binder-type-args",
      "type M(C) = + M(Int) in type G = + G in type Q(D, S) = + Q(Int) in type Db = + Db in let s : poly D, R, S -> Int -> Q(D, S) = ? in let m = s@<Db, Int, (a = String, b = M(G))>(1) in m",
      "Q(Db, (a = String, b = M(G)))",
      {|type M(?) = ? in type G = ? in type Q(?, ?) = ? in type Db = ? in let s:(poly D, R, S -> ? -> Q(D, S)) = ? in let m = s @< Db, ?, (a=String, b=M(G)) > (?) in m|},
    ),
  ],
);
