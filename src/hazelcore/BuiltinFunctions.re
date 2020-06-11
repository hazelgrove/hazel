let ctx: VarCtx.t = [
  ("length", HTyp.Arrow(String, Int)),
  ("string_of_int", Arrow(Int, String)),
  ("int_of_string", Arrow(String, Int)),
  ("string_of_bool", Arrow(Bool, String)),
  ("bool_of_string", Arrow(String, Bool)),
  ("string_of_float", Arrow(Float, String)),
  ("float_of_string", Arrow(String, Float)),
  ("int_of_float", Arrow(Float, Int)),
  ("float_of_int", Arrow(Int, Float)),
  ("equal", Arrow(String, Arrow(String, Bool))),
  ("compare", Arrow(String, Arrow(String, Int))),
  ("trim", Arrow(String, String)),
  ("escaped", Arrow(String, String)),
];

/* let shadowing_var = [("length", false), ("string_of_int", false)]; */

let lookup = x => VarMap.lookup(ctx, x);
/*
 let contains = x => VarMap.contains(ctx, x); */

/* let shadow_extend = x =>
   if (List.mem(x, shadowing_var) == false) {
     [x, ...shadowing_var];
   } else {
     shadowing_var;
   }; */

// let is_int_of_string = s =>

let evaluate = (x: string, d: DHExp.t): Dynamics.Evaluator.result =>
  switch (d) {
  | StringLit(s) =>
    switch (x) {
    | "length" => BoxedValue(IntLit(String.length(s)))
    | "int_of_string" => BoxedValue(IntLit(int_of_string(s)))
    | "bool_of_string" => BoxedValue(BoolLit(bool_of_string(s)))
    | "float_of_string" => BoxedValue(FloatLit(float_of_string(s)))
    | "trim" =>
      print_endline("TRIM s=" ++ s);
      BoxedValue(StringLit(String.trim(s)));
    | "escaped" =>
      print_endline("ESCAPED s=" ++ s);
      BoxedValue(StringLit(String.escaped(s)));
    | "equal" =>
      BoxedValue(Lam(Var(x), String, BoolLit(String.equal(s, x))))
    | "compare" =>
      BoxedValue(Lam(Var(x), String, IntLit(String.compare(s, x))))
    | _ => Indet(BuiltInLit(x), d)
    }
  | IntLit(n) =>
    switch (x) {
    | "string_of_int" => BoxedValue(StringLit(string_of_int(n)))
    | "float_of_int" => BoxedValue(FloatLit(float_of_int(n)))
    | _ => Indet(BuiltInLit(x), d)
    }
  | BoolLit(b) =>
    switch (x) {
    | "string_of_bool" => BoxedValue(StringLit(string_of_bool(b)))
    | _ => Indet(BuiltInLit(x), d)
    }
  | FloatLit(n) =>
    switch (x) {
    | "string_of_float" => BoxedValue(StringLit(string_of_float(n)))
    | "int_of_float" => BoxedValue(IntLit(int_of_float(n)))
    | _ => Indet(BuiltInLit(x), d)
    }
  | _ => Indet(BuiltInLit(x), d)
  };
