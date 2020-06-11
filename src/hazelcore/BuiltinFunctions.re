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
  ("assert", Arrow(Bool, Prod([]))),
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

let evaluate = (x: string, d: DHExp.t): DHExp.t =>
  switch (d) {
  | StringLit(s) =>
    switch (x) {
    | "length" => IntLit(String.length(s))
    | "int_of_string" => IntLit(int_of_string(s))
    | "bool_of_string" => BoolLit(bool_of_string(s))
    | "float_of_string" => FloatLit(float_of_string(s))
    | "trim" =>
      print_endline("TRIM s=" ++ s);
      StringLit(String.trim(s));
    | "escaped" =>
      print_endline("ESCAPED s=" ++ s);
      StringLit(String.escaped(s));
    | "equal" => Lam(Var(x), String, BoolLit(String.equal(s, x)))
    | "compare" => Lam(Var(x), String, IntLit(String.compare(s, x)))
    | _ => StringLit("Failed")
    }
  | IntLit(n) =>
    switch (x) {
    | "string_of_int" => StringLit(string_of_int(n))
    | "float_of_int" => FloatLit(float_of_int(n))
    | _ => StringLit("Failed")
    }
  | BoolLit(b) =>
    switch (x) {
    | "string_of_bool" => StringLit(string_of_bool(b))
    | "assert" => b ? Triv : StringLit("assertion")
    | _ => StringLit("Failed")
    }
  | FloatLit(n) =>
    switch (x) {
    | "string_of_float" => StringLit(string_of_float(n))
    | "int_of_float" => IntLit(int_of_float(n))
    | _ => StringLit("Failed")
    }
  | _ => StringLit("Failed")
  };
