let ctx: VarCtx.t = [
  ("length", HTyp.Arrow(String, Int)),
  ("string_of_int", HTyp.Arrow(Int, String)),
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

let evaluate = (x: string, d: DHExp.t): DHExp.t =>
  switch (d) {
  | StringLit(s) =>
    switch (x) {
    | "length" => IntLit(String.length(s))
    | _ => Triv
    }
  | IntLit(i) =>
    switch (x) {
    | "string_of_int" => StringLit(string_of_int(i))
    | _ => Triv
    }
  | _ => Triv
  };
