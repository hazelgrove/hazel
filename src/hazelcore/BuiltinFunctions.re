let ctx: VarCtx.t = [
  ("length", HTyp.Arrow(String, Int)),
  ("string_of_int", HTyp.Arrow(Int, String)),
];

let shadowing_ctx: VarCtx.t = [];

let lookup = x => VarMap.lookup(ctx, x);

let extend = x => VarMap.extend(shadowing_ctx, x);

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
