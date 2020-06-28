type t = string;
// should write like "expand"
let rec extract = (~dp: DHPat.t): t => {
  switch (dp) {
  | EmptyHole(_, _) => failwith("Pat: Empty Hole")
  | NonEmptyHole(_, _, _, _) => failwith("Pat: NonEmptyHole")
  | Wild => "_"
  | Keyword(_, _, _) => failwith("Pat: Incomplete Program, Keyword")
  | Var(s) => s
  | IntLit(i) => string_of_int(i)
  | FloatLit(f) => string_of_float(f)
  | BoolLit(b) => string_of_bool(b)
  | Inj(_side, t) => extract(~dp=t)
  | ListNil => "[]"
  | Cons(dp1, dp2) => extract(~dp=dp1) ++ "::" ++ extract(~dp=dp2)
  | Pair(dp1, dp2) =>
    "(" ++ extract(~dp=dp1) ++ ", " ++ extract(~dp=dp2) ++ ")"
  | Triv => "()"
  | Ap(dp1, dp2) => extract(~dp=dp1) ++ " " ++ extract(~dp=dp2)
  };
};
