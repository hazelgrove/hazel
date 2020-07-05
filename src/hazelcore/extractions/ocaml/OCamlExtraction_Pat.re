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
  // As the type design, we encode it to be "|`Left l | `Right r"
  // injection in pattern is used in pattern matching, injL matches the left side of type
  // Here we don't consider type... so it's easy
  | Inj(side, t) =>
    switch (side) {
    | L => "(`Left " ++ extract(~dp=t) ++ ")"
    | R => "(`Right " ++ extract(~dp=t) ++ ")"
    }
  | ListNil => "[]"
  | Cons(dp1, dp2) =>
    //:: has very low priority in OCaml, so we currently don't need to parenthesize two elements
    "(" ++ extract(~dp=dp1) ++ "::" ++ extract(~dp=dp2) ++ ")"
  | Pair(dp1, dp2) =>
    "(" ++ extract(~dp=dp1) ++ ", " ++ extract(~dp=dp2) ++ ")"
  | Triv => "()"
  | Ap(dp1, dp2) =>
    // same as cons
    "(" ++ extract(~dp=dp1) ++ " " ++ extract(~dp=dp2) ++ ")"
  };
};

// take dp as a pattern, pat_t the expected type, and ctx the original context
// used to update the pattern in case
let rec update_pattern =
        (dp: DHPat.t, pat_t: HTyp.t, ctx: Contexts.t): Contexts.t =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_) => failwith("Exp: Case wrong pattern")
  | Ap(_, _) => failwith("Exp: Case rule error, apply")
  | Var(x) => Contexts.extend_gamma(ctx, (x, pat_t))
  | Inj(side, p) =>
    // we should ensure that it's a sum type expected
    // FIXME: does we really need L,R to be neither Hole type?
    switch (pat_t) {
    | Sum(tl, tr) =>
      switch (side) {
      | L => update_pattern(p, tl, ctx)
      | R => update_pattern(p, tr, ctx)
      }
    | _ =>
      failwith("Pat: Injection pattern should have sum type where expected")
    }
  | Cons(p1, p2) =>
    switch (pat_t) {
    | List(t) =>
      // only add variable into context
      let ctx1 =
        switch (p1) {
        | Var(x) => Contexts.extend_gamma(ctx, (x, t))
        | _ => ctx
        };
      switch (p2) {
      | Var(y) => Contexts.extend_gamma(ctx1, (y, List(t)))
      | _ => ctx1
      };
    | _ => failwith("Exp: Case wrong rule pattern, list")
    }
  //TODO: rewrite it more beautiful
  | Pair(p1, p2) =>
    switch (pat_t) {
    | Prod([h, t]) =>
      let ctx1 =
        switch (p1) {
        | Var(x) => Contexts.extend_gamma(ctx, (x, h))
        | _ => ctx
        };
      switch (p2) {
      | Var(y) => Contexts.extend_gamma(ctx1, (y, t))
      | _ => ctx1
      };
    | Prod([h, m, ...t]) =>
      let ctx1 =
        switch (p1) {
        | Var(x) => Contexts.extend_gamma(ctx, (x, h))
        | _ => ctx
        };
      switch (p2) {
      | Var(y) => Contexts.extend_gamma(ctx1, (y, Prod([m, ...t])))
      | _ => ctx1
      };
    | _ => failwith("Exp: Case wrong rule pattern, pair")
    }

  | _ => ctx //don't need update context
  };
