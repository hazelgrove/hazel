// mark with a constructor to tell apart with typ, exp
type t =
  | OCamlPat(string);

exception Pat_Hole;
exception Pat_Keyword(ExpandingKeyword.t); //there's Keyword
exception Pat_Invalid(string);

// the ocaml naming standard can be found in Chapter 7.3 of ocaml-manual (https://caml.inria.fr/pub/docs/manual-ocaml/names.html)
let ocaml_name_check = (name: Var.t): bool => {
  let re = Re.Str.regexp("^[_a-z][_a-zA-Z0-9]*$");
  Re.Str.string_match(re, name, 0);
};

let rec extract = (dp: DHPat.t): t =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _) => raise(Pat_Hole)
  | Wild => OCamlPat("_")
  | Keyword(_, _, keyword) => raise(Pat_Keyword(keyword))
  | InvalidText(_, _, s) => raise(Pat_Invalid("text \"" ++ s ++ "\""))
  | Var(s) =>
    //check variable naming is valid
    if (ocaml_name_check(s)) {
      OCamlPat(s);
    } else {
      raise(Pat_Invalid("variable naming \"" ++ s ++ "\""));
    }
  | IntLit(i) => OCamlPat(string_of_int(i))
  | FloatLit(f) => OCamlPat(string_of_float(f))
  | BoolLit(b) => OCamlPat(string_of_bool(b))
  // As the type design, we encode it as "|`Left l | `Right r"
  // injection in pattern is used in pattern matching, injL matches the left side of type
  | Inj(side, t) =>
    let OCamlPat(pat) = extract(t);
    switch (side) {
    | L => OCamlPat("(`Left " ++ pat ++ ")")
    | R => OCamlPat("(`Right " ++ pat ++ ")")
    };
  | ListNil => OCamlPat("[]")
  | Cons(dp1, dp2) =>
    let OCamlPat(pat1) = extract(dp1);
    let OCamlPat(pat2) = extract(dp2);
    OCamlPat("(" ++ pat1 ++ "::" ++ pat2 ++ ")");
  | Pair(dp1, dp2) =>
    let OCamlPat(pat1) = extract(dp1);
    let OCamlPat(pat2) = extract(dp2);
    OCamlPat("(" ++ pat1 ++ ", " ++ pat2 ++ ")");
  | Triv => OCamlPat("()")
  | Ap(dp1, dp2) =>
    let OCamlPat(pat1) = extract(dp1);
    let OCamlPat(pat2) = extract(dp2);
    OCamlPat("(" ++ pat1 ++ " " ++ pat2 ++ ")");
  };

//a pattern is not valid in updating
exception Pat_InvalidUpdate(string);

// take a pattern, the expected type, and the original context, return a updated context with the pattern
// used in Let, Case, Lambda or similar things
let rec update_pattern =
        (dp: DHPat.t, pat_t: HTyp.t, ctx: Contexts.t): Contexts.t =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_) => raise(Pat_Hole)
  | Keyword(_, _, keyword) => raise(Pat_Keyword(keyword))
  | Ap(_, _) => raise(Pat_InvalidUpdate("apply"))
  | InvalidText(_, _, _) => raise(Pat_Invalid("text"))
  | Var(s) =>
    if (ocaml_name_check(s)) {
      Contexts.extend_gamma(ctx, (s, pat_t));
    } else {
      raise(Pat_Invalid("variable naming \"" ++ s ++ "\""));
    }
  | Inj(side, p) =>
    // update a side of the pattern to context
    switch (pat_t) {
    | Sum(tl, tr) =>
      switch (side) {
      | L => update_pattern(p, tl, ctx)
      | R => update_pattern(p, tr, ctx)
      }
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("injection", "sum"))
    }
  | Cons(dp1, dp2) =>
    // update the h,t to context for [h,...t]
    switch (pat_t) {
    | List(t) =>
      // Recursively add sub-patterns.
      let ctx' = update_pattern(dp1, t, ctx);
      update_pattern(dp2, List(t), ctx');
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("cons", "list"))
    }
  | Pair(dp1, dp2) =>
    // update the p1, p2 to context for (p1, p2)
    switch (pat_t) {
    // the base case, only two elements in a pair
    | Prod([h, t]) =>
      let ctx' = update_pattern(dp1, h, ctx);
      update_pattern(dp2, t, ctx');
    // nested pairs
    | Prod([h, m, ...t]) =>
      let ctx' = update_pattern(dp1, h, ctx);
      update_pattern(dp2, Prod([m, ...t]), ctx');
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("pair", "product"))
    }
  | Triv
  | Wild
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil => ctx //don't need update context
  };
