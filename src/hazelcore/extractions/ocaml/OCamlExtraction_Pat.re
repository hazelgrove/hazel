type t =
  | OcamlPat(string)
  | ExtractionFailed(string);

// Check whether a variable name is valid in ocaml, use standard library Re.Str
// should start with _ or lowercase
let ocaml_name_check = (name: Var.t): bool => {
  let re = Re.Str.regexp("^[_a-z][_a-zA-Z0-9]*$");
  Re.Str.string_match(re, name, 0);
};

// should write like "expand"
let rec extract = (dp: DHPat.t): t =>
  switch (dp) {
  | EmptyHole(_, _) => ExtractionFailed("Pat: Empty Hole")
  | NonEmptyHole(_, _, _, _) => ExtractionFailed("Pat: NonEmptyHole")
  | Wild => OcamlPat("_")
  | Keyword(_, _, _) => ExtractionFailed("Pat: Incomplete Program, Keyword")
  | InvalidText(_, _, _) => ExtractionFailed("Pat: InvalidText")
  | Var(s) =>
    // OCaml variable naming rule is same as Hazel
    if (ocaml_name_check(s)) {
      OcamlPat(s);
    } else {
      ExtractionFailed("Pat: Invalid OCaml variable name");
    }
  | IntLit(i) => OcamlPat(string_of_int(i))
  | FloatLit(f) => OcamlPat(string_of_float(f))
  | BoolLit(b) => OcamlPat(string_of_bool(b))
  // As the type design, we encode it to be "|`Left l | `Right r"
  // injection in pattern is used in pattern matching, injL matches the left side of type
  // Here we don't consider type... so it's easy
  | Inj(side, t) =>
    switch (extract(t)) {
    | OcamlPat(s) =>
      switch (side) {
      | L => OcamlPat("(`Left " ++ s ++ ")")
      | R => OcamlPat("(`Right " ++ s ++ ")")
      }
    | ExtractionFailed(s) => ExtractionFailed(s)
    }
  | ListNil => OcamlPat("[]")
  | Cons(dp1, dp2) =>
    switch (extract(dp1)) {
    | OcamlPat(s1) =>
      switch (extract(dp2)) {
      | OcamlPat(s2) => OcamlPat("(" ++ s1 ++ "::" ++ s2 ++ ")")
      | ExtractionFailed(s) => ExtractionFailed(s)
      }
    | ExtractionFailed(s) => ExtractionFailed(s)
    }
  | Pair(dp1, dp2) =>
    switch (extract(dp1)) {
    | OcamlPat(s1) =>
      switch (extract(dp2)) {
      | OcamlPat(s2) => OcamlPat("(" ++ s1 ++ ", " ++ s2 ++ ")")
      | ExtractionFailed(s) => ExtractionFailed(s)
      }
    | ExtractionFailed(s) => ExtractionFailed(s)
    }
  | Triv => OcamlPat("()")
  | Ap(dp1, dp2) =>
    switch (extract(dp1)) {
    | OcamlPat(s1) =>
      switch (extract(dp2)) {
      | OcamlPat(s2) => OcamlPat("(" ++ s1 ++ " " ++ s2 ++ ")")
      | ExtractionFailed(s) => ExtractionFailed(s)
      }
    | ExtractionFailed(s) => ExtractionFailed(s)
    }
  };

// take dp as a pattern, pat_t the expected type, and ctx the original context
// used to update the pattern in case
type ctx_update_t =
  | UpdateResult(Contexts.t)
  | UpdateFailed(string);

let rec update_pattern =
        (dp: DHPat.t, pat_t: HTyp.t, ctx: Contexts.t): ctx_update_t =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_) => UpdateFailed("Pat: Case wrong pattern")
  | Ap(_, _) => UpdateFailed("Pat: Case rule error, apply")
  | InvalidText(_, _, _) => UpdateFailed("Pat: Invalid Text")
  | Var(x) =>
    //I don't think we need to recheck it here...
    if (ocaml_name_check(x)) {
      UpdateResult(Contexts.extend_gamma(ctx, (x, pat_t)));
    } else {
      UpdateFailed("Pat: Invalid OCaml variable name");
    }
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
      UpdateFailed(
        "Pat: Injection pattern should have sum type where expected",
      )
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
      | Var(y) => UpdateResult(Contexts.extend_gamma(ctx1, (y, List(t))))
      | _ => UpdateResult(ctx1)
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
      | Var(y) => UpdateResult(Contexts.extend_gamma(ctx1, (y, t)))
      | _ => UpdateResult(ctx1)
      };
    | Prod([h, m, ...t]) =>
      let ctx1 =
        switch (p1) {
        | Var(x) => Contexts.extend_gamma(ctx, (x, h))
        | _ => ctx
        };
      switch (p2) {
      | Var(y) =>
        UpdateResult(Contexts.extend_gamma(ctx1, (y, Prod([m, ...t]))))
      | _ => UpdateResult(ctx1)
      };
    | _ => UpdateFailed("Exp: Case wrong rule pattern, pair")
    }

  | _ => UpdateResult(ctx) //don't need update context
  };
