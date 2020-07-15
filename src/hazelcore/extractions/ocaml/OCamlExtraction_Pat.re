type t =
  | OcamlPat(string)
  | ExtractionFailed(string);

// Check whether a variable name is valid in ocaml, use standard library Re.Str
// should start with _ or lowercase
let ocaml_name_check = (name: Var.t): bool => {
  let re = Re.Str.regexp("^[_a-z][_a-zA-Z0-9]*$");
  Re.Str.string_match(re, name, 0);
};

let rec extract = (dp: DHPat.t): t =>
  switch (dp) {
  | EmptyHole(_, _) => ExtractionFailed("Pat: Empty Hole")
  | NonEmptyHole(_, _, _, _) => ExtractionFailed("Pat: NonEmptyHole")
  | Wild => OcamlPat("_")
  | Keyword(_, _, _) => ExtractionFailed("Pat: Incomplete Program, Keyword")
  | InvalidText(_, _, _) => ExtractionFailed("Pat: InvalidText")
  | Var(s) =>
    //check variable naming is valid
    //FIXME: I think check is unnecessary here
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
// used to update the pattern in case or let
type ctx_update_t =
  | UpdateResult(Contexts.t)
  | UpdateFailed(string);

let rec update_pattern =
        (dp: DHPat.t, pat_t: HTyp.t, ctx: Contexts.t): ctx_update_t =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_) => UpdateFailed("Pat: Context Update wrong pattern")
  | Ap(_, _) => UpdateFailed("Pat: Context Update error, apply")
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
      // Recursively add sub-patterns.
      // TODO: Test them (passed now)
      switch (update_pattern(p1, t, ctx)) {
      | UpdateFailed(err) => UpdateFailed(err)
      | UpdateResult(ctx1) => update_pattern(p2, List(t), ctx1)
      }
    | _ => UpdateFailed("Pat: Cons are not list as Expected")
    }
  | Pair(p1, p2) =>
    switch (pat_t) {
    | Prod([h, t]) =>
      switch (update_pattern(p1, h, ctx)) {
      | UpdateFailed(err) => UpdateFailed(err)
      | UpdateResult(ctx1) => update_pattern(p2, t, ctx1)
      }
    | Prod([h, m, ...t]) =>
      switch (update_pattern(p1, h, ctx)) {
      | UpdateFailed(err) => UpdateFailed(err)
      | UpdateResult(ctx1) => update_pattern(p2, Prod([m, ...t]), ctx1)
      }
    | _ => UpdateFailed("Pat: Pairs are not as Expected")
    }
  | Triv
  | Wild
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil => UpdateResult(ctx) //don't need update context
  };
