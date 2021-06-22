// mark with a constructor to tell apart with typ, exp
type t =
  | OCamlPat(string, Contexts.t);

exception Pat_Hole;
exception Pat_Keyword(ExpandingKeyword.t); //there's Keyword
exception Pat_Invalid(string);
//a pattern is not valid in updating
exception Pat_InvalidUpdate(string);

// the ocaml naming standard can be found in Chapter 7.3 of ocaml-manual (https://caml.inria.fr/pub/docs/manual-ocaml/names.html)
let ocaml_name_check = (name: Var.t): bool => {
  let re = Re.Str.regexp("^[_a-z][_a-zA-Z0-9]*$");
  Re.Str.string_match(re, name, 0);
};

// extract the pattern and update the context
// pat_t is the expected type of the pattern
let rec extract = (dp: DHPat.t, pat_t: HTyp.t, ctx: Contexts.t): t =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _) => raise(Pat_Hole)
  | Wild => OCamlPat("_", ctx)
  | Keyword(_, _, keyword) => raise(Pat_Keyword(keyword))
  | InvalidText(_, _, s) => raise(Pat_Invalid("text \"" ++ s ++ "\""))
  | Var(s) =>
    //check variable naming is valid
    if (ocaml_name_check(s)) {
      let ctx' = Contexts.extend_gamma(ctx, (s, pat_t));
      OCamlPat(s, ctx');
    } else {
      raise(Pat_Invalid("variable naming \"" ++ s ++ "\""));
    }
  | IntLit(i) => OCamlPat(string_of_int(i), ctx)
  | FloatLit(f) =>
    let str =
      switch (string_of_float(f)) {
      | "inf" => "infinity"
      | "-inf" => "neg_infinity"
      | "nan" => "nan"
      | s => s
      };
    OCamlPat(str, ctx);
  | BoolLit(b) => OCamlPat(string_of_bool(b), ctx)
  // As the type design, we encode it as "|`Left l | `Right r"
  // injection in pattern is used in pattern matching, injL matches the left side of type
  | Inj(side, t) =>
    switch (pat_t) {
    | Sum(tl, tr) =>
      switch (side) {
      | L =>
        let OCamlPat(pat, ctx') = extract(t, tl, ctx);
        OCamlPat("(`Left " ++ pat ++ ")", ctx');
      | R =>
        let OCamlPat(pat, ctx') = extract(t, tr, ctx);
        OCamlPat("(`Right " ++ pat ++ ")", ctx');
      }
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("injection", "sum"))
    }
  | ListNil => OCamlPat("[]", ctx)
  | Cons(dp1, dp2) =>
    switch (pat_t) {
    | List(t) =>
      // Recursively add sub-patterns
      let OCamlPat(pat_hd, ctx_hd) = extract(dp1, t, ctx);
      let OCamlPat(pat_tl, ctx_tl) = extract(dp2, List(t), ctx_hd);
      OCamlPat("(" ++ pat_hd ++ "::" ++ pat_tl ++ ")", ctx_tl);
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("cons", "list"))
    }
  | Pair(dp1, dp2) =>
    switch (pat_t) {
    // the base case, only two elements in a pair, dp2 is not a pair
    | Prod([h, t]) =>
      let OCamlPat(pat1, ctx1) = extract(dp1, h, ctx);
      let OCamlPat(pat2, ctx2) = extract(dp2, t, ctx1);
      OCamlPat("(" ++ pat1 ++ ", " ++ pat2 ++ ")", ctx2);
    // nested pairs, i.e. dp2 is also a pair
    | Prod([h, m, ...t]) =>
      let OCamlPat(pat1, ctx1) = extract(dp1, h, ctx);
      let OCamlPat(pat2, ctx2) = extract(dp2, Prod([m, ...t]), ctx1);
      OCamlPat("(" ++ pat1 ++ ", " ++ pat2 ++ ")", ctx2);
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("pair", "product"))
    }
  | Triv => OCamlPat("()", ctx)
  // The apply case only exists syntactically, but is illegal as a pattern
  | Ap(_dp1, _dp2) => raise(Pat_InvalidUpdate("apply"))
  };
