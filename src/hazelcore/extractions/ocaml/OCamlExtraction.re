type t =
  | OCamlExp(string)
  | ExtractionFailed(string);

//TODO: test exception handling (seems good now)
let extract = (ctx: Contexts.t, exp: DHExp.t): t =>
  switch (OCamlExtraction_Exp.extract(ctx, exp)) {
  // normal return value
  | item =>
    switch (item) {
    | OCamlExp(exp_str, _exp_typ) => OCamlExp(exp_str)
    }
  // Exception handling
  //TODO: make the exceptions more human readable
  // type extraction exceptions
  | exception OCamlExtraction_Typ.Typ_Hole =>
    ExtractionFailed("Type exception: Hole exists in program")
  | exception (OCamlExtraction_Typ.Typ_NotMatch(expression, required_type)) =>
    ExtractionFailed(
      "Type exception: Expression "
      ++ expression
      ++ " requires "
      ++ required_type
      ++ " type",
    )
  // pattern extraction exceptions

  | exception OCamlExtraction_Pat.Pat_Hole =>
    ExtractionFailed("Pattern exception: Hole exists in program")
  | exception (OCamlExtraction_Pat.Pat_Keyword(keyword)) =>
    ExtractionFailed(
      "Pattern exception: Keyword "
      ++ ExpandingKeyword.to_string(keyword)
      ++ " is not allowed",
    )
  | exception (OCamlExtraction_Pat.Pat_Invalid(err)) =>
    ExtractionFailed("Pattern exception: " ++ err ++ " is invalid in OCaml")
  | exception (OCamlExtraction_Pat.Pat_InvalidUpdate(err)) =>
    ExtractionFailed(
      "Pattern exception: Pattern format \""
      ++ err
      ++ "\" is invalid in declaration",
    )
  // expression extraction exceptions
  | exception OCamlExtraction_Exp.Exp_Hole =>
    ExtractionFailed("Expression exception: Hole exists in program")
  | exception (OCamlExtraction_Exp.Exp_Keyword(keyword)) =>
    ExtractionFailed(
      "Expression exception: Keyword "
      ++ ExpandingKeyword.to_string(keyword)
      ++ " is not allowed",
    )
  | exception (OCamlExtraction_Exp.Exp_Invalid(err_s)) =>
    ExtractionFailed(
      "Expression exception: "
      ++ err_s
      ++ " is invalid in OCaml, unable to extract",
    )
  | exception (OCamlExtraction_Exp.Exp_Fixpoint(s)) =>
    ExtractionFailed(
      "Expression exception: Fixpoint \""
      ++ s
      ++ "\" should be declared in Let expression",
    )
  };
