/*
 ERRORS TODO:
 make multihole an error (say something about ap)
 do a completeness check
  */

let prn = Printf.sprintf;

let common_error: Info.error_common => string =
  fun
  | NoType(MultiError) =>
    /* NOTE: possible cause explanation actually helps.
       e.g. when generating
       "if i == index then (description, not(done)) else (description, done)"
       it would tend not to parethesize the argument to not
        */
    prn(
      "Incomplete syntax (possible cause: remember that function application is c-style and requires parentheses around the argument)",
    )

  | NoType(BadToken(token)) => prn("\"%s\" isn't a valid token", token)
  | NoType(BadTrivAp(ty)) =>
    prn(
      "Function argument type \"%s\" inconsistent with ()",
      Typ.to_string(ty),
    )
  | Inconsistent(WithArrow(ty)) =>
    prn("type %s is not consistent with arrow type", Typ.to_string(ty))
  | NoType(FreeConstructor(_name)) => prn("Constructor is not defined")
  | Inconsistent(Internal(tys)) =>
    prn(
      "Expecting branches to have consistent types but got types: %s",
      List.map(Typ.to_string, tys) |> String.concat(", "),
    )
  | Inconsistent(Expectation({ana, syn})) =>
    prn(
      "Expecting type %s but got inconsistent type %s",
      Typ.to_string(ana),
      Typ.to_string(syn),
    );

let exp_error: Info.error_exp => string =
  fun
  | FreeVariable(name) => "Variable " ++ name ++ " is not bound"
  | Common(error) => common_error(error);

let pat_error: Info.error_pat => string =
  fun
  | ExpectedConstructor => "Expected a constructor"
  | Common(error) => common_error(error);

let typ_error: Info.error_typ => string =
  fun
  | FreeTypeVariable(name) => prn("Type variable %s is not bound", name)
  | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
  | WantConstructorFoundAp => "Expected a constructor, found application"
  | WantConstructorFoundType(ty) =>
    prn("Expected a constructor, found type %s", Typ.to_string(ty))
  | WantTypeFoundAp => "Constructor application must be in sum"
  | DuplicateConstructor(name) =>
    prn("Constructor %s already used in this sum", name);

let tpat_error: Info.error_tpat => string =
  fun
  | NotAVar(_) => "Not a valid type name"
  | ShadowsType(name) => "Can't shadow type " ++ name;

let string_of: Info.error => string =
  fun
  | Exp(error) => exp_error(error)
  | Pat(error) => pat_error(error)
  | Typ(error) => typ_error(error)
  | TPat(error) => tpat_error(error);

let format_error = (term, error) =>
  prn("Error in term:\n  %s\nNature of error: %s", term, error);

let collect_static = (info_map: Statics.Map.t): list(string) => {
  let errors =
    Id.Map.fold(
      (_id, info: Info.t, acc) =>
        switch (Info.error_of(info)) {
        | None => acc
        | Some(_) => [info] @ acc
        },
      info_map,
      [],
    );
  let errors = List.sort_uniq(compare, errors);
  List.filter_map(
    info =>
      switch (Info.error_of(info)) {
      | None => None
      | Some(error) =>
        let term = Info.term_string_of(info);
        Some(format_error(term, string_of(error)));
      },
    errors,
  );
};
