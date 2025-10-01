open Util;
open Language;

/* Print static errors to a string */

[@deriving (show({with_path: false}), yojson, sexp)]
type t =
  | ParseError(string)
  | StaticErrors(list(string))
  | NoErrors;

let remove_projectors = (segment: Segment.t) =>
  //TODO: Remove this when splices is merged
  ZipperBase.MapPiece.of_segment(
    fun
    | Projector(pr) => [pr.syntax]
    | x => [x],
    segment,
  );

module Print = {
  let seg = (~holes, segment: Segment.t): string => {
    let segment = remove_projectors(segment);
    Printer.of_segment(
      ~holes,
      ~measured=Measured.of_segment(segment, Id.Map.empty),
      ~caret=None,
      ~indent=" ",
      segment,
    );
  };

  let term = (term: Any.t): string => {
    let settings =
      ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off);
    term |> ExpToSegment.any_to_pretty(~settings) |> seg(~holes="");
  };

  let typ = (ty: Typ.t): string => term(Typ(ty));
};

let prn = Printf.sprintf;

let common_error: Info.error_common => string =
  fun
  | NoType(BadLabel(_)) => "Invalid label"
  | NoType(InvalidLabel(_)) => "Invalid label"
  | DuplicateLabel(_, _) => "Duplicate label"
  | TupleLabelError(_) => "Invalid tuple label"
  | NoType(UnexpectedLabelSort(_)) => "Unexpected label sort"
  | NoType(BadToken(token)) => prn("\"%s\" isn't a valid token", token)
  | Inconsistent(WithArrow(ty)) =>
    prn("type %s is not consistent with arrow type", Print.typ(ty))
  | Inconsistent(CompareFun(ty)) =>
    prn("values of type %s cannot be compared", Print.typ(ty))
  | NoType(FreeConstructor(_name)) => prn("Constructor is not defined")
  | Inconsistent(Internal(tys)) =>
    prn(
      "Expecting branches to have consistent types but got types: %s",
      List.map(Print.typ, tys) |> String.concat(", "),
    )
  | Inconsistent(Expectation({ana, syn})) =>
    prn(
      "Expecting type %s but got inconsistent type %s",
      Print.typ(ana),
      Print.typ(syn),
    );

let exp_error: Info.error_exp => string =
  fun
  | DotOperatorRequiresTuple => "Expected a tuple"
  | TupleExtensionRequiresTuples => "Expected tuples for both arguments"
  | BuiltinError(ArgumentMustBeTuple) => "Argument must be a tuple"
  | BuiltinError(ProjectLabelsMissingLabels(labels)) =>
    prn(
      "Projected tuple does not have the following labels: %s",
      String.concat(", ", labels),
    )
  | BuiltinError(MissingLabels(labels)) =>
    prn(
      "Tuple does not have the following labels: %s",
      String.concat(", ", labels),
    )
  | BuiltinError(ToLvsMissingLabelsOnTuple(ty)) =>
    prn(
      "All entries in the argument must have labels, but some were not provided: %s",
      Print.typ(ty),
    )
  | BuiltinError(AtLeast2Arguments) => "Must have 2 or more direct arguments"
  | BuiltinError(Exactly2Arguments) => "Must have exactly 2 direct arguments"
  | BuiltinError(ArgumentMustBeListOfTuples) => "Argument must be a list of labeled tuples"
  | BuiltinError(PivotLabelIsNotString(ty)) =>
    prn("Pivot column must be a string, but got: %s", Print.typ(ty))
  | LabelNotFound(_, _) => "Label not found"
  | UnboundLivelit(_) => "Livelit unbound and not found"
  | BadTrivAp(ty) =>
    prn("Function argument type \"%s\" inconsistent with ()", Print.typ(ty))
  | InvalidUseMode({bad_typ, _}) =>
    prn(
      "Cannot use type %s for number operators and literals.",
      Print.typ(bad_typ),
    )
  | BadOperator(_) => "Invalid operator"
  | BadLivelitModel(_) => "Bad internal livelit model"
  | FreeVariable(name) => "Variable " ++ name ++ " is not bound"
  | InexhaustiveMatch(_) => "Match is not exhaustive" //TODO: elaborate
  | UnusedDeferral => "Unused deferral" //TODO: better message
  | BadPartialAp(_) => "Bad partial application" //TODO: elaborate
  | BadTheorem(typ) =>
    prn("Theorem pattern is not of the form p : t, got %s", Print.typ(typ))
  | Common(error) => common_error(error);

let pat_error: Info.error_pat => string =
  fun
  | ExpectedConstructor => "Expected a constructor"
  | Redundant(_) => "Redundant" //TODO: elaborate
  | Common(error) => common_error(error);

let typ_error: Info.error_typ => string =
  fun
  | FreeTypeVariable(name) => prn("Type variable %s is not bound", name)
  | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
  | WantConstructorFoundAp => "Expected a constructor, found application"
  | WantConstructorFoundType(ty) =>
    prn("Expected a constructor, found type %s", Print.typ(ty))
  | WantTypeFoundAp => "Constructor application must be in sum"
  | DuplicateConstructor(name) =>
    prn("Constructor %s already used in this sum", name)
  | WantLabel => "Expected a label"
  | ParseFailure => "Parse failure"
  | InvalidLabel(name, labels) =>
    prn(
      "Label %s is not valid. Valid labels are: %s",
      name,
      String.concat(", ", labels),
    )
  | DuplicateLabels(labels, ty) =>
    prn(
      "Duplicate labels in type %s: %s",
      Print.typ(ty),
      String.concat(", ", labels),
    )
  | Duplicate(name, _) => prn("Type %s is already defined", name)
  | WantProduct(ty) =>
    prn("Expected a tuple type, found type %s", Print.typ(ty));

let underdetermined_typ: Info.underdetermined_typ => string =
  fun
  | ProdExtensionUnderdetermined(tys) =>
    prn(
      "Cannot determine type of tuple extension with argument types: %s",
      List.map(Print.typ, tys) |> String.concat(", "),
    )
  | ProdProjectionMissingLabel(label, labels) =>
    prn(
      "Cannot project label %s. Valid labels are: %s",
      label,
      String.concat(", ", labels),
    )
  | ProdProjectionBadArgs({product, label}) =>
    prn(
      "Cannot determine projection type because %s",
      String.concat(
        " and ",
        [
          switch (product) {
          | Some(ty) => "Type is not a tuple type: " ++ Print.typ(ty)
          | None => ""
          },
          switch (label) {
          | Some(ty) => "Label is not a valid label: " ++ Print.typ(ty)
          | None => ""
          },
        ],
      ),
    );

let tpat_error: Info.error_tpat => string =
  fun
  | NotAVar(_) => "Not a valid type name" //TODO: elaborate
  | ShadowsType(name, _source) => "Can't shadow type " ++ name; //TODO: elaborate

let string_of: Info.error => string =
  fun
  | Exp(error) => exp_error(error)
  | Pat(error) => pat_error(error)
  | Typ(error) => typ_error(error)
  | TPat(error) => tpat_error(error);

let format_error = (term, error) =>
  prn("Error in term:\n  %s\nNature of error: %s", term, error);

let term_string_of: Info.t => string =
  fun
  | InfoExp({term, _}) => Print.term(Exp(term))
  | InfoPat({term, _}) => Print.term(Pat(term))
  | InfoTyp({term, _}) => Print.term(Typ(term))
  | InfoTPat({term, _}) => Print.term(TPat(term))
  | Secondary(_) => failwith("ChatLSP: term_string_of: Secondary");

let all = (info_map: Statics.Map.t): list(string) => {
  Id.Map.fold(
    (_id: Id.t, info: Info.t, acc) =>
      switch (Info.error_of(info)) {
      | None => acc
      | Some(_) => [info] @ acc
      },
    info_map,
    [],
  )
  |> List.sort_uniq(compare)
  |> List.filter_map(info =>
       switch (Info.error_of(info)) {
       | None => None
       | Some(error) =>
         let term = term_string_of(info);
         Some(format_error(term, string_of(error)));
       }
     );
};
