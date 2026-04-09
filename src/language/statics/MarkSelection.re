let rank = (m: Mark.t): int =>
  switch (m) {
  | BuiltinError(_) => 100
  | Free(_) => 95
  | IsBadPartialAp(_) => 92
  | InexhaustiveMatch(_) => 90
  | InvalidUseMode(_) => 88
  | BadTrivAp(_)
  | DotOperatorRequiresTuple
  | TupleExtensionRequiresTuples
  | LabelNotFound(_, _)
  | BadOperator(_)
  | BadLivelitModel(_)
  | BadTheorem(_) => 85
  | IsLivelitName(_) => 82
  | ExpectationMismatch(_) => 80
  | InvalidLabel(_, _) => 70
  | UnexpectedLabelSort(_) => 65
  | ExplicitNonlabel => 60
  | TPatShadowsType(_, _)
  | TPatNotAVar(_) => 50
  | TupleLabelError(_) => 45
  | IsDeferral(_) => 40
  | FreeConstructor(_) => 30
  | CompareFun(_) => 25
  | NoMeet(_) => 20
  | Redundant
  | ExpectedConstructor => 10
  | IsMulti => 10
  | DuplicateLabel(_, _)
  | DuplicateVar(_, _) => 0
  | BadLabel(_)
  | BadToken(_) => 75
  | TypFreeTypeVariable(_)
  | TypDuplicateConstructor(_)
  | TypDuplicateLabels(_, _)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(_)
  | TypWantConstructorFoundType(_)
  | TypWantConstructorFoundAp
  | TypParseFailure => 50
  };

let highest_ranked_mark = (marks: list(Mark.t)): option(Mark.t) =>
  switch (marks) {
  | [] => None
  | [h, ...tl] =>
    Some(
      List.fold_left(
        (best, cur) => rank(cur) > rank(best) ? cur : best,
        h,
        tl,
      ),
    )
  };
