open Language;

/* Print static errors to a string */

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
      ~measured=
        Measured.of_segment(
          segment,
          ProjectorCore.Shape.Map.empty,
          Id.Map.empty,
        ),
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

let core_mark_string = (ctx: Ctx.t, ana: Typ.t, m: Mark.t): string => {
  let ana = Statics.ana_skip_explicit_nonlabel(ana);
  let expectation = (ana: Typ.t, syn: Typ.t) =>
    prn(
      "Expecting type %s but got inconsistent type %s",
      Print.typ(ana),
      Print.typ(syn),
    );
  switch (m) {
  | BadLabel(_)
  | InvalidLabel(_, _) => "Invalid label"
  | DuplicateLabel(_, _) => "Duplicate label"
  | DuplicateVar(_, _) => "Duplicate variable"
  | TupleLabelError(_) => "Invalid tuple label"
  | UnexpectedLabelSort(_) => "Unexpected label sort"
  | BadToken(token) => prn("\"%s\" isn't a valid token", token)
  | IsMulti => "Broken expression"
  | CompareFun(ty) =>
    prn("values of type %s cannot be compared", Print.typ(ty))
  | FreeConstructor(_name) => prn("Constructor is not defined")
  | ExpectationMismatch({ana, syn}) => expectation(ana, syn)
  | NoMeet(PolyEq, tys)
  | NoMeet(_, tys) when ana.term == Unknown(SynSwitch) =>
    prn(
      "Expecting branches to have consistent types but got types: %s",
      List.map(Print.typ, Typ.of_source(tys)) |> String.concat(", "),
    )
  | NoMeet(wrap, _tys) =>
    let syn: Typ.t = SynTy.meet_of(wrap, Unknown(Internal) |> Typ.temp);
    switch (Typ.meet(ctx, ana, syn)) {
    | Some(_) => "(static error)"
    | None =>
      switch (ana.term, syn.term) {
      | (Label(_), _) => "Invalid label"
      | _ => expectation(ana, syn)
      }
    };
  | ExplicitNonlabel => "(static error)"
  | _ => "(static error)"
  };
};

let drv_error: DrvInfo.error => string =
  fun
  | DrvInfo.BadToken(token) => prn("\"%s\" isn't a valid token", token)
  | DrvInfo.MultiHole => "Multiple holes in term"
  | DrvInfo.FreeVar => "Free variable"
  | DrvInfo.VarNoJoin(expect, actual) =>
    prn(
      "Expecting variable to have a derivation sort %s but got %s",
      DrvSort.to_string(expect),
      Print.typ(actual),
    )
  | DrvInfo.NoJoin(expect, actuals) =>
    prn(
      "Expecting terms to have a derivation sort of %s but got potential derivation sorts: %s",
      DrvSort.to_string(expect),
      List.map(DrvSort.to_string, actuals) |> String.concat(", "),
    );

let exp_mark_to_string = (ctx: Ctx.t, ana: Typ.t, m: Mark.t): string => {
  let common_from_core = () => core_mark_string(ctx, ana, m);
  switch (m) {
  | Free(name) => "Variable " ++ name ++ " is not bound"
  | InexhaustiveMatch(_) => "Match is not exhaustive"
  | IsDeferral(InAp) => "(internal)"
  | IsDeferral(_) => "Unused deferral"
  | IsBadPartialAp(NoDeferredArgs) => "Bad partial application"
  | IsBadPartialAp(ArityMismatch(_)) => "Bad partial application"
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
  | InvalidUseMode({bad_typ, _}) =>
    prn(
      "Cannot use type %s for number operators and literals.",
      Print.typ(bad_typ),
    )
  | BadTrivAp(ty) =>
    prn("Function argument type \"%s\" inconsistent with ()", Print.typ(ty))
  | DotOperatorRequiresTuple => "Expected a module or tuple"
  | TupleExtensionRequiresTuples => "Expected tuples for both arguments"
  | BadOperator(_) => "Invalid operator"
  | BadLivelitModel(_) => "Bad internal livelit model"
  | BadTheorem(typ) =>
    prn("Theorem pattern is not of the form p : t, got %s", Print.typ(typ))
  | LabelNotFound(_, _) => "Label not found"
  | ModuleMissingMembers(names) =>
    prn("Module is missing members: %s", String.concat(", ", names))
  | ModuleMemberNotFound({name, members, type_member}) =>
    if (type_member) {
      prn(
        "%s is a type member of the module, not a value; use it in a type position",
        name,
      );
    } else {
      switch (members) {
      | [] => prn("Module has no member %s; it has no members", name)
      | _ =>
        prn(
          "Module has no member %s; its members are %s",
          name,
          String.concat(", ", members),
        )
      };
    }
  | ModuleTypeMemberMismatch({name, expected, actual}) =>
    prn(
      "Type member %s is %s but its signature declares %s",
      name,
      Print.typ(actual),
      Print.typ(expected),
    )
  | IsLivelitName({name, _}) =>
    switch (Ctx.lookup_livelit(ctx, name)) {
    | None => "Livelit unbound and not found"
    | Some(_) => "(internal)"
    }
  | TypFreeTypeVariable(_)
  | TypDuplicateConstructor(_)
  | TypDuplicateLabels(_, _)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(_)
  | ModuleTypeMemberNotFound(_)
  | TypWantModule(_)
  | TypWantConstructorFoundType(_)
  | TypWantConstructorFoundAp
  | TypParseFailure
  | TPatShadowsType(_)
  | TPatNotAVar(_) => "(internal)"
  | Redundant
  | ExpectedConstructor => "(internal)"
  | FreeConstructor(_)
  | BadToken(_)
  | BadLabel(_)
  | ExplicitNonlabel
  | UnexpectedLabelSort(_)
  | InvalidLabel(_, _)
  | TupleLabelError(_)
  | IsMulti
  | DuplicateLabel(_, _)
  | DuplicateVar(_, _)
  | ExpectationMismatch(_)
  | NoMeet(_)
  | CompareFun(_) => common_from_core()
  };
};

let pat_marks_to_string =
    (ctx: Ctx.t, ana: Typ.t, marks: list(Mark.t)): string =>
  switch (marks) {
  | [Redundant, ...tl] =>
    switch (Mark.highest(tl)) {
    | None => "Redundant"
    | Some(m) => core_mark_string(ctx, ana, m) ++ "; pattern is redundant"
    }
  | [ExpectedConstructor, ..._] => "Expected a constructor"
  | _ =>
    switch (Mark.highest(marks)) {
    | None => "(static error)"
    | Some(m) => core_mark_string(ctx, ana, m)
    }
  };

let typ_mark_string: Mark.t => string =
  fun
  | TypFreeTypeVariable(name) => prn("Type variable %s is not bound", name)
  | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
  | TypWantConstructorFoundAp => "Expected a constructor, found application"
  | TypWantConstructorFoundType(ty) =>
    prn("Expected a constructor, found type %s", Print.typ(ty))
  | TypWantTypeFoundAp => "Constructor application must be in sum"
  | TypDuplicateConstructor(name) =>
    prn("Constructor %s already used in this sum", name)
  | TypWantLabel => "Expected a label"
  | TypParseFailure => "Parse failure"
  | InvalidLabel(name, labels) =>
    prn(
      "Label %s is not valid. Valid labels are: %s",
      name,
      String.concat(", ", labels),
    )
  | TypDuplicateLabels(labels, ty) =>
    prn(
      "Duplicate labels in type %s: %s",
      Print.typ(ty),
      String.concat(", ", labels),
    )
  | DuplicateLabel(name, _) => prn("Type %s is already defined", name)
  | TypWantProduct(ty) =>
    prn("Expected a module or tuple type, found type %s", Print.typ(ty))
  | ModuleTypeMemberNotFound({name, members, submodule}) => {
      let what = submodule ? "sub-module" : "type member";
      switch (members) {
      | [] => prn("Module has no %s %s; it has no %ss", what, name, what)
      | _ =>
        prn(
          "Module has no %s %s; its %ss are %s",
          what,
          name,
          what,
          String.concat(", ", members),
        )
      };
    }
  | TypWantModule({name, typ}) =>
    prn("%s is a value of type %s, not a module", name, Print.typ(typ))
  | _ => "(static error)";

let tpat_mark_string: Mark.t => string =
  fun
  | TPatNotAVar(_) => "Not a valid type name" //TODO: elaborate
  | TPatShadowsType(name, _) => "Can't shadow type " ++ name //TODO: elaborate
  | _ => "(static error)";

let string_of_marks = (info: Info.t, marks: list(Mark.t)): string =>
  switch (info) {
  | InfoDrv(drv) =>
    switch (DrvInfo.error_of(drv)) {
    | Some(err) => drv_error(err)
    | None => "(static error)"
    }
  | InfoExp({ctx, ana, _}) =>
    switch (Mark.highest(marks)) {
    | Some(m) => exp_mark_to_string(ctx, ana, m)
    | None => "(static error)"
    }
  | InfoPat({ctx, ana, _}) => pat_marks_to_string(ctx, ana, marks)
  | InfoTyp(_) =>
    switch (marks) {
    | [] => "(static error)"
    | ms =>
      switch (Mark.highest(ms)) {
      | Some(m) => typ_mark_string(m)
      | None => ""
      }
    }
  | InfoTPat(_) =>
    switch (marks) {
    | [] => "(static error)"
    | ms =>
      switch (Mark.highest(ms)) {
      | Some(m) => tpat_mark_string(m)
      | None => ""
      }
    }
  | _ => "(static error)"
  };

let format_error = (term, error) =>
  prn("Error in term:\n  %s\nNature of error: %s", term, error);

let term_string_of: Info.t => string =
  fun
  | InfoDrv({term, _}) => Print.term(Drv(term))
  | InfoExp({user_term, _}) => Print.term(Exp(user_term))
  | InfoPat({user_term, _}) => Print.term(Pat(user_term))
  | InfoTyp({user_term, _}) => Print.term(Typ(user_term))
  | InfoTPat({user_term, _}) => Print.term(TPat(user_term))
  | InfoMod({user_term, _}) => Print.term(Mod(user_term))
  | InfoSig({user_term, _}) => Print.term(Sig(user_term))
  | InfoMPat({user_term, _}) => Print.term(MPat(user_term))
  | Secondary(_) => failwith("ChatLSP: term_string_of: Secondary");

let all = (info_map: Statics.Map.t): list(string) => {
  Id.Map.fold(
    (_id: Id.t, info: Info.t, acc) =>
      Info.is_error(info) ? [info, ...acc] : acc,
    info_map,
    [],
  )
  |> List.sort_uniq(compare)
  |> List.map(info => {
       let term = term_string_of(info);
       format_error(term, string_of_marks(info, Info.marks_of(info)));
     });
};
