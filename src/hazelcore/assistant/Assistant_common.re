[@deriving sexp]
type mode =
  | Analytic
  | Synthetic
  | UnknownMode;

/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 * Return a VarCtx.t
 */
let extract_vars = (ctx: Contexts.t, ty: HTyp.t) => {
  ctx
  |> Contexts.gamma
  |> VarMap.filter(((_, ty1: HTyp.t)) =>
       HTyp.consistent(Contexts.tyvars(ctx), ty1, ty)
     );
};

/**
   * Filter the variables that are functions that have the correct resulting type
   */
let fun_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  let rec compatible_funs = right_ty =>
    if (HTyp.consistent(Contexts.tyvars(ctx), right_ty, typ)) {
      true;
    } else {
      switch (right_ty) {
      | Arrow(_, right_ty) => compatible_funs(right_ty)
      | _ => false
      };
    };
  let can_extract = ((_, ty: HTyp.t)) => {
    switch (ty) {
    | Arrow(_, t2) => compatible_funs(t2)
    | _ => false
    };
  };
  ctx |> Contexts.gamma |> VarMap.filter(can_extract);
};

/**
   * Collates expected type, actual type,
   * and mode information from CursorInfo
   */
let rec get_types_and_mode = (typed: CursorInfo.typed) => {
  switch (typed) {
  | AnaAnnotatedLambda(expected, actual)
  | AnaTypeInconsistent(expected, actual)
  | AnaSubsumed(expected, actual) => (
      Some(expected),
      Some(actual),
      Analytic,
    )

  | AnaWrongLength(_, _, expected)
  | AnaFree(expected)
  | AnaInvalid(expected)
  | AnaKeyword(expected, _)
  | Analyzed(expected) => (Some(expected), None, Analytic)

  | SynErrorArrow(_, actual)
  | SynMatchingArrow(actual, _) => (Some(Hole), Some(actual), Synthetic)

  | SynFreeArrow(actual)
  | SynKeywordArrow(actual, _)
  | SynInvalidArrow(actual)
  | Synthesized(actual) => (Some(Hole), Some(actual), Synthetic)

  | SynInvalid
  | SynFree
  | SynKeyword(_) => (Some(Hole), Some(Hole), Synthetic)

  | SynBranchClause(join, typed, _) =>
    switch (join, typed) {
    | (JoinTy(ty), Synthesized(got_ty)) =>
      if (HTyp.normalized_consistent(ty, got_ty)) {
        (Some(Hole), Some(got_ty), Synthetic);
      } else {
        (Some(ty), Some(got_ty), Synthetic);
      }
    | _ => get_types_and_mode(typed)
    }
  | SynInconsistentBranchesArrow(_, _)
  | SynInconsistentBranches(_, _) => (Some(Hole), Some(Hole), Synthetic)

  | PatAnaTypeInconsistent(expected, actual)
  | PatAnaSubsumed(expected, actual) => (
      Some(expected),
      Some(actual),
      Analytic,
    )

  | PatAnaWrongLength(_, _, expected)
  | PatAnaInvalid(expected)
  | PatAnaKeyword(expected, _)
  | PatAnalyzed(expected) => (Some(expected), None, Analytic)

  | PatSynthesized(actual) => (Some(Hole), Some(actual), Synthetic)

  | PatSynKeyword(_) => (Some(Hole), Some(Hole), Synthetic)

  | OnTPat(status_opt) =>
    switch (status_opt) {
    | None => (None, None, UnknownMode)
    // | Some(BuiltInType(bt)) =>
    //   let actual: HTyp.t =
    //     switch (bt) {
    //     | Bool => Bool
    //     | Float => Float
    //     | Int => Int
    //     };
    //   (None, Some(actual), UnknownMode);
    | Some(InHole(_)) => (None, None, UnknownMode)
    | Some(NotInHole) => (None, None, UnknownMode)
    }
  | OnTPatHole => (None, Some(Hole), UnknownMode)

  | TypKeyword(_) => (None, None, UnknownMode)
  | TypFree => (None, Some(Hole), UnknownMode)
  | OnType(_)
  | OnNonLetLine
  | OnRule => (None, None, UnknownMode)
  };
};

/**
   * Gets the type of the expression at the cursor.
   * Return HTyp.t
   */
let get_type = (cursor_info: CursorInfo.t): option(HTyp.t) => {
  let (expected_ty, actual_ty, mode) = get_types_and_mode(cursor_info.typed);
  switch (mode) {
  | Analytic => expected_ty
  | Synthetic => actual_ty
  | UnknownMode => None
  };
};

let get_mode = (cursor_info: CursorInfo.t) => {
  let (_, _, mode) = get_types_and_mode(cursor_info.typed);
  mode;
};

let valid_assistant_term = (term: CursorInfo.cursor_term): bool => {
  CursorInfo_common.on_empty_expr_hole(term)
  || CursorInfo_common.on_expr_var(term);
};

/**
 * Gets the type in string format.
 * Return string
 */
let type_to_str = (ty: HTyp.t) => {
  switch (ty) {
  | Hole => "a"
  | TyVarHole(_) => "a"
  | TyVar(_, name) => "a " ++ TyVar.Name.to_string(name)
  | Int => "an Integer"
  | Float => "a Float"
  | Bool => "a Boolean"
  | Arrow(_, _) => "a Function"
  | Sum(_, _) => "a Sum"
  | Prod(_) => "a Product"
  | List(_) => "a List"
  };
};

/**
 * Extacts a text for of the current cursor term, suitable for search
 *  or filtering. Currently only supports Vars.
*/
let term_to_str = (term: CursorInfo.cursor_term): string => {
  switch (term) {
  | ExpOperand(_, Var(_, _, s)) => s
  | _ => ""
  };
};
