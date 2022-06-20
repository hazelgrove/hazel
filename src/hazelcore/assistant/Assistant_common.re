[@deriving sexp]
type mode =
  | Analytic
  | Synthetic
  | UnknownMode;

/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 */
let extract_vars = (ctx: Context.t, ty: HTyp.t): list((Var.t, HTyp.t)) =>
  Context.vars(ctx)
  |> List.filter_map(((_, x, ty_x)) =>
       HTyp.consistent(ctx, ty_x, ty) ? Some((x, ty_x)) : None
     );

/**
   * Filter the variables that are functions that have the correct resulting type
   */
let fun_vars = (ctx: Context.t, ty: HTyp.t): list((Var.t, HTyp.t)) => {
  let rec compatible_funs = (right_ty: HTyp.t) =>
    if (HTyp.consistent(ctx, right_ty, ty)) {
      true;
    } else {
      switch (HTyp.head_normalize(ctx, right_ty)) {
      | Arrow(_, right_ty) => compatible_funs(right_ty)
      | _ => false
      };
    };
  let can_extract = ((_, _, ty: HTyp.t)) => {
    switch (HTyp.head_normalize(ctx, ty)) {
    | Arrow(_, ty2) => compatible_funs(ty2)
    | _ => false
    };
  };
  Context.vars(ctx)
  |> List.filter_map(((_, x: Var.t, ty: HTyp.t) as var) =>
       can_extract(var) ? Some((x, ty)) : None
     );
};

/**
   * Collates expected type, actual type,
   * and mode information from CursorInfo
   */
let rec get_types_and_mode = (typed: CursorInfo.typed) => {
  switch (typed) {
  | AnaAnnotatedFun(_, expected, actual)
  | AnaTypeInconsistent(expected, actual)
  | AnaSubsumed(_, expected, actual) => (
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
  | SynMatchingArrow(actual, _) => (
      Some(HTyp.hole()),
      Some(actual),
      Synthetic,
    )

  | SynFreeArrow(actual)
  | SynKeywordArrow(actual, _)
  | SynInvalidArrow(actual)
  | Synthesized(actual) => (Some(HTyp.hole()), Some(actual), Synthetic)

  | SynInvalid
  | SynFree
  | SynKeyword(_) => (Some(HTyp.hole()), Some(HTyp.hole()), Synthetic)

  | SynBranchClause(join, typed, _, ctx) =>
    switch (join, typed) {
    | (JoinTy(ty), Synthesized(got_ty)) =>
      if (HTyp.consistent(ctx, ty, got_ty)) {
        (Some(HTyp.hole()), Some(got_ty), Synthetic);
      } else {
        (Some(ty), Some(got_ty), Synthetic);
      }
    | _ => get_types_and_mode(typed)
    }
  | SynInconsistentBranchesArrow(_, _)
  | SynInconsistentBranches(_, _) => (
      Some(HTyp.hole()),
      Some(HTyp.hole()),
      Synthetic,
    )

  | PatAnaTypeInconsistent(expected, actual)
  | PatAnaSubsumed(_, expected, actual) => (
      Some(expected),
      Some(actual),
      Analytic,
    )

  | PatAnaWrongLength(_, _, expected)
  | PatAnaInvalid(expected)
  | PatAnaKeyword(expected, _)
  | PatAnalyzed(expected) => (Some(expected), None, Analytic)

  | PatSynthesized(actual) => (Some(HTyp.hole()), Some(actual), Synthetic)

  | PatSynKeyword(_) => (Some(HTyp.hole()), Some(HTyp.hole()), Synthetic)

  | OnTPat(_) => (None, None, UnknownMode)
  | OnTPatHole => (None, Some(HTyp.hole()), UnknownMode)

  | TypKeyword(_) => (None, None, UnknownMode)
  | TypFree
  | TypInvalid => (None, Some(HTyp.hole()), UnknownMode)

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
 * Extacts a text for of the current cursor term, suitable for search
 *  or filtering. Currently only supports Vars.
*/
let term_to_str = (term: CursorInfo.cursor_term): string => {
  switch (term) {
  | ExpOperand(_, Var(_, _, s)) => s
  | _ => ""
  };
};
