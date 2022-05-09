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
let extract_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  print_endline("consistent: extract_vars");
  ctx
  |> Contexts.gamma
  |> VarMap.filter(((_, ty: HTyp.t)) => {HTyp.consistent(ty, typ)});
};

/**
   * Filter the variables that are functions that have the correct resulting type
   */
let fun_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  print_endline("consistent: fun_vars");
  let rec compatible_funs = right_ty =>
    if (HTyp.consistent(right_ty, typ)) {
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
  | AnaAnnotatedFun(expected, actual)
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
  | SynMatchingArrow(actual, _) => (
      Some(Unknown(Internal)),
      Some(actual),
      Synthetic,
    )

  | SynFreeArrow(actual)
  | SynKeywordArrow(actual, _)
  | SynInvalidArrow(actual)
  | Synthesized(actual) => (
      Some(Unknown(Internal)),
      Some(actual),
      Synthetic,
    )

  | SynInvalid
  | SynFree
  | SynKeyword(_) => (
      Some(Unknown(Internal)),
      Some(Unknown(Internal)),
      Synthetic,
    )

  | SynBranchClause(join, typed, _) =>
    switch (join, typed) {
    | (JoinTy(ty), Synthesized(got_ty)) =>
      print_endline("consistent: get_types_and_mode");
      if (HTyp.consistent(ty, got_ty)) {
        (Some(Unknown(Internal)), Some(got_ty), Synthetic);
      } else {
        (Some(ty), Some(got_ty), Synthetic);
      };
    | _ => get_types_and_mode(typed)
    }
  | SynInconsistentBranchesArrow(_, _)
  | SynInconsistentBranches(_, _) => (
      Some(Unknown(Internal)),
      Some(Unknown(Internal)),
      Synthetic,
    )

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

  | PatSynthesized(actual) => (
      Some(Unknown(Internal)),
      Some(actual),
      Synthetic,
    )

  | PatSynKeyword(_) => (
      Some(Unknown(Internal)),
      Some(Unknown(Internal)),
      Synthetic,
    )

  | OnType
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
  | ModeSwitch
  | Unknown(_) => "a"
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
