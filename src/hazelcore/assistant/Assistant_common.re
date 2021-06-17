open OptUtil.Syntax;

[@deriving sexp]
type mode =
  | Analytic
  | Synthetic
  | UnknownMode;

type cursor_info_pro = {
  expected_ty: HTyp.t,
  actual_ty: option(HTyp.t),
  mode,
  term: CursorInfo.cursor_term,
  ctx: Contexts.t,
  uses: option(UsageAnalysis.uses_list),
  u_gen: MetaVarGen.t,
  syntactic_context: CursorInfo.syntactic_context,
};

/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 * Return a VarCtx.t
 */
let extract_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  ctx
  |> Contexts.gamma
  |> VarMap.filter(((_, ty: HTyp.t)) => HTyp.consistent(ty, typ));
};

/**
   * Filter the variables that are functions that have the correct resulting type
   */
let fun_vars = (ctx: Contexts.t, typ: HTyp.t) => {
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

  | SynErrorArrow(expected, actual)
  | SynMatchingArrow(expected, actual) => (
      Some(expected),
      Some(actual),
      Synthetic,
    )

  | SynFreeArrow(actual)
  | SynKeywordArrow(actual, _)
  | SynInvalidArrow(actual)
  | Synthesized(actual) => (Some(Hole), Some(actual), Synthetic)

  | SynInvalid
  | SynFree
  | SynKeyword(_) => (Some(Hole), None, Synthetic)

  | SynBranchClause(join, typed, _) =>
    switch (join, typed) {
    | (JoinTy(ty), Synthesized(got_ty)) =>
      switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
      | (true, _) => (Some(Hole), None, Synthetic)
      | _ => (None, None, Synthetic)
      }
    | (NoBranches, _) => get_types_and_mode(typed)
    | _ => (None, None, Synthetic)
    }

  | _ => (None, None, UnknownMode)
  };
};

/**
   * Gets the type of the expression at the cursor.
   * Return HTyp.t
   */
let get_type = (cursor_info: CursorInfo.t): option(HTyp.t) => {
  let (expected_ty, _, _) = get_types_and_mode(cursor_info.typed);
  let+ expected_ty = expected_ty;
  expected_ty;
};

let get_mode = (cursor_info: CursorInfo.t) => {
  let (_, _, mode) = get_types_and_mode(cursor_info.typed);
  mode;
};

let on_empty_expr_hole: CursorInfo.cursor_term => bool =
  fun
  | Exp(_, EmptyHole(_)) => true
  | Exp(_, _) => false
  | Pat(_, EmptyHole(_)) => false
  | Pat(_, _) => false
  | Typ(_, Hole) => false
  | Typ(_, _) => false
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _)
  | Line(_, _)
  | Rule(_, _) => false;

/* TODO(andrew): Make this function less conceptually bullshit */
let on_textable_expr: CursorInfo.cursor_term => bool =
  fun
  | ExpOp(_, _) // TODO(andrew)
  | Exp(_, InvalidText(_))
  | Exp(_, IntLit(_))
  | Exp(_, FloatLit(_))
  | Exp(_, BoolLit(_))
  | Exp(_, Var(_)) => true
  | Exp(_, _) => false
  | _ => false;

let valid_assistant_term = (term: CursorInfo.cursor_term): bool => {
  on_empty_expr_hole(term) || on_textable_expr(term);
};

let promote_cursor_info =
    (
      {cursor_term, typed, ctx, uses, syntactic_context}: CursorInfo.t,
      u_gen: MetaVarGen.t,
    )
    : option(cursor_info_pro) => {
  let (expected_ty, actual_ty, mode) = get_types_and_mode(typed);
  let+ expected_ty = expected_ty;
  {
    expected_ty,
    actual_ty,
    mode,
    u_gen,
    term: cursor_term,
    ctx,
    uses,
    syntactic_context,
  };
};

/**
   * Gets the type in string format.
   * Return string
   */
let type_to_str = (~empty_hole=false, ty: option(HTyp.t)) => {
  switch (ty) {
  | Some(Hole) => empty_hole ? "" : "a"
  | Some(Int) => "Integer"
  | Some(Float) => "Float"
  | Some(Bool) => "Boolean"
  | Some(Arrow(_, _)) => "Function"
  | Some(Sum(_, _)) => "Sum"
  | Some(Prod(_)) => "Product"
  | Some(List(_)) => "List"
  | _ => raise(Invalid_argument("No Literal"))
  };
};

// TODO: expand to other forms?
let term_to_str = (term: CursorInfo.cursor_term): string => {
  switch (term) {
  | Exp(_, Var(_, _, s))
  | Exp(_, InvalidText(_, s))
  | Exp(_, IntLit(_, s))
  | Exp(_, FloatLit(_, s)) => s
  | Exp(_, BoolLit(_, b)) => string_of_bool(b)
  | _ => ""
  };
};

let get_action_index = (assistant_selection: option(int), actions): int => {
  let num_actions = List.length(actions);
  switch (assistant_selection) {
  | None => 0
  | Some(i) =>
    let z = num_actions == 0 ? 0 : i mod num_actions;
    z + (z < 0 ? num_actions : 0);
  };
};
