open OptUtil.Syntax;

[@deriving sexp]
type mode =
  | Analytic
  | Synthetic;

type cursor_info_pro = {
  ty: HTyp.t,
  mode,
  term: CursorInfo.cursor_term,
  ctx: Contexts.t,
  uses: option(UsageAnalysis.uses_list),
  u_gen: MetaVarGen.t,
};

/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 * Return a VarCtx.t
 */
let extract_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  let (vars, _) = ctx;
  let can_extract = ((_, ty: HTyp.t)) => {
    HTyp.consistent(ty, typ);
  };
  vars |> VarMap.filter(can_extract);
};

/**
   * Filter the variables that are functions that have the correct resulting type
   */
let fun_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  let (vars, _) = ctx;
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
  vars |> VarMap.filter(can_extract);
};

let rec get_type_and_mode = (typed: CursorInfo.typed) => {
  switch (typed) {
  | Analyzed(ty) => Some((ty, Analytic))
  | AnaAnnotatedLambda(expected_ty, _) => Some((expected_ty, Analytic))
  | AnaSubsumed(expected_ty, _) => Some((expected_ty, Analytic))
  | Synthesized(ty) => Some((ty, Synthetic))
  | SynMatchingArrow(syn_ty, _) => Some((syn_ty, Synthetic))
  | SynBranchClause(join, typed, _) =>
    switch (join, typed) {
    | (JoinTy(ty), Synthesized(got_ty)) =>
      switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
      | (true, _) => Some((ty, Synthetic))
      | _ => None
      }
    | (NoBranches, _) => get_type_and_mode(typed)
    | _ => None
    }
  | _ => None
  };
};

/**
   * Gets the type of the expression at the cursor.
   * Return HTyp.t
   */
let get_type = (cursor_info: CursorInfo.t) => {
  let+ (ty, _) = get_type_and_mode(cursor_info.typed);
  ty;
};

let get_mode = (cursor_info: CursorInfo.t) => {
  let+ (_, mode) = get_type_and_mode(cursor_info.typed);
  mode;
};

let promote_cursor_info =
    ({cursor_term, typed, ctx, uses}: CursorInfo.t, u_gen: MetaVarGen.t)
    : cursor_info_pro => {
  let (ty, mode) =
    get_type_and_mode(typed)
    |> OptUtil.get(_ => failwith("promote_cursor_info failed"));
  {ty, mode, u_gen, term: cursor_term, ctx, uses};
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
