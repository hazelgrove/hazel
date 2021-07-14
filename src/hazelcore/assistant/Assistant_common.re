/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 * Return a VarCtx.t
 */
let extract_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  ctx
  |> Contexts.gamma
  |> VarMap.filter(((_, ty)) => HTyp.consistent(ty, typ));
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

let get_type = CursorInfo_common.get_type;

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
