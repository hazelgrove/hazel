open Assistant_common;

[@deriving sexp]
type generator = Suggestion.generator;

let mk_operand_suggestion =
    (
      ~strategy: Suggestion.operand_strategy,
      ~operand: UHExp.operand,
      ci: CursorInfo.t,
    )
    : Suggestion.t =>
  Suggestion.mk(~strategy=Suggestion.ReplaceOperand(strategy, operand), ~ci);

let mk_operand_suggestion_from_uhexp = (~strategy, ~uhexp, ci) =>
  mk_operand_suggestion(~strategy, ~operand=UHExp.Parenthesized(uhexp), ci);

let mk_lit_suggestion = mk_operand_suggestion(~strategy=InsertLit);

let mk_bool_lit_suggestion = (b: bool, ci: CursorInfo.t): Suggestion.t =>
  mk_lit_suggestion(~operand=UHExp.boollit(b), ci);

let mk_int_lit_suggestion = (s: string, ci: CursorInfo.t): Suggestion.t =>
  mk_lit_suggestion(~operand=UHExp.intlit(s), ci);

let mk_float_lit_suggestion = (s: string, ci: CursorInfo.t): Suggestion.t =>
  mk_lit_suggestion(~operand=UHExp.floatlit(s), ci);

let mk_nil_list_suggestion = (ci: CursorInfo.t): Suggestion.t =>
  mk_lit_suggestion(~operand=UHExp.listnil(), ci);

let mk_var_suggestion = (ci: CursorInfo.t, (s: string, _)): Suggestion.t =>
  mk_operand_suggestion(~strategy=InsertVar, ~operand=UHExp.var(s), ci);

let mk_empty_hole_suggestion = (ci: CursorInfo.t): Suggestion.t =>
  mk_operand_suggestion(~strategy=Delete, ~operand=hole_operand, ci);

let mk_inj_suggestion = (side: InjSide.t, ci: CursorInfo.t): Suggestion.t =>
  mk_operand_suggestion(~strategy=InsertLit, ~operand=mk_inj(side), ci);

let _mk_case_suggestion = (ci: CursorInfo.t): Suggestion.t =>
  mk_operand_suggestion(
    ~strategy=InsertCase,
    ~operand=mk_case(hole_exp),
    ci,
  );

let mk_lambda_suggestion = (ci: CursorInfo.t): Suggestion.t =>
  mk_operand_suggestion(~strategy=InsertLit, ~operand=lambda_operand, ci);

let mk_pair_suggestion = (ci: CursorInfo.t): Suggestion.t => {
  let seq = mk_n_seq(Operators_Exp.Comma, 2);
  mk_operand_suggestion_from_uhexp(
    ~strategy=InsertLit,
    ~uhexp=seq_to_uhexp(seq),
    ci,
  );
};

let mk_list_suggestion = (ci: CursorInfo.t): Suggestion.t => {
  let seq = mk_n_seq(Operators_Exp.Cons, 2);
  mk_operand_suggestion_from_uhexp(
    ~strategy=InsertLit,
    ~uhexp=seq_to_uhexp(seq),
    ci,
  );
};

let mk_app_suggestion =
    ({expected_ty, _} as ci: CursorInfo.t, (name: string, f_ty: HTyp.t))
    : Suggestion.t => {
  let uhexp =
    mk_ap_iter(expected_ty, name, f_ty)
    |> OptUtil.get(_ => failwith("mk_app_suggestion"));
  mk_operand_suggestion_from_uhexp(~strategy=InsertApp, ~uhexp, ci);
};

let is_substring_of_var_name = (str, (name, _)) =>
  switch (StringUtil.search_forward_opt(Str.regexp(str), name)) {
  | None => false
  | Some(_) => true
  };

let vars_satisfying_p = (ctx: Contexts.t, p) => {
  ctx |> Contexts.gamma |> VarMap.filter(p);
};

let get_wrapped_operand =
    (
      {ctx, _}: CursorInfo.t,
      p,
      wrap_name: string,
      cursor_term: CursorInfo.cursor_term,
    ) => {
  switch (cursor_term) {
  | ExpOperand(
      OnText(i),
      (Var(_, InVarHole(_), s) | InvalidText(_, s)) as operand,
    ) =>
    /*
      If we're on an unbound variable or invalidtext, try to interpret
      it as the user attempting to wrap the current operand, splitting
      the cursortext at the cursor and trying to match the wrapper to
      the prefix and find a wrappee matching the suffix
     */
    let (pre, suf) = StringUtil.split_string(i, s);
    switch (StringUtil.search_forward_opt(Str.regexp(pre), wrap_name)) {
    | None => operand
    | Some(_) =>
      let suf_op = UHExp.operand_of_string(suf);
      if (UHExp.is_atomic_operand(suf_op) && UHExp.is_literal_operand(suf_op)) {
        suf_op;
      } else {
        switch (
          vars_satisfying_p(ctx, x =>
            p(x) && is_substring_of_var_name(suf, x)
          )
        ) {
        | [] => operand
        | [(name, _), ..._] =>
          // TODO: return best match rather than first
          UHExp.operand_of_string(name)
        };
      };
    };
  | ExpOperand(
      _,
      (
        Var(NotInHole, NotInVarHole, _) |
        Var(InHole(TypeInconsistent, _), _, _)
      ) as operand,
    ) =>
    // TODO: consider bound variables which are coincidentally wraps
    operand
  | ExpOperand(_, operand) => operand
  | _ => failwith("get_wrap_operand impossible")
  };
};

let mk_wrap_app_suggestion =
    ({cursor_term, _} as ci: CursorInfo.t, (name: string, _)) => {
  // TODO(andrew): get arg type and use for predicate
  let wrapped_operand = get_wrapped_operand(ci, _ => true, name, cursor_term);
  mk_operand_suggestion_from_uhexp(
    ~strategy=WrapApp,
    ~uhexp=mk_bin_ap_uhexp(UHExp.var(name), wrapped_operand),
    ci,
  );
};

let result_type_consistent_with = (expected_ty, s: Suggestion.t) =>
  switch (s.report) {
  | ExpOperand({ty, _}) => HTyp.consistent(ty, expected_ty)
  };

let convert_operand = (operand: UHExp.operand): option(UHExp.operand) =>
  switch (operand) {
  | FloatLit(_, s) when s |> float_of_string |> Float.is_integer =>
    Some(
      s |> float_of_string |> Float.to_int |> string_of_int |> UHExp.intlit,
    )
  | IntLit(_, s) =>
    Some(
      s |> int_of_string |> Float.of_int |> string_of_float |> UHExp.floatlit,
    )
  | _ => None
  };

// GENERATORS --------------------------------------------------------

let rec mk_constructors = (expected_ty: HTyp.t, ci) =>
  // add ability to insert triv
  switch (expected_ty) {
  | Bool => [
      mk_bool_lit_suggestion(true, ci),
      mk_bool_lit_suggestion(false, ci),
    ]
  | Int => [mk_int_lit_suggestion("1", ci)]
  | Float => [mk_float_lit_suggestion("1.", ci)]
  | List(_) => [mk_nil_list_suggestion(ci), mk_list_suggestion(ci)]
  | Sum(_, _) => [mk_inj_suggestion(L, ci), mk_inj_suggestion(R, ci)]
  | Prod(_) => [mk_pair_suggestion(ci)] // TODO: n-tuples
  | Arrow(_, _) => [mk_lambda_suggestion(ci)] // TODO: nested lambdas (both)
  | Hole =>
    // add new types here for synthetic position
    // ordering here is becomes default for UI
    HTyp.[
      Bool,
      Int,
      Float,
      List(Hole),
      Sum(Hole, Hole),
      Prod([]),
      Arrow(Hole, Hole),
    ]
    |> List.map(ty => mk_constructors(ty, ci))
    |> List.concat
  };

let mk_delete_suggestions: generator = ci => [mk_empty_hole_suggestion(ci)];

let mk_insert_lit_suggestions: generator =
  ci => mk_constructors(ci.expected_ty, ci);

let mk_insert_var_suggestions: generator =
  ({ctx, expected_ty, _} as ci) =>
    expected_ty
    |> Assistant_common.extract_vars(ctx)
    |> List.map(mk_var_suggestion(ci));

let mk_insert_app_suggestions: generator =
  ({ctx, expected_ty, _} as ci) => {
    expected_ty
    |> Assistant_common.fun_vars(ctx)
    |> List.map(mk_app_suggestion(ci));
  };

let mk_wrap_app_suggestions: generator =
  ({ctx, expected_ty, actual_ty, cursor_term, _} as ci) =>
    switch (cursor_term) {
    | ExpOperand(_, EmptyHole(_)) =>
      /* Wrapping empty holes is redundant to ap. Revisit when there's
         a mechanism to eliminate duplicate suggestions */
      []
    | _ =>
      let arrow_consistent = ((_, f_ty)) =>
        HTyp.consistent(
          f_ty,
          HTyp.Arrow(HTyp.relax(actual_ty), expected_ty),
        );
      Assistant_common.fun_vars(ctx, expected_ty)
      |> List.filter(arrow_consistent)
      |> List.map(mk_wrap_app_suggestion(ci));
    };

let mk_wrap_case_suggestions: generator =
  ci => {
    let operand =
      ci.cursor_term
      |> get_wrapped_operand(ci, ((_, ty)) => HTyp.is_sumlike(ty), "case")
      |> UHExp.Block.wrap
      |> mk_case;
    [mk_operand_suggestion(~strategy=WrapCase, ~operand, ci)];
  };

let mk_convert_suggestions: generator =
  ci =>
    switch (ci.cursor_term) {
    | ExpOperand(_, operand) =>
      switch (convert_operand(operand)) {
      | None => []
      | Some(operand) =>
        // TODO(andrew): just use expected type in convert_operand?
        [mk_operand_suggestion(~strategy=ConvertLit, ~operand, ci)]
        |> List.filter(result_type_consistent_with(ci.expected_ty))
      }
    | _ => []
    };

/*
 * The order of the below list represents the default or tie-breaker order,
 * in the sense the equally-ranked suggestions will appear in this order.
 */
let exp_operand_generators = [
  mk_convert_suggestions,
  mk_delete_suggestions,
  mk_insert_lit_suggestions,
  mk_insert_var_suggestions,
  mk_wrap_app_suggestions,
  mk_insert_app_suggestions,
  mk_wrap_case_suggestions,
];

let mk: generator = Suggestion.generate(List.rev(exp_operand_generators));
