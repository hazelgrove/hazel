open Assistant_common;

[@deriving sexp]
type generator = Suggestion.generator;

let mk_result =
    (
      ~operand: UHExp.operand,
      ~show_uhexp=UHExp.Block.wrap(operand),
      ~action: Action.t,
      ~ci: CursorInfo.t,
    ) => {
  let ty = HTyp.relax(Statics_Exp.syn_operand(ci.ctx, operand));
  let show_text = UHExp.string_of_operand(operand);
  let score = SuggestionScore.mk(action, ty, show_text, ci);
  Suggestion.ExpOperand({show_uhexp, ty, show_text, score});
};

// move to sugg, make function of operand, make private
let mk_operand_suggestion =
    (
      ~strategy: Suggestion.strategy,
      ~operand: UHExp.operand,
      ~show_uhexp=UHExp.Block.wrap(operand),
      ci: CursorInfo.t,
    )
    : Suggestion.t => {
  let action = Action.ReplaceOperand(operand, None);
  let result = mk_result(~operand, ~show_uhexp, ~ci, ~action);
  {strategy, action, result};
};

let mk_operand_suggestion_from_uhexp = (~strategy, ~uhexp, ci) =>
  mk_operand_suggestion(
    ~strategy,
    ~operand=UHExp.Parenthesized(uhexp),
    ~show_uhexp=uhexp,
    ci,
  );

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

let mk_case_suggestion = (ci: CursorInfo.t): Suggestion.t =>
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

let get_wrapped_operand =
    (
      {ctx, _}: CursorInfo.t,
      arg_ty,
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
        switch (vars_of_type_matching_str(ctx, arg_ty, suf)) {
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

let mk_wrap_suggestion =
    ({cursor_term, _} as ci: CursorInfo.t, (name: string, _)) => {
  // TODO(andrew): get type
  let get_arg_type_somehow_TODO = HTyp.Hole;
  let wrapped_operand =
    get_wrapped_operand(ci, get_arg_type_somehow_TODO, name, cursor_term);
  let show_uhexp = mk_bin_ap_uhexp(UHExp.var(name), wrapped_operand);
  mk_operand_suggestion(
    ~strategy=WrapApp,
    ~operand=UHExp.Parenthesized(show_uhexp),
    ~show_uhexp,
    ci,
  );
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
      |> List.map(mk_wrap_suggestion(ci));
    };

let str_float_to_int = s =>
  s |> float_of_string |> Float.to_int |> string_of_int;
let str_int_to_float = s =>
  s |> int_of_string |> Float.of_int |> string_of_float;

let int_float_conversion_suggestions: generator =
  // just use expected type
  ({cursor_term, _} as ci) =>
    switch (cursor_term) {
    | ExpOperand(_, IntLit(_, s)) => [
        mk_float_lit_suggestion(str_int_to_float(s), ci),
      ]
    | ExpOperand(_, FloatLit(_, s))
        when s |> float_of_string |> Float.is_integer => [
        mk_int_lit_suggestion(str_float_to_int(s), ci),
      ]
    | _ => []
    };

let mk_wrap_case_suggestions: generator =
  ({cursor_term, _} as ci) => {
    let operand =
      cursor_term
      |> get_wrapped_operand(ci, HTyp.Hole, "case")
      |> UHExp.Block.wrap
      |> mk_case;
    [mk_operand_suggestion(~strategy=WrapCase, ~operand, ci)];
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

let result_type_consistent_with = (expected_ty, s: Suggestion.t) =>
  switch (s.result) {
  | ExpOperand({ty, _}) => HTyp.consistent(ty, expected_ty)
  };

let mk_insert_lit_suggestions: generator =
  ({expected_ty, _} as ci) =>
    ci
    |> mk_constructors(ci.expected_ty)
    |> List.filter(result_type_consistent_with(expected_ty));

let mk_convert_suggestions: generator =
  ({expected_ty, _} as ci) =>
    ci
    |> int_float_conversion_suggestions
    |> List.filter(result_type_consistent_with(expected_ty));

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

let exp_operand_generators = [
  mk_insert_app_suggestions,
  mk_wrap_case_suggestions,
  mk_wrap_app_suggestions,
  mk_delete_suggestions,
  mk_convert_suggestions,
  mk_insert_var_suggestions,
  mk_insert_lit_suggestions,
];

let mk: generator = Suggestion.generate(exp_operand_generators);
