open Assistant_common;

[@deriving sexp]
type generator = Suggestion.generator;
[@deriving sexp]
type generator' = Suggestion.generator';

let mk_operand_suggestion =
    (
      ~strategy: Suggestion.operand_strategy,
      ~operand: UHExp.operand,
      ci: CursorInfo.t,
    )
    : Suggestion.t => {
  let action = Action.ReplaceOperand(Exp(operand, None));
  let report = SuggestionReportExp.mk_operand_report(action, operand, ci);
  ReplaceExpOperand({operand, operand_strategy: strategy, report});
};

let mk_operand_suggestion_from_uhexp = (~strategy, ~uhexp, ci) =>
  mk_operand_suggestion(~strategy, ~operand=UHExp.Parenthesized(uhexp), ci);

let mk_lit_suggestion = mk_operand_suggestion(~strategy=InsertLit);

let mk_nil_list_suggestion: generator' =
  mk_lit_suggestion(~operand=UHExp.listnil());

let mk_bool_lit_suggestion: bool => generator' =
  b => mk_lit_suggestion(~operand=UHExp.boollit(b));

let mk_int_lit_suggestion: string => generator' =
  s => mk_lit_suggestion(~operand=UHExp.intlit(s));

let mk_float_lit_suggestion: string => generator' =
  s => mk_lit_suggestion(~operand=UHExp.floatlit(s));

let mk_empty_hole_suggestion: generator' =
  mk_operand_suggestion(~strategy=Delete, ~operand=hole_operand);

let mk_lambda_suggestion: generator' =
  mk_operand_suggestion(~strategy=InsertLit, ~operand=lambda_operand);

let mk_inj_suggestion: (InjSide.t, UHExp.operand) => generator' =
  (side, operand) =>
    mk_operand_suggestion(
      ~strategy=InsertLit,
      ~operand=mk_inj(side, operand),
    );

let mk_var_suggestion = (ci: CursorInfo.t, (s: string, _)): Suggestion.t =>
  mk_operand_suggestion(~strategy=InsertVar, ~operand=UHExp.var(s), ci);

let mk_pair_suggestion: generator' =
  mk_operand_suggestion_from_uhexp(
    ~strategy=InsertLit,
    ~uhexp=seq_to_uhexp(mk_n_seq(Operators_Exp.Comma, 2)),
  );

let mk_list_suggestion: generator' =
  mk_operand_suggestion_from_uhexp(
    ~strategy=InsertLit,
    ~uhexp=seq_to_uhexp(mk_n_seq(Operators_Exp.Cons, 2)),
  );

let mk_app_suggestion =
    ({expected_ty, _} as ci: CursorInfo.t, (name: string, f_ty: HTyp.t))
    : Suggestion.t => {
  let uhexp =
    mk_ap_iter(expected_ty, name, f_ty)
    |> OptUtil.get(_ => failwith("mk_app_suggestion"));
  mk_operand_suggestion_from_uhexp(~strategy=InsertApp, ~uhexp, ci);
};

let vars_satisfying_p = (ctx: Contexts.t, p: ((string, HTyp.t)) => bool) => {
  ctx |> Contexts.gamma |> VarMap.filter(p);
};

let rec get_wrapped_operand' =
        (
          ctx: Contexts.t,
          target_type: HTyp.t,
          prefix: string,
          operand_str: string,
        )
        : list(UHExp.operand) => {
  let prefix_match =
    if (StringUtil.match_prefix(
          String.lowercase_ascii(prefix),
          String.lowercase_ascii(operand_str),
        )) {
      let (_, last) =
        StringUtil.split_string(String.length(prefix), operand_str);
      Some(last);
    } else {
      None;
    };
  let rec_case =
    switch (String.length(prefix)) {
    | 0 => []
    | n =>
      get_wrapped_operand'(
        ctx,
        target_type,
        String.sub(prefix, 0, n - 1),
        operand_str,
      )
    };
  switch (prefix_match) {
  | Some(target_second) =>
    let suf_op = UHExp.operand_of_string(target_second);
    if (UHExp.is_atomic_operand(suf_op) && UHExp.is_literal_operand(suf_op)) {
      [suf_op];
    } else {
      switch (
        vars_satisfying_p(ctx, ((name, ty)) => {
          HTyp.consistent(ty, target_type)
          && String.lowercase_ascii(name)
          == String.lowercase_ascii(target_second)
        })
      ) {
      | [] => rec_case
      | [(name, _), ..._] =>
        // TODO:
        // 1. right now result must be unique bc exact name match. relax?
        // 2. instead of just stopping, could continue to recurse and collect all
        [UHExp.operand_of_string(name)]
      };
    };
  | None => rec_case
  };
};

/*
  * If the cursor is on an operand which is basically text, take that string
  * and try to split it into two strings, operand_first and operand_second,
  * such that operand_first is a MAXIMAL prefix of target_first, and operand_second
  * is EITHER an EXACT string match against a var from the ctx which is also consistent
  * with the provided target_type OR a valid literal.
  *
  * Be aware of the following complicating scenarios:
  *
  * 1. There may be vars in the ctx which begin with (a prefix of) target_first.
  *
  * 2. There may be operand_strings such that the maximal prefix matching target_first
  *    overlaps with a var in the ctx. For example, say numFleens is in the ctx,
  *    target_first is "inj", and operand_str is "inumFleens".
  *
 */
let get_wrapped_operand =
    (
      {ctx, cursor_term, _}: CursorInfo.t,
      target_first: string,
      target_type: HTyp.t,
    )
    : list(UHExp.operand) => {
  switch (CursorInfo_common.cursor_position_of_cursor_term(cursor_term)) {
  | OnText(_) =>
    let operand_text = CursorInfo_common.string_of_cursor_term(cursor_term);
    get_wrapped_operand'(ctx, target_type, target_first, operand_text);
  | _ =>
    // TODO: handle wrapping composite things e.g. parenthesized
    []
  };
};

let convert_operand =
    (operand: UHExp.operand, expected_ty: HTyp.t): option(UHExp.operand) =>
  switch (operand) {
  | FloatLit(_, s)
      when
        Float.is_integer(float_of_string(s))
        && HTyp.consistent(expected_ty, Int) =>
    Some(
      s |> float_of_string |> Float.to_int |> string_of_int |> UHExp.intlit,
    )
  | IntLit(_, s) when HTyp.consistent(expected_ty, Float) =>
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
  | Sum(_, _) => [] // TODO
  // wrap cases handles this
  /*
   [
       mk_inj_suggestion(L, hole_operand, ci),
       mk_inj_suggestion(R, hole_operand, ci),
     ]*/
  | Prod(_) => [mk_pair_suggestion(ci)] // TODO: n-tuples
  | Arrow(_, _) => [mk_lambda_suggestion(ci)] // TODO: nested lambdas
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

let mk_wrap_app_suggestion = (ci: CursorInfo.t, (name: string, f_ty: HTyp.t)) =>
  switch (f_ty) {
  | Arrow(arg_ty, _) =>
    switch (get_wrapped_operand(ci, name, arg_ty)) {
    | [op] => [
        mk_operand_suggestion_from_uhexp(
          ~strategy=WrapApp,
          ~uhexp=mk_bin_ap_uhexp(UHExp.var(name), op),
          ci,
        ),
      ]
    | _ => []
    }
  | _ => []
  };

let mk_wrap_app_suggestions: generator =
  ({ctx, expected_ty, actual_ty, cursor_term, _} as ci) =>
    switch (cursor_term) {
    | ExpOperand(_, EmptyHole(_)) =>
      /* TODO Wrapping empty holes is redundant to ap. Revisit when there's
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
      |> List.map(mk_wrap_app_suggestion(ci))
      |> List.flatten;
    };

let mk_wrap_case_suggestions: generator =
  ci => {
    switch (get_wrapped_operand(ci, "case", HTyp.Hole)) {
    | [op] => [
        mk_operand_suggestion(~strategy=WrapCase, ~operand=mk_case(op), ci),
      ]
    | _ => [
        mk_operand_suggestion(
          ~strategy=InsertCase,
          ~operand=mk_case(hole_operand),
          ci,
        ),
      ]
    };
  };

let mk_wrap_inj_suggestions: generator =
  ({expected_ty, _} as ci) => {
    let mk_wrap_sug = (side, operand) =>
      mk_operand_suggestion(
        ~strategy=WrapInj,
        ~operand=mk_inj(side, operand),
        ci,
      );
    let mk_insert_sug = (side, operand) =>
      mk_operand_suggestion(
        ~strategy=InsertLit,
        ~operand=mk_inj(side, operand),
        ci,
      );
    switch (HTyp.matched_sum(expected_ty)) {
    | None => []
    | Some((l_ty, r_ty)) =>
      let l_sug =
        switch (get_wrapped_operand(ci, "inj", l_ty)) {
        | [guy] => mk_wrap_sug(L, guy)
        | _ => mk_insert_sug(L, hole_operand)
        };
      let r_sug =
        switch (get_wrapped_operand(ci, "inj", r_ty)) {
        | [guy] => mk_wrap_sug(R, guy)
        | _ => mk_insert_sug(R, hole_operand)
        };
      [l_sug, r_sug];
    };
  };

let mk_convert_suggestions: generator =
  ci =>
    switch (ci.cursor_term) {
    | ExpOperand(_, operand) =>
      switch (convert_operand(operand, ci.expected_ty)) {
      | None => []
      | Some(operand) => [
          mk_operand_suggestion(~strategy=ConvertLit, ~operand, ci),
        ]
      }
    | _ => []
    };

/*
 * The order of the below list represents the default or tie-breaker order,
 * in the sense the equally-ranked suggestions will appear in this order.
 */
let exp_operand_generators = [
  mk_convert_suggestions,
  mk_insert_lit_suggestions,
  mk_insert_var_suggestions,
  mk_wrap_app_suggestions,
  mk_insert_app_suggestions,
  mk_delete_suggestions,
  mk_wrap_case_suggestions,
  mk_wrap_inj_suggestions,
  //mk_insert_case_suggestions,
];

let mk: generator = Suggestion.generate(List.rev(exp_operand_generators));
