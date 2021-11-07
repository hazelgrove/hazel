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
      (Var(_, InVarHole(_), s) | InvalidText(_, s)) as _operand,
    ) =>
    /*
      If we're on an unbound variable or invalidtext, try to interpret
      it as the user attempting to wrap the current operand, splitting
      the cursortext at the cursor and trying to match the wrapper to
      the prefix and find a wrappee matching the suffix
     */
    let (pre, suf) = StringUtil.split_string(i, s);
    switch (StringUtil.search_forward_opt(Str.regexp(pre), wrap_name)) {
    | None => None
    //don't bother suggesting unknowns
    //operand
    | Some(_) =>
      let suf_op = UHExp.operand_of_string(suf);
      if (UHExp.is_atomic_operand(suf_op) && UHExp.is_literal_operand(suf_op)) {
        Some(suf_op);
      } else {
        switch (
          vars_satisfying_p(ctx, x =>
            p(x) && is_substring_of_var_name(suf, x)
          )
        ) {
        | [] => None
        //don't bother suggesting unknowns
        //operand
        | [(name, _), ..._] =>
          // TODO: return best match rather than first
          Some(UHExp.operand_of_string(name))
        };
      };
    };
  | ExpOperand(_, operand) =>
    // TODO: consider bound variables which are coincidentally wraps
    Some(UHExp.set_err_status_operand(NotInHole, operand))
  | _ => failwith("get_wrap_operand impossible")
  };
};

let prefix_matcher = (prefix: string, operand_text: string): option(string) =>
  if (StringUtil.match_prefix(prefix, operand_text)) {
    let (_, last) =
      StringUtil.split_string(String.length(prefix), operand_text);
    Some(last);
  } else {
    None;
  };

let rec prefix_matcher' =
        (ctx, target_type, prefix: string, operand_text: string)
        : option(UHExp.operand) => {
  print_endline("TRYING PREFIX");
  print_endline(prefix);
  let rec_case =
    switch (String.length(prefix)) {
    | 0 => None
    | n =>
      prefix_matcher'(
        ctx,
        target_type,
        String.sub(prefix, 0, n - 1),
        operand_text,
      )
    };
  switch (prefix_matcher(prefix, operand_text)) {
  //| _ when prefix == "" => Some(operand_text)
  | Some(target_second) =>
    let suf_op = UHExp.operand_of_string(target_second);
    if (UHExp.is_atomic_operand(suf_op) && UHExp.is_literal_operand(suf_op)) {
      Some(suf_op);
    } else {
      switch (
        vars_satisfying_p(ctx, ((var_name, var_type)) => {
          HTyp.consistent(var_type, target_type) && var_name == target_second
        })
      ) {
      | [] =>
        print_endline("NONE first");
        rec_case;
      | [(name, _), ..._] =>
        // TODO: return best match rather than first?
        // instead of just stropping, could continue to recurse and collect all potentials
        Some(UHExp.operand_of_string(name))
      };
    };
  | None =>
    print_endline("NONE second");
    rec_case;
  };
};

// get_wrapped_operand_2(ci, "case", HTyp.Hole)
let get_wrapped_operand_2 =
    (
      {ctx, cursor_term, _}: CursorInfo.t,
      target_first: string,
      target_type: HTyp.t,
    )
    : option(UHExp.operand) => {
  /*
    new strategy: try:
    whenever on any text operand (as judged by cursorpos = OnText(..))
    (alternatively: only invalidtext or freevar or keywordvar)
    take the operand text string
    and try to split into into a operand_first and operand_second
    such that the operand_first is a MAXIMAL prefix of our target_first
    and operand_second is an exact string match against a var from the context
    (for uniqueness; weaken later) consistent with a provided type_restriction
    OR a literal consistent with the type restriction

    so for example:
    target_first: "case"
    operand_text: "casfoo"
    ctx: [("foo", Int)]
    type_restriction: Hole

    target_first: "inj"
    operand_text: "infoo"
    ctx: [("foo", Int)]
    type_restriction: Int

    concerns:

    1. do we accept the case of an empty prefix match?
    maybe yes; then this just becomes the regular wrap strategy

    NOW A CRITICAL BUG!!!!
    2. what if we have a var which starts with (a prefix of the) target?
    e.g. casefoo, or simple cfoo. do we suggest both wrappings?

    2'. what if we have many vars cfoo, cafoo, casfoo, casefoo....

    2''. what if we create a match eg var in numFleens, want to wrap
    in int2Float. when start typing i "inumFleens", it will now find
    an overlapping prefix "in" and try looking for "umFleens"
   */
  switch (CursorInfo_common.cursor_position_of_cursor_term(cursor_term)) {
  | OnText(_) =>
    let operand_text = CursorInfo_common.string_of_cursor_term(cursor_term);
    prefix_matcher'(ctx, target_type, target_first, operand_text);
  /*
   switch (prefix_matcher'(ctx, target_type,target_first, operand_text)) {
   | None =>
     print_endline("NONE first");
     None;
   | Some(target_second) =>
     let suf_op = UHExp.operand_of_string(target_second);
     if (UHExp.is_atomic_operand(suf_op) && UHExp.is_literal_operand(suf_op)) {
       Some(suf_op);
     } else {
       switch (
         vars_satisfying_p(ctx, ((var_name, var_type)) => {
           HTyp.consistent(var_type, target_type)
           && var_name == target_second
         })
       ) {
       | [] =>
         print_endline("NONE second");
         None;
       | [(name, _), ..._] =>
         // TODO: return best match rather than first?
         Some(UHExp.operand_of_string(name))
       };
     };
   };
   */
  | _ =>
    print_endline("NONE third");
    None;
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
  | Sum(_, _) => []
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

let mk_wrap_app_suggestion = (ci: CursorInfo.t, (name: string, f_ty: HTyp.t)) => {
  // TODO(andrew): get arg type (how?) and use for predicate
  switch (f_ty) {
  | Arrow(arg_ty, _) =>
    switch (get_wrapped_operand_2(ci, name, arg_ty)) {
    | Some(op) => [
        mk_operand_suggestion_from_uhexp(
          ~strategy=WrapApp,
          ~uhexp=mk_bin_ap_uhexp(UHExp.var(name), op),
          ci,
        ),
      ]
    | None => []
    }
  | _ => []
  };
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
      |> List.map(mk_wrap_app_suggestion(ci))
      |> List.flatten;
    };

let mk_wrap_case_suggestions: generator =
  ci => {
    switch (get_wrapped_operand_2(ci, "case", HTyp.Hole)) {
    | None => [
        mk_operand_suggestion(
          ~strategy=InsertCase,
          ~operand=mk_case(hole_operand),
          ci,
        ),
      ]
    | Some(op) => [
        mk_operand_suggestion(~strategy=WrapCase, ~operand=mk_case(op), ci),
      ]
    };
  };

let mk_wrap_inj_suggestions: generator =
  ({expected_ty, _} as ci) => {
    //TODO(andrew): clarify wrap versus insert suggestions
    let mk_wrap_sug = (side, operand) =>
      mk_operand_suggestion(
        ~strategy=WrapCase, // TODO(andrew): new case
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
        switch (get_wrapped_operand_2(ci, "inj", l_ty)) {
        | None => mk_insert_sug(L, hole_operand)
        | Some(guy) => mk_wrap_sug(L, guy)
        };
      let r_sug =
        switch (get_wrapped_operand_2(ci, "inj", r_ty)) {
        | None => mk_insert_sug(R, hole_operand)
        | Some(guy) => mk_wrap_sug(R, guy)
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
