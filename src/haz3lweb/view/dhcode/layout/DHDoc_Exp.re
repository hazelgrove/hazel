open Haz3lcore;
open EvaluatorStep;
open Transition;
open Util;
module Doc = Pretty.Doc;

let precedence_bin_bool_op = (op: TermBase.UExp.op_bin_bool) =>
  switch (op) {
  | And => DHDoc_common.precedence_And
  | Or => DHDoc_common.precedence_Or
  };

let precedence_bin_int_op = (bio: TermBase.UExp.op_bin_int) =>
  switch (bio) {
  | Times => DHDoc_common.precedence_Times
  | Power => DHDoc_common.precedence_Power
  | Divide => DHDoc_common.precedence_Divide
  | Plus => DHDoc_common.precedence_Plus
  | Minus => DHDoc_common.precedence_Minus
  | Equals => DHDoc_common.precedence_Equals
  | NotEquals => DHDoc_common.precedence_Equals
  | LessThan => DHDoc_common.precedence_LessThan
  | LessThanOrEqual => DHDoc_common.precedence_LessThan
  | GreaterThan => DHDoc_common.precedence_GreaterThan
  | GreaterThanOrEqual => DHDoc_common.precedence_GreaterThan
  };
let precedence_bin_float_op = (bfo: TermBase.UExp.op_bin_float) =>
  switch (bfo) {
  | Times => DHDoc_common.precedence_Times
  | Power => DHDoc_common.precedence_Power
  | Divide => DHDoc_common.precedence_Divide
  | Plus => DHDoc_common.precedence_Plus
  | Minus => DHDoc_common.precedence_Minus
  | Equals => DHDoc_common.precedence_Equals
  | NotEquals => DHDoc_common.precedence_Equals
  | LessThan => DHDoc_common.precedence_LessThan
  | LessThanOrEqual => DHDoc_common.precedence_LessThan
  | GreaterThan => DHDoc_common.precedence_GreaterThan
  | GreaterThanOrEqual => DHDoc_common.precedence_GreaterThan
  };
let precedence_bin_string_op = (bso: TermBase.UExp.op_bin_string) =>
  switch (bso) {
  | Concat => DHDoc_common.precedence_Plus
  | Equals => DHDoc_common.precedence_Equals
  };
let rec precedence = (~show_casts: bool, d: DHExp.t) => {
  let precedence' = precedence(~show_casts);
  switch (d) {
  | BoundVar(_)
  | FreeVar(_)
  | InvalidText(_)
  | ExpandingKeyword(_)
  | BoolLit(_)
  | IntLit(_)
  | Sequence(_)
  | Test(_)
  | FloatLit(_)
  | StringLit(_)
  | ListLit(_)
  | Prj(_)
  | EmptyHole(_)
  | Constructor(_)
  | FailedCast(_)
  | InvalidOperation(_)
  | IfThenElse(_)
  | Closure(_)
  | BuiltinFun(_)
  | Filter(_) => DHDoc_common.precedence_const
  | Cast(d1, _, _) =>
    show_casts ? DHDoc_common.precedence_const : precedence'(d1)
  | Ap(_) => DHDoc_common.precedence_Ap
  | ApBuiltin(_) => DHDoc_common.precedence_Ap
  | Cons(_) => DHDoc_common.precedence_Cons
  | ListConcat(_) => DHDoc_common.precedence_Plus
  | Tuple(_) => DHDoc_common.precedence_Comma
  | Fun(_) => DHDoc_common.precedence_max
  | Let(_)
  | FixF(_)
  | ConsistentCase(_)
  | InconsistentBranches(_) => DHDoc_common.precedence_max

  | BinBoolOp(op, _, _) => precedence_bin_bool_op(op)
  | BinIntOp(op, _, _) => precedence_bin_int_op(op)
  | BinFloatOp(op, _, _) => precedence_bin_float_op(op)
  | BinStringOp(op, _, _) => precedence_bin_string_op(op)

  | NonEmptyHole(_, _, _, d) => precedence'(d)
  };
};

let mk_bin_bool_op = (op: TermBase.UExp.op_bin_bool): DHDoc.t =>
  Doc.text(TermBase.UExp.bool_op_to_string(op));

let mk_bin_int_op = (op: TermBase.UExp.op_bin_int): DHDoc.t =>
  Doc.text(TermBase.UExp.int_op_to_string(op));

let mk_bin_float_op = (op: TermBase.UExp.op_bin_float): DHDoc.t =>
  Doc.text(TermBase.UExp.float_op_to_string(op));

let mk_bin_string_op = (op: TermBase.UExp.op_bin_string): DHDoc.t =>
  Doc.text(TermBase.UExp.string_op_to_string(op));

let mk =
    (
      ~settings: CoreSettings.Evaluation.t,
      ~enforce_inline: bool,
      ~selected_hole_instance: option(HoleInstance.t),
      // The next four are used when drawing the stepper to track where we can annotate changes
      ~previous_step: option(step), // The step that will be displayed above this one
      ~hidden_steps: list(step), // The hidden steps between the above and the current one
      ~chosen_step: option(step), // The step that will be taken next
      ~next_steps: list(EvalObj.t), // The options for the next step, if it hasn't been chosen yet
      ~show_steppable: bool=false, // Whether to show the steppable annotation
      ~env: ClosureEnvironment.t,
      d: DHExp.t,
    )
    : DHDoc.t => {
  let precedence = precedence(~show_casts=settings.show_casts);
  let rec go =
          (
            d: DHExp.t,
            env: ClosureEnvironment.t,
            full_ctx: EvalCtx.t,
            enforce_inline: bool,
            previous_step: option(step),
            hidden_steps: list(step),
            chosen_step: option(step),
            next_steps: list((EvalCtx.t, EvalObj.t)),
            recent_subst: list(Var.t),
            recursive_calls: list(Var.t),
          )
          : DHDoc.t => {
    open Doc;
    let recent_subst =
      switch (previous_step) {
      | Some(ps) when ps.ctx == Mark =>
        switch (ps.knd, ps.d_loc) {
        | (FunAp, Ap(Fun(p, _, _, _), _)) => DHPat.bound_vars(p)
        | (FunAp, _) => []
        | (LetBind, Let(p, _, _)) => DHPat.bound_vars(p)
        | (LetBind, _) => []
        | (FixUnwrap, _) // TODO[Matt]: Could do something here?
        | (InvalidStep, _)
        | (VarLookup, _)
        | (Sequence, _)
        | (FunClosure, _)
        | (UpdateTest, _)
        | (CastAp, _)
        | (BuiltinWrap, _)
        | (BuiltinAp(_), _)
        | (BinBoolOp(_), _)
        | (BinIntOp(_), _)
        | (BinFloatOp(_), _)
        | (BinStringOp(_), _)
        | (Projection, _)
        | (ListCons, _)
        | (ListConcat, _)
        | (CaseApply, _)
        | (CaseNext, _)
        | (CompleteClosure, _)
        | (CompleteFilter, _)
        | (Cast, _)
        | (Conditional(_), _)
        | (Rewrite(_), _)
        | (Skip, _) => []
        }
      | _ => recent_subst
      };
    let go' =
        (
          ~env=env,
          ~enforce_inline=enforce_inline,
          ~recent_subst=recent_subst,
          ~recursive_calls=recursive_calls,
          d,
          ctx,
          full_ctx,
        ) => {
      go(
        d,
        env,
        full_ctx,
        enforce_inline,
        Option.join(
          Option.map(EvaluatorStep.unwrap(_, ctx), previous_step),
        ),
        hidden_steps
        |> List.filter(s => !EvalCtx.fuzzy_mark(s.ctx))
        |> List.filter_map(EvaluatorStep.unwrap(_, ctx)),
        Option.join(Option.map(EvaluatorStep.unwrap(_, ctx), chosen_step)),
        List.filter_map(
          ((x, y)) =>
            switch (EvalCtx.unwrap(x, ctx)) {
            | None => None
            | Some(x') => Some((x', y))
            },
          next_steps,
        ),
        recent_subst,
        recursive_calls,
      );
    };
    let parenthesize = (b, doc) =>
      if (b) {
        hcats([
          DHDoc_common.Delim.open_Parenthesized,
          doc |> DHDoc_common.pad_child(~enforce_inline),
          DHDoc_common.Delim.close_Parenthesized,
        ]);
      } else {
        doc(~enforce_inline);
      };
    let go_case_rule =
        (
          dscrut,
          all_rules,
          consistent: bool,
          rule_idx: int,
          Rule(dp, dclause): DHExp.rule,
        )
        : DHDoc.t => {
      let kind: EvalCtx.cls =
        if (consistent) {
          ConsistentCaseRule(rule_idx);
        } else {
          InconsistentBranchesRule(rule_idx);
        };
      let fctx: EvalCtx.t = {
        let (ll, lr) = ListUtil.split_n(rule_idx, all_rules);
        let lr = List.tl(lr);
        if (consistent) {
          ConsistentCaseRule(dscrut, dp, full_ctx, (ll, lr), rule_idx);
        } else {
          // TODO[Matt]: put correct id in here
          InconsistentBranchesRule(
            dscrut,
            Id.invalid,
            0,
            dp,
            full_ctx,
            (ll, lr),
            rule_idx,
          );
        };
      };
      let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
      let clause_doc =
        settings.show_case_clauses
          ? choices([
              hcats([
                space(),
                go'(~enforce_inline=true, dclause, kind, fctx),
              ]),
              hcats([
                linebreak(),
                indent_and_align(
                  go'(~enforce_inline=false, dclause, kind, fctx),
                ),
              ]),
            ])
          : hcat(space(), hidden_clause);
      hcats([
        DHDoc_common.Delim.bar_Rule,
        DHDoc_Pat.mk(dp)
        |> DHDoc_common.pad_child(
             ~inline_padding=(space(), space()),
             ~enforce_inline=false,
           ),
        DHDoc_common.Delim.arrow_Rule,
        clause_doc,
      ]);
    };
    let go_case = (dscrut: DHExp.t, drs, rule_id, consistent, ctx) =>
      if (enforce_inline) {
        fail();
      } else {
        let kind: EvalCtx.cls =
          if (consistent) {ConsistentCase} else {InconsistentBranches};
        let case: EvalCtx.case = Case(ctx, drs, rule_id);
        let ctx: EvalCtx.t =
          if (consistent) {
            ConsistentCase(case);
          } else {
            // TODO: Put the correct Id in here!!
            InconsistentBranches(
              Id.invalid,
              0,
              case,
            );
          };
        let scrut_doc =
          choices([
            hcats([space(), go'(~enforce_inline=true, dscrut, kind, ctx)]),
            hcats([
              linebreak(),
              indent_and_align(
                go'(~enforce_inline=false, dscrut, kind, ctx),
              ),
            ]),
          ]);
        vseps(
          List.concat([
            [hcat(DHDoc_common.Delim.open_Case, scrut_doc)],
            drs |> List.mapi(go_case_rule(dscrut, drs, consistent)),
            [DHDoc_common.Delim.close_Case],
          ]),
        );
      };
    let go_formattable = (~enforce_inline) => go'(~enforce_inline);
    let mk_left_associative_operands =
        (precedence_op, (d1, l, ctxl), (d2, r, ctxr)) => (
      go_formattable(d1, l, ctxl)
      |> parenthesize(precedence(d1) > precedence_op),
      go_formattable(d2, r, ctxr)
      |> parenthesize(precedence(d2) >= precedence_op),
    );
    let mk_right_associative_operands =
        (precedence_op, (d1, l, ctxl), (d2, r, ctxr)) => (
      go_formattable(d1, l, ctxl)
      |> parenthesize(precedence(d1) >= precedence_op),
      go_formattable(d2, r, ctxr)
      |> parenthesize(precedence(d2) > precedence_op),
    );
    let doc = {
      switch (d) {
      | Closure(env', d') =>
        go'(d', Closure, Closure(env', full_ctx), ~env=env')
      | Filter(flt, d') =>
        if (settings.show_stepper_filters) {
          switch (flt) {
          | Filter({pat, act}) =>
            let keyword = FilterAction.string_of_t(act);
            // HACK[Matt]: Replace full_ctx
            let flt_doc = go_formattable(pat, FilterPattern, full_ctx);
            vseps([
              hcats([
                DHDoc_common.Delim.mk(keyword),
                flt_doc
                |> DHDoc_common.pad_child(
                     ~inline_padding=(space(), space()),
                     ~enforce_inline=false,
                   ),
                DHDoc_common.Delim.mk("in"),
              ]),
              go'(d', Filter, Filter(flt, full_ctx)),
            ]);
          | Residue(_, act) =>
            let keyword = FilterAction.string_of_t(act);
            // HACK[Matt]: Replace full_ctx
            vseps([
              DHDoc_common.Delim.mk(keyword),
              go'(d', Filter, full_ctx),
            ]);
          };
        } else {
          switch (flt) {
          // HACK[Matt]: Replace full_ctx
          | Residue(_) => go'(d', Filter, full_ctx)
          | Filter(_) => go'(d', Filter, full_ctx)
          };
        }

      /* Hole expressions must appear within a closure in
         the postprocessed result */
      | EmptyHole(u, i) =>
        let selected =
          switch (selected_hole_instance) {
          | None => false
          | Some((u', i')) => u == u' && i == i'
          };
        DHDoc_common.mk_EmptyHole(~selected, (u, i));
      //HACK(andrew): suppress err holes for free vars for demo
      | NonEmptyHole(reason, u, i, FreeVar(_) as d') =>
        go'(d', NonEmptyHole, NonEmptyHole(reason, u, i, full_ctx))
      | NonEmptyHole(reason, u, i, d') =>
        go'(d', NonEmptyHole, NonEmptyHole(reason, u, i, full_ctx))
        |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | ExpandingKeyword(u, i, k) =>
        DHDoc_common.mk_ExpandingKeyword((u, i), k)
      | FreeVar(u, i, x) =>
        text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
      | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
      | InconsistentBranches(u, i, Case(dscrut, drs, _)) =>
        go_case(dscrut, drs, i, false, full_ctx)
        |> annot(DHAnnot.InconsistentBranches((u, i)))
      | BoundVar(x) when List.mem(x, recursive_calls) => text(x)
      | BoundVar(x) when settings.show_lookup_steps => text(x)
      | BoundVar(x) =>
        switch (ClosureEnvironment.lookup(env, x)) {
        | None => text(x)
        | Some(d') =>
          if (List.mem(x, recent_subst)) {
            hcats([
              go'(
                ~env=ClosureEnvironment.empty,
                BoundVar(x),
                BoundVar,
                full_ctx,
              )
              |> annot(DHAnnot.Substituted),
              go'(~env=ClosureEnvironment.empty, d', BoundVar, full_ctx),
            ]);
          } else {
            go'(~env=ClosureEnvironment.empty, d', BoundVar, full_ctx);
          }
        }
      | BuiltinFun(f) => text(f)
      | Constructor(name) => DHDoc_common.mk_ConstructorLit(name)
      | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
      | IntLit(n) => DHDoc_common.mk_IntLit(n)
      | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
      | StringLit(s) => DHDoc_common.mk_StringLit(s)
      | Test(id, d) =>
        DHDoc_common.mk_Test(go'(d, Test, Test(id, full_ctx)))
      | Sequence(d1, d2) =>
        let (doc1, doc2) = (
          go'(d1, Sequence1, Sequence1(full_ctx, d2)),
          go'(d2, Sequence2, Sequence2(d1, full_ctx)),
        );
        DHDoc_common.mk_Sequence(doc1, doc2);
      | ListLit(mv, mvi, t, d_list) =>
        let ol =
          d_list
          |> List.mapi((i, d) => {
               let (ll, lr) = ListUtil.split_n(i, d_list);
               let lr = List.tl(lr);
               go'(d, ListLit(i), ListLit(mv, mvi, t, full_ctx, (ll, lr)));
             });
        DHDoc_common.mk_ListLit(ol);
      | Ap(d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1, Ap1, Ap1(full_ctx, d2))
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2, Ap2, Ap2(d1, full_ctx)),
        );
        DHDoc_common.mk_Ap(doc1, doc2);
      | ApBuiltin(ident, d) =>
        DHDoc_common.mk_Ap(
          text(ident),
          go_formattable(d, ApBuiltin, ApBuiltin(ident, full_ctx))
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Ap),
        )
      | BinIntOp(op, d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_int_op(op),
            (d1, BinIntOp1, BinIntOp1(op, full_ctx, d2)),
            (d2, BinIntOp2, BinIntOp2(op, d1, full_ctx)),
          );
        hseps([doc1, mk_bin_int_op(op), doc2]);
      | BinFloatOp(op, d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_float_op(op),
            (d1, BinFloatOp1, BinFloatOp1(op, full_ctx, d2)),
            (d2, BinFloatOp2, BinFloatOp2(op, d1, full_ctx)),
          );
        hseps([doc1, mk_bin_float_op(op), doc2]);
      | BinStringOp(op, d1, d2) =>
        // TODO assumes all bin string ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_string_op(op),
            (d1, BinStringOp1, BinStringOp1(op, full_ctx, d2)),
            (d2, BinStringOp2, BinStringOp2(op, d1, full_ctx)),
          );
        hseps([doc1, mk_bin_string_op(op), doc2]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Cons,
            (d1, Cons1, Cons1(full_ctx, d2)),
            (d2, Cons2, Cons2(d1, full_ctx)),
          );
        DHDoc_common.mk_Cons(doc1, doc2);
      | ListConcat(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Plus,
            (d1, ListConcat1, ListConcat1(full_ctx, d2)),
            (d2, ListConcat2, ListConcat2(d1, full_ctx)),
          );
        DHDoc_common.mk_ListConcat(doc1, doc2);
      | BinBoolOp(op, d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            precedence_bin_bool_op(op),
            (d1, BinBoolOp1, BinBoolOp1(op, full_ctx, d2)),
            (d2, BinBoolOp2, BinBoolOp2(op, d1, full_ctx)),
          );
        hseps([doc1, mk_bin_bool_op(op), doc2]);
      | Tuple([]) => DHDoc_common.Delim.triv
      | Tuple(ds) =>
        DHDoc_common.mk_Tuple(
          ds
          |> List.mapi((i, d) => {
               let (ll, lr) = ListUtil.split_n(i, ds);
               let lr = List.tl(lr);
               go'(d, Tuple(i), Tuple(full_ctx, (List.rev(ll), lr)));
             }),
        )
      | Prj(d, n) => DHDoc_common.mk_Prj(go'(d, Prj, Prj(full_ctx, n)), n)
      | ConsistentCase(Case(dscrut, drs, i)) =>
        go_case(dscrut, drs, i, true, full_ctx)
      | Cast(d, t1, ty) when settings.show_casts =>
        // TODO[Matt]: Roll multiple casts into one cast
        let doc = go'(d, Cast, Cast(full_ctx, t1, ty));
        Doc.(
          hcat(
            doc,
            annot(
              DHAnnot.CastDecoration,
              DHDoc_Typ.mk(~enforce_inline=true, ty),
            ),
          )
        );
      | Cast(d, t1, t2) =>
        let doc = go'(d, Cast, Cast(full_ctx, t1, t2));
        doc;
      | Let(dp, ddef, dbody) =>
        if (enforce_inline) {
          fail();
        } else {
          let bindings = DHPat.bound_vars(dp);
          print_endline("===");
          print_endline(ClosureEnvironment.show(env));
          print_endline(
            ClosureEnvironment.show(
              ClosureEnvironment.without_keys(bindings, env),
            ),
          );
          let def_doc =
            go_formattable(ddef, Let1, Let1(dp, full_ctx, dbody));
          vseps([
            hcats([
              DHDoc_common.Delim.mk("let"),
              DHDoc_Pat.mk(dp)
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
              DHDoc_common.Delim.mk("="),
              def_doc
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline=false,
                 ),
              DHDoc_common.Delim.mk("in"),
            ]),
            go'(
              ~enforce_inline=false,
              ~env=ClosureEnvironment.without_keys(bindings, env),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), recent_subst),
              dbody,
              Let2,
              Let2(dp, ddef, full_ctx),
            ),
          ]);
        }
      | FailedCast(Cast(d, ty1, ty2), ty2', ty3) when Typ.eq(ty2, ty2') =>
        let d_doc = go'(d, FailedCastCast, FailedCast(full_ctx, ty1, ty2));
        let cast_decoration =
          hcats([
            DHDoc_common.Delim.open_FailedCast,
            hseps([
              DHDoc_Typ.mk(~enforce_inline=true, ty1),
              DHDoc_common.Delim.arrow_FailedCast,
              DHDoc_Typ.mk(~enforce_inline=true, ty3),
            ]),
            DHDoc_common.Delim.close_FailedCast,
          ])
          |> annot(DHAnnot.FailedCastDecoration);
        hcats([d_doc, cast_decoration]);
      | FailedCast(_d, _ty1, _ty2) =>
        failwith("unexpected FailedCast without inner cast")
      | InvalidOperation(d, err) =>
        let d_doc =
          go'(d, InvalidOperation, InvalidOperation(full_ctx, err));
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);

      | IfThenElse(t, c, d1, d2) =>
        let c_doc =
          go_formattable(c, IfThenElse1, IfThenElse1(t, full_ctx, d1, d2));
        let d1_doc =
          go_formattable(d1, IfThenElse2, IfThenElse2(t, c, full_ctx, d2));
        let d2_doc =
          go_formattable(d2, IfThenElse3, IfThenElse3(t, c, d1, full_ctx));
        hcats([
          DHDoc_common.Delim.mk("("),
          DHDoc_common.Delim.mk("if"),
          c_doc
          |> DHDoc_common.pad_child(
               ~inline_padding=(space(), space()),
               ~enforce_inline=false,
             ),
          DHDoc_common.Delim.mk("then"),
          d1_doc
          |> DHDoc_common.pad_child(
               ~inline_padding=(space(), space()),
               ~enforce_inline=false,
             ),
          DHDoc_common.Delim.mk("else"),
          d2_doc
          |> DHDoc_common.pad_child(
               ~inline_padding=(space(), empty()),
               ~enforce_inline=false,
             ),
          DHDoc_common.Delim.mk(")"),
        ]);
      | Fun(dp, ty, Closure(env', d), s) =>
        print_endline(DHExp.show(d));
        if (settings.show_fn_bodies) {
          let bindings = DHPat.bound_vars(dp);
          let new_env =
            ClosureEnvironment.without_keys(Option.to_list(s), env');
          let body_doc =
            go_formattable(
              Closure(new_env, d),
              ~env=
                ClosureEnvironment.without_keys(
                  DHPat.bound_vars(dp) @ Option.to_list(s),
                  env,
                ),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), recent_subst),
              Fun,
              Fun(dp, ty, full_ctx, s),
            );
          hcats(
            [
              DHDoc_common.Delim.sym_Fun,
              DHDoc_Pat.mk(dp)
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
            ]
            @ (
              settings.show_casts
                ? [
                  DHDoc_common.Delim.colon_Fun,
                  space(),
                  DHDoc_Typ.mk(~enforce_inline=true, ty),
                  space(),
                ]
                : []
            )
            @ [
              DHDoc_common.Delim.arrow_Fun,
              space(),
              body_doc |> DHDoc_common.pad_child(~enforce_inline),
            ],
          );
        } else {
          switch (s) {
          | None => annot(DHAnnot.Collapsed, text("<anon fn>"))
          | Some(name) => annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"))
          };
        };
      | Fun(dp, ty, dbody, s) =>
        if (settings.show_fn_bodies) {
          let bindings = DHPat.bound_vars(dp);
          let body_doc =
            go_formattable(
              dbody,
              ~env=ClosureEnvironment.without_keys(bindings, env),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), recent_subst),
              ~recursive_calls=Option.to_list(s) @ recursive_calls,
              Fun,
              Fun(dp, ty, full_ctx, s),
            );
          hcats(
            [
              DHDoc_common.Delim.sym_Fun,
              DHDoc_Pat.mk(dp)
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
            ]
            @ (
              settings.show_casts
                ? [
                  DHDoc_common.Delim.colon_Fun,
                  space(),
                  DHDoc_Typ.mk(~enforce_inline=true, ty),
                  space(),
                ]
                : []
            )
            @ [
              DHDoc_common.Delim.arrow_Fun,
              space(),
              body_doc |> DHDoc_common.pad_child(~enforce_inline),
            ],
          );
        } else {
          switch (s) {
          | None => annot(DHAnnot.Collapsed, text("<anon fn>"))
          | Some(name) => annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"))
          };
        }
      | FixF(x, ty, dbody) when settings.show_fixpoints =>
        let doc_body =
          go_formattable(
            dbody,
            ~env=ClosureEnvironment.without_keys([x], env),
            FixF,
            FixF(x, ty, full_ctx),
          );
        hcats(
          [DHDoc_common.Delim.fix_FixF, space(), text(x)]
          @ (
            settings.show_casts
              ? [
                DHDoc_common.Delim.colon_Fun,
                space(),
                DHDoc_Typ.mk(~enforce_inline=true, ty),
                space(),
              ]
              : []
          )
          @ [
            DHDoc_common.Delim.arrow_FixF,
            space(),
            doc_body |> DHDoc_common.pad_child(~enforce_inline),
          ],
        );
      | FixF(x, ty, d) =>
        go'(
          ~env=ClosureEnvironment.without_keys([x], env),
          d,
          FixF,
          FixF(x, ty, full_ctx),
        )
      };
    };
    let steppable =
      next_steps |> List.find_opt(((ctx, _)) => ctx == EvalCtx.Mark);
    let stepped =
      chosen_step
      |> Option.map(x => x.ctx == Mark)
      |> Option.value(~default=false);
    let substitution =
      hidden_steps
      |> List.find_opt(step =>
           step.knd == VarLookup
           // HACK[Matt]: to prevent substitutions hiding inside casts
           && EvalCtx.fuzzy_mark(step.ctx)
         );
    let doc =
      switch (substitution) {
      | Some({d_loc: BoundVar(v), _}) when List.mem(v, recent_subst) =>
        hcats([text(v) |> annot(DHAnnot.Substituted), doc])
      | Some(_)
      | None => doc
      };
    let rewrite_action_of = (r): UpdateAction.stepper_action =>
      Rewrite({focus: d, ctx: full_ctx, rule: r});
    let rewrite_actions: list(UpdateAction.stepper_action) =
      List.map(rewrite_action_of, RewriteStep.matching_rewrites(d));
    switch (steppable) {
    | _ when stepped => annot(DHAnnot.Stepped, doc)
    | Some((_, full)) when show_steppable =>
      let step_action = UpdateAction.StepForward(full);
      annot(DHAnnot.Steppable([step_action] @ rewrite_actions), doc);
    | _ when show_steppable && RewriteStep.matching_rewrites(d) != [] =>
      annot(DHAnnot.Steppable(rewrite_actions), doc)
    | _ => doc
    };
  };
  go(
    d,
    env,
    Mark,
    enforce_inline,
    previous_step,
    hidden_steps,
    chosen_step,
    List.map((x: EvalObj.t) => (x.ctx, x), next_steps),
    [],
    [],
  );
};
