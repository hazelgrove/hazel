open Haz3lcore;
open EvaluatorStep;
open Transition;
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
  | BuiltinFun(_)
  | Filter(_)
  | Closure(_) => DHDoc_common.precedence_const
  | Cast(d1, _, _) =>
    show_casts ? DHDoc_common.precedence_const : precedence'(d1)
  | Ap(_)
  | TypAp(_) => DHDoc_common.precedence_Ap
  | ApBuiltin(_) => DHDoc_common.precedence_Ap
  | Cons(_) => DHDoc_common.precedence_Cons
  | ListConcat(_) => DHDoc_common.precedence_Plus
  | Tuple(_) => DHDoc_common.precedence_Comma
  | TypFun(_)
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
      ~env: ClosureEnvironment.t,
      d: DHExp.t,
    )
    : DHDoc.t => {
  let precedence = precedence(~show_casts=settings.show_casts);
  let rec go =
          (
            d: DHExp.t,
            env: ClosureEnvironment.t,
            enforce_inline: bool,
            previous_step: option(step),
            hidden_steps: list(step),
            chosen_step: option(step),
            next_steps: list((EvalCtx.t, int)),
            recent_subst: list(Var.t),
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
        | (FixUnwrap, FixF(f, _, _)) => [f]
        | (FixUnwrap, _) => []
        | (TypFunAp, _) // TODO: Could also do something here for type variable substitution like in FunAp?
        | (InvalidStep, _)
        | (VarLookup, _)
        | (Sequence, _)
        | (FunClosure, _)
        | (FixClosure, _)
        | (UpdateTest, _)
        | (CastTypAp, _)
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
        | (Skip, _) => []
        }
      | _ => recent_subst
      };
    let substitution =
      hidden_steps
      |> List.find_opt(step =>
           step.knd == VarLookup
           // HACK[Matt]: to prevent substitutions hiding inside casts
           && EvalCtx.fuzzy_mark(step.ctx)
         );
    let next_recent_subst =
      switch (substitution) {
      | Some({d_loc: BoundVar(v), _}) =>
        List.filter(u => u != v, recent_subst)
      | _ => recent_subst
      };
    let go' =
        (
          ~env=env,
          ~enforce_inline=enforce_inline,
          ~recent_subst=next_recent_subst,
          d,
          ctx,
        ) => {
      go(
        d,
        env,
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
        (consistent: bool, rule_idx: int, Rule(dp, dclause): DHExp.rule)
        : DHDoc.t => {
      let kind: EvalCtx.cls =
        if (consistent) {
          ConsistentCaseRule(rule_idx);
        } else {
          InconsistentBranchesRule(rule_idx);
        };
      let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
      let clause_doc =
        settings.show_case_clauses
          ? choices([
              hcats([space(), go'(~enforce_inline=true, dclause, kind)]),
              hcats([
                linebreak(),
                indent_and_align(go'(~enforce_inline=false, dclause, kind)),
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
    let go_case = (dscrut, drs, consistent) =>
      if (enforce_inline) {
        fail();
      } else {
        let kind: EvalCtx.cls =
          if (consistent) {ConsistentCase} else {InconsistentBranches};
        let scrut_doc =
          choices([
            hcats([space(), go'(~enforce_inline=true, dscrut, kind)]),
            hcats([
              linebreak(),
              indent_and_align(go'(~enforce_inline=false, dscrut, kind)),
            ]),
          ]);
        vseps(
          List.concat([
            [hcat(DHDoc_common.Delim.open_Case, scrut_doc)],
            drs |> List.mapi(go_case_rule(consistent)),
            [DHDoc_common.Delim.close_Case],
          ]),
        );
      };
    let go_formattable = (~enforce_inline) => go'(~enforce_inline);
    let mk_left_associative_operands = (precedence_op, (d1, l), (d2, r)) => (
      go_formattable(d1, l) |> parenthesize(precedence(d1) > precedence_op),
      go_formattable(d2, r) |> parenthesize(precedence(d2) >= precedence_op),
    );
    let mk_right_associative_operands = (precedence_op, (d1, l), (d2, r)) => (
      go_formattable(d1, l) |> parenthesize(precedence(d1) >= precedence_op),
      go_formattable(d2, r) |> parenthesize(precedence(d2) > precedence_op),
    );
    let doc = {
      switch (d) {
      | Closure(env', d') => go'(d', Closure, ~env=env')
      | Filter(flt, d') =>
        if (settings.show_stepper_filters) {
          switch (flt) {
          | Filter({pat, act}) =>
            let keyword = FilterAction.string_of_t(act);
            let flt_doc = go_formattable(pat, FilterPattern);
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
              go'(d', Filter),
            ]);
          | Residue(_, act) =>
            let keyword = FilterAction.string_of_t(act);
            vseps([DHDoc_common.Delim.mk(keyword), go'(d', Filter)]);
          };
        } else {
          switch (flt) {
          | Residue(_) => go'(d', Filter)
          | Filter(_) => go'(d', Filter)
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
      | NonEmptyHole(reason, u, i, d') =>
        go'(d', NonEmptyHole)
        |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | FreeVar(u, i, x) =>
        text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
      | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
      | InconsistentBranches(u, i, Case(dscrut, drs, _)) =>
        go_case(dscrut, drs, false)
        |> annot(DHAnnot.InconsistentBranches((u, i)))
      | BoundVar(x) when settings.show_lookup_steps => text(x)
      | BoundVar(x) =>
        switch (ClosureEnvironment.lookup(env, x)) {
        | None => text(x)
        | Some(d') =>
          if (List.mem(x, recent_subst)) {
            hcats([
              go'(~env=ClosureEnvironment.empty, BoundVar(x), BoundVar)
              |> annot(DHAnnot.Substituted),
              go'(
                ~env=ClosureEnvironment.empty,
                ~recent_subst=List.filter(u => u != x, next_recent_subst),
                d',
                BoundVar,
              ),
            ]);
          } else {
            go'(~env=ClosureEnvironment.empty, d', BoundVar);
          }
        }
      | BuiltinFun(f) => text(f)
      | Constructor(name) => DHDoc_common.mk_ConstructorLit(name)
      | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
      | IntLit(n) => DHDoc_common.mk_IntLit(n)
      | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
      | StringLit(s) => DHDoc_common.mk_StringLit(s)
      | Test(_, d) => DHDoc_common.mk_Test(go'(d, Test))
      | Sequence(d1, d2) =>
        let (doc1, doc2) = (go'(d1, Sequence1), go'(d2, Sequence2));
        DHDoc_common.mk_Sequence(doc1, doc2);
      | ListLit(_, _, _, d_list) =>
        let ol = d_list |> List.mapi((i, d) => go'(d, ListLit(i)));
        DHDoc_common.mk_ListLit(ol);

      | Ap(d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1, Ap1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2, Ap2),
        );
        DHDoc_common.mk_Ap(doc1, doc2);
      | TypAp(d1, ty) =>
        let doc1 = go'(d1, TypAp);
        let doc2 = DHDoc_Typ.mk(~enforce_inline=true, ty);
        DHDoc_common.mk_TypAp(doc1, doc2);
      | ApBuiltin(ident, d) =>
        DHDoc_common.mk_Ap(
          text(ident),
          go_formattable(d, ApBuiltin)
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Ap),
        )
      | BinIntOp(op, d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_int_op(op),
            (d1, BinIntOp1),
            (d2, BinIntOp2),
          );
        hseps([doc1, mk_bin_int_op(op), doc2]);
      | BinFloatOp(op, d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_float_op(op),
            (d1, BinFloatOp1),
            (d2, BinFloatOp2),
          );
        hseps([doc1, mk_bin_float_op(op), doc2]);
      | BinStringOp(op, d1, d2) =>
        // TODO assumes all bin string ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_string_op(op),
            (d1, BinStringOp1),
            (d2, BinStringOp2),
          );
        hseps([doc1, mk_bin_string_op(op), doc2]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Cons,
            (d1, Cons1),
            (d2, Cons2),
          );
        DHDoc_common.mk_Cons(doc1, doc2);
      | ListConcat(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Plus,
            (d1, ListConcat1),
            (d2, ListConcat2),
          );
        DHDoc_common.mk_ListConcat(doc1, doc2);
      | BinBoolOp(op, d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            precedence_bin_bool_op(op),
            (d1, BinBoolOp1),
            (d2, BinBoolOp2),
          );
        hseps([doc1, mk_bin_bool_op(op), doc2]);
      | Tuple([]) => DHDoc_common.Delim.triv
      | Tuple(ds) =>
        DHDoc_common.mk_Tuple(ds |> List.mapi((i, d) => go'(d, Tuple(i))))
      | Prj(d, n) => DHDoc_common.mk_Prj(go'(d, Prj), n)
      | ConsistentCase(Case(dscrut, drs, _)) => go_case(dscrut, drs, true)
      | Cast(d, _, ty) when settings.show_casts =>
        // TODO[Matt]: Roll multiple casts into one cast
        let doc = go'(d, Cast);
        Doc.(
          hcat(
            doc,
            annot(
              DHAnnot.CastDecoration,
              DHDoc_Typ.mk(~enforce_inline=true, ty),
            ),
          )
        );
      | Cast(d, _, _) =>
        let doc = go'(d, Cast);
        doc;
      | Let(dp, ddef, dbody) =>
        if (enforce_inline) {
          fail();
        } else {
          let bindings = DHPat.bound_vars(dp);
          let def_doc = go_formattable(ddef, Let1);
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
                List.filter(x => !List.mem(x, bindings), next_recent_subst),
              dbody,
              Let2,
            ),
          ]);
        }
      | FailedCast(Cast(d, ty1, ty2), ty2', ty3) when Typ.eq(ty2, ty2') =>
        let d_doc = go'(d, FailedCastCast);
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
        let d_doc = go'(d, InvalidOperation);
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);

      | IfThenElse(_, c, d1, d2) =>
        let c_doc = go_formattable(c, IfThenElse1);
        let d1_doc = go_formattable(d1, IfThenElse2);
        let d2_doc = go_formattable(d2, IfThenElse3);
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
      | Fun(dp, ty, dbody, s) when settings.show_fn_bodies =>
        let bindings = DHPat.bound_vars(dp);
        let body_doc =
          switch (dbody) {
          | Closure(env', dbody) =>
            go_formattable(
              Closure(env', dbody),
              ~env=
                ClosureEnvironment.without_keys(
                  DHPat.bound_vars(dp) @ Option.to_list(s),
                  env,
                ),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), next_recent_subst),
              Fun,
            )
          | _ =>
            go_formattable(
              dbody,
              ~env=ClosureEnvironment.without_keys(bindings, env),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), next_recent_subst),
              Fun,
            )
          };
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
            body_doc |> DHDoc_common.pad_child(~enforce_inline=false),
          ],
        );
      | Fun(_, _, _, s) =>
        let name =
          switch (s) {
          | None => "anon fn"
          | Some(name)
              when
                !settings.show_fixpoints
                && String.ends_with(~suffix="+", name) =>
            String.sub(name, 0, String.length(name) - 1)
          | Some(name) => name
          };
        annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"));
      | TypFun(_tpat, _dbody, s) =>
        /* same display as with Fun but with anon typfn in the nameless case. */
        let name =
          switch (s) {
          | None => "anon typfn"
          | Some(name)
              when
                !settings.show_fixpoints
                && String.ends_with(~suffix="+", name) =>
            String.sub(name, 0, String.length(name) - 1)
          | Some(name) => name
          };
        annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"));
      | FixF(x, ty, dbody)
          when settings.show_fn_bodies && settings.show_fixpoints =>
        let doc_body =
          go_formattable(
            dbody,
            ~env=ClosureEnvironment.without_keys([x], env),
            FixF,
          );
        hcats(
          [DHDoc_common.Delim.fix_FixF, space(), text(x)]
          @ (
            settings.show_casts
              ? [
                DHDoc_common.Delim.colon_Fun,
                space(),
                DHDoc_Typ.mk(~enforce_inline=true, ty),
              ]
              : []
          )
          @ [
            space(),
            DHDoc_common.Delim.arrow_FixF,
            space(),
            doc_body |> DHDoc_common.pad_child(~enforce_inline),
          ],
        );
      | FixF(x, _, _) => annot(DHAnnot.Collapsed, text("<" ++ x ++ ">"))
      };
    };
    let steppable =
      next_steps |> List.find_opt(((ctx, _)) => ctx == EvalCtx.Mark);
    let stepped =
      chosen_step
      |> Option.map(x => x.ctx == Mark)
      |> Option.value(~default=false);
    let doc =
      switch (substitution) {
      | Some({d_loc: BoundVar(v), _}) when List.mem(v, recent_subst) =>
        hcats([text(v) |> annot(DHAnnot.Substituted), doc])
      | Some(_)
      | None => doc
      };
    let doc =
      if (stepped) {
        annot(DHAnnot.Stepped, doc);
      } else {
        switch (steppable) {
        | Some((_, full)) => annot(DHAnnot.Steppable(full), doc)
        | None => doc
        };
      };
    doc;
  };
  go(
    d,
    env,
    enforce_inline,
    previous_step,
    hidden_steps,
    chosen_step,
    List.mapi((idx, x: EvalObj.t) => (x.ctx, idx), next_steps),
    [],
  );
};
