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
  switch (DHExp.term_of(d)) {
  | Var(_)
  | FreeVar(_)
  | InvalidText(_)
  | Bool(_)
  | Int(_)
  | Seq(_)
  | Test(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | EmptyHole
  | Constructor(_)
  | FailedCast(_)
  | InvalidOperation(_)
  | If(_)
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
  | Match(_) => DHDoc_common.precedence_max

  | BinOp(Bool(op), _, _) => precedence_bin_bool_op(op)
  | BinOp(Int(op), _, _) => precedence_bin_int_op(op)
  | BinOp(Float(op), _, _) => precedence_bin_float_op(op)
  | BinOp(String(op), _, _) => precedence_bin_string_op(op)

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
      ~selected_hole_instance: option(Id.t),
      // The next four are used when drawing the stepper to track where we can annotate changes
      ~previous_step: option((step, Id.t)), // The step that will be displayed above this one (an Id in included because it may have changed since the step was taken)
      ~hidden_steps: list((step, Id.t)), // The hidden steps between the above and the current one (an Id in included because it may have changed since the step was taken)
      ~chosen_step: option(step), // The step that will be taken next
      ~next_steps: list((int, Id.t)), // The options for the next step, if it hasn't been chosen yet
      ~env: ClosureEnvironment.t,
      d: DHExp.t,
    )
    : DHDoc.t => {
  // // print_endline("");
  // // let _ =
  // //   List.map(
  // //     ((x, y)) => {
  // //       print_endline(Id.show(y));
  // //       print_endline(show_step_kind(x.knd));
  // //     },
  // //     hidden_steps,
  // //   );
  // let _ = print_endline("============");
  let precedence = precedence(~show_casts=settings.show_casts);
  let rec go =
          (
            d: DHExp.t,
            env: ClosureEnvironment.t,
            enforce_inline: bool,
            recent_subst: list(Var.t),
            recursive_calls: list(Var.t),
          )
          : DHDoc.t => {
    open Doc;
    let recent_subst =
      switch (previous_step) {
      | Some((ps, id)) when id == DHExp.rep_id(d) =>
        switch (ps.knd, DHExp.term_of(ps.d_loc)) {
        | (FunAp, Ap(_, d2, _)) =>
          switch (DHExp.term_of(d2)) {
          | Fun(p, _, _, _, _) => DHPat.bound_vars(p)
          | _ => []
          }
        | (FunAp, _) => []
        | (LetBind, Let(p, _, _)) => DHPat.bound_vars(p)
        | (LetBind, _) => []
        | (FixUnwrap, FixF(p, _, _)) => DHPat.bound_vars(p)
        | (FixUnwrap, _) => []
        | (InvalidStep, _)
        | (VarLookup, _)
        | (Seq, _)
        | (FunClosure, _)
        | (FixClosure, _)
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
        | (CompleteClosure, _)
        | (CompleteFilter, _)
        | (Cast, _)
        | (Conditional(_), _)
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
        ) => {
      go(d, env, enforce_inline, recent_subst, recursive_calls);
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
    let go_case_rule = ((dp, dclause)): DHDoc.t => {
      let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
      let clause_doc =
        settings.show_case_clauses
          ? choices([
              hcats([space(), go'(~enforce_inline=true, dclause)]),
              hcats([
                linebreak(),
                indent_and_align(go'(~enforce_inline=false, dclause)),
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
    let go_case = (dscrut, drs) =>
      if (enforce_inline) {
        fail();
      } else {
        let scrut_doc =
          choices([
            hcats([space(), go'(~enforce_inline=true, dscrut)]),
            hcats([
              linebreak(),
              indent_and_align(go'(~enforce_inline=false, dscrut)),
            ]),
          ]);
        vseps(
          List.concat([
            [hcat(DHDoc_common.Delim.open_Case, scrut_doc)],
            drs |> List.map(go_case_rule),
            [DHDoc_common.Delim.close_Case],
          ]),
        );
      };
    let go_formattable = (~enforce_inline) => go'(~enforce_inline);
    let mk_left_associative_operands = (precedence_op, d1, d2) => (
      go_formattable(d1) |> parenthesize(precedence(d1) > precedence_op),
      go_formattable(d2) |> parenthesize(precedence(d2) >= precedence_op),
    );
    let mk_right_associative_operands = (precedence_op, d1, d2) => (
      go_formattable(d1) |> parenthesize(precedence(d1) >= precedence_op),
      go_formattable(d2) |> parenthesize(precedence(d2) > precedence_op),
    );
    let doc = {
      switch (DHExp.term_of(d)) {
      | Closure(env', d') => go'(d', ~env=env')
      | Filter(flt, d') =>
        if (settings.show_stepper_filters) {
          switch (flt) {
          | Filter({pat, act}) =>
            let keyword = FilterAction.string_of_t(act);
            let flt_doc = go_formattable(pat);
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
              go'(d'),
            ]);
          | Residue(_, act) =>
            let keyword = FilterAction.string_of_t(act);
            vseps([DHDoc_common.Delim.mk(keyword), go'(d')]);
          };
        } else {
          switch (flt) {
          | Residue(_) => go'(d')
          | Filter(_) => go'(d')
          };
        }

      /* Hole expressions must appear within a closure in
         the postprocessed result */
      | EmptyHole =>
        DHDoc_common.mk_EmptyHole(
          ~selected=Some(DHExp.rep_id(d)) == selected_hole_instance,
          env,
        )
      | NonEmptyHole(reason, u, i, d') =>
        go'(d') |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | FreeVar(u, i, x) =>
        text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
      | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
      | Match(Inconsistent(u, i), dscrut, drs) =>
        go_case(dscrut, drs) |> annot(DHAnnot.InconsistentBranches((u, i)))
      | Var(x) when List.mem(x, recursive_calls) => text(x)
      | Var(x) when settings.show_lookup_steps => text(x)
      | Var(x) =>
        switch (ClosureEnvironment.lookup(env, x)) {
        | None => text(x)
        | Some(d') =>
          if (List.mem(x, recent_subst)) {
            hcats([
              go'(~env=ClosureEnvironment.empty, d)
              |> annot(DHAnnot.Substituted),
              go'(~env=ClosureEnvironment.empty, d'),
            ]);
          } else {
            go'(~env=ClosureEnvironment.empty, d');
          }
        }
      | BuiltinFun(f) => text(f)
      | Constructor(name) => DHDoc_common.mk_ConstructorLit(name)
      | Bool(b) => DHDoc_common.mk_BoolLit(b)
      | Int(n) => DHDoc_common.mk_IntLit(n)
      | Float(f) => DHDoc_common.mk_FloatLit(f)
      | String(s) => DHDoc_common.mk_StringLit(s)
      | Test(_, d) => DHDoc_common.mk_Test(go'(d))
      | Seq(d1, d2) =>
        let (doc1, doc2) = (go'(d1), go'(d2));
        DHDoc_common.mk_Sequence(doc1, doc2);
      | ListLit(_, _, _, d_list) =>
        let ol = d_list |> List.map(d => go'(d));
        DHDoc_common.mk_ListLit(ol);
      | Ap(Forward, d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2),
        );
        DHDoc_common.mk_Ap(doc1, doc2);
      | Ap(Reverse, d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2),
        );
        DHDoc_common.mk_rev_Ap(doc2, doc1);
      | ApBuiltin(ident, d) =>
        DHDoc_common.mk_Ap(
          text(ident),
          go_formattable(d)
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Ap),
        )
      | BinOp(Int(op), d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_int_op(op), d1, d2);
        hseps([doc1, mk_bin_int_op(op), doc2]);
      | BinOp(Float(op), d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_float_op(op), d1, d2);
        hseps([doc1, mk_bin_float_op(op), doc2]);
      | BinOp(String(op), d1, d2) =>
        // TODO assumes all bin string ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_string_op(op), d1, d2);
        hseps([doc1, mk_bin_string_op(op), doc2]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(DHDoc_common.precedence_Cons, d1, d2);
        DHDoc_common.mk_Cons(doc1, doc2);
      | ListConcat(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(DHDoc_common.precedence_Plus, d1, d2);
        DHDoc_common.mk_ListConcat(doc1, doc2);
      | BinOp(Bool(op), d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(precedence_bin_bool_op(op), d1, d2);
        hseps([doc1, mk_bin_bool_op(op), doc2]);
      | Tuple([]) => DHDoc_common.Delim.triv
      | Tuple(ds) => DHDoc_common.mk_Tuple(ds |> List.map(d => go'(d)))
      | Match(Consistent, dscrut, drs) => go_case(dscrut, drs)
      | Cast(d, _, ty) when settings.show_casts =>
        // TODO[Matt]: Roll multiple casts into one cast
        let doc = go'(d);
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
        let doc = go'(d);
        doc;
      | Let(dp, ddef, dbody) =>
        if (enforce_inline) {
          fail();
        } else {
          let bindings = DHPat.bound_vars(dp);
          let def_doc = go_formattable(ddef);
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
            ),
          ]);
        }
      | FailedCast(d1, ty2', ty3) =>
        switch (DHExp.term_of(d1)) {
        | Cast(d, ty1, ty2) when Typ.eq(ty2, ty2') =>
          let d_doc = go'(d);
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
        | _ => failwith("unexpected FailedCast without inner cast")
        }
      | InvalidOperation(d, err) =>
        let d_doc = go'(d);
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);

      | If(_, c, d1, d2) =>
        let c_doc = go_formattable(c);
        let d1_doc = go_formattable(d1);
        let d2_doc = go_formattable(d2);
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
      | Fun(dp, ty, d, Some(env'), s) =>
        if (settings.show_fn_bodies) {
          let bindings = DHPat.bound_vars(dp);
          let body_doc =
            go_formattable(
              Closure(
                ClosureEnvironment.without_keys(Option.to_list(s), env'),
                d,
              )
              |> DHExp.fresh,
              ~env=
                ClosureEnvironment.without_keys(
                  DHPat.bound_vars(dp) @ Option.to_list(s),
                  env,
                ),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), recent_subst),
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
              body_doc |> DHDoc_common.pad_child(~enforce_inline=false),
            ],
          );
        } else {
          switch (s) {
          | None => annot(DHAnnot.Collapsed, text("<anon fn>"))
          | Some(name) => annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"))
          };
        }
      | Fun(dp, ty, dbody, None, s) =>
        if (settings.show_fn_bodies) {
          let bindings = DHPat.bound_vars(dp);
          let body_doc =
            go_formattable(
              dbody,
              ~env=ClosureEnvironment.without_keys(bindings, env),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), recent_subst),
              ~recursive_calls=Option.to_list(s) @ recursive_calls,
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
      | FixF(dp, ty, dbody) when settings.show_fixpoints =>
        let doc_body =
          go_formattable(
            dbody,
            ~env=ClosureEnvironment.without_keys(DHPat.bound_vars(dp), env),
          );
        hcats(
          [
            DHDoc_common.Delim.fix_FixF,
            space(),
            DHDoc_Pat.mk(dp, ~enforce_inline=true),
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
            DHDoc_common.Delim.arrow_FixF,
            space(),
            doc_body |> DHDoc_common.pad_child(~enforce_inline),
          ],
        );
      | FixF(dp, _, d) =>
        go'(
          ~env=ClosureEnvironment.without_keys(DHPat.bound_vars(dp), env),
          d,
        )
      };
    };
    let steppable =
      next_steps |> List.find_opt(((_, id)) => id == DHExp.rep_id(d));
    let stepped =
      chosen_step
      |> Option.map(x => DHExp.rep_id(x.d_loc) == DHExp.rep_id(d))
      |> Option.value(~default=false);
    let substitution =
      hidden_steps
      |> List.find_opt(((step, id)) =>
           step.knd == VarLookup
           // HACK[Matt]: to prevent substitutions hiding inside casts
           && id == DHExp.rep_id(d)
         );
    let doc =
      switch (substitution) {
      | Some((step, _)) =>
        switch (DHExp.term_of(step.d_loc)) {
        | Var(v) when List.mem(v, recent_subst) =>
          hcats([text(v) |> annot(DHAnnot.Substituted), doc])
        | _ => doc
        }
      | None => doc
      };
    let doc =
      if (stepped) {
        annot(DHAnnot.Stepped, doc);
      } else {
        switch (steppable) {
        | Some((i, _)) => annot(DHAnnot.Steppable(i), doc)
        | None => doc
        };
      };
    doc;
  };
  go(d, env, enforce_inline, [], []);
};
