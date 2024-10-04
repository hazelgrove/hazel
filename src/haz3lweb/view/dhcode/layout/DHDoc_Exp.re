open Haz3lcore;
open EvaluatorStep;
open Transition;
module Doc = Pretty.Doc;

let precedence_bin_bool_op = (op: Operators.op_bin_bool) =>
  switch (op) {
  | And => DHDoc_common.precedence_And
  | Or => DHDoc_common.precedence_Or
  };

let precedence_bin_int_op = (bio: Operators.op_bin_int) =>
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
let precedence_bin_float_op = (bfo: Operators.op_bin_float) =>
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
let precedence_bin_string_op = (bso: Operators.op_bin_string) =>
  switch (bso) {
  | Concat => DHDoc_common.precedence_Plus
  | Equals => DHDoc_common.precedence_Equals
  };
let rec precedence = (~show_function_bodies, ~show_casts: bool, d: DHExp.t) => {
  let precedence' = precedence(~show_function_bodies, ~show_casts);
  switch (DHExp.term_of(d)) {
  | Var(_)
  | Invalid(_)
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
  | DynamicErrorHole(_)
  | If(_)
  | Closure(_)
  | BuiltinFun(_)
  | Deferral(_)
  | Undefined
  | Filter(_) => DHDoc_common.precedence_const
  | Cast(d1, _, _) =>
    show_casts ? DHDoc_common.precedence_Ap : precedence'(d1)
  | DeferredAp(_)
  | Ap(_)
  | TypAp(_) => DHDoc_common.precedence_Ap
  | Cons(_) => DHDoc_common.precedence_Cons
  | ListConcat(_) => DHDoc_common.precedence_Plus
  | Tuple(_) => DHDoc_common.precedence_Comma
  | TypFun(_)
  | Fun(_) when !show_function_bodies => DHDoc_common.precedence_const
  | TypFun(_)
  | Fun(_) => DHDoc_common.precedence_max
  | Let(_)
  | Theorem(_)
  | TyAlias(_)
  | FixF(_)
  | Match(_) => DHDoc_common.precedence_max
  | UnOp(Meta(Unquote), _) => DHDoc_common.precedence_Ap
  | UnOp(Bool(Not), _) => DHDoc_common.precedence_Not
  | UnOp(Int(Minus), _) => DHDoc_common.precedence_Minus
  | BinOp(Bool(op), _, _) => precedence_bin_bool_op(op)
  | BinOp(Int(op), _, _) => precedence_bin_int_op(op)
  | BinOp(Float(op), _, _) => precedence_bin_float_op(op)
  | BinOp(String(op), _, _) => precedence_bin_string_op(op)
  | MultiHole(_) => DHDoc_common.precedence_max
  | Parens(d) => precedence'(d)
  };
};

let mk_bin_bool_op = (op: Operators.op_bin_bool): DHDoc.t =>
  Doc.text(Operators.bool_op_to_string(op));

let mk_bin_int_op = (op: Operators.op_bin_int): DHDoc.t =>
  Doc.text(Operators.int_op_to_string(op));

let mk_bin_float_op = (op: Operators.op_bin_float): DHDoc.t =>
  Doc.text(Operators.float_op_to_string(op));

let mk_bin_string_op = (op: Operators.op_bin_string): DHDoc.t =>
  Doc.text(Operators.string_op_to_string(op));

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
      ~infomap: Statics.Map.t,
      d: DHExp.t,
    )
    : DHDoc.t => {
  let precedence =
    precedence(
      ~show_casts=settings.show_casts,
      ~show_function_bodies=settings.show_fn_bodies,
    );
  let rec go =
          (
            d: DHExp.t,
            env: ClosureEnvironment.t,
            enforce_inline: bool,
            recent_subst: list(Var.t),
          )
          : DHDoc.t => {
    open Doc;
    let recent_subst =
      switch (previous_step) {
      | Some((ps, id)) when id == DHExp.rep_id(d) =>
        switch (ps.knd, DHExp.term_of(ps.d_loc)) {
        | (FunAp, Ap(_, d2, _)) =>
          switch (DHExp.term_of(d2)) {
          | Fun(p, _, _, _) => DHPat.bound_vars(p)
          | _ => []
          }
        | (FunAp, _) => []
        | (LetBind, Let(p, _, _)) => DHPat.bound_vars(p)
        | (LetBind, _) => []
        | (FixUnwrap, FixF(p, _, _)) => DHPat.bound_vars(p)
        | (FixUnwrap, _) => []
        | (TypFunAp, _) // TODO: Could also do something here for type variable substitution like in FunAp?
        | (InvalidStep, _)
        | (VarLookup, _)
        | (Seq, _)
        | (FunClosure, _)
        | (FixClosure, _)
        | (DeferredAp, _)
        | (UpdateTest, _)
        | (CastTypAp, _)
        | (CastAp, _)
        | (BuiltinWrap, _)
        | (UnOp(_), _)
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
        | (RemoveParens, _)
        | (RemoveTypeAlias, _) => [] // Maybe this last one could count as a substitution?
        }
      | _ => recent_subst
      };
    let go' =
        (
          ~env=env,
          ~enforce_inline=enforce_inline,
          ~recent_subst=recent_subst,
          d,
        ) => {
      go(d, env, enforce_inline, recent_subst);
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
        DHDoc_Pat.mk(~infomap, ~show_casts=settings.show_casts, dp)
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
      | Parens(d') => go'(d')
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
      | MultiHole(_ds) =>
        DHDoc_common.mk_EmptyHole(
          ~selected=Some(DHExp.rep_id(d)) == selected_hole_instance,
          env,
        )
      | Invalid(t) => DHDoc_common.mk_InvalidText(t)
      | Var(x) when settings.show_lookup_steps => text(x)
      | Var(x) =>
        switch (ClosureEnvironment.lookup(env, x)) {
        | None => text(x)
        | Some(d') =>
          if (List.mem(x, recent_subst)) {
            hcats([
              go'(~env=ClosureEnvironment.empty, d)
              |> annot(DHAnnot.Substituted),
              go'(
                ~env=ClosureEnvironment.empty,
                ~recent_subst=List.filter(u => u != x, recent_subst),
                d',
              ),
            ]);
          } else {
            go'(~env=ClosureEnvironment.empty, d');
          }
        }
      | BuiltinFun(f) => text(f)
      | Constructor(name, _) => DHDoc_common.mk_ConstructorLit(name)
      | Bool(b) => DHDoc_common.mk_BoolLit(b)
      | Int(n) => DHDoc_common.mk_IntLit(n)
      | Float(f) => DHDoc_common.mk_FloatLit(f)
      | String(s) => DHDoc_common.mk_StringLit(s)
      | Undefined => DHDoc_common.mk_Undefined()
      | Test(d) => DHDoc_common.mk_Test(go'(d))
      | Deferral(_) => text("_")
      | Seq(d1, d2) =>
        let (doc1, doc2) = (go'(d1), go'(d2));
        DHDoc_common.mk_Sequence(doc1, doc2);
      | ListLit(d_list) =>
        let ol = d_list |> List.map(d => go'(d));
        DHDoc_common.mk_ListLit(ol);
      | Ap(Forward, d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2),
        );
        DHDoc_common.mk_Ap(doc1, doc2);
      | DeferredAp(d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(Tuple(d2) |> DHExp.fresh),
        );
        DHDoc_common.mk_Ap(doc1, doc2);
      | TypAp(d1, ty) =>
        let doc1 = go'(d1);
        let doc2 = DHDoc_Typ.mk(~enforce_inline=true, ty);
        DHDoc_common.mk_TypAp(doc1, doc2);
      | Ap(Reverse, d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2),
        );
        DHDoc_common.mk_rev_Ap(doc2, doc1);
      | UnOp(Meta(Unquote), d) =>
        DHDoc_common.mk_Ap(
          text("$"),
          go_formattable(d)
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Ap),
        )
      | UnOp(Bool(Not), d) =>
        DHDoc_common.mk_Ap(
          text("!"),
          go_formattable(d)
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Not),
        )
      | UnOp(Int(Minus), d) =>
        DHDoc_common.mk_Ap(
          text("-"),
          go_formattable(d)
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Minus),
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
      | Match(dscrut, drs) => go_case(dscrut, drs)
      | TyAlias(_, _, d) => go'(d)
      | Cast(d, t1, t2) when settings.show_casts =>
        // TODO[Matt]: Roll multiple casts into one cast
        let doc =
          go_formattable(d)
          |> parenthesize(precedence(d) > DHDoc_common.precedence_Ap);
        Doc.(
          hcat(
            doc,
            annot(
              DHAnnot.CastDecoration,
              hcats([
                DHDoc_common.Delim.open_Cast,
                DHDoc_Typ.mk(~enforce_inline=true, t1),
                DHDoc_common.Delim.arrow_Cast,
                DHDoc_Typ.mk(~enforce_inline=true, t2),
                DHDoc_common.Delim.close_Cast,
              ]),
            ),
          )
        );
      | Cast(d, _, _) =>
        let doc = go'(d);
        doc;
      | Theorem(dp, ddef, dbody)
      | Let(dp, ddef, dbody) =>
        if (enforce_inline) {
          fail();
        } else {
          let bindings = DHPat.bound_vars(dp);
          let def_doc = go_formattable(ddef);
          vseps([
            hcats([
              DHDoc_common.Delim.mk("let"),
              DHDoc_Pat.mk(~infomap, ~show_casts=settings.show_casts, dp)
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
      | FailedCast(d1, ty1, ty3) =>
        let d_doc = go'(d1);
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
      | DynamicErrorHole(d, err) =>
        let d_doc = go'(d);
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);
      | If(c, d1, d2) =>
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
      | Fun(dp, d, Some(env'), s) =>
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
              DHDoc_Pat.mk(~infomap, ~show_casts=settings.show_casts, dp)
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
            ]
            @ [
              DHDoc_common.Delim.arrow_Fun,
              space(),
              body_doc |> DHDoc_common.pad_child(~enforce_inline=false),
            ],
          );
        } else {
          annot(
            DHAnnot.Collapsed,
            text(
              switch (s) {
              | None => "<anon fn>"
              | Some(name)
                  when
                    !settings.show_fixpoints
                    && String.ends_with(~suffix="+", name) =>
                "<" ++ String.sub(name, 0, String.length(name) - 1) ++ ">"
              | Some(name) => "<" ++ name ++ ">"
              },
            ),
          );
        }
      | Fun(dp, dbody, None, s) =>
        if (settings.show_fn_bodies) {
          let bindings = DHPat.bound_vars(dp);
          let body_doc =
            go_formattable(
              dbody,
              ~env=ClosureEnvironment.without_keys(bindings, env),
              ~recent_subst=
                List.filter(x => !List.mem(x, bindings), recent_subst),
            );
          hcats(
            [
              DHDoc_common.Delim.sym_Fun,
              DHDoc_Pat.mk(~infomap, ~show_casts=settings.show_casts, dp)
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
            ]
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
      | FixF(dp, dbody, _)
          when settings.show_fn_bodies && settings.show_fixpoints =>
        let doc_body =
          go_formattable(
            dbody,
            ~env=ClosureEnvironment.without_keys(DHPat.bound_vars(dp), env),
          );
        hcats(
          [
            DHDoc_common.Delim.fix_FixF,
            space(),
            DHDoc_Pat.mk(
              ~infomap,
              dp,
              ~show_casts=settings.show_casts,
              ~enforce_inline=true,
            ),
          ]
          @ [
            space(),
            DHDoc_common.Delim.arrow_FixF,
            space(),
            doc_body |> DHDoc_common.pad_child(~enforce_inline),
          ],
        );
      | FixF(_, {term: Fun(_, _, _, Some(x)), _}, _) =>
        if (String.ends_with(~suffix="+", x)) {
          annot(
            DHAnnot.Collapsed,
            text("<" ++ String.sub(x, 0, String.length(x) - 1) ++ ">"),
          );
        } else {
          annot(DHAnnot.Collapsed, text("<" ++ x ++ ">"));
        }
      | FixF(_, _, _) => annot(DHAnnot.Collapsed, text("<anon fn>"))
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
  go(d, env, enforce_inline, []);
};
