open Haz3lcore;
open DHDoc_Exp;
module Doc = Pretty.Doc;

let rec mk =
        (
          ~settings: Settings.Evaluation.t,
          ~parenthesize=false,
          ~enforce_inline: bool,
          ~selected_hole_instance: option(HoleInstance.t),
          ~next_steps: list(EvaluatorStep.EvalObj.t),
          ~disabled=false,
          d: DHExp.t,
        )
        : DHDoc.t => {
  let precedence = precedence(~show_casts=settings.show_casts);
  let mk_cast = ((doc: DHDoc.t, ty: option(Typ.t))): DHDoc.t =>
    switch (ty) {
    | Some(ty) when settings.show_casts =>
      Doc.(
        hcat(
          doc,
          annot(
            DHAnnot.CastDecoration,
            DHDoc_Typ.mk(~enforce_inline=true, ty),
          ),
        )
      )
    | _ => doc
    };
  let rec go =
          (
            ~parenthesize=false,
            ~enforce_inline,
            d: DHExp.t,
            objs: list((EvaluatorStep.EvalObj.t, EvaluatorStep.EvalObj.t)),
          )
          : (DHDoc.t, option(Typ.t)) => {
    open Doc;
    let go' = go(~enforce_inline);
    let go_case = (dscrut, objs, drs) =>
      if (enforce_inline) {
        fail();
      } else {
        let scrut_doc =
          choices([
            hcats([
              space(),
              mk_cast(go(~enforce_inline=true, dscrut, objs)),
            ]),
            hcats([
              linebreak(),
              indent_and_align(
                mk_cast(go(~enforce_inline=false, dscrut, objs)),
              ),
            ]),
          ]);
        vseps(
          List.concat([
            [hcat(DHDoc_common.Delim.open_Case, scrut_doc)],
            drs
            |> List.map(
                 mk_rule(~settings, ~selected_hole_instance, ~next_steps),
               ),
            [DHDoc_common.Delim.close_Case],
          ]),
        );
      };
    let mk_left_associative_operands =
        (precedence_op, (d1, objs1), (d2, objs2)) => (
      go'(~parenthesize=precedence(d1) > precedence_op, d1, objs1),
      go'(~parenthesize=precedence(d2) >= precedence_op, d2, objs2),
    );
    let mk_right_associative_operands =
        (precedence_op, (d1, objs1), (d2, objs2)) => (
      go'(~parenthesize=precedence(d1) >= precedence_op, d1, objs1),
      go'(~parenthesize=precedence(d2) > precedence_op, d2, objs2),
    );
    let steppable =
      objs
      |> List.filter(((step, _)) =>
           switch (EvaluatorStep.EvalObj.get_ctx(step)) {
           | Mark => true
           | _ => false
           }
         )
      |> List.nth_opt(_, 0);
    let cast =
      switch (d) {
      | Cast(_, _, ty) => Some(ty)
      | _ => None
      };
    let unwrap =
      if (next_steps != []) {
        (l, sel) =>
          List.fold_right(
            ((step, full), lst) =>
              switch (EvaluatorStep.EvalObj.unwrap(step, sel)) {
              | Some(obj) => [(obj, full), ...lst]
              | None => lst
              },
            l,
            [],
          );
      } else {
        (l, _) => l;
      };
    let fdoc =
        (
          ~enforce_inline,
          ~d: DHExp.t,
          ~objs: list((EvaluatorStep.EvalObj.t, EvaluatorStep.EvalObj.t)),
        ) => {
      switch (d) {
      /* Now any of the postprocess checking is not done since most of
         the time the result is partial evaluated and those conditions
         cannot be met. */
      | Closure(_, _, d') => go'(d', unwrap(objs, Closure)) |> mk_cast

      | Filter({pat, act}, dbody) =>
        let keyword =
          switch (act) {
          | Pause => "step"
          | Eval => "skip"
          };
        let flt_doc = (~enforce_inline) =>
          mk_cast(go(~enforce_inline, pat, unwrap(objs, Filter)));
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
          mk_cast(go(~enforce_inline=false, dbody, [])),
        ]);
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
        go'(d', unwrap(objs, NonEmptyHole))
        |> mk_cast
        |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | ExpandingKeyword(u, i, k) =>
        DHDoc_common.mk_ExpandingKeyword((u, i), k)
      | FreeVar(u, i, x) =>
        text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
      | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
      | InconsistentBranches(u, i, Case(dscrut, drs, _)) =>
        let objs = unwrap(objs, InconsistentBranches);
        go_case(dscrut, objs, drs)
        |> annot(DHAnnot.InconsistentBranches((u, i)));

      | BoundVar(x) => text(x)
      | Constructor(name) => DHDoc_common.mk_ConstructorLit(name)
      | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
      | IntLit(n) => DHDoc_common.mk_IntLit(n)
      | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
      | StringLit(s) => DHDoc_common.mk_StringLit(s)
      | TestLit(_) => Doc.text(ExpandingKeyword.to_string(Test))
      | Sequence(d1, d2) =>
        let (doc1, doc2) = (go'(d1, unwrap(objs, Sequence)), go'(d2, []));
        DHDoc_common.mk_Sequence(mk_cast(doc1), mk_cast(doc2));
      | ListLit(_, _, _, d_list) =>
        let ol =
          d_list
          |> List.mapi((i, d) => go'(d, unwrap(objs, ListLit(i))))
          |> List.map(mk_cast);
        DHDoc_common.mk_ListLit(ol);
      | Ap(d1, d2) =>
        let (doc1, doc2) = (
          go'(
            ~parenthesize=precedence(d1) > DHDoc_common.precedence_Ap,
            d1,
            unwrap(objs, Ap1),
          ),
          go'(~parenthesize=false, d2, unwrap(objs, Ap2)),
        );
        DHDoc_common.mk_Ap(mk_cast(doc1), mk_cast(doc2));
      | ApBuiltin(ident, args) =>
        switch (args) {
        | [hd, ...tl] =>
          let d' = List.fold_left((d1, d2) => DHExp.Ap(d1, d2), hd, tl);
          let (doc1, doc2) =
            mk_left_associative_operands(
              DHDoc_common.precedence_Ap,
              (BoundVar(ident), []),
              (d', []),
            );
          DHDoc_common.mk_Ap(mk_cast(doc1), mk_cast(doc2));
        | [] => text(ident)
        }
      | BinIntOp(op, d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_int_op(op),
            (d1, unwrap(objs, BinIntOp1)),
            (d2, unwrap(objs, BinIntOp2)),
          );
        hseps([mk_cast(doc1), mk_bin_int_op(op), mk_cast(doc2)]);
      | BinFloatOp(op, d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_float_op(op),
            (d1, unwrap(objs, BinFloatOp1)),
            (d2, unwrap(objs, BinFloatOp2)),
          );
        hseps([mk_cast(doc1), mk_bin_float_op(op), mk_cast(doc2)]);
      | BinStringOp(op, d1, d2) =>
        // TODO assumes all bin string ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_string_op(op),
            (d1, unwrap(objs, BinStringOp1)),
            (d2, unwrap(objs, BinStringOp2)),
          );
        hseps([mk_cast(doc1), mk_bin_string_op(op), mk_cast(doc2)]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Cons,
            (d1, unwrap(objs, Cons1)),
            (d2, unwrap(objs, Cons2)),
          );
        DHDoc_common.mk_Cons(mk_cast(doc1), mk_cast(doc2));
      | ListConcat(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Plus,
            (d1, unwrap(objs, ListConcat1)),
            (d2, unwrap(objs, ListConcat2)),
          );
        DHDoc_common.mk_ListConcat(mk_cast(doc1), mk_cast(doc2));
      | BinBoolOp(op, d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            precedence_bin_bool_op(op),
            (d1, unwrap(objs, BinBoolOp1)),
            (d2, unwrap(objs, BinBoolOp2)),
          );
        hseps([mk_cast(doc1), mk_bin_bool_op(op), mk_cast(doc2)]);
      | Tuple([]) => DHDoc_common.Delim.triv
      | Tuple(ds) =>
        DHDoc_common.mk_Tuple(
          ds
          |> List.mapi((i, d) => mk_cast(go'(d, unwrap(objs, Tuple(i))))),
        )
      | Prj(d, n) =>
        DHDoc_common.mk_Prj(mk_cast(go'(d, unwrap(objs, Prj))), n)
      | ConsistentCase(Case(dscrut, drs, _)) =>
        let objs = unwrap(objs, ConsistentCase);
        go_case(dscrut, objs, drs);
      | Cast(d, _, _) =>
        let objs = unwrap(objs, Cast);
        let (doc, _) = go'(d, objs);
        doc;
      | Let(dp, ddef, dbody) =>
        let def_doc = (~enforce_inline) =>
          mk_cast(go(~enforce_inline, ddef, unwrap(objs, Let)));
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
          mk_cast(go(~enforce_inline=false, dbody, [])),
        ]);
      | FailedCast(Cast(d, ty1, ty2), ty2', ty3) when Typ.eq(ty2, ty2') =>
        let (d_doc, _) = go'(d, objs);
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
        let (d_doc, _) = go'(d, objs);
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);
      /* | InvalidOperation(d, err) => */
      /*   switch (err) { */
      /*   | DivideByZero => */
      /*     let (d_doc, _) = go'(d); */
      /*     let decoration = */
      /*       Doc.text(InvalidOperationError.err_msg(err)) */
      /*       |> annot(DHAnnot.DivideByZero); */
      /*     hcats([d_doc, decoration]); */
      /*   } */
      /*
       let (d_doc, d_cast) as dcast_doc = go'(d);
       let cast_decoration =
         hcats([
           DHDoc_common.Delim.open_FailedCast,
           hseps([
             DHDoc_Typ.mk(~enforce_inline=true, ty1),
             DHDoc_common.Delim.arrow_FailedCast,
             DHDoc_Typ.mk(~enforce_inline=true, ty2),
           ]),
           DHDoc_common.Delim.close_FailedCast,
         ])
         |> annot(DHAnnot.FailedCastDecoration);
       switch (d_cast) {
       | Some(ty1') when Typ.eq(ty1, ty1') =>
         hcats([d_doc, cast_decoration])
       | _ => hcats([mk_cast(dcast_doc), cast_decoration])
       };
       */

      | Fun(dp, ty, dbody, s) =>
        if (settings.show_fn_bodies) {
          let body_doc = (~enforce_inline) =>
            mk_cast(go(~enforce_inline, dbody, objs));
          hcats([
            DHDoc_common.Delim.sym_Fun,
            DHDoc_Pat.mk(dp)
            |> DHDoc_common.pad_child(
                 ~inline_padding=(space(), space()),
                 ~enforce_inline,
               ),
            DHDoc_common.Delim.colon_Fun,
            space(),
            DHDoc_Typ.mk(~enforce_inline=true, ty),
            space(),
            DHDoc_common.Delim.open_Fun,
            body_doc |> DHDoc_common.pad_child(~enforce_inline),
            DHDoc_common.Delim.close_Fun,
          ]);
        } else {
          switch (s) {
          | None => annot(DHAnnot.Collapsed, text("<anon fn>"))
          | Some(name) => annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"))
          };
        }
      | FixF(x, ty, dbody) =>
        if (settings.show_fn_bodies) {
          let doc_body = (~enforce_inline) =>
            go(~enforce_inline, dbody, objs) |> mk_cast;
          hcats([
            DHDoc_common.Delim.fix_FixF,
            space(),
            text(x),
            DHDoc_common.Delim.colon_FixF,
            DHDoc_Typ.mk(~enforce_inline=true, ty),
            DHDoc_common.Delim.open_FixF,
            doc_body |> DHDoc_common.pad_child(~enforce_inline),
            DHDoc_common.Delim.close_FixF,
          ]);
        } else {
          annot(DHAnnot.Collapsed, text("<fn>"));
        }
      };
    };
    let parenthesize = doc =>
      if (parenthesize) {
        hcats([
          DHDoc_common.Delim.open_Parenthesized,
          doc |> DHDoc_common.pad_child(~enforce_inline),
          DHDoc_common.Delim.close_Parenthesized,
        ]);
      } else {
        doc(~enforce_inline);
      };
    let doc = (~enforce_inline) =>
      switch (steppable) {
      | Some((_, full)) =>
        fdoc(~enforce_inline, ~d, ~objs=[])
        |> annot(
             if (disabled) {
               DHAnnot.Stepped;
             } else {
               DHAnnot.Steppable(full);
             },
           )
      | None => fdoc(~enforce_inline, ~d, ~objs)
      };
    (parenthesize(doc), cast);
  };
  mk_cast(
    go(
      ~parenthesize,
      ~enforce_inline,
      d,
      List.combine(next_steps, next_steps),
    ),
  );
}
and mk_rule =
    (
      ~settings,
      ~selected_hole_instance,
      ~next_steps,
      Rule(dp, dclause): DHExp.rule,
    )
    : DHDoc.t => {
  open Doc;
  let mk' = mk(~settings, ~selected_hole_instance);
  let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
  let clause_doc =
    settings.show_case_clauses
      ? choices([
          hcats([space(), mk'(~enforce_inline=true, ~next_steps, dclause)]),
          hcats([
            linebreak(),
            indent_and_align(
              mk'(~enforce_inline=false, ~next_steps, dclause),
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
