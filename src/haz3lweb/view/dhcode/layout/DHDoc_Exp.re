open Haz3lcore;
module Doc = Pretty.Doc;

let precedence_un_int_op = (op: DHExp.UnIntOp.t) =>
  switch (op) {
  | Minus => DHDoc_common.precedence_Neg
  };

let precedence_bin_bool_op = (op: DHExp.BinBoolOp.t) =>
  switch (op) {
  | And => DHDoc_common.precedence_And
  | Or => DHDoc_common.precedence_Or
  };

let precedence_bin_int_op = (bio: DHExp.BinIntOp.t) =>
  switch (bio) {
  | Times => DHDoc_common.precedence_Times
  | Power => DHDoc_common.precedence_Power
  | Divide => DHDoc_common.precedence_Divide
  | Plus => DHDoc_common.precedence_Plus
  | Minus => DHDoc_common.precedence_Minus
  | Equals => DHDoc_common.precedence_Equals
  | LessThan => DHDoc_common.precedence_LessThan
  | LessThanOrEqual => DHDoc_common.precedence_LessThan
  | GreaterThan => DHDoc_common.precedence_GreaterThan
  | GreaterThanOrEqual => DHDoc_common.precedence_GreaterThan
  };
let precedence_bin_float_op = (bfo: DHExp.BinFloatOp.t) =>
  switch (bfo) {
  | Times => DHDoc_common.precedence_Times
  | Power => DHDoc_common.precedence_Power
  | Divide => DHDoc_common.precedence_Divide
  | Plus => DHDoc_common.precedence_Plus
  | Minus => DHDoc_common.precedence_Minus
  | Equals => DHDoc_common.precedence_Equals
  | LessThan => DHDoc_common.precedence_LessThan
  | LessThanOrEqual => DHDoc_common.precedence_LessThan
  | GreaterThan => DHDoc_common.precedence_GreaterThan
  | GreaterThanOrEqual => DHDoc_common.precedence_GreaterThan
  };
let precedence_bin_string_op = (bso: DHExp.BinStringOp.t) =>
  switch (bso) {
  | Equals => DHDoc_common.precedence_Equals
  };
let rec precedence = (~show_casts: bool, d: DHExp.t) => {
  let precedence' = precedence(~show_casts);
  switch (d.term) {
  | Parens(_)
  | Var(_)
  | Hole(_, FreeVar(_))
  | Hole(_, Invalid(_))
  | Hole(_, InvalidText(_))
  | Hole(_, ExpandingKeyword(_))
  | Bool(_)
  | Int(_)
  | Seq(_)
  | Test(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | Inj(_)
  | Prj(_)
  | Tag(_)
  | Hole(_, EmptyHole)
  | Hole(_, MultiHole(_))
  | Hole(_, FailedCast(_))
  | Hole(_, InvalidOperation(_))
  | Fun(_)
  | Closure(_) => DHDoc_common.precedence_const
  | Cast(d1, _, _) =>
    show_casts ? DHDoc_common.precedence_const : precedence'(d1)
  | Let(_)
  | FixF(_)
  | If(_)
  | Match(_)
  | Hole(_, InconsistentBranches(_)) => DHDoc_common.precedence_max
  | UnOp(Int(op), _) => precedence_un_int_op(op)
  | BinOp(Bool(op), _, _) => precedence_bin_bool_op(op)
  | BinOp(Int(op), _, _) => precedence_bin_int_op(op)
  | BinOp(Float(op), _, _) => precedence_bin_float_op(op)
  | BinOp(String(op), _, _) => precedence_bin_string_op(op)
  | Ap(_) => DHDoc_common.precedence_Ap
  | ApBuiltin(_) => DHDoc_common.precedence_Ap
  | Cons(_) => DHDoc_common.precedence_Cons
  | Triv
  | Tuple(_) => DHDoc_common.precedence_Comma

  | Hole(_, NonEmptyHole(_, d)) => precedence'(d)
  };
};

let mk_un_int_op = (op: DHExp.UnIntOp.t): DHDoc.t =>
  Doc.text(
    switch (op) {
    | Minus => "-"
    },
  );

let mk_bin_bool_op = (op: DHExp.BinBoolOp.t): DHDoc.t =>
  Doc.text(
    switch (op) {
    | And => "&&"
    | Or => "||"
    },
  );

let mk_bin_int_op = (op: DHExp.BinIntOp.t): DHDoc.t =>
  Doc.text(
    switch (op) {
    | Minus => "-"
    | Plus => "+"
    | Times => "*"
    | Power => "**"
    | Divide => "/"
    | LessThan => "<"
    | LessThanOrEqual => "<="
    | GreaterThan => ">"
    | GreaterThanOrEqual => ">="
    | Equals => "=="
    },
  );

let mk_bin_float_op = (op: DHExp.BinFloatOp.t): DHDoc.t =>
  Doc.text(
    switch (op) {
    | Minus => "-."
    | Plus => "+."
    | Times => "*."
    | Power => "**."
    | Divide => "/."
    | LessThan => "<."
    | LessThanOrEqual => "<=."
    | GreaterThan => ">."
    | GreaterThanOrEqual => ">=."
    | Equals => "==."
    },
  );

let mk_bin_string_op = (op: DHExp.BinStringOp.t): DHDoc.t =>
  Doc.text(
    switch (op) {
    | Equals => "$=="
    },
  );

let rec mk =
        (
          ~settings: Settings.Evaluation.t,
          ~parenthesize=false,
          ~enforce_inline: bool,
          ~selected_hole_instance: option(HoleInstance.t),
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
          (~parenthesize=false, ~enforce_inline, d: DHExp.t)
          : (DHDoc.t, option(Typ.t)) => {
    open Doc;
    let go' = go(~enforce_inline);
    let go_case = (dscrut, drs) =>
      if (enforce_inline) {
        fail();
      } else {
        let scrut_doc =
          choices([
            hcats([space(), mk_cast(go(~enforce_inline=true, dscrut))]),
            hcats([
              linebreak(),
              indent_and_align(mk_cast(go(~enforce_inline=false, dscrut))),
            ]),
          ]);
        vseps(
          List.concat([
            [hcat(DHDoc_common.Delim.open_Case, scrut_doc)],
            drs |> List.map(mk_rule(~settings, ~selected_hole_instance)),
            [DHDoc_common.Delim.close_Case],
          ]),
        );
      };
    let go_if = (c, t, e) =>
      if (enforce_inline) {
        fail();
      } else {
        let c_doc =
          choices([
            hcats([space(), mk_cast(go(~enforce_inline=true, c))]),
            hcats([
              linebreak(),
              indent_and_align(mk_cast(go(~enforce_inline=false, c))),
            ]),
          ]);
        let mk_clause = mk_clause(~settings, ~selected_hole_instance);
        vseps(
          List.concat([
            [
              hcats([
                DHDoc_common.Delim.open_If,
                c_doc,
                DHDoc_common.Delim.then_If,
              ]),
            ],
            [mk_clause(t)],
            [DHDoc_common.Delim.else_If],
            [mk_clause(e)],
          ]),
        );
      };
    let mk_left_associative_operands = (precedence_op, d1, d2) => (
      go'(~parenthesize=precedence(d1) > precedence_op, d1),
      go'(~parenthesize=precedence(d2) >= precedence_op, d2),
    );
    let mk_right_associative_operands = (precedence_op, d1, d2) => (
      go'(~parenthesize=precedence(d1) >= precedence_op, d1),
      go'(~parenthesize=precedence(d2) > precedence_op, d2),
    );
    let cast =
      switch (d) {
      | {term: Cast(_, _, ty), _} => Some(ty)
      | _ => None
      };
    let fdoc = (~enforce_inline) =>
      switch (d.term) {
      | Parens(d) =>
        hcats([
          DHDoc_common.Delim.open_Parenthesized,
          mk_cast(go'(~parenthesize=true, d)),
          DHDoc_common.Delim.close_Parenthesized,
        ])
      /* A closure may only exist around hole expressions in
         the postprocessed result */
      | Closure(_, d') =>
        switch (d'.term) {
        | Hole(Some((u, i)), EmptyHole) =>
          let selected =
            switch (selected_hole_instance) {
            | None => false
            | Some((u', i')) => u == u' && i == i'
            };
          DHDoc_common.mk_EmptyHole(~selected, (u, i));
        | Hole(Some(hi), NonEmptyHole(reason, d')) =>
          go'(d') |> mk_cast |> annot(DHAnnot.NonEmptyHole(reason, hi))
        | Hole(Some(hi), ExpandingKeyword(k)) =>
          DHDoc_common.mk_ExpandingKeyword(hi, k)
        | Hole(Some(hi), FreeVar(x)) =>
          text(x) |> annot(DHAnnot.VarHole(Free, hi))
        | Hole(Some(hi), InvalidText(t)) =>
          DHDoc_common.mk_InvalidText(t, hi)
        | Hole(Some(hi), InconsistentBranches(dscrut, drs, _)) =>
          go_case(dscrut, drs) |> annot(DHAnnot.InconsistentBranches(hi))
        | Hole(_, _) => raise(EvaluatorPost.Exception(MalformedHole))
        | _ => raise(EvaluatorPost.Exception(PostprocessedNonHoleInClosure))
        }

      /* Hole expressions must appear within a closure in
         the postprocessed result */
      | Hole(_, EmptyHole)
      | Hole(_, MultiHole(_))
      | Hole(_, NonEmptyHole(_))
      | Hole(_, ExpandingKeyword(_))
      | Hole(_, FreeVar(_))
      | Hole(_, Invalid(_))
      | Hole(_, InvalidText(_))
      | Hole(_, InconsistentBranches(_)) =>
        raise(EvaluatorPost.Exception(PostprocessedHoleOutsideClosure))

      | Var(x) => text(x)
      | Tag(name) => DHDoc_common.mk_TagLit(name)
      | Bool(b) => DHDoc_common.mk_BoolLit(b)
      | Int(n) => DHDoc_common.mk_IntLit(n)
      | Float(f) => DHDoc_common.mk_FloatLit(f)
      | String(s) => DHDoc_common.mk_StringLit(s)
      | Test(_) => Doc.text(ExpandingKeyword.to_string(Test))
      | Seq(d1, d2) =>
        let (doc1, doc2) = (go'(d1), go'(d2));
        DHDoc_common.mk_Sequence(mk_cast(doc1), mk_cast(doc2));
      | ListLit(d_list, Some((_, _, StandardErrStatus(_), _))) =>
        let ol = d_list |> List.map(go') |> List.map(mk_cast);
        DHDoc_common.mk_ListLit(ol);
      | ListLit(d_list, Some((u, i, InconsistentBranches(_, _), _))) =>
        let ol = d_list |> List.map(go') |> List.map(mk_cast);
        DHDoc_common.mk_ListLit(ol)
        |> annot(DHAnnot.InconsistentBranches((u, i)));
      | ListLit(_, None) => failwith("DHDoc_Exp.mk on ListLit(_, None")
      | Inj(_, inj_side, d) =>
        let child = (~enforce_inline) => mk_cast(go(~enforce_inline, d));
        DHDoc_common.mk_Inj(
          inj_side,
          child |> DHDoc_common.pad_child(~enforce_inline),
        );
      | Ap(d1, d2) =>
        let (doc1, doc2) =
          mk_left_associative_operands(DHDoc_common.precedence_Ap, d1, d2);
        DHDoc_common.mk_Ap(mk_cast(doc1), mk_cast(doc2));
      | ApBuiltin(ident, args) =>
        switch (args) {
        | [hd, ...tl] =>
          let d' =
            List.fold_left(
              (d1: DHExp.t, d2: DHExp.t) =>
                {ids: d1.ids @ d2.ids, term: DHExp.Ap(d1, d2)},
              hd,
              tl,
            );
          let (doc1, doc2) =
            mk_left_associative_operands(
              DHDoc_common.precedence_Ap,
              {ids: d'.ids, term: Var(ident)},
              d',
            );
          DHDoc_common.mk_Ap(mk_cast(doc1), mk_cast(doc2));
        | [] => text(ident)
        }
      | UnOp(Int(op), d) =>
        let doc = go'(~parenthesize=true, d);
        hseps([mk_un_int_op(op), mk_cast(doc)]);
      | BinOp(Int(op), d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_int_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_int_op(op), mk_cast(doc2)]);
      | BinOp(Float(op), d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_float_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_float_op(op), mk_cast(doc2)]);
      | BinOp(String(op), d1, d2) =>
        // TODO assumes all bin string ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_string_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_string_op(op), mk_cast(doc2)]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(DHDoc_common.precedence_Cons, d1, d2);
        DHDoc_common.mk_Cons(mk_cast(doc1), mk_cast(doc2));
      | BinOp(Bool(op), d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(precedence_bin_bool_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_bool_op(op), mk_cast(doc2)]);
      | Triv => DHDoc_common.Delim.triv
      | Tuple([]) => DHDoc_common.Delim.triv
      | Tuple(ds) =>
        DHDoc_common.mk_Tuple(ds |> List.map(d => mk_cast(go'(d))))
      | Prj(d, n) => DHDoc_common.mk_Prj(mk_cast(go'(d)), n)
      | If(c, t, e) => go_if(c, t, e)
      | Match(dscrut, drs, _) => go_case(dscrut, drs)
      | Cast(d, _, _) =>
        let (doc, _) = go'(d);
        doc;
      | Let(dp, ddef, dbody) =>
        let def_doc = (~enforce_inline) =>
          mk_cast(go(~enforce_inline, ddef));
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
          mk_cast(go(~enforce_inline=false, dbody)),
        ]);
      | Hole(None, FailedCast({term: Cast(d, ty1, ty2), _}, ty2', ty3))
          when Typ.eq(ty2, ty2') =>
        let (d_doc, _) = go'(d);
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
      | Hole(None, FailedCast(_d, _ty1, _ty2)) =>
        failwith("unexpected FailedCast without inner cast")
      | Hole(Some(_), FailedCast(_)) =>
        raise(EvaluatorPost.Exception(MalformedHole))
      | Hole(None, InvalidOperation(err, d)) =>
        let (d_doc, _) = go'(d);
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);
      | Hole(Some(_), InvalidOperation(_)) =>
        raise(EvaluatorPost.Exception(MalformedHole))
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

      | Fun(_, None, _, _) => failwith("DHDoc_Exp.mk on UExp")
      | Fun(dp, Some(ty), dbody, s) =>
        if (settings.show_fn_bodies) {
          let body_doc = (~enforce_inline) =>
            mk_cast(go(~enforce_inline, dbody));
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
            go(~enforce_inline, dbody) |> mk_cast;
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
    let doc =
      parenthesize
        ? hcats([
            DHDoc_common.Delim.open_Parenthesized,
            fdoc |> DHDoc_common.pad_child(~enforce_inline),
            DHDoc_common.Delim.close_Parenthesized,
          ])
        : fdoc(~enforce_inline);
    (doc, cast);
  };
  mk_cast(go(~parenthesize, ~enforce_inline, d));
}
and mk_clause =
    (~settings, ~selected_hole_instance, dclause: DHExp.t): DHDoc.t => {
  open Doc;
  let mk' = mk(~settings, ~selected_hole_instance);
  let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
  settings.show_case_clauses
    ? choices([
        hcats([space(), mk'(~enforce_inline=true, dclause)]),
        hcats([
          linebreak(),
          indent_and_align(mk'(~enforce_inline=false, dclause)),
        ]),
      ])
    : hcat(space(), hidden_clause);
}
and mk_rule =
    (~settings, ~selected_hole_instance, (dp, dclause): DHExp.rule): DHDoc.t => {
  open Doc;
  let clause_doc = mk_clause(~settings, ~selected_hole_instance, dclause);
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
