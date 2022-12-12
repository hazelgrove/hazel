module Doc = Pretty.Doc;

let precedence_bin_bool_op = (op: DHExp.BinBoolOp.t) =>
  switch (op) {
  | And => DHDoc_common.precedence_And
  | Or => DHDoc_common.precedence_Or
  };

let precedence_bin_int_op = (bio: DHExp.BinIntOp.t) =>
  switch (bio) {
  | Times => DHDoc_common.precedence_Times
  | Divide => DHDoc_common.precedence_Divide
  | Plus => DHDoc_common.precedence_Plus
  | Minus => DHDoc_common.precedence_Minus
  | Equals => DHDoc_common.precedence_Equals
  | LessThan => DHDoc_common.precedence_LessThan
  | GreaterThan => DHDoc_common.precedence_GreaterThan
  };
let precedence_bin_float_op = (bfo: DHExp.BinFloatOp.t) =>
  switch (bfo) {
  | FTimes => DHDoc_common.precedence_Times
  | FDivide => DHDoc_common.precedence_Divide
  | FPlus => DHDoc_common.precedence_Plus
  | FMinus => DHDoc_common.precedence_Minus
  | FEquals => DHDoc_common.precedence_Equals
  | FLessThan => DHDoc_common.precedence_LessThan
  | FGreaterThan => DHDoc_common.precedence_GreaterThan
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
  | FloatLit(_)
  | ListNil(_)
  | Inj(_)
  | EmptyHole(_)
  | Triv
  | FailedCast(_)
  | InvalidOperation(_)
  | Fun(_) => DHDoc_common.precedence_const
  | Cast(d1, _, _) =>
    show_casts ? DHDoc_common.precedence_const : precedence'(d1)
  | Let(_)
  | FixF(_)
  | ConsistentCase(_)
  | InconsistentBranches(_) => DHDoc_common.precedence_max
  | BinBoolOp(op, _, _) => precedence_bin_bool_op(op)
  | BinIntOp(op, _, _) => precedence_bin_int_op(op)
  | BinFloatOp(op, _, _) => precedence_bin_float_op(op)
  | Ap(_) => DHDoc_common.precedence_Ap
  | ApBuiltin(_) => DHDoc_common.precedence_Ap
  | Cons(_) => DHDoc_common.precedence_Cons
  | Pair(_) => DHDoc_common.precedence_Comma
  | NonEmptyHole(_, _, _, _, d) => precedence'(d)
  };
};

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
    | Divide => "/"
    | LessThan => "<"
    | GreaterThan => ">"
    | Equals => "=="
    },
  );

let mk_bin_float_op = (op: DHExp.BinFloatOp.t): DHDoc.t =>
  Doc.text(
    switch (op) {
    | FMinus => "-."
    | FPlus => "+."
    | FTimes => "*."
    | FDivide => "/."
    | FLessThan => "<."
    | FGreaterThan => ">."
    | FEquals => "==."
    },
  );

let rec mk =
        (
          ~settings: DHSettings.t,
          ~parenthesize=false,
          ~enforce_inline: bool,
          ~selected_instance: option(HoleInstance.t),
          d: DHExp.t,
        )
        : DHDoc.t => {
  let precedence = precedence(~show_casts=settings.show_casts);
  let mk_cast = ((doc: DHDoc.t, ty: option(HTyp.t))): DHDoc.t =>
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
          : (DHDoc.t, option(HTyp.t)) => {
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
            drs |> List.map(mk_rule(~settings, ~selected_instance)),
            [DHDoc_common.Delim.close_Case],
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
      | Cast(_, _, ty) => Some(ty)
      | _ => None
      };
    let fdoc = (~enforce_inline) =>
      switch (d) {
      | EmptyHole(u, i, _sigma) =>
        let selected =
          switch (selected_instance) {
          | None => false
          | Some((u', i')) => u == u' && i == i'
          };
        DHDoc_common.mk_EmptyHole(~selected, (u, i));
      | NonEmptyHole(reason, u, i, _sigma, d) =>
        go'(d) |> mk_cast |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))

      | ExpandingKeyword(u, i, _sigma, k) =>
        DHDoc_common.mk_ExpandingKeyword(u, i, k)
      | FreeVar(u, i, _sigma, x) =>
        text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
      | InvalidText(u, i, _sigma, t) =>
        DHDoc_common.mk_InvalidText(t, (u, i))
      | BoundVar(x) => text(x)
      | Triv => DHDoc_common.Delim.triv
      | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
      | IntLit(n) => DHDoc_common.mk_IntLit(n)
      | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
      | ListNil(_) => DHDoc_common.Delim.list_nil
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
          let d' = List.fold_left((d1, d2) => DHExp.Ap(d1, d2), hd, tl);
          let (doc1, doc2) =
            mk_left_associative_operands(
              DHDoc_common.precedence_Ap,
              BoundVar(ident),
              d',
            );
          DHDoc_common.mk_Ap(mk_cast(doc1), mk_cast(doc2));
        | [] => text(ident)
        }
      | BinIntOp(op, d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_int_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_int_op(op), mk_cast(doc2)]);
      | BinFloatOp(op, d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_bin_float_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_float_op(op), mk_cast(doc2)]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(DHDoc_common.precedence_Cons, d1, d2);
        DHDoc_common.mk_Cons(mk_cast(doc1), mk_cast(doc2));
      | BinBoolOp(op, d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(precedence_bin_bool_op(op), d1, d2);
        hseps([mk_cast(doc1), mk_bin_bool_op(op), mk_cast(doc2)]);
      | Pair(d1, d2) =>
        DHDoc_common.mk_Pair(mk_cast(go'(d1)), mk_cast(go'(d2)))
      | InconsistentBranches(u, i, _sigma, Case(dscrut, drs, _)) =>
        go_case(dscrut, drs) |> annot(DHAnnot.InconsistentBranches((u, i)))
      | ConsistentCase(Case(dscrut, drs, _)) => go_case(dscrut, drs)
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
      | FailedCast(Cast(d, ty1, ty2), ty2', ty3) when HTyp.eq(ty2, ty2') =>
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
      | FailedCast(_d, _ty1, _ty2) =>
        failwith("unexpected FailedCast without inner cast")
      | InvalidOperation(d, err) =>
        switch (err) {
        | DivideByZero =>
          let (d_doc, _) = go'(d);
          let decoration =
            Doc.text(InvalidOperationError.err_msg(err))
            |> annot(DHAnnot.DivideByZero);
          hcats([d_doc, decoration]);
        } /*
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
       | Some(ty1') when HTyp.eq(ty1, ty1') =>
         hcats([d_doc, cast_decoration])
       | _ => hcats([mk_cast(dcast_doc), cast_decoration])
       };
       */

      | Fun(dp, ty, dbody) =>
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
          annot(DHAnnot.Collapsed, text("<fn>"));
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
and mk_rule =
    (~settings, ~selected_instance, Rule(dp, dclause): DHExp.rule): DHDoc.t => {
  open Doc;
  let mk' = mk(~settings, ~selected_instance);
  let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
  let clause_doc =
    settings.show_case_clauses
      ? choices([
          hcats([space(), mk'(~enforce_inline=true, dclause)]),
          hcats([
            linebreak(),
            indent_and_align(mk'(~enforce_inline=false, dclause)),
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
