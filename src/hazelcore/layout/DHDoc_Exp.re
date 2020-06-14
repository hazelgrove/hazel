open Pretty;

type t = Doc.t(DHAnnot.t);

module Exp = {
  let precedence_bin_bool_op = (op: DHExp.BinBoolOp.t) =>
    switch (op) {
    | And => DHDoc_Util.precedence_And
    | Or => DHDoc_Util.precedence_Or
    };

  let precedence_bin_int_op = (bio: DHExp.BinIntOp.t) =>
    switch (bio) {
    | Times => DHDoc_Util.precedence_Times
    | Divide => DHDoc_Util.precedence_Divide
    | Plus => DHDoc_Util.precedence_Plus
    | Minus => DHDoc_Util.precedence_Minus
    | Equals => DHDoc_Util.precedence_Equals
    | LessThan => DHDoc_Util.precedence_LessThan
    | GreaterThan => DHDoc_Util.precedence_GreaterThan
    };
  let precedence_bin_float_op = (bfo: DHExp.BinFloatOp.t) =>
    switch (bfo) {
    | FTimes => DHDoc_Util.precedence_Times
    | FDivide => DHDoc_Util.precedence_Divide
    | FPlus => DHDoc_Util.precedence_Plus
    | FMinus => DHDoc_Util.precedence_Minus
    | FEquals => DHDoc_Util.precedence_Equals
    | FLessThan => DHDoc_Util.precedence_LessThan
    | FGreaterThan => DHDoc_Util.precedence_GreaterThan
    };
  let rec precedence = (~show_casts: bool, d: DHExp.t) => {
    let precedence' = precedence(~show_casts);
    switch (d) {
    | BoundVar(_)
    | FreeVar(_)
    | Keyword(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | ListNil(_)
    | Inj(_)
    | EmptyHole(_)
    | Triv
    | FailedCast(_)
    | InvalidOperation(_)
    | Lam(_) => DHDoc_Util.precedence_const
    | Cast(d1, _, _) =>
      show_casts ? DHDoc_Util.precedence_const : precedence'(d1)
    | Let(_)
    | FixF(_)
    | ConsistentCase(_)
    | InconsistentBranches(_) =>
      DHDoc_Util.precedence_max /* TODO: is this right */
    | BinBoolOp(op, _, _) => precedence_bin_bool_op(op)
    | BinIntOp(op, _, _) => precedence_bin_int_op(op)
    | BinFloatOp(op, _, _) => precedence_bin_float_op(op)
    | Ap(_) => DHDoc_Util.precedence_Ap
    | Cons(_) => DHDoc_Util.precedence_Cons
    | Pair(_) => DHDoc_Util.precedence_Comma
    | NonEmptyHole(_, _, _, _, d) => precedence'(d)
    };
  };

  let mk_bin_bool_op = (op: DHExp.BinBoolOp.t): t =>
    Doc.text(
      switch (op) {
      | And => "&&"
      | Or => "||"
      },
    );

  let mk_bin_int_op = (op: DHExp.BinIntOp.t): t =>
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

  let mk_bin_float_op = (op: DHExp.BinFloatOp.t): t =>
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
            ~show_casts: bool,
            ~show_fn_bodies: bool,
            ~show_case_clauses: bool,
            ~parenthesize=false,
            ~enforce_inline: bool,
            ~selected_instance: option(HoleInstance.t),
            d: DHExp.t,
          )
          : t => {
    let precedence = precedence(~show_casts);
    let mk_cast = ((doc: t, ty: option(HTyp.t))): t =>
      switch (ty) {
      | Some(ty) when show_casts =>
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
            : (t, option(HTyp.t)) => {
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
                indent_and_align(
                  mk_cast(go(~enforce_inline=false, dscrut)),
                ),
              ]),
            ]);
          vseps(
            List.concat([
              [hcat(Delim.open_Case, scrut_doc)],
              drs
              |> List.map(
                   mk_rule(
                     ~show_fn_bodies,
                     ~show_case_clauses,
                     ~show_casts,
                     ~selected_instance,
                   ),
                 ),
              [Delim.close_Case],
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
          mk_EmptyHole(~selected, (u, i));
        | NonEmptyHole(reason, u, i, _sigma, d) =>
          go'(d) |> mk_cast |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))

        | Keyword(u, i, _sigma, k) => mk_Keyword(u, i, k)
        | FreeVar(u, i, _sigma, x) =>
          text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
        | BoundVar(x) => text(x)
        | Triv => Delim.triv
        | BoolLit(b) => mk_BoolLit(b)
        | IntLit(n) => mk_IntLit(n)
        | FloatLit(f) => mk_FloatLit(f)
        | ListNil(_) => Delim.list_nil
        | Inj(_, inj_side, d) =>
          let child = (~enforce_inline) => mk_cast(go(~enforce_inline, d));
          mk_Inj(inj_side, child |> pad_child(~enforce_inline));
        | Ap(d1, d2) =>
          let (doc1, doc2) =
            mk_left_associative_operands(DHDoc_Util.precedence_Ap, d1, d2);
          mk_Ap(mk_cast(doc1), mk_cast(doc2));
        | BinIntOp(op, d1, d2) =>
          // TODO assumes all bin int ops are left associative
          let (doc1, doc2) =
            mk_left_associative_operands(
              DHDoc_Util.precedence_bin_int_op(op),
              d1,
              d2,
            );
          hseps([mk_cast(doc1), mk_bin_int_op(op), mk_cast(doc2)]);
        | BinFloatOp(op, d1, d2) =>
          // TODO assumes all bin float ops are left associative
          let (doc1, doc2) =
            mk_left_associative_operands(
              DHDoc_Util.precedence_bin_float_op(op),
              d1,
              d2,
            );
          hseps([mk_cast(doc1), mk_bin_float_op(op), mk_cast(doc2)]);
        | Cons(d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(DHDoc_Util.precedence_Cons, d1, d2);
          mk_Cons(mk_cast(doc1), mk_cast(doc2));
        | BinBoolOp(op, d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(
              DHDoc_Util.precedence_bin_bool_op(op),
              d1,
              d2,
            );
          hseps([mk_cast(doc1), mk_bin_bool_op(op), mk_cast(doc2)]);
        | Pair(d1, d2) => mk_Pair(mk_cast(go'(d1)), mk_cast(go'(d2)))
        | InconsistentBranches(u, i, _sigma, Case(dscrut, drs, _)) =>
          go_case(dscrut, drs)
          |> annot(DHAnnot.InconsistentBranches((u, i)))
        | ConsistentCase(Case(dscrut, drs, _)) => go_case(dscrut, drs)
        | Cast(d, _, _) =>
          let (doc, _) = go'(d);
          doc;
        | Let(dp, ddef, dbody) =>
          let def_doc = (~enforce_inline) =>
            mk_cast(go(~enforce_inline, ddef));
          vseps([
            hcats([
              Delim.mk("let"),
              Pat.mk(dp)
              |> pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
              Delim.mk("="),
              def_doc
              |> pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline=false,
                 ),
              Delim.mk("in"),
            ]),
            mk_cast(go(~enforce_inline=false, dbody)),
          ]);
        | FailedCast(Cast(d, ty1, ty2), ty2', ty3) when HTyp.eq(ty2, ty2') =>
          let (d_doc, _) = go'(d);
          let cast_decoration =
            hcats([
              Delim.open_FailedCast,
              hseps([
                Typ.mk(~enforce_inline=true, ty1),
                Delim.arrow_FailedCast,
                Typ.mk(~enforce_inline=true, ty3),
              ]),
              Delim.close_FailedCast,
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
               Delim.open_FailedCast,
               hseps([
                 Typ.mk(~enforce_inline=true, ty1),
                 Delim.arrow_FailedCast,
                 Typ.mk(~enforce_inline=true, ty2),
               ]),
               Delim.close_FailedCast,
             ])
             |> annot(DHAnnot.FailedCastDecoration);
           switch (d_cast) {
           | Some(ty1') when HTyp.eq(ty1, ty1') =>
             hcats([d_doc, cast_decoration])
           | _ => hcats([mk_cast(dcast_doc), cast_decoration])
           };
           */

        | Lam(dp, ty, dbody) =>
          if (show_fn_bodies) {
            let body_doc = (~enforce_inline) =>
              mk_cast(go(~enforce_inline, dbody));
            hcats([
              Delim.sym_Lam,
              Pat.mk(~enforce_inline=true, dp),
              Delim.colon_Lam,
              Typ.mk(~enforce_inline=true, ty),
              Delim.open_Lam,
              body_doc |> pad_child(~enforce_inline),
              Delim.close_Lam,
            ]);
          } else {
            annot(DHAnnot.Collapsed, text("<fn>"));
          }
        | FixF(x, ty, dbody) =>
          if (show_fn_bodies) {
            let doc_body = (~enforce_inline) =>
              go(~enforce_inline, dbody) |> mk_cast;
            hcats([
              Delim.fix_FixF,
              space(),
              text(x),
              Delim.colon_FixF,
              Typ.mk(~enforce_inline=true, ty),
              Delim.open_FixF,
              doc_body |> pad_child(~enforce_inline),
              Delim.close_FixF,
            ]);
          } else {
            annot(DHAnnot.Collapsed, text("<fn>"));
          }
        };
      let doc =
        parenthesize
          ? hcats([
              Delim.open_Parenthesized,
              fdoc |> pad_child(~enforce_inline),
              Delim.close_Parenthesized,
            ])
          : fdoc(~enforce_inline);
      (doc, cast);
    };
    mk_cast(go(~parenthesize, ~enforce_inline, d));
  }
  and mk_rule =
      (
        ~show_casts,
        ~show_fn_bodies,
        ~show_case_clauses,
        ~selected_instance,
        Rule(dp, dclause): DHExp.rule,
      )
      : t => {
    open Doc;
    let mk' =
      mk(
        ~show_casts,
        ~show_fn_bodies,
        ~show_case_clauses,
        ~selected_instance,
      );
    let hidden_clause =
      annot(DHAnnot.Collapsed, text(UnicodeConstants.ellipsis));
    let clause_doc =
      show_case_clauses
        ? choices([
            hcats([space(), mk'(~enforce_inline=true, dclause)]),
            hcats([
              linebreak(),
              indent_and_align(mk'(~enforce_inline=false, dclause)),
            ]),
          ])
        : hcat(space(), hidden_clause);
    hcats([
      Delim.bar_Rule,
      Pat.mk(dp)
      |> pad_child(~inline_padding=(space(), space()), ~enforce_inline=false),
      Delim.arrow_Rule,
      clause_doc,
    ]);
  };
};
