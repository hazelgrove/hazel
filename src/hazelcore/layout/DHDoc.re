open Pretty;

[@deriving sexp]
type t = Doc.t(DHAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let precedence_const = 0;
let precedence_Ap = 1;
let precedence_Times = 2;
let precedence_Divide = 2;
let precedence_Plus = 3;
let precedence_Minus = 3;
let precedence_Cons = 4;
let precedence_Equals = 5;
let precedence_LessThan = 5;
let precedence_GreaterThan = 5;
let precedence_And = 6;
let precedence_Or = 7;
let precedence_Comma = 8;
let precedence_max = 9;

let pad_child =
    (
      ~inline_padding as (l, r)=(Doc.empty(), Doc.empty()),
      ~enforce_inline: bool,
      child: formattable_child,
    )
    : t => {
  let inline_choice = Doc.hcats([l, child(~enforce_inline=true), r]);
  let para_choice =
    Doc.(
      hcats([
        linebreak(),
        indent_and_align(child(~enforce_inline=false)),
        linebreak(),
      ])
    );
  enforce_inline ? inline_choice : Doc.choice(inline_choice, para_choice);
};

module Delim = {
  let mk = (delim_text: string): t =>
    Doc.text(delim_text) |> Doc.annot(DHAnnot.Delim);

  let empty_hole = ((u, i): HoleInstance.t): t => {
    let lbl =
      StringUtil.cat([string_of_int(u + 1), ":", string_of_int(i + 1)]);
    Doc.text(lbl)
    |> Doc.annot(DHAnnot.HoleLabel)
    |> Doc.annot(DHAnnot.Delim);
  };

  let list_nil = mk("[]");
  let triv = mk("()");
  let wild = mk("_");

  let open_Parenthesized = mk("(");
  let close_Parenthesized = mk(")");

  let sym_Lam = mk(UnicodeConstants.lamSym);
  let colon_Lam = mk(":");
  let open_Lam = mk(".{");
  let close_Lam = mk("}");

  let fix_FixF = mk("fix");
  let colon_FixF = mk(":");
  let open_FixF = mk(".{");
  let close_FixF = mk("}");

  let open_Inj = (inj_side: InjSide.t) =>
    mk(StringUtil.cat([InjSide.to_string(inj_side), "("]));
  let close_Inj = mk(")");

  let open_Case = mk("case");
  let close_Case = mk("end");

  let bar_Rule = mk("|");
  let arrow_Rule = mk("=>");

  let open_Cast = mk("<");
  let arrow_Cast = mk(UnicodeConstants.castArrowSym);
  let close_Cast = mk(">");

  let open_FailedCast = open_Cast |> Doc.annot(DHAnnot.FailedCastDelim);
  let arrow_FailedCast =
    mk(UnicodeConstants.castArrowSym) |> Doc.annot(DHAnnot.FailedCastDelim);
  let close_FailedCast = close_Cast |> Doc.annot(DHAnnot.FailedCastDelim);
};

let mk_EmptyHole = (~selected=false, (u, i)) =>
  Delim.empty_hole((u, i))
  |> Doc.annot(DHAnnot.EmptyHole(selected, (u, i)));

let mk_Keyword = (u, i, k) =>
  Doc.text(ExpandingKeyword.to_string(k))
  |> Doc.annot(DHAnnot.VarHole(Keyword(k), (u, i)));

let mk_IntLit = n => Doc.text(string_of_int(n));

let mk_FloatLit = (f: float) =>
  switch (f < 0., Float.is_infinite(f), Float.is_nan(f)) {
  | (false, true, _) => Doc.text("Inf")
  /* TODO: NegInf is temporarily introduced until unary minus is introduced to Hazel */
  | (true, true, _) => Doc.text("NegInf")
  | (_, _, true) => Doc.text("NaN")
  | _ => Doc.text(string_of_float(f))
  };

let mk_BoolLit = b => Doc.text(string_of_bool(b));

let mk_Inj = (inj_side, padded_child) =>
  Doc.hcats([Delim.open_Inj(inj_side), padded_child, Delim.close_Inj]);

let mk_Cons = (hd, tl) => Doc.(hcats([hd, text("::"), tl]));

let mk_Pair = (doc1, doc2) => Doc.(hcats([doc1, text(", "), doc2]));

let mk_Ap = (doc1, doc2) => Doc.hseps([doc1, doc2]);

module Typ = {
  let promote_annot =
    fun
    | HTypAnnot.HoleLabel => DHAnnot.HoleLabel
    | HTypAnnot.Delim => DHAnnot.Delim;
  let promote = (d: HTypDoc.t): t => d |> Doc.map_annot(promote_annot);
  let mk = (~enforce_inline: bool, ty: HTyp.t): t =>
    ty |> HTypDoc.mk(~enforce_inline) |> promote;
};

module Pat = {
  let precedence = (dp: DHPat.t) =>
    switch (dp) {
    | EmptyHole(_)
    | NonEmptyHole(_)
    | Wild
    | Keyword(_)
    | Var(_)
    | IntLit(_)
    | FloatLit(_)
    | BoolLit(_)
    | Inj(_)
    | Triv
    | ListNil
    | Pair(_) => precedence_const
    | Cons(_) => precedence_Cons
    | Ap(_) => precedence_Ap
    };

  let rec mk = (~parenthesize=false, ~enforce_inline: bool, dp: DHPat.t): t => {
    let mk' = mk(~enforce_inline);
    let mk_left_associative_operands = (precedence_op, dp1, dp2) => (
      mk'(~parenthesize=precedence(dp1) > precedence_op, dp1),
      mk'(~parenthesize=precedence(dp2) >= precedence_op, dp2),
    );
    let mk_right_associative_operands = (precedence_op, dp1, dp2) => (
      mk'(~parenthesize=precedence(dp1) >= precedence_op, dp1),
      mk'(~parenthesize=precedence(dp2) > precedence_op, dp2),
    );
    let doc =
      switch (dp) {
      | EmptyHole(u, i) => mk_EmptyHole((u, i))
      | NonEmptyHole(reason, u, i, dp) =>
        mk'(dp) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | Keyword(u, i, k) => mk_Keyword(u, i, k)
      | Var(x) => Doc.text(x)
      | Wild => Delim.wild
      | Triv => Delim.triv
      | IntLit(n) => mk_IntLit(n)
      | FloatLit(f) => mk_FloatLit(f)
      | BoolLit(b) => mk_BoolLit(b)
      | Inj(inj_side, dp) =>
        mk_Inj(inj_side, mk(dp) |> pad_child(~enforce_inline))
      | ListNil => Delim.list_nil
      | Cons(dp1, dp2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(precedence_Cons, dp1, dp2);
        mk_Cons(doc1, doc2);
      | Pair(dp1, dp2) => mk_Pair(mk'(dp1), mk'(dp2))
      | Ap(dp1, dp2) =>
        let (doc1, doc2) =
          mk_left_associative_operands(precedence_Ap, dp1, dp2);
        mk_Ap(doc1, doc2);
      };
    parenthesize
      ? Doc.hcats([Delim.open_Parenthesized, doc, Delim.close_Parenthesized])
      : doc;
  };
};

module Exp = {
  let precedence_bin_bool_op = (op: DHExp.BinBoolOp.t) =>
    switch (op) {
    | And => precedence_And
    | Or => precedence_Or
    };

  let precedence_bin_int_op = (bio: DHExp.BinIntOp.t) =>
    switch (bio) {
    | Times => precedence_Times
    | Divide => precedence_Divide
    | Plus => precedence_Plus
    | Minus => precedence_Minus
    | Equals => precedence_Equals
    | LessThan => precedence_LessThan
    | GreaterThan => precedence_GreaterThan
    };
  let precedence_bin_float_op = (bfo: DHExp.BinFloatOp.t) =>
    switch (bfo) {
    | FTimes => precedence_Times
    | FDivide => precedence_Divide
    | FPlus => precedence_Plus
    | FMinus => precedence_Minus
    | FEquals => precedence_Equals
    | FLessThan => precedence_LessThan
    | FGreaterThan => precedence_GreaterThan
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
    | Lam(_) => precedence_const
    | Cast(d1, _, _) => show_casts ? precedence_const : precedence'(d1)
    | Let(_)
    | FixF(_)
    | ConsistentCase(_)
    | InconsistentBranches(_) => precedence_max /* TODO: is this right */
    | BinBoolOp(op, _, _) => precedence_bin_bool_op(op)
    | BinIntOp(op, _, _) => precedence_bin_int_op(op)
    | BinFloatOp(op, _, _) => precedence_bin_float_op(op)
    | Ap(_) => precedence_Ap
    | Cons(_) => precedence_Cons
    | Pair(_) => precedence_Comma
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
            annot(DHAnnot.CastDecoration, Typ.mk(~enforce_inline=true, ty)),
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
            mk_left_associative_operands(precedence_Ap, d1, d2);
          mk_Ap(mk_cast(doc1), mk_cast(doc2));
        | BinIntOp(op, d1, d2) =>
          // TODO assumes all bin int ops are left associative
          let (doc1, doc2) =
            mk_left_associative_operands(precedence_bin_int_op(op), d1, d2);
          hseps([mk_cast(doc1), mk_bin_int_op(op), mk_cast(doc2)]);
        | BinFloatOp(op, d1, d2) =>
          // TODO assumes all bin float ops are left associative
          let (doc1, doc2) =
            mk_left_associative_operands(
              precedence_bin_float_op(op),
              d1,
              d2,
            );
          hseps([mk_cast(doc1), mk_bin_float_op(op), mk_cast(doc2)]);
        | Cons(d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(precedence_Cons, d1, d2);
          mk_Cons(mk_cast(doc1), mk_cast(doc2));
        | BinBoolOp(op, d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(
              precedence_bin_bool_op(op),
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
          }
        /*
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
