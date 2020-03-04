open Pretty;

let _SHOW_FN_BODIES = false;

type t = Doc.t(DHAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let precedence_const = 0;
let precedence_Ap = 1;
let precedence_Times = 2;
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
        Doc.indent_and_align(child(~enforce_inline=false)),
        linebreak(),
      ])
    );
  enforce_inline ? inline_choice : Doc.choice(inline_choice, para_choice);
};

module Delim = {
  let mk = (delim_text: string): t =>
    Doc.text(delim_text) |> Doc.annot(DHAnnot.Delim);

  let empty_hole = (u: MetaVar.t, i: MetaVarInst.t): t => {
    let lbl = StringUtil.cat([string_of_int(u), ":", string_of_int(i)]);
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
  let arrow_Rule = mk(UnicodeConstants.caseArrowSym);

  let open_Cast = mk("<");
  let close_Cast = mk(">");

  let open_FailedCast = open_Cast |> Doc.annot(DHAnnot.FailedCastDelim);
  let close_FailedCast = close_Cast |> Doc.annot(DHAnnot.FailedCastDelim);

  let cast_arrow = mk(UnicodeConstants.castArrowSym);

  let failed_cast_arrow =
    mk(UnicodeConstants.castArrowSym) |> Doc.annot(DHAnnot.FailedCastDelim);
};

let mk_EmptyHole = (u, i) =>
  Delim.empty_hole(u, i) |> Doc.annot(DHAnnot.EmptyHole((u, i)));

let mk_Keyword = (u, i, k) =>
  Doc.text(ExpandingKeyword.to_string(k))
  |> Doc.annot(DHAnnot.VarHole(Keyword(k), (u, i)));

let mk_NumLit = n => Doc.text(string_of_int(n));

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
    | NumLit(_)
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
      | EmptyHole(u, i) => mk_EmptyHole(u, i)
      | NonEmptyHole(reason, u, i, dp) =>
        mk'(dp) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | Keyword(u, i, k) => mk_Keyword(u, i, k)
      | Var(x) => Doc.text(x)
      | Wild => Delim.wild
      | Triv => Delim.triv
      | NumLit(n) => mk_NumLit(n)
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
  let precedence_bin_num_op = (bno: DHExp.bin_num_op) =>
    switch (bno) {
    | Times => precedence_Times
    | Plus => precedence_Plus
    | Minus => precedence_Minus
    | Equals => precedence_Equals
    | LessThan => precedence_LessThan
    | GreaterThan => precedence_GreaterThan
    };
  let rec precedence = (~show_casts: bool, d: DHExp.t) => {
    let precedence' = precedence(~show_casts);
    switch (d) {
    | BoundVar(_)
    | FreeVar(_)
    | Keyword(_)
    | BoolLit(_)
    | NumLit(_)
    | ListNil(_)
    | Inj(_)
    | EmptyHole(_)
    | Triv
    | FailedCast(_) => precedence_const
    | Cast(d1, _, _) => show_casts ? precedence_const : precedence'(d1)
    | Let(_)
    | FixF(_)
    | Lam(_)
    | Case(_) => precedence_max
    | BinNumOp(op, _, _) => precedence_bin_num_op(op)
    | Ap(_) => precedence_Ap
    | Cons(_) => precedence_Cons
    | And(_) => precedence_And
    | Or(_) => precedence_Or
    | Pair(_) => precedence_Comma
    | NonEmptyHole(_, _, _, _, d) => precedence'(d)
    };
  };

  let mk_bin_num_op = (op: DHExp.bin_num_op): t =>
    Doc.text(
      switch (op) {
      | Minus => "-"
      | Plus => "+"
      | Times => "*"
      | LessThan => "<"
      | GreaterThan => ">"
      | Equals => "=="
      },
    );

  let rec mk =
          (
            ~show_casts: bool,
            ~parenthesize=false,
            ~enforce_inline: bool,
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
      let mk_left_associative_operands = (precedence_op, d1, d2) => (
        go'(~parenthesize=precedence(d1) > precedence_op, d1),
        go'(~parenthesize=precedence(d2) >= precedence_op, d2),
      );
      let mk_right_associative_operands = (precedence_op, d1, d2) => (
        go'(~parenthesize=precedence(d1) >= precedence_op, d1),
        go'(~parenthesize=precedence(d2) > precedence_op, d2),
      );
      let no_cast = doc => (doc, None);
      let (doc, cast) =
        switch (d) {
        | EmptyHole(u, i, _sigma) => mk_EmptyHole(u, i) |> no_cast
        | NonEmptyHole(reason, u, i, _sigma, d) =>
          go'(d)
          |> mk_cast
          |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))
          |> no_cast
        | Keyword(u, i, _sigma, k) => mk_Keyword(u, i, k) |> no_cast
        | FreeVar(u, i, _sigma, x) =>
          text(x) |> annot(DHAnnot.VarHole(Free, (u, i))) |> no_cast
        | BoundVar(x) => text(x) |> no_cast
        | Triv => Delim.triv |> no_cast
        | BoolLit(b) => mk_BoolLit(b) |> no_cast
        | NumLit(n) => mk_NumLit(n) |> no_cast
        | ListNil(_) => Delim.list_nil |> no_cast
        | Inj(_, inj_side, d) =>
          let child = (~enforce_inline) => go(~enforce_inline, d) |> mk_cast;
          mk_Inj(inj_side, child |> pad_child(~enforce_inline)) |> no_cast;
        | Ap(d1, d2) =>
          let (doc1, doc2) =
            mk_left_associative_operands(precedence_Ap, d1, d2);
          mk_Ap(mk_cast(doc1), mk_cast(doc2)) |> no_cast;
        | BinNumOp(op, d1, d2) =>
          // TODO assumes all bin num ops are left associative
          let (doc1, doc2) =
            mk_left_associative_operands(precedence_bin_num_op(op), d1, d2);
          hseps([mk_cast(doc1), mk_bin_num_op(op), mk_cast(doc2)])
          |> no_cast;
        | Cons(d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(precedence_Cons, d1, d2);
          mk_Cons(mk_cast(doc1), mk_cast(doc2)) |> no_cast;
        | And(d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(precedence_And, d1, d2);
          hseps([mk_cast(doc1), text("&&"), mk_cast(doc2)]) |> no_cast;
        | Or(d1, d2) =>
          let (doc1, doc2) =
            mk_right_associative_operands(precedence_Or, d1, d2);
          hseps([mk_cast(doc1), text("||"), mk_cast(doc2)]) |> no_cast;
        | Pair(d1, d2) =>
          mk_Pair(mk_cast(go'(d1)), mk_cast(go'(d2))) |> no_cast
        | Case(dscrut, drs, _) =>
          if (enforce_inline) {
            fail() |> no_cast;
          } else {
            vseps(
              List.concat([
                [
                  hseps([
                    Delim.open_Case,
                    mk_cast(go(~enforce_inline=true, dscrut)),
                  ]),
                ],
                drs |> List.map(mk_rule(~show_casts)),
                [Delim.close_Case],
              ]),
            )
            |> no_cast;
          }
        | Cast(d, _ty1, ty2) =>
          let (doc, _) = go'(d);
          (doc, Some(ty2));
        | Let(dp, ddef, dbody) =>
          let def_doc = (~enforce_inline) =>
            go(~enforce_inline, ddef) |> mk_cast;
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
                   ~enforce_inline,
                 ),
              Delim.mk("in"),
            ]),
            mk_cast(go(~enforce_inline=false, dbody)),
          ])
          |> no_cast;
        | FailedCast(d, ty1, ty2) =>
          let (d_doc, d_cast) as dcast_doc = go'(d);
          let cast_decoration =
            hcats([
              Delim.open_FailedCast,
              hseps([
                Typ.mk(~enforce_inline=true, ty1),
                Delim.failed_cast_arrow,
                Typ.mk(~enforce_inline=true, ty2),
              ]),
              Delim.close_FailedCast,
            ])
            |> annot(DHAnnot.FailedCastDecoration);
          switch (d_cast) {
          | Some(ty1') when HTyp.eq(ty1, ty1') =>
            hcats([d_doc, cast_decoration]) |> no_cast
          | _ => hcats([mk_cast(dcast_doc), cast_decoration]) |> no_cast
          };
        | Lam(dp, ty, dbody) =>
          let doc_body = (~enforce_inline) =>
            go(~enforce_inline, dbody) |> mk_cast;
          hcats([
            Delim.sym_Lam,
            Pat.mk(~enforce_inline=true, dp),
            Delim.colon_Lam,
            Typ.mk(~enforce_inline=true, ty),
            Delim.open_Lam,
            doc_body |> pad_child(~enforce_inline),
            Delim.close_Lam,
          ])
          |> no_cast;
        | FixF(x, ty, dbody) =>
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
          ])
          |> no_cast;
        };
      parenthesize
        ? (
          hcats([Delim.open_Parenthesized, doc, Delim.close_Parenthesized]),
          cast,
        )
        : (doc, cast);
    };
    mk_cast(go(~parenthesize, ~enforce_inline, d));
  }
  and mk_rule = (~show_casts, Rule(dp, dclause): DHExp.rule): t =>
    Doc.(
      hcats([
        Delim.bar_Rule,
        Pat.mk(dp)
        |> pad_child(
             ~inline_padding=(space(), space()),
             ~enforce_inline=false,
           ),
        Delim.arrow_Rule,
        choices([
          hcats([space(), mk(~show_casts, ~enforce_inline=true, dclause)]),
          hcats([
            linebreak(),
            mk(~show_casts, ~enforce_inline=false, dclause),
          ]),
        ]),
      ])
    );
};
