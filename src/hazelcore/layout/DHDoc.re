[@deriving sexp]
type t = Doc.t(DHAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let pad_child =
    (
      ~inline_padding as (l, r)=(Doc.empty, Doc.empty),
      ~enforce_inline: bool,
      child: formattable_child,
    )
    : t => {
  let inline_choice = Doc.hcats([l, child(~enforce_inline=true), r]);
  let para_choice =
    Doc.hcats([
      Linebreak,
      Doc.indent_and_align(child(~enforce_inline=false)),
      Linebreak,
    ]);
  enforce_inline ? inline_choice : Doc.Choice(inline_choice, para_choice);
};

module Delim = {
  let mk = (delim_text: string): t =>
    Doc.Text(delim_text) |> Doc.annot(DHAnnot.Delim);

  let empty_hole = (u: MetaVar.t, i: MetaVarInst.t): t => {
    let lbl = StringUtil.cat([string_of_int(u), ":", string_of_int(i)]);
    Doc.Text(lbl)
    |> Doc.annot(DHAnnot.HoleLabel)
    |> Doc.annot(DHAnnot.Delim);
  };

  let list_nil = mk("[]");
  let triv = mk("()");
  let wild = mk("_");

  let open_Parenthesized = mk("(");
  let close_Parenthesized = mk(")");

  let open_Inj = (inj_side: InjSide.t) =>
    mk(StringUtil.cat(["inj[", InjSide.to_string(inj_side), "]("]));
  let close_Inj = mk(")");

  let open_Case = mk("case");
  let close_Case = mk("end");

  let bar_Rule = mk("|");
  let arrow_Rule = mk(UnicodeConstants.caseArrowSym);
};

let mk_EmptyHole = (u, i) =>
  Delim.empty_hole(u, i) |> Doc.annot(DHAnnot.EmptyHole((u, i)));

let mk_Keyword = (u, i, k) =>
  Doc.Text(ExpandingKeyword.to_string(k))
  |> Doc.annot(DHAnnot.VarHole(Keyword(k), (u, i)));

let mk_NumLit = n => Doc.Text(string_of_int(n));

let mk_BoolLit = b => Doc.Text(string_of_bool(b));

let mk_Inj = (inj_side, padded_child) =>
  Doc.hcats([Delim.open_Inj(inj_side), padded_child, Delim.close_Inj]);

let mk_Cons = (hd, tl) => Doc.hseps([hd, Text("::"), tl]);

let mk_Pair = (doc1, doc2) => Doc.hcats([doc1, Text(", "), doc2]);

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
  let rec mk = (~enforce_inline: bool, dp: DHPat.t): t => {
    let mk' = mk(~enforce_inline);
    switch (dp) {
    | EmptyHole(u, i) => mk_EmptyHole(u, i)
    | NonEmptyHole(reason, u, i, dp) =>
      mk'(dp) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
    | Keyword(u, i, k) => mk_Keyword(u, i, k)
    | Var(x) => Doc.Text(x)
    | Wild => Delim.wild
    | Triv => Delim.triv
    | NumLit(n) => mk_NumLit(n)
    | BoolLit(b) => mk_BoolLit(b)
    | Inj(inj_side, d) =>
      mk_Inj(inj_side, mk(d) |> pad_child(~enforce_inline))
    | ListNil => Delim.list_nil
    | Cons(d1, d2) => mk_Cons(mk'(d1), mk'(d2))
    | Pair(d1, d2) => mk_Pair(mk'(d1), mk'(d2))
    | Ap(d1, d2) => mk_Ap(mk'(d1), mk'(d2))
    };
  };
};

module Exp = {
  let mk_bin_num_op = (op: DHExp.bin_num_op): t =>
    Doc.Text(
      switch (op) {
      | Minus => "-"
      | Plus => "+"
      | Times => "*"
      | LessThan => "<"
      | GreaterThan => ">"
      | Equals => "=="
      },
    );

  let rec mk = (~enforce_inline: bool, d: DHExp.t): t => {
    let mk' = mk(~enforce_inline);
    switch (d) {
    | EmptyHole(u, i, _sigma) => mk_EmptyHole(u, i)
    | NonEmptyHole(reason, u, i, _sigma, d) =>
      mk'(d) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
    | Keyword(u, i, _sigma, k) => mk_Keyword(u, i, k)
    | FreeVar(u, i, _sigma, x) =>
      Doc.Text(x) |> Doc.annot(DHAnnot.VarHole(Free, (u, i)))
    | BoundVar(x) => Doc.Text(x)
    | Let(dp, ddef, dbody) =>
      Doc.(
        vseps([
          hcats([
            Delim.mk("let"),
            Pat.mk(dp)
            |> pad_child(
                 ~inline_padding=(Doc.space, Doc.space),
                 ~enforce_inline,
               ),
            Delim.mk("="),
            mk(ddef)
            |> pad_child(
                 ~inline_padding=(Doc.space, Doc.space),
                 ~enforce_inline,
               ),
            Delim.mk("in"),
          ]),
          mk(~enforce_inline=false, dbody),
        ])
      )
    | Ap(d1, d2) => mk_Ap(mk'(d1), mk'(d2))
    | BoolLit(b) => mk_BoolLit(b)
    | NumLit(n) => mk_NumLit(n)
    | BinNumOp(op, d1, d2) =>
      Doc.hseps([mk'(d1), mk_bin_num_op(op), mk'(d2)])
    | And(d1, d2) => Doc.hseps([mk'(d1), Text("&&"), mk'(d2)])
    | Or(d1, d2) => Doc.hseps([mk'(d1), Text("||"), mk'(d2)])
    | ListNil(_) => Delim.list_nil
    | Cons(d1, d2) => mk_Cons(mk'(d1), mk'(d2))
    | Inj(_, inj_side, d) =>
      mk_Inj(inj_side, mk(d) |> pad_child(~enforce_inline))
    | Pair(d1, d2) => mk_Pair(mk'(d1), mk'(d2))
    | Triv => Delim.triv
    | Case(dscrut, drs, _) =>
      Doc.(
        vseps(
          List.concat([
            [hseps([Delim.open_Case, mk(~enforce_inline=true, dscrut)])],
            drs |> List.map(mk_rule),
            [Delim.close_Case],
          ]),
        )
      )
    | Cast(d1, _ty1, _ty2) =>
      Doc.hseps([
        mk'(d1),
        /*Typ.mk(~enforce_inline, ty1),
          Typ.mk(~enforce_inline, ty2),*/
      ])
    | FixF(_)
    | Lam(_)
    | FailedCast(_) => failwith("unimplemented")
    };
  }
  and mk_rule = (Rule(dp, dclause): DHExp.rule): t =>
    Doc.(
      hcats([
        Delim.bar_Rule,
        Pat.mk(dp)
        |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
        Delim.arrow_Rule,
        choices([
          hcats([space, mk(~enforce_inline=true, dclause)]),
          hcats([Linebreak, mk(~enforce_inline=false, dclause)]),
        ]),
      ])
    );
};
