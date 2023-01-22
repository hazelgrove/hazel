open Pretty;
open Haz3lcore;

let precedence = (dp: DHPat.t) =>
  switch (dp.term) {
  | Parens(_)
  | Hole(_)
  | Wild
  | Var(_)
  | Triv
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Inj(_)
  | ListLit(_)
  | TypeAnn(_)
  | Tag(_) => DHDoc_common.precedence_const
  | Tuple(_) => DHDoc_common.precedence_Comma
  | Cons(_) => DHDoc_common.precedence_Cons
  | Ap(_) => DHDoc_common.precedence_Ap
  };

let rec mk =
        (~parenthesize=false, ~enforce_inline: bool, dp: DHPat.t): DHDoc.t => {
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
    switch (dp.term) {
    | Parens(dp) =>
      Doc.hcats([
        DHDoc_common.Delim.open_Parenthesized,
        mk(~parenthesize, ~enforce_inline, dp),
        DHDoc_common.Delim.close_Parenthesized,
      ])
    | TypeAnn(_)
    | Hole(_, Invalid(_))
    | Hole(None, _)
    | Hole(Some(_), MultiHole(_)) => failwith("mk on UPat")
    | Hole(Some((u, i)), EmptyHole) => DHDoc_common.mk_EmptyHole((u, i))
    | Hole(Some((u, i)), NonEmptyHole(reason, dp)) =>
      mk'(dp) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
    | Hole(Some((u, i)), ExpandingKeyword(k)) =>
      DHDoc_common.mk_ExpandingKeyword((u, i), k)
    | Hole(Some((u, i)), InvalidText(t)) =>
      DHDoc_common.mk_InvalidText(t, (u, i))
    | Var(x) => Doc.text(x)
    | Wild => DHDoc_common.Delim.wild
    | Tag(name) => DHDoc_common.mk_TagLit(name)
    | Int(n) => DHDoc_common.mk_IntLit(n)
    | Float(f) => DHDoc_common.mk_FloatLit(f)
    | Bool(b) => DHDoc_common.mk_BoolLit(b)
    | String(s) => DHDoc_common.mk_StringLit(s)
    | Inj(inj_side, dp) =>
      DHDoc_common.mk_Inj(
        inj_side,
        mk(dp) |> DHDoc_common.pad_child(~enforce_inline),
      )
    | ListLit(d_list, _) =>
      let ol = List.map(mk', d_list);
      DHDoc_common.mk_ListLit(ol);
    | Cons(dp1, dp2) =>
      let (doc1, doc2) =
        mk_right_associative_operands(DHDoc_common.precedence_Cons, dp1, dp2);
      DHDoc_common.mk_Cons(doc1, doc2);
    | Triv
    | Tuple([]) => DHDoc_common.Delim.triv
    | Tuple(ds) => DHDoc_common.mk_Tuple(List.map(mk', ds))
    | Ap(dp1, dp2) =>
      let (doc1, doc2) =
        mk_left_associative_operands(DHDoc_common.precedence_Ap, dp1, dp2);
      DHDoc_common.mk_Ap(doc1, doc2);
    };
  parenthesize
    ? Doc.hcats([
        DHDoc_common.Delim.open_Parenthesized,
        doc,
        DHDoc_common.Delim.close_Parenthesized,
      ])
    : doc;
};
