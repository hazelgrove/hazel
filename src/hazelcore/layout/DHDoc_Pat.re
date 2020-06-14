open Pretty;

type t = Doc.t(DHAnnot.t);

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
    | Pair(_) => DHDoc_Util.precedence_const
    | Cons(_) => DHDoc_Util.precedence_Cons
    | Ap(_) => DHDoc_Util.precedence_Ap
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
          mk_right_associative_operands(DHDoc_Util.precedence_Cons, dp1, dp2);
        mk_Cons(doc1, doc2);
      | Pair(dp1, dp2) => mk_Pair(mk'(dp1), mk'(dp2))
      | Ap(dp1, dp2) =>
        let (doc1, doc2) =
          mk_left_associative_operands(DHDoc_Util.precedence_Ap, dp1, dp2);
        mk_Ap(doc1, doc2);
      };
    parenthesize
      ? Doc.hcats([Delim.open_Parenthesized, doc, Delim.close_Parenthesized])
      : doc;
  };
};
