open Pretty;
open Haz3lcore;

let precedence = (dp: DHPat.t) =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Wild
  | ExpandingKeyword(_)
  | InvalidText(_)
  | BadConstructor(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | StringLit(_)
  | ListLit(_)
  | Constructor(_) => DHDoc_common.precedence_const
  | Tuple(_) => DHDoc_common.precedence_Comma
  | Cons(_) => DHDoc_common.precedence_Cons
  | Ap(_) => DHDoc_common.precedence_Ap
  };

let rec mk =
        (~parenthesize=false, ~enforce_inline: bool, dp: DHPat.t): DHDoc.t => {
  let mk' = mk(~enforce_inline);
  let mk_left_associative_operands = (precedence_op, dp1, dp2) => (
    mk'(~parenthesize=precedence(dp1) < precedence_op, dp1),
    mk'(~parenthesize=precedence(dp2) <= precedence_op, dp2),
  );
  let mk_right_associative_operands = (precedence_op, dp1, dp2) => (
    mk'(~parenthesize=precedence(dp1) <= precedence_op, dp1),
    mk'(~parenthesize=precedence(dp2) < precedence_op, dp2),
  );
  let doc =
    switch (dp) {
    | EmptyHole(u, i) => DHDoc_common.mk_EmptyHole((u, i))
    | NonEmptyHole(reason, u, i, dp) =>
      mk'(dp) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
    | ExpandingKeyword(u, i, k) =>
      DHDoc_common.mk_ExpandingKeyword((u, i), k)
    | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
    | BadConstructor(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
    | Var(x) => Doc.text(x)
    | Wild => DHDoc_common.Delim.wild
    | Constructor(name) => DHDoc_common.mk_ConstructorLit(name)
    | IntLit(n) => DHDoc_common.mk_IntLit(n)
    | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
    | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
    | StringLit(s) => DHDoc_common.mk_StringLit(s)
    | ListLit(_, d_list) =>
      let ol = List.map(mk', d_list);
      DHDoc_common.mk_ListLit(ol);
    | Cons(dp1, dp2) =>
      let (doc1, doc2) =
        mk_right_associative_operands(DHDoc_common.precedence_Cons, dp1, dp2);
      DHDoc_common.mk_Cons(doc1, doc2);
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
