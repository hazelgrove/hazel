open Pretty;

let precedence = (dp: DHPat.t) =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Wild
  | Keyword(_)
  | InvalidText(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | Triv
  | ListNil
  | Pair(_)
  | Inj(_)
  | InjError(_) => DHDoc_common.precedence_const
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
    switch (dp) {
    | EmptyHole(u, i) => DHDoc_common.mk_EmptyHole((u, i))
    | NonEmptyHole(reason, u, i, dp) =>
      mk'(dp) |> Doc.annot(DHAnnot.NonEmptyHole(reason, (u, i)))
    | Keyword(u, i, k) => DHDoc_common.mk_Keyword(u, i, k)
    | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
    | Var(x) => Doc.text(x)
    | Wild => DHDoc_common.Delim.wild
    | Triv => DHDoc_common.Delim.triv
    | IntLit(n) => DHDoc_common.mk_IntLit(n)
    | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
    | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
    | Inj((tag, body_opt)) =>
      let tag_doc = DHDoc_Tag.mk(tag);
      let padded_child_opt =
        switch (body_opt) {
        | Some(body) =>
          Some(DHDoc_common.pad_child(~enforce_inline, mk(body)))
        | None => None
        };
      DHDoc_common.mk_Inj(tag_doc, padded_child_opt);
    | InjError(reason, u, i, inj) =>
      mk'(Inj(inj)) |> Doc.annot(DHAnnot.InjHole(reason, (u, i)))
    | ListNil => DHDoc_common.Delim.list_nil
    | Cons(dp1, dp2) =>
      let (doc1, doc2) =
        mk_right_associative_operands(DHDoc_common.precedence_Cons, dp1, dp2);
      DHDoc_common.mk_Cons(doc1, doc2);
    | Pair(dp1, dp2) => DHDoc_common.mk_Pair(mk'(dp1), mk'(dp2))
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
