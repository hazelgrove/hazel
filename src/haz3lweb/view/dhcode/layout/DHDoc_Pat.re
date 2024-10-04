open Pretty;
open Haz3lcore;

let precedence = (dp: Pat.t) =>
  switch (DHPat.term_of(dp)) {
  | EmptyHolePat
  | MultiHolePat(_)
  | Wild
  | InvalidPat(_)
  | VarPat(_)
  | IntPat(_)
  | FloatPat(_)
  | BoolPat(_)
  | StringPat(_)
  | ListLitPat(_)
  | ConstructorPat(_) => DHDoc_common.precedence_const
  | TuplePat(_) => DHDoc_common.precedence_Comma
  | ConsPat(_) => DHDoc_common.precedence_Cons
  | ApPat(_) => DHDoc_common.precedence_Ap
  | ParensPat(_) => DHDoc_common.precedence_const
  | CastPat(_) => DHDoc_common.precedence_Ap
  };

let rec mk =
        (
          ~infomap: Statics.Map.t,
          ~parenthesize=false,
          ~show_casts,
          ~enforce_inline: bool,
          dp: Pat.t,
        )
        : DHDoc.t => {
  let mk' = mk(~enforce_inline, ~infomap, ~show_casts);
  let mk_left_associative_operands = (precedence_op, dp1, dp2) => (
    mk'(~parenthesize=precedence(dp1) > precedence_op, dp1),
    mk'(~parenthesize=precedence(dp2) >= precedence_op, dp2),
  );
  let mk_right_associative_operands = (precedence_op, dp1, dp2) => (
    mk'(~parenthesize=precedence(dp1) >= precedence_op, dp1),
    mk'(~parenthesize=precedence(dp2) > precedence_op, dp2),
  );
  let doc =
    switch (DHPat.term_of(dp)) {
    | MultiHolePat(_)
    | EmptyHolePat => DHDoc_common.mk_EmptyHole(ClosureEnvironment.empty)
    | InvalidPat(t) => DHDoc_common.mk_InvalidText(t)
    | VarPat(x) => Doc.text(x)
    | Wild => DHDoc_common.Delim.wild
    | ConstructorPat(name, _) => DHDoc_common.mk_ConstructorLit(name)
    | IntPat(n) => DHDoc_common.mk_IntLit(n)
    | FloatPat(f) => DHDoc_common.mk_FloatLit(f)
    | BoolPat(b) => DHDoc_common.mk_BoolLit(b)
    | StringPat(s) => DHDoc_common.mk_StringLit(s)
    | ListLitPat(d_list) =>
      let ol = List.map(mk', d_list);
      DHDoc_common.mk_ListLit(ol);
    | ConsPat(dp1, dp2) =>
      let (doc1, doc2) =
        mk_right_associative_operands(DHDoc_common.precedence_Cons, dp1, dp2);
      DHDoc_common.mk_Cons(doc1, doc2);
    | TuplePat([]) => DHDoc_common.Delim.triv
    | TuplePat(ds) => DHDoc_common.mk_Tuple(List.map(mk', ds))
    // TODO: Print type annotations
    | CastPat(dp, t1, t2) when show_casts =>
      Doc.hcats([
        mk'(dp),
        Doc.annot(
          DHAnnot.CastDecoration,
          Doc.hcats([
            DHDoc_common.Delim.open_Cast,
            DHDoc_Typ.mk(~enforce_inline=true, t1),
            DHDoc_common.Delim.back_arrow_Cast,
            DHDoc_Typ.mk(~enforce_inline=true, t2),
            DHDoc_common.Delim.close_Cast,
          ]),
        ),
      ])
    | CastPat(dp, _, _) => mk'(~parenthesize, dp)
    | ParensPat(dp) =>
      mk(~enforce_inline, ~parenthesize=true, ~infomap, ~show_casts, dp)
    | ApPat(dp1, dp2) =>
      let (doc1, doc2) =
        mk_left_associative_operands(DHDoc_common.precedence_Ap, dp1, dp2);
      DHDoc_common.mk_Ap(doc1, doc2);
    };
  let doc =
    switch (Statics.get_pat_error_at(infomap, DHPat.rep_id(dp))) {
    | Some(_) => Doc.annot(DHAnnot.NonEmptyHole, doc)
    | None => doc
    };
  parenthesize
    ? Doc.hcats([
        DHDoc_common.Delim.open_Parenthesized,
        doc,
        DHDoc_common.Delim.close_Parenthesized,
      ])
    : doc;
};
