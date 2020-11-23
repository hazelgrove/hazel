let inline_padding_of_operator: UHPat.operator => (UHDoc.t, UHDoc.t) =
  fun
  | Comma => (UHDoc_common.empty_, UHDoc_common.space_)
  | Space
  | Cons => (UHDoc_common.empty_, UHDoc_common.empty_);

let mk_EmptyHole: string => UHDoc.t = UHDoc_common.mk_EmptyHole(~sort=Pat);
let mk_InvalidText: string => UHDoc.t =
  UHDoc_common.mk_InvalidText(~sort=Pat);
let mk_IntLit: string => UHDoc.t = UHDoc_common.mk_IntLit(~sort=Pat);
let mk_FloatLit: string => UHDoc.t = UHDoc_common.mk_FloatLit(~sort=Pat);
let mk_BoolLit: bool => UHDoc.t = UHDoc_common.mk_BoolLit(~sort=Pat);
let mk_ListNil: unit => UHDoc.t = UHDoc_common.mk_ListNil(~sort=Pat);
let mk_Var: string => UHDoc.t = UHDoc_common.mk_Var(~sort=Pat);
let mk_Parenthesized: UHDoc_common.formatted_child => UHDoc.t =
  UHDoc_common.mk_Parenthesized(~sort=Pat);
let mk_Inj: (~inj_side: InjSide.t, UHDoc_common.formatted_child) => UHDoc.t =
  UHDoc_common.mk_Inj(~sort=Pat);
let mk_NTuple:
  (
    ~mk_operand: (~enforce_inline: bool, 'a) => UHDoc.t,
    ~mk_operator: UHPat.operator => UHDoc.t,
    ~enforce_inline: bool,
    OpSeq.t('a, UHPat.operator)
  ) =>
  UHDoc.t =
  UHDoc_common.mk_NTuple(
    ~sort=Pat,
    ~get_tuple_elements=UHPat.get_tuple_elements,
    ~inline_padding_of_operator,
  );

let rec mk =
  lazy(
    UHDoc_common.memoize((~memoize: bool, ~enforce_inline: bool, p: UHPat.t) =>
      (Lazy.force(mk_opseq, ~memoize, ~enforce_inline, p): UHDoc.t)
    )
  )
and mk_opseq =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, opseq: UHPat.opseq) =>
      (
        mk_NTuple(
          ~mk_operand=Lazy.force(mk_operand, ~memoize),
          ~mk_operator,
          ~enforce_inline,
          opseq,
        ): UHDoc.t
      )
    )
  )
and mk_operator = (op: UHPat.operator): UHDoc.t =>
  op |> Operators_Pat.is_Space
    ? UHDoc_common.mk_space_op
    : UHDoc_common.mk_op(Operators_Pat.to_string(op))
and mk_operand =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, operand: UHPat.operand) =>
      (
        switch (operand) {
        | EmptyHole(u) => mk_EmptyHole(UHDoc_common.hole_lbl(u + 1))
        | Wild(_) => UHDoc_common.mk_Wild()
        | InvalidText(_, t) => mk_InvalidText(t)
        | Var(_, _, x) => mk_Var(x)
        | IntLit(_, n) => mk_IntLit(n)
        | FloatLit(_, f) => mk_FloatLit(f)
        | BoolLit(_, b) => mk_BoolLit(b)
        | ListNil(_) => mk_ListNil()
        | Parenthesized(body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Parenthesized(body);
        | Inj(_, inj_side, body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Inj(~inj_side, body);
        }: UHDoc.t
      )
    )
  )
and mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, p: UHPat.t)
    : UHDoc_common.formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk, ~memoize, ~enforce_inline, p)
    |> UHDoc_common.annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline=true))
    : Unformatted(formattable);
};
