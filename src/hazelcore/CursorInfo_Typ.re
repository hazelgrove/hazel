type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let rec extract_cursor_term = (ZOpSeq(_, zseq): ZTyp.t): cursor_term =>
  switch (zseq) {
  | ZOperand(ztyp_operand, _) => extract_from_ztyp_operand(ztyp_operand)
  | ZOperator(ztyp_operator, _) =>
    let (cursor_pos, uop) = ztyp_operator;
    TypOp(cursor_pos, uop);
  }

and extract_from_ztyp_operand = (ztyp_operand: ZTyp.zoperand): cursor_term =>
  switch (ztyp_operand) {
  | CursorT(cursor_pos, utyp_operand) => Typ(cursor_pos, utyp_operand)
  | ParenthesizedZ(ztyp)
  | ListZ(ztyp) => extract_cursor_term(ztyp)
  | SumZ(zsumbody) => extract_from_zsumbody(zsumbody)
  }

and extract_from_zsumbody = (ZOpSeq(_, zseq): ZTyp.zsumbody): cursor_term =>
  switch (zseq) {
  | ZOperand(zoperand, _) => extract_from_zsumbody_operand(zoperand)
  | ZOperator((cursor, operator), _) => SumBodyOp(cursor, operator)
  }

and extract_from_zsumbody_operand =
    (zoperand: ZTyp.zsumbody_operand): cursor_term =>
  switch (zoperand) {
  | CursorArgTag(cursor, _, _) =>
    SumBody(cursor, zoperand |> ZTyp.erase_zsumbody_operand)
  | ConstTagZ(ztag) => CursorInfo_Tag.extract_cursor_term(ztag)
  | ArgTagZT(ztag, _) => CursorInfo_Tag.extract_cursor_term(ztag)
  | ArgTagZA(_, zty) => extract_cursor_term(zty)
  };

let rec get_zoperand_from_ztyp = (ztyp: ZTyp.t): option(zoperand) => {
  get_zoperand_from_ztyp_opseq(ztyp);
}
and get_zoperand_from_ztyp_opseq = (zopseq: ZTyp.zopseq): option(zoperand) => {
  switch (zopseq) {
  | ZOpSeq(_, zseq) =>
    switch (zseq) {
    | ZOperand(ztyp_operand, _) =>
      get_zoperand_from_ztyp_operand(ztyp_operand)
    | ZOperator(_, _) => None
    }
  };
}
and get_zoperand_from_ztyp_operand =
    (zoperand: ZTyp.zoperand): option(zoperand) => {
  switch (zoperand) {
  | CursorT(_, _) => Some(ZTyp(zoperand))
  | SumZ(zsumbody) => Some(ZTyp(SumZ(zsumbody)))
  | ParenthesizedZ(ztyp)
  | ListZ(ztyp) => get_zoperand_from_ztyp(ztyp)
  };
};

// TODO: make a bugfix issue to fix this after the type variables PR is merged
let cursor_info =
    (~steps as _, ctx: Contexts.t, typ: ZTyp.t): option(CursorInfo.t) => {
  let cursor_term = extract_cursor_term(typ);
  let typed: CursorInfo.typed =
    switch (cursor_term) {
    | Exp(_, _)
    | Pat(_, _)
    | Typ(_, _)
    | ExpOp(_, _)
    | PatOp(_, _)
    | TypOp(_, _)
    | Line(_, _)
    | Rule(_, _) => OnType
    | Tag(_, tag) => CursorInfo_Tag.cursor_info_typed(tag)
    | SumBody(_, _)
    | SumBodyOp(_, _) => OnSumBody
    };
  Some(CursorInfo_common.mk(typed, ctx, cursor_term));
};
