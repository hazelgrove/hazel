type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let rec extract_cursor_term = (ztyp: ZTyp.t): cursor_term => {
  switch (ztyp) {
  | ZOpSeq(_, zseq) =>
    switch (zseq) {
    | ZOperand(ztyp_operand, _) => extract_from_ztyp_operand(ztyp_operand)
    | ZOperator(ztyp_operator, _) =>
      let (cursor_pos, uop) = ztyp_operator;
      TypOperator(cursor_pos, uop);
    }
  };
}
and extract_from_ztyp_operand = (ztyp_operand: ZTyp.zoperand): cursor_term =>
  switch (ztyp_operand) {
  | CursorT(cursor_pos, utyp_operand) => TypOperand(cursor_pos, utyp_operand)
  | ParenthesizedZ(ztyp)
  | ListZ(ztyp) => extract_cursor_term(ztyp)
  | FiniteSumZ(zsum_body) => extract_from_zsum_body(zsum_body)
  | ElidedSumZ(zoperand) => extract_from_zsum_body_operand(zoperand)
  }

and extract_from_zsum_body = (ZOpSeq(_, zseq): ZTyp.zsum_body): cursor_term =>
  switch (zseq) {
  | ZOperand(zoperand, _) => extract_from_zsum_body_operand(zoperand)
  | ZOperator((cursor, operator), _) => SumBodyOperator(cursor, operator)
  }

and extract_from_zsum_body_operand =
    (zoperand: ZTyp.zsum_body_operand): cursor_term =>
  switch (zoperand) {
  | CursorArgTag(cursor, _, _) =>
    SumBodyOperand(cursor, zoperand |> ZTyp.erase_zsum_body_operand)
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
  | CursorT(_, _)
  | FiniteSumZ(_)
  | ElidedSumZ(_) => Some(ZTyp(zoperand))
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
    | ExpOperand(_, _)
    | PatOperand(_, _)
    | TypOperand(_, _)
    | ExpOperator(_, _)
    | PatOperator(_, _)
    | TypOperator(_, _)
    | Line(_, _)
    | Rule(_, _) => OnType
    | Tag(_, tag) => CursorInfo_Tag.cursor_info_typed(tag)
    | SumBodyOperand(_, _) => OnSumBodyOperand
    | SumBodyOperator(_, _) => OnSumBodyOperator
    };
  Some(CursorInfo_common.mk(typed, ctx, cursor_term));
};
