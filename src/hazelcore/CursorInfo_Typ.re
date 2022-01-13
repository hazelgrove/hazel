type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let extract_parent_opseq =
    (ZOpSeq(skel, zseq) as zopseq: ZTyp.t): UHTyp.opseq => {
  switch (zseq) {
  | ZOperator((_, Prod), _) => ZTyp.erase_zopseq(zopseq) // Cursor on tuple comma
  | ZOperator(_, _)
  | ZOperand(_) =>
    // Cursor within tuple element
    let cursor_skel =
      skel
      |> UHTyp.get_prod_elements
      |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
    let rec go_to_cursor = skel =>
      if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
        // found the cursor
        ZTyp.erase_zopseq(ZOpSeq(skel, zseq));
      } else {
        // recurse toward cursor
        switch (skel) {
        | Placeholder(_)
        | BinOp(_, _, Prod, _, _) =>
          failwith(
            "Typ.extract_parent_opseq: expected commas and placeholders to be handled at opseq level",
          )
        | BinOp(_, _, _, skel1, skel2) =>
          ZOpSeq.skel_contains_cursor(skel1, zseq)
            ? go_to_cursor(skel1) : go_to_cursor(skel2)
        };
      };
    go_to_cursor(cursor_skel);
  };
};
let rec extract_cursor_term =
        (ZOpSeq(_skel, zseq) as ztyp: ZTyp.t): cursor_term => {
  switch (zseq) {
  | ZOperand(ztyp_operand, _) => extract_from_ztyp_operand(ztyp_operand)
  | ZOperator(ztyp_operator, _) =>
    let (cursor_pos, uop) = ztyp_operator;
    let OpSeq(skel, _) as opseq = extract_parent_opseq(ztyp);
    TypOp(cursor_pos, uop, Skel.get_root_num(skel), opseq);
  };
}
and extract_from_ztyp_operand = (ztyp_operand: ZTyp.zoperand): cursor_term => {
  switch (ztyp_operand) {
  | CursorT(cursor_pos, utyp_operand) => Typ(cursor_pos, utyp_operand)
  | ParenthesizedZ(ztyp)
  | ListZ(ztyp) => extract_cursor_term(ztyp)
  };
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
  | ParenthesizedZ(ztyp)
  | ListZ(ztyp) => get_zoperand_from_ztyp(ztyp)
  };
};

let cursor_info =
    (~steps as _, ctx: Contexts.t, typ: ZTyp.t): option(CursorInfo.t) =>
  Some(CursorInfo_common.mk(OnType, ctx, extract_cursor_term(typ)));
