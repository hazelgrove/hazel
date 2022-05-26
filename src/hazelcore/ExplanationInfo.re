open Sexplib.Std;
//open Sexplib;

[@deriving sexp]
type pattern_info =
  | Operand(UHPat.operand, option(UHTyp.t))
  | CommaOperator(list(UHPat.t), option(UHTyp.t))
  | BinOperator(UHPat.operator, UHPat.t, UHPat.t, option(UHTyp.t));

[@deriving sexp]
type type_info =
  | Operand(UHTyp.operand)
  | CommaOperator(list(UHTyp.t))
  | BinOperator(UHTyp.operator, UHTyp.t, UHTyp.t);

[@deriving sexp]
type explanation_info =
  | EmptyLine
  | CommentLine
  | Block(UHExp.block, int, UHExp.line)
  | LetLine(pattern_info, UHExp.t, int, UHExp.t)
  | ExpBaseOperand(UHExp.operand)
  | Lambda(pattern_info, UHExp.t)
  | Rule(int, UHExp.t, pattern_info, UHExp.t)
  | ExpCommaOperator(list(UHExp.opseq))
  | ExpBinOperator(UHExp.operator, UHExp.opseq, UHExp.opseq)
  | Pattern(pattern_info)
  | Typ(type_info);

let rec mk_explanation_info =
        (cursor_term: CursorInfo.cursor_term): explanation_info => {
  /*print_endline(
      "Cursor Term: "
      ++ Sexp.to_string(CursorInfo.sexp_of_cursor_term(cursor_term)),
    );*/
  switch (cursor_term) {
  | ExpOperand(_, operand) => extract_exp_operand_info(operand)
  | PatOperand(_, operand) => Pattern(extract_pat_operand_info(operand))
  | TypOperand(_, operand) => Typ(extract_typ_operand_info(operand))
  | ExpOperator(_, operator, op_index, parent_opseq) =>
    extract_exp_opseq_info(parent_opseq, Some((op_index, operator)))
  | PatOperator(_, operator, op_index, parent_opseq) =>
    Pattern(
      extract_pat_opseq_info(parent_opseq, Some((op_index, operator))),
    )
  | TypOperator(_, operator, op_index, parent_opseq) =>
    Typ(extract_typ_opseq_info(parent_opseq, Some((op_index, operator))))
  | Line(_, line, opt_body) => extract_exp_line_info(line, opt_body)
  | Rule(_, Rule(pat, clause), index, scrut) =>
    Rule(index, scrut, extract_pat_opseq_info(pat, None), clause)
  };
}
and extract_exp_line_info =
    (line: UHExp.line, opt_body: option((UHExp.block, int)))
    : explanation_info => {
  switch (line) {
  | EmptyLine => EmptyLine
  | CommentLine(_) => CommentLine
  | LetLine(pat, def) =>
    switch (opt_body) {
    | Some((body, start_index)) =>
      let pattern_info = extract_pat_opseq_info(pat, None);
      LetLine(pattern_info, def, start_index, body);
    | None => failwith("Missing body info for let line")
    }
  | ExpLine(opseq) =>
    /*TODO: Hannah this one hasn't really been checked (does this case ever actually happen with how the cursor info is now?) */
    /*print_endline("This case has been executed");*/
    extract_exp_opseq_info(opseq, None)
  };
}
and extract_exp_operand_info = (exp: UHExp.operand): explanation_info => {
  switch (exp) {
  | Lam(_, pat, body) => Lambda(extract_pat_opseq_info(pat, None), body)
  | Parenthesized(exp) =>
    switch (exp) {
    | [LetLine(pat, def), ...body] =>
      extract_exp_line_info(LetLine(pat, def), Some((body, 1)))
    | [line] => extract_exp_line_info(line, None)
    | [_line, ...rest] =>
      let last_index = List.length(rest);
      Block(exp, last_index, List.nth(rest, last_index - 1));
    | [] => failwith("No empty blocks")
    }
  | Case(_)
  | EmptyHole(_)
  | InvalidText(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Inj(_) => ExpBaseOperand(exp)
  | ApPalette(_) => failwith("ApPalette not implemented")
  };
}
and extract_pat_operand_info = (pat: UHPat.operand): pattern_info => {
  switch (pat) {
  | EmptyHole(_)
  | Wild(_)
  | Var(_)
  | InvalidText(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Inj(_) => Operand(pat, None)
  | TypeAnn(_, operand, typ) =>
    let pat_info = extract_pat_operand_info(operand);
    switch (pat_info) {
    | Operand(pat, None) => Operand(pat, Some(typ))
    | CommaOperator(pats, None) => CommaOperator(pats, Some(typ))
    | BinOperator(op, left, right, None) =>
      BinOperator(op, left, right, Some(typ))
    | _ => pat_info
    };
  | Parenthesized(pat) => extract_pat_opseq_info(pat, None)
  };
}
and extract_typ_operand_info = (typ: UHTyp.operand): type_info => {
  switch (typ) {
  | Parenthesized(typ) => extract_typ_opseq_info(typ, None)
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | List(_) => Operand(typ)
  };
}
and extract_pat_opseq_info =
    (
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      operator_info: option((int, UHPat.operator)),
    )
    : pattern_info => {
  switch (operator_info) {
  | Some((operator_index, operator)) =>
    switch (operator) {
    | Comma =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHPat.get_tuple_indices,
          UHPat.mk_OpSeq,
          opseq,
        );
      CommaOperator(subparts, None);
    | Space
    | Cons =>
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(operator_index, UHPat.mk_OpSeq, seq);
      BinOperator(operator, subpart1, subpart2, None);
    }
  | None =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      extract_pat_operand_info(pn);
    | BinOp(_, _, Comma, _, _) =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHPat.get_tuple_indices,
          UHPat.mk_OpSeq,
          opseq,
        );
      CommaOperator(subparts, None);
    | BinOp(index, _, operator, _skel1, _skel2) =>
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(index, UHPat.mk_OpSeq, seq);
      BinOperator(operator, subpart1, subpart2, None);
    }
  };
}
and extract_exp_opseq_info =
    (
      OpSeq(skel, seq) as opseq: UHExp.opseq,
      operator_info: option((int, UHExp.operator)),
    )
    : explanation_info => {
  switch (operator_info) {
  | Some((operator_index, operator)) =>
    switch (operator) {
    | Comma =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHExp.get_tuple_indices,
          UHExp.mk_OpSeq,
          opseq,
        );
      ExpCommaOperator(subparts);
    | _ =>
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(operator_index, UHExp.mk_OpSeq, seq);
      ExpBinOperator(operator, subpart1, subpart2);
    }
  | None =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      extract_exp_operand_info(pn);
    | BinOp(_, _, Comma, _, _) =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHExp.get_tuple_indices,
          UHExp.mk_OpSeq,
          opseq,
        );
      ExpCommaOperator(subparts);
    | BinOp(index, _, operator, _skel1, _skel2) =>
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(index, UHExp.mk_OpSeq, seq);
      ExpBinOperator(operator, subpart1, subpart2);
    }
  };
}
and extract_typ_opseq_info =
    (
      OpSeq(skel, seq) as opseq: UHTyp.opseq,
      operator_info: option((int, UHTyp.operator)),
    )
    : type_info => {
  switch (operator_info) {
  | Some((operator_index, operator)) =>
    switch (operator) {
    | Prod =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHTyp.get_prod_indices,
          UHTyp.mk_OpSeq,
          opseq,
        );
      CommaOperator(subparts);
    | _ =>
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(operator_index, UHTyp.mk_OpSeq, seq);
      BinOperator(operator, subpart1, subpart2);
    }
  | None =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      extract_typ_operand_info(pn);
    | BinOp(_, _, Prod, _, _) =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHTyp.get_prod_indices,
          UHTyp.mk_OpSeq,
          opseq,
        );
      CommaOperator(subparts);
    | BinOp(index, _, operator, _skel1, _skel2) =>
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(index, UHTyp.mk_OpSeq, seq);
      BinOperator(operator, subpart1, subpart2);
    }
  };
};
