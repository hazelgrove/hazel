open Sexplib.Std;

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
  | LetLine(pattern_info, UHExp.t, UHExp.t)
  | ExpBaseOperand(UHExp.operand)
  | Lambda(pattern_info, UHExp.t)
  | Rule(int, UHExp.t, pattern_info, UHExp.t)
  | ExpCommaOperator(list(UHExp.opseq))
  | ExpBinOperator(UHExp.operator, UHExp.opseq, UHExp.opseq)
  | Pattern(pattern_info)
  | Typ(type_info) /* Things need to handle here:   - parenthesis (I think this might be one of the trickiest parts to handle)   - type annotations of patterns   - pulling out opseq children (including in case of pattern)   */;

let rec mk_explanation_info =
        (cursor_term: CursorInfo.cursor_term): explanation_info => {
  switch (cursor_term) {
  | Exp(_, operand) => extract_exp_operand_info(operand)
  | Pat(_, operand) => Pattern(extract_pat_operand_info(operand))
  | Typ(_, operand) => Typ(extract_typ_operand_info(operand))
  | ExpOp(_, operator, op_index, parent_opseq) =>
    extract_exp_opseq_info(parent_opseq, Some((op_index, operator)))
  | PatOp(_, operator, op_index, parent_opseq) =>
    Pattern(
      extract_pat_opseq_info(parent_opseq, Some((op_index, operator))),
    )
  | TypOp(_, operator, op_index, parent_opseq) =>
    Typ(extract_typ_opseq_info(parent_opseq, Some((op_index, operator))))
  | Line(_, line, opt_body) =>
    switch (line) {
    | EmptyLine => EmptyLine
    | CommentLine(_) => CommentLine
    | LetLine(pat, def) =>
      switch (opt_body) {
      | Some(body) =>
        let pattern_info = extract_pat_opseq_info(pat, None);
        LetLine(pattern_info, def, body);
      | None => failwith("Missing body info for let line")
      }
    | ExpLine(opseq) =>
      /*TODO: Hannah this one hasn't really been checked (does this case ever actually happen with how the cursor info is now?) */
      print_endline("This case has been executed");
      extract_exp_opseq_info(opseq, None);
    }
  | Rule(_, Rule(pat, clause), index, scrut) =>
    Rule(index, scrut, extract_pat_opseq_info(pat, None), clause)
  };
}
and extract_exp_operand_info = (exp: UHExp.operand): explanation_info => {
  switch (exp) {
  | Lam(_, pat, body) => Lambda(extract_pat_opseq_info(pat, None), body)
  | Parenthesized(_exp) =>
    /* TODO: Hannah - what should be happening here? maybe highlight block except for last line with an expression and then the last expresssion line separately to to explain that the other lines will be evaluated but the whole expression evaluates to the last expression line */ failwith(
      "Not yet implemented",
    )
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
    | BinOp(_, Comma, _, _) =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHPat.get_tuple_indices,
          UHPat.mk_OpSeq,
          opseq,
        );
      CommaOperator(subparts, None);
    | BinOp(_, operator, _skel1, _skel2) =>
      let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, Seq.length(seq));
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(
          AnnotatedSkel.get_root_num(annotated_skel),
          UHPat.mk_OpSeq,
          seq,
        );
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
    | BinOp(_, Comma, _, _) =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHExp.get_tuple_indices,
          UHExp.mk_OpSeq,
          opseq,
        );
      ExpCommaOperator(subparts);
    | BinOp(_, operator, _skel1, _skel2) =>
      let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, Seq.length(seq));
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(
          AnnotatedSkel.get_root_num(annotated_skel),
          UHExp.mk_OpSeq,
          seq,
        );
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
    | BinOp(_, Prod, _, _) =>
      let subparts =
        OpSeq.get_sub_parts_comma(
          UHTyp.get_prod_indices,
          UHTyp.mk_OpSeq,
          opseq,
        );
      CommaOperator(subparts);
    | BinOp(_, operator, _skel1, _skel2) =>
      let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, Seq.length(seq));
      let (subpart1, subpart2) =
        OpSeq.get_sub_parts_binop(
          AnnotatedSkel.get_root_num(annotated_skel),
          UHTyp.mk_OpSeq,
          seq,
        );
      BinOperator(operator, subpart1, subpart2);
    }
  };
};

let cons_all =
    (step: int, steps: list(CursorPath.steps)): list(CursorPath.steps) => {
  List.map(steps => [step, ...steps], steps);
};

let rec explanation_paths = (ze: ZExp.t): list(CursorPath.steps) =>
  explanation_pathsblock(ze)
and explanation_pathsblock = (zblock: ZExp.zblock): list(CursorPath.steps) => {
  let prefix_len = ZList.prefix_length(zblock);
  let zline = ZList.prj_z(zblock);
  cons_all(prefix_len, explanation_paths_zline(zline));
}
and explanation_paths_zline = (zline: ZExp.zline): list(CursorPath.steps) =>
  switch (zline) {
  | CursorL(_, line) => explanation_paths_line(line) /* I think this is where I would need to check for a let line and get the body which would just be all the rest of the lines in the block */
  | LetLineZP(zp, _) => cons_all(0, pattern_paths(zp))
  | LetLineZE(_, zdef) => cons_all(1, explanation_paths(zdef))
  | ExpLineZ(zopseq) => explanation_paths_zopseq(zopseq)
  }
and explanation_paths_line = (line: UHExp.line): list(CursorPath.steps) =>
  switch (line) {
  | EmptyLine
  | CommentLine(_) => [[]]
  | ExpLine(OpSeq(skel, seq)) =>
    let length = Seq.length(seq);
    let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, length);
    switch (annotated_skel) {
    | BinOp(Comma, _, _, _) =>
      List.map(
        skel => [AnnotatedSkel.get_root_num(skel)],
        UHExp.get_annotated_tuple_elements(annotated_skel),
      )
    | BinOp(_, _, skel1, skel2) => [
        [AnnotatedSkel.get_root_num(skel1)],
        [AnnotatedSkel.get_root_num(skel2)],
      ]
    | Placeholder(_) => cons_all(0, operand_paths(Seq.nth_operand(0, seq)))
    };
  | LetLine(pat, def) =>
    cons_all(0, child_pattern_paths(pat, false))
    @ cons_all(1, expblock_paths(def, child_expline_paths)) /* TODO Hannah get the body */
  }
and explanation_paths_zopseq =
    (ZOpSeq(skel, zseq): ZExp.zopseq): list(CursorPath.steps) => {
  // handle n-tuples
  switch (zseq) {
  | ZOperator((_, Comma), (prefix, suffix)) =>
    // cursor on tuple comma
    let length = Seq.length(prefix) + Seq.length(suffix);
    let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, length);
    List.map(
      skel => [AnnotatedSkel.get_root_num(skel)],
      UHExp.get_annotated_tuple_elements(annotated_skel),
    );
  | _ =>
    // cursor within tuple element
    let cursor_skel =
      skel
      |> UHExp.get_tuple_elements
      |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
    let (annotated_skel, _) =
      AnnotatedSkel.mk(cursor_skel, 0, ZSeq.length(zseq));
    CursorPath_common.explanation_paths_skel(
      explanation_pathsoperand,
      cursor_skel,
      annotated_skel,
      zseq,
    );
  };
}
and operand_paths = (operand: UHExp.operand): list(CursorPath.steps) => {
  switch (operand) {
  | EmptyHole(_)
  | InvalidText(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_) => [[]]
  | Lam(_, pat, body) =>
    cons_all(0, child_pattern_paths(pat, false))
    @ cons_all(1, expblock_paths(body, child_expline_paths))
  | Inj(_, _, exp) => cons_all(0, expblock_paths(exp, child_expline_paths))
  | Case(_, scrut, rules) =>
    cons_all(0, expblock_paths(scrut, child_expline_paths))
    @ List.flatten(
        List.mapi(
          (index, UHExp.Rule(pat, exp)) => {
            let pat_paths = [
              0,
              ...CursorPath_common.child_root_node_path(pat),
            ];
            let exp_paths =
              cons_all(1, expblock_paths(exp, child_expline_paths));
            cons_all(index + 1, [pat_paths, ...exp_paths]);
          },
          rules,
        ),
      )
  | Parenthesized(operand) =>
    cons_all(0, expblock_paths(operand, explanation_paths_line))
  | ApPalette(_) => []
  };
}
and expblock_paths =
    (block: UHExp.block, process_line: UHExp.line => list(CursorPath.steps))
    : list(CursorPath.steps) => {
  switch (block) {
  | [] => []
  | [line] => cons_all(0, process_line(line))
  | lines =>
    /* TODO: Hannah - figure out how to just highlight the block or otherwise get this to appear in the desired way (e.g. maybe only last line in block) */
    List.flatten(
      List.mapi(
        (index, line) => cons_all(index, process_line(line)),
        lines,
      ),
    )
  };
}
and child_expline_paths = (line: UHExp.line): list(CursorPath.steps) => {
  switch (line) {
  | EmptyLine
  | CommentLine(_)
  | LetLine(_) => [[]]
  | ExpLine(opseq) => [CursorPath_common.child_root_node_path(opseq)]
  };
}
and explanation_pathsoperand =
    (zoperand: ZExp.zoperand): list(CursorPath.steps) =>
  switch (zoperand) {
  | CursorE(_, operand) => operand_paths(operand)
  | ParenthesizedZ(zbody) => cons_all(0, explanation_paths(zbody))
  | LamZP(_, zp, _) => cons_all(0, pattern_paths(zp))
  | LamZE(_, _, zdef) => cons_all(1, explanation_paths(zdef))
  | InjZ(_, _, zbody) => cons_all(0, explanation_paths(zbody))
  | CaseZE(_, zscrut, _) => cons_all(0, explanation_paths(zscrut))
  | CaseZR(_, _, zrules) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons_all(prefix_len + 1, explanation_pathsrule(zrule));
  | ApPaletteZ(_, _, _, zpsi) =>
    let zhole_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZIntMap.prj_z_kv(zhole_map);
    cons_all(n, explanation_paths(ze));
  }
and explanation_pathsrule = (zrule: ZExp.zrule): list(CursorPath.steps) =>
  switch (zrule) {
  | CursorR(_, Rule(pat, clause)) =>
    cons_all(0, child_pattern_paths(pat, true))
    @ cons_all(1, expblock_paths(clause, child_expline_paths))
  | RuleZP(zp, _) => cons_all(0, pattern_paths(zp))
  | RuleZE(_, zclause) => cons_all(1, explanation_paths(zclause))
  }
and child_pattern_paths =
    (OpSeq(skel, seq): UHPat.opseq, show_inj_child: bool)
    : list(CursorPath.steps) => {
  let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, Seq.length(seq));
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    cons_all(n, pattern_operand(pn, show_inj_child));
  | BinOp(_, Comma, _, _) =>
    List.map(
      skel => [AnnotatedSkel.get_root_num(skel)],
      UHPat.get_annotated_tuple_elements(annotated_skel),
    )
  | BinOp(_, Space, _, _) => [[AnnotatedSkel.get_root_num(annotated_skel)]]
  | BinOp(_, Cons, _, _) =>
    switch (annotated_skel) {
    | Placeholder(_) => failwith("Can I reach here?")
    | BinOp(_, _, skel1, skel2) =>
      /*let root1 = AnnotatedSkel.get_root_num(skel1);
        print_endline("Root: " ++ string_of_int(root1));
        print_endline(
          "Root of annotated: "
          ++ string_of_int(
               AnnotatedSkel.get_root_num(annotated_skel) - Seq.length(seq),
             ),
        );
        switch (
          Seq.opt_split_nth_operand(1, seq),
          Seq.opt_split_nth_operator(0, seq),
        ) {
        | (None, None) => print_endline("Here 1")
        | (_, Some((_, (surround1, surround2)))) =>
          print_endline(
            "Surround 1: " ++ Sexp.to_string(UHPat.sexp_of_seq(surround1)),
          );
          print_endline(
            "Surround 2: " ++ Sexp.to_string(UHPat.sexp_of_seq(surround2)),
          );
        | (Some((_, (surround1, surround2))), _) =>
          print_endline(
            "Surround 1: " ++ Sexp.to_string(UHPat.sexp_of_affix(surround1)),
          );
          print_endline(
            "Surround 2: " ++ Sexp.to_string(UHPat.sexp_of_affix(surround2)),
          );
          print_endline("Here 2");
        };*/
      [
        [AnnotatedSkel.get_root_num(skel1)],
        [AnnotatedSkel.get_root_num(skel2)],
      ]
    }
  };
}
and pattern_paths = (ZOpSeq(skel, zseq): ZPat.t): list(CursorPath.steps) => {
  // handle n-tuples
  switch (zseq) {
  | ZOperator((_, Comma), (prefix, suffix)) =>
    // cursor on tuple comma
    let length = Seq.length(prefix) + Seq.length(suffix);
    let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, length);
    List.map(
      skel => [AnnotatedSkel.get_root_num(skel)],
      UHPat.get_annotated_tuple_elements(annotated_skel),
    );
  | _ =>
    // cursor within tuple element
    let cursor_skel =
      skel
      |> UHPat.get_tuple_elements
      |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
    let (annotated_skel, _) =
      AnnotatedSkel.mk(cursor_skel, 0, ZSeq.length(zseq));
    CursorPath_common.explanation_paths_skel(
      pattern_zoperand,
      cursor_skel,
      annotated_skel,
      zseq,
    );
  };
}
and pattern_zoperand = (zoperand: ZPat.zoperand): list(CursorPath.steps) => {
  switch (zoperand) {
  | CursorP(_, operand) => pattern_operand(operand, true)
  | ParenthesizedZ(zpat) => cons_all(0, pattern_paths(zpat))
  | TypeAnnZP(_, zoperand, _) => cons_all(0, pattern_zoperand(zoperand))
  | TypeAnnZA(_, _, ztyp) => cons_all(1, typ_paths(ztyp))
  | InjZ(_, _, zpat) => cons_all(0, pattern_paths(zpat))
  };
}
and pattern_operand =
    (operand: UHPat.operand, show_inj_child: bool): list(CursorPath.steps) => {
  switch (operand) {
  | EmptyHole(_)
  | Wild(_)
  | InvalidText(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_) => [[]]
  | Inj(_, _, pat) =>
    if (show_inj_child) {
      [[0, ...CursorPath_common.child_root_node_path(pat)]];
    } else {
      [[]];
    }
  | TypeAnn(_, operand, typ) =>
    cons_all(0, pattern_operand(operand, show_inj_child))
    @ [[1, ...CursorPath_common.child_root_node_path(typ)]]
  | Parenthesized(pat) =>
    cons_all(0, child_pattern_paths(pat, show_inj_child))
  };
}
and typ_paths = (ZOpSeq(skel, zseq): ZTyp.t): list(CursorPath.steps) => {
  // handle n-tuples
  switch (zseq) {
  | ZOperator((_, Prod), (prefix, suffix)) =>
    // cursor on tuple comma
    let length = Seq.length(prefix) + Seq.length(suffix);
    let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, length);
    List.map(
      skel => [AnnotatedSkel.get_root_num(skel)],
      UHTyp.get_annotated_prod_elements(annotated_skel),
    );
  | _ =>
    // cursor within tuple element
    let cursor_skel =
      skel
      |> UHTyp.get_prod_elements
      |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
    let (annotated_skel, _) =
      AnnotatedSkel.mk(cursor_skel, 0, ZSeq.length(zseq));
    CursorPath_common.explanation_paths_skel(
      typ_zoperand,
      cursor_skel,
      annotated_skel,
      zseq,
    );
  };
}
and typ_zoperand = (zoperand: ZTyp.zoperand): list(CursorPath.steps) => {
  switch (zoperand) {
  | CursorT(_, operand) => typ_operand(operand)
  | ParenthesizedZ(zty)
  | ListZ(zty) => cons_all(0, typ_paths(zty))
  };
}
and typ_operand = (operand: UHTyp.operand): list(CursorPath.steps) => {
  switch (operand) {
  | Hole
  | Unit
  | Int
  | Float
  | Bool => [[]]
  | Parenthesized(typ) => cons_all(0, child_typ_paths(typ))
  | List(typ) => [[0, ...CursorPath_common.child_root_node_path(typ)]]
  };
}
and child_typ_paths =
    (OpSeq(skel, seq): UHTyp.opseq): list(CursorPath.steps) => {
  let (annotated_skel, _) = AnnotatedSkel.mk(skel, 0, Seq.length(seq));
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    cons_all(n, typ_operand(pn));
  | BinOp(_, Prod, _, _) =>
    List.map(
      skel => [AnnotatedSkel.get_root_num(skel)],
      UHTyp.get_annotated_prod_elements(annotated_skel),
    )
  | BinOp(_, Arrow, _, _)
  | BinOp(_, Sum, _, _) =>
    switch (annotated_skel) {
    | Placeholder(_) => failwith("Can I reach here?")
    | BinOp(_, _, skel1, skel2) => [
        [AnnotatedSkel.get_root_num(skel1)],
        [AnnotatedSkel.get_root_num(skel2)],
      ]
    }
  };
};
