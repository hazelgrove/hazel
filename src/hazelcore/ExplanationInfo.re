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
  | CursorL(_, line) => explanation_paths_line(line)
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
    | BinOp(_, _, skel1, skel2) => [
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
