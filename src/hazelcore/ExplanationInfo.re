let cons_all =
    (step: int, steps: list(CursorPath.steps)): list(CursorPath.steps) => {
  List.map(steps => [step, ...steps], steps);
};

let rec explanation_paths = (ze: ZExp.t): list(CursorPath.steps) =>
  explanation_pathsblock(ze)
and explanation_pathsblock = (zblock: ZExp.zblock): list(CursorPath.steps) => {
  let prefix_len = ZList.prefix_length(zblock);
  let zline = ZList.prj_z(zblock);
  cons_all(prefix_len, explanation_pathsline(zline));
}
and explanation_pathsline = (zline: ZExp.zline): list(CursorPath.steps) =>
  switch (zline) {
  | CursorL(_, EmptyLine | CommentLine(_)) => [[]]
  | CursorL(_, ExpLine(opseq)) => operand_child_paths(opseq)
  | CursorL(_, LetLine(pat, _def)) => cons_all(0, child_pattern_paths(pat)) @ [[1]] /* TODO Hannah get the body */
  | LetLineZP(zp, _) => cons_all(0, pattern_paths(zp))
  | LetLineZE(_, zdef) => cons_all(1, explanation_paths(zdef))
  | ExpLineZ(zopseq) =>
    CursorPath_common.explanation_pathsopseq(
      ~of_zoperand=explanation_pathsoperand,
      zopseq,
    )
  }

and operand_paths = (operand: UHExp.operand): list(CursorPath.steps) =>
  switch (operand) {
  | EmptyHole(_)
  | InvalidText(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_) => [[]]
  | Lam(_, pat, _body) => cons_all(0, child_pattern_paths(pat)) @ [[1]]
  | Inj(_, _, _) => [[0]]
  | Case(_, _scrut, rules) => [
      [0],
      ...List.flatten(
           List.mapi(
             (index, _) => [[index + 1, 0], [index + 1, 1]],
             rules,
           ),
         ),
    ]
  | Parenthesized(operand) => cons_all(0, child_expblock_paths(operand))
  | ApPalette(_) => []
  }
and child_expblock_paths = (block: UHExp.block): list(CursorPath.steps) => {
  switch (block) {
  | [] => []
  | [line] => cons_all(0, child_expline_paths(line))
  | lines => List.mapi((index, _) => [index], lines)
  };
}
and child_expline_paths = (line: UHExp.line): list(CursorPath.steps) => {
  switch (line) {
  | EmptyLine
  | CommentLine(_) => [[]]
  | LetLine(pat, _def) => cons_all(0, child_pattern_paths(pat)) @ [[1]] /* TODO: Hannah get the body of the let */
  | ExpLine(opseq) => cons_all(0, operand_child_paths(opseq))
  };
}
and operand_child_paths =
    (OpSeq(skel, seq): UHExp.opseq): list(CursorPath.steps) => {
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    operand_paths(pn);
  | BinOp(_, Comma, _, _) =>
    List.mapi((index, _) => [index], UHExp.get_tuple_elements(skel))
  | BinOp(_, Space | Cons, _, _)
  | BinOp(_, And | Or, _, _)
  | BinOp(
      _,
      Plus | Minus | Times | Divide | LessThan | GreaterThan | Equals,
      _,
      _,
    )
  | BinOp(
      _,
      FPlus | FMinus | FTimes | FDivide | FLessThan | FGreaterThan | FEquals,
      _,
      _,
    ) => [
      [0],
      [1],
    ]
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
  | CursorR(_, Rule(pat, _)) => child_pattern_paths(pat) @ [[1]]
  | RuleZP(zp, _) => cons_all(0, pattern_paths(zp))
  | RuleZE(_, zclause) => cons_all(1, explanation_paths(zclause))
  }
and child_pattern_paths =
    (OpSeq(skel, seq): UHPat.opseq): list(CursorPath.steps) => {
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let rec handle_op = (pn: UHPat.operand) =>
      switch (pn) {
      | TypeAnn(_, operand, _) => cons_all(0, handle_op(operand)) @ [[1]]
      | Parenthesized(pattern) => cons_all(0, child_pattern_paths(pattern))
      | _ => [[]]
      };
    handle_op(pn);
  | BinOp(_, Comma, _, _) =>
    List.mapi((index, _) => [index], UHPat.get_tuple_elements(skel))
  | BinOp(_, Space, _, _)
  | BinOp(_, Cons, _, _) => [[0], [1]]
  };
}
and pattern_paths = (zp: ZPat.t): list(CursorPath.steps) =>
  CursorPath_common.explanation_pathsopseq(~of_zoperand=pattern_zoperand, zp)
and pattern_zoperand = (zoperand: ZPat.zoperand): list(CursorPath.steps) => {
  switch (zoperand) {
  | CursorP(_, operand) => pattern_operand(operand)
  | ParenthesizedZ(zpat) => cons_all(0, pattern_paths(zpat))
  | TypeAnnZP(_, zoperand, _) => cons_all(0, pattern_zoperand(zoperand))
  | TypeAnnZA(_, _, ztyp) => cons_all(1, typ_paths(ztyp))
  | InjZ(_, _, zpat) => cons_all(0, pattern_paths(zpat))
  };
}
and pattern_operand = (operand: UHPat.operand): list(CursorPath.steps) => {
  switch (operand) {
  | EmptyHole(_)
  | Wild(_)
  | InvalidText(_)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_) => [[]]
  | TypeAnn(_, operand, _typ) =>
    cons_all(1, pattern_operand(operand)) @ [[1]]
  | Parenthesized(pat) => cons_all(0, child_pattern_paths(pat))
  | Inj(_, _, _pat) => [[0]]
  };
}
and typ_paths = (zty: ZTyp.t): list(CursorPath.steps) =>
  CursorPath_common.explanation_pathsopseq(~of_zoperand=typ_zoperand, zty)
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
  | List(_) => [[0]]
  };
}
and child_typ_paths =
    (OpSeq(skel, seq): UHTyp.opseq): list(CursorPath.steps) => {
  switch (skel) {
  | Placeholder(n) =>
    let pn: UHTyp.operand = Seq.nth_operand(n, seq);
    switch (pn) {
    | Parenthesized(pattern) => child_typ_paths(pattern)
    | _ => [[]]
    };
  | BinOp(_, Prod, _, _) =>
    List.mapi((index, _) => [index], UHTyp.get_prod_elements(skel))
  | BinOp(_, Arrow, _, _)
  | BinOp(_, Sum, _, _) => [[0], [1]]
  };
};
