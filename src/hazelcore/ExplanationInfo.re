open Sexplib;
open Sexplib.Std;

let cons_all =
    (step: int, steps: list(CursorPath.steps)): list(CursorPath.steps) => {
  List.map(steps => [step, ...steps], steps);
};

[@deriving sexp]
type annotated_skel =
  | Placeholder(int)
  | BinOp(Operators_Exp.t, int, annotated_skel, annotated_skel);
let length = (zseq: ZExp.zseq): int =>
  switch (zseq) {
  | ZOperand(_, (_prefix, _suffix)) => (-1)
  | ZOperator((_, _), (prefix, suffix)) =>
    Seq.length(prefix) + Seq.length(suffix)
  };
let rec mk_annotated_skel =
        (skel: UHExp.skel, start_index: int, length: int)
        : (annotated_skel, int) => {
  switch (skel) {
  | Placeholder(n) => (Placeholder(n), start_index)
  | BinOp(_, op, skel1, skel2) =>
    let (left_annotated, left_index) =
      mk_annotated_skel(skel1, start_index, length);
    let (right_annotated, right_index) =
      mk_annotated_skel(skel2, left_index + 1, length);
    (
      BinOp(op, left_index + length, left_annotated, right_annotated),
      right_index,
    );
  };
};
let get_root_num = (skel: annotated_skel): int => {
  switch (skel) {
  | Placeholder(n)
  | BinOp(_, n, _, _) => n
  };
};
let rec get_tuple_elements: annotated_skel => list(annotated_skel) =
  fun
  | BinOp(Comma, _, skel1, skel2) =>
    get_tuple_elements(skel1) @ get_tuple_elements(skel2)
  | skel => [skel];

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
  | CursorL(_, LetLine(pat, def)) =>
    cons_all(0, child_pattern_paths(pat))
    @ cons_all(1, child_expblock_paths(def)) /* TODO Hannah get the body */
  | LetLineZP(zp, _) => cons_all(0, pattern_paths(zp))
  | LetLineZE(_, zdef) => cons_all(1, explanation_paths(zdef))
  | ExpLineZ(zopseq) =>
    print_endline(Sexp.to_string(ZExp.sexp_of_zopseq(zopseq))) /*CursorPath_common.explanation_pathsopseq(
    ~of_zoperand=explanation_pathsoperand,
    zopseq,
  );*/;

    explanation_paths_zopseq(zopseq);
  }
and explanation_paths_zopseq =
    (ZOpSeq(skel, zseq): ZExp.zopseq): list(CursorPath.steps) => {
  // handle n-tuples
  switch (zseq) {
  | ZOperator((_, Comma), (prefix, suffix)) =>
    // cursor on tuple comma
    let length = Seq.length(prefix) + Seq.length(suffix);
    let (annotated_skel, _) = mk_annotated_skel(skel, 0, length);
    List.map(
      skel => {
        [
          get_root_num(skel),
        ]
      },
      get_tuple_elements(annotated_skel),
    );
  | _ =>
    // cursor within tuple element
    let cursor_skel =
      skel
      |> UHExp.get_tuple_elements
      |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
    let (annotated_skel, _) =
      mk_annotated_skel(cursor_skel, 0, length(zseq));
    explanation_paths_skel(cursor_skel, annotated_skel, zseq);
  };
}
and explanation_paths_skel =
    (skel: UHExp.skel, ann_skel: annotated_skel, zseq: ZExp.zseq)
    : list(CursorPath.steps) =>
  if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
    // found cursor
    switch (zseq) {
    | ZOperand(zoperand, (prefix, _)) =>
      // skel must be Placeholder
      cons_all(
        Seq.length_of_affix(prefix),
        explanation_pathsoperand(zoperand),
      )
    | ZOperator(_, (_, _)) =>
      switch (ann_skel) {
      | Placeholder(_) => failwith("Can I reach here?")
      | BinOp(_, _, skel1, skel2) => [
          [get_root_num(skel1)],
          [get_root_num(skel2)],
        ]
      }
    };
  } else {
    // recurse toward cursor
    switch (skel) {
    | Placeholder(_) => []
    | BinOp(_, Comma, _, _) =>
      failwith(
        "Exp.syn_cursor_info_skel: expected commas to be handled at opseq level",
      )
    | BinOp(_, _, skel1, skel2) =>
      switch (ann_skel) {
      | Placeholder(_) => failwith("Error constructing annotated skel")
      | BinOp(_, _, ann1, ann2) =>
        explanation_paths_skel(skel1, ann1, zseq)
        @ explanation_paths_skel(skel2, ann2, zseq)
      }
    };
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
  | Lam(_, pat, body) =>
    cons_all(0, child_pattern_paths(pat))
    @ cons_all(1, child_expblock_paths(body))
  | Inj(_, _, _) => [[0]]
  | Case(_, scrut, rules) =>
    print_endline(Sexp.to_string(UHExp.sexp_of_t(scrut)));
    cons_all(0, child_expblock_paths(scrut))
    @ List.flatten(
        List.mapi(
          (index, UHExp.Rule(pat, exp)) => {
            let pat_paths = cons_all(0, child_pattern_paths(pat));
            let exp_paths = cons_all(1, child_expblock_paths(exp));
            cons_all(index + 1, pat_paths @ exp_paths);
          },
          rules,
        ),
      );
  | Parenthesized(operand) => cons_all(0, child_expblock_paths(operand))
  | ApPalette(_) => []
  }
and child_expblock_paths = (block: UHExp.block): list(CursorPath.steps) => {
  switch (block) {
  | [] => []
  | [line] => cons_all(0, child_expline_paths(line))
  | lines =>
    List.flatten(
      List.mapi(
        (index, line) => cons_all(index, child_expline_paths(line)),
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
  | ExpLine(opseq) => operand_child_paths(opseq)
  };
}
and operand_child_paths =
    (OpSeq(skel, seq): UHExp.opseq): list(CursorPath.steps) => {
  let (annotated_skel, _) = mk_annotated_skel(skel, 0, Seq.length(seq));
  [[get_root_num(annotated_skel)]];
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
    /* TODO Hannah this probably isn't correct */
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
