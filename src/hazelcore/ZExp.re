open Sexplib.Std;

[@deriving sexp]
type t = zblock
and zblock = ZList.t(zline, UHExp.line)
and zline =
  | CursorL(CursorPosition.t, UHExp.line)
  | ExpLineZ(zopseq)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.t)
  | LetLineZE(UHPat.t, option(UHTyp.t), t)
  | StructLineZP(ZPat.t, unit, UHExp.t)
  | StructLineZE(UHPat.t, unit, t)
and zopseq = ZOpSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator)
and zoperand =
  | CursorE(CursorPosition.t, UHExp.operand)
  | ParenthesizedZ(t)
  | LamZP(ErrStatus.t, ZPat.t, option(UHTyp.t), UHExp.t)
  | LamZA(ErrStatus.t, UHPat.t, ZTyp.t, UHExp.t)
  | LamZE(ErrStatus.t, UHPat.t, option(UHTyp.t), t)
  | InjZ(ErrStatus.t, InjSide.t, t)
  | CaseZE(CaseErrStatus.t, t, list(UHExp.rule))
  | CaseZR(CaseErrStatus.t, UHExp.t, zrules)
  | ApPaletteZ(
      ErrStatus.t,
      PaletteName.t,
      SerializedModel.t,
      ZSpliceInfo.t(UHExp.t, t),
    )
  // ECD TODO: Does the label need to be a zexpression to be able to edit it? Probably, changing that now
  | PrjZE(ErrStatus.t, zoperand, Label.t)
and zoperator = (CursorPosition.t, UHExp.operator)
and zrules = ZList.t(zrule, UHExp.rule)
and zrule =
  | CursorR(CursorPosition.t, UHExp.rule)
  | RuleZP(ZPat.t, UHExp.t)
  | RuleZE(UHPat.t, t);

type operand_surround = Seq.operand_surround(UHExp.operand, UHExp.operator);
type operator_surround = Seq.operator_surround(UHExp.operand, UHExp.operator);
type zseq = ZSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator);

let prune_type_annotation: zoperand => zoperand =
  fun
  | CursorE(OnDelim(k, _) as cursor, Lam(err, p, Some(_), body)) when k != 1 =>
    CursorE(cursor, Lam(err, p, None, body))
  | LamZP(err, zp, Some(_), body) => LamZP(err, zp, None, body)
  | LamZE(err, p, Some(_), zbody) => LamZE(err, p, None, zbody)
  | zoperand => zoperand;

let line_can_be_swapped = (line: zline): bool =>
  switch (line) {
  | CursorL(_)
  | LetLineZP(_)
  | LetLineZA(_)
  | StructLineZP(_)
  | ExpLineZ(ZOpSeq(_, ZOperator(_)))
  | ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(LamZP(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(LamZA(_), _))) => true
  | LetLineZE(_)
  | StructLineZE(_)
  | ExpLineZ(ZOpSeq(_, ZOperand(LamZE(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(InjZ(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(CaseZE(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(CaseZR(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(ParenthesizedZ(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(ApPaletteZ(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(PrjZE(_, _, _), _))) => false
  };
let valid_cursors_line = (line: UHExp.line): list(CursorPosition.t) =>
  switch (line) {
  | CommentLine(comment) =>
    CursorPosition.[OnDelim(0, Before), OnDelim(0, After)]
    @ (
      ListUtil.range(String.length(comment) + 1)
      |> List.map(i => CursorPosition.OnText(i))
    )
  | ExpLine(_) => []
  | EmptyLine => [OnText(0)]
  | LetLine(_, ann, _) =>
    let ann_cursors =
      switch (ann) {
      | None => []
      | Some(_) => CursorPosition.delim_cursors_k(1)
      };
    CursorPosition.delim_cursors_k(0)
    @ ann_cursors
    @ CursorPosition.delim_cursors_k(2)
    @ CursorPosition.delim_cursors_k(3);
  | StructLine(_) => CursorPosition.delim_cursors(3) // TODO (hejohns): check
  };
let valid_cursors_operator: UHExp.operator => list(CursorPosition.t) =
  fun
  | _ => [OnOp(Before), OnOp(After)];
let valid_cursors_operand: UHExp.operand => list(CursorPosition.t) =
  fun
  /* outer nodes - delimiter */
  | EmptyHole(_)
  | ListNil(_) => CursorPosition.delim_cursors(1)
  /* outer nodes - text */
  | InvalidText(_, t) => CursorPosition.text_cursors(String.length(t))
  | Var(_, _, x) => CursorPosition.text_cursors(Var.length(x))
  | IntLit(_, n) => CursorPosition.text_cursors(String.length(n))
  | FloatLit(_, f) => CursorPosition.text_cursors(String.length(f))
  | BoolLit(_, b) => CursorPosition.text_cursors(b ? 4 : 5)
  /* inner nodes */
  | Lam(_, _, ann, _) => {
      let colon_positions =
        switch (ann) {
        | Some(_) => CursorPosition.delim_cursors_k(1)
        | None => []
        };
      CursorPosition.delim_cursors_k(0)
      @ colon_positions
      @ CursorPosition.delim_cursors_k(2)
      @ CursorPosition.delim_cursors_k(3);
    }
  | Inj(_) => CursorPosition.delim_cursors(2)
  | Case(_) => CursorPosition.delim_cursors(2)
  | Parenthesized(_) => CursorPosition.delim_cursors(2)
  | ApPalette(_) => CursorPosition.delim_cursors(1) /* TODO[livelits] */
  | Label(_, l) => CursorPosition.text_cursors(Label.length(l))
  | Prj(_, _, pl) => CursorPosition.text_cursors(Label.length(pl));
let valid_cursors_rule = (_: UHExp.rule): list(CursorPosition.t) =>
  CursorPosition.delim_cursors(2);

let is_valid_cursor_line = (cursor: CursorPosition.t, line: UHExp.line): bool =>
  valid_cursors_line(line) |> List.mem(cursor);
let is_valid_cursor_operand =
    (cursor: CursorPosition.t, operand: UHExp.operand): bool =>
  valid_cursors_operand(operand) |> List.mem(cursor);
let is_valid_cursor_operator =
    (cursor: CursorPosition.t, operator: UHExp.operator): bool =>
  valid_cursors_operator(operator) |> List.mem(cursor);
let is_valid_cursor_rule = (cursor: CursorPosition.t, rule: UHExp.rule): bool =>
  valid_cursors_rule(rule) |> List.mem(cursor);

module ZLine = {
  let force_get_zopseq =
    fun
    | CursorL(_)
    | LetLineZP(_)
    | LetLineZA(_)
    | LetLineZE(_)
    | StructLineZP(_)
    | StructLineZE(_) => failwith("force_get_zopseq: expected ExpLineZ")
    | ExpLineZ(zopseq) => zopseq;
};

module ZBlock = {
  let wrap' = (zopseq: zopseq): zblock => ([], ExpLineZ(zopseq), []);
  let wrap = (zoperand: zoperand): zblock => wrap'(ZOpSeq.wrap(zoperand));
};

let rec is_before = (ze: t): bool => ze |> is_before_zblock
and is_before_zblock = ((prefix, zline, _): zblock): bool =>
  switch (prefix) {
  | []
  | [EmptyLine] => is_before_zline(zline)
  | _ => false
  }
and is_before_zline = (zline: zline): bool =>
  switch (zline) {
  | CursorL(cursor, CommentLine(_)) => cursor == OnDelim(0, Before)
  | CursorL(cursor, EmptyLine) => cursor == OnText(0)
  | CursorL(cursor, LetLine(_, _, _)) => cursor == OnDelim(0, Before)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | CursorL(cursor, StructLine(_, _, _)) => cursor == OnDelim(0, Before)
  | ExpLineZ(zopseq) => is_before_zopseq(zopseq)
  | LetLineZP(_)
  | LetLineZA(_)
  | LetLineZE(_)
  | StructLineZP(_)
  | StructLineZE(_) => false
  }
and is_before_zopseq = zopseq => ZOpSeq.is_before(~is_before_zoperand, zopseq)
and is_before_zoperand =
  fun
  | CursorE(cursor, EmptyHole(_))
  | CursorE(cursor, ListNil(_)) => cursor == OnDelim(0, Before)
  | CursorE(cursor, InvalidText(_, _))
  | CursorE(cursor, Var(_))
  | CursorE(cursor, IntLit(_))
  | CursorE(cursor, FloatLit(_))
  | CursorE(cursor, BoolLit(_))
  | CursorE(cursor, Label(_))
  | CursorE(cursor, Prj(_, _, _)) => cursor == OnText(0)
  | CursorE(cursor, Lam(_))
  | CursorE(cursor, Inj(_))
  | CursorE(cursor, Case(_))
  | CursorE(cursor, Parenthesized(_)) => cursor == OnDelim(0, Before)
  | CursorE(cursor, ApPalette(_)) => cursor == OnDelim(0, Before) /* TODO[livelits] */
  | PrjZE(_, zop, _) => is_before_zoperand(zop)
  | ParenthesizedZ(_)
  | LamZP(_)
  | LamZA(_)
  | LamZE(_)
  | InjZ(_)
  | CaseZE(_)
  | CaseZR(_)
  | ApPaletteZ(_) => false;
// The following 2 functions are specifically for CommentLines!!
// Check if the cursor at "OnDelim(After)" in a "CommentLine"
/* For example:
           # Comment1
           #| Comment2

   */
let is_begin_of_comment = ((prefix, zline, _): zblock): bool =>
  switch (zline) {
  | CursorL(cursor, CommentLine(_)) =>
    switch (prefix |> ListUtil.split_last_opt) {
    | Some((_, CommentLine(_))) => cursor == OnDelim(0, After)
    | _ => false
    }
  | _ => false
  };
// Check if the cursor at the end of a "CommentLine"
/* For example:
           # Comment1|
           # Comment2

   */
let is_end_of_comment = ((_, zline, suffix): zblock): bool =>
  switch (zline) {
  | CursorL(cursor, CommentLine(comment)) =>
    switch (suffix) {
    | [CommentLine(_), ..._] => cursor == OnText(String.length(comment))
    | _ => false
    }
  | _ => false
  };

let is_before_zrule =
  fun
  | CursorR(OnDelim(0, Before), _) => true
  | _ => false;
let is_before_zoperator: zoperator => bool =
  fun
  | (OnOp(Before), _) => true
  | _ => false;

let rec is_after = (ze: t): bool => ze |> is_after_zblock
and is_after_zblock = ((_, zline, suffix): zblock): bool =>
  switch (suffix) {
  | [] => is_after_zline(zline)
  | _ => false
  }
and is_after_zline =
  fun
  | CursorL(cursor, CommentLine(comment)) =>
    cursor == OnText(String.length(comment))
  | CursorL(cursor, EmptyLine) => cursor == OnText(0)
  | CursorL(cursor, LetLine(_, _, _)) => cursor == OnDelim(3, After)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | CursorL(cursor, StructLine(_, _, _)) => cursor == OnDelim(2, After)
  | ExpLineZ(zopseq) => is_after_zopseq(zopseq)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _)
  | StructLineZP(_, _, _)
  | StructLineZE(_, _, _) => false
and is_after_zopseq = zopseq => ZOpSeq.is_after(~is_after_zoperand, zopseq)
and is_after_zoperand =
  fun
  | CursorE(cursor, EmptyHole(_))
  | CursorE(cursor, ListNil(_)) => cursor == OnDelim(0, After)
  | CursorE(cursor, InvalidText(_, t)) =>
    cursor == OnText(String.length(t))
  | CursorE(cursor, Var(_, _, x)) => cursor == OnText(Var.length(x))
  | CursorE(cursor, IntLit(_, n)) => cursor == OnText(String.length(n))
  | CursorE(cursor, FloatLit(_, f)) => cursor == OnText(String.length(f))
  | CursorE(cursor, BoolLit(_, true)) => cursor == OnText(4)
  | CursorE(cursor, BoolLit(_, false)) => cursor == OnText(5)
  | CursorE(cursor, Lam(_)) => cursor == OnDelim(3, After)
  | CursorE(cursor, Case(_)) => cursor == OnDelim(1, After)
  | CursorE(cursor, Inj(_)) => cursor == OnDelim(1, After)
  | CursorE(cursor, Parenthesized(_)) => cursor == OnDelim(1, After)
  | CursorE(_, ApPalette(_)) => false /* TODO[livelits] */
  | CursorE(cursor, Label(_, l)) => cursor == OnText(Label.length(l))
  | CursorE(cursor, Prj(_, _, _)) => cursor == OnDelim(1, After)
  | ParenthesizedZ(_) => false
  | LamZP(_)
  | LamZA(_)
  | LamZE(_)
  | InjZ(_)
  | CaseZE(_)
  | CaseZR(_)
  | ApPaletteZ(_)
  | PrjZE(_, _, _) => false;

let is_after_zrule =
  fun
  | RuleZE(_, zclause) => is_after(zclause)
  | _ => false;
let is_after_zoperator: zoperator => bool =
  fun
  | (OnOp(After), _) => true
  | _ => false;

let rec is_outer = (ze: t): bool => ze |> is_outer_zblock
and is_outer_zblock = ((_, zline, suffix): zblock): bool =>
  switch (suffix) {
  | [] => is_outer_zline(zline)
  | _ => false
  }
and is_outer_zline = (zline: zline): bool =>
  switch (zline) {
  | CursorL(_, EmptyLine)
  | CursorL(_, CommentLine(_))
  | CursorL(_, LetLine(_, _, _))
  | CursorL(_, StructLine(_, _, _)) => true
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | ExpLineZ(zopseq) => is_outer_zopseq(zopseq)
  | LetLineZP(_)
  | LetLineZA(_)
  | LetLineZE(_)
  | StructLineZP(_)
  | StructLineZE(_) => false
  }
and is_outer_zopseq = zopseq => ZOpSeq.is_outer(~is_outer_zoperand, zopseq)
and is_outer_zoperand =
  fun
  | CursorE(_, EmptyHole(_))
  | CursorE(_, InvalidText(_, _))
  | CursorE(_, ListNil(_))
  | CursorE(_, Var(_))
  | CursorE(_, IntLit(_))
  | CursorE(_, FloatLit(_))
  | CursorE(_, BoolLit(_))
  | CursorE(_, Lam(_))
  | CursorE(_, Inj(_))
  | CursorE(_, Case(_))
  | CursorE(_, Parenthesized(_))
  | CursorE(_, ApPalette(_))
  | CursorE(_, Label(_))
  | CursorE(_, Prj(_)) => true
  | ParenthesizedZ(zexp) => is_outer(zexp)
  | LamZP(_)
  | LamZA(_)
  | LamZE(_)
  | InjZ(_)
  | CaseZE(_)
  | CaseZR(_)
  | ApPaletteZ(_)
  | PrjZE(_, _, _) => false;

let rec place_before = (e: UHExp.t): t => e |> place_before_block
and place_before_block =
  fun
  | [] => failwith("place_before_block: empty block")
  | [EmptyLine, first, ...rest] => (
      [EmptyLine],
      first |> place_before_line,
      rest,
    )
  | [first, ...rest] => ([], first |> place_before_line, rest)
and place_before_line =
  fun
  | CommentLine(_) as line => CursorL(OnDelim(0, Before), line)
  | EmptyLine => CursorL(OnText(0), EmptyLine)
  | LetLine(_, _, _) as line => CursorL(OnDelim(0, Before), line)
  | ExpLine(opseq) => ExpLineZ(place_before_opseq(opseq))
  | StructLine(_, _, _) as line => CursorL(OnDelim(0, Before), line)
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand = operand =>
  switch (operand) {
  | EmptyHole(_)
  | ListNil(_) => CursorE(OnDelim(0, Before), operand)
  | InvalidText(_, _)
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | Label(_) => CursorE(OnText(0), operand)
  | Lam(_)
  | Inj(_)
  | Case(_)
  | Parenthesized(_) => CursorE(OnDelim(0, Before), operand)
  | ApPalette(_) => CursorE(OnDelim(0, Before), operand) /* TODO[livelits] */
  | Prj(err, exp, label) => PrjZE(err, place_before_operand(exp), label)
  };
let place_before_rule = (rule: UHExp.rule): zrule =>
  CursorR(OnDelim(0, Before), rule);
let place_before_operator = (op: UHExp.operator): option(zoperator) =>
  switch (op) {
  | Space => None
  | _ => Some((OnOp(Before), op))
  };

let rec place_after = (e: UHExp.t): t => e |> place_after_block
and place_after_block = (block: UHExp.block): zblock =>
  switch (block |> ListUtil.split_last_opt) {
  | None => failwith("place_after_block: empty block")
  | Some((leading, last)) => (leading, last |> place_after_line, [])
  }
and place_after_line =
  fun
  | CommentLine(comment) as line =>
    CursorL(OnText(String.length(comment)), line)
  | EmptyLine => CursorL(OnText(0), EmptyLine)
  | LetLine(_) as line => CursorL(OnDelim(3, After), line)
  | ExpLine(e) => ExpLineZ(place_after_opseq(e))
  | StructLine(_) as line => CursorL(OnDelim(2, After), line)
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand = operand =>
  switch (operand) {
  | EmptyHole(_)
  | ListNil(_) => CursorE(OnDelim(0, After), operand)
  | InvalidText(_, t) => CursorE(OnText(String.length(t)), operand)
  | Var(_, _, x) => CursorE(OnText(Var.length(x)), operand)
  | IntLit(_, n) => CursorE(OnText(String.length(n)), operand)
  | FloatLit(_, f) => CursorE(OnText(String.length(f)), operand)
  | BoolLit(_, true) => CursorE(OnText(4), operand)
  | BoolLit(_, false) => CursorE(OnText(5), operand)
  | Lam(_) => CursorE(OnDelim(3, After), operand)
  | Case(_) => CursorE(OnDelim(1, After), operand)
  | Inj(_) => CursorE(OnDelim(1, After), operand)
  | Parenthesized(_) => CursorE(OnDelim(1, After), operand)
  | ApPalette(_) => CursorE(OnDelim(0, After), operand) /* TODO[livelits] */
  | Label(_, l) => CursorE(OnText(Label.length(l)), operand)
  | Prj(_, _, _) => CursorE(OnDelim(1, After), operand)
  };
let place_after_rule = (Rule(p, clause): UHExp.rule): zrule =>
  RuleZE(p, place_after(clause));
let place_after_operator = (op: UHExp.operator): option(zoperator) =>
  switch (op) {
  | Space => None
  | _ => Some((OnOp(After), op))
  };

let place_cursor_operator =
    (cursor: CursorPosition.t, operator: UHExp.operator): option(zoperator) =>
  is_valid_cursor_operator(cursor, operator)
    ? Some((cursor, operator)) : None;
let place_cursor_operand =
    (cursor: CursorPosition.t, operand: UHExp.operand): option(zoperand) =>
  is_valid_cursor_operand(cursor, operand)
    ? Some(CursorE(cursor, operand)) : None;
let place_cursor_line =
    (cursor: CursorPosition.t, line: UHExp.line): option(zline) =>
  switch (line) {
  | ExpLine(_) =>
    // all cursor positions in a zopseq are
    // encoded in steps, not CursorPosition.t
    None
  | EmptyLine
  | CommentLine(_)
  | LetLine(_, _, _)
  | StructLine(_, _, _) =>
    is_valid_cursor_line(cursor, line) ? Some(CursorL(cursor, line)) : None
  };
let place_cursor_rule =
    (cursor: CursorPosition.t, rule: UHExp.rule): option(zrule) =>
  is_valid_cursor_rule(cursor, rule) ? Some(CursorR(cursor, rule)) : None;

let prune_empty_hole_line = (zli: zline): zline =>
  switch (zli) {
  | ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_, EmptyHole(_)), (E, E)))) =>
    place_before_line(EmptyLine)
  | ExpLineZ(_)
  | LetLineZP(_)
  | LetLineZA(_)
  | LetLineZE(_)
  | StructLineZP(_)
  | StructLineZE(_)
  | CursorL(_) => zli
  };
let prune_empty_hole_lines = ((prefix, zline, suffix): zblock): zblock =>
  switch (suffix) {
  | [] => (prefix |> UHExp.Lines.prune_empty_holes, zline, [])
  | [_, ..._] => (
      prefix |> UHExp.Lines.prune_empty_holes,
      prune_empty_hole_line(zline),
      suffix |> UHExp.Block.prune_empty_hole_lines,
    )
  };

let rec erase = (ze: t): UHExp.t => ze |> erase_zblock
and erase_zblock = ((prefix, zline, suffix): zblock): UHExp.block =>
  prefix @ [zline |> erase_zline] @ suffix
and erase_zline =
  fun
  | CursorL(_, line) => line
  | ExpLineZ(zopseq) => ExpLine(erase_zopseq(zopseq))
  | LetLineZP(zp, ann, def) => LetLine(ZPat.erase(zp), ann, def)
  | LetLineZA(p, zann, def) => LetLine(p, Some(ZTyp.erase(zann)), def)
  | LetLineZE(p, ann, zdef) => LetLine(p, ann, erase(zdef))
  | StructLineZP(zp, ann, def) => StructLine(ZPat.erase(zp), ann, def) // ann == (), placeholder
  | StructLineZE(p, ann, zdef) => StructLine(p, ann, erase(zdef)) // ann == (), placeholder
and erase_zopseq = zopseq =>
  ZOpSeq.erase(~erase_zoperand, ~erase_zoperator, zopseq)
and erase_zoperator =
  fun
  | (_, operator) => operator
and erase_zoperand =
  fun
  | CursorE(_, operand) => operand
  | ParenthesizedZ(zbody) => Parenthesized(erase(zbody))
  | LamZP(err, zp, ann, body) => Lam(err, ZPat.erase(zp), ann, body)
  | LamZA(err, p, zann, body) => Lam(err, p, Some(ZTyp.erase(zann)), body)
  | LamZE(err, p, ann, zbody) => Lam(err, p, ann, erase(zbody))
  | InjZ(err, side, zbody) => Inj(err, side, erase(zbody))
  | CaseZE(err, zscrut, rules) => Case(err, erase(zscrut), rules)
  | CaseZR(err, scrut, zrules) => Case(err, scrut, erase_zrules(zrules))
  | ApPaletteZ(err, palette_name, serialized_model, zpsi) => {
      let psi = ZSpliceInfo.erase(zpsi, ((ty, z)) => (ty, erase(z)));
      ApPalette(err, palette_name, serialized_model, psi);
    }
  | PrjZE(err, zop, l) => Prj(err, erase_zoperand(zop), l)

and erase_zrules =
  fun
  | zrules => ZList.erase(zrules, erase_zrule)
and erase_zrule =
  fun
  | CursorR(_, rule) => rule
  | RuleZP(zp, clause) => Rule(ZPat.erase(zp), clause)
  | RuleZE(p, zclause) => Rule(p, erase(zclause));

let erase_zseq = ZSeq.erase(~erase_zoperand, ~erase_zoperator);

let mk_ZOpSeq =
  ZOpSeq.mk(~associate=UHExp.associate, ~erase_zoperand, ~erase_zoperator);

let get_err_status = ze => ze |> erase |> UHExp.get_err_status;
let get_err_status_zblock = zblock =>
  zblock |> erase_zblock |> UHExp.get_err_status_block;

let get_err_status_zopseq = zopseq =>
  zopseq |> erase_zopseq |> UHExp.get_err_status_opseq;
let get_err_status_zoperand = zoperand =>
  zoperand |> erase_zoperand |> UHExp.get_err_status_operand;

let rec set_err_status = (err: ErrStatus.t, ze: t): t =>
  ze |> set_err_status_zblock(err)
and set_err_status_zblock =
    (err: ErrStatus.t, (prefix, zline, suffix): zblock): zblock =>
  switch (suffix |> ListUtil.split_last_opt) {
  | None =>
    let zopseq = zline |> ZLine.force_get_zopseq;
    (prefix, ExpLineZ(zopseq |> set_err_status_zopseq(err)), []);
  | Some((suffix_leading, suffix_last)) =>
    let opseq = suffix_last |> UHExp.Line.force_get_opseq;
    (
      prefix,
      zline,
      suffix_leading @ [ExpLine(opseq |> UHExp.set_err_status_opseq(err))],
    );
  }
and set_err_status_zopseq = (err, zopseq) =>
  ZOpSeq.set_err_status(~set_err_status_zoperand, err, zopseq)
and set_err_status_zoperand = (err, zoperand) =>
  switch (zoperand) {
  | CursorE(cursor, operand) =>
    CursorE(cursor, UHExp.set_err_status_operand(err, operand))
  | ParenthesizedZ(zbody) => ParenthesizedZ(set_err_status(err, zbody))
  | LamZP(_, zp, ann, body) => LamZP(err, zp, ann, body)
  | LamZA(_, p, zann, body) => LamZA(err, p, zann, body)
  | LamZE(_, p, ann, zbody) => LamZE(err, p, ann, zbody)
  | InjZ(_, inj_side, zbody) => InjZ(err, inj_side, zbody)
  | CaseZE(_, zscrut, rules) =>
    CaseZE(StandardErrStatus(err), zscrut, rules)
  | CaseZR(_, scrut, zrules) =>
    CaseZR(StandardErrStatus(err), scrut, zrules)
  | ApPaletteZ(_, name, model, psi) => ApPaletteZ(err, name, model, psi)
  | PrjZE(_, zexp, label) => PrjZE(err, zexp, label)
  };

let rec mk_inconsistent = (u_gen: MetaVarGen.t, ze: t): (t, MetaVarGen.t) =>
  ze |> mk_inconsistent_zblock(u_gen)
and mk_inconsistent_zblock =
    (u_gen: MetaVarGen.t, (prefix, zline, suffix): zblock)
    : (zblock, MetaVarGen.t) =>
  switch (suffix |> ListUtil.split_last_opt) {
  | None =>
    let (zconclusion, u_gen) =
      zline |> ZLine.force_get_zopseq |> mk_inconsistent_zopseq(u_gen);
    ((prefix, ExpLineZ(zconclusion), []), u_gen);
  | Some((suffix_leading, suffix_last)) =>
    let (conclusion, u_gen) =
      suffix_last
      |> UHExp.Line.force_get_opseq
      |> UHExp.mk_inconsistent_opseq(u_gen);
    ((prefix, zline, suffix_leading @ [ExpLine(conclusion)]), u_gen);
  }
and mk_inconsistent_zopseq = (u_gen, zopseq) =>
  ZOpSeq.mk_inconsistent(~mk_inconsistent_zoperand, u_gen, zopseq)
and mk_inconsistent_zoperand = (u_gen, zoperand) =>
  switch (zoperand) {
  | CursorE(cursor, operand) =>
    let (operand, u_gen) = operand |> UHExp.mk_inconsistent_operand(u_gen);
    (CursorE(cursor, operand), u_gen);
  | ParenthesizedZ(zbody) =>
    let (zbody, u_gen) = mk_inconsistent(u_gen, zbody);
    (ParenthesizedZ(zbody), u_gen);
  /* already in hole */
  | LamZP(InHole(TypeInconsistent, _), _, _, _)
  | LamZA(InHole(TypeInconsistent, _), _, _, _)
  | LamZE(InHole(TypeInconsistent, _), _, _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | CaseZR(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | PrjZE(InHole(TypeInconsistent, _), _, _)
  | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _) => (zoperand, u_gen)
  /* not in hole */
  | LamZP(NotInHole | InHole(WrongLength, _), _, _, _)
  | LamZA(NotInHole | InHole(WrongLength, _), _, _, _)
  | LamZE(NotInHole | InHole(WrongLength, _), _, _, _)
  | InjZ(NotInHole | InHole(WrongLength, _), _, _)
  | CaseZE(
      StandardErrStatus(NotInHole | InHole(WrongLength, _)) |
      InconsistentBranches(_, _),
      _,
      _,
    )
  | CaseZR(
      StandardErrStatus(NotInHole | InHole(WrongLength, _)) |
      InconsistentBranches(_, _),
      _,
      _,
    )
  | PrjZE(NotInHole | InHole(WrongLength, _), _, _)
  | ApPaletteZ(NotInHole | InHole(WrongLength, _), _, _, _) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let zoperand =
      zoperand |> set_err_status_zoperand(InHole(TypeInconsistent, u));
    (zoperand, u_gen);
  };
let new_EmptyHole = (u_gen: MetaVarGen.t): (zoperand, MetaVarGen.t) => {
  let (hole, u_gen) = UHExp.new_EmptyHole(u_gen);
  (place_before_operand(hole), u_gen);
};

let empty_zrule = (u_gen: MetaVarGen.t): (zrule, MetaVarGen.t) => {
  let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
  let (clause, u_gen) = UHExp.new_EmptyHole(u_gen);
  let zrule = RuleZP(ZOpSeq.wrap(zp), UHExp.Block.wrap(clause));
  (zrule, u_gen);
};

let is_inconsistent = zoperand =>
  zoperand |> erase_zoperand |> UHExp.is_inconsistent;

let rec move_cursor_left = (ze: t): option(t) =>
  ze |> move_cursor_left_zblock
and move_cursor_left_zblock =
  fun
  | (prefix, zline, suffix) =>
    switch (move_cursor_left_zline(zline)) {
    | Some(zline) => Some((prefix, zline, suffix))
    | None =>
      switch (prefix |> ListUtil.split_last_opt) {
      | None
      | Some(([], EmptyLine)) => None
      | Some((prefix_leading, prefix_last)) =>
        Some((
          prefix_leading,
          prefix_last |> place_after_line,
          [zline |> erase_zline, ...suffix],
        ))
      }
    }
and move_cursor_left_zline = (zline: zline): option(zline) =>
  switch (zline) {
  | _ when is_before_zline(zline) => None

  | CursorL(OnOp(_), _) => None

  | CursorL(OnText(_), EmptyLine) => None
  | CursorL(OnText(0), CommentLine(_) as line) =>
    Some(CursorL(OnDelim(0, After), line))
  | CursorL(OnText(k), CommentLine(_) as line) =>
    Some(CursorL(OnText(k - 1), line))
  | CursorL(OnText(_), ExpLine(_) | LetLine(_) | StructLine(_)) => None

  | CursorL(OnDelim(_), EmptyLine | CommentLine(_) | ExpLine(_)) => None
  | CursorL(OnDelim(k, After), line) =>
    Some(CursorL(OnDelim(k, Before), line))
  | CursorL(OnDelim(k, Before), LetLine(p, ann, def)) =>
    // k == 1 || k == 2 || k == 3
    switch (k, ann) {
    | (1, _) => Some(LetLineZP(ZPat.place_after(p), ann, def))
    | (2, None) => Some(LetLineZP(ZPat.place_after(p), ann, def))
    | (2, Some(ann)) => Some(LetLineZA(p, ZTyp.place_after(ann), def))
    | (_, _) => Some(LetLineZE(p, ann, place_after(def)))
    }
  | CursorL(OnDelim(k, Before), StructLine(p, ann, def)) =>
    switch (k) {
    | 1 => Some(StructLineZP(ZPat.place_after(p), ann, def))
    | _ => Some(StructLineZE(p, ann, place_after(def))) // TODO (hejohns): why?
    }
  | ExpLineZ(zopseq) =>
    switch (move_cursor_left_zopseq(zopseq)) {
    | None => None
    | Some(zopseq) => Some(ExpLineZ(zopseq))
    }
  | LetLineZP(zp, ann, def) =>
    switch (ZPat.move_cursor_left(zp)) {
    | Some(zp) => Some(LetLineZP(zp, ann, def))
    | None =>
      Some(CursorL(OnDelim(0, After), LetLine(ZPat.erase(zp), ann, def)))
    }
  | LetLineZA(p, zann, def) =>
    switch (ZTyp.move_cursor_left(zann)) {
    | Some(zann) => Some(LetLineZA(p, zann, def))
    | None =>
      Some(
        CursorL(
          OnDelim(1, After),
          LetLine(p, Some(ZTyp.erase(zann)), def),
        ),
      )
    }
  | LetLineZE(p, ann, zdef) =>
    switch (move_cursor_left(zdef)) {
    | Some(zdef) => Some(LetLineZE(p, ann, zdef))
    | None =>
      Some(CursorL(OnDelim(2, After), LetLine(p, ann, erase(zdef))))
    }
  | StructLineZP(zp, ann, def) =>
    switch (ZPat.move_cursor_left(zp)) {
    | Some(zp) => Some(StructLineZP(zp, ann, def))
    | None =>
      Some(
        CursorL(OnDelim(0, After), StructLine(ZPat.erase(zp), ann, def)),
      )
    }
  | StructLineZE(p, ann, zdef) =>
    switch (move_cursor_left(zdef)) {
    | Some(zdef) => Some(StructLineZE(p, ann, zdef))
    | None =>
      Some(CursorL(OnDelim(1, After), StructLine(p, ann, erase(zdef))))
    }
  }
and move_cursor_left_zopseq = zopseq =>
  ZOpSeq.move_cursor_left(
    ~move_cursor_left_zoperand,
    ~move_cursor_left_zoperator,
    ~place_after_operand,
    ~place_after_operator,
    ~erase_zoperand,
    ~erase_zoperator,
    zopseq,
  )
and move_cursor_left_zoperator =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(Before), _) => None
  | (OnOp(After), op) => Some((OnOp(Before), op))
and move_cursor_left_zoperand =
  fun
  | z when is_before_zoperand(z) => None
  | CursorE(OnOp(_), _) => None
  | CursorE(OnText(j), e) => Some(CursorE(OnText(j - 1), e))
  | CursorE(OnDelim(k, After), e) => Some(CursorE(OnDelim(k, Before), e))
  | CursorE(OnDelim(_, Before), EmptyHole(_) | ListNil(_)) => None
  | CursorE(OnDelim(_k, Before), Parenthesized(body)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(body)))
  | CursorE(OnDelim(_k, Before), Inj(err, side, body)) =>
    // _k == 1
    Some(InjZ(err, side, place_after(body)))
  | CursorE(OnDelim(k, Before), Lam(err, arg, ann, body)) =>
    // k == 1 || k == 2 || k == 3
    if (k == 1) {
      Some(LamZP(err, ZPat.place_after(arg), ann, body));
    } else if (k == 2) {
      switch (ann) {
      | None => Some(LamZP(err, ZPat.place_after(arg), ann, body))
      | Some(ann) => Some(LamZA(err, arg, ZTyp.place_after(ann), body))
      };
    } else {
      Some(LamZE(err, arg, ann, place_after(body)));
    }
  | CursorE(OnDelim(_k, Before), Case(err, scrut, rules)) =>
    // _k == 1
    switch (List.rev(rules)) {
    | [] => Some(CaseZE(err, place_after(scrut), rules))
    | [last_rule, ...rev_prefix] =>
      Some(
        CaseZR(
          err,
          scrut,
          (List.rev(rev_prefix), place_after_rule(last_rule), []),
        ),
      )
    }
  | CursorE(_, ApPalette(_)) => None
  | CursorE(OnDelim(k, Before), Prj(err, exp, label)) =>
    // ECD TODO: Is the on delim k 0 indexed or 1 indexed?
    if (k == 0) {
      Some(PrjZE(err, place_after_operand(exp), label));
    } else {
      None; // Invalid cursor position
    }
  | CursorE(
      OnDelim(_),
      InvalidText(_, _) | Var(_) | BoolLit(_) | IntLit(_) | FloatLit(_) |
      Label(_),
    ) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zbody) =>
    switch (move_cursor_left(zbody)) {
    | Some(zbody) => Some(ParenthesizedZ(zbody))
    | None =>
      Some(CursorE(OnDelim(0, After), Parenthesized(erase(zbody))))
    }
  | InjZ(err, side, zbody) =>
    switch (move_cursor_left(zbody)) {
    | Some(zbody) => Some(InjZ(err, side, zbody))
    | None =>
      Some(CursorE(OnDelim(0, After), Inj(err, side, erase(zbody))))
    }
  | LamZP(err, zarg, ann, body) =>
    switch (ZPat.move_cursor_left(zarg)) {
    | Some(zarg) => Some(LamZP(err, zarg, ann, body))
    | None =>
      Some(
        CursorE(OnDelim(0, After), Lam(err, ZPat.erase(zarg), ann, body)),
      )
    }
  | LamZA(err, arg, zann, body) =>
    switch (ZTyp.move_cursor_left(zann)) {
    | Some(zann) => Some(LamZA(err, arg, zann, body))
    | None =>
      Some(
        CursorE(
          OnDelim(1, After),
          Lam(err, arg, Some(ZTyp.erase(zann)), body),
        ),
      )
    }
  | LamZE(err, arg, ann, zbody) =>
    switch (move_cursor_left(zbody)) {
    | Some(zbody) => Some(LamZE(err, arg, ann, zbody))
    | None =>
      Some(CursorE(OnDelim(2, After), Lam(err, arg, ann, erase(zbody))))
    }
  | CaseZE(err, zscrut, rules) =>
    switch (move_cursor_left(zscrut)) {
    | Some(zscrut) => Some(CaseZE(err, zscrut, rules))
    | None =>
      Some(CursorE(OnDelim(0, After), Case(err, erase(zscrut), rules)))
    }
  | CaseZR(err, scrut, zrules) =>
    switch (zrules |> move_cursor_left_zrules) {
    | Some(zrules) => Some(CaseZR(err, scrut, zrules))
    | None => Some(CaseZE(err, scrut |> place_after, zrules |> erase_zrules))
    }
  | ApPaletteZ(_, _, _, _) => None
  | PrjZE(err, zexp, label) =>
    switch (move_cursor_left_zoperand(zexp)) {
    | Some(zexp_) => Some(PrjZE(err, zexp_, label))
    | None => None
    }
and move_cursor_left_zrules =
  fun
  | (prefix, zrule, suffix) =>
    switch (move_cursor_left_zrule(zrule)) {
    | Some(zrule) => Some((prefix, zrule, suffix))
    | None =>
      switch (List.rev(prefix)) {
      | [] => None
      | [rule_before, ...rev_prefix] =>
        Some((
          List.rev(rev_prefix),
          place_after_rule(rule_before),
          [erase_zrule(zrule), ...suffix],
        ))
      }
    }
and move_cursor_left_zrule =
  fun
  | z when is_before_zrule(z) => None
  | CursorR(OnOp(_), _) => None
  | CursorR(OnText(_), _) => None
  | CursorR(OnDelim(k, After), rule) =>
    Some(CursorR(OnDelim(k, Before), rule))
  | CursorR(OnDelim(_one, Before), Rule(p, clause)) =>
    Some(RuleZP(ZPat.place_after(p), clause))
  | RuleZP(zp, clause) =>
    switch (ZPat.move_cursor_left(zp)) {
    | Some(zp) => Some(RuleZP(zp, clause))
    | None =>
      Some(CursorR(OnDelim(0, After), Rule(ZPat.erase(zp), clause)))
    }
  | RuleZE(p, zclause) =>
    switch (move_cursor_left(zclause)) {
    | Some(zclause) => Some(RuleZE(p, zclause))
    | None => Some(CursorR(OnDelim(1, After), Rule(p, erase(zclause))))
    };

let rec move_cursor_right = (ze: t): option(t) =>
  ze |> move_cursor_right_zblock
and move_cursor_right_zblock =
  fun
  | (prefix, zline, suffix) =>
    switch (move_cursor_right_zline(zline)) {
    | Some(zline) => Some((prefix, zline, suffix))
    | None =>
      switch (suffix) {
      | [] => None
      | [suffix_first, ...suffix_trailing] =>
        Some((
          prefix @ [erase_zline(zline)],
          place_before_line(suffix_first),
          suffix_trailing,
        ))
      }
    }
and move_cursor_right_zline =
  fun
  | z when is_after_zline(z) => None
  | CursorL(OnOp(_), _) => None
  | CursorL(OnText(k), CommentLine(_) as line) =>
    Some(CursorL(OnText(k + 1), line))
  | CursorL(OnText(_), EmptyLine | ExpLine(_) | LetLine(_) | StructLine(_)) =>
    None
  | CursorL(OnDelim(k, Before), line) =>
    Some(CursorL(OnDelim(k, After), line))

  | CursorL(OnDelim(_, After), CommentLine(_) as line) =>
    Some(CursorL(OnText(0), line))

  | CursorL(OnDelim(_, _), EmptyLine | ExpLine(_)) => None
  | CursorL(OnDelim(k, After), LetLine(p, ann, def)) =>
    switch (k, ann) {
    | (0, _) => Some(LetLineZP(ZPat.place_before(p), ann, def))
    | (1, None) =>
      // invalid cursor position
      None
    | (1, Some(ann)) => Some(LetLineZA(p, ZTyp.place_before(ann), def))
    | (2, _) => Some(LetLineZE(p, ann, place_before(def)))
    | (_, _) => None
    }
  | CursorL(OnDelim(k, After), StructLine(p, ann, def)) =>
    switch (k) {
    | 0 => Some(StructLineZP(ZPat.place_before(p), ann, def))
    | _ => Some(StructLineZE(p, ann, place_before(def))) // TODO (hejohns): why?
    }
  | ExpLineZ(zopseq) =>
    switch (move_cursor_right_zopseq(zopseq)) {
    | None => None
    | Some(zopseq) => Some(ExpLineZ(zopseq))
    }
  | LetLineZP(zp, ann, def) =>
    switch (ZPat.move_cursor_right(zp)) {
    | Some(zp) => Some(LetLineZP(zp, ann, def))
    | None =>
      Some(
        CursorL(
          OnDelim(
            switch (ann) {
            | None => 2
            | Some(_) => 1
            },
            Before,
          ),
          LetLine(ZPat.erase(zp), ann, def),
        ),
      )
    }
  | LetLineZA(p, zann, def) =>
    switch (ZTyp.move_cursor_right(zann)) {
    | Some(zann) => Some(LetLineZA(p, zann, def))
    | None =>
      Some(
        CursorL(
          OnDelim(2, Before),
          LetLine(p, Some(ZTyp.erase(zann)), def),
        ),
      )
    }
  | LetLineZE(p, ann, zdef) =>
    switch (move_cursor_right(zdef)) {
    | Some(zdef) => Some(LetLineZE(p, ann, zdef))
    | None =>
      Some(CursorL(OnDelim(3, Before), LetLine(p, ann, erase(zdef))))
    }
  | StructLineZP(zp, ann, def) =>
    switch (ZPat.move_cursor_right(zp)) {
    | Some(zp) => Some(StructLineZP(zp, ann, def))
    | None =>
      Some(
        CursorL(OnDelim(1, Before), StructLine(ZPat.erase(zp), ann, def)),
      )
    }
  | StructLineZE(p, ann, zdef) =>
    switch (move_cursor_right(zdef)) {
    | Some(zdef) => Some(StructLineZE(p, ann, zdef))
    | None =>
      Some(CursorL(OnDelim(2, Before), StructLine(p, ann, erase(zdef))))
    }
and move_cursor_right_zopseq = zopseq =>
  ZOpSeq.move_cursor_right(
    ~move_cursor_right_zoperand,
    ~move_cursor_right_zoperator,
    ~place_before_operand,
    ~place_before_operator,
    ~erase_zoperand,
    ~erase_zoperator,
    zopseq,
  )
and move_cursor_right_zoperator =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(After), _) => None
  | (OnOp(Before), op) => Some((OnOp(After), op))
and move_cursor_right_zoperand =
  fun
  | z when is_after_zoperand(z) => None
  | CursorE(OnOp(_), _) => None
  | CursorE(OnText(j), e) => Some(CursorE(OnText(j + 1), e))
  | CursorE(OnDelim(k, Before), e) => Some(CursorE(OnDelim(k, After), e))
  | CursorE(OnDelim(_, After), EmptyHole(_) | ListNil(_)) => None
  | CursorE(OnDelim(_k, After), Parenthesized(body)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(body)))
  | CursorE(OnDelim(_k, After), Inj(err, side, body)) =>
    // _k == 0
    Some(InjZ(err, side, place_before(body)))
  | CursorE(OnDelim(k, After), Lam(err, arg, ann, body)) =>
    // k == 0 || k == 1 || k == 2
    switch (k == 0, k == 1, ann) {
    | (true, _, _) => Some(LamZP(err, ZPat.place_before(arg), ann, body))
    | (_, true, None) =>
      // invalid cursor position
      None
    | (_, true, Some(ann)) =>
      Some(LamZA(err, arg, ZTyp.place_before(ann), body))
    | (false, false, _) => Some(LamZE(err, arg, ann, place_before(body)))
    }
  | CursorE(OnDelim(_k, After), Case(err, scrut, rules)) =>
    // _k == 0
    Some(CaseZE(err, place_before(scrut), rules))
  | CursorE(OnDelim(k, After), Prj(err, exp, label)) =>
    if (k == 0) {
      Some(CursorE(OnText(0), Prj(err, exp, label)));
    } else {
      None;
    }
  | CursorE(_, ApPalette(_)) => None
  | CursorE(
      OnDelim(_),
      InvalidText(_, _) | Var(_) | BoolLit(_) | IntLit(_) | FloatLit(_) |
      Label(_),
    ) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zbody) =>
    switch (move_cursor_right(zbody)) {
    | Some(zbody) => Some(ParenthesizedZ(zbody))
    | None =>
      Some(CursorE(OnDelim(1, Before), Parenthesized(erase(zbody))))
    }
  | InjZ(err, side, zbody) =>
    switch (move_cursor_right(zbody)) {
    | Some(zbody) => Some(InjZ(err, side, zbody))
    | None =>
      Some(CursorE(OnDelim(1, Before), Inj(err, side, erase(zbody))))
    }
  | LamZP(err, zarg, ann, body) =>
    switch (ZPat.move_cursor_right(zarg)) {
    | Some(zarg) => Some(LamZP(err, zarg, ann, body))
    | None =>
      Some(
        CursorE(
          OnDelim(
            switch (ann) {
            | None => 2
            | Some(_) => 1
            },
            Before,
          ),
          Lam(err, ZPat.erase(zarg), ann, body),
        ),
      )
    }
  | LamZA(err, arg, zann, body) =>
    switch (ZTyp.move_cursor_right(zann)) {
    | Some(zann) => Some(LamZA(err, arg, zann, body))
    | None =>
      Some(
        CursorE(
          OnDelim(2, Before),
          Lam(err, arg, Some(ZTyp.erase(zann)), body),
        ),
      )
    }
  | LamZE(err, arg, ann, zbody) =>
    switch (move_cursor_right(zbody)) {
    | None =>
      Some(CursorE(OnDelim(3, Before), Lam(err, arg, ann, erase(zbody))))
    | Some(zbody) => Some(LamZE(err, arg, ann, zbody))
    }
  | CaseZE(err, zscrut, rules) =>
    switch (move_cursor_right(zscrut)) {
    | Some(zscrut) => Some(CaseZE(err, zscrut, rules))
    | None =>
      switch (rules) {
      | [] =>
        Some(CursorE(OnDelim(1, Before), Case(err, erase(zscrut), rules)))
      | [r, ...rs] =>
        Some(CaseZR(err, erase(zscrut), ([], place_before_rule(r), rs)))
      }
    }
  | CaseZR(err, scrut, zrules) =>
    switch (zrules |> move_cursor_right_zrules) {
    | Some(zrules) => Some(CaseZR(err, scrut, zrules))
    | None =>
      Some(
        CursorE(
          OnDelim(1, Before),
          Case(err, scrut, zrules |> erase_zrules),
        ),
      )
    }
  | ApPaletteZ(_, _, _, _) => None
  | PrjZE(err, zexp, label) =>
    switch (move_cursor_right_zoperand(zexp)) {
    | Some(zexp_) => Some(PrjZE(err, zexp_, label))
    | None =>
      Some(
        CursorE(OnDelim(0, Before), Prj(err, erase_zoperand(zexp), label)),
      )
    }
and move_cursor_right_zrules =
  fun
  | (prefix, zrule, suffix) =>
    switch (move_cursor_right_zrule(zrule)) {
    | Some(zrule) => Some((prefix, zrule, suffix))
    | None =>
      switch (suffix) {
      | [] => None
      | [rule_after, ...new_suffix] =>
        Some((
          prefix @ [erase_zrule(zrule)],
          place_before_rule(rule_after),
          new_suffix,
        ))
      }
    }
and move_cursor_right_zrule =
  fun
  | z when is_after_zrule(z) => None
  | CursorR(OnOp(_), _) => None
  | CursorR(OnText(_), _) => None
  | CursorR(OnDelim(k, Before), rule) =>
    Some(CursorR(OnDelim(k, After), rule))
  | CursorR(OnDelim(k, After), Rule(p, clause)) =>
    // k == 0 || k == 1
    k == 0
      ? Some(RuleZP(ZPat.place_before(p), clause))
      : Some(RuleZE(p, place_before(clause)))
  | RuleZP(zp, clause) =>
    switch (ZPat.move_cursor_right(zp)) {
    | Some(zp) => Some(RuleZP(zp, clause))
    | None =>
      Some(CursorR(OnDelim(1, Before), Rule(ZPat.erase(zp), clause)))
    }
  | RuleZE(p, zclause) =>
    switch (move_cursor_right(zclause)) {
    | None => None
    | Some(zclause) => Some(RuleZE(p, zclause))
    };

let rec cursor_on_EmptyHole = ze => cursor_on_EmptyHole_zblock(ze)
and cursor_on_EmptyHole_zblock = ((_, zline, _)) =>
  cursor_on_EmptyHole_zline(zline)
and cursor_on_EmptyHole_zline =
  fun
  | CursorL(_) => None
  | ExpLineZ(zopseq) => cursor_on_EmptyHole_zopseq(zopseq)
  | LetLineZP(_)
  | LetLineZA(_) => None
  | LetLineZE(_, _, ze) => cursor_on_EmptyHole(ze)
  | StructLineZP(_) => None
  | StructLineZE(_, _, ze) => cursor_on_EmptyHole(ze)
and cursor_on_EmptyHole_zopseq =
  fun
  | ZOpSeq(_, ZOperator(_)) => None
  | ZOpSeq(_, ZOperand(zoperand, _)) =>
    cursor_on_EmptyHole_zoperand(zoperand)
and cursor_on_EmptyHole_zoperand =
  fun
  | CursorE(_, EmptyHole(u)) => Some(u)
  | CursorE(_)
  | LamZP(_)
  | LamZA(_) => None
  | LamZE(_, _, _, ze)
  | ParenthesizedZ(ze)
  | InjZ(_, _, ze)
  | CaseZE(_, ze, _) => cursor_on_EmptyHole(ze)
  | ApPaletteZ(_) => failwith("unimplemented")
  | CaseZR(_, _, (_, zrule, _)) => cursor_on_EmptyHole_zrule(zrule)
  | PrjZE(_, CursorE(_, EmptyHole(u)), _) => Some(u)
  | PrjZE(_, _, _) => None
and cursor_on_EmptyHole_zrule =
  fun
  | CursorR(_)
  | RuleZP(_) => None
  | RuleZE(_, ze) => cursor_on_EmptyHole(ze);
