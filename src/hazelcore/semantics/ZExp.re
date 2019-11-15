open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type t = zblock
and zblock =
  | BlockZL(zlines, UHExp.opseq)
  | BlockZE(UHExp.lines, zopseq)
and zlines = ZList.t(zline, UHExp.line)
and zline =
  | CursorL(CursorPosition.t, UHExp.line)
  | ExpLineZ(zopseq)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.t)
  | LetLineZE(UHPat.t, option(UHTyp.t), t)
and zopseq = ZOpSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator)
and zoperand =
  | CursorE(CursorPosition.t, UHExp.operand)
  | ParenthesizedZ(t)
  | LamZP(ErrStatus.t, ZPat.t, option(UHTyp.t), UHExp.t)
  | LamZA(ErrStatus.t, UHPat.t, ZTyp.t, UHExp.t)
  | LamZE(ErrStatus.t, UHPat.t, option(UHTyp.t), t)
  | InjZ(ErrStatus.t, InjSide.t, t)
  | CaseZE(ErrStatus.t, t, list(UHExp.rule), option(UHTyp.t))
  | CaseZR(ErrStatus.t, UHExp.t, zrules, option(UHTyp.t))
  | CaseZA(ErrStatus.t, UHExp.t, list(UHExp.rule), ZTyp.t)
  | ApPaletteZ(
      ErrStatus.t,
      PaletteName.t,
      SerializedModel.t,
      ZSpliceInfo.t(UHExp.t, t),
    )
and zoperator = (Side.t, UHExp.operator)
and zrules = ZList.t(zrule, UHExp.rule)
and zrule =
  | CursorR(CursorPosition.t, UHExp.rule)
  | RuleZP(ZPat.t, UHExp.t)
  | RuleZE(UHPat.t, t);

let valid_cursors_line = (line: UHExp.line): list(CursorPosition.t) =>
  switch (line) {
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
  };
let valid_cursors_operand = (e: UHExp.operand): list(CursorPosition.t) =>
  switch (e) {
  /* outer nodes - delimiter */
  | EmptyHole(_)
  | ListNil(_) => CursorPosition.delim_cursors(1)
  /* outer nodes - text */
  | Var(_, _, x) => CursorPosition.text_cursors(Var.length(x))
  | NumLit(_, n) => CursorPosition.text_cursors(num_digits(n))
  | BoolLit(_, b) => CursorPosition.text_cursors(b ? 4 : 5)
  /* inner nodes */
  | Lam(_, _, ann, _) =>
    let colon_positions =
      switch (ann) {
      | Some(_) => CursorPosition.delim_cursors_k(1)
      | None => []
      };
    CursorPosition.delim_cursors_k(0)
    @ colon_positions
    @ CursorPosition.delim_cursors_k(2);
  | Inj(_, _, _) => CursorPosition.delim_cursors(2)
  | Case(_, _, _, _) => CursorPosition.delim_cursors(2)
  | Parenthesized(_) => CursorPosition.delim_cursors(2)
  | ApPalette(_, _, _, _) => CursorPosition.delim_cursors(1) /* TODO[livelits] */
  };
let valid_cursors_rule = (_: UHExp.rule): list(CursorPosition.t) =>
  CursorPosition.delim_cursors(2);

let is_valid_cursor_line = (cursor: CursorPosition.t, line: UHExp.line): bool =>
  valid_cursors_line(line) |> contains(cursor);
let is_valid_cursor_operand =
    (cursor: CursorPosition.t, operand: UHExp.operand): bool =>
  valid_cursors_operand(operand) |> contains(cursor);
let is_valid_cursor_rule = (cursor: CursorPosition.t, rule: UHExp.rule): bool =>
  valid_cursors_rule(rule) |> contains(cursor);

let wrap_in_block = (zconclusion: zopseq): zblock =>
  BlockZE([], zconclusion);

let parenthesize = (zbody: zoperand): zoperand =>
  ParenthesizedZ(zbody |> ZOpSeq.wrap |> wrap_in_block);

let bidelimit = (zoperand: zoperand): zoperand =>
  switch (zoperand) {
  | CursorE(_, operand) =>
    if (UHExp.bidelimited(operand)) {
      zoperand;
    } else {
      parenthesize(zoperand);
    }
  /* bidelimited */
  | ParenthesizedZ(_)
  | InjZ(_, _, _)
  | ApPaletteZ(_, _, _, _) => zoperand
  /* not bidelimited */
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _)
  | CaseZA(_, _, _, _)
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | LamZE(_, _, _, _) => parenthesize(zoperand)
  };

exception InconsistentOpSeq;

let rec is_before = (ze: t): bool => is_before_zblock(ze)
and is_before_zblock = (zblock: zblock): bool =>
  switch (zblock) {
  | BlockZL(zleading, _) => is_before_zlines(zleading)
  | BlockZE([], zconclusion) => is_before_zopseq(zconclusion)
  | BlockZE(_, _) => false
  }
and is_before_zlines = ((prefix, zline, _): zlines): bool =>
  switch (prefix) {
  | [] => is_before_zline(zline)
  | _ => false
  }
and is_before_zline = (zline: zline): bool =>
  switch (zline) {
  | CursorL(cursor, EmptyLine) => cursor == OnText(0)
  | CursorL(cursor, LetLine(_, _, _)) => cursor == OnDelim(0, Before)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | ExpLineZ(zopseq) => is_before_zopseq(zopseq)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _) => false
  }
and is_before_zopseq = zopseq => ZOpSeq.is_before(~is_before_zoperand, zopseq)
and is_before_zoperand =
  fun
  | CursorE(cursor, EmptyHole(_))
  | CursorE(cursor, ListNil(_)) => cursor == OnDelim(0, Before)
  | CursorE(cursor, Var(_, _, _))
  | CursorE(cursor, NumLit(_, _))
  | CursorE(cursor, BoolLit(_, _)) => cursor == OnText(0)
  | CursorE(cursor, Lam(_, _, _, _))
  | CursorE(cursor, Inj(_, _, _))
  | CursorE(cursor, Case(_, _, _, _))
  | CursorE(cursor, Parenthesized(_)) => cursor == OnDelim(0, Before)
  | CursorE(cursor, ApPalette(_, _, _, _)) => cursor == OnDelim(0, Before) /* TODO[livelits] */
  | ParenthesizedZ(_) => false
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | LamZE(_, _, _, _) => false
  | InjZ(_, _, _) => false
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _)
  | CaseZA(_, _, _, _) => false
  | ApPaletteZ(_, _, _, _) => false;
let is_before_zrule =
  fun
  | CursorR(OnDelim(0, Before), _) => true
  | _ => false;

let rec is_after = ze => is_after_zblock(ze)
and is_after_zblock =
  fun
  | BlockZL(_, _) => false
  | BlockZE(_, zopseq) => is_after_zopseq(zopseq)
and is_after_zline =
  fun
  | CursorL(cursor, EmptyLine) => cursor == OnText(0)
  | CursorL(cursor, LetLine(_, _, _)) => cursor == OnDelim(3, After)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | ExpLineZ(zopseq) => is_after_zopseq(zopseq)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _) => false
and is_after_zopseq = zopseq => ZOpSeq.is_after(~is_after_zoperand, zopseq)
and is_after_zoperand =
  fun
  | CursorE(cursor, EmptyHole(_))
  | CursorE(cursor, ListNil(_)) => cursor == OnDelim(0, After)
  | CursorE(cursor, Var(_, _, x)) => cursor == OnText(Var.length(x))
  | CursorE(cursor, NumLit(_, n)) => cursor == OnText(num_digits(n))
  | CursorE(cursor, BoolLit(_, true)) => cursor == OnText(4)
  | CursorE(cursor, BoolLit(_, false)) => cursor == OnText(5)
  | CursorE(_, Lam(_, _, _, _)) => false
  | CursorE(_, Case(_, _, _, Some(_))) => false
  | CursorE(cursor, Case(_, _, _, None)) => cursor == OnDelim(1, After)
  | CursorE(cursor, Inj(_, _, _)) => cursor == OnDelim(1, After)
  | CursorE(cursor, Parenthesized(_)) => cursor == OnDelim(1, After)
  | CursorE(_, ApPalette(_, _, _, _)) => false /* TODO[livelits] */
  | ParenthesizedZ(_) => false
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _) => false
  | LamZE(_, _, _, zbody) => is_after(zbody)
  | InjZ(_, _, _) => false
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _) => false
  | CaseZA(_, _, _, zann) => ZTyp.is_after(zann)
  | ApPaletteZ(_, _, _, _) => false;
let is_after_zlines = ((_, zline, suffix): zlines): bool =>
  switch (suffix) {
  | [] => is_after_zline(zline)
  | _ => false
  };
let is_after_zrule =
  fun
  | RuleZE(_, zclause) => is_after(zclause)
  | _ => false;

let rec is_after_case_rule = ze => is_after_case_rule_zblock(ze)
and is_after_case_rule_zblock =
  fun
  | BlockZL((_, zline, _), _) => zline |> is_after_case_rule_line
  | BlockZE(_, zconclusion) => zconclusion |> is_after_case_rule_zopseq
and is_after_case_rule_line =
  fun
  | CursorL(_, _) => false
  | ExpLineZ(zopseq) => is_after_case_rule_zopseq(zopseq)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => false
  | LetLineZE(_, _, zdef) => zdef |> is_after_case_rule
and is_after_case_rule_zopseq =
  fun
  | ZOperator(_, _, _) => false
  | ZOperand(_, zoperand, _) => is_after_case_rule_zoperand(zoperand)
and is_after_case_rule_zoperand =
  fun
  | CaseZR(_, _, (_, RuleZE(_, zclause), _), _) => is_after(zclause)
  | ParenthesizedZ(ze)
  | InjZ(_, _, ze)
  | LamZE(_, _, _, ze)
  | CaseZE(_, ze, _, _) => ze |> is_after_case_rule
  | CursorE(_, _)
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | CaseZR(_, _, (_, CursorR(_, _), _), _)
  | CaseZR(_, _, (_, RuleZP(_, _), _), _)
  | CaseZA(_, _, _, _)
  | ApPaletteZ(_, _, _, _) => false;

let rec is_on_user_newlineable_hole = (~is_root=true, ze) =>
  is_on_user_newlineable_hole_zblock(~is_root, ze)
and is_on_user_newlineable_hole_zblock = (~is_root=true, zblock) =>
  switch (zblock) {
  | BlockZL((_, zline, _), _) => zline |> is_on_user_newlineable_hole_zline
  | BlockZE(leading, zconclusion) =>
    switch (leading |> split_last, zconclusion) {
    | (None, ZOperand(_, CursorE(_, EmptyHole(_)), _)) => !is_root
    | (
        Some((_, LetLine(_, _, _))),
        ZOperand(_, CursorE(_, EmptyHole(_)), _),
      ) =>
      true
    | (_, _) => zconclusion |> is_on_user_newlineable_hole_zopseq
    }
  }
and is_on_user_newlineable_hole_zline =
  fun
  | CursorL(_, _) => false
  | ExpLineZ(zopseq) => zopseq |> is_on_user_newlineable_hole_zopseq
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => false
  | LetLineZE(_, _, zdef) => zdef |> is_on_user_newlineable_hole
and is_on_user_newlineable_hole_zopseq =
  fun
  | ZOperator(_, _, _) => false
  | ZOperand(_, zoperand, _) =>
    is_on_user_newlineable_hole_zoperand(zoperand)
and is_on_user_newlineable_hole_zoperand =
  fun
  | CursorE(_, _) => false
  | ParenthesizedZ(zblock)
  | LamZE(_, _, _, zblock)
  | InjZ(_, _, zblock)
  | CaseZE(_, zblock, _, _)
  | CaseZR(_, _, (_, RuleZE(_, zblock), _), _) =>
    zblock |> is_on_user_newlineable_hole
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | CaseZR(_, _, (_, CursorR(_, _) | RuleZP(_, _), _), _)
  | CaseZA(_, _, _, _)
  | ApPaletteZ(_, _, _, _) => false;

let rec place_before = (e: UHExp.t): t => place_before_block(e)
and place_before_block = (block: UHExp.block): zblock =>
  switch (block) {
  | Block([], conclusion) => BlockZE([], place_before_opseq(conclusion))
  | Block([line, ...lines], conclusion) =>
    let zline = place_before_line(line);
    let zlines = ([], zline, lines);
    BlockZL(zlines, conclusion);
  }
and place_before_line = (line: UHExp.line): zline =>
  switch (line) {
  | EmptyLine => CursorL(OnText(0), EmptyLine)
  | LetLine(_, _, _) => CursorL(OnDelim(0, Before), line)
  | ExpLine(opseq) => ExpLineZ(place_before_opseq(opseq))
  }
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand = operand =>
  switch (operand) {
  | EmptyHole(_)
  | ListNil(_) => CursorE(OnDelim(0, Before), operand)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _) => CursorE(OnText(0), operand)
  | Lam(_, _, _, _)
  | Inj(_, _, _)
  | Case(_, _, _, _)
  | Parenthesized(_) => CursorE(OnDelim(0, Before), operand)
  | ApPalette(_, _, _, _) => CursorE(OnDelim(0, Before), operand) /* TODO[livelits] */
  };
let place_before_lines = (lines: UHExp.lines): option(zlines) =>
  switch (lines) {
  | [] => None
  | [line, ...lines] => Some(([], place_before_line(line), lines))
  };
let place_before_rule = (rule: UHExp.rule): zrule =>
  CursorR(OnDelim(0, Before), rule);
let place_before_operator = (op: UHExp.operator): zoperator => (Before, op);

let rec place_after = (e: UHExp.t): t => place_after_block(e)
and place_after_block = (Block(leading, conclusion): UHExp.block): zblock =>
  BlockZE(leading, place_after_opseq(conclusion))
and place_after_line = (line: UHExp.line): zline =>
  switch (line) {
  | EmptyLine => CursorL(OnText(0), line)
  | LetLine(_, _, _) => CursorL(OnDelim(3, After), line)
  | ExpLine(e) => ExpLineZ(place_after_opseq(e))
  }
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand = operand =>
  switch (operand) {
  | EmptyHole(_)
  | ListNil(_) => CursorE(OnDelim(0, After), operand)
  | Var(_, _, x) => CursorE(OnText(Var.length(x)), operand)
  | NumLit(_, n) => CursorE(OnText(num_digits(n)), operand)
  | BoolLit(_, true) => CursorE(OnText(4), operand)
  | BoolLit(_, false) => CursorE(OnText(5), operand)
  | Lam(err, p, ann, body) => LamZE(err, p, ann, place_after(body))
  | Case(err, block, rules, Some(uty)) =>
    CaseZA(err, block, rules, ZTyp.place_after(uty))
  | Case(_, _, _, None) => CursorE(OnDelim(1, After), operand)
  | Inj(_, _, _) => CursorE(OnDelim(1, After), operand)
  | Parenthesized(_) => CursorE(OnDelim(1, After), operand)
  | ApPalette(_, _, _, _) => CursorE(OnDelim(0, After), operand) /* TODO[livelits] */
  };
let place_after_lines = (lines: UHExp.lines): option(zlines) =>
  switch (split_last(lines)) {
  | None => None
  | Some((prefix, last_line)) =>
    Some((prefix, place_after_line(last_line), []))
  };
let place_after_rule = (Rule(p, clause): UHExp.rule): zrule =>
  RuleZE(p, place_after(clause));
let place_after_operator = (op: UHExp.operator): zoperator => (After, op);

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
  | LetLine(_, _, _) =>
    is_valid_cursor_line(cursor, line) ? Some(CursorL(cursor, line)) : None
  };
let place_cursor_rule =
    (cursor: CursorPosition.t, rule: UHExp.rule): option(zrule) =>
  is_valid_cursor_rule(cursor, rule) ? Some(CursorR(cursor, rule)) : None;

let prune_empty_hole_line = (zli: zline): zline =>
  switch (zli) {
  | ExpLineZ(ZOperand(_, CursorE(_, EmptyHole(_)), (E, E))) =>
    place_before_line(EmptyLine)
  | ExpLineZ(_)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _)
  | CursorL(_) => zli
  };
let prune_empty_hole_lines = ((prefix, zline, suffix): zlines): zlines => (
  UHExp.prune_empty_hole_lines(prefix),
  prune_empty_hole_line(zline),
  UHExp.prune_empty_hole_lines(suffix),
);

let rec set_err_status = (err: ErrStatus.t, ze: t): t =>
  set_err_status_zblock(err, ze)
and set_err_status_zblock = (err: ErrStatus.t, zblock: zblock): zblock =>
  switch (zblock) {
  | BlockZL(zlines, conclusion) =>
    BlockZL(zlines, UHExp.set_err_status_opseq(err, conclusion))
  | BlockZE(lines, zconclusion) =>
    BlockZE(lines, set_err_status_zopseq(err, zconclusion))
  }
and set_err_status_zopseq = (err, zopseq) =>
  ZOpSeq.set_err_status(~set_err_status_zoperand, err, zopseq)
and set_err_status_zoperand = (err, zoperand) =>
  switch (zoperand) {
  | CursorE(cursor, operand) =>
    CursorE(cursor, UHExp.set_err_status_operand(err, operand))
  | ParenthesizedZ(zbody) =>
    ParenthesizedZ(set_err_status_zblock(err, zbody))
  | LamZP(_, zp, ann, body) => LamZP(err, zp, ann, body)
  | LamZA(_, p, zann, body) => LamZA(err, p, zann, body)
  | LamZE(_, p, ann, zbody) => LamZE(err, p, ann, zbody)
  | InjZ(_, inj_side, zbody) => InjZ(err, inj_side, zbody)
  | CaseZE(_, zscrut, rules, ann) => CaseZE(err, zscrut, rules, ann)
  | CaseZR(_, scrut, zrules, ann) => CaseZR(err, scrut, zrules, ann)
  | CaseZA(_, scrut, rules, zann) => CaseZA(err, scrut, rules, zann)
  | ApPaletteZ(_, name, model, psi) => ApPaletteZ(err, name, model, psi)
  };

let rec make_inconsistent = (u_gen: MetaVarGen.t, ze: t): (t, MetaVarGen.t) =>
  make_inconsistent_zblock(u_gen, ze)
and make_inconsistent_zblock =
    (u_gen: MetaVarGen.t, zblock: zblock): (zblock, MetaVarGen.t) =>
  switch (zblock) {
  | BlockZL(zlines, conclusion) =>
    let (conclusion, u_gen) =
      conclusion |> UHExp.make_inconsistent_opseq(u_gen);
    (BlockZL(zlines, conclusion), u_gen);
  | BlockZE(lines, zconclusion) =>
    let (zconclusion, u_gen) =
      zconclusion |> make_inconsistent_zopseq(u_gen);
    (BlockZE(lines, zconclusion), u_gen);
  }
and make_inconsistent_zopseq = (u_gen, zopseq) =>
  ZOpSeq.make_inconsistent(~make_inconsistent_zoperand, u_gen, zopseq)
and make_inconsistent_zoperand = (u_gen, zoperand) =>
  switch (zoperand) {
  | CursorE(cursor, operand) =>
    let (operand, u_gen) = operand |> UHExp.make_inconsistent_operand(u_gen);
    (CursorE(cursor, operand), u_gen);
  | ParenthesizedZ(zbody) =>
    let (zbody, u_gen) = make_inconsistent(u_gen, zbody);
    (ParenthesizedZ(zbody), u_gen);
  /* already in hole */
  | LamZP(InHole(TypeInconsistent, _), _, _, _)
  | LamZA(InHole(TypeInconsistent, _), _, _, _)
  | LamZE(InHole(TypeInconsistent, _), _, _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(InHole(TypeInconsistent, _), _, _, _)
  | CaseZR(InHole(TypeInconsistent, _), _, _, _)
  | CaseZA(InHole(TypeInconsistent, _), _, _, _)
  | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _) => (zoperand, u_gen)
  /* not in hole */
  | LamZP(NotInHole | InHole(WrongLength, _), _, _, _)
  | LamZA(NotInHole | InHole(WrongLength, _), _, _, _)
  | LamZE(NotInHole | InHole(WrongLength, _), _, _, _)
  | InjZ(NotInHole | InHole(WrongLength, _), _, _)
  | CaseZE(NotInHole | InHole(WrongLength, _), _, _, _)
  | CaseZR(NotInHole | InHole(WrongLength, _), _, _, _)
  | CaseZA(NotInHole | InHole(WrongLength, _), _, _, _)
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

let rec cursor_on_outer_expr =
        (zoperand: zoperand): option((UHExp.t, CursorPosition.t)) =>
  switch (zoperand) {
  | CursorE(cursor, operand) =>
    Some((UHExp.drop_outer_parentheses(operand), cursor))
  | ParenthesizedZ(BlockZE([], ZOperand(_, zoperand, _))) =>
    cursor_on_outer_expr(zoperand)
  | ParenthesizedZ(_)
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | LamZE(_, _, _, _)
  | InjZ(_, _, _)
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _)
  | CaseZA(_, _, _, _)
  | ApPaletteZ(_, _, _, _) => None
  };

let empty_zrule = (u_gen: MetaVarGen.t): (zrule, MetaVarGen.t) => {
  let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
  let (clause, u_gen) = UHExp.new_EmptyHole(u_gen);
  let zrule =
    RuleZP(zp |> ZOpSeq.wrap, clause |> OpSeq.wrap |> UHExp.wrap_in_block);
  (zrule, u_gen);
};

let rec erase = (ze: t): UHExp.t => erase_zblock(ze)
and erase_zblock = (zblock: zblock): UHExp.block =>
  switch (zblock) {
  | BlockZL(zleading, conclusion) =>
    Block(erase_zlines(zleading), conclusion)
  | BlockZE(leading, zconclusion) =>
    Block(leading, erase_zopseq(zconclusion))
  }
and erase_zlines = (zlis: zlines): UHExp.lines =>
  ZList.erase(zlis, erase_zline)
and erase_zline = (zline: zline): UHExp.line =>
  switch (zline) {
  | CursorL(_, line) => line
  | ExpLineZ(zopseq) => ExpLine(erase_zopseq(zopseq))
  | LetLineZP(zp, ann, block) => LetLine(ZPat.erase(zp), ann, block)
  | LetLineZA(p, zann, block) => LetLine(p, Some(ZTyp.erase(zann)), block)
  | LetLineZE(p, ann, zblock) => LetLine(p, ann, erase_zblock(zblock))
  }
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
  | CaseZE(err, zscrut, rules, ann) => Case(err, erase(zscrut), rules, ann)
  | CaseZR(err, scrut, zrules, ann) =>
    Case(err, scrut, ZList.erase(zrules, erase_zrule), ann)
  | CaseZA(err, scrut, rules, zann) =>
    Case(err, scrut, rules, Some(ZTyp.erase(zann)))
  | ApPaletteZ(err, palette_name, serialized_model, zpsi) => {
      let psi =
        ZSpliceInfo.erase(zpsi, ((ty, z)) => (ty, erase_zblock(z)));
      ApPalette(err, palette_name, serialized_model, psi);
    }
and erase_zrule =
  fun
  | CursorR(_, rule) => rule
  | RuleZP(zp, clause) => Rule(ZPat.erase(zp), clause)
  | RuleZE(p, zclause) => Rule(p, erase(zclause));

let is_inconsistent = zoperand =>
  zoperand |> erase_zoperand |> UHExp.is_inconsistent;

let zblock_to_zlines = (zblock: zblock): zlines =>
  switch (zblock) {
  | BlockZL((prefix, zline, suffix), e) => (
      prefix,
      zline,
      suffix @ [ExpLine(e)],
    )
  | BlockZE(lines, ze) => (lines, ExpLineZ(ze), [])
  };

let rec move_cursor_left = (ze: t): option(t) => move_cursor_left_zblock(ze)
and move_cursor_left_zblock = (zblock: zblock): option(zblock) =>
  switch (zblock) {
  | BlockZL((prefix, zline, suffix), e) =>
    switch (move_cursor_left_zline(zline)) {
    | Some(zline) => Some(BlockZL((prefix, zline, suffix), e))
    | None =>
      switch (List.rev(prefix)) {
      | [] => None
      | [line_before, ...rev_prefix] =>
        Some(
          BlockZL(
            (
              List.rev(rev_prefix),
              place_after_line(line_before),
              [erase_zline(zline), ...suffix],
            ),
            e,
          ),
        )
      }
    }
  | BlockZE(leading, zconclusion) =>
    switch (move_cursor_left_zopseq(zconclusion)) {
    | Some(zconclusion) => Some(BlockZE(leading, zconclusion))
    | None =>
      switch (List.rev(leading)) {
      | [] => None
      | [last_line, ...rev_prefix] =>
        Some(
          BlockZL(
            (List.rev(rev_prefix), place_after_line(last_line), []),
            erase_zopseq(zconclusion),
          ),
        )
      }
    }
  }
and move_cursor_left_zline = (zline: zline): option(zline) =>
  switch (zline) {
  | _ when is_before_zline(zline) => None
  | CursorL(Staging(_), _) => None
  | CursorL(OnText(_), _) => None
  | CursorL(OnDelim(k, After), line) =>
    Some(CursorL(OnDelim(k, Before), line))
  | CursorL(OnDelim(_, _), EmptyLine | ExpLine(_)) => None
  | CursorL(OnDelim(k, Before), LetLine(p, ann, def)) =>
    // k == 1 || k == 2 || k == 3
    switch (k == 1, k == 2, ann) {
    | (true, _, _) => Some(LetLineZP(ZPat.place_after(p), ann, def))
    | (_, true, None) => Some(LetLineZP(ZPat.place_after(p), ann, def))
    | (_, true, Some(ann)) =>
      Some(LetLineZA(p, ZTyp.place_after(ann), def))
    | (_, _, _) => Some(LetLineZE(p, ann, place_after(def)))
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
    switch (move_cursor_left_zblock(zdef)) {
    | Some(zdef) => Some(LetLineZE(p, ann, zdef))
    | None =>
      Some(
        CursorL(OnDelim(2, After), LetLine(p, ann, erase_zblock(zdef))),
      )
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
  | (After, op) => Some((Before, op))
  | (Before, _) => None
and move_cursor_left_zoperand =
  fun
  | z when is_before_zoperand(z) => None
  | CursorE(Staging(_), _) => None
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
    // k == 1 || k == 2
    switch (k == 1, ann) {
    | (true, _) => Some(LamZP(err, ZPat.place_after(arg), ann, body))
    | (_, None) => Some(LamZP(err, ZPat.place_after(arg), ann, body))
    | (_, Some(ann)) => Some(LamZA(err, arg, ZTyp.place_after(ann), body))
    }
  | CursorE(OnDelim(_k, Before), Case(err, scrut, rules, ann)) =>
    // _k == 1
    switch (List.rev(rules)) {
    | [] => Some(CaseZE(err, place_after(scrut), rules, ann))
    | [last_rule, ...rev_prefix] =>
      Some(
        CaseZR(
          err,
          scrut,
          (List.rev(rev_prefix), place_after_rule(last_rule), []),
          ann,
        ),
      )
    }
  | CursorE(_, ApPalette(_, _, _, _)) => None
  | CursorE(OnDelim(_, _), Var(_, _, _) | BoolLit(_, _) | NumLit(_, _)) =>
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
      Some(
        CursorE(
          OnDelim(2, After),
          Lam(err, arg, ann, erase_zblock(zbody)),
        ),
      )
    }
  | CaseZE(err, zscrut, rules, ann) =>
    switch (move_cursor_left(zscrut)) {
    | Some(zscrut) => Some(CaseZE(err, zscrut, rules, ann))
    | None =>
      Some(
        CursorE(
          OnDelim(0, After),
          Case(err, erase_zblock(zscrut), rules, ann),
        ),
      )
    }
  | CaseZR(err, scrut, (prefix, zrule, suffix), ann) =>
    switch (move_cursor_left_zrule(zrule)) {
    | Some(zrule) => Some(CaseZR(err, scrut, (prefix, zrule, suffix), ann))
    | None =>
      switch (List.rev(prefix)) {
      | [] =>
        Some(
          CaseZE(
            err,
            place_after(scrut),
            prefix @ [erase_zrule(zrule)] @ suffix,
            ann,
          ),
        )
      | [rule_before, ...rev_prefix] =>
        Some(
          CaseZR(
            err,
            scrut,
            (
              List.rev(rev_prefix),
              place_after_rule(rule_before),
              [erase_zrule(zrule), ...suffix],
            ),
            ann,
          ),
        )
      }
    }
  | CaseZA(err, scrut, rules, zann) =>
    switch (ZTyp.move_cursor_left(zann)) {
    | Some(zann) => Some(CaseZA(err, scrut, rules, zann))
    | None =>
      Some(
        CursorE(
          OnDelim(1, After),
          Case(err, scrut, rules, Some(ZTyp.erase(zann))),
        ),
      )
    }
  | ApPaletteZ(_, _, _, _) => None
and move_cursor_left_zrule =
  fun
  | z when is_before_zrule(z) => None
  | CursorR(Staging(_), _) => None
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
    | None =>
      Some(CursorR(OnDelim(1, After), Rule(p, erase_zblock(zclause))))
    };

let rec move_cursor_right = (ze: t): option(t) =>
  move_cursor_right_zblock(ze)
and move_cursor_right_zblock =
  fun
  | BlockZL((prefix, zline, suffix), conclusion) =>
    switch (move_cursor_right_zline(zline)) {
    | Some(zline) => Some(BlockZL((prefix, zline, suffix), conclusion))
    | None =>
      switch (suffix) {
      | [] =>
        Some(
          BlockZE(
            prefix @ [erase_zline(zline)] @ suffix,
            place_before_opseq(conclusion),
          ),
        )
      | [line_after, ...suffix] =>
        Some(
          BlockZL(
            (
              prefix @ [erase_zline(zline)],
              place_before_line(line_after),
              suffix,
            ),
            conclusion,
          ),
        )
      }
    }
  | BlockZE(leading, zconclusion) =>
    switch (move_cursor_right_zopseq(zconclusion)) {
    | None => None
    | Some(zconclusion) => Some(BlockZE(leading, zconclusion))
    }
and move_cursor_right_zline =
  fun
  | z when is_after_zline(z) => None
  | CursorL(Staging(_), _) => None
  | CursorL(OnText(_), _) => None
  | CursorL(OnDelim(k, Before), line) =>
    Some(CursorL(OnDelim(k, After), line))
  | CursorL(OnDelim(_, _), EmptyLine | ExpLine(_)) => None
  | CursorL(OnDelim(k, After), LetLine(p, ann, def)) =>
    // k == 0 || k == 1 || k == 2 || k == 3
    switch (k == 0, k == 1, k == 2, ann) {
    | (true, _, _, _) => Some(LetLineZP(ZPat.place_before(p), ann, def))
    | (_, true, _, None) =>
      // invalid cursor position
      None
    | (_, true, _, Some(ann)) =>
      Some(LetLineZA(p, ZTyp.place_before(ann), def))
    | (_, _, true, _) => Some(LetLineZE(p, ann, place_before(def)))
    | (_, _, _, _) => None
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
  | (Before, op) => Some((After, op))
  | (After, _) => None
and move_cursor_right_zoperand =
  fun
  | z when is_after_zoperand(z) => None
  | CursorE(Staging(_), _) => None
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
  | CursorE(OnDelim(_k, After), Case(err, scrut, rules, None)) =>
    // _k == 0
    Some(CaseZE(err, place_before(scrut), rules, None))
  | CursorE(OnDelim(k, After), Case(err, scrut, rules, Some(ann))) =>
    // k == 0 || k == 1
    k == 0
      ? Some(CaseZE(err, place_before(scrut), rules, Some(ann)))
      : Some(CaseZA(err, scrut, rules, ZTyp.place_before(ann)))
  | CursorE(_, ApPalette(_, _, _, _)) => None
  | CursorE(OnDelim(_, _), Var(_, _, _) | BoolLit(_, _) | NumLit(_, _)) =>
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
    | None => None
    | Some(zbody) => Some(LamZE(err, arg, ann, zbody))
    }
  | CaseZE(err, zscrut, rules, ann) =>
    switch (move_cursor_right(zscrut)) {
    | Some(zscrut) => Some(CaseZE(err, zscrut, rules, ann))
    | None =>
      switch (rules) {
      | [] =>
        Some(
          CursorE(
            OnDelim(1, Before),
            Case(err, erase(zscrut), rules, ann),
          ),
        )
      | [r, ...rs] =>
        Some(
          CaseZR(err, erase(zscrut), ([], place_before_rule(r), rs), ann),
        )
      }
    }
  | CaseZR(err, scrut, (prefix, zrule, suffix), ann) =>
    switch (move_cursor_right_zrule(zrule)) {
    | Some(zrule) => Some(CaseZR(err, scrut, (prefix, zrule, suffix), ann))
    | None =>
      switch (suffix) {
      | [] =>
        Some(
          CursorE(
            OnDelim(1, Before),
            Case(err, scrut, prefix @ [erase_zrule(zrule)], ann),
          ),
        )
      | [rule_after, ...new_suffix] =>
        Some(
          CaseZR(
            err,
            scrut,
            (
              prefix @ [erase_zrule(zrule)],
              place_before_rule(rule_after),
              new_suffix,
            ),
            ann,
          ),
        )
      }
    }
  | CaseZA(err, scrut, rules, zann) =>
    switch (ZTyp.move_cursor_right(zann)) {
    | None => None
    | Some(zann) => Some(CaseZA(err, scrut, rules, zann))
    }
  | ApPaletteZ(_, _, _, _) => None
and move_cursor_right_zrule =
  fun
  | z when is_after_zrule(z) => None
  | CursorR(Staging(_), _) => None
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

type zexp_or_zblock =
  | E(t)
  | B(zblock);

let has_concluding_let_line = zblock =>
  zblock |> erase_zblock |> UHExp.has_concluding_let_line;

let is_multi_line = zblock => zblock |> erase_zblock |> UHExp.is_multi_line;
