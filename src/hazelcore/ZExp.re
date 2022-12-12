open Sexplib.Std;

[@deriving sexp]
type t = zblock
and zblock = ZList.t(zline, UHExp.line)
and zline =
  | CursorL(CursorPosition.t, UHExp.line)
  | ExpLineZ(zopseq)
  | LetLineZP(ZPat.t, UHExp.t)
  | LetLineZE(UHPat.t, t)
and zopseq = ZOpSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator)
and zoperand =
  | CursorE(CursorPosition.t, UHExp.operand)
  | ParenthesizedZ(t)
  | FunZP(ErrStatus.t, ZPat.t, UHExp.t)
  | FunZE(ErrStatus.t, UHPat.t, t)
  | InjZ(ErrStatus.t, InjSide.t, t)
  | CaseZE(CaseErrStatus.t, t, list(UHExp.rule))
  | CaseZR(CaseErrStatus.t, UHExp.t, zrules)
and zoperator = (CursorPosition.t, UHExp.operator)
and zrules = ZList.t(zrule, UHExp.rule)
and zrule =
  | CursorR(CursorPosition.t, UHExp.rule)
  | RuleZP(ZPat.t, UHExp.t)
  | RuleZE(UHPat.t, t);

type operand_surround = Seq.operand_surround(UHExp.operand, UHExp.operator);
type operator_surround = Seq.operator_surround(UHExp.operand, UHExp.operator);
type zseq = ZSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator);

let line_can_be_swapped = (line: zline): bool =>
  switch (line) {
  | CursorL(_)
  | LetLineZP(_)
  | ExpLineZ(ZOpSeq(_, ZOperator(_)))
  | ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(FunZP(_), _))) => true
  | LetLineZE(_)
  | ExpLineZ(ZOpSeq(_, ZOperand(FunZE(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(InjZ(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(CaseZE(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(CaseZR(_), _)))
  | ExpLineZ(ZOpSeq(_, ZOperand(ParenthesizedZ(_), _))) => false
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
  | LetLine(_) =>
    CursorPosition.delim_cursors_k(0)
    @ CursorPosition.delim_cursors_k(1)
    @ CursorPosition.delim_cursors_k(2)
  };
let valid_cursors_operator: UHExp.operator => list(CursorPosition.t) =
  fun
  | _ => [OnOp(Before), OnOp(After)];
let valid_cursors_operand: UHExp.operand => list(CursorPosition.t) =
  fun /* outer nodes - delimiter */
  | EmptyHole(_)
  | ListNil(_) => CursorPosition.delim_cursors(1) /* outer nodes - text */
  | InvalidText(_, t) => CursorPosition.text_cursors(String.length(t))
  | Var(_, _, x) => CursorPosition.text_cursors(Var.length(x))
  | IntLit(_, n) => CursorPosition.text_cursors(String.length(n))
  | FloatLit(_, f) => CursorPosition.text_cursors(String.length(f))
  | BoolLit(_, b) => CursorPosition.text_cursors(b ? 4 : 5) /* inner nodes */
  | Fun(_, _, _) => {
      CursorPosition.delim_cursors_k(0)
      @ CursorPosition.delim_cursors_k(1)
      @ CursorPosition.delim_cursors_k(2);
    }
  | Inj(_) => CursorPosition.delim_cursors(2)
  | Case(_) => CursorPosition.delim_cursors(2)
  | Parenthesized(_) => CursorPosition.delim_cursors(2);
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
    | LetLineZE(_) => failwith("force_get_zopseq: expected ExpLineZ")
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
  | CursorL(cursor, LetLine(_)) => cursor == OnDelim(0, Before)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | ExpLineZ(zopseq) => is_before_zopseq(zopseq)
  | LetLineZP(_)
  | LetLineZE(_) => false
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
  | CursorE(cursor, BoolLit(_)) => cursor == OnText(0)
  | CursorE(cursor, Fun(_))
  | CursorE(cursor, Inj(_))
  | CursorE(cursor, Case(_))
  | CursorE(cursor, Parenthesized(_)) => cursor == OnDelim(0, Before)
  | ParenthesizedZ(_)
  | FunZP(_)
  | FunZE(_)
  | InjZ(_)
  | CaseZE(_)
  | CaseZR(_) =>
    false /* For example:              # Comment1              #| Comment2         */;

// The following 2 functions are specifically for CommentLines!!
// Check if the cursor at "OnDelim(After)" in a "CommentLine"

let is_begin_of_comment = ((prefix, zline, _): zblock): bool =>
  switch (zline) {
  | CursorL(cursor, CommentLine(_)) =>
    switch (prefix |> ListUtil.split_last_opt) {
    | Some((_, CommentLine(_))) => cursor == OnDelim(0, After)
    | _ => false
    }
  | _ => false
  } /* For example:
           # Comment1|
           # Comment2

   */;

// Check if the cursor at the end of a "CommentLine"
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
  | CursorL(cursor, LetLine(_)) => cursor == OnDelim(2, After)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | ExpLineZ(zopseq) => is_after_zopseq(zopseq)
  | LetLineZP(_)
  | LetLineZE(_) => false
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
  | CursorE(cursor, Fun(_)) => cursor == OnDelim(2, After)
  | CursorE(cursor, Case(_)) => cursor == OnDelim(1, After)
  | CursorE(cursor, Inj(_)) => cursor == OnDelim(1, After)
  | CursorE(cursor, Parenthesized(_)) => cursor == OnDelim(1, After)
  | ParenthesizedZ(_) => false
  | FunZP(_)
  | FunZE(_)
  | InjZ(_)
  | CaseZE(_)
  | CaseZR(_) => false;
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
  | CursorL(_, LetLine(_)) => true
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  | ExpLineZ(zopseq) => is_outer_zopseq(zopseq)
  | LetLineZP(_)
  | LetLineZE(_) => false
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
  | CursorE(_, Fun(_))
  | CursorE(_, Inj(_))
  | CursorE(_, Case(_))
  | CursorE(_, Parenthesized(_)) => true
  | ParenthesizedZ(zexp) => is_outer(zexp)
  | FunZP(_)
  | FunZE(_)
  | InjZ(_)
  | CaseZE(_)
  | CaseZR(_) => false;

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
  | LetLine(_) as line => CursorL(OnDelim(0, Before), line)
  | ExpLine(opseq) => ExpLineZ(place_before_opseq(opseq))
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
  | BoolLit(_) => CursorE(OnText(0), operand)
  | Fun(_)
  | Inj(_)
  | Case(_)
  | Parenthesized(_) => CursorE(OnDelim(0, Before), operand)
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
  | LetLine(_) as line => CursorL(OnDelim(2, After), line)
  | ExpLine(e) => ExpLineZ(place_after_opseq(e))
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
  | Fun(_) => CursorE(OnDelim(2, After), operand)
  | Case(_) => CursorE(OnDelim(1, After), operand)
  | Inj(_) => CursorE(OnDelim(1, After), operand)
  | Parenthesized(_) => CursorE(OnDelim(1, After), operand)
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
  | LetLine(_) =>
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
  | LetLineZE(_)
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

let is_before_empty_hole_line = (zli: zline): bool =>
  switch (zli) {
  | ExpLineZ(
      ZOpSeq(
        _,
        ZOperand(CursorE(OnDelim(0, Before), EmptyHole(_)), (E, E)),
      ),
    ) =>
    true
  | _ => false
  };

let rec erase = (ze: t): UHExp.t => ze |> erase_zblock
and erase_zblock = ((prefix, zline, suffix): zblock): UHExp.block =>
  prefix @ [zline |> erase_zline] @ suffix
and erase_zline =
  fun
  | CursorL(_, line) => line
  | ExpLineZ(zopseq) => ExpLine(erase_zopseq(zopseq))
  | LetLineZP(zp, def) => LetLine(ZPat.erase(zp), def)
  | LetLineZE(p, zdef) => LetLine(p, erase(zdef))
and erase_zopseq = zopseq =>
  ZOpSeq.erase(~erase_zoperand, ~erase_zoperator, zopseq)
and erase_zoperator =
  fun
  | (_, operator) => operator
and erase_zoperand =
  fun
  | CursorE(_, operand) => operand
  | ParenthesizedZ(zbody) => Parenthesized(erase(zbody))
  | FunZP(err, zp, body) => Fun(err, ZPat.erase(zp), body)
  | FunZE(err, p, zbody) => Fun(err, p, erase(zbody))
  | InjZ(err, side, zbody) => Inj(err, side, erase(zbody))
  | CaseZE(err, zscrut, rules) => Case(err, erase(zscrut), rules)
  | CaseZR(err, scrut, zrules) => Case(err, scrut, erase_zrules(zrules))
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
  | FunZP(_, zp, body) => FunZP(err, zp, body)
  | FunZE(_, p, zbody) => FunZE(err, p, zbody)
  | InjZ(_, inj_side, zbody) => InjZ(err, inj_side, zbody)
  | CaseZE(_, zscrut, rules) =>
    CaseZE(StandardErrStatus(err), zscrut, rules)
  | CaseZR(_, scrut, zrules) =>
    CaseZR(StandardErrStatus(err), scrut, zrules)
  };

let rec mk_inconsistent = (id_gen: IDGen.t, ze: t): (t, IDGen.t) =>
  ze |> mk_inconsistent_zblock(id_gen)
and mk_inconsistent_zblock =
    (id_gen: IDGen.t, (prefix, zline, suffix): zblock): (zblock, IDGen.t) =>
  switch (suffix |> ListUtil.split_last_opt) {
  | None =>
    let (zconclusion, id_gen) =
      zline |> ZLine.force_get_zopseq |> mk_inconsistent_zopseq(id_gen);
    ((prefix, ExpLineZ(zconclusion), []), id_gen);
  | Some((suffix_leading, suffix_last)) =>
    let (conclusion, id_gen) =
      suffix_last
      |> UHExp.Line.force_get_opseq
      |> UHExp.mk_inconsistent_opseq(id_gen);
    ((prefix, zline, suffix_leading @ [ExpLine(conclusion)]), id_gen);
  }
and mk_inconsistent_zopseq = (id_gen, zopseq) =>
  ZOpSeq.mk_inconsistent(~mk_inconsistent_zoperand, id_gen, zopseq)
and mk_inconsistent_zoperand = (id_gen, zoperand) =>
  switch (zoperand) {
  | CursorE(cursor, operand) =>
    let (operand, id_gen) = operand |> UHExp.mk_inconsistent_operand(id_gen);
    (CursorE(cursor, operand), id_gen);
  | ParenthesizedZ(zbody) =>
    let (zbody, id_gen) = mk_inconsistent(id_gen, zbody);
    (ParenthesizedZ(zbody), id_gen /* already in hole */);
  | FunZP(InHole(TypeInconsistent, _), _, _)
  | FunZE(InHole(TypeInconsistent, _), _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | CaseZR(StandardErrStatus(InHole(TypeInconsistent, _)), _, _) => (
      zoperand,
      id_gen,
    ) /* not in hole */
  | FunZP(NotInHole | InHole(WrongLength, _), _, _)
  | FunZE(NotInHole | InHole(WrongLength, _), _, _)
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
    ) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    let zoperand =
      zoperand |> set_err_status_zoperand(InHole(TypeInconsistent, u));
    (zoperand, id_gen);
  };
let new_EmptyHole = (id_gen: IDGen.t): (zoperand, IDGen.t) => {
  let (hole, id_gen) = UHExp.new_EmptyHole(id_gen);
  (place_before_operand(hole), id_gen);
};

let empty_zrule = (id_gen: IDGen.t): (zrule, IDGen.t) => {
  let (zp, id_gen) = ZPat.new_EmptyHole(id_gen);
  let (clause, id_gen) = UHExp.new_EmptyHole(id_gen);
  let zrule = RuleZP(ZOpSeq.wrap(zp), UHExp.Block.wrap(clause));
  (zrule, id_gen);
};

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
  | CursorL(OnText(_), ExpLine(_) | LetLine(_)) => None

  | CursorL(OnDelim(_), EmptyLine | CommentLine(_) | ExpLine(_)) => None
  | CursorL(OnDelim(k, After), line) =>
    Some(CursorL(OnDelim(k, Before), line))
  | CursorL(OnDelim(k, Before), LetLine(p, def)) =>
    switch (k) {
    | 1 => Some(LetLineZP(ZPat.place_after(p), def))
    | _ => Some(LetLineZE(p, place_after(def)))
    }
  | ExpLineZ(zopseq) =>
    switch (move_cursor_left_zopseq(zopseq)) {
    | None => None
    | Some(zopseq) => Some(ExpLineZ(zopseq))
    }
  | LetLineZP(zp, def) =>
    switch (ZPat.move_cursor_left(zp)) {
    | Some(zp) => Some(LetLineZP(zp, def))
    | None =>
      Some(CursorL(OnDelim(0, After), LetLine(ZPat.erase(zp), def)))
    }
  | LetLineZE(p, zdef) =>
    switch (move_cursor_left(zdef)) {
    | Some(zdef) => Some(LetLineZE(p, zdef))
    | None => Some(CursorL(OnDelim(1, After), LetLine(p, erase(zdef))))
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
  | CursorE(OnDelim(k, Before), Fun(err, arg, body)) =>
    switch (k) {
    | 1 => Some(FunZP(err, ZPat.place_after(arg), body))
    | 2 => Some(FunZE(err, arg, place_after(body)))
    | _ => None
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
  | CursorE(
      OnDelim(_),
      InvalidText(_, _) | Var(_) | BoolLit(_) | IntLit(_) | FloatLit(_),
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
  | FunZP(err, zarg, body) =>
    switch (ZPat.move_cursor_left(zarg)) {
    | Some(zarg) => Some(FunZP(err, zarg, body))
    | None =>
      Some(CursorE(OnDelim(0, After), Fun(err, ZPat.erase(zarg), body)))
    }
  | FunZE(err, arg, zbody) =>
    switch (move_cursor_left(zbody)) {
    | Some(zbody) => Some(FunZE(err, arg, zbody))
    | None =>
      Some(CursorE(OnDelim(1, After), Fun(err, arg, erase(zbody))))
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
  | CursorL(OnText(_), EmptyLine | ExpLine(_) | LetLine(_)) => None
  | CursorL(OnDelim(k, Before), line) =>
    Some(CursorL(OnDelim(k, After), line))

  | CursorL(OnDelim(_, After), CommentLine(_) as line) =>
    Some(CursorL(OnText(0), line))

  | CursorL(OnDelim(_, _), EmptyLine | ExpLine(_)) => None
  | CursorL(OnDelim(k, After), LetLine(p, def)) =>
    switch (k) {
    | 0 => Some(LetLineZP(ZPat.place_before(p), def))
    | 1 => Some(LetLineZE(p, place_before(def)))
    | _ => None
    }
  | ExpLineZ(zopseq) =>
    switch (move_cursor_right_zopseq(zopseq)) {
    | None => None
    | Some(zopseq) => Some(ExpLineZ(zopseq))
    }
  | LetLineZP(zp, def) =>
    switch (ZPat.move_cursor_right(zp)) {
    | Some(zp) => Some(LetLineZP(zp, def))
    | None =>
      Some(CursorL(OnDelim(1, Before), LetLine(ZPat.erase(zp), def)))
    }
  | LetLineZE(p, zdef) =>
    switch (move_cursor_right(zdef)) {
    | Some(zdef) => Some(LetLineZE(p, zdef))
    | None => Some(CursorL(OnDelim(2, Before), LetLine(p, erase(zdef))))
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
  | CursorE(OnDelim(k, After), Fun(err, arg, body)) =>
    switch (k) {
    | 0 => Some(FunZP(err, ZPat.place_before(arg), body))
    | 1 => Some(FunZE(err, arg, place_before(body)))
    | _ => None // invalid cursor position
    }
  | CursorE(OnDelim(_k, After), Case(err, scrut, rules)) =>
    // _k == 0
    Some(CaseZE(err, place_before(scrut), rules))
  | CursorE(
      OnDelim(_),
      InvalidText(_, _) | Var(_) | BoolLit(_) | IntLit(_) | FloatLit(_),
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
  | FunZP(err, zarg, body) =>
    switch (ZPat.move_cursor_right(zarg)) {
    | Some(zarg) => Some(FunZP(err, zarg, body))
    | None =>
      Some(CursorE(OnDelim(1, Before), Fun(err, ZPat.erase(zarg), body)))
    }
  | FunZE(err, arg, zbody) =>
    switch (move_cursor_right(zbody)) {
    | None =>
      Some(CursorE(OnDelim(2, Before), Fun(err, arg, erase(zbody))))
    | Some(zbody) => Some(FunZE(err, arg, zbody))
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
  | LetLineZP(_) => None
  | LetLineZE(_, ze) => cursor_on_EmptyHole(ze)
and cursor_on_EmptyHole_zopseq =
  fun
  | ZOpSeq(_, ZOperator(_)) => None
  | ZOpSeq(_, ZOperand(zoperand, _)) =>
    cursor_on_EmptyHole_zoperand(zoperand)
and cursor_on_EmptyHole_zoperand =
  fun
  | CursorE(_, EmptyHole(u)) => Some(u)
  | CursorE(_)
  | FunZP(_) => None
  | FunZE(_, _, ze)
  | ParenthesizedZ(ze)
  | InjZ(_, _, ze)
  | CaseZE(_, ze, _) => cursor_on_EmptyHole(ze)
  | CaseZR(_, _, (_, zrule, _)) => cursor_on_EmptyHole_zrule(zrule)
and cursor_on_EmptyHole_zrule =
  fun
  | CursorR(_)
  | RuleZP(_) => None
  | RuleZE(_, ze) => cursor_on_EmptyHole(ze);

let zline_is_just_empty_hole = (zline: zline): bool =>
  switch (zline) {
  | ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_, EmptyHole(_)), (E, E)))) =>
    true
  | _ => false
  };
