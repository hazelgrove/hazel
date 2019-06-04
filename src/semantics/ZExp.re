open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

[@deriving sexp]
type opseq_surround = OperatorSeq.opseq_surround(UHExp.t, UHExp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHExp.t, UHExp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHExp.t, UHExp.op);

[@deriving sexp]
type zblock =
  | BlockZL(zlines, UHExp.t)
  | BlockZE(UHExp.lines, t)
and zlines = ZList.t(zline, UHExp.line)
and zline =
  | CursorL(cursor_pos, UHExp.line)
  | ExpLineZ(t)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.block)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.block)
  | LetLineZE(UHPat.t, option(UHTyp.t), zblock)
and t =
  | CursorE(cursor_pos, UHExp.t)
  | ParenthesizedZ(zblock)
  | OpSeqZ(UHExp.skel_t, t, opseq_surround)
  | LamZP(err_status, ZPat.t, option(UHTyp.t), UHExp.block)
  | LamZA(err_status, UHPat.t, ZTyp.t, UHExp.block)
  | LamZE(err_status, UHPat.t, option(UHTyp.t), zblock)
  | InjZ(err_status, inj_side, zblock)
  | CaseZE(err_status, zblock, list(UHExp.rule), option(UHTyp.t))
  | CaseZR(err_status, UHExp.block, zrules, option(UHTyp.t))
  | CaseZA(err_status, UHExp.block, list(UHExp.rule), ZTyp.t)
  | ApPaletteZ(
      err_status,
      PaletteName.t,
      SerializedModel.t,
      ZSpliceInfo.t(UHExp.block, zblock),
    )
/* | CursorPalette : PaletteName.t -> PaletteSerializedModel.t -> hole_ref -> t -> t */
and zrules = ZList.t(zrule, UHExp.rule)
and zrule =
  | CursorR(cursor_pos, UHExp.rule)
  | RuleZP(ZPat.t, UHExp.block)
  | RuleZE(UHPat.t, zblock);

let valid_cursors_line = (line: UHExp.line): list(cursor_pos) =>
  switch (line) {
  | ExpLine(_) => []
  | EmptyLine => [outer_cursor(0)]
  | LetLine(_, _, _) => [
      inner_cursor(0, Before), /* |let _ = _ */
      inner_cursor(0, After), /* let| _ = _ */
      inner_cursor(1, Before), /* let _ |= _ */
      inner_cursor(1, After) /* let _ =| _ */
    ]
  };
let valid_cursors_exp = (e: UHExp.t): list(cursor_pos) =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(_) => outer_cursors(1)
  | Var(_, _, x) => outer_cursors(Var.length(x))
  | NumLit(_, n) => outer_cursors(num_digits(n))
  | BoolLit(_, b) => outer_cursors(b ? 4 : 5)
  | ListNil(_) => outer_cursors(2)
  /* inner nodes */
  | Lam(_, _, ann, _) =>
    let colon_positions =
      switch (ann) {
      | Some(_) => inner_cursors_k(1)
      | None => []
      };
    inner_cursors_k(0) @ colon_positions @ inner_cursors_k(2);
  | Inj(_, _, _) => inner_cursors(2)
  | Case(_, _, _, _) => inner_cursors(2)
  | Parenthesized(_) => inner_cursors(2)
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.map(k => k + 1)
    |> List.map(k => inner_cursors_k(k))
    |> List.flatten
  | ApPalette(_, _, _, _) => inner_cursors(1) /* TODO */
  };
let valid_cursors_rule = (_: UHExp.rule): list(cursor_pos) =>
  inner_cursors(2);

let is_valid_cursor_line = (cursor: cursor_pos, line: UHExp.line): bool =>
  contains(valid_cursors_line(line), cursor);
let is_valid_cursor_exp = (cursor: cursor_pos, e: UHExp.t): bool =>
  contains(valid_cursors_exp(e), cursor);
let is_valid_cursor_rule = (cursor: cursor_pos, rule: UHExp.rule): bool =>
  contains(valid_cursors_rule(rule), cursor);

let wrap_in_block = (ze: t): zblock => BlockZE([], ze);

let parenthesize = (ze: t): t => ParenthesizedZ(wrap_in_block(ze));

let bidelimit = (ze: t): t =>
  switch (ze) {
  | CursorE(_, e) =>
    if (UHExp.bidelimited(e)) {
      ze;
    } else {
      parenthesize(ze);
    }
  /* bidelimited */
  | ParenthesizedZ(_)
  | InjZ(_, _, _)
  | ApPaletteZ(_, _, _, _) => ze
  /* not bidelimited */
  | OpSeqZ(_, _, _)
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _)
  | CaseZA(_, _, _, _)
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | LamZE(_, _, _, _) => parenthesize(ze)
  };

exception SkelInconsistentWithOpSeq;

let rec is_before_block = (zblock: zblock): bool =>
  switch (zblock) {
  | BlockZL(zlines, _) => is_before_lines(zlines)
  | BlockZE([], ze) => is_before_exp(ze)
  | BlockZE(_, _) => false
  }
and is_before_lines = ((prefix, zline, _): zlines): bool =>
  switch (prefix) {
  | [] => is_before_line(zline)
  | _ => false
  }
and is_before_line = (zline: zline): bool =>
  switch (zline) {
  /* outer nodes */
  | CursorL(cursor, EmptyLine) => cursor == outer_cursor(0)
  /* inner nodes */
  | CursorL(cursor, LetLine(_, _, _)) => cursor == outer_cursor(0)
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  /* zipper cases */
  | ExpLineZ(ze) => is_before_exp(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _) => false
  }
and is_before_exp = (ze: t): bool =>
  switch (ze) {
  /* outer nodes */
  | CursorE(cursor, EmptyHole(_))
  | CursorE(cursor, Var(_, _, _))
  | CursorE(cursor, NumLit(_, _))
  | CursorE(cursor, BoolLit(_, _))
  | CursorE(cursor, ListNil(_)) => cursor == outer_cursor(0)
  /* inner nodes */
  | CursorE(cursor, Lam(_, _, _, _))
  | CursorE(cursor, Inj(_, _, _))
  | CursorE(cursor, Case(_, _, _, _))
  | CursorE(cursor, Parenthesized(_)) => cursor == inner_cursor(0, Before)
  | CursorE(_, OpSeq(_, _)) => false
  | CursorE(cursor, ApPalette(_, _, _, _)) =>
    cursor == inner_cursor(0, Before) /* TODO */
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, ze1, EmptyPrefix(_)) => is_before_exp(ze1)
  | OpSeqZ(_, _, _) => false
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _)
  | LamZE(_, _, _, _) => false
  | InjZ(_, _, _) => false
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _)
  | CaseZA(_, _, _, _) => false
  | ApPaletteZ(_, _, _, _) => false
  };

let rec is_after_block = (zblock: zblock): bool =>
  switch (zblock) {
  | BlockZL(_, _) => false
  | BlockZE(_, ze) => is_after_exp(ze)
  }
and is_after_line = (zli: zline): bool =>
  switch (zli) {
  /* outer nodes */
  | CursorL(cursor, EmptyLine) => cursor == outer_cursor(0)
  /* inner nodes */
  | CursorL(_, LetLine(_, _, _)) => false
  | CursorL(_, ExpLine(_)) => false /* ghost node */
  /* zipper cases */
  | ExpLineZ(ze) => is_after_exp(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => false
  | LetLineZE(_, _, zblock) => is_after_block(zblock)
  }
and is_after_exp = (ze: t): bool =>
  switch (ze) {
  /* outer nodes */
  | CursorE(cursor, EmptyHole(_)) => cursor == outer_cursor(1)
  | CursorE(cursor, Var(_, _, x)) => cursor == outer_cursor(Var.length(x))
  | CursorE(cursor, NumLit(_, n)) => cursor == outer_cursor(num_digits(n))
  | CursorE(cursor, BoolLit(_, true)) => cursor == outer_cursor(4)
  | CursorE(cursor, BoolLit(_, false)) => cursor == outer_cursor(5)
  | CursorE(cursor, ListNil(_)) => cursor == outer_cursor(2)
  /* inner nodes */
  | CursorE(_, Lam(_, _, _, _)) => false
  | CursorE(_, Case(_, _, _, Some(_))) => false
  | CursorE(cursor, Case(_, _, _, None)) => cursor == inner_cursor(1, After)
  | CursorE(cursor, Inj(_, _, _)) => cursor == inner_cursor(1, After)
  | CursorE(cursor, Parenthesized(_)) => cursor == inner_cursor(1, After)
  | CursorE(_, OpSeq(_, _)) => false
  | CursorE(_, ApPalette(_, _, _, _)) => false /* TODO */
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, ze1, EmptySuffix(_)) => is_after_exp(ze1)
  | OpSeqZ(_, _, _) => false
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _) => false
  | LamZE(_, _, _, zblock) => is_after_block(zblock)
  | InjZ(_, _, _) => false
  | CaseZE(_, _, _, _)
  | CaseZR(_, _, _, _) => false
  | CaseZA(_, _, _, zann) => ZTyp.is_after(zann)
  | ApPaletteZ(_, _, _, _) => false
  };
let is_after_lines = ((_, zline, suffix): zlines): bool =>
  switch (suffix) {
  | [] => is_after_line(zline)
  | _ => false
  };

let rec place_before_block = (block: UHExp.block): zblock =>
  switch (block) {
  | Block([], e) => BlockZE([], place_before_exp(e))
  | Block([line, ...lines], e) =>
    let zline = place_before_line(line);
    let zlines = ([], zline, lines);
    BlockZL(zlines, e);
  }
and place_before_line = (line: UHExp.line): zline =>
  switch (line) {
  | EmptyLine => CursorL(outer_cursor(0), EmptyLine)
  | LetLine(_, _, _) => CursorL((0, Before), line)
  | ExpLine(e) => ExpLineZ(place_before_exp(e))
  }
and place_before_exp = (e: UHExp.t): t =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => CursorE(outer_cursor(0), e)
  /* inner nodes */
  | Lam(_, _, _, _)
  | Inj(_, _, _)
  | Case(_, _, _, _)
  | Parenthesized(_) => CursorE(inner_cursor(0, Before), e)
  | OpSeq(skel, seq) =>
    let (e1, suffix) = OperatorSeq.split0(seq);
    let ze1 = place_before_exp(e1);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    OpSeqZ(skel, ze1, surround);
  | ApPalette(_, _, _, _) => CursorE(inner_cursor(0, Before), e) /* TODO */
  };
let place_before_lines = (lines: UHExp.lines): option(zlines) =>
  switch (lines) {
  | [] => None
  | [line, ...lines] => Some(([], place_before_line(line), lines))
  };
let place_before_rule = (rule: UHExp.rule): zrule =>
  CursorR(inner_cursor(0, Before), rule);

let rec place_after_block = (Block(lines, e): UHExp.block): zblock =>
  BlockZE(lines, place_after_exp(e))
and place_after_line = (line: UHExp.line): zline =>
  switch (line) {
  | EmptyLine => CursorL(outer_cursor(0), EmptyLine)
  | LetLine(p, ann, block) => LetLineZE(p, ann, place_after_block(block))
  | ExpLine(e) => ExpLineZ(place_after_exp(e))
  }
and place_after_exp = (e: UHExp.t): t =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(_) => CursorE(outer_cursor(1), e)
  | Var(_, _, x) => CursorE(outer_cursor(Var.length(x)), e)
  | NumLit(_, n) => CursorE(outer_cursor(num_digits(n)), e)
  | BoolLit(_, true) => CursorE(outer_cursor(4), e)
  | BoolLit(_, false) => CursorE(outer_cursor(5), e)
  | ListNil(_) => CursorE(outer_cursor(2), e)
  /* inner nodes */
  | Lam(err, p, ann, block) => LamZE(err, p, ann, place_after_block(block))
  | Case(err, block, rules, Some(uty)) =>
    CaseZA(err, block, rules, ZTyp.place_after(uty))
  | Case(_, _, _, None) => CursorE((1, After), e)
  | Inj(_, _, _) => CursorE((1, After), e)
  | Parenthesized(_) => CursorE((1, After), e)
  | OpSeq(skel, seq) =>
    let (e1, prefix) = OperatorSeq.split_tail(seq);
    let ze1 = place_after_exp(e1);
    let surround = OperatorSeq.EmptySuffix(prefix);
    OpSeqZ(skel, ze1, surround);
  | ApPalette(_, _, _, _) => CursorE((0, After), e) /* TODO */
  };
let place_after_lines = (lines: UHExp.lines): option(zlines) =>
  switch (split_last(lines)) {
  | None => None
  | Some((prefix, last_line)) =>
    Some((prefix, place_after_line(last_line), []))
  };
let place_after_rule = (Rule(p, block): UHExp.rule): zrule =>
  RuleZE(p, place_after_block(block));

let place_cursor_exp = (cursor: cursor_pos, e: UHExp.t): option(t) =>
  is_valid_cursor_exp(cursor, e) ? Some(CursorE(cursor, e)) : None;
let place_cursor_line = (cursor: cursor_pos, line: UHExp.line): option(zline) =>
  switch (line) {
  | ExpLine(e) =>
    switch (place_cursor_exp(cursor, e)) {
    | None => None
    | Some(ze) => Some(ExpLineZ(ze))
    }
  | EmptyLine
  | LetLine(_, _, _) =>
    is_valid_cursor_line(cursor, line) ? Some(CursorL(cursor, line)) : None
  };
let place_cursor_rule = (cursor: cursor_pos, rule: UHExp.rule): option(zrule) =>
  is_valid_cursor_rule(cursor, rule) ? Some(CursorR(cursor, rule)) : None;

let prune_empty_hole_line = (zli: zline): zline =>
  switch (zli) {
  | ExpLineZ(CursorE(_, EmptyHole(_))) => place_before_line(EmptyLine)
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

let rec get_err_status_block = (zblock: zblock): err_status =>
  switch (zblock) {
  | BlockZL(_, e) => UHExp.get_err_status_t(e)
  | BlockZE(_, ze) => get_err_status_t(ze)
  }
and get_err_status_t = (ze: t): err_status =>
  switch (ze) {
  | CursorE(_, e) => UHExp.get_err_status_t(e)
  | ParenthesizedZ(zblock) => get_err_status_block(zblock)
  | OpSeqZ(skel, ze_n, surround) =>
    get_err_status_opseq(skel, ze_n, surround)
  | LamZP(err, _, _, _)
  | LamZA(err, _, _, _)
  | LamZE(err, _, _, _)
  | InjZ(err, _, _)
  | CaseZE(err, _, _, _)
  | CaseZR(err, _, _, _)
  | CaseZA(err, _, _, _)
  | ApPaletteZ(err, _, _, _) => err
  }
and get_err_status_opseq =
    (skel: UHExp.skel_t, ze_n: t, surround: opseq_surround): err_status =>
  switch (skel) {
  | Placeholder(m) =>
    if (m === OperatorSeq.surround_prefix_length(surround)) {
      get_err_status_t(ze_n);
    } else {
      switch (OperatorSeq.surround_nth(m, surround)) {
      | None => raise(SkelInconsistentWithOpSeq)
      | Some(e_m) => UHExp.get_err_status_t(e_m)
      };
    }
  | BinOp(err, _, _, _) => err
  };

let rec set_err_status_block = (err: err_status, zblock: zblock): zblock =>
  switch (zblock) {
  | BlockZL(zlines, e) => BlockZL(zlines, UHExp.set_err_status_t(err, e))
  | BlockZE(lines, ze) => BlockZE(lines, set_err_status_t(err, ze))
  }
and set_err_status_t = (err: err_status, ze: t): t =>
  switch (ze) {
  | CursorE(cursor, eo) => CursorE(cursor, UHExp.set_err_status_t(err, eo))
  | ParenthesizedZ(zblock) =>
    ParenthesizedZ(set_err_status_block(err, zblock))
  | OpSeqZ(skel, ze_n, surround) =>
    let (skel, ze_n, surround) =
      set_err_status_opseq(err, skel, ze_n, surround);
    OpSeqZ(skel, ze_n, surround);
  | LamZP(_, zp, ann, block) => LamZP(err, zp, ann, block)
  | LamZA(_, p, zann, block) => LamZA(err, p, zann, block)
  | LamZE(_, p, ann, zblock) => LamZE(err, p, ann, zblock)
  | InjZ(_, inj_side, zblock) => InjZ(err, inj_side, zblock)
  | CaseZE(_, zblock, rules, ann) => CaseZE(err, zblock, rules, ann)
  | CaseZR(_, block, zrules, ann) => CaseZR(err, block, zrules, ann)
  | CaseZA(_, block, rules, zann) => CaseZA(err, block, rules, zann)
  | ApPaletteZ(_, name, model, psi) => ApPaletteZ(err, name, model, psi)
  }
and set_err_status_opseq =
    (err: err_status, skel: UHExp.skel_t, ze_n: t, surround: opseq_surround)
    : (UHExp.skel_t, t, opseq_surround) =>
  switch (skel) {
  | Placeholder(m) =>
    if (m === OperatorSeq.surround_prefix_length(surround)) {
      let ze_n = set_err_status_t(err, ze_n);
      (skel, ze_n, surround);
    } else {
      switch (OperatorSeq.surround_nth(m, surround)) {
      | None => raise(SkelInconsistentWithOpSeq)
      | Some(e_m) =>
        let e_m = UHExp.set_err_status_t(err, e_m);
        switch (OperatorSeq.surround_update_nth(m, surround, e_m)) {
        | None => raise(SkelInconsistentWithOpSeq)
        | Some(surround) => (skel, ze_n, surround)
        };
      };
    }
  | BinOp(_, op, skel1, skel2) =>
    let skel = Skel.BinOp(err, op, skel1, skel2);
    (skel, ze_n, surround);
  };

let rec make_block_inconsistent =
        (u_gen: MetaVarGen.t, zblock: zblock): (zblock, MetaVarGen.t) =>
  switch (zblock) {
  | BlockZL(zlines, e) =>
    let (e, u_gen) = UHExp.make_t_inconsistent(u_gen, e);
    (BlockZL(zlines, e), u_gen);
  | BlockZE(lines, ze) =>
    let (ze, u_gen) = make_t_inconsistent(u_gen, ze);
    (BlockZE(lines, ze), u_gen);
  }
and make_t_inconsistent = (u_gen: MetaVarGen.t, ze: t): (t, MetaVarGen.t) =>
  switch (ze) {
  | CursorE(cursor, e) =>
    let (e, u_gen) = UHExp.make_t_inconsistent(u_gen, e);
    (CursorE(cursor, e), u_gen);
  | ParenthesizedZ(zblock) =>
    let (zblock, u_gen) = make_block_inconsistent(u_gen, zblock);
    (ParenthesizedZ(zblock), u_gen);
  | OpSeqZ(skel, ze_n, surround) =>
    let (skel, ze_n, surround, u_gen) =
      make_skel_inconsistent(u_gen, skel, ze_n, surround);
    (OpSeqZ(skel, ze_n, surround), u_gen);
  /* already in hole */
  | LamZP(InHole(TypeInconsistent, _), _, _, _)
  | LamZA(InHole(TypeInconsistent, _), _, _, _)
  | LamZE(InHole(TypeInconsistent, _), _, _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(InHole(TypeInconsistent, _), _, _, _)
  | CaseZR(InHole(TypeInconsistent, _), _, _, _)
  | CaseZA(InHole(TypeInconsistent, _), _, _, _)
  | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _) => (ze, u_gen)
  /* not in hole */
  | LamZP(NotInHole | InHole(WrongLength, _), _, _, _)
  | LamZA(NotInHole | InHole(WrongLength, _), _, _, _)
  | LamZE(NotInHole | InHole(WrongLength, _), _, _, _)
  | InjZ(NotInHole | InHole(WrongLength, _), _, _)
  | CaseZE(NotInHole | InHole(WrongLength, _), _, _, _)
  | CaseZR(NotInHole | InHole(WrongLength, _), _, _, _)
  | CaseZA(NotInHole | InHole(WrongLength, _), _, _, _)
  | ApPaletteZ(NotInHole | InHole(WrongLength, _), _, _, _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let ze = set_err_status_t(InHole(TypeInconsistent, u), ze);
    (ze, u_gen);
  }
and make_skel_inconsistent =
    (
      u_gen: MetaVarGen.t,
      skel: UHExp.skel_t,
      ze_n: t,
      surround: opseq_surround,
    )
    : (UHExp.skel_t, t, opseq_surround, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(m) =>
    if (m === OperatorSeq.surround_prefix_length(surround)) {
      let (ze_n, u_gen) = make_t_inconsistent(u_gen, ze_n);
      (skel, ze_n, surround, u_gen);
    } else {
      switch (OperatorSeq.surround_nth(m, surround)) {
      | None => raise(SkelInconsistentWithOpSeq)
      | Some(e_m) =>
        let (e_m, u_gen) = UHExp.make_t_inconsistent(u_gen, e_m);
        switch (OperatorSeq.surround_update_nth(m, surround, e_m)) {
        | None => raise(SkelInconsistentWithOpSeq)
        | Some(surround) => (skel, ze_n, surround, u_gen)
        };
      };
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) => (
      skel,
      ze_n,
      surround,
      u_gen,
    )
  | BinOp(NotInHole, op, skel1, skel2)
  | BinOp(InHole(WrongLength, _), op, skel1, skel2) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (
      BinOp(InHole(TypeInconsistent, u), op, skel1, skel2),
      ze_n,
      surround,
      u_gen,
    );
  };

let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (e, u_gen) = UHExp.new_EmptyHole(u_gen);
  (place_before_exp(e), u_gen);
};

let rec cursor_on_outer_expr = (ze: t): option((UHExp.block, cursor_pos)) =>
  switch (ze) {
  | CursorE(cursor, e) => Some((UHExp.drop_outer_parentheses(e), cursor))
  | ParenthesizedZ(BlockZE([], ze)) => cursor_on_outer_expr(ze)
  | ParenthesizedZ(_)
  | OpSeqZ(_, _, _)
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
  let (e, u_gen) = UHExp.new_EmptyHole(u_gen);
  let zrule = RuleZP(zp, UHExp.wrap_in_block(e));
  (zrule, u_gen);
};

let rec erase_block = (zblock: zblock): UHExp.block =>
  switch (zblock) {
  | BlockZL(zlines, e) => Block(erase_lines(zlines), e)
  | BlockZE(lines, ze) => Block(lines, erase(ze))
  }
and erase_lines = (zlis: zlines): UHExp.lines =>
  ZList.erase(zlis, erase_line)
and erase_line = (zline: zline): UHExp.line =>
  switch (zline) {
  | CursorL(_, line) => line
  | ExpLineZ(ze) => ExpLine(erase(ze))
  | LetLineZP(zp, ann, block) => LetLine(ZPat.erase(zp), ann, block)
  | LetLineZA(p, zann, block) => LetLine(p, Some(ZTyp.erase(zann)), block)
  | LetLineZE(p, ann, zblock) => LetLine(p, ann, erase_block(zblock))
  }
and erase = (ze: t): UHExp.t =>
  switch (ze) {
  | CursorE(_, e) => e
  | ParenthesizedZ(zblock) => Parenthesized(erase_block(zblock))
  | OpSeqZ(skel, ze', surround) =>
    let e = erase(ze');
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(e, surround));
  | LamZP(err, zp, ann, block) => Lam(err, ZPat.erase(zp), ann, block)
  | LamZA(err, p, zann, block) =>
    Lam(err, p, Some(ZTyp.erase(zann)), block)
  | LamZE(err, p, ann, zblock) => Lam(err, p, ann, erase_block(zblock))
  | InjZ(err, side, zblock) => Inj(err, side, erase_block(zblock))
  | CaseZE(err, zblock, rules, ann) =>
    Case(err, erase_block(zblock), rules, ann)
  | CaseZR(err, block, zrules, ann) =>
    Case(err, block, ZList.erase(zrules, erase_rule), ann)
  | CaseZA(err, e1, rules, zann) =>
    Case(err, e1, rules, Some(ZTyp.erase(zann)))
  | ApPaletteZ(err, palette_name, serialized_model, zpsi) =>
    let psi = ZSpliceInfo.erase(zpsi, ((ty, z)) => (ty, erase_block(z)));
    ApPalette(err, palette_name, serialized_model, psi);
  }
and erase_rule = (zr: zrule): UHExp.rule =>
  switch (zr) {
  | CursorR(_, rule) => rule
  | RuleZP(zp, block) => Rule(ZPat.erase(zp), block)
  | RuleZE(p, zblock) => Rule(p, erase_block(zblock))
  };

let rec cursor_on_opseq_block = (zblock: zblock): bool =>
  switch (zblock) {
  | BlockZL(zlines, _) => cursor_on_opseq_lines(zlines)
  | BlockZE(_, ze) => cursor_on_opseq_exp(ze)
  }
and cursor_on_opseq_lines = ((_, zline, _): zlines): bool =>
  switch (zline) {
  | CursorL(_, _) => false
  | LetLineZP(zp, _, _) => ZPat.cursor_on_opseq(zp)
  | LetLineZA(_, zann, _) => ZTyp.cursor_on_opseq(zann)
  | LetLineZE(_, _, zblock) => cursor_on_opseq_block(zblock)
  | ExpLineZ(ze) => cursor_on_opseq_exp(ze)
  }
and cursor_on_opseq_exp = (ze: t): bool =>
  switch (ze) {
  | CursorE(_, OpSeq(_, _)) => true
  | CursorE(_, _) => false
  | ParenthesizedZ(zblock) => cursor_on_opseq_block(zblock)
  | OpSeqZ(_, ze, _) => cursor_on_opseq_exp(ze)
  | LamZP(_, zp, _, _) => ZPat.cursor_on_opseq(zp)
  | LamZA(_, _, zann, _) => ZTyp.cursor_on_opseq(zann)
  | LamZE(_, _, _, zblock) => cursor_on_opseq_block(zblock)
  | InjZ(_, _, zblock) => cursor_on_opseq_block(zblock)
  | CaseZE(_, zblock, _, _) => cursor_on_opseq_block(zblock)
  | CaseZR(_, _, zrules, _) => cursor_on_opseq_rules(zrules)
  | CaseZA(_, _, _, zann) => ZTyp.cursor_on_opseq(zann)
  | ApPaletteZ(_, _, _, _) => false /* TODO */
  }
and cursor_on_opseq_rules = ((_, zrule, _): zrules): bool =>
  switch (zrule) {
  | CursorR(_, _) => false
  | RuleZP(zp, _) => ZPat.cursor_on_opseq(zp)
  | RuleZE(_, zblock) => cursor_on_opseq_block(zblock)
  };

let zblock_to_zlines = (zblock: zblock): zlines =>
  switch (zblock) {
  | BlockZL((prefix, zline, suffix), e) => (
      prefix,
      zline,
      suffix @ [ExpLine(e)],
    )
  | BlockZE(lines, ze) => (lines, ExpLineZ(ze), [])
  };

let node_positions_line = (line: UHExp.line): list(node_pos) =>
  switch (line) {
  | ExpLine(_) => []
  | EmptyLine => []
  | LetLine(_, ann, _) =>
    let ann_positions =
      switch (ann) {
      | None => []
      | Some(_) => node_positions(inner_cursors_k(1)) @ [Deeper(1)]
      };
    node_positions(inner_cursors_k(0))
    @ [Deeper(0)]
    @ ann_positions
    @ node_positions(inner_cursors_k(2))
    @ [Deeper(2)];
  };

let node_positions_exp = (e: UHExp.t): list(node_pos) =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => node_positions(valid_cursors_exp(e))
  /* inner nodes */
  | Parenthesized(_)
  | Inj(_, _, _) =>
    node_positions(inner_cursors_k(0))
    @ [Deeper(0)]
    @ node_positions(inner_cursors_k(1))
  | Lam(_, _, ann, _) =>
    let ann_positions =
      switch (ann) {
      | None => []
      | Some(_) => node_positions(inner_cursors_k(1)) @ [Deeper(1)]
      };
    node_positions(inner_cursors_k(0))
    @ [Deeper(0)]
    @ ann_positions
    @ node_positions(inner_cursors_k(2))
    @ [Deeper(2)];
  | Case(_, _, rules, ann) =>
    let ann_positions =
      switch (ann) {
      | None => []
      | Some(_) => [Deeper(List.length(rules) + 1)]
      };
    node_positions(inner_cursors_k(0))
    @ [Deeper(0)]
    @ (range(List.length(rules)) |> List.map(i => Deeper(i + 1)))
    @ node_positions(inner_cursors_k(1))
    @ ann_positions;
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.fold_left(
         (lstSoFar, i) =>
           switch (lstSoFar) {
           | [] => [Deeper(i)]
           | [_, ..._] =>
             lstSoFar @ node_positions(inner_cursors_k(i)) @ [Deeper(i)]
           },
         [],
       )
  | ApPalette(_, _, _, _) => [] /* TODO */
  };

let node_position_of_zline = (zline: zline): option(node_pos) =>
  switch (zline) {
  | CursorL(cursor, _) => Some(On(cursor))
  | LetLineZP(_, _, _) => Some(Deeper(0))
  | LetLineZA(_, _, _) => Some(Deeper(1))
  | LetLineZE(_, _, _) => Some(Deeper(2))
  | ExpLineZ(_) => None
  };

let node_position_of_t = (ze: t): node_pos =>
  switch (ze) {
  | CursorE(cursor, _) => On(cursor)
  | ParenthesizedZ(_) => Deeper(0)
  | OpSeqZ(_, _, surround) =>
    Deeper(OperatorSeq.surround_prefix_length(surround))
  | LamZP(_, _, _, _) => Deeper(0)
  | LamZA(_, _, _, _) => Deeper(1)
  | LamZE(_, _, _, _) => Deeper(2)
  | InjZ(_, _, _) => Deeper(0)
  | CaseZE(_, _, _, _) => Deeper(0)
  | CaseZR(_, _, (prefix, _, _), _) => Deeper(List.length(prefix) + 1)
  | CaseZA(_, _, rules, _) => Deeper(List.length(rules) + 1)
  | ApPaletteZ(_, _, _, _) => Deeper(0) /* TODO */
  };

let rec cursor_node_type_zblock = (zblock: zblock): node_type =>
  switch (zblock) {
  | BlockZL((_, zline, _), _) => cursor_node_type_zline(zline)
  | BlockZE(_, ze) => cursor_node_type_t(ze)
  }
and cursor_node_type_zline = (zline: zline): node_type =>
  switch (zline) {
  /* outer nodes */
  | CursorL(_, EmptyLine) => Outer
  /* inner nodes */
  | CursorL(_, LetLine(_, _, _)) => Inner
  | CursorL(_, ExpLine(_)) => Inner /* ghost node, should never happen */
  /* zipper */
  | ExpLineZ(ze) => cursor_node_type_t(ze)
  | LetLineZP(zp, _, _) => ZPat.cursor_node_type(zp)
  | LetLineZA(_, zann, _) => ZTyp.cursor_node_type(zann)
  | LetLineZE(_, _, zblock) => cursor_node_type_zblock(zblock)
  }
and cursor_node_type_t = (ze: t): node_type =>
  switch (ze) {
  /* outer nodes */
  | CursorE(_, EmptyHole(_))
  | CursorE(_, Var(_, _, _))
  | CursorE(_, NumLit(_, _))
  | CursorE(_, BoolLit(_, _))
  | CursorE(_, ListNil(_)) => Outer
  /* inner nodes */
  | CursorE(_, Lam(_, _, _, _))
  | CursorE(_, Inj(_, _, _))
  | CursorE(_, Case(_, _, _, _))
  | CursorE(_, Parenthesized(_))
  | CursorE(_, OpSeq(_, _))
  | CursorE(_, ApPalette(_, _, _, _)) => Inner
  /* zipper */
  | ParenthesizedZ(zblock) => cursor_node_type_zblock(zblock)
  | OpSeqZ(_, ze1, _) => cursor_node_type_t(ze1)
  | LamZP(_, zp, _, _) => ZPat.cursor_node_type(zp)
  | LamZA(_, _, zann, _) => ZTyp.cursor_node_type(zann)
  | LamZE(_, _, _, zblock) => cursor_node_type_zblock(zblock)
  | InjZ(_, _, zblock) => cursor_node_type_zblock(zblock)
  | CaseZE(_, zblock, _, _) => cursor_node_type_zblock(zblock)
  | CaseZR(_, _, (_, zrule, _), _) => cursor_node_type_rule(zrule)
  | CaseZA(_, _, _, zann) => ZTyp.cursor_node_type(zann)
  | ApPaletteZ(_, _, _, _) => Inner /* TODO */
  }
and cursor_node_type_rule = (zrule: zrule): node_type =>
  switch (zrule) {
  | CursorR(_, _) => Inner
  | RuleZP(zp, _) => ZPat.cursor_node_type(zp)
  | RuleZE(_, zblock) => cursor_node_type_zblock(zblock)
  };
