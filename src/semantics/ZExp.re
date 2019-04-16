open SemanticsCommon;
open GeneralUtil;

type opseq_surround = OperatorSeq.opseq_surround(UHExp.t, UHExp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHExp.t, UHExp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHExp.t, UHExp.op);

type zblock =
  | BlockZL(zlines, UHExp.t)
  | BlockZE(UHExp.lines, t)
and zlines = ZList.t(zline, UHExp.line)
and zline =
  | CursorLO(outer_cursor, UHExp.line_outer)
  | CursorLI(inner_cursor, UHExp.line_inner)
  /* zipper cases */
  | ExpLineZ(t)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.block)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.block)
  | LetLineZE(UHPat.t, option(UHTyp.t), zblock)
and t =
  | CursorEO(outer_cursor, UHExp.t_outer)
  | CursorEI(inner_cursor, UHExp.t_inner)
  /* zipper cases */
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
  | CursorR(inner_cursor, UHExp.rule)
  | RuleZP(ZPat.t, UHExp.block)
  | RuleZE(UHPat.t, zblock);

let has_closing_delimiter_line = (li: UHExp.line_inner): bool =>
  switch (li) {
  | LetLine(_, _, _) => false
  };
let has_closing_delimiter_exp = (ei: UHExp.t_inner): bool =>
  switch (ei) {
  | Lam(_, _, _, _)
  | Case(_, _, _, Some(_))
  | OpSeq(_, _) => false
  | Inj(_, _, _)
  | Case(_, _, _, None)
  | Parenthesized(_)
  | ApPalette(_, _, _, _) => true
  };

let children_line = (li: UHExp.line_inner): list(int) =>
  switch (li) {
  | LetLine(_, None, _) => [0, 2]
  | LetLine(_, Some(_), _) => [0, 1, 2]
  };

let children_following_delimiters_line = (li: UHExp.line_inner): list(int) =>
  switch (li) {
  | LetLine(_, None, _) => [0, 2]
  | LetLine(_, Some(_), _) => [0, 1, 2]
  };
let children_following_delimiters_exp = (ei: UHExp.t_inner): list(int) =>
  switch (ei) {
  | Lam(_, _, None, _) => [0, 2]
  | Lam(_, _, Some(_), _) => [0, 1, 2]
  | Inj(_, _, _) => [0]
  | Case(_, _, _, None) => [0]
  | Case(_, _, rules, Some(_)) => [0, List.length(rules) + 1]
  | Parenthesized(_) => [0]
  | OpSeq(_, seq) => range(~lo=1, OperatorSeq.seq_length(seq))
  | ApPalette(_, _, _, _) => [0] /* TODO */
  };

let valid_outer_cursors_line = (lo: UHExp.line_outer): list(outer_cursor) =>
  range(UHExp.line_outer_length(lo)) |> List.map(j => Char(j));

let valid_inner_cursors_line = (li: UHExp.line_inner): list(inner_cursor) => {
  let before_child_positions =
    children_following_delimiters_line(li)
    |> List.map(k => [BeforeChild(k, Before), BeforeChild(k, After)])
    |> List.flatten;
  let closing_delimiter_positions =
    has_closing_delimiter_line(li)
      ? [ClosingDelimiter(Before), ClosingDelimiter(After)] : [];
  before_child_positions @ closing_delimiter_positions;
};

let is_valid_inner_cursor_line =
    (inner_cursor: inner_cursor, li: UHExp.line_inner): bool =>
  contains(valid_inner_cursors_line(li), inner_cursor);
let is_valid_inner_cursor_exp =
    (inner_cursor: inner_cursor, ei: UHExp.t_inner): bool =>
  switch (inner_cursor) {
  | BeforeChild(k, _) => contains(children_following_delimiters_exp(ei), k)
  | ClosingDelimiter(_) =>
    switch (ei) {
    | Lam(_, _, _, _)
    | OpSeq(_, _)
    | Case(_, _, _, Some(_)) => false
    | Case(_, _, _, None)
    | Inj(_, _, _)
    | Parenthesized(_)
    | ApPalette(_, _, _, _) => true
    }
  };

let is_valid_outer_cursor_line =
    (outer_cursor: outer_cursor, lo: UHExp.line_outer): bool =>
  contains(valid_outer_cursors_line(lo), outer_cursor);
let is_valid_outer_cursor_exp =
    (Char(j): outer_cursor, eo: UHExp.t_outer): bool =>
  0 <= j && j < UHExp.t_outer_length(eo);

let wrap_in_block = (ze: t): zblock => BlockZE([], ze);

let parenthesize = (ze: t): t => ParenthesizedZ(wrap_in_block(ze));

let bidelimit = (ze: t): t =>
  switch (ze) {
  | CursorEO(_, eo) =>
    if (UHExp.bidelimited_outer(eo)) {
      ze;
    } else {
      parenthesize(ze);
    }
  | CursorEI(_, ei) =>
    if (UHExp.bidelimited_inner(ei)) {
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
  /* leaf nodes */
  | CursorLO(Char(0), EmptyLine) => true
  | CursorLO(Char(_), _) => false
  /* branch nodes */
  | CursorLI(inner_cursor, LetLine(_, _, _)) =>
    inner_cursor === BeforeChild(0, Before)
  /* zipper cases */
  | ExpLineZ(ze) => is_before_exp(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _) => false
  }
and is_before_exp = (ze: t): bool =>
  switch (ze) {
  /* outer nodes */
  | CursorEO(Char(j), _) => j === 0
  /* inner nodes */
  | CursorEI(inner_cursor, Lam(_, _, _, _))
  | CursorEI(inner_cursor, Inj(_, _, _))
  | CursorEI(inner_cursor, Case(_, _, _, _))
  | CursorEI(inner_cursor, Parenthesized(_)) =>
    inner_cursor === BeforeChild(0, Before)
  | CursorEI(_, OpSeq(_, _)) => false
  | CursorEI(inner_cursor, ApPalette(_, _, _, _)) =>
    inner_cursor === BeforeChild(0, Before) /* TODO */
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
  | CursorLO(Char(0), EmptyLine) => true
  | CursorLO(Char(_), EmptyLine) => false
  /* inner nodes */
  | CursorLI(_, LetLine(_, _, _)) => false
  /* zipper cases */
  | ExpLineZ(ze) => is_after_exp(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => false
  | LetLineZE(_, _, zblock) => is_after_block(zblock)
  }
and is_after_exp = (ze: t): bool =>
  switch (ze) {
  /* outer nodes */
  | CursorEO(Char(j), eo) => j === UHExp.t_outer_length(eo)
  /* inner nodes */
  | CursorEI(_, Lam(_, _, _, _)) => false
  | CursorEI(_, Case(_, _, _, Some(_))) => false
  | CursorEI(inner_cursor, Case(_, _, _, None))
  | CursorEI(inner_cursor, Inj(_, _, _))
  | CursorEI(inner_cursor, Parenthesized(_)) =>
    inner_cursor === ClosingDelimiter(After)
  | CursorEI(_, OpSeq(_, _)) => false
  | CursorEI(inner_cursor, ApPalette(_, _, _, _)) =>
    inner_cursor === ClosingDelimiter(After) /* TODO */
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
  | LO(EmptyLine) => CursorLO(Char(0), EmptyLine)
  | LI(LetLine(_, _, _) as li) => CursorLI(BeforeChild(0, Before), li)
  | ExpLine(e) => ExpLineZ(place_before_exp(e))
  }
and place_before_exp = (e: UHExp.t): t =>
  switch (e) {
  /* outer nodes */
  | EO(eo) => CursorEO(Char(0), eo)
  /* inner nodes */
  | EI(Lam(_, _, _, _) as ei)
  | EI(Inj(_, _, _) as ei)
  | EI(Case(_, _, _, _) as ei)
  | EI(Parenthesized(_) as ei) => CursorEI(BeforeChild(0, Before), ei)
  | EI(OpSeq(skel, seq)) =>
    let (e1, suffix) = OperatorSeq.split0(seq);
    let ze1 = place_before_exp(e1);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    OpSeqZ(skel, ze1, surround);
  | EI(ApPalette(_, _, _, _) as ei) => CursorEI(BeforeChild(0, Before), ei) /* TODO */
  };
let place_before_lines = (lines: UHExp.lines): option(zlines) =>
  switch (lines) {
  | [] => None
  | [line, ...lines] => Some(([], place_before_line(line), lines))
  };
let place_before_rule = (rule: UHExp.rule): zrule =>
  CursorR(BeforeChild(0, Before), rule);

let rec place_after_block = (Block(lines, e): UHExp.block): zblock =>
  BlockZE(lines, place_after_exp(e))
and place_after_line = (line: UHExp.line): zline =>
  switch (line) {
  | LO(EmptyLine) => CursorLO(Char(0), EmptyLine)
  | LI(LetLine(p, ann, block)) =>
    LetLineZE(p, ann, place_after_block(block))
  | ExpLine(e) => ExpLineZ(place_after_exp(e))
  }
and place_after_exp = (e: UHExp.t): t =>
  switch (e) {
  /* outer nodes */
  | EO(eo) => CursorEO(Char(UHExp.t_outer_length(eo)), eo)
  /* inner nodes */
  | EI(Lam(err, p, ann, block)) =>
    LamZE(err, p, ann, place_after_block(block))
  | EI(Case(err, block, rules, Some(uty))) =>
    CaseZA(err, block, rules, ZTyp.place_after(uty))
  | EI(Case(_, _, _, None) as ei)
  | EI(Inj(_, _, _) as ei)
  | EI(Parenthesized(_) as ei) => CursorEI(ClosingDelimiter(After), ei)
  | EI(OpSeq(skel, seq)) =>
    let (e1, prefix) = OperatorSeq.split_tail(seq);
    let ze1 = place_after_exp(e1);
    let surround = OperatorSeq.EmptySuffix(prefix);
    OpSeqZ(skel, ze1, surround);
  | EI(ApPalette(_, _, _, _) as ei) =>
    CursorEI(ClosingDelimiter(After), ei) /* TODO */
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
  switch (cursor, e) {
  | (O(outer_cursor), EO(eo)) =>
    is_valid_outer_cursor_exp(outer_cursor, eo)
      ? Some(CursorEO(outer_cursor, eo)) : None
  | (I(inner_cursor), EI(ei)) =>
    is_valid_inner_cursor_exp(inner_cursor, ei)
      ? Some(CursorEI(inner_cursor, ei)) : None
  | (O(_), EI(_))
  | (I(_), EO(_)) => None
  };
let place_cursor_line = (cursor: cursor_pos, line: UHExp.line): option(zline) =>
  switch (cursor, line) {
  | (_, ExpLine(e)) =>
    switch (place_cursor_exp(cursor, e)) {
    | None => None
    | Some(ze) => Some(ExpLineZ(ze))
    }
  | (O(outer_cursor), LO(lo)) =>
    is_valid_outer_cursor_line(outer_cursor, lo)
      ? Some(CursorLO(outer_cursor, lo)) : None
  | (I(inner_cursor), LI(li)) =>
    is_valid_inner_cursor_line(inner_cursor, li)
      ? Some(CursorLI(inner_cursor, li)) : None
  | (O(_), LI(_))
  | (I(_), LO(_)) => None
  };
let place_cursor_rule = (cursor: cursor_pos, rule: UHExp.rule): option(zrule) =>
  switch (cursor) {
  | O(_) => None
  | I(inner_cursor) => Some(CursorR(inner_cursor, rule))
  };

let prune_empty_hole_line = (zli: zline): zline =>
  switch (zli) {
  | ExpLineZ(CursorEO(_, EmptyHole(_))) => place_before_line(LO(EmptyLine))
  | ExpLineZ(_)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _)
  | CursorLO(_)
  | CursorLI(_) => zli
  };

let rec get_err_status_block = (zblock: zblock): err_status =>
  switch (zblock) {
  | BlockZL(_, e) => UHExp.get_err_status_t(e)
  | BlockZE(_, ze) => get_err_status_t(ze)
  }
and get_err_status_t = (ze: t): err_status =>
  switch (ze) {
  | CursorEO(_, eo) => UHExp.get_err_status_t_outer(eo)
  | CursorEI(_, ei) => UHExp.get_err_status_t_inner(ei)
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
  | CursorEO(outer_cursor, eo) =>
    CursorEO(outer_cursor, UHExp.set_err_status_t_outer(err, eo))
  | CursorEI(inner_cursor, ei) =>
    CursorEI(inner_cursor, UHExp.set_err_status_t_inner(err, ei))
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
  | CursorEO(outer_cursor, eo) =>
    let (eo, u_gen) = UHExp.make_t_outer_inconsistent(u_gen, eo);
    (CursorEO(outer_cursor, eo), u_gen);
  | CursorEI(inner_cursor, ei) =>
    let (ei, u_gen) = UHExp.make_t_inner_inconsistent(u_gen, ei);
    (CursorEI(inner_cursor, ei), u_gen);
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
  | LamZP(NotInHole, _, _, _)
  | LamZP(InHole(WrongLength, _), _, _, _)
  | LamZA(NotInHole, _, _, _)
  | LamZA(InHole(WrongLength, _), _, _, _)
  | LamZE(NotInHole, _, _, _)
  | LamZE(InHole(WrongLength, _), _, _, _)
  | InjZ(NotInHole, _, _)
  | InjZ(InHole(WrongLength, _), _, _)
  | CaseZE(NotInHole, _, _, _)
  | CaseZE(InHole(WrongLength, _), _, _, _)
  | CaseZR(NotInHole, _, _, _)
  | CaseZR(InHole(WrongLength, _), _, _, _)
  | CaseZA(NotInHole, _, _, _)
  | CaseZA(InHole(WrongLength, _), _, _, _)
  | ApPaletteZ(NotInHole, _, _, _)
  | ApPaletteZ(InHole(WrongLength, _), _, _, _) =>
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

/* TODO
   let rec cursor_on_root_expr = (ze: t): option((UHExp.block, cursor_pos)) =>
     switch (ze) {
     | CursorEO(outer_cursor, eo)
     | CursorE(side, e) => Some((UHExp.drop_outer_parentheses(e), side))
     | ParenthesizedZ(BlockZE([], ze)) => cursor_on_root_expr(ze)
     | ParenthesizedZ(_) => None
     | OpSeqZ(_, _, _) => None
     | DeeperE(_, _) => None
     };
   */

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
  | CursorLO(_, lo) => LO(lo)
  | CursorLI(_, li) => LI(li)
  | ExpLineZ(ze) => ExpLine(erase(ze))
  | LetLineZP(zp, ann, block) => LI(LetLine(ZPat.erase(zp), ann, block))
  | LetLineZA(p, zann, block) =>
    LI(LetLine(p, Some(ZTyp.erase(zann)), block))
  | LetLineZE(p, ann, zblock) => LI(LetLine(p, ann, erase_block(zblock)))
  }
and erase = (ze: t): UHExp.t =>
  switch (ze) {
  | CursorEO(_, eo) => EO(eo)
  | CursorEI(_, ei) => EI(ei)
  | ParenthesizedZ(zblock) => EI(Parenthesized(erase_block(zblock)))
  | OpSeqZ(skel, ze', surround) =>
    let e = erase(ze');
    EI(OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(e, surround)));
  | LamZP(err, zp, ann, block) => EI(Lam(err, ZPat.erase(zp), ann, block))
  | LamZA(err, p, zann, block) =>
    EI(Lam(err, p, Some(ZTyp.erase(zann)), block))
  | LamZE(err, p, ann, zblock) => EI(Lam(err, p, ann, erase_block(zblock)))
  | InjZ(err, side, zblock) => EI(Inj(err, side, erase_block(zblock)))
  | CaseZE(err, zblock, rules, ann) =>
    EI(Case(err, erase_block(zblock), rules, ann))
  | CaseZR(err, block, zrules, ann) =>
    EI(Case(err, block, ZList.erase(zrules, erase_rule), ann))
  | CaseZA(err, e1, rules, zann) =>
    EI(Case(err, e1, rules, Some(ZTyp.erase(zann))))
  | ApPaletteZ(err, palette_name, serialized_model, zpsi) =>
    let psi = ZSpliceInfo.erase(zpsi, ((ty, z)) => (ty, erase_block(z)));
    EI(ApPalette(err, palette_name, serialized_model, psi));
  }
and erase_rule = (zr: zrule): UHExp.rule =>
  switch (zr) {
  | CursorR(_, rule) => rule
  | RuleZP(zp, block) => Rule(ZPat.erase(zp), block)
  | RuleZE(p, zblock) => Rule(p, erase_block(zblock))
  };
