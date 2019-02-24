open SemanticsCommon;
open Util;

type nat = int;

type cursor_side = SemanticsCommon.cursor_side;

type opseq_surround = OperatorSeq.opseq_surround(UHExp.t, UHExp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHExp.t, UHExp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHExp.t, UHExp.op);

type t =
  | CursorE(cursor_side, UHExp.t)
  /* | CursorPalette : PaletteName.t -> PaletteSerializedModel.t -> hole_ref -> t -> t */
  | Deeper(err_status, t')
  | ParenthesizedZ(t)
and t' =
  | AscZ1(t, UHTyp.t)
  | AscZ2(UHExp.t, ZTyp.t)
  | LineItemZL(zline_item, UHExp.t)
  | LineItemZE(UHExp.line_item, t)
  | LamZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LamZA(UHPat.t, ZTyp.t, UHExp.t)
  | LamZE(UHPat.t, option(UHTyp.t), t)
  | InjZ(inj_side, t)
  | CaseZE(t, list(UHExp.rule))
  | CaseZR(UHExp.t, ZList.t(zrule, UHExp.rule))
  | OpSeqZ(UHExp.skel_t, t, OperatorSeq.opseq_surround(UHExp.t, UHExp.op))
  | ApPaletteZ(PaletteName.t, SerializedModel.t, ZSpliceInfo.t(UHExp.t, t))
and zline_item =
  | CursorL(cursor_side, UHExp.line_item)
  | DeeperL(zline_item')
and zline_item' =
  | ExpLineZ(t)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.t)
  | LetLineZE(UHPat.t, option(UHTyp.t), t)
and zrule =
  | RuleZP(ZPat.t, UHExp.t)
  | RuleZE(UHPat.t, t)
and zrules = ZList.t(zrule, UHExp.rule);

let bidelimit = ze =>
  switch (ze) {
  | CursorE(cursor_side, e) => CursorE(cursor_side, UHExp.bidelimit(e))
  | ParenthesizedZ(_)
  | Deeper(_, InjZ(_, _))
  | Deeper(_, ApPaletteZ(_, _, _))
  | Deeper(_, CaseZE(_, _))
  | Deeper(_, CaseZR(_, _)) =>
    /* | Deeper _ (ListLitZ _) */
    ze
  | Deeper(_, AscZ1(_, _))
  | Deeper(_, AscZ2(_, _))
  | Deeper(_, LineItemZL(_, _))
  | Deeper(_, LineItemZE(_, _))
  | Deeper(_, LamZP(_, _, _))
  | Deeper(_, LamZA(_, _, _))
  | Deeper(_, LamZE(_, _, _))
  | Deeper(_, OpSeqZ(_, _, _)) => ParenthesizedZ(ze)
  };

let rec set_err_status = (err, ze) =>
  switch (ze) {
  | CursorE(cursor_side, e) =>
    let e = UHExp.set_err_status(err, e);
    CursorE(cursor_side, e);
  | Deeper(_, OpSeqZ(Skel.BinOp(_, op, skel1, skel2), ze0, surround)) =>
    Deeper(err, OpSeqZ(Skel.BinOp(err, op, skel1, skel2), ze0, surround))
  | Deeper(_, ze') => Deeper(err, ze')
  | ParenthesizedZ(ze1) => ParenthesizedZ(set_err_status(err, ze1))
  };

let rec make_inconsistent = (u_gen: MetaVarGen.t, ze: t): (t, MetaVarGen.t) =>
  switch (ze) {
  | CursorE(cursor_side, e) =>
    let (e', u_gen) = UHExp.make_inconsistent(u_gen, e);
    (CursorE(cursor_side, e'), u_gen);
  | Deeper(NotInHole, ze')
  | Deeper(InHole(WrongLength, _), ze') =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let ze' = set_err_status(InHole(TypeInconsistent, u), ze);
    (ze', u_gen);
  | Deeper(InHole(TypeInconsistent, _), _) => (ze, u_gen)
  | ParenthesizedZ(ze1) =>
    let (ze1', u_gen) = make_inconsistent(u_gen, ze1);
    (ParenthesizedZ(ze1), u_gen);
  };

let new_EmptyHole = (u_gen: MetaVarGen.t) => {
  let (e, u_gen) = UHExp.new_EmptyHole(u_gen);
  (CursorE(Before, e), u_gen);
};

let rec cursor_on_outer_expr = (ze: t): option((UHExp.t, cursor_side)) =>
  switch (ze) {
  | CursorE(side, e) => Some((UHExp.drop_outer_parentheses(e), side))
  | ParenthesizedZ(ze') => cursor_on_outer_expr(ze')
  | Deeper(_, _) => None
  };

let empty_zrule = (u_gen: MetaVarGen.t): (zrule, MetaVarGen.t) => {
  let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
  let (rule_e, u_gen) = UHExp.new_EmptyHole(u_gen);
  let zrule = RuleZP(zp, rule_e);
  (zrule, u_gen);
};

let rec erase = (ze: t): UHExp.t =>
  switch (ze) {
  | CursorE(_, e) => e
  | Deeper(err_state, ze') =>
    let e' = erase'(ze');
    UHExp.Tm(err_state, e');
  | ParenthesizedZ(ze1) => UHExp.Parenthesized(erase(ze1))
  }
and erase' = (ze: t'): UHExp.t' =>
  switch (ze) {
  | AscZ1(ze', ty) => UHExp.Asc(erase(ze'), ty)
  | AscZ2(e', zty) => UHExp.Asc(e', ZTyp.erase(zty))
  | LineItemZL(zli, e) => UHExp.LineItem(erase_line_item(zli), e)
  | LineItemZE(lis, ze) => UHExp.LineItem(lis, erase(ze))
  | LamZP(zp, ann, e1) => UHExp.Lam(ZPat.erase(zp), ann, e1)
  | LamZA(p, zann, e1) => UHExp.Lam(p, Some(ZTyp.erase(zann)), e1)
  | LamZE(p, ann, ze1) => UHExp.Lam(p, ann, erase(ze1))
  | InjZ(side, ze) => UHExp.Inj(side, erase(ze))
  /* | ListLitZ zes -> UHExp.ListLit (ZList.erase zes erase) */
  | CaseZE(ze1, rules) => UHExp.Case(erase(ze1), rules)
  | CaseZR(e1, zrules) => UHExp.Case(e1, ZList.erase(zrules, erase_rule))
  | OpSeqZ(skel, ze', surround) =>
    let e = erase(ze');
    UHExp.OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(e, surround));
  | ApPaletteZ(palette_name, serialized_model, zpsi) =>
    let psi = ZSpliceInfo.erase(zpsi, ((ty, z)) => (ty, erase(z)));
    UHExp.ApPalette(palette_name, serialized_model, psi);
  }
and erase_line_item = (zli: zline_item): UHExp.line_item =>
  switch (zli) {
  | CursorL(_, li) => li
  | DeeperL(zli') => erase_line_item'(zli')
  }
and erase_line_item' = (zli': zline_item'): UHExp.line_item =>
  switch (zli') {
  | ExpLineZ(ze) => UHExp.ExpLine(erase(ze))
  | LetLineZP(zp, ann, e) => UHExp.LetLine(ZPat.erase(zp), ann, e)
  | LetLineZA(p, zann, e) => UHExp.LetLine(p, Some(ZTyp.erase(zann)), e)
  | LetLineZE(p, ann, ze) => UHExp.LetLine(p, ann, erase(ze))
  }
and erase_rule = (zr: zrule): UHExp.rule =>
  switch (zr) {
  | RuleZP(zp, e) => UHExp.Rule(ZPat.erase(zp), e)
  | RuleZE(p, ze) => UHExp.Rule(p, erase(ze))
  };

let rec cursor_at_start = (ze: t): bool =>
  switch (ze) {
  | CursorE(Before, _) => true
  | CursorE(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, AscZ1(ze1, _)) => cursor_at_start(ze1)
  | Deeper(_, AscZ2(_, _)) => false
  | Deeper(_, LineItemZL(zli, _)) => cursor_at_start_line_item(zli)
  | Deeper(_, LineItemZE(_, _)) => false
  | Deeper(_, LamZP(_, _, _))
  | Deeper(_, LamZA(_, _, _))
  | Deeper(_, LamZE(_, _, _)) => false
  | Deeper(_, InjZ(_, _)) => false
  | Deeper(_, CaseZE(_, _))
  | Deeper(_, CaseZR(_, _)) => false
  | Deeper(_, OpSeqZ(_, ze1, OperatorSeq.EmptyPrefix(_))) =>
    cursor_at_start(ze1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, ApPaletteZ(_, _, _)) => false
  }
and cursor_at_start_line_item = (zli: zline_item): bool =>
  switch (zli) {
  | CursorL(_, UHExp.EmptyLine) => true
  | CursorL(Before, _) => true
  | CursorL(_, _) => false
  | DeeperL(zli') => cursor_at_start_line_item'(zli')
  }
and cursor_at_start_line_item' = (zli': zline_item'): bool =>
  switch (zli') {
  | ExpLineZ(ze) => cursor_at_start(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _) => false
  };

let rec cursor_at_end = (ze: t): bool =>
  switch (ze) {
  | CursorE(After, _) => true
  | CursorE(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, AscZ1(_, _)) => false
  | Deeper(_, AscZ2(_, zty)) => ZTyp.cursor_at_end(zty)
  | Deeper(_, LineItemZL(_, _)) => false
  | Deeper(_, LineItemZE(_, ze1)) => cursor_at_end(ze1)
  | Deeper(_, LamZP(_, _, _))
  | Deeper(_, LamZA(_, _, _)) => false
  | Deeper(_, LamZE(_, _, ze1)) => cursor_at_end(ze1)
  | Deeper(_, InjZ(_, _)) => false
  | Deeper(_, CaseZE(_, _))
  | Deeper(_, CaseZR(_, _)) => false
  | Deeper(_, OpSeqZ(_, ze1, OperatorSeq.EmptySuffix(_))) =>
    cursor_at_end(ze1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, ApPaletteZ(_, _, _)) => false
  };

let cursor_at_end_line_item' = (zli': zline_item'): bool =>
  switch (zli') {
  | ExpLineZ(ze) => cursor_at_end(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => false
  | LetLineZE(_, _, ze) => cursor_at_end(ze)
  };
let cursor_at_end_line_item = (zli: zline_item): bool =>
  switch (zli) {
  | CursorL(_, UHExp.EmptyLine) => true
  | CursorL(After, _) => true
  | CursorL(_, _) => false
  | DeeperL(zli') => cursor_at_end_line_item'(zli')
  };

let rec place_Before = (e: UHExp.t): t =>
  switch (e) {
  | UHExp.Tm(err_status, UHExp.Asc(e1, ty)) =>
    let ze1 = place_Before(e1);
    Deeper(err_status, AscZ1(ze1, ty));
  | UHExp.Tm(err_status, UHExp.LineItem(EmptyLine, e1)) =>
    Deeper(err_status, LineItemZL(CursorL(Before, UHExp.EmptyLine), e1))
  | UHExp.Tm(err_status, UHExp.LineItem(ExpLine(e1), e2)) =>
    let ze1 = place_Before(e1);
    Deeper(err_status, LineItemZL(DeeperL(ExpLineZ(ze1)), e2));
  | UHExp.Tm(err_status, UHExp.LineItem(LetLine(_, _, _), _)) =>
    /* TODO this selects the entire block, perhaps should consider enabling selecting single line items */
    CursorE(Before, e)
  | UHExp.Tm(err_status, UHExp.OpSeq(skel, opseq)) =>
    let (e1, suffix) = OperatorSeq.split0(opseq);
    let ze1 = place_Before(e1);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    Deeper(err_status, OpSeqZ(skel, ze1, surround));
  | UHExp.Tm(_, UHExp.Var(_, _))
  | UHExp.Tm(_, UHExp.Lam(_, _, _))
  | UHExp.Tm(_, UHExp.NumLit(_))
  | UHExp.Tm(_, UHExp.BoolLit(_))
  | UHExp.Tm(_, UHExp.Inj(_, _))
  | UHExp.Tm(_, UHExp.Case(_, _))
  | UHExp.Tm(_, UHExp.ListNil)
  | UHExp.Tm(_, UHExp.EmptyHole(_))
  | UHExp.Tm(_, UHExp.ApPalette(_, _, _))
  | UHExp.Parenthesized(_) => CursorE(Before, e)
  };

let rec place_After = (e: UHExp.t): t =>
  switch (e) {
  | UHExp.Tm(err_status, UHExp.Asc(e1, ty)) =>
    let zty = ZTyp.place_After(ty);
    Deeper(err_status, AscZ2(e1, zty));
  | UHExp.Tm(err_status, UHExp.LineItem(li, e2)) =>
    let ze2 = place_After(e2);
    Deeper(err_status, LineItemZE(li, ze2));
  | UHExp.Tm(err_status, UHExp.OpSeq(skel, opseq)) =>
    let (en, prefix) = OperatorSeq.split_tail(opseq);
    let zen = place_After(en);
    let surround = OperatorSeq.EmptySuffix(prefix);
    Deeper(err_status, OpSeqZ(skel, zen, surround));
  | UHExp.Tm(_, UHExp.Var(_, _))
  | UHExp.Tm(_, UHExp.Lam(_, _, _))
  | UHExp.Tm(_, UHExp.NumLit(_))
  | UHExp.Tm(_, UHExp.BoolLit(_))
  | UHExp.Tm(_, UHExp.Inj(_, _))
  | UHExp.Tm(_, UHExp.Case(_, _))
  | UHExp.Tm(_, UHExp.ListNil)
  | UHExp.Tm(_, UHExp.EmptyHole(_))
  | UHExp.Tm(_, UHExp.ApPalette(_, _, _))
  | UHExp.Parenthesized(_) => CursorE(After, e)
  };

let rec place_After_line_item = (li: UHExp.line_item): zline_item =>
  switch (li) {
  | UHExp.EmptyLine => CursorL(After, li)
  | UHExp.ExpLine(e) => DeeperL(ExpLineZ(place_After(e)))
  | UHExp.LetLine(p, ann, e) => DeeperL(LetLineZE(p, ann, place_After(e)))
  };