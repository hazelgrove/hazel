open SemanticsCommon;
open HazelUtil;

[@deriving show({with_path: false})]
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
  | LineItemZL(zline_item, UHExp.t)
  | LineItemZE(UHExp.line_item, t)
  | LamZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LamZA(UHPat.t, ZTyp.t, UHExp.t)
  | LamZE(UHPat.t, option(UHTyp.t), t)
  | InjZ(inj_side, t)
  | CaseZE(t, list(UHExp.rule), option(UHTyp.t))
  | CaseZR(UHExp.t, ZList.t(zrule, UHExp.rule), option(UHTyp.t))
  | CaseZA(UHExp.t, list(UHExp.rule), ZTyp.t)
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

let bidelimit = (ze: t): t =>
  switch (ze) {
  | CursorE(cursor_side, e) => CursorE(cursor_side, UHExp.bidelimit(e))
  | ParenthesizedZ(_)
  | Deeper(_, InjZ(_, _))
  | Deeper(_, ApPaletteZ(_, _, _)) =>
    /* | Deeper _ (ListLitZ _) */
    ze
  | Deeper(_, CaseZE(_, _, _))
  | Deeper(_, CaseZR(_, _, _))
  | Deeper(_, CaseZA(_, _, _))
  | Deeper(_, LineItemZL(_, _))
  | Deeper(_, LineItemZE(_, _))
  | Deeper(_, LamZP(_, _, _))
  | Deeper(_, LamZA(_, _, _))
  | Deeper(_, LamZE(_, _, _))
  | Deeper(_, OpSeqZ(_, _, _)) => ParenthesizedZ(ze)
  };

let rec set_err_status = (err: err_status, ze: t): t =>
  switch (ze) {
  | CursorE(cursor_side, e) =>
    let e = UHExp.set_err_status(err, e);
    CursorE(cursor_side, e);
  | Deeper(_, OpSeqZ(BinOp(_, op, skel1, skel2), ze0, surround)) =>
    Deeper(err, OpSeqZ(BinOp(err, op, skel1, skel2), ze0, surround))
  | Deeper(_, ze') => Deeper(err, ze')
  | ParenthesizedZ(ze1) => ParenthesizedZ(set_err_status(err, ze1))
  };

let rec make_inconsistent = (u_gen: MetaVarGen.t, ze: t): (t, MetaVarGen.t) =>
  switch (ze) {
  | CursorE(cursor_side, e) =>
    let (e', u_gen) = UHExp.make_inconsistent(u_gen, e);
    (CursorE(cursor_side, e'), u_gen);
  | Deeper(NotInHole, _)
  | Deeper(InHole(WrongLength, _), _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let ze' = set_err_status(InHole(TypeInconsistent, u), ze);
    (ze', u_gen);
  | Deeper(InHole(TypeInconsistent, _), _) => (ze, u_gen)
  | ParenthesizedZ(ze1) =>
    let (ze1', u_gen) = make_inconsistent(u_gen, ze1);
    (ParenthesizedZ(ze1'), u_gen);
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
    Tm(err_state, e');
  | ParenthesizedZ(ze1) => UHExp.Parenthesized(erase(ze1))
  }
and erase' = (ze: t'): UHExp.t' =>
  switch (ze) {
  | LineItemZL(zli, e) => LineItem(erase_line_item(zli), e)
  | LineItemZE(lis, ze) => LineItem(lis, erase(ze))
  | LamZP(zp, ann, e1) => Lam(ZPat.erase(zp), ann, e1)
  | LamZA(p, zann, e1) => Lam(p, Some(ZTyp.erase(zann)), e1)
  | LamZE(p, ann, ze1) => Lam(p, ann, erase(ze1))
  | InjZ(side, ze) => Inj(side, erase(ze))
  /* | ListLitZ zes -> UHExp.ListLit (ZList.erase zes erase) */
  | CaseZE(ze1, rules, ann) => Case(erase(ze1), rules, ann)
  | CaseZR(e1, zrules, ann) =>
    Case(e1, ZList.erase(zrules, erase_rule), ann)
  | CaseZA(e1, rules, zann) =>
    UHExp.Case(e1, rules, Some(ZTyp.erase(zann)))
  | OpSeqZ(skel, ze', surround) =>
    let e = erase(ze');
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(e, surround));
  | ApPaletteZ(palette_name, serialized_model, zpsi) =>
    let psi = ZSpliceInfo.erase(zpsi, ((ty, z)) => (ty, erase(z)));
    ApPalette(palette_name, serialized_model, psi);
  }
and erase_line_item = (zli: zline_item): UHExp.line_item =>
  switch (zli) {
  | CursorL(_, li) => li
  | DeeperL(zli') => erase_line_item'(zli')
  }
and erase_line_item' = (zli': zline_item'): UHExp.line_item =>
  switch (zli') {
  | ExpLineZ(ze) => ExpLine(erase(ze))
  | LetLineZP(zp, ann, e) => LetLine(ZPat.erase(zp), ann, e)
  | LetLineZA(p, zann, e) => LetLine(p, Some(ZTyp.erase(zann)), e)
  | LetLineZE(p, ann, ze) => LetLine(p, ann, erase(ze))
  }
and erase_rule = (zr: zrule): UHExp.rule =>
  switch (zr) {
  | RuleZP(zp, e) => Rule(ZPat.erase(zp), e)
  | RuleZE(p, ze) => Rule(p, erase(ze))
  };

let rec is_before = (ze: t): bool =>
  switch (ze) {
  | CursorE(Before, _) => true
  | CursorE(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, LineItemZL(zli, _)) => is_before_line_item(zli)
  | Deeper(_, LineItemZE(_, _)) => false
  | Deeper(_, LamZP(_, _, _))
  | Deeper(_, LamZA(_, _, _))
  | Deeper(_, LamZE(_, _, _)) => false
  | Deeper(_, InjZ(_, _)) => false
  | Deeper(_, CaseZE(_, _, _))
  | Deeper(_, CaseZR(_, _, _))
  | Deeper(_, CaseZA(_, _, _)) => false
  | Deeper(_, OpSeqZ(_, ze1, EmptyPrefix(_))) => is_before(ze1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, ApPaletteZ(_, _, _)) => false
  }
and is_before_line_item = (zli: zline_item): bool =>
  switch (zli) {
  | CursorL(_, EmptyLine) => true
  | CursorL(Before, _) => true
  | CursorL(_, _) => false
  | DeeperL(zli') => is_before_line_item'(zli')
  }
and is_before_line_item' = (zli': zline_item'): bool =>
  switch (zli') {
  | ExpLineZ(ze) => is_before(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _)
  | LetLineZE(_, _, _) => false
  };

let rec is_after = (ze: t): bool =>
  switch (ze) {
  | CursorE(After, _) => true
  | CursorE(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, LineItemZL(_, _)) => false
  | Deeper(_, LineItemZE(_, ze1)) => is_after(ze1)
  | Deeper(_, LamZP(_, _, _))
  | Deeper(_, LamZA(_, _, _)) => false
  | Deeper(_, LamZE(_, _, ze1)) => is_after(ze1)
  | Deeper(_, InjZ(_, _)) => false
  | Deeper(_, CaseZE(_, _, _))
  | Deeper(_, CaseZR(_, _, _)) => false
  | Deeper(_, CaseZA(_, _, zann)) => ZTyp.is_after(zann)
  | Deeper(_, OpSeqZ(_, ze1, EmptySuffix(_))) => is_after(ze1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, ApPaletteZ(_, _, _)) => false
  };

let is_after_line_item' = (zli': zline_item'): bool =>
  switch (zli') {
  | ExpLineZ(ze) => is_after(ze)
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => false
  | LetLineZE(_, _, ze) => is_after(ze)
  };
let is_after_line_item = (zli: zline_item): bool =>
  switch (zli) {
  | CursorL(_, EmptyLine) => true
  | CursorL(After, _) => true
  | CursorL(_, _) => false
  | DeeperL(zli') => is_after_line_item'(zli')
  };

let rec place_before = (e: UHExp.t): t =>
  switch (e) {
  | Tm(err_status, LineItem(EmptyLine, e1)) =>
    Deeper(err_status, LineItemZL(CursorL(Before, EmptyLine), e1))
  | Tm(err_status, LineItem(ExpLine(e1), e2)) =>
    let ze1 = place_before(e1);
    Deeper(err_status, LineItemZL(DeeperL(ExpLineZ(ze1)), e2));
  | Tm(_err_status, LineItem(LetLine(_, _, _), _)) =>
    /* TODO this selects the entire block, perhaps should consider enabling selecting single line items */
    CursorE(Before, e)
  | Tm(err_status, OpSeq(skel, opseq)) =>
    let (e1, suffix) = OperatorSeq.split0(opseq);
    let ze1 = place_before(e1);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    Deeper(err_status, OpSeqZ(skel, ze1, surround));
  | Tm(_, Var(_, _))
  | Tm(_, Lam(_, _, _))
  | Tm(_, NumLit(_))
  | Tm(_, BoolLit(_))
  | Tm(_, Inj(_, _))
  | Tm(_, Case(_, _, _))
  | Tm(_, ListNil)
  | Tm(_, EmptyHole(_))
  | Tm(_, ApPalette(_, _, _))
  | Parenthesized(_) => CursorE(Before, e)
  };

let rec place_after = (e: UHExp.t): t =>
  switch (e) {
  | Tm(err_status, LineItem(li, e2)) =>
    let ze2 = place_after(e2);
    Deeper(err_status, LineItemZE(li, ze2));
  | Tm(err_status, OpSeq(skel, opseq)) =>
    let (en, prefix) = OperatorSeq.split_tail(opseq);
    let zen = place_after(en);
    let surround = OperatorSeq.EmptySuffix(prefix);
    Deeper(err_status, OpSeqZ(skel, zen, surround));
  | Tm(_, Case(e, rules, Some(ty))) =>
    Deeper(NotInHole, CaseZA(e, rules, ZTyp.place_after(ty)))
  | Tm(_, Case(_, _, None))
  | Tm(_, Var(_, _))
  | Tm(_, Lam(_, _, _))
  | Tm(_, NumLit(_))
  | Tm(_, BoolLit(_))
  | Tm(_, Inj(_, _))
  | Tm(_, ListNil)
  | Tm(_, EmptyHole(_))
  | Tm(_, ApPalette(_, _, _))
  | Parenthesized(_) => CursorE(After, e)
  };

let place_after_line_item = (li: UHExp.line_item): zline_item =>
  switch (li) {
  | EmptyLine => CursorL(After, li)
  | ExpLine(e) => DeeperL(ExpLineZ(place_after(e)))
  | LetLine(p, ann, e) => DeeperL(LetLineZE(p, ann, place_after(e)))
  };

let prune_single_hole_line = (zli: zline_item): zline_item =>
  switch (zli) {
  | CursorL(side, li) => CursorL(side, UHExp.prune_single_hole_line(li))
  | DeeperL(ExpLineZ(CursorE(_, Tm(_, EmptyHole(_))))) =>
    CursorL(Before, EmptyLine)
  | DeeperL(ExpLineZ(_))
  | DeeperL(LetLineZP(_, _, _))
  | DeeperL(LetLineZA(_, _, _))
  | DeeperL(LetLineZE(_, _, _)) => zli
  };

let default_NotInHole = (e: option(err_status)): err_status =>
  switch (e) {
  | None => NotInHole
  | Some(e) => e
  };

let prepend_line = (~err_status=?, li: UHExp.line_item, ze: t): t =>
  Deeper(default_NotInHole(err_status), LineItemZE(li, ze));

let prepend_zline = (~err_status=?, zli: zline_item, e: UHExp.t): t =>
  Deeper(default_NotInHole(err_status), LineItemZL(zli, e));

let prune_and_prepend_line = (~err_status=?, li: UHExp.line_item, ze: t): t =>
  prepend_line(
    ~err_status=default_NotInHole(err_status),
    UHExp.prune_single_hole_line(li),
    ze,
  );

let rec prune_and_prepend_lines = (e1: UHExp.t, ze2: t): t =>
  switch (e1) {
  | Tm(_, LineItem(li, e3)) =>
    prune_and_prepend_line(li, prune_and_prepend_lines(e3, ze2))
  | Tm(_, _)
  | Parenthesized(_) => prune_and_prepend_line(ExpLine(e1), ze2)
  };

let prune_and_prepend_zline = (~err_status=?, zli: zline_item, e: UHExp.t): t =>
  prepend_zline(
    ~err_status=default_NotInHole(err_status),
    prune_single_hole_line(zli),
    e,
  );
