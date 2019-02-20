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
  | LetZP(ZPat.t, option(UHTyp.t), UHExp.t, UHExp.t)
  | LetZA(UHPat.t, ZTyp.t, UHExp.t, UHExp.t)
  | LetZE1(UHPat.t, option(UHTyp.t), t, UHExp.t)
  | LetZE2(UHPat.t, option(UHTyp.t), UHExp.t, t)
  | LamZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LamZA(UHPat.t, ZTyp.t, UHExp.t)
  | LamZE(UHPat.t, option(UHTyp.t), t)
  | InjZ(inj_side, t)
  | CaseZE(t, list(UHExp.rule))
  | CaseZR(UHExp.t, ZList.t(zrule, UHExp.rule))
  | OpSeqZ(UHExp.skel_t, t, OperatorSeq.opseq_surround(UHExp.t, UHExp.op))
  | ApPaletteZ(
      PaletteName.t,
      SerializedModel.t,
      ZSpliceInfo.t(UHExp.t, t)
    )
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
  | Deeper(_, LetZP(_, _, _, _))
  | Deeper(_, LetZA(_, _, _, _))
  | Deeper(_, LetZE1(_, _, _, _))
  | Deeper(_, LetZE2(_, _, _, _))
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
    (ze', u_gen)
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
  | LetZP(zp, ann, e1, e2) => UHExp.Let(ZPat.erase(zp), ann, e1, e2)
  | LetZA(p, zann, e1, e2) => UHExp.Let(p, Some(ZTyp.erase(zann)), e1, e2)
  | LetZE1(p, ann, ze, e) => UHExp.Let(p, ann, erase(ze), e)
  | LetZE2(p, ann, e, ze) => UHExp.Let(p, ann, e, erase(ze))
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
and erase_rule = (zr: zrule): UHExp.rule =>
  switch (zr) {
  | RuleZP(zp, e) => UHExp.Rule(ZPat.erase(zp), e)
  | RuleZE(p, ze) => UHExp.Rule(p, erase(ze))
  };

