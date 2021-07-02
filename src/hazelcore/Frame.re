open Sexplib.Std;

[@deriving sexp]
type slice =
  | ExpBlock(ZExp.zblock)
  | ExpLine(ZExp.zline)
  | ExpSeq(ZExp.zopseq)
  | ExpOperand(ZExp.zoperand)
  | ExpOperator(ZExp.zoperator)
  | ExpRules(ZExp.zrules)
  | ExpRule(ZExp.zrule)
  | PatSeq(ZPat.zopseq)
  | PatOperand(ZPat.zoperand)
  | PatOperator(ZPat.zoperator)
  | TypSeq(ZTyp.zopseq)
  | TypOperand(ZTyp.zoperand)
  | TypOperator(ZTyp.zoperator);

[@deriving sexp]
type t = list(slice);

[@deriving sexp]
type cursor_term =
  | Exp(CursorPosition.t, UHExp.operand)
  | Pat(CursorPosition.t, UHPat.operand)
  | Typ(CursorPosition.t, UHTyp.operand)
  | ExpOp(CursorPosition.t, UHExp.operator)
  | PatOp(CursorPosition.t, UHPat.operator)
  | TypOp(CursorPosition.t, UHTyp.operator)
  | Line(CursorPosition.t, UHExp.line)
  | Rule(CursorPosition.t, UHExp.rule);

let cons = List.cons;

let rec get_frame = (ze: ZExp.t): t => [
  ExpBlock(ze),
  ...get_frame_zblock(ze),
]
and get_frame_zblock = ((_, zline, _): ZExp.zblock): t =>
  cons(ExpLine(zline), get_frame_zline(zline))
and get_frame_zline = (zline: ZExp.zline): t =>
  switch (zline) {
  | CursorL(_) => []
  | ExpLineZ(zopseq) => cons(ExpSeq(zopseq), get_frame_exp_zopseq(zopseq))
  | LetLineZE(_, zblock) =>
    cons(ExpBlock(zblock), get_frame_zblock(zblock))
  | LetLineZP(zpat, _) => cons(PatSeq(zpat), get_frame_pat(zpat))
  }
and get_frame_exp_zopseq = (ZOpSeq(_, zseq): ZExp.zopseq): t =>
  switch (zseq) {
  | ZOperand(zop, _) => cons(ExpOperand(zop), get_frame_exp_zoperand(zop))
  | ZOperator(zop, _) => [ExpOperator(zop)]
  }
and get_frame_exp_zoperand = (zoperand: ZExp.zoperand): t =>
  switch (zoperand) {
  | CursorE(_) => []
  | ApPaletteZ(_) => []
  | ParenthesizedZ(zexp)
  | LamZE(_, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => get_frame(zexp)
  | LamZP(_, zpat, _) => cons(PatSeq(zpat), get_frame_pat(zpat))
  | CaseZR(_, _, zrules) =>
    cons(ExpRules(zrules), get_frame_zrules(zrules))
  }
and get_frame_zrules = ((_, zrule, _): ZExp.zrules): t =>
  cons(ExpRule(zrule), get_frame_zrule(zrule))
and get_frame_zrule = (zrule: ZExp.zrule): t =>
  switch (zrule) {
  | CursorR(_) => []
  | RuleZP(zpat, _) => cons(PatSeq(zpat), get_frame_pat(zpat))
  | RuleZE(_, zexp) => get_frame(zexp)
  }
and get_frame_pat = (ZOpSeq(_, zseq): ZPat.zopseq): t =>
  switch (zseq) {
  | ZOperand(zop, _) => cons(PatOperand(zop), get_frame_pat_zoperand(zop))
  | ZOperator(zop, _) => [PatOperator(zop)]
  }
and get_frame_pat_zoperand = (zoperand: ZPat.zoperand): t =>
  switch (zoperand) {
  | CursorP(_) => []
  | ParenthesizedZ(zpat)
  | InjZ(_, _, zpat) => get_frame_pat(zpat)
  | TypeAnnZP(_, zop, _) => get_frame_pat_zoperand(zop)
  | TypeAnnZA(_, _, ztyp) => cons(TypSeq(ztyp), get_frame_typ(ztyp))
  }
and get_frame_typ = (ZOpSeq(_, zseq): ZTyp.zopseq): t =>
  switch (zseq) {
  | ZOperand(zop, _) => cons(TypOperand(zop), get_frame_typ_zoperand(zop))
  | ZOperator(zop, _) => [TypOperator(zop)]
  }
and get_frame_typ_zoperand = (zoperand: ZTyp.zoperand): t =>
  switch (zoperand) {
  | CursorT(_) => []
  | ParenthesizedZ(ztyp)
  | ListZ(ztyp) => get_frame_typ(ztyp)
  };

let frame = (zexp: ZExp.t): t => zexp |> get_frame |> List.rev;

let extract_cursor_term = (zexp: ZExp.t): cursor_term => {
  switch (frame(zexp)) {
  | [ExpLine(CursorL(c, s)), ..._] => Line(c, s)
  | [ExpOperand(CursorE(c, s)), ..._] => Exp(c, s)
  | [ExpOperator((c, s)), ..._] => ExpOp(c, s)
  | [ExpRule(CursorR(c, s)), ..._] => Rule(c, s)
  | [PatOperand(CursorP(c, s)), ..._] => Pat(c, s)
  | [PatOperator((c, s)), ..._] => PatOp(c, s)
  | [TypOperand(CursorT(c, s)), ..._] => Typ(c, s)
  | [TypOperator((c, s)), ..._] => TypOp(c, s)
  | frame =>
    print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(frame)));
    failwith("INVALID FRAME (extract_cursor_term)");
  };
};

let get_nearest_zopseq = (zexp: ZExp.t): option(ZExp.zopseq) => {
  let frame = frame(zexp);
  let is_expseq = s =>
    switch (s) {
    | ExpSeq(_) => true
    | _ => false
    };
  switch (List.find_opt(is_expseq, frame)) {
  | Some(ExpSeq(zopseq)) => Some(zopseq)
  | _ => None
  //print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(frame)));
  //failwith("INVALID FRAME (get_nearest_opseq)");
  };
};
let get_opParent = (zexp: ZExp.t): option(ZExp.zoperand) => {
  let frame = frame(zexp);
  let frame =
    switch (frame) {
    | [ExpOperand(_), ...xs]
    | xs => xs
    };
  let is_expoperand = s =>
    switch (s) {
    | ExpOperand(_) => true
    | _ => false
    };
  switch (List.find_opt(is_expoperand, frame)) {
  | Some(ExpOperand(zoperand)) => Some(zoperand)
  | _ => None
  };
};

/*
 to think about: transformations for moving stuff around
 that might be simpler in terms of frame...
 like how about moving lines up or down to different blocks
 or operator-operand pairs to different opseqs?
 actually d's stuff i guess

 maybe still worth doing rezipping transformations...
 like, to replace most local opseq:
 split frame at most local opseq : [cursorterm .... x] old_opseq [y .... zexp]
 then go thru [zexp ... y], ignoring Z part, and inserting new opseq for Z part when get to end
 is this actually any simpler?
  */
