open Sexplib.Std;
open OptUtil.Syntax;

[@deriving sexp]
type slice =
  | ExpBlock(ZExp.zblock)
  | Line(ZExp.zline)
  | ExpSeq(ZExp.zopseq)
  | ExpOperand(ZExp.zoperand)
  | ExpOperator(ZExp.zoperator)
  | Rules(ZExp.zrules)
  | Rule(ZExp.zrule)
  | PatSeq(ZPat.zopseq)
  | PatOperand(ZPat.zoperand)
  | PatOperator(ZPat.zoperator)
  | TypSeq(ZTyp.zopseq)
  | TypOperand(ZTyp.zoperand)
  | TypOperator(ZTyp.zoperator);

[@deriving sexp]
type slice_info = {
  slice,
  ty_e: option(HTyp.t),
  ty_a: option(HTyp.t),
  ctx: Contexts.t,
};

let mk_si =
    (~ctx, ~ty_e: option(HTyp.t), ~ty_a: option(HTyp.t), ~slice: slice)
    : slice_info => {
  ctx,
  slice,
  ty_e,
  ty_a,
};

[@deriving sexp]
type t = list(slice_info);

let actual = (~ctx: Contexts.t, slice: slice): option(HTyp.t) =>
  switch (slice) {
  | ExpBlock(zblock) => zblock |> ZExp.erase |> Statics_Exp.syn_block(ctx)
  | Line(CursorL(_, ExpLine(opseq))) => Statics_Exp.syn_opseq(ctx, opseq)
  | Line(_) => None
  | ExpSeq(zopseq) =>
    zopseq
    |> ZExp.erase_zopseq
    |> UHExp.set_err_status_opseq(NotInHole)
    |> Statics_Exp.syn_opseq(ctx)
  | ExpOperand(zoperand) =>
    zoperand
    |> ZExp.erase_zoperand
    |> UHExp.set_err_status_operand(NotInHole)
    |> Statics_Exp.syn_operand(ctx)
  | ExpOperator(_zoperator) => None // TODO(andrew)
  | Rules(zrules) =>
    // last arg was scrut_ty  but not sure that makes sense...
    Statics_Exp.syn_rules(ctx, ZExp.erase_zrules(zrules), HTyp.Hole)
  | Rule(zrule) =>
    Statics_Exp.syn_rule(ctx, ZExp.erase_zrule(zrule), HTyp.Hole)
  | PatSeq(zopseq) =>
    zopseq
    |> ZPat.erase_zopseq
    |> UHPat.set_err_status_opseq(NotInHole)
    |> Statics_Pat.syn_opseq(ctx)
    |> Option.map(((ty, _)) => ty)
  | PatOperand(zoperand) =>
    zoperand
    |> ZPat.erase_zoperand
    |> UHPat.set_err_status_operand(NotInHole)
    |> Statics_Pat.syn_operand(ctx)
    |> Option.map(((ty, _)) => ty)
  | PatOperator(_zoperator) => None // TODO(andrew)
  | TypSeq(_)
  | TypOperand(_)
  | TypOperator(_) => None
  };

let expected_ty_from_ty_mode: Statics.type_mode => HTyp.t =
  fun
  | Ana(ty) => ty
  | Syn => HTyp.Hole;

let expected =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), slice: slice): option(HTyp.t) =>
  switch (slice) {
  | ExpOperand(CursorE(_))
  | ExpOperand(ApPaletteZ(_))
  | PatOperand(CursorP(_))
  | Line(CursorL(_))
  | Rule(CursorR(_)) => None // these have no children
  | ExpBlock((_, _, [])) =>
    // last line inherits type (could be last ExpLine)
    ty_e
  | ExpBlock((_, _, [_, ..._])) =>
    // non-last lines have no expected type
    None
  | Line(LetLineZE(p, _)) =>
    // ctx already extended by get_ctx
    switch (Statics_Pat.syn(ctx, p)) {
    | Some((ty, _)) => Some(ty)
    | None => Some(HTyp.Hole)
    }
  | Line(LetLineZP(_)) =>
    // could incorporate def type if any
    None
  | Line(ExpLineZ(_)) => ty_e
  | ExpOperand(ParenthesizedZ(_))
  | PatOperand(ParenthesizedZ(_)) => ty_e
  | ExpOperand(InjZ(_, side, _)) =>
    let* ty = ty_e;
    let+ (ty1, ty2) = HTyp.matched_sum(ty);
    InjSide.pick(side, ty1, ty2);
  | PatOperand(InjZ(_, side, _)) =>
    let* ty = ty_e;
    let+ (ty1, ty2) = HTyp.matched_sum(ty);
    InjSide.pick(side, ty1, ty2);
  | ExpOperand(LamZE(_)) =>
    let* ty_e' = ty_e;
    let+ (_, ty_body) = HTyp.matched_arrow(ty_e');
    ty_body;
  | ExpOperand(LamZP(_)) =>
    let* ty_e' = ty_e;
    let+ (ty_pat, _) = HTyp.matched_arrow(ty_e');
    ty_pat;
  | PatOperand(TypeAnnZP(_, _, ann)) =>
    let ty_ann = UHTyp.expand(ann);
    let* ty_e' = ty_e;
    HTyp.consistent(ty_e', ty_ann) ? Some(ty_ann) : None;
  | PatOperand(TypeAnnZA(_, _, _)) => None
  | ExpOperand(CaseZE(_)) =>
    // could incoporate joint pattern type here if any...
    None
  | ExpOperand(CaseZR(_, scrut, _)) =>
    // let's pretend rules have type ty_scrut => ty_expected
    let* ty_e' = ty_e;
    let+ ty_scrut = Statics_Exp.syn(ctx, scrut);
    HTyp.Arrow(ty_scrut, ty_e');
  | Rules(_) => ty_e
  | Rule(RuleZP(_)) =>
    let* ty_e' = ty_e;
    let+ (ty_scrut, _) = HTyp.matched_arrow(ty_e');
    ty_scrut;
  | Rule(RuleZE(_)) =>
    let* ty_e' = ty_e;
    let+ (_, ty_body) = HTyp.matched_arrow(ty_e');
    ty_body;
  | ExpSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZExp.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    switch (ty_e) {
    | Some(ty_e) =>
      let+ ty_mode_operand =
        Statics_Exp.ana_nth_type_mode(ctx, operand_index, opseq, ty_e);
      expected_ty_from_ty_mode(ty_mode_operand);
    | None =>
      // TODO: not sure this case should be necessary...
      let+ ty_mode_operand =
        Statics_Exp.syn_nth_type_mode(ctx, operand_index, opseq);
      expected_ty_from_ty_mode(ty_mode_operand);
    };
  | PatSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZPat.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    let* ty_e' = ty_e;
    let+ ty_mode_operand =
      Statics_Pat.ana_nth_type_mode(ctx, operand_index, opseq, ty_e');
    expected_ty_from_ty_mode(ty_mode_operand);
  | ExpSeq(ZOpSeq(_, ZOperator(_zop, _))) =>
    //TODO(andrew): FIX adapt syn/ana_nth_type_mode to return operator types
    None
  | PatSeq(ZOpSeq(_, ZOperator(_))) =>
    // TODO(andrew): FIX adapt syn/ana_nth_type_mode to return operator types
    None
  | TypSeq(_)
  | TypOperand(_) => None
  | ExpOperator(_)
  | PatOperator(_)
  | TypOperator(_) => None
  };

let get_ctx_for_child =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), slice: slice): Contexts.t => {
  // TODO(andrew): does let body ctx get incorporated somewhere?
  //let body_ctx = Statics_Exp.extend_let_body_ctx(ctx, p, def);
  switch (slice) {
  | ExpBlock((prefix, _zline, _suffix)) =>
    //print_endline("get_ctx: calculating context for zline");
    switch (Statics_Exp.syn_lines(ctx, prefix)) {
    | None =>
      //P.p("new ctx (none): %s\n", Contexts.sexp_of_t(ctx));
      ctx
    | Some(ctx) =>
      //P.p("new ctx (some): %s\n", Contexts.sexp_of_t(ctx));
      ctx
    }
  | Line(LetLineZE(p, zblock)) =>
    zblock |> ZExp.erase |> Statics_Exp.extend_let_def_ctx(ctx, p)
  | ExpOperand(LamZE(_, p, _)) =>
    switch (
      {
        let* ty_e' = ty_e;
        let* (ty_p_given, _) = HTyp.matched_arrow(ty_e');
        Statics_Pat.ana(ctx, p, ty_p_given);
      }
    ) {
    | None => ctx
    | Some(ctx) => ctx
    }
  | Rule(RuleZE(pat, _)) =>
    switch (ty_e) {
    | Some(Arrow(pat_ty, _)) =>
      switch (Statics_Pat.ana(ctx, pat, pat_ty)) {
      | Some(ctx) => ctx
      | _ => ctx
      }
    | _ => ctx
    }
  //TODO(andrew): pattern cases!!!!
  | _ => ctx
  };
};

let get_zchild_slice = (slice: slice): list(slice) => {
  switch (slice) {
  | ExpBlock((_, zline, _)) => [Line(zline)]
  | Line(CursorL(_)) => []
  | Line(ExpLineZ(zopseq)) => [ExpSeq(zopseq)]
  | Line(LetLineZE(_, zblock)) => [ExpBlock(zblock)]
  | Line(LetLineZP(zopseq, _)) => [PatSeq(zopseq)]
  | ExpSeq(ZOpSeq(_, ZOperand(zoperand, _))) => [ExpOperand(zoperand)]
  | ExpSeq(ZOpSeq(_, ZOperator(zoperator, _))) => [ExpOperator(zoperator)]
  | ExpOperator(_zoperator) => [] // TODO(andrew)
  | ExpOperand(CursorE(_)) => []
  | ExpOperand(ApPaletteZ(_)) => []
  | ExpOperand(ParenthesizedZ(zblock))
  | ExpOperand(LamZE(_, _, zblock))
  | ExpOperand(InjZ(_, _, zblock))
  | ExpOperand(CaseZE(_, zblock, _)) => [ExpBlock(zblock)]
  | ExpOperand(LamZP(_, zopseq, _)) => [PatSeq(zopseq)]
  | ExpOperand(CaseZR(_, _, zrules)) => [Rules(zrules)]
  | Rules((_, zrule, _)) => [Rule(zrule)]
  | Rule(CursorR(_)) => []
  | Rule(RuleZP(zopseq, _)) => [PatSeq(zopseq)]
  | Rule(RuleZE(_, zblock)) => [ExpBlock(zblock)]
  | PatSeq(ZOpSeq(_, ZOperand(zoperand, _))) => [PatOperand(zoperand)]
  | PatSeq(ZOpSeq(_, ZOperator(zoperator, _))) => [PatOperator(zoperator)]
  | PatOperator(_zop) => [] // TODO(andrew)
  | PatOperand(CursorP(_)) => []
  | PatOperand(ParenthesizedZ(zopseq))
  | PatOperand(InjZ(_, _, zopseq)) => [PatSeq(zopseq)]
  | PatOperand(TypeAnnZA(_, _, zopseq)) => [TypSeq(zopseq)]
  | PatOperand(TypeAnnZP(_, zoperand, _)) => [PatOperand(zoperand)]
  | TypSeq(ZOpSeq(_, ZOperand(zoperand, _))) => [TypOperand(zoperand)]
  | TypSeq(ZOpSeq(_, ZOperator(zoperator, _))) => [TypOperator(zoperator)]
  | TypOperand(_tyoperand) => [] // TODO(andrew)
  | TypOperator(_tyoperator) => [] // TODO(andrew)
  };
};

open Statics;
let get_child_mode =
    (~ctx: Contexts.t, mode: option(Statics.type_mode), slice: slice)
    : option(Statics.type_mode) => {
  // first pass: assuming no error holes
  // ASSUME: ctx here get passed ctx from get_ctx
  switch (slice) {
  | ExpBlock((_, _, [])) => mode
  | ExpBlock((_, _, [_, ..._])) => Some(Syn) //non-last lines always syn
  | Line(CursorL(_)) => None // no child
  | Line(ExpLineZ(_)) => mode
  | Line(LetLineZE(p, _zdef)) =>
    // same as get_expected xcept Hole vs None
    let+ (ty, _) = Statics_Pat.syn(ctx, p);
    Ana(ty);
  | Line(LetLineZP(_, _)) => Some(Syn)
  | ExpSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZExp.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    let* mode' = mode;
    switch (mode') {
    | Ana(ty_e) =>
      Statics_Exp.ana_nth_type_mode(ctx, operand_index, opseq, ty_e)
    | Syn => Statics_Exp.syn_nth_type_mode(ctx, operand_index, opseq)
    };
  | ExpSeq(ZOpSeq(_, ZOperator(_zoperator, _))) =>
    // TODO??
    None
  | ExpOperator(_) => None // no child
  | ExpOperand(CursorE(_)) => None // no child
  | ExpOperand(ApPaletteZ(_)) => None // no child
  | ExpOperand(ParenthesizedZ(_)) => mode
  | ExpOperand(LamZE(_)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      let+ (_, ty_body) = HTyp.matched_arrow(ty);
      Ana(ty_body);
    | Syn => Some(Syn)
    };
  | ExpOperand(LamZP(_)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      let+ (ty_p_given, _) = HTyp.matched_arrow(ty);
      Ana(ty_p_given);
    | Syn => Some(Syn)
    };
  | ExpOperand(InjZ(_, side, _)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      let+ (ty1, ty2) = HTyp.matched_sum(ty);
      Ana(InjSide.pick(side, ty1, ty2));
    | Syn => Some(Syn)
    };
  | ExpOperand(CaseZE(_)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(_) => Some(Syn)
    | Syn => Some(Syn)
    };
  | ExpOperand(CaseZR(_, scrut, _)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      let+ ty_scrut = Statics_Exp.syn(ctx, scrut);
      Ana(HTyp.Arrow(ty_scrut, ty));
    | Syn => Some(Syn)
    };
  | Rules(_) => mode
  | Rule(CursorR(_)) => None // no child
  | Rule(RuleZP(_)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      let+ (ty_pat, _) = HTyp.matched_arrow(ty);
      Ana(ty_pat);
    | Syn => Some(Syn)
    // TODO: technically should be??:
    /*let+ (ty_pat, _) = HTyp.matched_arrow(ty);
      Ana(ty_pat);*/
    };
  | Rule(RuleZE(_)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      // need to get ctx from pattern to generate real body ty
      let+ (_, ty_body) = HTyp.matched_arrow(ty);
      Ana(ty_body);
    | Syn => Some(Syn) // correct?
    };
  | PatSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZPat.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    let* mode' = mode;
    switch (mode') {
    | Ana(ty_e) =>
      Statics_Pat.ana_nth_type_mode(ctx, operand_index, opseq, ty_e)
    | Syn => Statics_Pat.syn_nth_type_mode(ctx, operand_index, opseq)
    };
  | PatSeq(ZOpSeq(_, ZOperator(_zoperator, _))) =>
    // TODO??
    None
  | PatOperator(_) => None // no child
  | PatOperand(CursorP(_)) => None // no child
  | PatOperand(ParenthesizedZ(_)) => mode
  | PatOperand(InjZ(_, side, _)) =>
    let* mode' = mode;
    switch (mode') {
    | Ana(ty) =>
      let+ (tyL, tyR) = HTyp.matched_sum(ty);
      Ana(InjSide.pick(side, tyL, tyR));
    | Syn => Some(Syn)
    };
  | PatOperand(TypeAnnZA(_)) => None // ZA is type
  | PatOperand(TypeAnnZP(_, _, ann)) =>
    let* mode' = mode;
    let ty_ann = UHTyp.expand(ann);
    switch (mode') {
    | Ana(ty) => HTyp.consistent(ty, ty_ann) ? Some(Ana(ty_ann)) : None
    | Syn => Some(Ana(ty_ann))
    };
  | TypSeq(_)
  | TypOperand(_)
  | TypOperator(_) => None
  };
};

// *****************************************************************

let rec mk_frame =
        (slice: slice, ~ctx: Contexts.t, ~ty_e: option(HTyp.t))
        : list(slice_info) => {
  let head = mk_si(~ctx, ~slice, ~ty_e, ~ty_a=actual(~ctx, slice));
  let tail =
    switch (get_zchild_slice(slice)) {
    | [child_slice] =>
      let ctx_new = get_ctx_for_child(~ctx, ~ty_e, slice);
      let ty_e_new = expected(~ctx=ctx_new, ~ty_e, slice);
      // TODO: doublecheck logic about new_ctx getting used for ty_e_new
      // i.e. make sure we dont sometimes have to use ty_e_new for getting ctx_new
      mk_frame(child_slice, ~ctx=ctx_new, ~ty_e=ty_e_new);
    | _ => []
    };
  [head, ...tail];
};

let mk = (zexp: ZExp.t): t =>
  ExpBlock(zexp)
  |> mk_frame(~ctx=Contexts.empty, ~ty_e=Some(Hole))
  |> List.rev;

// *****************************************************************

let get_cursor_slice = (zexp: ZExp.t): option(slice_info) =>
  switch (mk(zexp)) {
  | [si, ..._] => Some(si)
  | [] => None
  };

let first_exp_operand = si =>
  switch (si.slice) {
  | ExpOperand(zop) => Some(zop)
  | _ => None
  };

let first_exp_seq_zopseq = si =>
  switch (si.slice) {
  | ExpSeq(zopseq) => Some(zopseq)
  | _ => None
  };

let first_exp_seq_ty_e = si =>
  switch (si) {
  | {slice: ExpSeq(_), ty_e, _} => ty_e
  | _ => None
  };

// omcaml 4.10.0 sneak peal:
let rec find_map = (f: 'a => option('b), xs: list('a)): option('b) => {
  switch (xs) {
  | [] => None
  | [x, ...xs'] =>
    switch (f(x)) {
    | None => find_map(f, xs')
    | x => x
    }
  };
};

let pop_exp_operand = frame =>
  switch (frame) {
  | [{slice: ExpOperand(_), _}, ...xs]
  | xs => xs
  };

let get_opParent = (zexp: ZExp.t): option(ZExp.zoperand) =>
  // skip cursor_term if it's an operand
  zexp |> mk |> pop_exp_operand |> find_map(first_exp_operand);

let enclosing_zopseq = (zexp: ZExp.t): option(ZExp.zopseq) =>
  zexp |> mk |> find_map(first_exp_seq_zopseq);

let enclosing_zopseq_expected_ty = (zexp: ZExp.t): option(HTyp.t) =>
  zexp |> mk |> find_map(first_exp_seq_ty_e);

let get_expected_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_slice(zexp);
  slice.ty_e;
};

let get_actual_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_slice(zexp);
  slice.ty_a;
};
