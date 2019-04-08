open SemanticsCommon;
open GeneralUtil;

type cursor_mode =
  /*
   *  # cursor in analytic position
   */
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  /* cursor is on a lambda with an argument type annotation */
  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  /* cursor is on a type inconsistent expression */
  | AnaWrongLength(
      int /* expected length */,
      int, /* got length */
      HTyp.t,
    ) /* expected type */
  /* cursor is on a tuple of the wrong length */
  | AnaFree(HTyp.t)
  /* cursor is on a free variable */
  | Analyzed(HTyp.t)
  /* none of the above and didn't go through subsumption */
  | AnaSubsumed(HTyp.t, HTyp.t)
  /* none of the above and went through subsumption */
  | AnaKeyword(HTyp.t, keyword)
  /* cursor is on a keyword */
  /*
   *  # cursor in synthetic position
   */
  | SynErrorArrow(HTyp.t /* expected */, HTyp.t) /* got */
  /* cursor is on the function position of an ap,
     and that expression does not synthesize a type
     with a matched arrow type */
  | SynMatchingArrow(HTyp.t, HTyp.t)
  /* cursor is on the function position of an ap,
     and that expression does synthesize a type
     with a matched arrow type */
  | SynFreeArrow(HTyp.t)
  /* cursor is on a free variable in the function
     position of an ap */
  | SynKeywordArrow(HTyp.t, keyword)
  /* cursor is on a keyword in the function position of an ap */
  | SynFree
  /* none of the above, cursor is on a free variable */
  | SynKeyword(keyword)
  /* cursor is on a keyword */
  | Synthesized(HTyp.t)
  /* none of the above */
  /*
   * # cursor in type position
   */
  | TypePosition
  /* (we will have a richer structure here later) */
  /*
   *  # cursor in analytic pattern position
   */
  | PatAnaTypeInconsistent(HTyp.t, HTyp.t)
  /* cursor is on a type inconsistent pattern */
  | PatAnaWrongLength(
      int /* expected length */,
      int, /* got length */
      HTyp.t,
    ) /* expected type */
  /* cursor is on a tuple pattern of the wrong length */
  | PatAnalyzed(HTyp.t)
  /* none of the above and didn't go through subsumption */
  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* none of the above and went through subsumption */
  | PatAnaKeyword(HTyp.t, keyword)
  /* cursor is on a keyword */
  /*
   * # cursor in synthetic pattern position
   */
  | PatSynthesized(HTyp.t)
  /* cursor is on a keyword */
  | PatSynKeyword(keyword)
  /*
   *  # cursor on line
   */
  | Line;

type cursor_sort =
  | IsExpr(UHExp.t)
  | IsPat(UHPat.t)
  | IsType
  | IsLine(UHExp.line)
  | IsBlock(UHExp.block);

type t = {
  mode: cursor_mode,
  sort: cursor_sort,
  side: cursor_pos,
  ctx: Contexts.t,
};

let mk_cursor_info = (mode, sort, side, ctx) => {mode, sort, side, ctx};

let update_sort = (ci: t, sort: cursor_sort): t => {
  let {mode, sort: _, side, ctx} = ci;
  {mode, sort, side, ctx};
};

let rec cursor_info_typ = (ctx: Contexts.t, zty: ZTyp.t): option(t) =>
  switch (zty) {
  | CursorTO(outer_cursor, _) =>
    Some(mk_cursor_info(TypePosition, IsType, O(outer_cursor), ctx))
  | CursorTI(inner_cursor, _) =>
    Some(mk_cursor_info(TypePosition, IsType, I(inner_cursor), ctx))
  | ParenthesizedZ(zty1)
  | ListZ(zty1)
  | OpSeqZ(_, zty1, _) => cursor_info_typ(ctx, zty1)
  };

let is_po = (po: UHPat.t_outer): cursor_sort => IsPat(PO(po));
let is_pi = (pi: UHPat.t_inner): cursor_sort => IsPat(PI(pi));

let rec _ana_cursor_found_pat =
        (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t)
        : option((cursor_mode, cursor_sort, Contexts.t)) =>
  switch (p) {
  | PO(po) => _ana_cursor_found_pat_outer(ctx, po, ty)
  | PI(pi) => _ana_cursor_found_pat_inner(ctx, pi, ty)
  }
and _ana_cursor_found_pat_outer =
    (ctx: Contexts.t, po: UHPat.t_outer, ty: HTyp.t)
    : option((cursor_mode, cursor_sort, Contexts.t)) =>
  switch (po) {
  /* in hole */
  | EmptyHole(_) => Some((PatAnaSubsumed(ty, Hole), is_po(po), ctx))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _)) =>
    let po_nih = UHPat.set_err_status_t_outer(NotInHole, po);
    switch (Statics.syn_pat_outer(ctx, po_nih)) {
    | None => None
    | Some((ty', _)) =>
      Some((PatAnaTypeInconsistent(ty, ty'), is_po(po), ctx))
    };
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | NumLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _)) => None
  /* not in hole */
  | Wild(NotInHole)
  | Var(NotInHole, _, _)
  | ListNil(NotInHole) => Some((PatAnalyzed(ty), is_po(po), ctx))
  | NumLit(NotInHole, _) => Some((PatAnaSubsumed(ty, Num), is_po(po), ctx))
  | BoolLit(NotInHole, _) =>
    Some((PatAnaSubsumed(ty, Bool), is_po(po), ctx))
  }
and _ana_cursor_found_pat_inner =
    (ctx: Contexts.t, pi: UHPat.t_inner, ty: HTyp.t)
    : option((cursor_mode, cursor_sort, Contexts.t)) =>
  switch (pi) {
  /* in hole */
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let pi_nih = UHPat.set_err_status_t_inner(NotInHole, pi);
    switch (Statics.syn_pat_inner(ctx, pi_nih)) {
    | None => None
    | Some((ty', _)) =>
      Some((PatAnaTypeInconsistent(ty, ty'), is_pi(pi), ctx))
    };
  | Inj(InHole(WrongLength, _), _, _) => None
  /* not in hole */
  | Inj(NotInHole, _, _) => Some((PatAnalyzed(ty), is_pi(pi), ctx))
  | Parenthesized(p1) =>
    switch (_ana_cursor_found_pat(ctx, p1, ty)) {
    | None => None
    | Some((mode, _, ctx)) => Some((mode, is_pi(pi), ctx))
    }
  | OpSeq(BinOp(NotInHole, Comma, _, _), _)
  | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
    Some((PatAnalyzed(ty), is_pi(pi), ctx))
  | OpSeq(BinOp(InHole(WrongLength, _), Comma, skel1, skel2), _) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
      let n_elts = ListMinTwo.length(UHPat.get_tuple(skel1, skel2));
      let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
      Some((PatAnaWrongLength(n_types, n_elts, ty), is_pi(pi), ctx));
    | _ => None
    }
  | OpSeq(BinOp(InHole(_, _), _, _, _), _) => None
  | OpSeq(Placeholder(_), _) => None
  | OpSeq(BinOp(_, Space, _, _), _) => None
  };

let ana_cursor_found_pat_outer =
    (
      ctx: Contexts.t,
      po: UHPat.t_outer,
      ty: HTyp.t,
      outer_cursor: outer_cursor,
    )
    : option(t) =>
  switch (_ana_cursor_found_pat_outer(ctx, po, ty)) {
  | None => None
  | Some((mode, sort, ctx)) =>
    Some(mk_cursor_info(mode, sort, O(outer_cursor), ctx))
  };
let ana_cursor_found_pat_inner =
    (
      ctx: Contexts.t,
      pi: UHPat.t_inner,
      ty: HTyp.t,
      inner_cursor: inner_cursor,
    )
    : option(t) =>
  switch (_ana_cursor_found_pat_inner(ctx, pi, ty)) {
  | None => None
  | Some((mode, sort, ctx)) =>
    Some(mk_cursor_info(mode, sort, I(inner_cursor), ctx))
  };

let rec syn_cursor_info_pat = (ctx: Contexts.t, zp: ZPat.t): option(t) =>
  switch (zp) {
  | CursorPO(outer_cursor, Var(_, InVHole(Keyword(k), _), _) as po) =>
    Some(
      mk_cursor_info(PatSynKeyword(k), is_po(po), O(outer_cursor), ctx),
    )
  | CursorPO(outer_cursor, po) =>
    switch (Statics.syn_pat_outer(ctx, po)) {
    | None => None
    | Some((ty, _)) =>
      Some(
        mk_cursor_info(
          PatSynthesized(ty),
          is_po(po),
          O(outer_cursor),
          ctx,
        ),
      )
    }
  | CursorPI(inner_cursor, pi) =>
    switch (Statics.syn_pat_inner(ctx, pi)) {
    | None => None
    | Some((ty, _)) =>
      Some(
        mk_cursor_info(
          PatSynthesized(ty),
          is_pi(pi),
          I(inner_cursor),
          ctx,
        ),
      )
    }
  | InjZ(_, _, zp1) => syn_cursor_info_pat(ctx, zp1)
  | ParenthesizedZ(zp1) => syn_cursor_info_pat(ctx, zp1)
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1);
  }
and syn_cursor_info_pat_skel =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: int,
      zp1: ZPat.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      syn_cursor_info_pat(ctx, zp1);
    } else {
      None;
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (syn_cursor_info_pat_skel(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None => syn_cursor_info_pat_skel(ctx, skel2, seq, n, zp1)
    }
  | BinOp(_, Space, skel1, skel2) =>
    switch (ana_cursor_info_pat_skel(ctx, skel1, seq, n, zp1, HTyp.Hole)) {
    | Some(_) as result => result
    | None => ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, Hole)
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (syn_cursor_info_pat_skel(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel_pat(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, ctx, _)) =>
        let list_ty = HTyp.List(ty_elt);
        ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, list_ty);
      }
    }
  }
and ana_cursor_info_pat =
    (ctx: Contexts.t, zp: ZPat.t, ty: HTyp.t): option(t) =>
  switch (zp) {
  | CursorPO(outer_cursor, Var(_, InVHole(Keyword(k), _), _) as po) =>
    Some(
      mk_cursor_info(
        PatAnaKeyword(ty, k),
        is_po(po),
        O(outer_cursor),
        ctx,
      ),
    )
  | CursorPO(outer_cursor, po) =>
    ana_cursor_found_pat_outer(ctx, po, ty, outer_cursor)
  | CursorPI(inner_cursor, pi) =>
    ana_cursor_found_pat_inner(ctx, pi, ty, inner_cursor)
  | InjZ(InHole(WrongLength, _), _, _) => None
  | InjZ(InHole(TypeInconsistent, _), _, _) => syn_cursor_info_pat(ctx, zp)
  | InjZ(NotInHole, side, zp1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      ana_cursor_info_pat(ctx, zp1, ty1);
    }
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_cursor_info_pat_skel(ctx, skel, seq, n, zp1, ty);
  | ParenthesizedZ(zp) => ana_cursor_info_pat(ctx, zp, ty)
  }
and ana_cursor_info_pat_skel =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: int,
      zp1: ZPat.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      ana_cursor_info_pat(ctx, zp1, ty);
    } else {
      None;
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1)
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (ana_cursor_info_pat_skel(ctx, skel1, seq, n, zp1, Hole)) {
      | Some(_) as result => result
      | None => ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, Hole)
      }
    | Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      switch (ListMinTwo.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_cursor_info_pat_skel(ctx, skel, seq, n, zp1, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
      let ana_zipped =
        List.fold_left(
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_cursor_info_pat_skel(ctx, skel, seq, n, zp1, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), _, _, _) => None
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_cursor_info_pat_skel(ctx, skel1, seq, n, zp1, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, ty_list);
      }
    }
  | BinOp(NotInHole, Space, _, _) =>
    syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1)
  };

let is_eo = (eo: UHExp.t_outer): cursor_sort => IsExpr(EO(eo));
let is_ei = (ei: UHExp.t_inner): cursor_sort => IsExpr(EI(ei));

let rec _ana_cursor_found_block =
        (ctx: Contexts.t, Block(lines, e): UHExp.block, ty: HTyp.t)
        : option(cursor_mode) =>
  switch (Statics.syn_lines(ctx, lines)) {
  | None => None
  | Some(ctx) =>
    switch (_ana_cursor_found_exp(ctx, e, ty)) {
    | None => None
    | Some((mode, _, _)) => Some(mode)
    }
  }
and _ana_cursor_found_exp =
    (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t)
    : option((cursor_mode, cursor_sort, Contexts.t)) =>
  switch (e) {
  | EO(eo) => _ana_cursor_found_exp_outer(ctx, eo, ty)
  | EI(ei) => _ana_cursor_found_exp_inner(ctx, ei, ty)
  }
and _ana_cursor_found_exp_outer =
    (ctx: Contexts.t, eo: UHExp.t_outer, ty: HTyp.t)
    : option((cursor_mode, cursor_sort, Contexts.t)) =>
  switch (eo) {
  /* in hole */
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _)) =>
    let eo_nih = UHExp.set_err_status_t_outer(NotInHole, eo);
    switch (Statics.syn_exp_outer(ctx, eo_nih)) {
    | None => None
    | Some(ty') => Some((AnaTypeInconsistent(ty, ty'), is_eo(eo), ctx))
    };
  | Var(InHole(WrongLength, _), _, _)
  | NumLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _)) => None
  /* not in hole */
  | Var(_, InVHole(Keyword(k), _), _) =>
    Some((AnaKeyword(ty, k), is_eo(eo), ctx))
  | Var(_, InVHole(Free, _), _) => Some((AnaFree(ty), is_eo(eo), ctx))
  | ListNil(NotInHole) => Some((Analyzed(ty), is_eo(eo), ctx))
  | EmptyHole(_)
  | Var(NotInHole, NotInVHole, _)
  | NumLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    switch (Statics.syn_exp_outer(ctx, eo)) {
    | None => None
    | Some(ty') => Some((AnaSubsumed(ty, ty'), is_eo(eo), ctx))
    }
  }
and _ana_cursor_found_exp_inner =
    (ctx: Contexts.t, ei: UHExp.t_inner, ty: HTyp.t)
    : option((cursor_mode, cursor_sort, Contexts.t)) =>
  switch (ei) {
  /* in hole */
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(InHole(TypeInconsistent, _), _, _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _)
  | OpSeq(BinOp(InHole(TypeInconsistent, _), _, _, _), _) =>
    let ei_nih = UHExp.set_err_status_t_inner(NotInHole, ei);
    switch (Statics.syn_exp_inner(ctx, ei_nih)) {
    | None => None
    | Some(ty') => Some((AnaTypeInconsistent(ty, ty'), is_ei(ei), ctx))
    };
  | Lam(InHole(WrongLength, _), _, _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(InHole(WrongLength, _), _, _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => None
  | OpSeq(BinOp(InHole(WrongLength, _), Comma, skel1, skel2), _) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
      let n_elts = ListMinTwo.length(UHExp.get_tuple(skel1, skel2));
      let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
      Some((AnaWrongLength(n_types, n_elts, ty), is_ei(ei), ctx));
    | _ => None
    }
  | OpSeq(BinOp(InHole(WrongLength, _), _, _, _), _) => None
  /* not in hole */
  | Case(NotInHole, _, _, _) => Some((Analyzed(ty), is_ei(ei), ctx))
  | Lam(NotInHole, _, ann, _) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        switch (HTyp.consistent(ty1_ann, ty1_given)) {
        | false => None
        | true =>
          Some((
            AnaAnnotatedLambda(ty, Arrow(ty1_ann, ty2)),
            is_ei(ei),
            ctx,
          ))
        };
      | None => Some((Analyzed(ty), is_ei(ei), ctx))
      }
    }
  | Inj(NotInHole, _, _) => Some((Analyzed(ty), is_ei(ei), ctx))
  | OpSeq(BinOp(NotInHole, Comma, _, _), _)
  | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
    Some((Analyzed(ty), is_ei(ei), ctx))
  | OpSeq(Placeholder(_), _) => None
  | OpSeq(BinOp(NotInHole, Plus, _, _), _)
  | OpSeq(BinOp(NotInHole, Times, _, _), _)
  | OpSeq(BinOp(NotInHole, LessThan, _, _), _)
  | OpSeq(BinOp(NotInHole, Space, _, _), _)
  | ApPalette(NotInHole, _, _, _) =>
    switch (Statics.syn_exp_inner(ctx, ei)) {
    | None => None
    | Some(ty') => Some((AnaSubsumed(ty, ty'), is_ei(ei), ctx))
    }
  | Parenthesized(block) =>
    switch (_ana_cursor_found_block(ctx, block, ty)) {
    | None => None
    | Some(mode) => Some((mode, is_ei(ei), ctx))
    }
  };

let ana_cursor_found_exp_outer =
    (
      ctx: Contexts.t,
      eo: UHExp.t_outer,
      ty: HTyp.t,
      outer_cursor: outer_cursor,
    )
    : option(t) =>
  switch (_ana_cursor_found_exp_outer(ctx, eo, ty)) {
  | None => None
  | Some((mode, sort, ctx)) =>
    Some(mk_cursor_info(mode, sort, O(outer_cursor), ctx))
  };
let ana_cursor_found_exp_inner =
    (
      ctx: Contexts.t,
      ei: UHExp.t_inner,
      ty: HTyp.t,
      inner_cursor: inner_cursor,
    )
    : option(t) =>
  switch (_ana_cursor_found_exp_inner(ctx, ei, ty)) {
  | None => None
  | Some((mode, sort, ctx)) =>
    Some(mk_cursor_info(mode, sort, I(inner_cursor), ctx))
  };

let rec syn_cursor_info_block =
        (ctx: Contexts.t, zblock: ZExp.zblock): option(t) =>
  switch (zblock) {
  | BlockZL((prefix, zline, _), _) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => None
    | Some(ctx) => syn_cursor_info_line(ctx, zline)
    }
  | BlockZE(lines, ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) => syn_cursor_info(ctx, ze)
    }
  }
and syn_cursor_info_line = (ctx: Contexts.t, zli: ZExp.zline): option(t) =>
  switch (zli) {
  | CursorLO(outer_cursor, lo) =>
    Some(mk_cursor_info(Line, IsLine(LO(lo)), O(outer_cursor), ctx))
  | CursorLI(inner_cursor, li) =>
    Some(mk_cursor_info(Line, IsLine(LI(li)), I(inner_cursor), ctx))
  | ExpLineZ(ze) => syn_cursor_info(ctx, ze)
  | LetLineZP(zp, ann, block) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      ana_cursor_info_pat(ctx, zp, ty1);
    | None =>
      switch (Statics.syn_block(ctx, block)) {
      | None => None
      | Some(ty1) => ana_cursor_info_pat(ctx, zp, ty1)
      }
    }
  | LetLineZA(_, zann, _) => cursor_info_typ(ctx, zann)
  | LetLineZE(p, ann, zblock) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase_block(zblock));
      ana_cursor_info_block(ctx1, zblock, ty1);
    | None => syn_cursor_info_block(ctx, zblock)
    }
  }
and syn_cursor_info = (ctx: Contexts.t, ze: ZExp.t): option(t) =>
  switch (ze) {
  | CursorEO(outer_cursor, Var(_, InVHole(Keyword(k), _), _) as eo) =>
    Some(
      mk_cursor_info(PatSynKeyword(k), is_eo(eo), O(outer_cursor), ctx),
    )
  | CursorEO(outer_cursor, Var(_, InVHole(Free, _), _) as eo) =>
    Some(mk_cursor_info(SynFree, is_eo(eo), O(outer_cursor), ctx))
  | CursorEO(outer_cursor, eo) =>
    switch (Statics.syn_exp_outer(ctx, eo)) {
    | None => None
    | Some(ty) =>
      Some(
        mk_cursor_info(Synthesized(ty), is_eo(eo), O(outer_cursor), ctx),
      )
    }
  | CursorEI(inner_cursor, ei) =>
    switch (Statics.syn_exp_inner(ctx, ei)) {
    | None => None
    | Some(ty) =>
      Some(
        mk_cursor_info(Synthesized(ty), is_ei(ei), I(inner_cursor), ctx),
      )
    }
  | ParenthesizedZ(zblock) => syn_cursor_info_block(ctx, zblock)
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_cursor_info_skel(ctx, skel, seq, n, ze0);
  | LamZP(_, zp, ann, _) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    ana_cursor_info_pat(ctx, zp, ty1);
  | LamZA(_, _, zann, _) => cursor_info_typ(ctx, zann)
  | LamZE(_, p, ann, zblock) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    switch (Statics.ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) => syn_cursor_info_block(ctx1, zblock)
    };
  | InjZ(_, _, zblock) => syn_cursor_info_block(ctx, zblock)
  | CaseZE(_, _, _, None)
  | CaseZR(_, _, _, None) => None
  | CaseZE(_, zblock, _, Some(_)) => syn_cursor_info_block(ctx, zblock)
  | CaseZR(_, block, zrules, Some(uty)) =>
    let ty = UHTyp.expand(uty);
    switch (Statics.syn_block(ctx, block)) {
    | None => None
    | Some(ty1) =>
      let zrule = GeneralUtil.ZList.prj_z(zrules);
      ana_cursor_info_rule(ctx, zrule, ty1, ty);
    };
  | CaseZA(_, _, _, zann) => cursor_info_typ(ctx, zann)
  | ApPaletteZ(_, _, _, zpsi) =>
    let (ty, zblock) = GeneralUtil.ZNatMap.prj_z_v(zpsi.zsplice_map);
    ana_cursor_info_block(ctx, zblock, ty);
  }
and ana_cursor_info_block =
    (ctx: Contexts.t, zblock: ZExp.zblock, ty: HTyp.t): option(t) =>
  switch (zblock) {
  | BlockZL((prefix, zline, _), _) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => None
    | Some(ctx) => syn_cursor_info_line(ctx, zline)
    }
  | BlockZE(lines, ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) => ana_cursor_info(ctx, ze, ty)
    }
  }
and ana_cursor_info = (ctx: Contexts.t, ze: ZExp.t, ty: HTyp.t): option(t) =>
  switch (ze) {
  | CursorEO(outer_cursor, eo) =>
    ana_cursor_found_exp_outer(ctx, eo, ty, outer_cursor)
  | CursorEI(inner_cursor, ei) =>
    ana_cursor_found_exp_inner(ctx, ei, ty, inner_cursor)
  /* zipper cases */
  | ParenthesizedZ(zblock) => ana_cursor_info_block(ctx, zblock, ty)
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_cursor_info_skel(ctx, skel, seq, n, ze0, ty);
  /* zipper in hole */
  | LamZP(InHole(WrongLength, _), _, _, _)
  | LamZA(InHole(WrongLength, _), _, _, _)
  | LamZE(InHole(WrongLength, _), _, _, _)
  | InjZ(InHole(WrongLength, _), _, _)
  | CaseZE(InHole(WrongLength, _), _, _, _)
  | CaseZR(InHole(WrongLength, _), _, _, _)
  | CaseZA(InHole(WrongLength, _), _, _, _)
  | ApPaletteZ(InHole(WrongLength, _), _, _, _) => None
  | LamZP(InHole(TypeInconsistent, _), _, _, _)
  | LamZA(InHole(TypeInconsistent, _), _, _, _)
  | LamZE(InHole(TypeInconsistent, _), _, _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(InHole(TypeInconsistent, _), _, _, _)
  | CaseZR(InHole(TypeInconsistent, _), _, _, _)
  | CaseZA(InHole(TypeInconsistent, _), _, _, _)
  | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _) =>
    syn_cursor_info(ctx, ze)
  /* zipper not in hole */
  | LamZP(NotInHole, zp, ann, _) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, _)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      ana_cursor_info_pat(ctx, zp, ty1);
    }
  | LamZA(NotInHole, _, zann, _) => cursor_info_typ(ctx, zann)
  | LamZE(NotInHole, p, ann, zblock) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx) => ana_cursor_info_block(ctx, zblock, ty2)
      };
    }
  | InjZ(NotInHole, side, zblock) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      ana_cursor_info_block(ctx, zblock, pick_side(side, ty1, ty2))
    }
  | CaseZE(NotInHole, zblock, _, _) => syn_cursor_info_block(ctx, zblock)
  | CaseZR(NotInHole, block, zrules, _) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => None
    | Some(ty1) =>
      let zrule = ZList.prj_z(zrules);
      ana_cursor_info_rule(ctx, zrule, ty1, ty);
    }
  | CaseZA(NotInHole, _, _, zann) => cursor_info_typ(ctx, zann)
  | ApPaletteZ(NotInHole, _, _, _) => syn_cursor_info(ctx, ze)
  }
and ana_cursor_info_rule =
    (ctx: Contexts.t, zrule: ZExp.zrule, pat_ty: HTyp.t, clause_ty: HTyp.t)
    : option(t) =>
  switch (zrule) {
  | CursorR(inner_cursor, _) =>
    /* TODO */
    Some(mk_cursor_info(TypePosition, IsType, I(inner_cursor), ctx))
  | RuleZP(zp, _) => ana_cursor_info_pat(ctx, zp, pat_ty)
  | RuleZE(p, zblock) =>
    switch (Statics.ana_pat(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) => ana_cursor_info_block(ctx, zblock, clause_ty)
    }
  }
and syn_cursor_info_skel =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: int,
      ze_n: ZExp.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      syn_cursor_info(ctx, ze_n);
    } else {
      None;
    }
  | BinOp(_, Plus, skel1, skel2)
  | BinOp(_, Times, skel1, skel2)
  | BinOp(_, LessThan, skel1, skel2) =>
    switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Num)) {
    | Some(_) as result => result
    | None =>
      switch (ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Num)) {
      | Some(_) as result => result
      | None => None
      }
    }
  /* TODO
     | BinOp(_, Space, Placeholder(n') as skel1, skel2) =>
          if (n == n') {
            let e_n = ZExp.erase(ze_n);
            switch (ZExp.cursor_on_outer_expr(ze_n)) {
            | Some((
                Block(_, Tm(InHole(TypeInconsistent, _), _)) as outer_block,
                side,
              )) =>
              let outer_block_nih =
                UHExp.set_err_status_block(NotInHole, outer_block);
              switch (Statics.syn_block(ctx, outer_block_nih)) {
              | None => None
              | Some(ty) =>
                Some(
                  mk_cursor_info(
                    SynErrorArrow(Arrow(Hole, Hole), ty),
                    IsExpr(e_n),
                    side,
                    ctx,
                  ),
                )
              };
            | Some((Block(_, Tm(_, Var(InVHole(Keyword(k), _), _))), side)) =>
              Some(
                mk_cursor_info(
                  SynKeywordArrow(Arrow(Hole, Hole), k),
                  IsExpr(e_n),
                  side,
                  ctx,
                ),
              )
            | Some((Block(_, Tm(_, Var(InVHole(Free, _), _))), side)) =>
              Some(
                mk_cursor_info(
                  SynFreeArrow(Arrow(Hole, Hole)),
                  IsExpr(e_n),
                  side,
                  ctx,
                ),
              )
            | Some((outer_block, side)) =>
              switch (Statics.syn_block(ctx, outer_block)) {
              | None => None
              | Some(ty) =>
                switch (HTyp.matched_arrow(ty)) {
                | None => None
                | Some((ty1, ty2)) =>
                  Some(
                    mk_cursor_info(
                      SynMatchingArrow(ty, Arrow(ty1, ty2)),
                      IsExpr(e_n),
                      side,
                      ctx,
                    ),
                  )
                }
              }
            | None => syn_cursor_info(ctx, ze_n)
            };
          } else {
            switch (Statics.syn_skel(ctx, skel1, seq, None)) {
            | None => None
            | Some((ty, _)) =>
              switch (HTyp.matched_arrow(ty)) {
              | None => None
              | Some((ty1, _)) =>
                ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty1)
              }
            };
          }
          */
  | BinOp(_, Space, Placeholder(_), _) => None
  | BinOp(_, Space, skel1, skel2) =>
    switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | None => None
        | Some((ty1, _)) =>
          ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty1)
        }
      }
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None => syn_cursor_info_skel(ctx, skel2, seq, n, ze_n)
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, _)) =>
        let ty_list = HTyp.List(ty_elt);
        ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  }
and ana_cursor_info_skel =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: int,
      ze_n: ZExp.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      ana_cursor_info(ctx, ze_n, ty);
    } else {
      None;
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Hole)) {
      | Some(_) as result => result
      | None => ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Hole)
      }
    | Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      switch (ListMinTwo.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_cursor_info_skel(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
      let ana_zipped =
        List.fold_left(
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_cursor_info_skel(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), _, _, _) => None
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  | BinOp(_, Plus, _, _)
  | BinOp(_, Times, _, _)
  | BinOp(_, LessThan, _, _)
  | BinOp(_, Space, _, _) => syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
  };
