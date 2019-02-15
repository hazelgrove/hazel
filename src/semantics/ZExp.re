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
  | ApPaletteZ(
      PaletteName.t,
      PaletteSerializedModel.t,
      (
        UHExp.PaletteHoleData.hole_ref_lbl,
        ZNatMap.t((HTyp.t, UHExp.t), (HTyp.t, t)),
      ),
    )
and zline_item =
  | EmptyLineZ
  | ExpLineZ(t)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.t)
  | LetLineZE(UHPat.t, option(UHTyp.t), t)
and zrule =
  | RuleZP(ZPat.t, UHExp.t)
  | RuleZE(UHPat.t, t);

type zrules = ZList.t(zrule, UHExp.rule);

module ZPaletteHoleData = {
  type z_hole_map = ZNatMap.t((HTyp.t, UHExp.t), (HTyp.t, t));
  type t = (UHExp.PaletteHoleData.hole_ref_lbl, z_hole_map);
};

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
  | ApPaletteZ(palette_name, serialized_model, zhole_data) =>
    let (next_hole_ref, zholemap) = zhole_data;
    let (holemap, z) = zholemap;
    let (hole_ref, tz) = z;
    let (ty, ze) = tz;
    let holemap' = NatMap.extend(holemap, (hole_ref, (ty, erase(ze))));
    let hole_data' = (next_hole_ref, holemap');
    UHExp.ApPalette(palette_name, serialized_model, hole_data');
  }
and erase_line_item = (zli: zline_item): UHExp.line_item =>
  switch (zli) {
  | EmptyLineZ => UHExp.EmptyLine
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

type cursor_mode =
  /* cursor in analytic position */
  | AnaOnly(HTyp.t)
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  | AnaWrongLength(
      nat /* expected length */,
      nat, /* got length */
      HTyp.t,
    ) /* expected type */
  | AnaFree(HTyp.t)
  | AnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic position */
  | SynOnly(HTyp.t)
  | SynFree
  | SynErrorArrow(HTyp.t /* expected */, HTyp.t) /* got */
  | SynMatchingArrow(HTyp.t, HTyp.t)
  | SynFreeArrow(HTyp.t)
  | SynEmptyLine
  /* cursor in type position */
  | TypePosition
  /* cursor in analytic pattern position */
  | PatAnaOnly(HTyp.t)
  | PatAnaTypeInconsistent(HTyp.t, HTyp.t)
  | PatAnaWrongLength(
      nat /* expected length */,
      nat, /* got length */
      HTyp.t,
    ) /* expected type */
  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic pattern position */
  | PatSynOnly(HTyp.t);

type cursor_sort =
  | IsExpr(UHExp.t)
  | IsPat(UHPat.t)
  | IsType
  | IsEmptyLine;

type cursor_info = {
  mode: cursor_mode,
  sort: cursor_sort,
  side: cursor_side,
  ctx: Contexts.t,
};

let mk_cursor_info = (mode, sort, side, ctx) => {mode, sort, side, ctx};

let update_sort = (ci: cursor_info, sort: cursor_sort): cursor_info => {
  let {mode, sort: _, side, ctx} = ci;
  {mode, sort, side, ctx};
};

let rec ana_pat_cursor_found =
        (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, side: cursor_side)
        : option(cursor_info) =>
  switch (p) {
  | UHPat.Parenthesized(p1) =>
    switch (ana_pat_cursor_found(ctx, p1, ty, side)) {
    | None => None
    | Some(ci) => Some(update_sort(ci, IsPat(p)))
    }
  | UHPat.Pat(InHole(TypeInconsistent, _), p') =>
    switch (UHExp.syn_pat'(ctx, p')) {
    | None => None
    | Some((ty', _)) =>
      Some(
        mk_cursor_info(
          PatAnaTypeInconsistent(ty, ty'),
          IsPat(p),
          side,
          ctx,
        ),
      )
    }
  | UHPat.Pat(NotInHole, UHPat.EmptyHole(_)) =>
    Some(
      mk_cursor_info(PatAnaSubsumed(ty, HTyp.Hole), IsPat(p), side, ctx),
    )
  | UHPat.Pat(NotInHole, UHPat.Wild)
  | UHPat.Pat(NotInHole, UHPat.Var(_)) =>
    Some(mk_cursor_info(PatAnaOnly(ty), IsPat(p), side, ctx))
  | UHPat.Pat(NotInHole, UHPat.NumLit(_)) =>
    Some(mk_cursor_info(PatAnaSubsumed(ty, HTyp.Num), IsPat(p), side, ctx))
  | UHPat.Pat(NotInHole, UHPat.BoolLit(_)) =>
    Some(
      mk_cursor_info(PatAnaSubsumed(ty, HTyp.Bool), IsPat(p), side, ctx),
    )
  | UHPat.Pat(NotInHole, UHPat.Inj(_, _)) =>
    Some(mk_cursor_info(PatAnaOnly(ty), IsPat(p), side, ctx))
  | UHPat.Pat(NotInHole, UHPat.ListNil) =>
    Some(mk_cursor_info(PatAnaOnly(ty), IsPat(p), side, ctx))
  /* | UHPat.Pat NotInHole (UHPat.ListLit _) ->
     Some
       (mk_cursor_info
         (PatAnaOnly ty)
         (IsPat p)
         side
         ctx) */
  | UHPat.Pat(
      NotInHole,
      UHPat.OpSeq(Skel.BinOp(NotInHole, Comma, skel1, skel2), seq),
    )
  | UHPat.Pat(
      NotInHole,
      UHPat.OpSeq(Skel.BinOp(NotInHole, Cons, skel1, skel2), seq),
    ) =>
    Some(mk_cursor_info(PatAnaOnly(ty), IsPat(p), side, ctx))
  | UHPat.Pat(
      InHole(WrongLength, _),
      UHPat.OpSeq(
        Skel.BinOp(InHole(WrongLength, _), Comma, skel1, skel2),
        _,
      ),
    ) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let n_elts = List.length(UHPat.get_tuple(skel1, skel2));
      let n_types = List.length(HTyp.get_tuple(ty1, ty2));
      Some(
        mk_cursor_info(
          PatAnaWrongLength(n_types, n_elts, ty),
          IsPat(p),
          side,
          ctx,
        ),
      );
    | _ => None
    }
  | UHPat.Pat(InHole(WrongLength, _), _) => None
  | UHPat.Pat(
      NotInHole,
      UHPat.OpSeq(Skel.BinOp(InHole(_, _), _, skel1, skel2), seq),
    ) =>
    None
  | UHPat.Pat(NotInHole, UHPat.OpSeq(Skel.Placeholder(_), _)) => None
  | UHPat.Pat(NotInHole, UHPat.OpSeq(Skel.BinOp(_, Space, _, _), _)) => None
  };

let rec syn_pat_cursor_info =
        (ctx: Contexts.t, zp: ZPat.t): option(cursor_info) =>
  switch (zp) {
  | ZPat.CursorP(side, p) =>
    switch (UHExp.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, _)) =>
      Some(mk_cursor_info(PatSynOnly(ty), IsPat(p), side, ctx))
    }
  | ZPat.Deeper(_, zp') => syn_pat_cursor_info'(ctx, zp')
  | ZPat.ParenthesizedZ(zp1) => syn_pat_cursor_info(ctx, zp1)
  }
and syn_pat_cursor_info' =
    (ctx: Contexts.t, zp': ZPat.t'): option(cursor_info) =>
  switch (zp') {
  | ZPat.InjZ(side, zp1) => syn_pat_cursor_info(ctx, zp1)
  /* | ZPat.ListLitZ ((prefix, zp), _) ->
     begin match prefix with
     | nil -> syn_pat_cursor_info ctx zp
     | cons _ _ ->
       let opt_result = List.fold_left (fun opt_result p ->
         begin match opt_result with
         | None -> None
         | Some (ty, ctx) ->
           begin match UHExp.syn_pat ctx p with
           | Some (ty', ctx) ->
             begin match HTyp.join ty ty' with
             | Some ty_joined -> Some (ty_joined, ctx)
             | None ->
               begin match UHExp.ana_pat ctx p ty with
               | None -> None
               | Some ctx -> Some (ty, ctx)
               end
             end
           | None ->
             begin match UHExp.ana_pat ctx p ty with
             | None -> None
             | Some ctx -> Some (ty, ctx)
             end
           end
         end) prefix (Some (HTyp.Hole, ctx)) in
       begin match opt_result with
       | None -> None
       | Some (ty, ctx) -> ana_pat_cursor_info ctx zp ty
       end
     end */
  | ZPat.OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1);
  }
and syn_skel_pat_cursor_info =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: nat,
      zp1: ZPat.t,
    )
    : option(cursor_info) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      syn_pat_cursor_info(ctx, zp1);
    } else {
      None;
    }
  | Skel.BinOp(_, UHPat.Comma, skel1, skel2) =>
    switch (syn_skel_pat_cursor_info(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None => syn_skel_pat_cursor_info(ctx, skel2, seq, n, zp1)
    }
  | Skel.BinOp(_, UHPat.Space, skel1, skel2) =>
    switch (syn_skel_pat_cursor_info(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None => syn_skel_pat_cursor_info(ctx, skel2, seq, n, zp1)
    }
  | Skel.BinOp(_, UHPat.Cons, skel1, skel2) =>
    switch (syn_skel_pat_cursor_info(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None =>
      switch (UHExp.syn_skel_pat(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, ctx, _)) =>
        let list_ty = HTyp.List(ty_elt);
        ana_skel_pat_cursor_info(ctx, skel2, seq, n, zp1, list_ty);
      }
    }
  }
and ana_pat_cursor_info =
    (ctx: Contexts.t, zp: ZPat.t, ty: HTyp.t): option(cursor_info) =>
  switch (zp) {
  | ZPat.CursorP(side, p) => ana_pat_cursor_found(ctx, p, ty, side)
  | ZPat.Deeper(InHole(TypeInconsistent, u), zp') =>
    syn_pat_cursor_info'(ctx, zp')
  | ZPat.Deeper(NotInHole, zp')
  | ZPat.Deeper(
      InHole(WrongLength, _),
      ZPat.OpSeqZ(Skel.BinOp(_, UHPat.Comma, _, _), _, _) as zp',
    ) =>
    ana_pat_cursor_info'(ctx, zp', ty)
  | ZPat.Deeper(InHole(WrongLength, _), _) => None
  | ZPat.ParenthesizedZ(zp) => ana_pat_cursor_info(ctx, zp, ty)
  }
and ana_pat_cursor_info' =
    (ctx: Contexts.t, zp': ZPat.t', ty: HTyp.t): option(cursor_info) =>
  switch (zp') {
  | ZPat.InjZ(side, zp1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      ana_pat_cursor_info(ctx, zp1, ty1);
    }
  /* | ZPat.ListLitZ zps ->
     begin match HTyp.matched_list ty with
     | None -> None
     | Some ty_elt ->
       let zp = ZList.prj_z zps in
       ana_pat_cursor_info ctx zp ty_elt
     end */
  | ZPat.OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_skel_pat_cursor_info(ctx, skel, seq, n, zp1, ty);
  }
and ana_skel_pat_cursor_info =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: nat,
      zp1: ZPat.t,
      ty: HTyp.t,
    )
    : option(cursor_info) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      ana_pat_cursor_info(ctx, zp1, ty);
    } else {
      None;
    }
  | Skel.BinOp(InHole(TypeInconsistent, _), _, skel1, skel2) =>
    syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1)
  | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (ana_skel_pat_cursor_info(ctx, skel1, seq, n, zp1, HTyp.Hole)) {
      | Some(_) as result => result
      | None => ana_skel_pat_cursor_info(ctx, skel2, seq, n, zp1, HTyp.Hole)
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      switch (Util.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_skel_pat_cursor_info(ctx, skel, seq, n, zp1, ty);
            },
          None,
          zipped,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
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
              ana_skel_pat_cursor_info(ctx, skel, seq, n, zp1, ty);
            },
          None,
          zipped,
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_skel_pat_cursor_info(ctx, skel1, seq, n, zp1, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_skel_pat_cursor_info(ctx, skel2, seq, n, zp1, ty_list);
      }
    }
  | Skel.BinOp(NotInHole, UHPat.Space, _, _) =>
    syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1)
  };

let rec ana_cursor_found =
        (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t, side: cursor_side)
        : option(cursor_info) =>
  switch (e) {
  | UHExp.Parenthesized(e') =>
    switch (ana_cursor_found(ctx, e', ty, side)) {
    | None => None
    | Some(ci) => Some(update_sort(ci, IsExpr(e)))
    }
  | UHExp.Tm(
      InHole(TypeInconsistent, _),
      UHExp.OpSeq(Skel.BinOp(_, op, skel1, skel2), surround),
    ) =>
    let e' = UHExp.OpSeq(Skel.BinOp(NotInHole, op, skel1, skel2), surround);
    switch (UHExp.syn'(ctx, e')) {
    | None => None
    | Some(ty') =>
      Some(
        mk_cursor_info(AnaTypeInconsistent(ty, ty'), IsExpr(e), side, ctx),
      )
    };
  | UHExp.Tm(InHole(TypeInconsistent, _), e') =>
    switch (UHExp.syn'(ctx, e')) {
    | None => 
      None
    | Some(ty') =>
      Some(
        mk_cursor_info(AnaTypeInconsistent(ty, ty'), IsExpr(e), side, ctx),
      )
    }
  | UHExp.Tm(_, UHExp.Var(InVHole(_), _)) =>
    Some(mk_cursor_info(AnaFree(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(NotInHole, UHExp.LineItem(_, _))
  | UHExp.Tm(NotInHole, UHExp.Case(_, _))
  | UHExp.Tm(NotInHole, UHExp.ListNil) =>
    /* | UHExp.Tm NotInHole (UHExp.ListLit _) */
    Some(mk_cursor_info(AnaOnly(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Comma, _, _), surround),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Cons, _, _), surround),
    ) =>
    Some(mk_cursor_info(AnaOnly(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(
      InHole(WrongLength, _),
      UHExp.OpSeq(
        Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, skel1, skel2),
        _,
      ),
    ) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let n_elts = List.length(UHExp.get_tuple(skel1, skel2));
      let n_types = List.length(HTyp.get_tuple(ty1, ty2));
      Some(
        mk_cursor_info(
          AnaWrongLength(n_types, n_elts, ty),
          IsExpr(e),
          side,
          ctx,
        ),
      );
    | _ => None
    }
  | UHExp.Tm(InHole(WrongLength, _), _) => None
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(InHole(WrongLength, _), _, _, _), _),
    ) =>
    None
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Plus, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Times, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.LessThan, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Space, _, _), _),
    )
  | UHExp.Tm(NotInHole, UHExp.EmptyHole(_))
  | UHExp.Tm(NotInHole, UHExp.Asc(_, _))
  | UHExp.Tm(NotInHole, UHExp.Var(NotInVHole, _))
  | UHExp.Tm(NotInHole, UHExp.NumLit(_))
  | UHExp.Tm(NotInHole, UHExp.BoolLit(_))
  | UHExp.Tm(NotInHole, UHExp.ApPalette(_, _, _)) =>
  { 
    switch (UHExp.syn(ctx, e)) {
    | Some(ty') =>
      if (HTyp.consistent(ty, ty')) {
        Some(mk_cursor_info(AnaSubsumed(ty, ty'), IsExpr(e), side, ctx));
      } else {
        None;
      }
    | None => None
    } }
  | UHExp.Tm(NotInHole, UHExp.Lam(_, ann, _)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        switch (HTyp.consistent(ty1_ann, ty1_given)) {
        | false => None
        | true =>
          Some(
            mk_cursor_info(
              AnaAnnotatedLambda(ty, HTyp.Arrow(ty1_ann, ty2)),
              IsExpr(e),
              side,
              ctx,
            ),
          )
        };
      | None => Some(mk_cursor_info(AnaOnly(ty), IsExpr(e), side, ctx))
      }
    }
  | UHExp.Tm(NotInHole, UHExp.Inj(_, _)) =>
    switch (ty) {
    | HTyp.Sum(_, _) =>
      Some(mk_cursor_info(AnaOnly(ty), IsExpr(e), side, ctx))
    | _ => None
    }
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(
        Skel.BinOp(InHole(TypeInconsistent, _), _, _, _),
        surround,
      ),
    ) =>
    None
  | UHExp.Tm(NotInHole, UHExp.OpSeq(Skel.Placeholder(_), surround)) => None
  };

let rec syn_cursor_info = (ctx: Contexts.t, ze: t): option(cursor_info) =>
  switch (ze) {
  | CursorE(side, UHExp.Tm(_, UHExp.Var(InVHole(_), _)) as e) =>
    Some(mk_cursor_info(SynFree, IsExpr(e), side, ctx))
  | CursorE(side, e) =>
    switch (UHExp.syn(ctx, e)) {
    | Some(ty) => Some(mk_cursor_info(SynOnly(ty), IsExpr(e), side, ctx))
    | None => None
    }
  | ParenthesizedZ(ze1) => syn_cursor_info(ctx, ze1)
  | Deeper(_, ze1') => syn_cursor_info'(ctx, ze1')
  }
and ana_cursor_info =
    (ctx: Contexts.t, ze: t, ty: HTyp.t): option(cursor_info) =>
  switch (ze) {
  | CursorE(side, e) => ana_cursor_found(ctx, e, ty, side)
  | ParenthesizedZ(ze1) => ana_cursor_info(ctx, ze1, ty)
  | Deeper(InHole(TypeInconsistent, u), ze1') => syn_cursor_info'(ctx, ze1')
  | Deeper(
      InHole(WrongLength, _),
      OpSeqZ(Skel.BinOp(_, UHExp.Comma, _, _), _, _) as ze1',
    )
  | Deeper(NotInHole, ze1') => ana_cursor_info'(ctx, ze1', ty)
  | Deeper(InHole(WrongLength, _), _) => None
  }
and syn_cursor_info' = (ctx: Contexts.t, ze: t'): option(cursor_info) =>
  switch (ze) {
  | AscZ1(ze1, uty) =>
    let ty = UHTyp.expand(uty);
    let e1 = erase(ze1);
    if (UHExp.bidelimited(e1)) {
      ana_cursor_info(ctx, ze1, ty);
    } else {
      None;
    };
  | AscZ2(e1, zty) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, /* TODO fix this once we use cursor info in type position! */
        ctx,
      ),
    )
  | LineItemZL(zli, e1) => syn_line_item_cursor_info(ctx, zli)
  | LineItemZE(li, ze1) =>
    switch (UHExp.syn_line_item(ctx, li)) {
    | None => None
    | Some(ctx) => syn_cursor_info(ctx, ze1)
    }
  | LamZP(zp, ann, _) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    ana_pat_cursor_info(ctx, zp, ty1);
  | LamZA(_, zann, _) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, /* TODO fix this once we use cursor info in type position */
        ctx,
      ),
    )
  | LamZE(p, ann, ze1) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    switch (UHExp.ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) => syn_cursor_info(ctx1, ze1)
    };
  | InjZ(side, ze1) => syn_cursor_info(ctx, ze1)
  /* | ListLitZ ((prefix, ze), _) ->
     begin match prefix with
     | nil -> syn_cursor_info ctx ze
     | cons _ _ ->
       let opt_result = List.fold_left (fun opt_result e ->
         begin match opt_result with
         | None -> None
         | Some ty ->
           begin match UHExp.syn ctx e with
           | None -> None
           | Some ty' ->
             begin match HTyp.join ty ty' with
             | Some ty_joined -> Some ty_joined
             | None ->
               begin match UHExp.ana ctx e ty with
               | None -> None
               | Some _ -> Some ty
               end
             end
           end
         end) prefix (Some HTyp.Hole) in
       begin match opt_result with
       | None -> None
       | Some ty -> ana_cursor_info ctx ze ty
       end
     end */
  | CaseZE(_, _)
  | CaseZR(_, _) => None
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_skel_cursor_info(ctx, skel, seq, n, ze0);
  | ApPaletteZ(_, _, zholedata) =>
    let (_, zholemap) = zholedata;
    let (_, tz) = zholemap;
    let (_, tz') = tz;
    let (ty, ze) = tz';
    ana_cursor_info(ctx, ze, ty);
  }
and syn_line_item_cursor_info = (ctx, zli) =>
  switch (zli) {
  | EmptyLineZ =>
    Some(mk_cursor_info(SynEmptyLine, IsEmptyLine, Before, ctx))
  | ExpLineZ(ze) => syn_cursor_info(ctx, ze)
  | LetLineZP(zp, ann, e1) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      ana_pat_cursor_info(ctx, zp, ty1);
    | None =>
      switch (UHExp.syn(ctx, e1)) {
      | None => None
      | Some(ty1) => ana_pat_cursor_info(ctx, zp, ty1)
      }
    }
  | LetLineZA(p, zann, e1) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, /* TODO fix this once we use cursor info in type position! */
        ctx,
      ),
    )
  | LetLineZE(p, ann, ze1) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = UHExp.ctx_for_let(ctx, p, ty1, erase(ze1));
      ana_cursor_info(ctx1, ze1, ty1);
    | None => syn_cursor_info(ctx, ze1)
    }
  }
and ana_cursor_info' =
    (ctx: Contexts.t, ze: t', ty: HTyp.t): option(cursor_info) =>
  switch (ze) {
  | LineItemZL(zli, e1) => syn_line_item_cursor_info(ctx, zli)
  | LineItemZE(li, ze1) =>
    switch (UHExp.syn_line_item(ctx, li)) {
    | None => None
    | Some(ctx) => ana_cursor_info(ctx, ze1, ty)
    }
  | LamZP(p, ann, e) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      ana_pat_cursor_info(ctx, p, ty1);
    }
  | LamZA(_, zann, _) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, /* TODO fix this once we use cursor info in type position */
        ctx,
      ),
    )
  | LamZE(p, ann, ze1) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (UHExp.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx) => 
        ana_cursor_info(ctx, ze1, ty2)
      };
    }
  | InjZ(side, ze1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      ana_cursor_info(ctx, ze1, pick_side(side, ty1, ty2))
    }
  /* | ListLitZ zes ->
     begin match HTyp.matched_list ty with
     | None -> None
     | Some ty_elt ->
       let ze0 = ZList.prj_z zes in
       ana_cursor_info ctx ze0 ty_elt
     end */
  | CaseZE(ze1, rules) => syn_cursor_info(ctx, ze1)
  | CaseZR(e1, zrules) =>
    switch (UHExp.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      let zrule = ZList.prj_z(zrules);
      ana_rule_cursor_info(ctx, zrule, ty1, ty);
    }
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_skel_cursor_info(ctx, skel, seq, n, ze0, ty);
  | AscZ1(_, _)
  | AscZ2(_, _)
  | ApPaletteZ(_, _, _) => syn_cursor_info'(ctx, ze)
  }
and ana_rule_cursor_info =
    (ctx: Contexts.t, zrule: zrule, pat_ty: HTyp.t, clause_ty: HTyp.t)
    : option(cursor_info) =>
  switch (zrule) {
  | RuleZP(zp, e) => ana_pat_cursor_info(ctx, zp, pat_ty)
  | RuleZE(p, ze) =>
    switch (UHExp.ana_pat(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) => ana_cursor_info(ctx, ze, clause_ty)
    }
  }
and syn_skel_cursor_info =
    (ctx: Contexts.t, skel: UHExp.skel_t, seq: UHExp.opseq, n: nat, ze_n: t)
    : option(cursor_info) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      syn_cursor_info(ctx, ze_n);
    } else {
      None;
    }
  | Skel.BinOp(_, UHExp.Plus, skel1, skel2)
  | Skel.BinOp(_, UHExp.Times, skel1, skel2)
  | Skel.BinOp(_, UHExp.LessThan, skel1, skel2) =>
    switch (ana_skel_cursor_info(ctx, skel1, seq, n, ze_n, HTyp.Num)) {
    | Some(_) as result => result
    | None =>
      switch (ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, HTyp.Num)) {
      | Some(_) as result => result
      | None => None
      }
    }
  | Skel.BinOp(_, UHExp.Space, Skel.Placeholder(n') as skel1, skel2) =>
    if (n == n') {
      switch (cursor_on_outer_expr(ze_n)) {
      | Some((UHExp.Tm(InHole(TypeInconsistent, u), e_n') as e_n, side)) =>
        switch (UHExp.syn'(ctx, e_n')) {
        | Some(ty) =>
          Some(
            mk_cursor_info(
              SynErrorArrow(HTyp.Arrow(HTyp.Hole, HTyp.Hole), ty),
              IsExpr(e_n),
              side,
              ctx,
            ),
          )
        | None => None
        }
      | Some((UHExp.Tm(_, UHExp.Var(InVHole(_), _)) as e_n, side)) =>
        Some(
          mk_cursor_info(
            SynFreeArrow(HTyp.Arrow(HTyp.Hole, HTyp.Hole)),
            IsExpr(e_n),
            side,
            ctx,
          ),
        )
      | Some((e_n, side)) =>
        switch (UHExp.syn(ctx, e_n)) {
        | Some(ty) =>
          switch (HTyp.matched_arrow(ty)) {
          | Some((ty1, ty2)) =>
            Some(
              mk_cursor_info(
                SynMatchingArrow(ty, HTyp.Arrow(ty1, ty2)),
                IsExpr(e_n),
                side,
                ctx,
              ),
            )
          | None => None
          }
        | None => None
        }
      | None => syn_cursor_info(ctx, ze_n)
      };
    } else {
      switch (UHExp.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | Some((ty1, ty2)) =>
          ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty1)
        | None => None
        }
      };
    }
  | Skel.BinOp(_, UHExp.Space, skel1, skel2) =>
    switch (syn_skel_cursor_info(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (UHExp.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | None => None
        | Some((ty1, ty2)) =>
          ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty1)
        }
      }
    }
  | Skel.BinOp(_, UHExp.Comma, skel1, skel2) =>
    switch (syn_skel_cursor_info(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None => syn_skel_cursor_info(ctx, skel2, seq, n, ze_n)
    }
  | Skel.BinOp(_, UHExp.Cons, skel1, skel2) =>
    switch (syn_skel_cursor_info(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (UHExp.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, _)) =>
        let ty_list = HTyp.List(ty_elt);
        ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  }
and ana_skel_cursor_info =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: nat,
      ze_n: t,
      ty: HTyp.t,
    )
    : option(cursor_info) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      ana_cursor_info(ctx, ze_n, ty);
    } else {
      None;
    }
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    syn_skel_cursor_info(ctx, skel, seq, n, ze_n)
  | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (ana_skel_cursor_info(ctx, skel1, seq, n, ze_n, HTyp.Hole)) {
      | Some(_) as result => result
      | None => ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, HTyp.Hole)
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      switch (Util.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_skel_cursor_info(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          zipped,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
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
              ana_skel_cursor_info(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          zipped,
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_skel_cursor_info(ctx, skel, seq, n, ze_n)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_skel_cursor_info(ctx, skel1, seq, n, ze_n, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  | Skel.BinOp(_, UHExp.Plus, _, _)
  | Skel.BinOp(_, UHExp.Times, _, _)
  | Skel.BinOp(_, UHExp.LessThan, _, _)
  | Skel.BinOp(_, UHExp.Space, _, _) =>
    syn_skel_cursor_info(ctx, skel, seq, n, ze_n)
  };