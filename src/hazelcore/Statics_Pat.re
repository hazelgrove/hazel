open OptUtil.Syntax;

let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHPat.get_tuple_elements);

let rec syn =
        (ctx: Contexts.t, u_gen: MetaVarGen.t, p: UHPat.t)
        : option((HTyp.t, Contexts.t, MetaVarGen.t)) =>
  syn_opseq(ctx, u_gen, p)
and syn_opseq =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, OpSeq(skel, seq): UHPat.opseq)
    : option((HTyp.t, Contexts.t, MetaVarGen.t)) =>
  syn_skel(ctx, u_gen, skel, seq)
and syn_skel =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, skel: UHPat.skel, seq: UHPat.seq)
    : option((HTyp.t, Contexts.t, MetaVarGen.t)) =>
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    syn_operand(ctx, u_gen, pn);
  | BinOp(InHole(_), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ (_, ctx, u_gen) = syn_skel(ctx, u_gen, skel_not_in_hole, seq);
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    (ty, ctx, u_gen);
  | BinOp(NotInHole, Comma, _, _) =>
    skel
    |> UHPat.get_tuple_elements
    |> ListUtil.map_with_accumulator_opt(
         ((ctx, u_gen), skel) => {
           let+ (ty, ctx, u_gen) = syn_skel(ctx, u_gen, skel, seq);
           ((ctx, u_gen), ty);
         },
         (ctx, u_gen),
       )
    |> Option.map((((ctx, u_gen), tys)) => (HTyp.Prod(tys), ctx, u_gen))
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let (ty1, _, u_gen) = HTyp.new_Hole(u_gen);
    let (ty2, _, u_gen) = HTyp.new_Hole(u_gen);
    let* (ctx, u_gen) = ana_skel(ctx, u_gen, skel1, seq, ty1);
    let+ (ctx, u_gen) = ana_skel(ctx, u_gen, skel2, seq, ty2);
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    (ty, ctx, u_gen);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* (ty1, ctx, u_gen) = syn_skel(ctx, u_gen, skel1, seq);
    let ty = HTyp.List(ty1);
    let+ (ctx, u_gen) = ana_skel(ctx, u_gen, skel2, seq, ty);
    (ty, ctx, u_gen);
  }
and syn_operand =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, operand: UHPat.operand)
    : option((HTyp.t, Contexts.t, MetaVarGen.t)) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    Some((ty, ctx, u_gen));
  | InvalidText(_) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    Some((ty, ctx, u_gen));
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _)
  | TypeAnn(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    let+ (_, gamma, u_gen) = syn_operand(ctx, u_gen, operand');
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    (ty, gamma, u_gen);
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _)
  | TypeAnn(InHole(WrongLength, _), _, _) => None
  /* not in hole */
  | Wild(NotInHole) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    Some((ty, ctx, u_gen));
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(_), _), _) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    Some((ty, ctx, u_gen));
  | Var(NotInHole, NotInVarHole, x) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    let delta = Contexts.extend_gamma(ctx, (x, ty));
    Var.check_valid(x, Some((ty, delta, u_gen)));
  | IntLit(NotInHole, _) => Some((Int, ctx, u_gen))
  | FloatLit(NotInHole, _) => Some((Float, ctx, u_gen))
  | BoolLit(NotInHole, _) => Some((Bool, ctx, u_gen))
  | ListNil(NotInHole) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    Some((List(ty), ctx, u_gen));
  | Inj(NotInHole, inj_side, p1) =>
    let+ (ty1, ctx, u_gen) = syn(ctx, u_gen, p1);
    let (hole, _, u_gen) = HTyp.new_Hole(u_gen);
    let ty =
      switch (inj_side) {
      | L => HTyp.Sum(ty1, hole)
      | R => HTyp.Sum(hole, ty1)
      };
    (ty, ctx, u_gen);
  | Parenthesized(p) => syn(ctx, u_gen, p)
  | TypeAnn(NotInHole, op, ann) =>
    let* (hty, _, _, u_gen) =
      Elaborator_Typ.syn(ctx, u_gen, Delta.empty, ann);
    let+ (op_ctx, u_gen) = ana_operand(ctx, u_gen, op, hty);
    (hty, op_ctx, u_gen);
  }
and ana =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, p: UHPat.t, ty: HTyp.t)
    : option((Contexts.t, MetaVarGen.t)) =>
  ana_opseq(ctx, u_gen, p, ty)
and ana_opseq =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : option((Contexts.t, MetaVarGen.t)) =>
  switch (tuple_zip(skel, ty)) {
  | None =>
    switch (UHPat.get_err_status_opseq(opseq), HTyp.get_prod_elements(ty)) {
    | (InHole(TypeInconsistent, _), [_])
    | (InHole(WrongLength, _), _) =>
      let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
      let+ (_, ctx', u_gen) = syn_opseq(ctx, u_gen, opseq');
      (ctx', u_gen);
    | _ => None
    }
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (acc: option((Contexts.t, MetaVarGen.t)), (skel, ty)) => {
           let* (ctx, u_gen) = acc;
           ana_skel(ctx, u_gen, skel, seq, ty);
         },
         Some((ctx, u_gen)),
       )
  }
and ana_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : option((Contexts.t, MetaVarGen.t)) =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    failwith("Pat.ana_skel: expected tuples to be handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    ana_operand(ctx, u_gen, pn, ty);
  | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ (_, ctx, u_gen) = syn_skel(ctx, u_gen, skel_not_in_hole, seq);
    (ctx, u_gen);
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    let* (ctx, u_gen) = ana_skel(ctx, u_gen, skel1, seq, ty);
    ana_skel(ctx, u_gen, skel2, seq, ty);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* ty_elt = HTyp.matched_list(ty);
    let* (ctx, u_gen) = ana_skel(ctx, u_gen, skel1, seq, ty_elt);
    ana_skel(ctx, u_gen, skel2, seq, HTyp.List(ty_elt));
  }
and ana_operand =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, operand: UHPat.operand, ty: HTyp.t)
    : option((Contexts.t, MetaVarGen.t)) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some((ctx, u_gen))
  | InvalidText(_) => Some((ctx, u_gen))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | TypeAnn(InHole(TypeInconsistent, _), _, _)
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    let+ (_, ctx, u_gen) = syn_operand(ctx, u_gen, operand');
    (ctx, u_gen);
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | TypeAnn(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _) =>
    ty |> HTyp.get_prod_elements |> List.length > 1
      ? Some((ctx, u_gen)) : None
  /* not in hole */
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(_), _), _) => Some((ctx, u_gen))
  | Var(NotInHole, NotInVarHole, x) =>
    Var.check_valid(x, Some((Contexts.extend_gamma(ctx, (x, ty)), u_gen)))
  | Wild(NotInHole) => Some((ctx, u_gen))
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    let* (ty', ctx', u_gen) = syn_operand(ctx, u_gen, operand);
    HTyp.consistent(Contexts.typing(ctx), ty, ty')
      ? Some((ctx', u_gen)) : None;
  | ListNil(NotInHole) =>
    let+ _ = HTyp.matched_list(ty);
    (ctx, u_gen);
  | Inj(NotInHole, side, p1) =>
    let* (tyL, tyR) = HTyp.matched_sum(ty);
    let ty1 = InjSide.pick(side, tyL, tyR);
    ana(ctx, u_gen, p1, ty1);
  | Parenthesized(p) => ana(ctx, u_gen, p, ty)
  | TypeAnn(NotInHole, op, ann) =>
    let* (hty, _, _, u_gen) =
      Elaborator_Typ.syn(ctx, u_gen, Delta.empty, ann);
    HTyp.consistent(Contexts.typing(ctx), ty, hty)
      ? ana_operand(ctx, u_gen, op, hty) : None;
  };

let rec syn_nth_type_mode =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          n: int,
          OpSeq(skel, seq): UHPat.opseq,
        )
        : option((Statics.type_mode, MetaVarGen.t)) =>
  syn_nth_type_mode'(ctx, u_gen, n, skel, seq)
and syn_nth_type_mode' =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      n: int,
      skel: UHPat.skel,
      seq: UHPat.seq,
    )
    : option((Statics.type_mode, MetaVarGen.t)) => {
  let ana_go = (skel, ty, u_gen) =>
    ana_nth_type_mode'(ctx, u_gen, n, skel, seq, ty);
  let rec go = (skel: UHPat.skel, u_gen: MetaVarGen.t) =>
    switch (skel) {
    | Placeholder(n') =>
      assert(n == n');
      Some((Statics.Syn, u_gen));
    | BinOp(InHole(_), op, skel1, skel2) =>
      go(BinOp(NotInHole, op, skel1, skel2), u_gen)
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, u_gen) : go(skel2, u_gen)
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_skel(ctx, u_gen, skel1, seq)) {
      | None => None
      | Some((ty1, _, u_gen)) =>
        if (n <= Skel.rightmost_tm_index(skel1)) {
          go(skel1, u_gen);
        } else {
          let* (ty2, _, u_gen) = HTyp.matched_arrow(ty1, u_gen);
          ana_go(skel2, ty2, u_gen);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* (ty1, _, u_gen) = syn_skel(ctx, u_gen, skel1, seq);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, u_gen) : ana_go(skel2, HTyp.List(ty1), u_gen);
    };
  go(skel, u_gen);
}
and ana_nth_type_mode =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      n: int,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : option((Statics.type_mode, MetaVarGen.t)) => {
  // handle n-tuples
  switch (tuple_zip(skel, ty)) {
  | None =>
    syn_nth_type_mode(
      ctx,
      u_gen,
      n,
      UHPat.set_err_status_opseq(NotInHole, opseq),
    )
  | Some(skel_tys) =>
    let (nskel, nty) =
      skel_tys
      |> List.find(((skel, _)) =>
           Skel.leftmost_tm_index(skel) <= n
           && n <= Skel.rightmost_tm_index(skel)
         );
    ana_nth_type_mode'(ctx, u_gen, n, nskel, seq, nty);
  };
}
and ana_nth_type_mode' =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      n: int,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : option((Statics.type_mode, MetaVarGen.t)) => {
  let rec go = (skel: UHPat.skel, ty: HTyp.t, u_gen: MetaVarGen.t) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      failwith(__LOC__ ++ ": expected tuples to be handled at opseq level")
    | Placeholder(n') =>
      assert(n == n');
      Some((Statics.Ana(ty), u_gen));
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      syn_nth_type_mode'(ctx, u_gen, n, skel_not_in_hole, seq);
    | BinOp(NotInHole, Space, skel1, skel2) =>
      let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, ty, u_gen) : go(skel2, ty, u_gen);
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty_elt = HTyp.matched_list(ty);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, ty_elt, u_gen) : go(skel2, ty, u_gen);
    };
  go(skel, ty, u_gen);
};

let rec syn_fix_holes =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          ~renumber_empty_holes=false,
          p: UHPat.t,
        )
        : (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t) =>
  syn_fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, p)
and syn_fix_holes_opseq =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq): UHPat.opseq,
    )
    : (UHPat.opseq, HTyp.t, Contexts.t, MetaVarGen.t) => {
  let (skel, seq, ty, ctx, u_gen) =
    syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
  (OpSeq(skel, seq), ty, ctx, u_gen);
}
and syn_fix_holes_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel,
      seq: UHPat.seq,
    )
    : (UHPat.skel, UHPat.seq, HTyp.t, Contexts.t, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let (pn, ty, ctx, u_gen) =
      syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, pn);
    let seq = seq |> Seq.update_nth_operand(n, pn);
    (skel, seq, ty, ctx, u_gen);
  | BinOp(_, Comma, _, _) =>
    let ((ctx, u_gen, seq), pairs) =
      skel
      |> UHPat.get_tuple_elements
      |> ListUtil.map_with_accumulator(
           ((ctx, u_gen, seq), skel) => {
             let (skel, seq, ty, ctx, u_gen) =
               syn_fix_holes_skel(
                 ctx,
                 u_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
               );
             ((ctx, u_gen, seq), (skel, ty));
           },
           (ctx, u_gen, seq),
         );
    let (skels, tys) = List.split(pairs);
    (UHPat.mk_tuple(skels), seq, Prod(tys), ctx, u_gen);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, u_gen) = {
      let (skel1, seq, ty, ctx, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let (skel1, seq, u_gen) =
        switch (HTyp.matched_arrow(ty, u_gen)) {
        | Some(_) => (skel1, seq, u_gen)
        | None =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let OpSeq(skel1, seq) =
            UHPat.set_err_status_opseq(
              InHole(TypeInconsistent, u),
              OpSeq(skel1, seq),
            );
          (skel1, seq, u_gen);
        };
      (skel1, seq, ctx, u_gen);
    };
    let (hole, _, u_gen) = HTyp.new_Hole(u_gen);
    let (skel2, seq, ctx, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq, hole);
    let (ty, u, u_gen) = HTyp.new_Hole(u_gen);
    let skel =
      Skel.BinOp(
        InHole(TypeInconsistent, u),
        Operators_Pat.Space,
        skel1,
        skel2,
      );
    (skel, seq, ty, ctx, u_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    let (skel1, seq, ty_elt, ctx, u_gen) =
      syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    let ty = HTyp.List(ty_elt);
    let (skel2, seq, ctx, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq, ty);
    let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
    (skel, seq, ty, ctx, u_gen);
  }
and syn_fix_holes_operand =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      operand: UHPat.operand,
    )
    : (UHPat.operand, HTyp.t, Contexts.t, MetaVarGen.t) => {
  let operand_nih = operand |> UHPat.set_err_status_operand(NotInHole);
  switch (operand) {
  | EmptyHole(_) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    if (renumber_empty_holes) {
      (EmptyHole(u), Hole(u), ctx, u_gen);
    } else {
      (operand, HTyp.Hole(u), ctx, u_gen);
    };
  | Wild(_) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    (operand_nih, ty, ctx, u_gen);
  | InvalidText(u, _) => (operand_nih, Hole(u), ctx, u_gen)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(Keyword(_), u), _) => (
      operand_nih,
      Hole(u),
      ctx,
      u_gen,
    )
  | Var(_, NotInVarHole, x) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    let ctx = Contexts.extend_gamma(ctx, (x, ty));
    (operand_nih, ty, ctx, u_gen);
  | IntLit(_, _) => (operand_nih, Int, ctx, u_gen)
  | FloatLit(_, _) => (operand_nih, Float, ctx, u_gen)
  | BoolLit(_, _) => (operand_nih, Bool, ctx, u_gen)
  | ListNil(_) =>
    let (ty, _, u_gen) = HTyp.new_Hole(u_gen);
    (operand_nih, List(ty), ctx, u_gen);
  | Parenthesized(p) =>
    let (p, ty, ctx, u_gen) =
      syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p);
    (Parenthesized(p), ty, ctx, u_gen);
  | Inj(_, side, p1) =>
    let (p1, ty1, ctx, u_gen) =
      syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p1);
    let p = UHPat.Inj(NotInHole, side, p1);
    let (hole, _, u_gen) = HTyp.new_Hole(u_gen);
    let ty =
      switch (side) {
      | L => HTyp.Sum(ty1, hole)
      | R => HTyp.Sum(hole, ty1)
      };
    (p, ty, ctx, u_gen);
  | TypeAnn(_, op, ann) =>
    let (uty, kind, u_gen) = Elaborator_Typ.syn_fix_holes(ctx, u_gen, ann);
    // TODO: Should syn_fix_holes just return the HTyp instead so we don't need a force unwrap here?
    let (ty, _, u_gen) =
      Elaborator_Typ.ana(ctx, u_gen, Delta.empty, uty, kind) |> Option.get;
    let (op, ctx, u_gen) =
      ana_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, op, ty);
    (UHPat.TypeAnn(NotInHole, op, ann), ty, ctx, u_gen);
  };
}
and ana_fix_holes =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      p: UHPat.t,
      ty: HTyp.t,
    )
    : (UHPat.t, Contexts.t, MetaVarGen.t) =>
  ana_fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, p, ty)
and ana_fix_holes_opseq =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : (UHPat.opseq, Contexts.t, MetaVarGen.t) => {
  // handle n-tuples
  switch (tuple_zip(skel, ty)) {
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (
           (
             rev_skels: list(UHPat.skel),
             seq: UHPat.seq,
             ctx: Contexts.t,
             u_gen: MetaVarGen.t,
           ),
           (skel: UHPat.skel, ty: HTyp.t),
         ) => {
           let (skel, seq, ctx, u_gen) =
             ana_fix_holes_skel(
               ctx,
               u_gen,
               ~renumber_empty_holes,
               skel,
               seq,
               ty,
             );
           ([skel, ...rev_skels], seq, ctx, u_gen);
         },
         ([], seq, ctx, u_gen),
       )
    |> (
      fun
      | (rev_skels, seq, ctx, u_gen) => {
          let skel = rev_skels |> List.rev |> UHPat.mk_tuple;
          (OpSeq.OpSeq(skel, seq), ctx, u_gen);
        }
    )
  | None =>
    if (List.length(HTyp.get_prod_elements(ty)) == 1) {
      skel
      |> UHPat.get_tuple_elements
      |> List.fold_left(
           (
             (
               rev_skels: list(UHPat.skel),
               seq: UHPat.seq,
               ctx: Contexts.t,
               u_gen: MetaVarGen.t,
             ),
             skel: UHPat.skel,
           ) => {
             let (skel, seq, _, ctx, u_gen) =
               syn_fix_holes_skel(
                 ctx,
                 u_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
               );
             ([skel, ...rev_skels], seq, ctx, u_gen);
           },
           ([], seq, ctx, u_gen),
         )
      |> (
        fun
        | (rev_skels, seq, ctx, u_gen) => {
            let (u, u_gen) = MetaVarGen.next(u_gen);
            let skel = UHPat.mk_tuple(List.rev(rev_skels));
            let opseq =
              UHPat.set_err_status_opseq(
                InHole(TypeInconsistent, u),
                OpSeq.OpSeq(skel, seq),
              );
            (opseq, ctx, u_gen);
          }
      );
    } else {
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let (opseq, _, _, u_gen) =
        syn_fix_holes_opseq(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          opseq |> UHPat.set_err_status_opseq(NotInHole),
        );
      (
        opseq |> UHPat.set_err_status_opseq(InHole(WrongLength, u)),
        ctx,
        u_gen,
      );
    }
  };
}
and ana_fix_holes_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : (UHPat.skel, UHPat.seq, Contexts.t, MetaVarGen.t) =>
  switch (skel) {
  | BinOp(_, Comma, _, _) =>
    failwith("Pat.ana_fix_holes_skel: tuples handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let (pn, ctx, u_gen) =
      ana_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, pn, ty);
    let seq = seq |> Seq.update_nth_operand(n, pn);
    (skel, seq, ctx, u_gen);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, u_gen) = {
      let (skel1, seq, ty, ctx, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let (skel1, seq, u_gen) =
        switch (HTyp.matched_arrow(ty, u_gen)) {
        | Some(_) => (skel1, seq, u_gen)
        | None =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let OpSeq(skel1, seq) =
            UHPat.set_err_status_opseq(
              InHole(TypeInconsistent, u),
              OpSeq(skel1, seq),
            );
          (skel1, seq, u_gen);
        };
      (skel1, seq, ctx, u_gen);
    };
    let (skel2, seq, ctx, u_gen) =
      ana_fix_holes_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.Hole(0),
      );
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let skel =
      Skel.BinOp(
        InHole(TypeInconsistent, u),
        Operators_Pat.Space,
        skel1,
        skel2,
      );
    (skel, seq, ctx, u_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | Some(ty_elt) =>
      let (skel1, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        );
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
      (skel, seq, ctx, u_gen);
    | None =>
      let (skel1, seq, ty_elt, ctx, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let skel =
        Skel.BinOp(
          InHole(TypeInconsistent, u),
          Operators_Pat.Cons,
          skel1,
          skel2,
        );
      (skel, seq, ctx, u_gen);
    }
  }
and ana_fix_holes_operand =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      operand: UHPat.operand,
      ty: HTyp.t,
    )
    : (UHPat.operand, Contexts.t, MetaVarGen.t) => {
  let operand_nih = UHPat.set_err_status_operand(NotInHole, operand);
  switch (operand) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (EmptyHole(u), ctx, u_gen);
    } else {
      (operand, ctx, u_gen);
    }
  | Wild(_) => (operand_nih, ctx, u_gen)
  | InvalidText(_) => (operand_nih, ctx, u_gen)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(Keyword(_), _), _) => (operand_nih, ctx, u_gen)
  | Var(_, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, ty));
    (operand_nih, ctx, u_gen);
  | IntLit(_, _)
  | FloatLit(_, _)
  | BoolLit(_, _) =>
    let (operand', ty', ctx, u_gen) =
      syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, operand);
    if (HTyp.consistent(Contexts.typing(ctx), ty, ty')) {
      (UHPat.set_err_status_operand(NotInHole, operand'), ctx, u_gen);
    } else {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (
        UHPat.set_err_status_operand(InHole(TypeInconsistent, u), operand'),
        ctx,
        u_gen,
      );
    };
  | ListNil(_) =>
    switch (HTyp.matched_list(ty)) {
    | Some(_) => (ListNil(NotInHole), ctx, u_gen)
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ListNil(InHole(TypeInconsistent, u)), ctx, u_gen);
    }
  | Parenthesized(p1) =>
    let (p1, ctx, u_gen) =
      ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p1, ty);
    (Parenthesized(p1), ctx, u_gen);
  | Inj(_, side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      let (p1, ctx, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p1, ty1);
      (Inj(NotInHole, side, p1), ctx, u_gen);
    | None =>
      let (p1, _, ctx, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (Inj(InHole(TypeInconsistent, u), side, p1), ctx, u_gen);
    }
  | TypeAnn(err, op, ann) =>
    let (uty, kind, u_gen) = Elaborator_Typ.syn_fix_holes(ctx, u_gen, ann);
    // TODO: Should syn_fix_holes just return the HTyp instead so we don't need a force unwrap here?
    let (ty_ann, _, u_gen) =
      Elaborator_Typ.ana(ctx, u_gen, Delta.empty, uty, kind) |> Option.get;

    if (HTyp.consistent(Contexts.typing(ctx), ty, ty_ann)) {
      let (op, ctx, u_gen) =
        ana_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, op, ty_ann);
      (TypeAnn(NotInHole, op, ann), ctx, u_gen);
    } else {
      let (op, _, _, u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, op);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (
        UHPat.set_err_status_operand(
          InHole(TypeInconsistent, u),
          TypeAnn(err, op, ann),
        ),
        ctx,
        u_gen,
      );
    };
  };
};

let syn_fix_holes_z =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t)
    : (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ty, ctx, u_gen) = syn_fix_holes(ctx, u_gen, ZPat.erase(zp));
  let zp =
    CursorPath_Pat.follow(path, p)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zp, ty, ctx, u_gen);
};

let ana_fix_holes_z =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t, ty: HTyp.t)
    : (ZPat.t, Contexts.t, MetaVarGen.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ctx, u_gen) = ana_fix_holes(ctx, u_gen, ZPat.erase(zp), ty);
  let zp =
    CursorPath_Pat.follow(path, p)
    |> OptUtil.get(() =>
         failwith(
           "ana_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zp, ctx, u_gen);
};
