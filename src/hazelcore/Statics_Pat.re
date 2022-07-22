open OptUtil.Syntax;

let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHPat.get_tuple_elements);

let rec syn =
        (ctx: Context.t, p: UHPat.t)
        : option((HTyp.t, Context.t, Constraints.t)) =>
  syn_opseq(ctx, p)

and syn_opseq =
    (ctx: Context.t, OpSeq(skel, seq): UHPat.opseq)
    : option((HTyp.t, Context.t, Constraints.t)) =>
  syn_skel(ctx, skel, seq)

and syn_skel =
    (ctx: Context.t, skel: UHPat.skel, seq: UHPat.seq)
    : option((HTyp.t, Context.t, Constraints.t)) =>
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    syn_operand(ctx, pn);
  | BinOp(InHole(_), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ (_, ctx, _) = syn_skel(ctx, skel_not_in_hole, seq);
    (HTyp.hole(), ctx, Constraints.Falsity);
  | BinOp(NotInHole, Comma, _, _) =>
    let+ (ctx, ty_xis) =
      skel
      |> UHPat.get_tuple_elements
      |> ListUtil.map_with_accumulator_opt(
           (ctx, skel) => {
             let+ (ty, ctx, xi) = syn_skel(ctx, skel, seq);
             (ctx, (ty, xi));
           },
           ctx,
         );
    let (tys, xis) = List.split(ty_xis);
    (HTyp.product(tys), ctx, Constraints.pair_constraints(xis));
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let* (ctx, _) = ana_skel(ctx, skel1, seq, HTyp.hole());
    let+ (ctx, _) = ana_skel(ctx, skel2, seq, HTyp.hole());
    (HTyp.hole(), ctx, Constraints.Hole);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* (ty1, ctx, xi1) = syn_skel(ctx, skel1, seq);
    let ty = HTyp.list(ty1);
    let+ (ctx, xi2) = ana_skel(ctx, skel2, seq, ty);
    (ty, ctx, Constraints.InjR(Pair(xi1, xi2)));
  }

and syn_operand =
    (ctx: Context.t, operand: UHPat.operand)
    : option((HTyp.t, Context.t, Constraints.t)) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some((HTyp.hole(), ctx, Hole))
  | InvalidText(_) => Some((HTyp.hole(), ctx, Falsity))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _)
  | TypeAnn(InHole(TypeInconsistent, _), _, _) =>
    let operand = UHPat.set_err_status_operand(NotInHole, operand);
    let+ (_, ctx, xi) = syn_operand(ctx, operand);
    (HTyp.hole(), ctx, xi);
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _)
  | TypeAnn(InHole(WrongLength, _), _, _) => None
  /* not in hole */
  | Wild(NotInHole) => Some((HTyp.hole(), ctx, Truth))
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) =>
    Some((HTyp.hole(), ctx, Falsity))
  | Var(NotInHole, NotInVarHole, x) =>
    Var.check_valid(
      x,
      Some((
        HTyp.hole(),
        Context.add_var(ctx, x, HTyp.hole()),
        Constraints.Truth,
      )),
    )
  | IntLit(NotInHole, n) => Some((HTyp.int(), ctx, Int(int_of_string(n))))
  | FloatLit(NotInHole, n) =>
    Some((HTyp.float(), ctx, Float(float_of_string(n))))
  | BoolLit(NotInHole, b) =>
    /* TODO: (eric) replace Truth with ADT-encoded boolean constraints */
    Some((HTyp.bool(), ctx, b ? Constraints.InjL(Truth) : InjR(Truth)))
  | ListNil(NotInHole) =>
    /* TODO: (eric) replace Truth with ADT-encoded list constraint */
    Some((HTyp.list(HTyp.hole()), ctx, Truth))
  | Inj(NotInHole, inj_side, p1) =>
    let+ (ty1, ctx, xi1) = syn(ctx, p1);
    switch (inj_side) {
    | L => (HTyp.sum(ty1, HTyp.hole()), ctx, Constraints.InjL(xi1))
    | R => (HTyp.sum(HTyp.hole(), ty1), ctx, InjR(xi1))
    };
  | Parenthesized(p) => syn(ctx, p)
  | TypeAnn(NotInHole, op, ann) =>
    let* (ty, _, _) = Elaborator_Typ.syn_elab(ctx, Delta.empty, ann);
    let+ (ctx, xi) = ana_operand(ctx, op, ty);
    (ty, ctx, xi);
  }

and ana =
    (ctx: Context.t, p: UHPat.t, ty: HTyp.t)
    : option((Context.t, Constraints.t)) =>
  ana_opseq(ctx, p, ty)

and ana_opseq =
    (ctx: Context.t, OpSeq(skel, seq) as opseq: UHPat.opseq, ty: HTyp.t)
    : option((Context.t, Constraints.t)) => {
  let ty_head_normed = HTyp.head_normalize(ctx, ty);
  switch (tuple_zip(skel, ty_head_normed)) {
  | None =>
    switch (
      UHPat.get_err_status_opseq(opseq),
      HTyp.get_prod_elements(ty_head_normed),
    ) {
    | (InHole(TypeInconsistent, _), [_])
    | (InHole(WrongLength, _), _) =>
      let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
      let+ (_, ctx, _) = syn_opseq(ctx, opseq');
      (ctx, Constraints.Hole);
    | _ => None
    }
  | Some(skel_tys) =>
    switch (List.rev(skel_tys)) {
    | [] => None
    | [(skel, ty)] => ana_skel(ctx, skel, seq, ty)
    | [(skel, ty), ...skel_tys] =>
      switch (ana_skel(ctx, skel, seq, ty)) {
      | None => None
      | Some((ctx, xi)) =>
        List.fold_left(
          (acc_opt, (skel, ty)) => {
            let* (ctx, xi) = acc_opt;
            let+ (ctx, xi') = ana_skel(ctx, skel, seq, ty);
            (ctx, Constraints.Pair(xi', xi));
          },
          Some((ctx, xi)),
          skel_tys,
        )
      }
    }
  };
}

and ana_skel =
    (ctx: Context.t, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t)
    : option((Context.t, Constraints.t)) =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    failwith("Pat.ana_skel: expected tuples to be handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    ana_operand(ctx, pn, ty);
  | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ (_, ctx, _) = syn_skel(ctx, skel_not_in_hole, seq);
    (ctx, Constraints.Hole);
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let* (ctx, _) = ana_skel(ctx, skel1, seq, HTyp.hole());
    let+ (ctx, _) = ana_skel(ctx, skel2, seq, HTyp.hole());
    (ctx, Constraints.Hole);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* ty_elt = HTyp.matched_list(ctx, ty);
    let* (ctx, xi1) = ana_skel(ctx, skel1, seq, ty_elt);
    let+ (ctx, xi2) = ana_skel(ctx, skel2, seq, HTyp.list(ty_elt));
    (ctx, Constraints.InjR(Pair(xi1, xi2)));
  }

and ana_operand =
    (ctx: Context.t, operand: UHPat.operand, ty: HTyp.t)
    : option((Context.t, Constraints.t)) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some((ctx, Hole))
  | InvalidText(_) => Some((ctx, Falsity))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | TypeAnn(InHole(TypeInconsistent, _), _, _)
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    let+ (_, ctx, _) = syn_operand(ctx, operand');
    (ctx, Constraints.Hole);
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | TypeAnn(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _) =>
    ty
    |> HTyp.head_normalize(ctx)
    |> HTyp.get_prod_elements
    |> List.length > 1
      ? Some((ctx, Constraints.Hole)) : None
  /* not in hole */
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) =>
    Some((ctx, Falsity))
  | Var(NotInHole, NotInVarHole, x) =>
    Var.check_valid(
      x,
      Some((Context.add_var(ctx, x, ty), Constraints.Truth)),
    )
  | Wild(NotInHole) => Some((ctx, Truth))
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    let* (ty', ctx', xi) = syn_operand(ctx, operand);
    HTyp.consistent(ctx, ty, ty') ? Some((ctx', xi)) : None;
  | ListNil(NotInHole) =>
    let+ _ = HTyp.matched_list(ctx, ty);
    (ctx, Constraints.InjL(Truth));
  | Inj(NotInHole, side, p1) =>
    let* (tyL, tyR) = HTyp.matched_sum(ctx, ty);
    let ty1 = InjSide.pick(side, tyL, tyR);
    ana(ctx, p1, ty1);
  | Parenthesized(p) => ana(ctx, p, ty)
  | TypeAnn(NotInHole, op, ann) =>
    let* (ty_ann, _, _) = Elaborator_Typ.syn_elab(ctx, Delta.empty, ann);
    if (HTyp.consistent(ctx, ty, ty_ann)) {
      ana_operand(ctx, op, ty_ann);
    } else {
      None;
    };
  };

let rec syn_nth_type_mode =
        (ctx: Context.t, n: int, OpSeq(skel, seq): UHPat.opseq)
        : option(Statics.type_mode) =>
  syn_nth_type_mode'(ctx, n, skel, seq)

and syn_nth_type_mode' =
    (ctx: Context.t, n: int, skel: UHPat.skel, seq: UHPat.seq)
    : option(Statics.type_mode) => {
  let ana_go = (skel, ty) => ana_nth_type_mode'(ctx, n, skel, seq, ty);
  let rec go = (skel: UHPat.skel) =>
    switch (skel) {
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics.Syn);
    | BinOp(InHole(_), op, skel1, skel2) =>
      go(BinOp(NotInHole, op, skel1, skel2))
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1) ? go(skel1) : go(skel2)
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq)) {
      | None => None
      | Some((ty1, _, _)) =>
        if (n <= Skel.rightmost_tm_index(skel1)) {
          go(skel1);
        } else {
          let* (ty2, _) = HTyp.matched_arrow(ctx, ty1);
          ana_go(skel2, ty2);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* (ty1, _, _) = syn_skel(ctx, skel1, seq);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1) : ana_go(skel2, HTyp.list(ty1));
    };
  go(skel);
}

and ana_nth_type_mode =
    (
      ctx: Context.t,
      n: int,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : option(Statics.type_mode) =>
  // handle n-tuples
  switch (tuple_zip(skel, HTyp.head_normalize(ctx, ty))) {
  | None =>
    syn_nth_type_mode(ctx, n, UHPat.set_err_status_opseq(NotInHole, opseq))
  | Some(skel_tys) =>
    let (nskel, nty) =
      skel_tys
      |> List.find(((skel, _)) =>
           Skel.leftmost_tm_index(skel) <= n
           && n <= Skel.rightmost_tm_index(skel)
         );
    ana_nth_type_mode'(ctx, n, nskel, seq, nty);
  }

and ana_nth_type_mode' =
    (ctx: Context.t, n: int, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t)
    : option(Statics.type_mode) => {
  let rec go = (skel: UHPat.skel, ty: HTyp.t) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      failwith(__LOC__ ++ ": expected tuples to be handled at opseq level")
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics.Ana(ty));
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      syn_nth_type_mode'(ctx, n, skel_not_in_hole, seq);
    | BinOp(NotInHole, Space, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, HTyp.hole()) : go(skel2, HTyp.hole())
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty_elt = HTyp.matched_list(ctx, ty);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, ty_elt) : go(skel2, ty);
    };
  go(skel, ty);
};

let rec syn_fix_holes =
        (
          ctx: Context.t,
          id_gen: IDGen.t,
          ~renumber_empty_holes=false,
          p: UHPat.t,
        )
        : (UHPat.t, HTyp.t, Context.t, IDGen.t, Constraints.t) =>
  syn_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, p)

and syn_fix_holes_opseq =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq): UHPat.opseq,
    )
    : (UHPat.opseq, HTyp.t, Context.t, IDGen.t, Constraints.t) => {
  let (skel, seq, ty, ctx, id_gen, xi) =
    syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel, seq);
  (OpSeq(skel, seq), ty, ctx, id_gen, xi);
}

and syn_fix_holes_skel =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel,
      seq: UHPat.seq,
    )
    : (UHPat.skel, UHPat.seq, HTyp.t, Context.t, IDGen.t, Constraints.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let (pn, ty, ctx, id_gen, xi) =
      syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, pn);
    let seq = seq |> Seq.update_nth_operand(n, pn);
    (skel, seq, ty, ctx, id_gen, xi);
  | BinOp(_, Comma, _, _) =>
    let ((ctx, id_gen, seq), pairs) =
      skel
      |> UHPat.get_tuple_elements
      |> ListUtil.map_with_accumulator(
           ((ctx, id_gen, seq), skel) => {
             let (skel, seq, ty, ctx, id_gen, xi) =
               syn_fix_holes_skel(
                 ctx,
                 id_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
               );
             ((ctx, id_gen, seq), ((skel, ty), xi));
           },
           (ctx, id_gen, seq),
         );
    let (pairs, xis) = List.split(pairs);
    let (skels, tys) = List.split(pairs);
    (
      UHPat.mk_tuple(skels),
      seq,
      HTyp.product(tys),
      ctx,
      id_gen,
      Constraints.pair_constraints(xis),
    );
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, id_gen) = {
      let (skel1, seq, ty, ctx, id_gen, _) =
        syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
      let (skel1, seq, id_gen) =
        switch (HTyp.matched_arrow(ctx, ty)) {
        | Some(_) => (skel1, seq, id_gen)
        | None =>
          let (u, id_gen) = IDGen.next_hole(id_gen);
          let OpSeq(skel1, seq) =
            UHPat.set_err_status_opseq(
              InHole(TypeInconsistent, u),
              OpSeq(skel1, seq),
            );
          (skel1, seq, id_gen);
        };
      (skel1, seq, ctx, id_gen);
    };
    let (skel2, seq, ctx, id_gen, _) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.hole(),
      );
    let (u, id_gen) = IDGen.next_hole(id_gen);
    let skel =
      Skel.BinOp(
        InHole(TypeInconsistent, u),
        Operators_Pat.Space,
        skel1,
        skel2,
      );
    let ty = HTyp.hole();
    (skel, seq, ty, ctx, id_gen, Falsity);
  | BinOp(_, Cons, skel1, skel2) =>
    let (skel1, seq, ty_elt, ctx, id_gen, _) =
      syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
    let ty = HTyp.list(ty_elt);
    let (skel2, seq, ctx, id_gen, xi) =
      ana_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel2, seq, ty);
    let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
    (skel, seq, ty, ctx, id_gen, xi);
  }

and syn_fix_holes_operand =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      operand: UHPat.operand,
    )
    : (UHPat.operand, HTyp.t, Context.t, IDGen.t, Constraints.t) => {
  let operand_nih = operand |> UHPat.set_err_status_operand(NotInHole);
  switch (operand) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyHole(u), HTyp.hole(), ctx, id_gen, Hole);
    } else {
      (operand, HTyp.hole(), ctx, id_gen, Hole);
    }
  | Wild(_) => (operand_nih, HTyp.hole(), ctx, id_gen, Truth)
  | InvalidText(_) => (operand_nih, HTyp.hole(), ctx, id_gen, Falsity)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(ExpandingKeyword(_), _), _) => (
      operand_nih,
      HTyp.hole(),
      ctx,
      id_gen,
      Falsity,
    )
  | Var(_, NotInVarHole, x) =>
    let ctx = Context.add_var(ctx, x, HTyp.hole());
    (operand_nih, HTyp.hole(), ctx, id_gen, Truth);
  | IntLit(_, n) => (
      operand_nih,
      HTyp.int(),
      ctx,
      id_gen,
      Int(int_of_string(n)),
    )
  | FloatLit(_, n) => (
      operand_nih,
      HTyp.float(),
      ctx,
      id_gen,
      Float(float_of_string(n)),
    )
  | BoolLit(_, b) =>
    /* TODO: (eric) replace Truth with ADT-encoded boolean constraints */
    (
      operand_nih,
      HTyp.bool(),
      ctx,
      id_gen,
      b ? Constraints.InjL(Truth) : InjR(Truth),
    )
  | ListNil(_) =>
    /* TODO: (eric) replace Truth with ADT-encoded list constraint */
    (operand_nih, HTyp.list(HTyp.hole()), ctx, id_gen, Truth)
  | Parenthesized(p) =>
    let (p, ty, ctx, id_gen, xi) =
      syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p);
    (Parenthesized(p), ty, ctx, id_gen, xi);
  | Inj(_, side, p1) =>
    let (p1, ty1, ctx, id_gen, xi1) =
      syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1);
    let p = UHPat.Inj(NotInHole, side, p1);
    let (ty, xi) =
      switch (side) {
      | L => (HTyp.sum(ty1, HTyp.hole()), Constraints.InjL(xi1))
      | R => (HTyp.sum(HTyp.hole(), ty1), Constraints.InjR(xi1))
      };
    (p, ty, ctx, id_gen, xi);
  | TypeAnn(_, op, ann) =>
    let (ann, _, id_gen) = Statics_UHTyp.syn_fix_holes(ctx, id_gen, ann);
    switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ann)) {
    | Some((ty_ann, _, _)) =>
      if (HTyp.complete(ty_ann)) {
        let (op, ctx, id_gen, xi) =
          ana_fix_holes_operand(
            ctx,
            id_gen,
            ~renumber_empty_holes,
            op,
            ty_ann,
          );
        (UHPat.TypeAnn(NotInHole, op, ann), ty_ann, ctx, id_gen, xi);
      } else {
        let (ann, _, id_gen) = Statics_UHTyp.syn_fix_holes(ctx, id_gen, ann);
        (UHPat.TypeAnn(NotInHole, op, ann), ty_ann, ctx, id_gen, Hole);
      }
    | None =>
      let (op, ty, ctx, id_gen, _) =
        syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, op);
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        UHPat.TypeAnn(InHole(TypeInconsistent, u), op, ann),
        ty,
        ctx,
        id_gen,
        Hole,
      );
    };
  };
}

and ana_fix_holes =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      p: UHPat.t,
      ty: HTyp.t,
    )
    : (UHPat.t, Context.t, IDGen.t, Constraints.t) =>
  ana_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, p, ty)

and ana_fix_holes_opseq =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : (UHPat.opseq, Context.t, IDGen.t, Constraints.t) => {
  let ty_head_normed = HTyp.head_normalize(ctx, ty);
  // handle n-tuples
  switch (tuple_zip(skel, ty_head_normed)) {
  | Some(skel_tys) =>
    switch (List.rev(skel_tys)) {
    | [] => failwith(__LOC__ ++ ": impossible branch")
    | [(skel, ty)] =>
      let (skel, seq, ctx, id_gen, xi) =
        ana_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel, seq, ty);
      (OpSeq(skel, seq), ctx, id_gen, xi);
    | [(skel, ty), ...skel_tys] =>
      let (skel, seq, ctx, id_gen, xi) =
        ana_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel, seq, ty);
      let (rev_skels, seq, ctx, id_gen, xi) =
        List.fold_left(
          ((rev_skels, seq, ctx, id_gen, xi), (skel, ty)) => {
            let (skel, seq, ctx, id_gen, xi') =
              ana_fix_holes_skel(
                ctx,
                id_gen,
                ~renumber_empty_holes,
                skel,
                seq,
                ty,
              );
            (
              [skel, ...rev_skels],
              seq,
              ctx,
              id_gen,
              Constraints.Pair(xi', xi),
            );
          },
          ([skel], seq, ctx, id_gen, xi),
          skel_tys,
        );
      let skel = UHPat.mk_tuple(List.rev(rev_skels));
      (OpSeq(skel, seq), ctx, id_gen, xi);
    }
  | None =>
    if (List.length(HTyp.get_prod_elements(ty_head_normed)) == 1) {
      skel
      |> UHPat.get_tuple_elements
      |> List.fold_left(
           (
             (
               rev_skels: list(UHPat.skel),
               seq: UHPat.seq,
               ctx: Context.t,
               id_gen: IDGen.t,
             ),
             skel: UHPat.skel,
           ) => {
             let (skel, seq, _, ctx, id_gen, _) =
               syn_fix_holes_skel(
                 ctx,
                 id_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
               );
             ([skel, ...rev_skels], seq, ctx, id_gen);
           },
           ([], seq, ctx, id_gen),
         )
      |> (
        fun
        | (rev_skels, seq, ctx, id_gen) => {
            let (u, id_gen) = IDGen.next_hole(id_gen);
            let skel = UHPat.mk_tuple(List.rev(rev_skels));
            let opseq =
              UHPat.set_err_status_opseq(
                InHole(TypeInconsistent, u),
                OpSeq.OpSeq(skel, seq),
              );
            (opseq, ctx, id_gen, Constraints.Hole);
          }
      );
    } else {
      let (u, id_gen) = id_gen |> IDGen.next_hole;
      let (opseq, _, _, id_gen, _) =
        syn_fix_holes_opseq(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          opseq |> UHPat.set_err_status_opseq(NotInHole),
        );
      (
        opseq |> UHPat.set_err_status_opseq(InHole(WrongLength, u)),
        ctx,
        id_gen,
        Hole,
      );
    }
  };
}

and ana_fix_holes_skel =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : (UHPat.skel, UHPat.seq, Context.t, IDGen.t, Constraints.t) =>
  switch (skel) {
  | BinOp(_, Comma, _, _) =>
    failwith("Pat.ana_fix_holes_skel: tuples handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let (pn, ctx, id_gen, xi) =
      ana_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, pn, ty);
    let seq = seq |> Seq.update_nth_operand(n, pn);
    (skel, seq, ctx, id_gen, xi);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, id_gen) = {
      let (skel1, seq, ty, ctx, id_gen, _) =
        syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
      let (skel1, seq, id_gen) =
        switch (HTyp.matched_arrow(ctx, ty)) {
        | Some(_) => (skel1, seq, id_gen)
        | None =>
          let (u, id_gen) = IDGen.next_hole(id_gen);
          let OpSeq(skel1, seq) =
            UHPat.set_err_status_opseq(
              InHole(TypeInconsistent, u),
              OpSeq(skel1, seq),
            );
          (skel1, seq, id_gen);
        };
      (skel1, seq, ctx, id_gen);
    };
    let (skel2, seq, ctx, id_gen, _) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.hole(),
      );
    let (u, id_gen) = IDGen.next_hole(id_gen);
    let skel =
      Skel.BinOp(
        InHole(TypeInconsistent, u),
        Operators_Pat.Space,
        skel1,
        skel2,
      );
    (skel, seq, ctx, id_gen, Hole);
  | BinOp(_, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ctx, ty)) {
    | Some(ty_elt) =>
      let (skel1, seq, ctx, id_gen, xi1) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        );
      let ty_list = HTyp.list(ty_elt);
      let (skel2, seq, ctx, id_gen, xi2) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
      (skel, seq, ctx, id_gen, InjR(Pair(xi1, xi2)));
    | None =>
      let (skel1, seq, ty_elt, ctx, id_gen, _) =
        syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
      let ty_list = HTyp.list(ty_elt);
      let (skel2, seq, ctx, id_gen, _) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let (u, id_gen) = IDGen.next_hole(id_gen);
      let skel =
        Skel.BinOp(
          InHole(TypeInconsistent, u),
          Operators_Pat.Cons,
          skel1,
          skel2,
        );
      (skel, seq, ctx, id_gen, Hole);
    }
  }

and ana_fix_holes_operand =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      operand: UHPat.operand,
      ty: HTyp.t,
    )
    : (UHPat.operand, Context.t, IDGen.t, Constraints.t) => {
  let operand_nih = UHPat.set_err_status_operand(NotInHole, operand);
  switch (operand) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyHole(u), ctx, id_gen, Hole);
    } else {
      (operand, ctx, id_gen, Hole);
    }
  | Wild(_) => (operand_nih, ctx, id_gen, Truth)
  | InvalidText(_) => (operand_nih, ctx, id_gen, Falsity)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(ExpandingKeyword(_), _), _) => (
      operand_nih,
      ctx,
      id_gen,
      Falsity,
    )
  | Var(_, NotInVarHole, x) =>
    let ctx = Context.add_var(ctx, x, ty);
    (operand_nih, ctx, id_gen, Truth);
  | IntLit(_, _)
  | FloatLit(_, _)
  | BoolLit(_, _) =>
    let (operand', ty', ctx, id_gen, xi) =
      syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, operand);
    if (HTyp.consistent(ctx, ty, ty')) {
      (UHPat.set_err_status_operand(NotInHole, operand'), ctx, id_gen, xi);
    } else {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        UHPat.set_err_status_operand(InHole(TypeInconsistent, u), operand'),
        ctx,
        id_gen,
        Hole,
      );
    };
  /* TODO: (eric) replace Truth with ADT-encoded list constraint */
  | ListNil(_) =>
    switch (HTyp.matched_list(ctx, ty)) {
    | Some(_) => (ListNil(NotInHole), ctx, id_gen, InjL(Truth))
    | None =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (ListNil(InHole(TypeInconsistent, u)), ctx, id_gen, InjL(Truth));
    }
  | Parenthesized(p1) =>
    let (p1, ctx, id_gen, xi) =
      ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1, ty);
    (Parenthesized(p1), ctx, id_gen, xi);
  | Inj(_, side, p1) =>
    switch (HTyp.matched_sum(ctx, ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      let (p1, ctx, id_gen, xi1) =
        ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1, ty1);
      let xi =
        switch (side) {
        | L => Constraints.InjL(xi1)
        | R => Constraints.InjR(xi1)
        };
      (Inj(NotInHole, side, p1), ctx, id_gen, xi);
    | None =>
      let (p1, _, ctx, id_gen, _) =
        syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1);
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (Inj(InHole(TypeInconsistent, u), side, p1), ctx, id_gen, Hole);
    }
  | TypeAnn(_, op, ann) =>
    switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ann)) {
    | Some((ty_ann, _, _)) when HTyp.consistent(ctx, ty, ty_ann) =>
      let (op, ctx, id_gen, xi) =
        ana_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, op, ty_ann);
      (TypeAnn(NotInHole, op, ann), ctx, id_gen, xi);
    | Some(_)
    | None =>
      let (op, _, _, id_gen, _) =
        syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, op);
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (TypeAnn(InHole(TypeInconsistent, u), op, ann), ctx, id_gen, Hole);
    }
  };
};

let syn_fix_holes_z =
    (ctx: Context.t, id_gen: IDGen.t, zp: ZPat.t)
    : (ZPat.t, HTyp.t, Context.t, IDGen.t, Constraints.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ty, ctx, id_gen, xi) = syn_fix_holes(ctx, id_gen, ZPat.erase(zp));
  let zp =
    CursorPath_Pat.follow(path, p)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zp, ty, ctx, id_gen, xi);
};

let ana_fix_holes_z =
    (ctx: Context.t, id_gen: IDGen.t, zp: ZPat.t, ty: HTyp.t)
    : (ZPat.t, Context.t, IDGen.t, Constraints.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ctx, id_gen, xi) = ana_fix_holes(ctx, id_gen, ZPat.erase(zp), ty);
  let zp =
    CursorPath_Pat.follow(path, p)
    |> OptUtil.get(() =>
         failwith(
           "ana_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zp, ctx, id_gen, xi);
};

/* let rec case_syn = */
/*         (ctx: Context.t, p: UHPat.t) */
/*         : option((HTyp.t, Context.t, Constraints.t)) => */
/*   case_syn_opseq(ctx, p) */
/* and case_syn_opseq = */
/*     (ctx: Context.t, OpSeq(skel, seq): UHPat.opseq) */
/*     : option((HTyp.t, Context.t, Constraints.t)) => */
/*   case_syn_skel(ctx, skel, seq) */
/* and case_syn_skel = */
/*     (ctx: Context.t, skel: UHPat.skel, seq: UHPat.seq) */
/*     : option((HTyp.t, Context.t, Constraints.t)) => { */
/*   switch (skel) { */
/*   | Placeholder(n) => */
/*     let pn = seq |> Seq.nth_operand(n); */
/*     case_syn_operand(ctx, pn); */
/*   | BinOp(InHole(_), op, skel1, skel2) => */
/*     let+ (_, ctx) = syn_skel(ctx, BinOp(NotInHole, op, skel1, skel2), seq); */
/*     (HTyp.hole(), ctx, Constraints.Hole); */
/*   | BinOp(NotInHole, Comma, _, _) => */
/*     let+ (ctx, tys_and_cons) = */
/*       skel */
/*       |> UHPat.get_tuple_elements */
/*       |> ListUtil.map_with_accumulator_opt( */
/*            (ctx, skel) => { */
/*              let+ (ty_elt, ctx, con_elt) = case_syn_skel(ctx, skel, seq); */
/*              (ctx, (ty_elt, con_elt)); */
/*            }, */
/*            ctx, */
/*          ); */
/*     let (tys, cons) = List.split(tys_and_cons); */
/*     switch (cons) { */
/*     | [] => failwith("not implemented") */
/*     | [con0, ...cons] => */
/*       let con = */
/*         List.fold_right( */
/*           (con, con_elt) => Constraints.Pair(con_elt, con), */
/*           cons, */
/*           con0, */
/*         ); */
/*       (HTyp.product(tys), ctx, con); */
/*     }; */
/*   | BinOp(NotInHole, Cons, skel1, skel2) => */
/*     let* (ty_elt, ctx) = syn_skel(ctx, skel1, seq); */
/*     let* (ctx, left_con) = case_ana_skel(ctx, skel1, seq, ty_elt); */
/*     let+ (ctx, right_con) = */
/*       case_ana_skel(ctx, skel2, seq, HTyp.list(ty_elt)); */
/*     (HTyp.list(ty_elt), ctx, Constraints.InjR(Pair(left_con, right_con))); */
/*   | BinOp(NotInHole, Space, _, _) => failwith("not implemented") */
/*   }; */
/* } */
/* and case_syn_operand = */
/*     (ctx: Context.t, operand: UHPat.operand) */
/*     : option((HTyp.t, Context.t, Constraints.t)) => */
/*   switch (operand) { */
/*   /\* in hole *\/ */
/*   | EmptyHole(_) => Some((HTyp.hole(), ctx, Hole)) */
/*   | InvalidText(_) => Some((HTyp.hole(), ctx, Falsity)) */
/*   | Wild(InHole(TypeInconsistent, _)) */
/*   | Var(InHole(TypeInconsistent, _), _, _) */
/*   | IntLit(InHole(TypeInconsistent, _), _) */
/*   | FloatLit(InHole(TypeInconsistent, _), _) */
/*   | BoolLit(InHole(TypeInconsistent, _), _) */
/*   | ListNil(InHole(TypeInconsistent, _)) */
/*   | Inj(InHole(TypeInconsistent, _), _, _) */
/*   | TypeAnn(InHole(TypeInconsistent, _), _, _) => */
/*     let operand' = UHPat.set_err_status_operand(NotInHole, operand); */
/*     case_syn_operand(ctx, operand') */
/*     |> Option.map(((_, gamma, con)) => (HTyp.hole(), gamma, con)); */
/*   | Wild(InHole(WrongLength, _)) */
/*   | Var(InHole(WrongLength, _), _, _) */
/*   | IntLit(InHole(WrongLength, _), _) */
/*   | FloatLit(InHole(WrongLength, _), _) */
/*   | BoolLit(InHole(WrongLength, _), _) */
/*   | ListNil(InHole(WrongLength, _)) */
/*   | Inj(InHole(WrongLength, _), _, _) */
/*   | TypeAnn(InHole(WrongLength, _), _, _) => None */
/*   /\* not in hole *\/ */
/*   | Wild(NotInHole) => Some((HTyp.hole(), ctx, Truth)) */
/*   | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat) */
/*   | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) => */
/*     Some((HTyp.hole(), ctx, Falsity)) */
/*   | Var(NotInHole, NotInVarHole, x) => */
/*     Var.check_valid( */
/*       x, */
/*       Some(( */
/*         HTyp.hole(), */
/*         Context.add_var(ctx, x, HTyp.hole()), */
/*         Constraints.Truth, */
/*       )), */
/*     ) */
/*   | IntLit(NotInHole, n) => Some((HTyp.int(), ctx, Int(int_of_string(n)))) */
/*   | FloatLit(NotInHole, n) => */
/*     Some((HTyp.float(), ctx, Float(float_of_string(n)))) */
/*   | BoolLit(NotInHole, b) => */
/*     let c = */
/*       if (b) { */
/*         Constraints.InjL(Truth); */
/*       } else { */
/*         InjR(Truth); */
/*       }; */
/*     Some((HTyp.bool(), ctx, c)); */
/*   | ListNil(NotInHole) => Some((HTyp.list(HTyp.hole()), ctx, Truth)) // todo: fix when list constr is defined */
/*   | Inj(NotInHole, inj_side, p1) => */
/*     switch (case_syn(ctx, p1)) { */
/*     | None => None */
/*     | Some((ty1, ctx, con)) => */
/*       switch (inj_side) { */
/*       | L => Some((HTyp.sum(ty1, HTyp.hole()), ctx, InjL(con))) */
/*       | R => Some((HTyp.sum(HTyp.hole(), ty1), ctx, InjR(con))) */
/*       } */
/*     } */
/*   | TypeAnn(NotInHole, op, ann) => */
/*     switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ann)) { */
/*     | None => None */
/*     | Some((ty, _, _)) => */
/*       switch (case_ana_operand(ctx, op, ty)) { */
/*       | None => None */
/*       | Some((ctx, con)) => Some((ty, ctx, con)) */
/*       } */
/*     } */
/*   | Parenthesized(p) => case_syn(ctx, p) */
/*   } */
/* and case_ana = */
/*     (ctx: Context.t, p: UHPat.t, ty: HTyp.t) */
/*     : option((Context.t, Constraints.t)) => */
/*   case_ana_opseq(ctx, p, ty) */
/* and case_ana_opseq = */
/*     (ctx: Context.t, OpSeq(skel, seq) as opseq: UHPat.opseq, ty: HTyp.t) */
/*     : option((Context.t, Constraints.t)) => { */
/*   let ty_head_normed = HTyp.head_normalize(ctx, ty); */
/*   switch (tuple_zip(skel, ty_head_normed)) { */
/*   | None => */
/*     switch ( */
/*       UHPat.get_err_status_opseq(opseq), */
/*       HTyp.get_prod_elements(ty_head_normed), */
/*     ) { */
/*     | (InHole(TypeInconsistent, _), [_]) */
/*     | (InHole(WrongLength, _), _) => */
/*       let opseq' = opseq |> UHPat.set_err_status_opseq(NotInHole); */
/*       syn_opseq(ctx, opseq') |> Option.map(_ => (ctx, Constraints.Hole)); */
/*     | _ => None */
/*     } */
/*   | Some(skel_tys) => */
/*     switch (List.rev(skel_tys)) { */
/*     | [] => None */
/*     | [(skel, ty)] => case_ana_skel(ctx, skel, seq, ty) */
/*     | [(skel, ty), ...skel_tys] => */
/*       switch (case_ana_skel(ctx, skel, seq, ty)) { */
/*       | None => None */
/*       | Some((ctx, con)) => */
/*         List.fold_left( */
/*           (acc: option((Context.t, Constraints.t)), (skel, ty)) => */
/*             switch (acc) { */
/*             | None => None */
/*             | Some((ctx, con)) => */
/*               switch (case_ana_skel(ctx, skel, seq, ty)) { */
/*               | None => None */
/*               | Some((ctx, con')) => */
/*                 Some((ctx, Constraints.Pair(con', con))) */
/*               } */
/*             }, */
/*           Some((ctx, con)), */
/*           skel_tys, */
/*         ) */
/*       } */
/*     } */
/*   }; */
/* } */
/* and case_ana_skel = */
/*     (ctx: Context.t, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t) */
/*     : option((Context.t, Constraints.t)) => */
/*   switch (skel) { */
/*   | BinOp(_, Comma, _, _) */
/*   | BinOp(InHole(WrongLength, _), _, _, _) => */
/*     failwith("Pat.ana_skel: expected tuples to be handled at opseq level") */
/*   | Placeholder(n) => */
/*     let pn = Seq.nth_operand(n, seq); */
/*     case_ana_operand(ctx, pn, ty); */
/*   | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) => */
/*     let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2); */
/*     switch (syn_skel(ctx, skel_not_in_hole, seq)) { */
/*     | None => None */
/*     | Some((_, ctx)) => Some((ctx, Constraints.Hole)) */
/*     }; */
/*   | BinOp(NotInHole, Cons, skel1, skel2) => */
/*     switch (HTyp.matched_list(ctx, ty)) { */
/*     | None => None */
/*     | Some(ty_elt) => */
/*       switch (case_ana_skel(ctx, skel1, seq, ty_elt)) { */
/*       | None => None */
/*       | Some((ctx, left_con)) => */
/*         switch (case_ana_skel(ctx, skel2, seq, HTyp.list(ty_elt))) { */
/*         | None => None */
/*         | Some((ctx, right_con)) => */
/*           Some((ctx, InjR(Pair(left_con, right_con)))) */
/*         } */
/*       } */
/*     } */
/*   | BinOp(NotInHole, Space, _, _) => failwith("not implemented") */
/*   } */
/* and case_ana_operand = */
/*     (ctx: Context.t, operand: UHPat.operand, ty: HTyp.t) */
/*     : option((Context.t, Constraints.t)) => */
/*   switch (operand) { */
/*   /\* in hole *\/ */
/*   | EmptyHole(_) => Some((ctx, Hole)) */
/*   | InvalidText(_) => Some((ctx, Falsity)) */
/*   | Wild(InHole(TypeInconsistent, _)) */
/*   | Var(InHole(TypeInconsistent, _), _, _) */
/*   | IntLit(InHole(TypeInconsistent, _), _) */
/*   | FloatLit(InHole(TypeInconsistent, _), _) */
/*   | BoolLit(InHole(TypeInconsistent, _), _) */
/*   | ListNil(InHole(TypeInconsistent, _)) */
/*   | Inj(InHole(TypeInconsistent, _), _, _) */
/*   | TypeAnn(InHole(TypeInconsistent, _), _, _) => */
/*     let operand' = UHPat.set_err_status_operand(NotInHole, operand); */
/*     syn_operand(ctx, operand') */
/*     |> Option.map(((_, ctx)) => (ctx, Constraints.Hole)); */
/*   | Wild(InHole(WrongLength, _)) */
/*   | Var(InHole(WrongLength, _), _, _) */
/*   | IntLit(InHole(WrongLength, _), _) */
/*   | FloatLit(InHole(WrongLength, _), _) */
/*   | BoolLit(InHole(WrongLength, _), _) */
/*   | ListNil(InHole(WrongLength, _)) */
/*   | Inj(InHole(WrongLength, _), _, _) */
/*   | TypeAnn(InHole(WrongLength, _), _, _) => */
/*     ty */
/*     |> HTyp.head_normalize(ctx) */
/*     |> HTyp.get_prod_elements */
/*     |> List.length > 1 */
/*       ? Some((ctx, Constraints.Hole)) : None /\* not in hole *\/ */
/*   | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat) */
/*   | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) => */
/*     Some((ctx, Falsity)) */
/*   | Var(NotInHole, NotInVarHole, x) => */
/*     Var.check_valid( */
/*       x, */
/*       Some((Context.add_var(ctx, x, ty), Constraints.Truth)), */
/*     ) */
/*   | Wild(NotInHole) => Some((ctx, Truth)) */
/*   | IntLit(NotInHole, n) => */
/*     switch (syn_operand(ctx, operand)) { */
/*     | None => None */
/*     | Some((ty', ctx')) => */
/*       if (HTyp.consistent(ctx, ty, ty')) { */
/*         Some((ctx', Int(int_of_string(n)))); */
/*       } else { */
/*         None; */
/*       } */
/*     } */
/*   | FloatLit(NotInHole, n) => */
/*     switch (syn_operand(ctx, operand)) { */
/*     | None => None */
/*     | Some((ty', ctx')) => */
/*       if (HTyp.consistent(ctx, ty, ty')) { */
/*         Some((ctx', Float(float_of_string(n)))); */
/*       } else { */
/*         None; */
/*       } */
/*     } */
/*   | BoolLit(NotInHole, b) => */
/*     switch (syn_operand(ctx, operand)) { */
/*     | None => None */
/*     | Some((ty', ctx')) => */
/*       if (HTyp.consistent(ctx, ty, ty')) { */
/*         let c = */
/*           if (b) { */
/*             Constraints.InjL(Truth); */
/*           } else { */
/*             InjR(Truth); */
/*           }; */
/*         Some((ctx', c)); */
/*       } else { */
/*         None; */
/*       } */
/*     } */
/*   | ListNil(NotInHole) => */
/*     switch (HTyp.matched_list(ctx, ty)) { */
/*     | None => None */
/*     | Some(_) => Some((ctx, InjL(Truth))) */
/*     } */
/*   | Inj(NotInHole, side, p1) => */
/*     let _ = print_endline("enter inj"); */
/*     switch (HTyp.matched_sum(ctx, ty)) { */
/*     | None => None */
/*     | Some((tyL, tyR)) => */
/*       let ty1 = InjSide.pick(side, tyL, tyR); */
/*       switch (case_ana(ctx, p1, ty1)) { */
/*       | None => None */
/*       | Some((ctx', con')) => */
/*         switch (side) { */
/*         | L => Some((ctx', InjL(con'))) */
/*         | R => Some((ctx', InjR(con'))) */
/*         } */
/*       }; */
/*     }; */
/*   | TypeAnn(NotInHole, op, ann) => */
/*     switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ann)) { */
/*     | None => None */
/*     | Some((ty_ann, _, _)) => */
/*       HTyp.consistent(ctx, ty, ty_ann) */
/*         ? case_ana_operand(ctx, op, ty_ann) : None */
/*     } */
/*   | Parenthesized(p) => case_ana(ctx, p, ty) */
/*   }; */

/* let generate_rules_constraints = */
/*     (ctx: Context.t, pats: list(UHPat.t), scrut_ty: HTyp.t) */
/*     : list(Constraints.t) => */
/*   List.map( */
/*     pat => */
/*       switch (case_ana(ctx, pat, scrut_ty)) { */
/*       | None => failwith("No constraint generated") */
/*       | Some((_, c)) => c */
/*       }, */
/*     pats, */
/*   ); */

/* let generate_one_constraints = */
/*     (ctx: Context.t, pats: list(UHPat.t), scrut_ty: HTyp.t): Constraints.t => { */
/*   let clst = generate_rules_constraints(ctx, pats, scrut_ty); */
/*   Constraints.or_constraints(List.rev(clst)); */
/* }; */
