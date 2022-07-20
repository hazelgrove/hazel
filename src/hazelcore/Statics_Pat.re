open OptUtil.Syntax;

[@deriving sexp]
type pattern_syn =
  | Moded
  | Internal;

let unknown: pattern_syn => HTyp.t =
  fun
  | Moded => Unknown(ModeSwitch)
  | Internal => Unknown(Internal);

let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHPat.get_tuple_elements);

let rec syn = syn_internal(~pattern_syn=Internal)
and syn_moded = (ctx: Contexts.t, p: UHPat.t): option(HTyp.t) =>
  Option.map(((a, _)) => a, syn_internal(ctx, p, ~pattern_syn=Moded))
and syn_internal =
    (ctx: Contexts.t, p: UHPat.t, ~pattern_syn: pattern_syn)
    : option((HTyp.t, Contexts.t)) =>
  syn_opseq_internal(ctx, p, ~pattern_syn: pattern_syn)
and syn_opseq = syn_opseq_internal(~pattern_syn=Internal)
and syn_opseq_internal =
    (
      ctx: Contexts.t,
      OpSeq(skel, seq): UHPat.opseq,
      ~pattern_syn: pattern_syn,
    )
    : option((HTyp.t, Contexts.t)) =>
  syn_skel_internal(ctx, skel, seq, ~pattern_syn)
and syn_skel = syn_skel_internal(~pattern_syn=Internal)
and syn_skel_internal =
    (
      ctx: Contexts.t,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ~pattern_syn: pattern_syn,
    )
    : option((HTyp.t, Contexts.t)) => {
  let unknown = unknown(pattern_syn);
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    syn_operand_internal(ctx, pn, ~pattern_syn);
  | BinOp(InHole(_), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ (_, ctx) =
      syn_skel_internal(ctx, skel_not_in_hole, seq, ~pattern_syn);
    (unknown, ctx);
  | BinOp(NotInHole, Comma, _, _) =>
    skel
    |> UHPat.get_tuple_elements
    |> ListUtil.map_with_accumulator_opt(
         (ctx, skel) =>
           syn_skel_internal(ctx, skel, seq, ~pattern_syn)
           |> Option.map(TupleUtil.swap),
         ctx,
       )
    |> Option.map(((ctx, tys)) => (HTyp.Prod(tys), ctx))
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let* ctx = ana_skel_internal(ctx, skel1, seq, unknown, ~pattern_syn);
    let+ ctx = ana_skel_internal(ctx, skel2, seq, unknown, ~pattern_syn);
    (unknown, ctx);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* (ty1, ctx) = syn_skel_internal(ctx, skel1, seq, ~pattern_syn);
    let ty = HTyp.List(ty1);
    let+ ctx = ana_skel_internal(ctx, skel2, seq, ty, ~pattern_syn);
    (ty, ctx);
  };
}
and syn_operand = syn_operand_internal(~pattern_syn=Internal)
and syn_operand_internal =
    (ctx: Contexts.t, operand: UHPat.operand, ~pattern_syn: pattern_syn)
    : option((HTyp.t, Contexts.t)) => {
  let unknown = unknown(pattern_syn);
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some((unknown, ctx))
  | InvalidText(_) => Some((unknown, ctx))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _)
  | TypeAnn(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    let+ (_, gamma) = syn_operand_internal(ctx, operand', ~pattern_syn);
    (unknown, gamma);
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _)
  | TypeAnn(InHole(WrongLength, _), _, _) => None
  /* not in hole */
  | Wild(NotInHole) => Some((unknown, ctx))
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) =>
    Some((unknown, ctx))
  | Var(NotInHole, NotInVarHole, x) =>
    switch (pattern_syn) {
    | Moded => Var.check_valid(x, Some((unknown, ctx)))
    | Internal =>
      Var.check_valid(x, Some((unknown, Contexts.extend_gamma(ctx, (x, ty)))));
    }
  | IntLit(NotInHole, _) => Some((Int, ctx))
  | FloatLit(NotInHole, _) => Some((Float, ctx))
  | BoolLit(NotInHole, _) => Some((Bool, ctx))
  | ListNil(NotInHole) => Some((List(unknown), ctx))
  | Inj(NotInHole, inj_side, p1) =>
    let+ (ty1, ctx) = syn_internal(ctx, p1, ~pattern_syn);
    let ty =
      switch (inj_side) {
      | L => HTyp.Sum(ty1, unknown)
      | R => HTyp.Sum(unknown, ty1)
      };
    (ty, ctx);
  | Parenthesized(p) => syn_internal(ctx, p, ~pattern_syn)
  | TypeAnn(NotInHole, op, ann) =>
    let ty_ann = UHTyp.expand(ann);
    let+ op_ctx = ana_operand_internal(ctx, op, ty_ann, ~pattern_syn);
    (ty_ann, op_ctx);
  };
}
and ana = ana_internal(~pattern_syn=Internal)
and ana_internal =
    (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, ~pattern_syn: pattern_syn)
    : option(Contexts.t) =>
  ana_opseq_internal(ctx, p, ty, ~pattern_syn)
and ana_opseq = ana_opseq_internal(~pattern_syn=Internal)
and ana_opseq_internal =
    (
      ctx: Contexts.t,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
      ~pattern_syn: pattern_syn,
    )
    : option(Contexts.t) =>
  switch (tuple_zip(skel, ty)) {
  | None =>
    switch (UHPat.get_err_status_opseq(opseq), HTyp.get_prod_elements(ty)) {
    | (InHole(TypeInconsistent, _), [_])
    | (InHole(WrongLength, _), _) =>
      let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
      let+ (_, ctx') = syn_opseq_internal(ctx, opseq', ~pattern_syn);
      ctx';
    | _ => None
    }
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (acc: option(Contexts.t), (skel, ty)) => {
           let* ctx = acc;
           ana_skel_internal(ctx, skel, seq, ty, ~pattern_syn);
         },
         Some(ctx),
       )
  }
and ana_skel = ana_skel_internal(~pattern_syn=Internal)
and ana_skel_internal =
    (
      ctx: Contexts.t,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
      ~pattern_syn: pattern_syn,
    )
    : option(Contexts.t) => {
  let unknown = unknown(pattern_syn);
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    failwith("Pat.ana_skel: expected tuples to be handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    ana_operand_internal(ctx, pn, ty, ~pattern_syn);
  | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ (_, ctx) =
      syn_skel_internal(ctx, skel_not_in_hole, seq, ~pattern_syn);
    ctx;
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let* ctx = ana_skel_internal(ctx, skel1, seq, unknown, ~pattern_syn);
    ana_skel_internal(ctx, skel2, seq, unknown, ~pattern_syn);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* ty_elt = HTyp.matched_list(ty);
    let* ctx = ana_skel_internal(ctx, skel1, seq, ty_elt, ~pattern_syn);
    ana_skel_internal(ctx, skel2, seq, HTyp.List(ty_elt), ~pattern_syn);
  };
}
and ana_operand = ana_operand_internal(~pattern_syn=Internal)
and ana_operand_internal =
    (
      ctx: Contexts.t,
      operand: UHPat.operand,
      ty: HTyp.t,
      ~pattern_syn: pattern_syn,
    )
    : option(Contexts.t) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some(ctx)
  | InvalidText(_) => Some(ctx)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | TypeAnn(InHole(TypeInconsistent, _), _, _)
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    let+ (_, ctx) = syn_operand_internal(ctx, operand', ~pattern_syn);
    ctx;
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | TypeAnn(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _) =>
    ty |> HTyp.get_prod_elements |> List.length > 1 ? Some(ctx) : None
  /* not in hole */
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) => Some(ctx)
  | Var(NotInHole, NotInVarHole, x) =>
    Var.check_valid(x, Some(Contexts.extend_gamma(ctx, (x, ty))))
  | Wild(NotInHole) => Some(ctx)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    let* (ty', ctx') = syn_operand_internal(ctx, operand, ~pattern_syn);
    HTyp.consistent(ty, ty') ? Some(ctx') : None;
  | ListNil(NotInHole) =>
    let+ _ = HTyp.matched_list(ty);
    ctx;
  | Inj(NotInHole, side, p1) =>
    let* (tyL, tyR) = HTyp.matched_sum(ty);
    let ty1 = InjSide.pick(side, tyL, tyR);
    ana_internal(ctx, p1, ty1, ~pattern_syn);
  | Parenthesized(p) => ana_internal(ctx, p, ty, ~pattern_syn)
  | TypeAnn(NotInHole, op, ann) =>
    let ty_ann = UHTyp.expand(ann);
    HTyp.consistent(ty, ty_ann)
      ? ana_operand_internal(ctx, op, ty_ann, ~pattern_syn) : None;
  };

let rec syn_nth_type_mode_internal =
        (
          ~pattern_syn: pattern_syn,
          ctx: Contexts.t,
          n: int,
          OpSeq(skel, seq): UHPat.opseq,
        )
        : option(Statics.type_mode) =>
  syn_nth_type_mode'(~pattern_syn, ctx, n, skel, seq)
and syn_nth_type_mode =
    (ctx: Contexts.t, n: int, opseq: UHPat.opseq): option(Statics.type_mode) =>
  syn_nth_type_mode_internal(ctx, n, opseq, ~pattern_syn=Internal)
and syn_nth_type_mode' =
    (
      ~pattern_syn: pattern_syn,
      ctx: Contexts.t,
      n: int,
      skel: UHPat.skel,
      seq: UHPat.seq,
    )
    : option(Statics.type_mode) => {
  let ana_go = (skel, ty) =>
    ana_nth_type_mode'(~pattern_syn, ctx, n, skel, seq, ty);
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
      | Some((ty1, _)) =>
        if (n <= Skel.rightmost_tm_index(skel1)) {
          go(skel1);
        } else {
          let* (ty2, _) = HTyp.matched_arrow(ty1);
          ana_go(skel2, ty2);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* (ty1, _) = syn_skel_internal(~pattern_syn, ctx, skel1, seq);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1) : ana_go(skel2, HTyp.List(ty1));
    };
  go(skel);
}
and ana_nth_type_mode = (ctx, n, opseq, ty) =>
  ana_nth_type_mode_internal(ctx, n, opseq, ty, ~pattern_syn=Internal)
and ana_nth_type_mode_internal =
    (
      ~pattern_syn: pattern_syn,
      ctx: Contexts.t,
      n: int,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : option(Statics.type_mode) => {
  // handle n-tuples
  switch (tuple_zip(skel, ty)) {
  | None =>
    syn_nth_type_mode_internal(
      ~pattern_syn,
      ctx,
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
    ana_nth_type_mode'(~pattern_syn, ctx, n, nskel, seq, nty);
  };
}
and ana_nth_type_mode' =
    (
      ~pattern_syn: pattern_syn,
      ctx: Contexts.t,
      n: int,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : option(Statics.type_mode) => {
  let unknown = unknown(pattern_syn);
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
      syn_nth_type_mode'(~pattern_syn, ctx, n, skel_not_in_hole, seq);
    | BinOp(NotInHole, Space, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, unknown) : go(skel2, unknown)
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty_elt = HTyp.matched_list(ty);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, ty_elt) : go(skel2, ty);
    };
  go(skel, ty);
};

let rec syn_fix_holes = syn_fix_holes_internal(~pattern_syn=Internal)
and syn_fix_holes_internal =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      p: UHPat.t,
    )
    : (UHPat.t, HTyp.t, Contexts.t, IDGen.t) =>
  syn_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, ~pattern_syn, p)
and syn_fix_holes_opseq =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      OpSeq(skel, seq): UHPat.opseq,
    )
    : (UHPat.opseq, HTyp.t, Contexts.t, IDGen.t) => {
  let (skel, seq, ty, ctx, id_gen) =
    syn_fix_holes_skel(
      ctx,
      id_gen,
      ~renumber_empty_holes,
      ~pattern_syn,
      skel,
      seq,
    );
  (OpSeq(skel, seq), ty, ctx, id_gen);
}
and syn_fix_holes_skel =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      skel: UHPat.skel,
      seq: UHPat.seq,
    )
    : (UHPat.skel, UHPat.seq, HTyp.t, Contexts.t, IDGen.t) => {
  let unknown = unknown(pattern_syn);
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let (pn, ty, ctx, id_gen) =
      syn_fix_holes_operand(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        pn,
      );
    let seq = seq |> Seq.update_nth_operand(n, pn);
    (skel, seq, ty, ctx, id_gen);
  | BinOp(_, Comma, _, _) =>
    let ((ctx, id_gen, seq), pairs) =
      skel
      |> UHPat.get_tuple_elements
      |> ListUtil.map_with_accumulator(
           ((ctx, id_gen, seq), skel) => {
             let (skel, seq, ty, ctx, id_gen) =
               syn_fix_holes_skel(
                 ctx,
                 id_gen,
                 ~renumber_empty_holes,
                 ~pattern_syn,
                 skel,
                 seq,
               );
             ((ctx, id_gen, seq), (skel, ty));
           },
           (ctx, id_gen, seq),
         );
    let (skels, tys) = List.split(pairs);
    (UHPat.mk_tuple(skels), seq, Prod(tys), ctx, id_gen);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, id_gen) = {
      let (skel1, seq, ty, ctx, id_gen) =
        syn_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          ~pattern_syn,
          skel1,
          seq,
        );
      let (skel1, seq, id_gen) =
        switch (HTyp.matched_arrow(ty)) {
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
    let (skel2, seq, ctx, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        skel2,
        seq,
        unknown,
      );
    let (u, id_gen) = IDGen.next_hole(id_gen);
    let skel =
      Skel.BinOp(
        InHole(TypeInconsistent, u),
        Operators_Pat.Space,
        skel1,
        skel2,
      );
    let ty = unknown;
    (skel, seq, ty, ctx, id_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    let (skel1, seq, ty_elt, ctx, id_gen) =
      syn_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        skel1,
        seq,
      );
    let ty = HTyp.List(ty_elt);
    let (skel2, seq, ctx, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        skel2,
        seq,
        ty,
      );
    let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
    (skel, seq, ty, ctx, id_gen);
  };
}
and syn_fix_holes_operand =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      operand: UHPat.operand,
    )
    : (UHPat.operand, HTyp.t, Contexts.t, IDGen.t) => {
  let unknown = unknown(pattern_syn);
  let operand_nih = operand |> UHPat.set_err_status_operand(NotInHole);
  switch (operand) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyHole(u), unknown, ctx, id_gen);
    } else {
      (operand, unknown, ctx, id_gen);
    }
  | Wild(_) => (operand_nih, unknown, ctx, id_gen)
  | InvalidText(_) => (operand_nih, unknown, ctx, id_gen)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(ExpandingKeyword(_), _), _) => (
      operand_nih,
      unknown,
      ctx,
      id_gen,
    )
  | Var(_, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, unknown));
    (operand_nih, unknown, ctx, id_gen);
  | IntLit(_, _) => (operand_nih, Int, ctx, id_gen)
  | FloatLit(_, _) => (operand_nih, Float, ctx, id_gen)
  | BoolLit(_, _) => (operand_nih, Bool, ctx, id_gen)
  | ListNil(_) => (operand_nih, List(unknown), ctx, id_gen)
  | Parenthesized(p) =>
    let (p, ty, ctx, id_gen) =
      syn_fix_holes_internal(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        p,
      );
    (Parenthesized(p), ty, ctx, id_gen);
  | Inj(_, side, p1) =>
    let (p1, ty1, ctx, id_gen) =
      syn_fix_holes_internal(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        p1,
      );
    let p = UHPat.Inj(NotInHole, side, p1);
    let ty =
      switch (side) {
      | L => HTyp.Sum(ty1, unknown)
      | R => HTyp.Sum(unknown, ty1)
      };
    (p, ty, ctx, id_gen);
  | TypeAnn(_, op, ann) =>
    let ty = UHTyp.expand(ann);
    let (op, ctx, id_gen) =
      ana_fix_holes_operand_internal(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        ~pattern_syn,
        op,
        ty,
      );
    (UHPat.TypeAnn(NotInHole, op, ann), ty, ctx, id_gen);
  };
}
and ana_fix_holes = ana_fix_holes_internal(~pattern_syn=Internal)
and ana_fix_holes_internal =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      p: UHPat.t,
      ty: HTyp.t,
    )
    : (UHPat.t, Contexts.t, IDGen.t) =>
  ana_fix_holes_opseq_internal(
    ctx,
    id_gen,
    ~renumber_empty_holes,
    ~pattern_syn,
    p,
    ty,
  )
and ana_fix_holes_opseq = ana_fix_holes_opseq_internal(~pattern_syn=Internal)
and ana_fix_holes_opseq_internal =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : (UHPat.opseq, Contexts.t, IDGen.t) => {
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
             id_gen: IDGen.t,
           ),
           (skel: UHPat.skel, ty: HTyp.t),
         ) => {
           let (skel, seq, ctx, id_gen) =
             ana_fix_holes_skel(
               ctx,
               id_gen,
               ~renumber_empty_holes,
               ~pattern_syn,
               skel,
               seq,
               ty,
             );
           ([skel, ...rev_skels], seq, ctx, id_gen);
         },
         ([], seq, ctx, id_gen),
       )
    |> (
      fun
      | (rev_skels, seq, ctx, id_gen) => {
          let skel = rev_skels |> List.rev |> UHPat.mk_tuple;
          (OpSeq.OpSeq(skel, seq), ctx, id_gen);
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
               id_gen: IDGen.t,
             ),
             skel: UHPat.skel,
           ) => {
             let (skel, seq, _, ctx, id_gen) =
               syn_fix_holes_skel(
                 ctx,
                 id_gen,
                 ~renumber_empty_holes,
                 ~pattern_syn,
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
            (opseq, ctx, id_gen);
          }
      );
    } else {
      let (u, id_gen) = id_gen |> IDGen.next_hole;
      let (opseq, _, _, id_gen) =
        syn_fix_holes_opseq(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          ~pattern_syn,
          opseq |> UHPat.set_err_status_opseq(NotInHole),
        );
      (
        opseq |> UHPat.set_err_status_opseq(InHole(WrongLength, u)),
        ctx,
        id_gen,
      );
    }
  };
}
and ana_fix_holes_skel =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : (UHPat.skel, UHPat.seq, Contexts.t, IDGen.t) => {
  let unknown = unknown(pattern_syn);
  switch (skel) {
  | BinOp(_, Comma, _, _) =>
    failwith("Pat.ana_fix_holes_skel: tuples handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    let (pn, ctx, id_gen) =
      ana_fix_holes_operand_internal(
        ~pattern_syn,
        ctx,
        id_gen,
        ~renumber_empty_holes,
        pn,
        ty,
      );
    let seq = seq |> Seq.update_nth_operand(n, pn);
    (skel, seq, ctx, id_gen);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, id_gen) = {
      let (skel1, seq, ty, ctx, id_gen) =
        syn_fix_holes_skel(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
        );
      let (skel1, seq, id_gen) =
        switch (HTyp.matched_arrow(ty)) {
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
    let (skel2, seq, ctx, id_gen) =
      ana_fix_holes_skel(
        ~pattern_syn,
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        unknown,
      );
    let (u, id_gen) = IDGen.next_hole(id_gen);
    let skel =
      Skel.BinOp(
        InHole(TypeInconsistent, u),
        Operators_Pat.Space,
        skel1,
        skel2,
      );
    (skel, seq, ctx, id_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | Some(ty_elt) =>
      let (skel1, seq, ctx, id_gen) =
        ana_fix_holes_skel(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        );
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, ctx, id_gen) =
        ana_fix_holes_skel(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
      (skel, seq, ctx, id_gen);
    | None =>
      let (skel1, seq, ty_elt, ctx, id_gen) =
        syn_fix_holes_skel(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
        );
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, ctx, id_gen) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          ~pattern_syn,
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
      (skel, seq, ctx, id_gen);
    }
  };
}
and ana_fix_holes_operand =
  ana_fix_holes_operand_internal(~pattern_syn=Internal)
and ana_fix_holes_operand_internal =
    (
      ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      ~pattern_syn: pattern_syn,
      operand: UHPat.operand,
      ty: HTyp.t,
    )
    : (UHPat.operand, Contexts.t, IDGen.t) => {
  let operand_nih = UHPat.set_err_status_operand(NotInHole, operand);
  switch (operand) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyHole(u), ctx, id_gen);
    } else {
      (operand, ctx, id_gen);
    }
  | Wild(_) => (operand_nih, ctx, id_gen)
  | InvalidText(_) => (operand_nih, ctx, id_gen)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(ExpandingKeyword(_), _), _) => (
      operand_nih,
      ctx,
      id_gen,
    )
  | Var(_, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, ty));
    (operand_nih, ctx, id_gen);
  | IntLit(_, _)
  | FloatLit(_, _)
  | BoolLit(_, _) =>
    let (operand', ty', ctx, id_gen) =
      syn_fix_holes_operand(
        ~pattern_syn,
        ctx,
        id_gen,
        ~renumber_empty_holes,
        operand,
      );
    if (HTyp.consistent(ty, ty')) {
      (UHPat.set_err_status_operand(NotInHole, operand'), ctx, id_gen);
    } else {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        UHPat.set_err_status_operand(InHole(TypeInconsistent, u), operand'),
        ctx,
        id_gen,
      );
    };
  | ListNil(_) =>
    switch (HTyp.matched_list(ty)) {
    | Some(_) => (ListNil(NotInHole), ctx, id_gen)
    | None =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (ListNil(InHole(TypeInconsistent, u)), ctx, id_gen);
    }
  | Parenthesized(p1) =>
    let (p1, ctx, id_gen) =
      ana_fix_holes_internal(
        ~pattern_syn,
        ctx,
        id_gen,
        ~renumber_empty_holes,
        p1,
        ty,
      );
    (Parenthesized(p1), ctx, id_gen);
  | Inj(_, side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      let (p1, ctx, id_gen) =
        ana_fix_holes_internal(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          p1,
          ty1,
        );
      (Inj(NotInHole, side, p1), ctx, id_gen);
    | None =>
      let (p1, _, ctx, id_gen) =
        syn_fix_holes_internal(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          p1,
        );
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (Inj(InHole(TypeInconsistent, u), side, p1), ctx, id_gen);
    }
  | TypeAnn(err, op, ann) =>
    let ty_ann = UHTyp.expand(ann);
    if (HTyp.consistent(ty, ty_ann)) {
      let (op, ctx, id_gen) =
        ana_fix_holes_operand_internal(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          op,
          ty_ann,
        );
      (TypeAnn(NotInHole, op, ann), ctx, id_gen);
    } else {
      let (op, _, _, id_gen) =
        syn_fix_holes_operand(
          ~pattern_syn,
          ctx,
          id_gen,
          ~renumber_empty_holes,
          op,
        );
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        UHPat.set_err_status_operand(
          InHole(TypeInconsistent, u),
          TypeAnn(err, op, ann),
        ),
        ctx,
        id_gen,
      );
    };
  };
};

let syn_fix_holes_z_internal =
    (~pattern_syn: pattern_syn, ctx: Contexts.t, id_gen: IDGen.t, zp: ZPat.t)
    : (ZPat.t, HTyp.t, Contexts.t, IDGen.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ty, ctx, id_gen) =
    syn_fix_holes_internal(~pattern_syn, ctx, id_gen, ZPat.erase(zp));
  let zp =
    CursorPath_Pat.follow(path, p)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zp, ty, ctx, id_gen);
};

let syn_fix_holes_z = syn_fix_holes_z_internal(~pattern_syn=Internal);

let ana_fix_holes_z_internal =
    (
      ~pattern_syn: pattern_syn,
      ctx: Contexts.t,
      id_gen: IDGen.t,
      zp: ZPat.t,
      ty: HTyp.t,
    )
    : (ZPat.t, Contexts.t, IDGen.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ctx, id_gen) =
    ana_fix_holes_internal(~pattern_syn, ctx, id_gen, ZPat.erase(zp), ty);
  let zp =
    CursorPath_Pat.follow(path, p)
    |> OptUtil.get(() =>
         failwith(
           "ana_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zp, ctx, id_gen);
};

let ana_fix_holes_z = ana_fix_holes_z_internal(~pattern_syn=Internal);
