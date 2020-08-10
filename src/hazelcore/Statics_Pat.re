let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHPat.get_tuple_elements);

let rec syn = (ctx: Contexts.t, p: UHPat.t): option((HTyp.t, Contexts.t)) =>
  syn_opseq(ctx, p)
and syn_opseq =
    (ctx: Contexts.t, OpSeq(skel, seq): UHPat.opseq)
    : option((HTyp.t, Contexts.t)) =>
  syn_skel(ctx, skel, seq)
and syn_skel =
    (ctx: Contexts.t, skel: UHPat.skel, seq: UHPat.seq)
    : option((HTyp.t, Contexts.t)) =>
  switch (skel) {
  | Placeholder(n) =>
    let pn = seq |> Seq.nth_operand(n);
    syn_operand(ctx, pn);
  | BinOp(InHole(_), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel(ctx, skel_not_in_hole, seq)) {
    | None => None
    | Some((_, ctx)) => Some((HTyp.Hole, ctx))
    };
  | BinOp(NotInHole, Comma, _, _) =>
    skel
    |> UHPat.get_tuple_elements
    |> ListUtil.map_with_accumulator_opt(
         (ctx, skel) =>
           syn_skel(ctx, skel, seq) |> Option.map(TupleUtil.swap),
         ctx,
       )
    |> Option.map(((ctx, tys)) => (HTyp.Prod(tys), ctx))
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Hole)) {
    | None => None
    | Some(ctx) =>
      switch (ana_skel(ctx, skel2, seq, HTyp.Hole)) {
      | None => None
      | Some(ctx) => Some((Hole, ctx))
      }
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (syn_skel(ctx, skel1, seq)) {
    | None => None
    | Some((ty1, ctx)) =>
      let ty = HTyp.List(ty1);
      switch (ana_skel(ctx, skel2, seq, ty)) {
      | None => None
      | Some(ctx) => Some((ty, ctx))
      };
    }
  }
and syn_operand =
    (ctx: Contexts.t, operand: UHPat.operand): option((HTyp.t, Contexts.t)) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some((Hole, ctx))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    syn_operand(ctx, operand')
    |> OptUtil.map(((_, gamma)) => (HTyp.Hole, gamma));
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _) => None
  /* not in hole */
  | Wild(NotInHole) => Some((Hole, ctx))
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(_), _), _) => Some((Hole, ctx))
  | Var(NotInHole, NotInVarHole, x) =>
    Var.check_valid(
      x,
      Some((HTyp.Hole, Contexts.extend_gamma(ctx, (x, Hole)))),
    )
  | IntLit(NotInHole, _) => Some((Int, ctx))
  | FloatLit(NotInHole, _) => Some((Float, ctx))
  | BoolLit(NotInHole, _) => Some((Bool, ctx))
  | ListNil(NotInHole) => Some((List(Hole), ctx))
  | Inj(NotInHole, inj_side, p1) =>
    switch (syn(ctx, p1)) {
    | None => None
    | Some((ty1, ctx)) =>
      let ty =
        switch (inj_side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Some((ty, ctx));
    }
  | Parenthesized(p) => syn(ctx, p)
  }
and ana = (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t): option(Contexts.t) =>
  ana_opseq(ctx, p, ty)
and ana_opseq =
    (ctx: Contexts.t, OpSeq(skel, seq) as opseq: UHPat.opseq, ty: HTyp.t)
    : option(Contexts.t) =>
  switch (tuple_zip(skel, ty)) {
  | None =>
    switch (UHPat.get_err_status_opseq(opseq), HTyp.get_prod_elements(ty)) {
    | (InHole(TypeInconsistent, _), [_])
    | (InHole(WrongLength, _), _) =>
      let opseq' = opseq |> UHPat.set_err_status_opseq(NotInHole);
      syn_opseq(ctx, opseq') |> OptUtil.map(_ => ctx);
    | _ => None
    }
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (acc: option(Contexts.t), (skel, ty)) =>
           switch (acc) {
           | None => None
           | Some(ctx) => ana_skel(ctx, skel, seq, ty)
           },
         Some(ctx),
       )
  }
and ana_skel =
    (ctx: Contexts.t, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t)
    : option(Contexts.t) =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    failwith("Pat.ana_skel: expected tuples to be handled at opseq level")
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    ana_operand(ctx, pn, ty);
  | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel(ctx, skel_not_in_hole, seq)) {
    | None => None
    | Some((_, ctx)) => Some(ctx)
    };
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Hole)) {
    | None => None
    | Some(ctx) => ana_skel(ctx, skel2, seq, HTyp.Hole)
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_skel(ctx, skel1, seq, ty_elt)) {
      | None => None
      | Some(ctx) => ana_skel(ctx, skel2, seq, HTyp.List(ty_elt))
      }
    }
  }
and ana_operand =
    (ctx: Contexts.t, operand: UHPat.operand, ty: HTyp.t): option(Contexts.t) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some(ctx)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let operand' = UHPat.set_err_status_operand(NotInHole, operand);
    syn_operand(ctx, operand') |> OptUtil.map(((_, ctx)) => ctx);
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _) =>
    ty |> HTyp.get_prod_elements |> List.length > 1 ? Some(ctx) : None
  /* not in hole */
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(_), _), _) => Some(ctx)
  | Var(NotInHole, NotInVarHole, x) =>
    Var.check_valid(x, Some(Contexts.extend_gamma(ctx, (x, ty))))
  | Wild(NotInHole) => Some(ctx)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    switch (syn_operand(ctx, operand)) {
    | None => None
    | Some((ty', ctx')) =>
      if (HTyp.consistent(ty, ty')) {
        Some(ctx');
      } else {
        None;
      }
    }
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(_) => Some(ctx)
    }
  | Inj(NotInHole, side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      ana(ctx, p1, ty1);
    }
  | Parenthesized(p) => ana(ctx, p, ty)
  };

/**
     * Get type mode of nth operand of an opseq in synthetic position
     */
let rec syn_nth_type_mode =
        (ctx: Contexts.t, n: int, OpSeq(skel, seq): UHPat.opseq)
        : option(Statics_common.type_mode) =>
  syn_nth_type_mode'(ctx, n, skel, seq)
and syn_nth_type_mode' =
    (ctx: Contexts.t, n: int, skel: UHPat.skel, seq: UHPat.seq)
    : option(Statics_common.type_mode) => {
  let ana_go = (skel, ty) => ana_nth_type_mode'(ctx, n, skel, seq, ty);
  let rec go = (skel: UHPat.skel) =>
    switch (skel) {
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics_common.Syn);
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
          switch (HTyp.matched_arrow(ty1)) {
          | None => None
          | Some((ty2, _)) => ana_go(skel2, ty2)
          };
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq)) {
      | None => None
      | Some((ty1, _)) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? go(skel1) : ana_go(skel2, HTyp.List(ty1))
      }
    };
  go(skel);
}
/**
     * Get type mode of nth operand of an opseq in analytic position
     */
and ana_nth_type_mode =
    (
      ctx: Contexts.t,
      n: int,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : option(Statics_common.type_mode) => {
  // handle n-tuples
  switch (tuple_zip(skel, ty)) {
  | None =>
    syn_nth_type_mode(ctx, n, opseq |> UHPat.set_err_status_opseq(NotInHole))
  | Some(skel_tys) =>
    let (nskel, nty) =
      skel_tys
      |> List.find(((skel, _)) =>
           Skel.leftmost_tm_index(skel) <= n
           && n <= Skel.rightmost_tm_index(skel)
         );
    ana_nth_type_mode'(ctx, n, nskel, seq, nty);
  };
}
and ana_nth_type_mode' =
    (ctx: Contexts.t, n: int, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t)
    : option(Statics_common.type_mode) => {
  let rec go = (skel: UHPat.skel, ty: HTyp.t) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      failwith(__LOC__ ++ ": expected tuples to be handled at opseq level")
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics_common.Ana(ty));
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      syn_nth_type_mode'(ctx, n, skel_not_in_hole, seq);
    | BinOp(NotInHole, Space, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1, HTyp.Hole) : go(skel2, HTyp.Hole)
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? go(skel1, ty_elt) : go(skel2, ty)
      }
    };
  go(skel, ty);
};

let stable_syn_fixer = f => {
  let g =
    Statics_common.stable_syn_fixer(
      (ctx, u_gen, ~renumber_empty_holes, ~extra_input as (), term) =>
      f(ctx, u_gen, ~renumber_empty_holes, term)
    );
  (ctx, u_gen, ~renumber_empty_holes, term) =>
    g(ctx, u_gen, ~renumber_empty_holes, ~extra_input=(), term);
};

let stable_ana_fixer = f => {
  let g =
    Statics_common.stable_ana_fixer(
      (ctx, u_gen, ~renumber_empty_holes, ~extra_input as (), term, ty) =>
      f(ctx, u_gen, ~renumber_empty_holes, term, ty)
    );
  (ctx, u_gen, ~renumber_empty_holes, term, ty) =>
    g(ctx, u_gen, ~renumber_empty_holes, ~extra_input=(), term, ty);
};

/**
 * Note: need to wrap these definitions with lazy in order to make them statically
 * constructive <https://caml.inria.fr/pub/docs/manual-ocaml/letrecvalues.html>
 */
let rec syn_fix_holes' =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          ~renumber_empty_holes=false,
          p: UHPat.t,
        )
        : (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t, bool) => {
  let (p, (ty, ctx), u_gen, fixed) =
    Lazy.force(syn_fix_holes_opseq', ctx, u_gen, ~renumber_empty_holes, p);
  (p, ty, ctx, u_gen, fixed);
}
and syn_fix_holes_opseq' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        OpSeq(skel, seq): UHPat.opseq,
      ) => {
      let ((skel, seq), (ty, ctx), u_gen, fixed) =
        Lazy.force(
          syn_fix_holes_skel',
          ctx,
          u_gen,
          ~renumber_empty_holes,
          (skel, seq),
        );
      (OpSeq(skel, seq), (ty, ctx), u_gen, fixed);
    })
  )
and syn_fix_holes_skel' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        (skel: UHPat.skel, seq: UHPat.seq),
      ) =>
      switch (skel) {
      | Placeholder(n) =>
        let pn = Seq.nth_operand(n, seq);
        let (pn, (ty, ctx), u_gen, fixed) =
          Lazy.force(
            syn_fix_holes_operand',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            pn,
          );
        let seq = seq |> Seq.update_nth_operand(n, pn);
        ((skel, seq), (ty, ctx), u_gen, fixed);
      | BinOp(err, Comma, _, _) =>
        let ((ctx, u_gen, seq, fixed), pairs) =
          skel
          |> UHPat.get_tuple_elements
          |> ListUtil.map_with_accumulator(
               ((ctx, u_gen, seq, fixed), skel) => {
                 let ((skel, seq), (ty, ctx), u_gen, elem_fixed) =
                   Lazy.force(
                     syn_fix_holes_skel',
                     ctx,
                     u_gen,
                     ~renumber_empty_holes,
                     (skel, seq),
                   );
                 ((ctx, u_gen, seq, fixed || elem_fixed), (skel, ty));
               },
               (ctx, u_gen, seq, err != NotInHole),
             );
        let (skels, tys) = List.split(pairs);
        (
          (UHPat.mk_tuple(skels), seq),
          (HTyp.Prod(tys), ctx),
          u_gen,
          fixed,
        );
      | BinOp(_, Space, skel1, skel2) =>
        let ((skel1, seq), ctx, u_gen, fixed1) =
          Lazy.force(
            ana_fix_holes_skel',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            (skel1, seq),
            HTyp.Hole,
          );
        let ((skel2, seq), ctx, u_gen, fixed2) =
          Lazy.force(
            ana_fix_holes_skel',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            (skel2, seq),
            HTyp.Hole,
          );
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let skel =
          Skel.BinOp(
            InHole(TypeInconsistent, u),
            Operators_Pat.Space,
            skel1,
            skel2,
          );
        let ty = HTyp.Hole;
        ((skel, seq), (ty, ctx), u_gen, fixed1 || fixed2);
      | BinOp(_, Cons, skel1, skel2) =>
        let ((skel1, seq), (ty_elt, ctx), u_gen, fixed1) =
          Lazy.force(
            syn_fix_holes_skel',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            (skel1, seq),
          );
        let ty = HTyp.List(ty_elt);
        let ((skel2, seq), ctx, u_gen, fixed2) =
          Lazy.force(
            ana_fix_holes_skel',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            (skel2, seq),
            ty,
          );
        let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
        ((skel, seq), (ty, ctx), u_gen, fixed1 || fixed2);
      }
    )
  )
and syn_fix_holes_operand' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        operand: UHPat.operand,
      ) =>
      (
        {
          let operand_nih = operand |> UHPat.set_err_status_operand(NotInHole);
          let was_in_hole =
            UHPat.get_err_status_operand(operand) != NotInHole;
          switch (operand) {
          | EmptyHole(_) =>
            if (renumber_empty_holes) {
              let (u, u_gen) = MetaVarGen.next(u_gen);
              (EmptyHole(u), (Hole, ctx), u_gen, true);
            } else {
              (operand, (HTyp.Hole, ctx), u_gen, false);
            }
          | Wild(_) => (operand_nih, (Hole, ctx), u_gen, was_in_hole)
          | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
          | Var(_, InVarHole(Keyword(_), _), _) => (
              operand_nih,
              (Hole, ctx),
              u_gen,
              was_in_hole,
            )
          | Var(_, NotInVarHole, x) =>
            let ctx = Contexts.extend_gamma(ctx, (x, Hole));
            (operand_nih, (Hole, ctx), u_gen, was_in_hole);
          | IntLit(_, _) => (operand_nih, (Int, ctx), u_gen, was_in_hole)
          | FloatLit(_, _) => (
              operand_nih,
              (Float, ctx),
              u_gen,
              was_in_hole,
            )
          | BoolLit(_, _) => (operand_nih, (Bool, ctx), u_gen, was_in_hole)
          | ListNil(_) => (
              operand_nih,
              (List(Hole), ctx),
              u_gen,
              was_in_hole,
            )
          | Parenthesized(p) =>
            let (p, ty, ctx, u_gen, fixed) =
              syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p);
            (Parenthesized(p), (ty, ctx), u_gen, fixed);
          | Inj(_, side, p1) =>
            let (p1, ty1, ctx, u_gen, fixed) =
              syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p1);
            let p = UHPat.Inj(NotInHole, side, p1);
            let ty =
              switch (side) {
              | L => HTyp.Sum(ty1, Hole)
              | R => HTyp.Sum(Hole, ty1)
              };
            (p, (ty, ctx), u_gen, was_in_hole || fixed);
          };
        }: (
          UHPat.operand,
          (HTyp.t, Contexts.t),
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes' =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      p: UHPat.t,
      ty: HTyp.t,
    )
    : (UHPat.t, Contexts.t, MetaVarGen.t, bool) => {
  let (p, ctx, u_gen, fixed) =
    Lazy.force(
      ana_fix_holes_opseq',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      p,
      ty,
    );
  (p, ctx, u_gen, fixed);
}
and ana_fix_holes_opseq' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        OpSeq(skel, seq) as opseq: UHPat.opseq,
        ty: HTyp.t,
      ) =>
      (
        {
          let err = UHPat.get_err_status_opseq(opseq);
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
                     fixed: bool,
                   ),
                   (skel: UHPat.skel, ty: HTyp.t),
                 ) => {
                   let ((skel, seq), ctx, u_gen, elem_fixed) =
                     Lazy.force(
                       ana_fix_holes_skel',
                       ctx,
                       u_gen,
                       ~renumber_empty_holes,
                       (skel, seq),
                       ty,
                     );
                   (
                     [skel, ...rev_skels],
                     seq,
                     ctx,
                     u_gen,
                     fixed || elem_fixed,
                   );
                 },
                 ([], seq, ctx, u_gen, false),
               )
            |> (
              fun
              | (rev_skels, seq, ctx, u_gen, fixed_elems) => {
                  let fixed = err != NotInHole || fixed_elems;
                  let skel = UHPat.mk_tuple(List.rev(rev_skels));
                  (OpSeq.OpSeq(skel, seq), ctx, u_gen, fixed);
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
                       fixed: bool,
                     ),
                     skel: UHPat.skel,
                   ) => {
                     let ((skel, seq), (_, ctx), u_gen, elem_fixed) =
                       Lazy.force(
                         syn_fix_holes_skel',
                         ctx,
                         u_gen,
                         ~renumber_empty_holes,
                         (skel, seq),
                       );
                     (
                       [skel, ...rev_skels],
                       seq,
                       ctx,
                       u_gen,
                       fixed || elem_fixed,
                     );
                   },
                   ([], seq, ctx, u_gen, false),
                 )
              |> (
                fun
                | (rev_skels, seq, ctx, u_gen, fixed_elems) => {
                    let (err, u_gen, fixed) =
                      Statics_common.set_hole_reason(
                        u_gen,
                        TypeInconsistent,
                        err,
                      );
                    let skel = UHPat.mk_tuple(List.rev(rev_skels));
                    let opseq =
                      UHPat.set_err_status_opseq(
                        err,
                        OpSeq.OpSeq(skel, seq),
                      );
                    (opseq, ctx, u_gen, fixed || fixed_elems);
                  }
              );
            } else {
              let (err, u_gen, fixed) =
                Statics_common.set_hole_reason(u_gen, WrongLength, err);
              let (opseq, (_, _), u_gen, fixed_elems) =
                Lazy.force(
                  syn_fix_holes_opseq',
                  ctx,
                  u_gen,
                  ~renumber_empty_holes,
                  UHPat.set_err_status_opseq(NotInHole, opseq),
                );
              (
                UHPat.set_err_status_opseq(err, opseq),
                ctx,
                u_gen,
                fixed || fixed_elems,
              );
            }
          };
        }: (
          UHPat.opseq,
          Contexts.t,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_skel' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        (skel: UHPat.skel, seq: UHPat.seq),
        ty: HTyp.t,
      ) =>
      (
        switch (skel) {
        | BinOp(_, Comma, _, _) =>
          failwith("Pat.ana_fix_holes_skel': tuples handled at opseq level")
        | Placeholder(n) =>
          let pn = Seq.nth_operand(n, seq);
          let (pn, ctx, u_gen, fixed) =
            Lazy.force(
              ana_fix_holes_operand',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              pn,
              ty,
            );
          let seq = seq |> Seq.update_nth_operand(n, pn);
          ((skel, seq), ctx, u_gen, fixed);
        | BinOp(err, Space, skel1, skel2) =>
          let ((skel1, seq), ctx, u_gen, fixed1) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
              HTyp.Hole,
            );
          let ((skel2, seq), ctx, u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              HTyp.Hole,
            );
          let (err, u_gen, fixed) =
            switch (err) {
            | InHole(TypeInconsistent, _) => (err, u_gen, fixed1 || fixed2)
            | _ =>
              let (u, u_gen) = MetaVarGen.next(u_gen);
              (InHole(TypeInconsistent, u), u_gen, true);
            };
          let skel = Skel.BinOp(err, Operators_Pat.Space, skel1, skel2);
          ((skel, seq), ctx, u_gen, fixed);
        | BinOp(err, Cons, skel1, skel2) =>
          switch (HTyp.matched_list(ty)) {
          | Some(ty_elt) =>
            let ((skel1, seq), ctx, u_gen, fixed1) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel1, seq),
                ty_elt,
              );
            let ty_list = HTyp.List(ty_elt);
            let ((skel2, seq), ctx, u_gen, fixed2) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel2, seq),
                ty_list,
              );
            let skel =
              Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
            ((skel, seq), ctx, u_gen, fixed1 || fixed2 || err != NotInHole);
          | None =>
            let ((skel1, seq), (ty_elt, ctx), u_gen, fixed1) =
              Lazy.force(
                syn_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel1, seq),
              );
            let ty_list = HTyp.List(ty_elt);
            let ((skel2, seq), ctx, u_gen, fixed2) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel2, seq),
                ty_list,
              );
            let (err, u_gen, outer_fixed) =
              Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
            let skel = Skel.BinOp(err, Operators_Pat.Cons, skel1, skel2);
            ((skel, seq), ctx, u_gen, outer_fixed || fixed1 || fixed2);
          }
        }: (
          (UHPat.skel, UHPat.seq),
          Contexts.t,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_operand' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        operand: UHPat.operand,
        ty: HTyp.t,
      ) =>
      (
        {
          let err = UHPat.get_err_status_operand(operand);
          let was_in_hole = err != NotInHole;
          let operand_nih = UHPat.set_err_status_operand(NotInHole, operand);
          switch (operand) {
          | EmptyHole(_) =>
            if (renumber_empty_holes) {
              let (u, u_gen) = MetaVarGen.next(u_gen);
              (EmptyHole(u), ctx, u_gen, true);
            } else {
              (operand, ctx, u_gen, false);
            }
          | Wild(_) => (operand_nih, ctx, u_gen, was_in_hole)
          | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
          | Var(_, InVarHole(Keyword(_), _), _) => (
              operand_nih,
              ctx,
              u_gen,
              was_in_hole,
            )
          | Var(_, NotInVarHole, x) =>
            let ctx = Contexts.extend_gamma(ctx, (x, ty));
            (operand_nih, ctx, u_gen, was_in_hole);
          | IntLit(_, _)
          | FloatLit(_, _)
          | BoolLit(_, _) =>
            let (operand', (ty', ctx), u_gen, _) =
              Lazy.force(
                syn_fix_holes_operand',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                operand,
              );
            if (HTyp.consistent(ty, ty')) {
              (
                UHPat.set_err_status_operand(NotInHole, operand'),
                ctx,
                u_gen,
                was_in_hole,
              );
            } else {
              let (err, u_gen, fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (
                UHPat.set_err_status_operand(err, operand'),
                ctx,
                u_gen,
                fixed,
              );
            };
          | ListNil(_) =>
            switch (HTyp.matched_list(ty)) {
            | Some(_) => (ListNil(NotInHole), ctx, u_gen, was_in_hole)
            | None =>
              let (err, u_gen, fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (ListNil(err), ctx, u_gen, fixed);
            }
          | Parenthesized(p1) =>
            let (p1, ctx, u_gen, fixed) =
              ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p1, ty);
            (Parenthesized(p1), ctx, u_gen, fixed);
          | Inj(_, side, p1) =>
            switch (HTyp.matched_sum(ty)) {
            | Some((tyL, tyR)) =>
              let ty1 = InjSide.pick(side, tyL, tyR);
              let (p1, ctx, u_gen, fixed) =
                ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p1, ty1);
              (Inj(NotInHole, side, p1), ctx, u_gen, was_in_hole || fixed);
            | None =>
              let (p1, _, ctx, u_gen, inner_fixed) =
                syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p1);
              let (err, u_gen, outer_fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (Inj(err, side, p1), ctx, u_gen, inner_fixed || outer_fixed);
            }
          };
        }: (
          UHPat.operand,
          Contexts.t,
          MetaVarGen.t,
          bool,
        )
      )
    )
  );

let ana_fix_holes = (ctx, u_gen, ~renumber_empty_holes=false, p, ty) => {
  let (p, ctx, u_gen, _) =
    ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p, ty);
  (p, ctx, u_gen);
};

let ana_fix_holes_opseq = (ctx, u_gen, ~renumber_empty_holes=false, opseq, ty) => {
  let (opseq, ctx, u_gen, _) =
    Lazy.force(
      ana_fix_holes_opseq',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      opseq,
      ty,
    );
  (opseq, ctx, u_gen);
};

let syn_fix_holes = (ctx, u_gen, ~renumber_empty_holes=false, p) => {
  let (p, ty, ctx, u_gen, _) =
    syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, p);
  (p, ty, ctx, u_gen);
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
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
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
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
         )
       );
  (zp, ctx, u_gen);
};
