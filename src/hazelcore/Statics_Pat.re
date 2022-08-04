open OptUtil.Syntax;

let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHPat.get_tuple_elements);

let rec syn =
        (ctx: Context.t, p: UHPat.t)
        : option((HTyp.t, Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("p", () => UHPat.sexp_of_t(p)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ty, ctx, xi)) =>
        List([
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          Constraint.sexp_of_t(xi),
        ])
      ),
    () =>
    syn_opseq(ctx, p)
  )

and syn_opseq =
    (ctx: Context.t, OpSeq(skel, seq): UHPat.opseq)
    : option((HTyp.t, Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("opseq", () => UHPat.sexp_of_opseq(OpSeq(skel, seq))),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ty, ctx, xi)) =>
        List([
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          Constraint.sexp_of_t(xi),
        ])
      ),
    () =>
    syn_skel(ctx, skel, seq)
  )

and syn_skel =
    (ctx: Context.t, skel: UHPat.skel, seq: UHPat.seq)
    : option((HTyp.t, Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("skel", () => UHPat.sexp_of_skel(skel)),
      ("seq", () => UHPat.sexp_of_seq(seq)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ty, ctx, xi)) =>
        List([
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          Constraint.sexp_of_t(xi),
        ])
      ),
    () =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      syn_operand(ctx, pn);
    | BinOp(InHole(_), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      let+ (_, ctx, _) = syn_skel(ctx, skel_not_in_hole, seq);
      (HTyp.hole(), ctx, Constraint.Hole);
    | BinOp(NotInHole, Comma, _, _) =>
      let+ (ctx, tys_and_xis) =
        skel
        |> UHPat.get_tuple_elements
        |> ListUtil.map_with_accumulator_opt(
             (ctx, skel) => {
               let+ (ty, ctx, xi) = syn_skel(ctx, skel, seq);
               (ctx, (ty, xi));
             },
             ctx,
           );
      let (tys, xis) = List.split(tys_and_xis);
      switch (xis) {
      | [] => failwith("not implemented")
      | [xi0, ...xis] =>
        let xi =
          List.fold_right((xi, xi1) => Constraint.Pair(xi1, xi), xis, xi0);
        (HTyp.product(tys), ctx, xi);
      };
    | BinOp(NotInHole, Space, skel1, skel2) =>
      let* (ctx, _) = ana_skel(ctx, skel1, seq, HTyp.hole());
      let+ (ctx, _) = ana_skel(ctx, skel2, seq, HTyp.hole());
      (HTyp.hole(), ctx, Constraint.Hole);
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* (ty1, ctx, _) = syn_skel(ctx, skel1, seq);
      let* (ctx, xi1) = ana_skel(ctx, skel1, seq, ty1);
      let+ (ctx, xi2) = ana_skel(ctx, skel2, seq, HTyp.list(ty1));
      (HTyp.list(ty1), ctx, Constraint.InjR(Pair(xi1, xi2)));
    }
  )

and syn_operand =
    (ctx: Context.t, operand: UHPat.operand)
    : option((HTyp.t, Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("operand", () => UHPat.sexp_of_operand(operand)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ty, ctx, xi)) =>
        List([
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          Constraint.sexp_of_t(xi),
        ])
      ),
    () =>
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
          Constraint.Truth,
        )),
      )
    | IntLit(NotInHole, n) =>
      Some((HTyp.int(), ctx, Int(int_of_string(n))))
    | FloatLit(NotInHole, n) =>
      Some((HTyp.float(), ctx, Float(float_of_string(n))))
    | BoolLit(NotInHole, b) =>
      Some((HTyp.bool(), ctx, b ? InjL(Truth) : InjR(Truth)))
    | ListNil(NotInHole) => Some((HTyp.list(HTyp.hole()), ctx, Truth))
    | Inj(NotInHole, inj_side, p1) =>
      let+ (ty1, ctx, xi1) = syn(ctx, p1);
      switch (inj_side) {
      | L => (HTyp.sum(ty1, HTyp.hole()), ctx, Constraint.InjL(xi1))
      | R => (HTyp.sum(HTyp.hole(), ty1), ctx, Constraint.InjR(xi1))
      };
    | Parenthesized(p) => syn(ctx, p)
    | TypeAnn(NotInHole, op, ann) =>
      let* (ty_ann, _, _) = Elaborator_Typ.syn_elab(ctx, Delta.empty, ann);
      let* (ctx_ann, xi_ann) = ana_operand(ctx, op, ty_ann);
      let+ x = List.nth_opt(ctx_ann, 0) |> Option.map(Context.binding_name);
      let ctx = Context.add_var(ctx, x, ty_ann);
      (ty_ann, ctx, xi_ann);
    }
  )

and ana =
    (ctx: Context.t, p: UHPat.t, ty: HTyp.t)
    : option((Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("p", () => UHPat.sexp_of_t(p)),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ctx, xi)) =>
        List([Context.sexp_of_t(ctx), Constraint.sexp_of_t(xi)])
      ),
    () =>
    ana_opseq(ctx, p, ty)
  )

and ana_opseq =
    (ctx: Context.t, OpSeq(skel, seq) as opseq: UHPat.opseq, ty: HTyp.t)
    : option((Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("opseq", () => UHPat.sexp_of_opseq(OpSeq(skel, seq))),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ctx, xi)) =>
        List([Context.sexp_of_t(ctx), Constraint.sexp_of_t(xi)])
      ),
    () => {
      let ty_head_normed = HTyp.head_normalize(ctx, ty);
      switch (tuple_zip(skel, ty_head_normed)) {
      | None =>
        switch (
          UHPat.get_err_status_opseq(opseq),
          HTyp.get_prod_elements(ty_head_normed),
        ) {
        | (InHole(TypeInconsistent, _), [_])
        | (InHole(WrongLength, _), _) =>
          let opseq = UHPat.set_err_status_opseq(NotInHole, opseq);
          let+ (_, ctx, _) = syn_opseq(ctx, opseq);
          (ctx, Constraint.Hole);
        | _ => None
        }
      | Some(skel_tys) =>
        switch (List.rev(skel_tys)) {
        | [] => None
        | [(skel, ty)] => ana_skel(ctx, skel, seq, ty)
        | [(skel, ty), ...skel_tys] =>
          let* (ctx, xi) = ana_skel(ctx, skel, seq, ty);
          List.fold_left(
            (acc, (skel, ty)) => {
              let* (ctx, xi) = acc;
              let+ (ctx, xi') = ana_skel(ctx, skel, seq, ty);
              (ctx, Constraint.Pair(xi', xi));
            },
            Some((ctx, xi)),
            skel_tys,
          );
        }
      };
    },
  )

and ana_skel =
    (ctx: Context.t, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t)
    : option((Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("skel", () => UHPat.sexp_of_skel(skel)),
      ("seq", () => UHPat.sexp_of_seq(seq)),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ctx, xi)) =>
        List([Context.sexp_of_t(ctx), Constraint.sexp_of_t(xi)])
      ),
    () =>
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
      (ctx, Constraint.Hole);
    | BinOp(NotInHole, Space, skel1, skel2) =>
      let* (ctx, _) = ana_skel(ctx, skel1, seq, HTyp.hole());
      let+ (ctx, _) = ana_skel(ctx, skel2, seq, HTyp.hole());
      (ctx, Constraint.Hole);
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty1 = HTyp.matched_list(ctx, ty);
      let* (ctx, xi1) = ana_skel(ctx, skel1, seq, ty1);
      let+ (ctx, xi2) = ana_skel(ctx, skel2, seq, HTyp.list(ty1));
      (ctx, Constraint.InjR(Pair(xi1, xi2)));
    }
  )

and ana_operand =
    (ctx: Context.t, operand: UHPat.operand, ty: HTyp.t)
    : option((Context.t, Constraint.t)) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("operand", () => UHPat.sexp_of_operand(operand)),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      Sexplib.Std.sexp_of_option(((ctx, xi)) =>
        List([Context.sexp_of_t(ctx), Constraint.sexp_of_t(xi)])
      ),
    () =>
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
      (ctx, Constraint.Hole);
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
        ? Some((ctx, Constraint.Hole)) : None
    /* not in hole */
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(ExpandingKeyword(_), _), _) =>
      Some((ctx, Falsity))
    | Var(NotInHole, NotInVarHole, x) =>
      Var.check_valid(
        x,
        Some((Context.add_var(ctx, x, ty), Constraint.Truth)),
      )
    | Wild(NotInHole) => Some((ctx, Truth))
    | IntLit(NotInHole, n) =>
      let* (ty', ctx', _) = syn_operand(ctx, operand);
      HTyp.consistent(ctx, ty, ty')
        ? Some((ctx', Constraint.Int(int_of_string(n)))) : None;
    | FloatLit(NotInHole, n) =>
      let* (ty', ctx', _) = syn_operand(ctx, operand);
      HTyp.consistent(ctx, ty, ty')
        ? Some((ctx', Constraint.Float(float_of_string(n)))) : None;
    | BoolLit(NotInHole, b) =>
      let* (ty', ctx', _) = syn_operand(ctx, operand);
      HTyp.consistent(ctx, ty, ty')
        ? Some((ctx', b ? Constraint.InjL(Truth) : InjR(Truth))) : None;
    | ListNil(NotInHole) =>
      let+ _ = HTyp.matched_list(ctx, ty);
      (ctx, Constraint.InjL(Truth));
    | Inj(NotInHole, side, p1) =>
      let* (tyL, tyR) = HTyp.matched_sum(ctx, ty);
      let ty1 = InjSide.pick(side, tyL, tyR);
      let* (ctx, xi1) = ana(ctx, p1, ty1);
      InjSide.pick(
        side,
        Some((ctx, Constraint.InjL(xi1))),
        Some((ctx, Constraint.InjR(xi1))),
      );
    | TypeAnn(NotInHole, op, ann) =>
      let* (ty_ann, _, _) = Elaborator_Typ.syn_elab(ctx, Delta.empty, ann);
      HTyp.consistent(ctx, ty, ty_ann) ? ana_operand(ctx, op, ty_ann) : None;
    | Parenthesized(p) => ana(ctx, p, ty)
    }
  );

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
        : (UHPat.t, HTyp.t, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("p", () => UHPat.sexp_of_t(p)),
    ],
    ~result_sexp=
      ((p, ty, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_t(p),
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () => syn_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, p),
  )

and syn_fix_holes_opseq =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq): UHPat.opseq,
    )
    : (UHPat.opseq, HTyp.t, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("opseq", () => UHPat.sexp_of_opseq(OpSeq(skel, seq))),
    ],
    ~result_sexp=
      ((p, ty, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_t(p),
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () => {
      let (skel, seq, ty, ctx, id_gen) =
        syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel, seq);
      (OpSeq(skel, seq), ty, ctx, id_gen);
    },
  )

and syn_fix_holes_skel =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel,
      seq: UHPat.seq,
    )
    : (UHPat.skel, UHPat.seq, HTyp.t, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("skel", () => UHPat.sexp_of_skel(skel)),
      ("seq", () => UHPat.sexp_of_seq(seq)),
    ],
    ~result_sexp=
      ((skel, seq, ty, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_skel(skel),
          UHPat.sexp_of_seq(seq),
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () =>
      switch (skel) {
      | Placeholder(n) =>
        let pn = Seq.nth_operand(n, seq);
        let (pn, ty, ctx, id_gen) =
          syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, pn);
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
                     skel,
                     seq,
                   );
                 ((ctx, id_gen, seq), (skel, ty));
               },
               (ctx, id_gen, seq),
             );
        let (skels, tys) = List.split(pairs);
        (UHPat.mk_tuple(skels), seq, HTyp.product(tys), ctx, id_gen);
      | BinOp(_, Space, skel1, skel2) =>
        let (skel1, seq, ctx, id_gen) = {
          let (skel1, seq, ty, ctx, id_gen) =
            syn_fix_holes_skel(
              ctx,
              id_gen,
              ~renumber_empty_holes,
              skel1,
              seq,
            );
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
        let (skel2, seq, ctx, id_gen) =
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
        (skel, seq, ty, ctx, id_gen);
      | BinOp(_, Cons, skel1, skel2) =>
        let (skel1, seq, ty_elt, ctx, id_gen) =
          syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
        let ty = HTyp.list(ty_elt);
        let (skel2, seq, ctx, id_gen) =
          ana_fix_holes_skel(
            ctx,
            id_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            ty,
          );
        let skel = Skel.BinOp(NotInHole, Operators_Pat.Cons, skel1, skel2);
        (skel, seq, ty, ctx, id_gen);
      },
  )

and syn_fix_holes_operand =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      operand: UHPat.operand,
    )
    : (UHPat.operand, HTyp.t, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("operand", () => UHPat.sexp_of_operand(operand)),
    ],
    ~result_sexp=
      ((operand, ty, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_operand(operand),
          HTyp.sexp_of_t(ty),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () => {
      let operand_nih = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (operand) {
      | EmptyHole(_) =>
        if (renumber_empty_holes) {
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (EmptyHole(u), HTyp.hole(), ctx, id_gen);
        } else {
          (operand, HTyp.hole(), ctx, id_gen);
        }
      | Wild(_) => (operand_nih, HTyp.hole(), ctx, id_gen)
      | InvalidText(_) => (operand_nih, HTyp.hole(), ctx, id_gen)
      | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
      | Var(_, InVarHole(ExpandingKeyword(_), _), _) => (
          operand_nih,
          HTyp.hole(),
          ctx,
          id_gen,
        )
      | Var(_, NotInVarHole, x) =>
        let ctx = Context.add_var(ctx, x, HTyp.hole());
        (operand_nih, HTyp.hole(), ctx, id_gen);
      | IntLit(_, _) => (operand_nih, HTyp.int(), ctx, id_gen)
      | FloatLit(_, _) => (operand_nih, HTyp.float(), ctx, id_gen)
      | BoolLit(_, _) => (operand_nih, HTyp.bool(), ctx, id_gen)
      | ListNil(_) => (operand_nih, HTyp.list(HTyp.hole()), ctx, id_gen)
      | Parenthesized(p) =>
        let (p, ty, ctx, id_gen) =
          syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p);
        (Parenthesized(p), ty, ctx, id_gen);
      | Inj(_, side, p1) =>
        let (p1, ty1, ctx, id_gen) =
          syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1);
        let p = UHPat.Inj(NotInHole, side, p1);
        let ty =
          switch (side) {
          | L => HTyp.sum(ty1, HTyp.hole())
          | R => HTyp.sum(HTyp.hole(), ty1)
          };
        (p, ty, ctx, id_gen);
      | TypeAnn(_, op, ann) =>
        let (ann, _, id_gen) = Statics_UHTyp.syn_fix_holes(ctx, id_gen, ann);
        switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ann)) {
        | Some((ty_ann, _, _)) =>
          if (HTyp.complete(ty_ann)) {
            let (op, ctx, id_gen) =
              ana_fix_holes_operand(
                ctx,
                id_gen,
                ~renumber_empty_holes,
                op,
                ty_ann,
              );
            (UHPat.TypeAnn(NotInHole, op, ann), ty_ann, ctx, id_gen);
          } else {
            let (ann, _, id_gen) =
              Statics_UHTyp.syn_fix_holes(ctx, id_gen, ann);
            (UHPat.TypeAnn(NotInHole, op, ann), ty_ann, ctx, id_gen);
          }
        | None =>
          let (op, ty, ctx, id_gen) =
            syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, op);
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (
            UHPat.TypeAnn(InHole(TypeInconsistent, u), op, ann),
            ty,
            ctx,
            id_gen,
          );
        };
      };
    },
  )

and ana_fix_holes =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      p: UHPat.t,
      ty: HTyp.t,
    )
    : (UHPat.t, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("p", () => UHPat.sexp_of_t(p)),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      ((p, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_t(p),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () => ana_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, p, ty),
  )

and ana_fix_holes_opseq =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : (UHPat.opseq, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("opseq", () => UHPat.sexp_of_opseq(OpSeq(skel, seq))),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      ((p, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_t(p),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () => {
      let ty_head_normed = HTyp.head_normalize(ctx, ty);
      // handle n-tuples
      switch (tuple_zip(skel, ty_head_normed)) {
      | Some(skel_tys) =>
        skel_tys
        |> List.fold_left(
             (
               (
                 rev_skels: list(UHPat.skel),
                 seq: UHPat.seq,
                 ctx: Context.t,
                 id_gen: IDGen.t,
               ),
               (skel: UHPat.skel, ty: HTyp.t),
             ) => {
               let (skel, seq, ctx, id_gen) =
                 ana_fix_holes_skel(
                   ctx,
                   id_gen,
                   ~renumber_empty_holes,
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
                 let (skel, seq, _, ctx, id_gen) =
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
              opseq |> UHPat.set_err_status_opseq(NotInHole),
            );
          (
            opseq |> UHPat.set_err_status_opseq(InHole(WrongLength, u)),
            ctx,
            id_gen,
          );
        }
      };
    },
  )

and ana_fix_holes_skel =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : (UHPat.skel, UHPat.seq, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("skel", () => UHPat.sexp_of_skel(skel)),
      ("seq", () => UHPat.sexp_of_seq(seq)),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      ((skel, seq, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_skel(skel),
          UHPat.sexp_of_seq(seq),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () =>
      switch (skel) {
      | BinOp(_, Comma, _, _) =>
        failwith("Pat.ana_fix_holes_skel: tuples handled at opseq level")
      | Placeholder(n) =>
        let pn = Seq.nth_operand(n, seq);
        let (pn, ctx, id_gen) =
          ana_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, pn, ty);
        let seq = seq |> Seq.update_nth_operand(n, pn);
        (skel, seq, ctx, id_gen);
      | BinOp(_, Space, skel1, skel2) =>
        let (skel1, seq, ctx, id_gen) = {
          let (skel1, seq, ty, ctx, id_gen) =
            syn_fix_holes_skel(
              ctx,
              id_gen,
              ~renumber_empty_holes,
              skel1,
              seq,
            );
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
        let (skel2, seq, ctx, id_gen) =
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
        (skel, seq, ctx, id_gen);
      | BinOp(_, Cons, skel1, skel2) =>
        switch (HTyp.matched_list(ctx, ty)) {
        | Some(ty_elt) =>
          let (skel1, seq, ctx, id_gen) =
            ana_fix_holes_skel(
              ctx,
              id_gen,
              ~renumber_empty_holes,
              skel1,
              seq,
              ty_elt,
            );
          let ty_list = HTyp.list(ty_elt);
          let (skel2, seq, ctx, id_gen) =
            ana_fix_holes_skel(
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
              ctx,
              id_gen,
              ~renumber_empty_holes,
              skel1,
              seq,
            );
          let ty_list = HTyp.list(ty_elt);
          let (skel2, seq, ctx, id_gen) =
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
          (skel, seq, ctx, id_gen);
        }
      },
  )

and ana_fix_holes_operand =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      operand: UHPat.operand,
      ty: HTyp.t,
    )
    : (UHPat.operand, Context.t, IDGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("id_gen", () => IDGen.sexp_of_t(id_gen)),
      (
        "renumber_empty_holes",
        () => Sexplib.Std.sexp_of_bool(renumber_empty_holes),
      ),
      ("operand", () => UHPat.sexp_of_operand(operand)),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=
      ((operand, ctx, id_gen)) =>
        List([
          UHPat.sexp_of_operand(operand),
          Context.sexp_of_t(ctx),
          IDGen.sexp_of_t(id_gen),
        ]),
    () => {
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
        let ctx = Context.add_var(ctx, x, ty);
        (operand_nih, ctx, id_gen);
      | IntLit(_, _)
      | FloatLit(_, _)
      | BoolLit(_, _) =>
        let (operand', ty', ctx, id_gen) =
          syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, operand);
        if (HTyp.consistent(ctx, ty, ty')) {
          (UHPat.set_err_status_operand(NotInHole, operand'), ctx, id_gen);
        } else {
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (
            UHPat.set_err_status_operand(
              InHole(TypeInconsistent, u),
              operand',
            ),
            ctx,
            id_gen,
          );
        };
      | ListNil(_) =>
        switch (HTyp.matched_list(ctx, ty)) {
        | Some(_) => (ListNil(NotInHole), ctx, id_gen)
        | None =>
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (ListNil(InHole(TypeInconsistent, u)), ctx, id_gen);
        }
      | Parenthesized(p1) =>
        let (p1, ctx, id_gen) =
          ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1, ty);
        (Parenthesized(p1), ctx, id_gen);
      | Inj(_, side, p1) =>
        switch (HTyp.matched_sum(ctx, ty)) {
        | Some((tyL, tyR)) =>
          let ty1 = InjSide.pick(side, tyL, tyR);
          let (p1, ctx, id_gen) =
            ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1, ty1);
          (Inj(NotInHole, side, p1), ctx, id_gen);
        | None =>
          let (p1, _, ctx, id_gen) =
            syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p1);
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (Inj(InHole(TypeInconsistent, u), side, p1), ctx, id_gen);
        }
      | TypeAnn(err, op, ann) =>
        switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ann)) {
        | Some((ty_ann, _, _)) when HTyp.consistent(ctx, ty, ty_ann) =>
          let (op, ctx, id_gen) =
            ana_fix_holes_operand(
              ctx,
              id_gen,
              ~renumber_empty_holes,
              op,
              ty_ann,
            );
          (TypeAnn(NotInHole, op, ann), ctx, id_gen);
        | Some(_)
        | None =>
          let (op, _, _, id_gen) =
            syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, op);
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (
            UHPat.set_err_status_operand(
              InHole(TypeInconsistent, u),
              TypeAnn(err, op, ann),
            ),
            ctx,
            id_gen,
          );
        }
      };
    },
  );

let syn_fix_holes_z =
    (ctx: Context.t, id_gen: IDGen.t, zp: ZPat.t)
    : (ZPat.t, HTyp.t, Context.t, IDGen.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ty, ctx, id_gen) = syn_fix_holes(ctx, id_gen, ZPat.erase(zp));
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

let ana_fix_holes_z =
    (ctx: Context.t, id_gen: IDGen.t, zp: ZPat.t, ty: HTyp.t)
    : (ZPat.t, Context.t, IDGen.t) => {
  let path = CursorPath_Pat.of_z(zp);
  let (p, ctx, id_gen) = ana_fix_holes(ctx, id_gen, ZPat.erase(zp), ty);
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
