module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHPat.t, HTyp.t, Context.t, Delta.t)
    | DoesNotElaborate;

  let to_option =
    fun
    | DoesNotElaborate => None
    | Elaborates(pat, ty, ctx, delta) => Some((pat, ty, ctx, delta));

  let from_option =
    fun
    | None => DoesNotElaborate
    | Some((pat, ty, ctx, delta)) => Elaborates(pat, ty, ctx, delta);

  let bind = (x: t, ~f: ((DHPat.t, HTyp.t, Context.t, Delta.t)) => t): t =>
    switch (x) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, ty, ctx, delta) => f((dp, ty, ctx, delta))
    };
};

module Let_syntax = ElaborationResult;

let rec syn_elab =
        (ctx: Context.t, delta: Delta.t, p: UHPat.t): ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("p", UHPat.sexp_of_t(p)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    syn_elab_opseq(ctx, delta, p)
  )

and syn_elab_opseq =
    (ctx: Context.t, delta: Delta.t, OpSeq(skel, seq): UHPat.opseq)
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("opseq", UHPat.sexp_of_opseq(OpSeq(skel, seq))),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    syn_elab_skel(ctx, delta, skel, seq)
  )

and syn_elab_skel =
    (ctx: Context.t, delta: Delta.t, skel: UHPat.skel, seq: UHPat.seq)
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("skel", UHPat.sexp_of_skel(skel)),
      ("seq", UHPat.sexp_of_seq(seq)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    switch (skel) {
    | Placeholder(n) =>
      syn_elab_operand(ctx, delta, seq |> Seq.nth_operand(n))
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp, _, ctx, delta) =>
        let delta =
          MetaVarMap.add(u, Delta.Hole.Pattern(HTyp.hole(), ctx), delta);
        Elaborates(NonEmptyHole(reason, u, 0, dp), HTyp.hole(), ctx, delta);
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotElaborate
    | BinOp(NotInHole, Comma, _, _) =>
      switch (UHPat.get_tuple_elements(skel)) {
      | [skel1, skel2, ...tail] =>
        let%bind (dp1, ty1, ctx, delta) =
          syn_elab_skel(ctx, delta, skel1, seq);
        let%bind (dp2, ty2, ctx, delta) =
          syn_elab_skel(ctx, delta, skel2, seq);
        tail
        |> ListUtil.map_with_accumulator_opt(
             ((dp_acc, ctx, delta), skel) => {
               syn_elab_skel(ctx, delta, skel, seq)
               |> ElaborationResult.to_option
               |> Option.map(((dp, ty, ctx, delta)) =>
                    ((DHPat.Pair(dp_acc, dp), ctx, delta), ty)
                  )
             },
             (DHPat.Pair(dp1, dp2), ctx, delta),
           )
        |> Option.map((((dp_acc, ctx, delta), tys)) =>
             (dp_acc, HTyp.product([ty1, ty2, ...tys]), ctx, delta)
           )
        |> ElaborationResult.from_option;
      | _ =>
        raise(
          Invalid_argument(
            "Encountered tuple pattern type with less than 2 elements!",
          ),
        )
      }
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_elab_skel(ctx, delta, skel1, seq)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        switch (syn_elab_skel(ctx, delta, skel2, seq)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp2, _, ctx, delta) =>
          let dp = DHPat.Ap(dp1, dp2);
          Elaborates(dp, HTyp.hole(), ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_elab_skel(ctx, delta, skel1, seq)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, ty1, ctx, delta) =>
        let ty = HTyp.list(ty1);
        switch (ana_elab_skel(ctx, delta, skel2, seq, ty)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp2, _, ctx, delta) =>
          let dp = DHPat.Cons(dp1, dp2);
          Elaborates(dp, ty, ctx, delta);
        };
      }
    }
  )

and syn_elab_operand =
    (ctx: Context.t, delta: Delta.t, operand: UHPat.operand)
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("operand", UHPat.sexp_of_operand(operand)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (syn_elab_operand(ctx, delta, operand')) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp, _, ctx, delta) =>
        let delta =
          MetaVarMap.add(u, Delta.Hole.Pattern(HTyp.hole(), ctx), delta);
        Elaborates(NonEmptyHole(reason, u, 0, dp), HTyp.hole(), ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => DoesNotElaborate
    | EmptyHole(u) =>
      let dp = DHPat.EmptyHole(u, 0);
      let ty = HTyp.hole();
      let delta = MetaVarMap.add(u, Delta.Hole.Pattern(ty, ctx), delta);
      Elaborates(dp, ty, ctx, delta);
    | InvalidText(u, t) =>
      let dp = DHPat.InvalidText(u, 0, t);
      let ty = HTyp.hole();
      let delta = MetaVarMap.add(u, Delta.Hole.Pattern(ty, ctx), delta);
      Elaborates(dp, ty, ctx, delta);
    | Wild(NotInHole) => Elaborates(Wild, HTyp.hole(), ctx, delta)
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
      Elaborates(Keyword(u, 0, k), HTyp.hole(), ctx, delta)
    | Var(NotInHole, NotInVarHole, x) =>
      let ctx = Context.add_var(ctx, x, HTyp.hole());
      Elaborates(Var(x), HTyp.hole(), ctx, delta);
    | IntLit(NotInHole, n) =>
      switch (int_of_string_opt(n)) {
      | Some(n) => Elaborates(IntLit(n), HTyp.int(), ctx, delta)
      | None => DoesNotElaborate
      }
    | FloatLit(NotInHole, f) =>
      switch (TextShape.hazel_float_of_string_opt(f)) {
      | Some(f) => Elaborates(FloatLit(f), HTyp.float(), ctx, delta)
      | None => DoesNotElaborate
      }
    | BoolLit(NotInHole, b) =>
      Elaborates(BoolLit(b), HTyp.bool(), ctx, delta)
    | ListNil(NotInHole) =>
      Elaborates(ListNil, HTyp.list(HTyp.hole()), ctx, delta)
    | Parenthesized(p1) => syn_elab(ctx, delta, p1)
    | Inj(NotInHole, side, p) =>
      switch (syn_elab(ctx, delta, p)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, ty1, ctx, delta) =>
        let dp = DHPat.Inj(side, dp1);
        let ty =
          switch (side) {
          | L => HTyp.sum(ty1, HTyp.hole())
          | R => HTyp.sum(HTyp.hole(), ty1)
          };
        Elaborates(dp, ty, ctx, delta);
      }
    | TypeAnn(_, p1, ty1) =>
      switch (Elaborator_Typ.syn_elab(ctx, delta, ty1)) {
      | None => DoesNotElaborate
      | Some((ty1, _, _)) => ana_elab_operand(ctx, delta, p1, ty1)
      }
    }
  )

and ana_elab =
    (ctx: Context.t, delta: Delta.t, p: UHPat.t, ty: HTyp.t)
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("p", UHPat.sexp_of_t(p)),
      ("ty", HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    ana_elab_opseq(ctx, delta, p, ty)
  )

and ana_elab_opseq =
    (
      ctx: Context.t,
      delta: Delta.t,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("skel", UHPat.sexp_of_skel(skel)),
      ("seq", UHPat.sexp_of_seq(seq)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () => {
      let ty_h = HTyp.head_normalize(ctx, ty);
      // handle n-tuples
      switch (Statics_Pat.tuple_zip(skel, ty_h)) {
      | Some(skel_tys) =>
        skel_tys
        |> List.fold_left(
             (
               acc: option((list(DHPat.t), Context.t, Delta.t)),
               (skel: UHPat.skel, ty: HTyp.t),
             ) =>
               switch (acc) {
               | None => None
               | Some((rev_dps, ctx, delta)) =>
                 switch (ana_elab_skel(ctx, delta, skel, seq, ty)) {
                 | DoesNotElaborate => None
                 | Elaborates(dp, _, ctx, delta) =>
                   Some(([dp, ...rev_dps], ctx, delta))
                 }
               },
             Some(([], ctx, delta)),
           )
        |> (
          fun
          | None => ElaborationResult.DoesNotElaborate
          | Some((rev_dps, ctx, delta)) => {
              let dp = rev_dps |> List.rev |> DHPat.mk_tuple;
              Elaborates(dp, ty, ctx, delta);
            }
        )
      | None =>
        if (List.length(HTyp.get_prod_elements(ty_h)) == 1) {
          skel
          |> UHPat.get_tuple_elements
          |> List.fold_left(
               (
                 acc: option((list(DHPat.t), Context.t, Delta.t)),
                 skel: UHPat.skel,
               ) =>
                 switch (acc) {
                 | None => None
                 | Some((rev_dps, ctx, delta)) =>
                   switch (syn_elab_skel(ctx, delta, skel, seq)) {
                   | DoesNotElaborate => None
                   | Elaborates(dp, _, ctx, delta) =>
                     Some(([dp, ...rev_dps], ctx, delta))
                   }
                 },
               Some(([], ctx, delta)),
             )
          |> (
            fun
            | None => ElaborationResult.DoesNotElaborate
            | Some((rev_dps, ctx, delta)) => {
                let dp = DHPat.mk_tuple(List.rev(rev_dps));
                Elaborates(dp, ty, ctx, delta);
              }
          );
        } else {
          switch (opseq |> UHPat.get_err_status_opseq) {
          | NotInHole
          | InHole(TypeInconsistent, _) => DoesNotElaborate
          | InHole(WrongLength as reason, u) =>
            switch (
              syn_elab_opseq(
                ctx,
                delta,
                opseq |> UHPat.set_err_status_opseq(NotInHole),
              )
            ) {
            | DoesNotElaborate => DoesNotElaborate
            | Elaborates(dp, _, _, delta) =>
              let delta =
                MetaVarMap.add(u, Delta.Hole.Pattern(ty, ctx), delta);
              Elaborates(NonEmptyHole(reason, u, 0, dp), ty, ctx, delta);
            }
          };
        }
      };
    },
  )

and ana_elab_skel =
    (
      ctx: Context.t,
      delta: Delta.t,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("skel", UHPat.sexp_of_skel(skel)),
      ("seq", UHPat.sexp_of_seq(seq)),
      ("ty", HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      // tuples handled at opseq level
      DoesNotElaborate
    | Placeholder(n) =>
      let pn = seq |> Seq.nth_operand(n);
      ana_elab_operand(ctx, delta, pn, ty);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
        let delta = MetaVarMap.add(u, Delta.Hole.Pattern(ty, ctx), delta);
        Elaborates(dp, ty, ctx, delta);
      };
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, HTyp.hole())) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, _ty1, ctx, delta) =>
        switch (ana_elab_skel(ctx, delta, skel2, seq, HTyp.hole())) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp2, _ty2, ctx, delta) =>
          let dp = DHPat.Ap(dp1, dp2);
          Elaborates(dp, HTyp.hole(), ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ctx, ty)) {
      | None => DoesNotElaborate
      | Some(ty_elt) =>
        switch (ana_elab_skel(ctx, delta, skel1, seq, ty_elt)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp1, _, ctx, delta) =>
          let ty_list = HTyp.list(ty_elt);
          switch (ana_elab_skel(ctx, delta, skel2, seq, ty_list)) {
          | DoesNotElaborate => DoesNotElaborate
          | Elaborates(dp2, _, ctx, delta) =>
            let dp = DHPat.Cons(dp1, dp2);
            Elaborates(dp, ty, ctx, delta);
          };
        }
      }
    }
  )

and ana_elab_operand =
    (ctx: Context.t, delta: Delta.t, operand: UHPat.operand, ty: HTyp.t)
    : ElaborationResult.t =>
  Log.debug_function(
    __FUNCTION__,
    [
      ("ctx", Context.sexp_of_t(ctx)),
      ("delta", Delta.sexp_of_t(delta)),
      ("operand", UHPat.sexp_of_operand(operand)),
      ("ty", HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=ElaborationResult.sexp_of_t,
    () =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | TypeAnn(InHole(TypeInconsistent as reason, u), _, _) =>
      let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (syn_elab_operand(ctx, delta, operand')) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
        let delta = MetaVarMap.add(u, Delta.Hole.Pattern(ty, ctx), delta);
        Elaborates(dp, ty, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _)
    | TypeAnn(InHole(WrongLength, _), _, _) => DoesNotElaborate
    | EmptyHole(u) =>
      let dp = DHPat.EmptyHole(u, 0);
      let delta = MetaVarMap.add(u, Delta.Hole.Pattern(ty, ctx), delta);
      Elaborates(dp, ty, ctx, delta);
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
      Elaborates(Keyword(u, 0, k), ty, ctx, delta)
    | Var(NotInHole, NotInVarHole, x) =>
      let ctx = Context.add_var(ctx, x, ty);
      Elaborates(Var(x), ty, ctx, delta);
    | Wild(NotInHole) => Elaborates(Wild, ty, ctx, delta)
    | InvalidText(_, _)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | BoolLit(NotInHole, _) => syn_elab_operand(ctx, delta, operand)
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ctx, ty)) {
      | None => DoesNotElaborate
      | Some(ty_elt) => Elaborates(ListNil, HTyp.list(ty_elt), ctx, delta)
      }
    | Parenthesized(p) => ana_elab(ctx, delta, p, ty)
    | Inj(NotInHole, side, p1) =>
      switch (HTyp.matched_sum(ctx, ty)) {
      | None => DoesNotElaborate
      | Some((tyL, tyR)) =>
        let ty1 = InjSide.pick(side, tyL, tyR);
        switch (ana_elab(ctx, delta, p1, ty1)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp1, ty1, ctx, delta) =>
          let ty =
            switch (side) {
            | L => HTyp.sum(ty1, tyR)
            | R => HTyp.sum(tyL, ty1)
            };
          Elaborates(Inj(side, dp1), ty, ctx, delta);
        };
      }
    | TypeAnn(NotInHole, op, _) => ana_elab_operand(ctx, delta, op, ty)
    }
  );

let rec renumber_result_only =
        (path: InstancePath.t, hii: HoleInstanceInfo.t, dp: DHPat.t)
        : (DHPat.t, HoleInstanceInfo.t) =>
  switch (dp) {
  | Wild
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | InvalidText(_)
  | BoolLit(_)
  | ListNil
  | Triv => (dp, hii)
  | EmptyHole(u, _) =>
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (EmptyHole(u, i), hii);
  | NonEmptyHole(reason, u, _, dp1) =>
    /* TODO: see above */
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    (NonEmptyHole(reason, u, i, dp1), hii);
  | Keyword(u, _, k) =>
    /* TODO: see above */
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (Keyword(u, i, k), hii);
  | Inj(side, dp1) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    (Inj(side, dp1), hii);
  | Pair(dp1, dp2) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    let (dp2, hii) = renumber_result_only(path, hii, dp2);
    (Pair(dp1, dp2), hii);
  | Cons(dp1, dp2) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    let (dp2, hii) = renumber_result_only(path, hii, dp2);
    (Cons(dp1, dp2), hii);
  | Ap(dp1, dp2) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    let (dp2, hii) = renumber_result_only(path, hii, dp2);
    (Pair(dp1, dp2), hii);
  };
