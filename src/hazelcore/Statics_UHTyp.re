let rec syn_fix_holes:
  (Context.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, Kind.t, MetaVarGen.t) =
  (ctx, u_gen, ty) =>
    Log.fun_call(
      __FUNCTION__,
      ~args=[
        ("ctx", () => Context.sexp_of_t(ctx)),
        ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
        ("ty", () => UHTyp.sexp_of_t(ty)),
      ],
      ~id=u_gen,
      ~result_sexp=
        ((ty, k, u_gen)) =>
          List([
            UHTyp.sexp_of_t(ty),
            Kind.sexp_of_t(k),
            MetaVarGen.sexp_of_t(u_gen),
          ]),
      () =>
        switch (ty) {
        | OpSeq(skel, seq) =>
          let (skel, seq, k, u_gen) =
            syn_fix_holes_skel(ctx, u_gen, skel, seq);
          (OpSeq(skel, seq), k, u_gen);
        },
    )

and syn_fix_holes_skel = (ctx, u_gen, skel, seq) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
      ("skel", () => UHTyp.sexp_of_skel(skel)),
      (
        "seq",
        () =>
          Seq.sexp_of_t(UHTyp.sexp_of_operand, UHTyp.sexp_of_operator, seq),
      ),
    ],
    ~id=u_gen,
    ~result_sexp=
      ((skel, seq, k, u_gen)) =>
        List([
          UHTyp.sexp_of_skel(skel),
          Seq.sexp_of_t(UHTyp.sexp_of_operand, UHTyp.sexp_of_operator, seq),
          Kind.sexp_of_t(k),
          MetaVarGen.sexp_of_t(u_gen),
        ]),
    () =>
      switch (skel) {
      | Placeholder(n) =>
        let ty_n = seq |> Seq.nth_operand(n);
        let (ty_n, k, u_gen) = syn_fix_holes_operand(ctx, u_gen, ty_n);
        let seq = seq |> Seq.update_nth_operand(n, ty_n);
        (skel, seq, k, u_gen);
      | BinOp(_, op, skel1, skel2) =>
        let (skel1, seq, _, u_gen) =
          ana_fix_holes_skel(ctx, u_gen, skel1, seq, Kind.Type);
        let (skel2, seq, _, u_gen) =
          ana_fix_holes_skel(ctx, u_gen, skel2, seq, Kind.Type);
        switch (
          Elaborator_Typ.syn_elab(ctx, Delta.empty, UHTyp.mk_OpSeq(seq))
        ) {
        | Some((_, k, _)) =>
          let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
          (skel, seq, k, u_gen);
        | None =>
          failwith(
            "TODO: Add inconsistent kind hole (this can't happen now) 1",
          )
        };
      },
  )

and syn_fix_holes_operand =
    (ctx: Context.t, u_gen: MetaVarGen.t, operand: UHTyp.operand)
    : (UHTyp.operand, Kind.t, MetaVarGen.t) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
      ("operand", () => UHTyp.sexp_of_operand(operand)),
    ],
    ~id=u_gen,
    ~result_sexp=
      ((operand, k, u_gen)) =>
        List([
          UHTyp.sexp_of_operand(operand),
          Kind.sexp_of_t(k),
          MetaVarGen.sexp_of_t(u_gen),
        ]),
    () => {
      switch (operand) {
      | Hole => (Hole, Kind.Hole, u_gen)
      | TyVar(NotInTyVarHole(index, stamp), t) =>
        let cref = KindSystem.ContextRef.{index, stamp};
        let cref = Context.rescope(ctx, cref);
        let k' = Kind.singleton(HTyp.tyvar(ctx, cref.index, t));
        switch (Context.tyvar_kind(ctx, cref)) {
        | Some(k) when Kind.consistent_subkind(ctx, k', k) => (
            operand,
            k',
            u_gen,
          )
        | Some(_)
        | None =>
          let reason: TyVarErrStatus.HoleReason.t =
            if (TyVar.reserved_word(t)) {
              Reserved;
            } else if (TyVar.valid_name(t)) {
              Unbound;
            } else {
              InvalidName;
            };
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let ty = UHTyp.TyVar(InHole(reason, u), t);
          let k = Kind.singleton(HTyp.tyvarhole(reason, u, t));
          (ty, k, u_gen);
        };
      | TyVar(InHole(_, u), t) =>
        if (TyVar.reserved_word(t)) {
          let ty = UHTyp.TyVar(InHole(Reserved, u), t);
          let k = Kind.singleton(HTyp.tyvarhole(Reserved, u, t));
          (ty, k, u_gen);
        } else if (TyVar.valid_name(t)) {
          switch (Context.tyvar_ref(ctx, t)) {
          | None =>
            let ty = UHTyp.TyVar(InHole(Unbound, u), t);
            let k = Kind.singleton(HTyp.tyvarhole(Unbound, u, t));
            (ty, k, u_gen);
          | Some(cref) =>
            let ty = UHTyp.TyVar(NotInTyVarHole(cref.index, cref.stamp), t);
            let k = Kind.singleton(HTyp.tyvar(ctx, cref.index, t));
            (ty, k, u_gen);
          };
        } else {
          let ty = UHTyp.TyVar(InHole(InvalidName, u), t);
          (ty, Kind.S(TyVarHole(InvalidName, u, t)), u_gen);
        }
      | Unit
      | Int
      | Float
      | Bool => (
          operand,
          Kind.singleton(UHTyp.expand_operand(operand)),
          u_gen,
        )
      | Parenthesized(body) =>
        let (block, k, u_gen) = syn_fix_holes(ctx, u_gen, body);
        (Parenthesized(block), k, u_gen);
      | List(opseq) =>
        let (opseq, k, u_gen) = syn_fix_holes(ctx, u_gen, opseq);
        (List(opseq), k, u_gen);
      }
    },
  )

and ana_fix_holes:
  (Context.t, MetaVarGen.t, UHTyp.t, Kind.t) =>
  (UHTyp.t, Kind.t, MetaVarGen.t) =
  (ctx, u_gen, ty, k) =>
    Log.fun_call(
      __FUNCTION__,
      ~args=[
        ("ctx", () => Context.sexp_of_t(ctx)),
        ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
        ("ty", () => UHTyp.sexp_of_t(ty)),
        ("k", () => Kind.sexp_of_t(k)),
      ],
      ~id=u_gen,
      ~result_sexp=
        ((ty, k, u_gen)) =>
          List([
            UHTyp.sexp_of_t(ty),
            Kind.sexp_of_t(k),
            MetaVarGen.sexp_of_t(u_gen),
          ]),
      () =>
        switch (ty) {
        | OpSeq(skel, seq) =>
          let (skel, seq, k', u_gen) =
            ana_fix_holes_skel(ctx, u_gen, skel, seq, k);
          if (Kind.consistent_subkind(ctx, k', k)) {
            (OpSeq(skel, seq), k', u_gen);
          } else {
            failwith(
              "TODO: Add inconsistent kind hole (this can't happen now) 2",
            );
          };
        },
    )

and ana_fix_holes_skel = (tyvars, u_gen, skel, seq, k) =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("tyvars", () => Context.sexp_of_t(tyvars)),
      ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
      ("skel", () => UHTyp.sexp_of_skel(skel)),
      (
        "seq",
        () =>
          Seq.sexp_of_t(UHTyp.sexp_of_operand, UHTyp.sexp_of_operator, seq),
      ),
      ("k", () => Kind.sexp_of_t(k)),
    ],
    ~id=u_gen,
    ~result_sexp=
      ((skel, seq, k, u_gen)) =>
        List([
          UHTyp.sexp_of_skel(skel),
          Seq.sexp_of_t(UHTyp.sexp_of_operand, UHTyp.sexp_of_operator, seq),
          Kind.sexp_of_t(k),
          MetaVarGen.sexp_of_t(u_gen),
        ]),
    () =>
      switch (skel) {
      | Placeholder(n) =>
        let ty_n = seq |> Seq.nth_operand(n);
        let (ty_n, k', u_gen) =
          ana_fix_holes_operand(tyvars, u_gen, ty_n, k);
        let seq = seq |> Seq.update_nth_operand(n, ty_n);
        (skel, seq, k', u_gen);
      | BinOp(_, _, _, _) =>
        let (skel, seq, k', u_gen) =
          syn_fix_holes_skel(tyvars, u_gen, skel, seq);
        if (Kind.consistent_subkind(tyvars, k', k)) {
          (skel, seq, k', u_gen);
        } else {
          failwith(
            "TODO: Add inconsistent kind hole (this can't happen now) 3",
          );
        };
      },
  )

and ana_fix_holes_operand = (ctx, u_gen, operand, k) => {
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
      ("operand", () => UHTyp.sexp_of_operand(operand)),
      ("k", () => Kind.sexp_of_t(k)),
    ],
    ~id=u_gen,
    ~result_sexp=
      ((operand, k, u_gen)) =>
        List([
          UHTyp.sexp_of_operand(operand),
          Kind.sexp_of_t(k),
          MetaVarGen.sexp_of_t(u_gen),
        ]),
    () =>
      switch (operand) {
      | UHTyp.Hole => (Hole, Kind.Hole, u_gen)
      | Parenthesized(body) =>
        let (block, k', u_gen) = ana_fix_holes(ctx, u_gen, body, k);
        if (Kind.consistent_subkind(ctx, k', k)) {
          (Parenthesized(block), k', u_gen);
        } else {
          failwith(
            "TODO: Add inconsistent kind hole (this can't happen now) 4",
          );
        };
      // subsumption
      | TyVar(_)
      | Unit
      | Int
      | Float
      | Bool
      | List(_) =>
        let (ty, k', u_gen) = syn_fix_holes_operand(ctx, u_gen, operand);
        if (Kind.consistent_subkind(ctx, k', k)) {
          (ty, k', u_gen);
        } else {
          failwith(
            "TODO: Add inconsistent kind hole (this can't happen now) 5",
          );
        };
      },
  );
};
