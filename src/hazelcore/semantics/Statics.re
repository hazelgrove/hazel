open GeneralUtil;

[@deriving sexp]
type edit_state = (ZExp.zblock, HTyp.t, MetaVarGen.t);

/* see syn_skel and ana_skel below */
type type_mode =
  | AnalyzedAgainst(HTyp.t)
  | Synthesized(HTyp.t);

let combine_modes = (mode1, mode2) =>
  switch (mode1, mode2) {
  | (Some(_), _) => mode1
  | (_, Some(_)) => mode2
  | (None, None) => None
  };

module Pat = {
  let rec syn = (ctx: Contexts.t, p: UHPat.t): option((HTyp.t, Contexts.t)) =>
    syn_opseq(ctx, p)
  and syn_opseq =
      (ctx: Contexts.t, OpSeq(skel, seq): UHPat.opseq)
      : option((HTyp.t, Contexts.t)) =>
    syn_skel(ctx, skel, seq, None) |> Opt.map(((ty, ctx, _)) => (ty, ctx))
  and syn_skel = (ctx: Contexts.t, skel: UHPat.skel, seq: UHPat.seq, monitor) =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = seq |> Seq.nth_operand(n);
      switch (UHPat.bidelimited(pn)) {
      | false => None
      | true =>
        switch (syn_operand(ctx, pn)) {
        | None => None
        | Some((ty, ctx)) =>
          let mode =
            switch (monitor) {
            | None => None
            | Some(n') =>
              if (n == n') {
                Some(Synthesized(ty));
              } else {
                None;
              }
            };
          Some((ty, ctx, mode));
        }
      };
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2)
    | BinOp(InHole(WrongLength, _), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_skel(ctx, skel_not_in_hole, seq, monitor)) {
      | None => None
      | Some((_, ctx, mode)) => Some((HTyp.Hole, ctx, mode))
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => None
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq, monitor)) {
      | None => None
      | Some((ty1, ctx, mode1)) =>
        switch (syn_skel(ctx, skel2, seq, monitor)) {
        | None => None
        | Some((ty2, ctx, mode2)) =>
          let ty = HTyp.Prod(ty1, ty2);
          let mode = combine_modes(mode1, mode2);
          Some((ty, ctx, mode));
        }
      }
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (ana_skel(ctx, skel1, seq, HTyp.Hole, monitor)) {
      | None => None
      | Some((ctx, mode1)) =>
        switch (ana_skel(ctx, skel2, seq, HTyp.Hole, monitor)) {
        | None => None
        | Some((ctx, mode2)) =>
          let ty = HTyp.Hole;
          let mode = combine_modes(mode1, mode2);
          Some((ty, ctx, mode));
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq, monitor)) {
      | None => None
      | Some((ty1, ctx, mode1)) =>
        let ty = HTyp.List(ty1);
        switch (ana_skel(ctx, skel2, seq, ty, monitor)) {
        | None => None
        | Some((ctx, mode2)) =>
          let mode = combine_modes(mode1, mode2);
          Some((ty, ctx, mode));
        };
      }
    }
  and syn_operand =
      (ctx: Contexts.t, operand: UHPat.operand)
      : option((HTyp.t, Contexts.t)) =>
    switch (operand) {
    /* in hole */
    | EmptyHole(_) => Some((Hole, ctx))
    | Wild(InHole(TypeInconsistent, _))
    | Var(InHole(TypeInconsistent, _), _, _)
    | NumLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | Inj(InHole(TypeInconsistent, _), _, _) =>
      let operand' = UHPat.set_err_status_operand(NotInHole, operand);
      syn_operand(ctx, operand')
      |> Opt.map(((_, gamma)) => (HTyp.Hole, gamma));
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
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
    | NumLit(NotInHole, _) => Some((Num, ctx))
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
      (ctx: Contexts.t, OpSeq(skel, seq): UHPat.opseq, ty: HTyp.t)
      : option(Contexts.t) =>
    ana_skel(ctx, skel, seq, ty, None) |> Opt.map(((ctx, _)) => ctx)
  and ana_skel =
      (ctx: Contexts.t, skel: UHPat.skel, seq: UHPat.seq, ty: HTyp.t, monitor)
      : option((Contexts.t, option(type_mode))) =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      switch (UHPat.bidelimited(pn)) {
      | false => None
      | true =>
        switch (ana_operand(ctx, pn, ty)) {
        | None => None
        | Some(ctx) =>
          let mode =
            switch (monitor) {
            | None => None
            | Some(n') =>
              if (n == n') {
                Some(AnalyzedAgainst(ty));
              } else {
                None;
              }
            };
          Some((ctx, mode));
        }
      };
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_skel(ctx, skel_not_in_hole, seq, monitor)) {
      | None => None
      | Some((_, ctx, mode)) => Some((ctx, mode))
      };
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (ty) {
      | Hole =>
        switch (ana_skel(ctx, skel1, seq, Hole, monitor)) {
        | None => None
        | Some((ctx, mode1)) =>
          switch (ana_skel(ctx, skel2, seq, Hole, monitor)) {
          | None => None
          | Some((ctx, mode2)) =>
            let mode = combine_modes(mode1, mode2);
            Some((ctx, mode));
          }
        }
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHPat.get_tuple(skel1, skel2);
        switch (ListMinTwo.zip_eq(skels, types)) {
        | None => None
        | Some(zipped) =>
          ListMinTwo.fold_left(
            (opt_result, skel_ty: (UHPat.skel, HTyp.t)) =>
              switch (opt_result) {
              | None => None
              | Some((ctx, mode)) =>
                let (skel, ty) = skel_ty;
                switch (ana_skel(ctx, skel, seq, ty, monitor)) {
                | None => None
                | Some((ctx, mode')) =>
                  let mode = combine_modes(mode, mode');
                  Some((ctx, mode));
                };
              },
            ((skel1, ty1), (skel2, ty2)) =>
              switch (ana_skel(ctx, skel1, seq, ty1, monitor)) {
              | None => None
              | Some((ctx, mode1)) =>
                switch (ana_skel(ctx, skel2, seq, ty2, monitor)) {
                | None => None
                | Some((ctx, mode2)) =>
                  let mode = combine_modes(mode1, mode2);
                  Some((ctx, mode));
                }
              },
            zipped,
          )
        };
      | _ => None
      }
    | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
      switch (ty) {
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHPat.get_tuple(skel1, skel2);
        let n_types = ListMinTwo.length(types);
        let n_skels = ListMinTwo.length(skels);
        n_types == n_skels
          ? None  /* make sure the lengths are actually different */
          : {
            let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
            let ana_zipped: option((Contexts.t, option(type_mode))) =
              ListMinTwo.fold_left(
                (opt_result, skel_ty: (UHPat.skel, HTyp.t)) =>
                  switch (opt_result) {
                  | None => None
                  | Some((ctx, mode)) =>
                    let (skel, ty) = skel_ty;
                    switch (ana_skel(ctx, skel, seq, ty, monitor)) {
                    | None => None
                    | Some((ctx, mode')) =>
                      let mode = combine_modes(mode, mode');
                      Some((ctx, mode));
                    };
                  },
                ((skel1, ty1), (skel2, ty2)) =>
                  switch (ana_skel(ctx, skel1, seq, ty1, monitor)) {
                  | None => None
                  | Some((ctx, mode1)) =>
                    switch (ana_skel(ctx, skel2, seq, ty2, monitor)) {
                    | None => None
                    | Some((ctx, mode2)) =>
                      let mode = combine_modes(mode1, mode2);
                      Some((ctx, mode));
                    }
                  },
                zipped,
              );
            switch (ana_zipped) {
            | None => None
            | Some((ctx, mode)) =>
              List.fold_left(
                (opt_result, skel) =>
                  switch (opt_result) {
                  | None => None
                  | Some((ctx, mode)) =>
                    switch (syn_skel(ctx, skel, seq, monitor)) {
                    | None => None
                    | Some((_, ctx, mode')) =>
                      let mode = combine_modes(mode, mode');
                      Some((ctx, mode));
                    }
                  },
                Some((ctx, mode)),
                remainder,
              )
            };
          };
      | _ => None
      }
    | BinOp(InHole(WrongLength, _), _, _, _) => None
    | BinOp(_, Space, skel1, skel2) =>
      switch (ana_skel(ctx, skel1, seq, HTyp.Hole, monitor)) {
      | None => None
      | Some((ctx, mode1)) =>
        switch (ana_skel(ctx, skel2, seq, HTyp.Hole, monitor)) {
        | None => None
        | Some((ctx, mode2)) =>
          let mode = combine_modes(mode1, mode2);
          Some((ctx, mode));
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        switch (ana_skel(ctx, skel1, seq, ty_elt, monitor)) {
        | None => None
        | Some((ctx, mode1)) =>
          let ty_list = HTyp.List(ty_elt);
          switch (ana_skel(ctx, skel2, seq, ty_list, monitor)) {
          | None => None
          | Some((ctx, mode2)) =>
            let mode = combine_modes(mode1, mode2);
            Some((ctx, mode));
          };
        }
      }
    }
  and ana_operand =
      (ctx: Contexts.t, operand: UHPat.operand, ty: HTyp.t)
      : option(Contexts.t) =>
    switch (operand) {
    /* in hole */
    | EmptyHole(_) => Some(ctx)
    | Wild(InHole(TypeInconsistent, _))
    | Var(InHole(TypeInconsistent, _), _, _)
    | NumLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | Inj(InHole(TypeInconsistent, _), _, _) =>
      let operand' = UHPat.set_err_status_operand(NotInHole, operand);
      syn_operand(ctx, operand') |> Opt.map(((_, ctx)) => ctx);
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => None
    /* not in hole */
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(_), _), _) => Some(ctx)
    | Var(NotInHole, NotInVarHole, x) =>
      Var.check_valid(x, Some(Contexts.extend_gamma(ctx, (x, ty))))
    | Wild(NotInHole) => Some(ctx)
    | NumLit(NotInHole, _)
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
    | BinOp(_, Comma, skel1, skel2) =>
      let (skel1, seq, ty1, ctx, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let (skel2, seq, ty2, ctx, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
      let skel = Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2);
      let ty = HTyp.Prod(ty1, ty2);
      (skel, seq, ty, ctx, u_gen);
    | BinOp(_, Space, skel1, skel2) =>
      let (skel1, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Hole,
        );
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Hole,
        );
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let skel =
        Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Space, skel1, skel2);
      let ty = HTyp.Hole;
      (skel, seq, ty, ctx, u_gen);
    | BinOp(_, Cons, skel1, skel2) =>
      let (skel1, seq, ty_elt, ctx, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let ty = HTyp.List(ty_elt);
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq, ty);
      let skel = Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2);
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
      if (renumber_empty_holes) {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EmptyHole(u), Hole, ctx, u_gen);
      } else {
        (operand, HTyp.Hole, ctx, u_gen);
      }
    | Wild(_) => (operand_nih, Hole, ctx, u_gen)
    | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(_, InVarHole(Keyword(_), _), _) => (operand_nih, Hole, ctx, u_gen)
    | Var(_, NotInVarHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, Hole));
      (operand_nih, Hole, ctx, u_gen);
    | NumLit(_, _) => (operand_nih, Num, ctx, u_gen)
    | BoolLit(_, _) => (operand_nih, Bool, ctx, u_gen)
    | ListNil(_) => (operand_nih, List(Hole), ctx, u_gen)
    | Parenthesized(p) =>
      let (p, ty, ctx, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p);
      (Parenthesized(p), ty, ctx, u_gen);
    | Inj(_, side, p1) =>
      let (p1, ty1, ctx, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p1);
      let p = UHPat.Inj(NotInHole, side, p1);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      (p, ty, ctx, u_gen);
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
        OpSeq(skel, seq): UHPat.opseq,
        ty: HTyp.t,
      )
      : (UHPat.opseq, Contexts.t, MetaVarGen.t) => {
    let (skel, seq, ctx, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq, ty);
    (OpSeq(skel, seq), ctx, u_gen);
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
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      let (pn, ctx, u_gen) =
        ana_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, pn, ty);
      let seq = seq |> Seq.update_nth_operand(n, pn);
      (skel, seq, ctx, u_gen);
    | BinOp(_, Comma, skel1, skel2) =>
      switch (ty) {
      | Hole =>
        let (skel1, seq, ctx, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel1,
            seq,
            HTyp.Hole,
          );
        let (skel2, seq, ctx, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            HTyp.Hole,
          );
        let skel = Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2);
        (skel, seq, ctx, u_gen);
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHPat.get_tuple(skel1, skel2);
        let f =
            (
              (skel, ty): (UHPat.skel, HTyp.t),
              (skels, seq, ctx, u_gen): (
                ListMinTwo.t(UHPat.skel),
                UHPat.seq,
                Contexts.t,
                MetaVarGen.t,
              ),
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
          (ListMinTwo.Cons(skel, skels), seq, ctx, u_gen);
        };
        let f0 =
            (
              (skel1, ty1): (UHPat.skel, HTyp.t),
              (skel2, ty2): (UHPat.skel, HTyp.t),
            ) => {
          let (skel1, seq, ctx, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel1,
              seq,
              ty1,
            );
          let (skel2, seq, ctx, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel2,
              seq,
              ty2,
            );
          (ListMinTwo.Pair(skel1, skel2), seq, ctx, u_gen);
        };
        switch (ListMinTwo.zip_eq(skels, types)) {
        | Some(zipped) =>
          let (skels, seq, ctx, u_gen) =
            ListMinTwo.fold_right(f, zipped, f0);
          let skel = UHPat.make_tuple(NotInHole, skels);
          (skel, seq, ctx, u_gen);
        | None =>
          let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
          let (skels1, seq, ctx, u_gen) =
            ListMinTwo.fold_right(f, zipped, f0);
          let (skels2, seq, ctx, u_gen) =
            List.fold_right(
              (skel: UHPat.skel, (skels, seq, ctx, u_gen)) => {
                let (skel, seq, _, ctx, u_gen) =
                  syn_fix_holes_skel(
                    ctx,
                    u_gen,
                    ~renumber_empty_holes,
                    skel,
                    seq,
                  );
                ([skel, ...skels], seq, ctx, u_gen);
              },
              remainder,
              ([], seq, ctx, u_gen),
            );
          let skels = ListMinTwo.append_list(skels1, skels2);
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let skel = UHPat.make_tuple(InHole(WrongLength, u), skels);
          (skel, seq, ctx, u_gen);
        };
      | _ =>
        let (skel1, seq, _, ctx, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
        let (skel2, seq, _, ctx, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let skel =
          Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Comma, skel1, skel2);
        (skel, seq, ctx, u_gen);
      }
    | BinOp(_, Space, skel1, skel2) =>
      let (skel1, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Hole,
        );
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Hole,
        );
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let skel =
        Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Space, skel1, skel2);
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
        let skel = Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2);
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
          Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Cons, skel1, skel2);
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
    | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(_, InVarHole(Keyword(_), _), _) => (operand_nih, ctx, u_gen)
    | Var(_, NotInVarHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, ty));
      (operand_nih, ctx, u_gen);
    | NumLit(_, _)
    | BoolLit(_, _) =>
      let (operand', ty', ctx, u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, operand);
      if (HTyp.consistent(ty, ty')) {
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
    };
  };

  let syn_fix_holes_z =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t)
      : (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
    let path = CursorPath.Pat.of_z(zp);
    let p = ZPat.erase(zp);
    let (p, ty, ctx, u_gen) = syn_fix_holes(ctx, u_gen, p);
    let zp = CursorPath.Pat.follow_or_fail(path, p);
    (zp, ty, ctx, u_gen);
  };

  let ana_fix_holes_z =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t, ty: HTyp.t)
      : (ZPat.t, Contexts.t, MetaVarGen.t) => {
    let path = CursorPath.Pat.of_z(zp);
    let p = ZPat.erase(zp);
    let (p, ctx, u_gen) = ana_fix_holes(ctx, u_gen, p, ty);
    let zp = CursorPath.Pat.follow_or_fail(path, p);
    (zp, ctx, u_gen);
  };
};

module Exp = {
  let ctx_for_let =
      (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, e: UHExp.t): Contexts.t =>
    switch (p, e) {
    | (
        OpSeq(_, S(Var(_, NotInVarHole, x), E)),
        [ExpLine(OpSeq(_, S(Lam(_, _, _, _), E)))],
      ) =>
      switch (HTyp.matched_arrow(ty)) {
      | Some(_) => Contexts.extend_gamma(ctx, (x, ty))
      | None => ctx
      }
    | _ => ctx
    };

  /* returns recursive ctx + name of recursively defined var */
  let ctx_for_let' =
      (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, block: UHExp.block)
      : (Contexts.t, option(Var.t)) =>
    switch (p, block) {
    | (
        OpSeq(_, S(Var(_, NotInVarHole, x), E)),
        [ExpLine(OpSeq(_, S(Lam(_, _, _, _), E)))],
      ) =>
      switch (HTyp.matched_arrow(ty)) {
      | Some(_) => (Contexts.extend_gamma(ctx, (x, ty)), Some(x))
      | None => (ctx, None)
      }
    | _ => (ctx, None)
    };

  /**
   * Synthesize a type, if possible, for e
   */
  let rec syn = (ctx: Contexts.t, e: UHExp.t): option(HTyp.t) =>
    syn_block(ctx, e)
  and syn_block = (ctx: Contexts.t, block: UHExp.block): option(HTyp.t) =>
    switch (block |> UHExp.split_conclusion) {
    | None => None
    | Some((leading, conclusion)) =>
      switch (syn_lines(ctx, leading)) {
      | None => None
      | Some(ctx) => syn_opseq(ctx, conclusion)
      }
    }
  and syn_lines =
      (ctx: Contexts.t, lines: list(UHExp.line)): option(Contexts.t) => {
    lines
    |> List.fold_left(
         (opt_ctx: option(Contexts.t), line: UHExp.line) =>
           switch (opt_ctx) {
           | None => None
           | Some(ctx) => syn_line(ctx, line)
           },
         Some(ctx),
       );
  }
  and syn_line = (ctx: Contexts.t, line: UHExp.line): option(Contexts.t) =>
    switch (line) {
    | ExpLine(opseq) => syn_opseq(ctx, opseq) |> Opt.map(_ => ctx)
    | EmptyLine => Some(ctx)
    | LetLine(p, ann, def) =>
      switch (ann) {
      | Some(uty) =>
        let ty = UHTyp.expand(uty);
        let ctx_def = ctx_for_let(ctx, p, ty, def);
        switch (ana(ctx_def, def, ty)) {
        | None => None
        | Some(_) => Pat.ana(ctx, p, ty)
        };
      | None =>
        switch (syn(ctx, def)) {
        | None => None
        | Some(ty) => Pat.ana(ctx, p, ty)
        }
      }
    }
  and syn_opseq =
      (ctx: Contexts.t, OpSeq(skel, seq): UHExp.opseq): option(HTyp.t) =>
    syn_skel(ctx, skel, seq, None) |> Opt.map(((ty, _)) => ty)
  and syn_skel =
      (
        ctx: Contexts.t,
        skel: UHExp.skel,
        seq: UHExp.seq,
        monitor: option(int),
      )
      : option((HTyp.t, option(type_mode))) =>
    switch (skel) {
    | Placeholder(n) =>
      let en = Seq.nth_operand(n, seq);
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (syn_operand(ctx, en)) {
        | None => None
        | Some(ty) =>
          let mode =
            switch (monitor) {
            | Some(n') =>
              if (n == n') {
                Some(Synthesized(ty));
              } else {
                None;
              }
            | None => None
            };
          Some((ty, mode));
        }
      };
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2)
    | BinOp(InHole(WrongLength, _), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_skel(ctx, skel_not_in_hole, seq, monitor)) {
      | None => None
      | Some((_, mode)) => Some((Hole, mode))
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => None
    | BinOp(NotInHole, Minus, skel1, skel2)
    | BinOp(NotInHole, Plus, skel1, skel2)
    | BinOp(NotInHole, Times, skel1, skel2) =>
      switch (ana_skel(ctx, skel1, seq, HTyp.Num, monitor)) {
      | None => None
      | Some(mode1) =>
        switch (ana_skel(ctx, skel2, seq, Num, monitor)) {
        | None => None
        | Some(mode2) => Some((Num, combine_modes(mode1, mode2)))
        }
      }
    | BinOp(NotInHole, And | Or, skel1, skel2) =>
      switch (ana_skel(ctx, skel1, seq, HTyp.Bool, monitor)) {
      | None => None
      | Some(mode1) =>
        switch (ana_skel(ctx, skel2, seq, HTyp.Bool, monitor)) {
        | None => None
        | Some(mode2) => Some((Bool, combine_modes(mode1, mode2)))
        }
      }
    | BinOp(NotInHole, LessThan | GreaterThan | Equals, skel1, skel2) =>
      switch (ana_skel(ctx, skel1, seq, Num, monitor)) {
      | None => None
      | Some(mode1) =>
        switch (ana_skel(ctx, skel2, seq, Num, monitor)) {
        | None => None
        | Some(mode2) => Some((Bool, combine_modes(mode1, mode2)))
        }
      }
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq, monitor)) {
      | None => None
      | Some((ty1, mode1)) =>
        switch (HTyp.matched_arrow(ty1)) {
        | None => None
        | Some((ty2, ty)) =>
          switch (ana_skel(ctx, skel2, seq, ty2, monitor)) {
          | None => None
          | Some(mode2) => Some((ty, combine_modes(mode1, mode2)))
          }
        }
      }
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq, monitor)) {
      | None => None
      | Some((ty1, mode1)) =>
        switch (syn_skel(ctx, skel2, seq, monitor)) {
        | None => None
        | Some((ty2, mode2)) =>
          let mode = combine_modes(mode1, mode2);
          let ty = HTyp.Prod(ty1, ty2);
          Some((ty, mode));
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq, monitor)) {
      | None => None
      | Some((ty1, mode1)) =>
        let ty = HTyp.List(ty1);
        switch (ana_skel(ctx, skel2, seq, ty, monitor)) {
        | None => None
        | Some(mode2) => Some((ty, combine_modes(mode1, mode2)))
        };
      }
    }
  and syn_operand = (ctx: Contexts.t, operand: UHExp.operand): option(HTyp.t) =>
    switch (operand) {
    /* in hole */
    | EmptyHole(_) => Some(Hole)
    | Var(InHole(TypeInconsistent, _), _, _)
    | NumLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | Lam(InHole(TypeInconsistent, _), _, _, _)
    | Inj(InHole(TypeInconsistent, _), _, _)
    | Case(InHole(TypeInconsistent, _), _, _, _)
    | ApPalette(InHole(TypeInconsistent, _), _, _, _) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      syn_operand(ctx, operand') |> Opt.map(_ => HTyp.Hole);
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(InHole(WrongLength, _), _, _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _) => None
    /* not in hole */
    | Var(NotInHole, NotInVarHole, x) =>
      VarMap.lookup(Contexts.gamma(ctx), x)
    | Var(NotInHole, InVarHole(_, _), _) => Some(Hole)
    | NumLit(NotInHole, _) => Some(Num)
    | BoolLit(NotInHole, _) => Some(Bool)
    | ListNil(NotInHole) => Some(List(Hole))
    | Lam(NotInHole, p, ann, block) =>
      let ty1 =
        switch (ann) {
        | Some(uty) => UHTyp.expand(uty)
        | None => HTyp.Hole
        };
      switch (Pat.ana(ctx, p, ty1)) {
      | None => None
      | Some(ctx) =>
        switch (syn_block(ctx, block)) {
        | None => None
        | Some(ty2) => Some(HTyp.Arrow(ty1, ty2))
        }
      };
    | Inj(NotInHole, side, block) =>
      switch (syn_block(ctx, block)) {
      | None => None
      | Some(ty) =>
        switch (side) {
        | L => Some(Sum(ty, Hole))
        | R => Some(Sum(Hole, ty))
        }
      }
    | Case(NotInHole, _, _, Some(uty)) => Some(UHTyp.expand(uty))
    | Case(NotInHole, _, _, None) => None
    | ApPalette(NotInHole, name, serialized_model, psi) =>
      let palette_ctx = Contexts.palette_ctx(ctx);
      switch (PaletteCtx.lookup(palette_ctx, name)) {
      | None => None
      | Some(palette_defn) =>
        switch (ana_splice_map(ctx, SpliceInfo.splice_map(psi))) {
        | None => None
        | Some(splice_ctx) =>
          let expansion_ty = palette_defn.expansion_ty;
          let expand = palette_defn.expand;
          let expansion = expand(serialized_model);
          switch (ana_block(splice_ctx, expansion, expansion_ty)) {
          | None => None
          | Some(_) => Some(expansion_ty)
          };
        }
      };
    | Parenthesized(block) => syn_block(ctx, block)
    }
  and ana_splice_map =
      (ctx: Contexts.t, splice_map: UHExp.splice_map): option(Contexts.t) =>
    NatMap.fold(
      splice_map,
      (c, (splice_name, (ty, block))) =>
        switch (c) {
        | None => None
        | Some(splice_ctx) =>
          switch (ana_block(ctx, block, ty)) {
          | None => None
          | Some(_) =>
            let splice_var = SpliceInfo.var_of_splice_name(splice_name);
            Some(Contexts.extend_gamma(splice_ctx, (splice_var, ty)));
          }
        },
      Some(Contexts.empty),
    )
  /**
   * Analyze e against expected type ty
   */
  and ana = (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t): option(unit) =>
    ana_block(ctx, e, ty)
  and ana_block =
      (ctx: Contexts.t, block: UHExp.block, ty: HTyp.t): option(unit) =>
    switch (block |> UHExp.split_conclusion) {
    | None => None
    | Some((leading, conclusion)) =>
      switch (syn_lines(ctx, leading)) {
      | None => None
      | Some(ctx) => ana_opseq(ctx, conclusion, ty)
      }
    }
  and ana_opseq =
      (ctx: Contexts.t, OpSeq(skel, seq): UHExp.opseq, ty: HTyp.t)
      : option(unit) =>
    ana_skel(ctx, skel, seq, ty, None) |> Opt.map(_ => ())
  and ana_skel =
      (
        ctx: Contexts.t,
        skel: UHExp.skel,
        seq: UHExp.seq,
        ty: HTyp.t,
        monitor: option(int),
      )
      : option(option(type_mode)) =>
    switch (skel) {
    | Placeholder(n) =>
      let en = Seq.nth_operand(n, seq);
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (ana_operand(ctx, en, ty)) {
        | None => None
        | Some(_) =>
          switch (monitor) {
          | Some(n') =>
            if (n == n') {
              Some(Some(AnalyzedAgainst(ty)));
            } else {
              Some(None);
            }
          | None => Some(None)
          }
        }
      };
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (ty) {
      | Hole =>
        switch (ana_skel(ctx, skel1, seq, Hole, monitor)) {
        | None => None
        | Some(mode1) =>
          switch (ana_skel(ctx, skel2, seq, Hole, monitor)) {
          | None => None
          | Some(mode2) =>
            let mode = combine_modes(mode1, mode2);
            Some(mode);
          }
        }
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        switch (ListMinTwo.zip_eq(skels, types)) {
        | None => None
        | Some(zipped) =>
          ListMinTwo.fold_left(
            (opt_result, skel_ty: (UHExp.skel, HTyp.t)) =>
              switch (opt_result) {
              | None => None
              | Some(mode) =>
                let (skel, ty) = skel_ty;
                switch (ana_skel(ctx, skel, seq, ty, monitor)) {
                | None => None
                | Some(mode') =>
                  let mode = combine_modes(mode, mode');
                  Some(mode);
                };
              },
            ((skel1, ty1), (skel2, ty2)) =>
              switch (ana_skel(ctx, skel1, seq, ty1, monitor)) {
              | None => None
              | Some(mode1) =>
                switch (ana_skel(ctx, skel2, seq, ty2, monitor)) {
                | None => None
                | Some(mode2) =>
                  let mode = combine_modes(mode1, mode2);
                  Some(mode);
                }
              },
            zipped,
          )
        };
      | _ => None
      }
    | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
      switch (ty) {
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        let n_types = ListMinTwo.length(types);
        let n_skels = ListMinTwo.length(skels);
        n_types == n_skels
          ? None  /* make sure the lengths are actually different */
          : {
            let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
            let ana_zipped: option(option(type_mode)) = (
              ListMinTwo.fold_left(
                (opt_result, skel_ty: (UHExp.skel, HTyp.t)) =>
                  switch (opt_result) {
                  | None => None
                  | Some(mode) =>
                    let (skel, ty) = skel_ty;
                    switch (ana_skel(ctx, skel, seq, ty, monitor)) {
                    | None => None
                    | Some(mode') =>
                      let mode = combine_modes(mode, mode');
                      Some(mode);
                    };
                  },
                ((skel1, ty1), (skel2, ty2)) =>
                  switch (ana_skel(ctx, skel1, seq, ty1, monitor)) {
                  | None => None
                  | Some(mode1) =>
                    switch (ana_skel(ctx, skel2, seq, ty2, monitor)) {
                    | None => None
                    | Some(mode2) =>
                      let mode = combine_modes(mode1, mode2);
                      Some(mode);
                    }
                  },
                zipped,
              ):
                option(option(type_mode))
            );
            switch (ana_zipped) {
            | None => None
            | Some(mode) =>
              List.fold_left(
                (opt_result, skel) =>
                  switch (opt_result) {
                  | None => None
                  | Some(mode) =>
                    switch (syn_skel(ctx, skel, seq, monitor)) {
                    | None => None
                    | Some((_, mode')) =>
                      let mode = combine_modes(mode, mode');
                      Some(mode);
                    }
                  },
                Some(mode),
                remainder,
              )
            };
          };
      | _ => None
      }
    | BinOp(InHole(WrongLength, _), _, _, _) => None
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        switch (ana_skel(ctx, skel1, seq, ty_elt, monitor)) {
        | None => None
        | Some(mode1) =>
          let ty_list = HTyp.List(ty_elt);
          switch (ana_skel(ctx, skel2, seq, ty_list, monitor)) {
          | None => None
          | Some(mode2) => Some(combine_modes(mode1, mode2))
          };
        }
      }
    | BinOp(InHole(TypeInconsistent, _), _, _, _)
    | BinOp(
        NotInHole,
        And | Or | Minus | Plus | Times | LessThan | GreaterThan | Equals |
        Space,
        _,
        _,
      ) =>
      switch (syn_skel(ctx, skel, seq, monitor)) {
      | None => None
      | Some((ty', mode)) =>
        if (HTyp.consistent(ty, ty')) {
          Some(mode);
        } else {
          None;
        }
      }
    }
  and ana_operand =
      (ctx: Contexts.t, operand: UHExp.operand, ty: HTyp.t): option(unit) =>
    switch (operand) {
    /* in hole */
    | EmptyHole(_) => Some()
    | Var(InHole(TypeInconsistent, _), _, _)
    | NumLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | Lam(InHole(TypeInconsistent, _), _, _, _)
    | Inj(InHole(TypeInconsistent, _), _, _)
    | Case(InHole(TypeInconsistent, _), _, _, _)
    | ApPalette(InHole(TypeInconsistent, _), _, _, _) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      switch (syn_operand(ctx, operand')) {
      | None => None
      | Some(_) => Some() /* this is a consequence of subsumption and hole universality */
      };
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(InHole(WrongLength, _), _, _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _) => None
    /* not in hole */
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(_) => Some()
      }
    | Var(NotInHole, _, _)
    | NumLit(NotInHole, _)
    | BoolLit(NotInHole, _) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      switch (syn_operand(ctx, operand')) {
      | None => None
      | Some(ty') =>
        if (HTyp.consistent(ty, ty')) {
          Some();
        } else {
          None;
        }
      };
    | Lam(NotInHole, p, ann, block) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => None
      | Some((ty1_given, ty2)) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1_ann = UHTyp.expand(uty1);
          switch (HTyp.consistent(ty1_ann, ty1_given)) {
          | false => None
          | true =>
            switch (Pat.ana(ctx, p, ty1_ann)) {
            | None => None
            | Some(ctx) => ana_block(ctx, block, ty2)
            }
          };
        | None =>
          switch (Pat.ana(ctx, p, ty1_given)) {
          | None => None
          | Some(ctx) => ana_block(ctx, block, ty2)
          }
        }
      }
    | Inj(NotInHole, side, block) =>
      switch (HTyp.matched_sum(ty)) {
      | None => None
      | Some((ty1, ty2)) =>
        ana_block(ctx, block, InjSide.pick(side, ty1, ty2))
      }
    | Case(NotInHole, block, rules, Some(uty)) =>
      let ty2 = UHTyp.expand(uty);
      if (HTyp.consistent(ty, ty2)) {
        switch (syn_block(ctx, block)) {
        | None => None
        | Some(ty1) => ana_rules(ctx, rules, ty1, ty2)
        };
      } else {
        None;
      };
    | Case(NotInHole, block, rules, None) =>
      switch (syn_block(ctx, block)) {
      | None => None
      | Some(ty1) => ana_rules(ctx, rules, ty1, ty)
      }
    | ApPalette(NotInHole, _, _, _) =>
      switch (syn_operand(ctx, operand)) {
      | None => None
      | Some(ty') =>
        if (HTyp.consistent(ty, ty')) {
          Some();
        } else {
          None;
        }
      }
    | Parenthesized(block) => ana_block(ctx, block, ty)
    }
  and ana_rules =
      (ctx: Contexts.t, rules: UHExp.rules, pat_ty: HTyp.t, clause_ty: HTyp.t)
      : option(unit) =>
    List.fold_left(
      (b, r) =>
        switch (b) {
        | None => None
        | Some(_) => ana_rule(ctx, r, pat_ty, clause_ty)
        },
      Some(),
      rules,
    )
  and ana_rule =
      (ctx: Contexts.t, rule: UHExp.rule, pat_ty: HTyp.t, clause_ty: HTyp.t)
      : option(unit) => {
    let Rule(p, block) = rule;
    switch (Pat.ana(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) => ana_block(ctx, block, clause_ty)
    };
  };

  /* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
   * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
   * regardless.
   */
  let rec syn_fix_holes =
          (
            ctx: Contexts.t,
            u_gen: MetaVarGen.t,
            ~renumber_empty_holes=false,
            e: UHExp.t,
          )
          : (UHExp.t, HTyp.t, MetaVarGen.t) =>
    syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, e)
  and syn_fix_holes_block =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        block: UHExp.block,
      )
      : (UHExp.block, HTyp.t, MetaVarGen.t) =>
    switch (block |> UHExp.split_conclusion) {
    | None =>
      let (leading, _ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, block);
      let (conclusion, u_gen) = u_gen |> UHExp.new_EmptyHole;
      (leading @ [UHExp.ExpLine(conclusion |> OpSeq.wrap)], Hole, u_gen);
    | Some((leading, conclusion)) =>
      let (leading, ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, leading);
      let (conclusion, ty, u_gen) =
        syn_fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, conclusion);
      (leading @ [UHExp.ExpLine(conclusion)], ty, u_gen);
    }
  and syn_fix_holes_lines =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        lines: list(UHExp.line),
      )
      : (list(UHExp.line), Contexts.t, MetaVarGen.t) => {
    let (rev_fixed_lines, ctx, u_gen) =
      lines
      |> List.fold_left(
           (
             (fixed_lines, ctx, u_gen): (
               list(UHExp.line),
               Contexts.t,
               MetaVarGen.t,
             ),
             line: UHExp.line,
           ) => {
             let (fixed_line, ctx, u_gen) =
               syn_fix_holes_line(ctx, u_gen, ~renumber_empty_holes, line);
             ([fixed_line, ...fixed_lines], ctx, u_gen);
           },
           ([], ctx, u_gen),
         );
    (rev_fixed_lines |> List.rev, ctx, u_gen);
  }
  and syn_fix_holes_line =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        line: UHExp.line,
      )
      : (UHExp.line, Contexts.t, MetaVarGen.t) =>
    switch (line) {
    | ExpLine(e) =>
      let (e, _, u_gen) =
        syn_fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, e);
      (ExpLine(e), ctx, u_gen);
    | EmptyLine => (line, ctx, u_gen)
    | LetLine(p, ann, def) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let ctx_def = ctx_for_let(ctx, p, ty1, def);
        let (def, u_gen) =
          ana_fix_holes(ctx_def, u_gen, ~renumber_empty_holes, def, ty1);
        let (p, ctx, u_gen) =
          Pat.ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p, ty1);
        (LetLine(p, ann, def), ctx, u_gen);
      | None =>
        let (def, ty1, u_gen) =
          syn_fix_holes(~renumber_empty_holes, ctx, u_gen, def);
        let (p, ctx, u_gen) =
          Pat.ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p, ty1);
        (LetLine(p, ann, def), ctx, u_gen);
      }
    }
  and syn_fix_holes_opseq =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        OpSeq(skel, seq): UHExp.opseq,
      )
      : (UHExp.opseq, HTyp.t, MetaVarGen.t) => {
    let (skel, seq, ty, u_gen) =
      syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
    (OpSeq(skel, seq), ty, u_gen);
  }
  and syn_fix_holes_skel =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        skel: UHExp.skel,
        seq: UHExp.seq,
      )
      : (UHExp.skel, UHExp.seq, HTyp.t, MetaVarGen.t) =>
    switch (skel) {
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      let (en, ty, u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, en);
      let seq = seq |> Seq.update_nth_operand(n, en);
      (skel, seq, ty, u_gen);
    | BinOp(_, (Minus | Plus | Times) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Num,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Num,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Num, u_gen);
    | BinOp(_, (And | Or) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Bool,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Bool,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Bool, u_gen);
    | BinOp(_, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Num,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Num,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Bool, u_gen);
    | BinOp(_, Space, skel1, skel2) =>
      let (skel1, seq, ty1, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      switch (HTyp.matched_arrow(ty1)) {
      | Some((ty2, ty)) =>
        let (skel2, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            ty2,
          );
        (BinOp(NotInHole, Space, skel1, skel2), seq, ty, u_gen);
      | None =>
        let (skel2, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            HTyp.Hole,
          );
        let (OpSeq(skel1, seq), u_gen) =
          UHExp.make_inconsistent_opseq(u_gen, OpSeq(skel1, seq));
        (BinOp(NotInHole, Space, skel1, skel2), seq, Hole, u_gen);
      };
    | BinOp(_, Comma, skel1, skel2) =>
      let (skel1, seq, ty1, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let (skel2, seq, ty2, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
      let skel = Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2);
      let ty = HTyp.Prod(ty1, ty2);
      (skel, seq, ty, u_gen);
    | BinOp(_, Cons, skel1, skel2) =>
      let (skel1, seq, ty_elt, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let ty = HTyp.List(ty_elt);
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq, ty);
      let skel = Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2);
      (skel, seq, ty, u_gen);
    }
  and syn_fix_holes_operand =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        e: UHExp.operand,
      )
      : (UHExp.operand, HTyp.t, MetaVarGen.t) => {
    let e_nih = UHExp.set_err_status_operand(NotInHole, e);
    switch (e) {
    | EmptyHole(_) =>
      if (renumber_empty_holes) {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EmptyHole(u), Hole, u_gen);
      } else {
        (e, Hole, u_gen);
      }
    | Var(_, var_err_status, x) =>
      let gamma = Contexts.gamma(ctx);
      switch (VarMap.lookup(gamma, x)) {
      | Some(ty) => (UHExp.Var(NotInHole, NotInVarHole, x), ty, u_gen)
      | None =>
        switch (var_err_status) {
        | InVarHole(_, _) => (e_nih, HTyp.Hole, u_gen)
        | NotInVarHole =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let reason: VarErrStatus.HoleReason.t =
            switch (Var.is_let(x), Var.is_case(x)) {
            | (true, _) => Keyword(Let)
            | (_, true) => Keyword(Case)
            | _ => Free
            };
          (Var(NotInHole, InVarHole(reason, u), x), Hole, u_gen);
        }
      };
    | NumLit(_, _) => (e_nih, Num, u_gen)
    | BoolLit(_, _) => (e_nih, Bool, u_gen)
    | ListNil(_) => (e_nih, List(Hole), u_gen)
    | Parenthesized(block) =>
      let (block, ty, u_gen) =
        syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
      (Parenthesized(block), ty, u_gen);
    | Lam(_, p, ann, block) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      let (p, ctx, u_gen) =
        Pat.ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p, ty1);
      let (block, ty2, u_gen) =
        syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
      (Lam(NotInHole, p, ann, block), Arrow(ty1, ty2), u_gen);
    | Inj(_, side, block) =>
      let (block, ty1, u_gen) =
        syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      (Inj(NotInHole, side, block), ty, u_gen);
    | Case(_, block, rules, Some(uty)) =>
      let ty = UHTyp.expand(uty);
      let (block, ty1, u_gen) =
        syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
      let (rules, u_gen) =
        ana_fix_holes_rules(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          rules,
          ty1,
          ty,
        );
      (Case(NotInHole, block, rules, Some(uty)), ty, u_gen);
    | Case(_, block, rules, None) =>
      let (block, ty1, u_gen) =
        syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
      let (rules, u_gen) =
        ana_fix_holes_rules(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          rules,
          ty1,
          HTyp.Hole,
        );
      (
        Case(NotInHole, block, rules, Some(UHTyp.Hole |> OpSeq.wrap)),
        HTyp.Hole,
        u_gen,
      );
    | ApPalette(_, name, serialized_model, psi) =>
      let palette_ctx = Contexts.palette_ctx(ctx);
      switch (PaletteCtx.lookup(palette_ctx, name)) {
      | None => raise(PaletteCtx.InvalidPaletteHoleName) /* TODO invalid palette name hole */
      | Some(palette_defn) =>
        let (splice_map, u_gen) =
          ana_fix_holes_splice_map(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            SpliceInfo.splice_map(psi),
          );
        let psi = SpliceInfo.update_splice_map(psi, splice_map);
        let expansion_ty = palette_defn.expansion_ty;
        (
          ApPalette(NotInHole, name, serialized_model, psi),
          expansion_ty,
          u_gen,
        );
      };
    };
  }
  and ana_fix_holes_rules =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        rules: UHExp.rules,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : (UHExp.rules, MetaVarGen.t) => {
    let (rev_fixed_rules, u_gen) =
      List.fold_left(
        ((rules, u_gen), r) => {
          let (r, u_gen) =
            ana_fix_holes_rule(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              r,
              pat_ty,
              clause_ty,
            );
          ([r, ...rules], u_gen);
        },
        ([], u_gen),
        rules,
      );
    (List.rev(rev_fixed_rules), u_gen);
  }
  and ana_fix_holes_rule =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        rule: UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : (UHExp.rule, MetaVarGen.t) => {
    let Rule(p, block) = rule;
    let (p, ctx, u_gen) =
      Pat.ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p, pat_ty);
    let (block, u_gen) =
      ana_fix_holes_block(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        block,
        clause_ty,
      );
    (Rule(p, block), u_gen);
  }
  and ana_fix_holes_splice_map =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        splice_map: UHExp.splice_map,
      )
      : (UHExp.splice_map, MetaVarGen.t) =>
    NatMap.fold(
      splice_map,
      ((splice_map, u_gen), (splice_name, (ty, block))) => {
        let (block, u_gen) =
          ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block, ty);
        let splice_map =
          NatMap.extend_unique(splice_map, (splice_name, (ty, block)));
        (splice_map, u_gen);
      },
      (splice_map, u_gen),
    )
  and ana_fix_holes =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        e: UHExp.t,
        ty: HTyp.t,
      )
      : (UHExp.t, MetaVarGen.t) =>
    ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, e, ty)
  and ana_fix_holes_block =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        block: UHExp.block,
        ty: HTyp.t,
      )
      : (UHExp.block, MetaVarGen.t) =>
    switch (block |> UHExp.split_conclusion) {
    | None =>
      let (leading, _ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, block);
      let (conclusion, u_gen) = u_gen |> UHExp.new_EmptyHole;
      (leading @ [UHExp.ExpLine(conclusion |> OpSeq.wrap)], u_gen);
    | Some((leading, conclusion)) =>
      let (leading, ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, leading);
      let (conclusion, u_gen) =
        ana_fix_holes_opseq(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          conclusion,
          ty,
        );
      (leading @ [UHExp.ExpLine(conclusion)], u_gen);
    }
  and ana_fix_holes_opseq =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        OpSeq(skel, seq): UHExp.opseq,
        ty: HTyp.t,
      )
      : (UHExp.opseq, MetaVarGen.t) => {
    let (skel, seq, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq, ty);
    (OpSeq(skel, seq), u_gen);
  }
  and ana_fix_holes_operand =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        e: UHExp.operand,
        ty: HTyp.t,
      )
      : (UHExp.operand, MetaVarGen.t) =>
    switch (e) {
    | EmptyHole(_) =>
      if (renumber_empty_holes) {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EmptyHole(u), u_gen);
      } else {
        (e, u_gen);
      }
    | Var(_, _, _)
    | NumLit(_, _)
    | BoolLit(_, _) =>
      let (e, ty', u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
      if (HTyp.consistent(ty, ty')) {
        (UHExp.set_err_status_operand(NotInHole, e), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e),
          u_gen,
        );
      };
    | ListNil(_) =>
      switch (HTyp.matched_list(ty)) {
      | Some(_) => (UHExp.set_err_status_operand(NotInHole, e), u_gen)
      | None =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (ListNil(InHole(TypeInconsistent, u)), u_gen);
      }
    | Parenthesized(body) =>
      let (body, u_gen) =
        ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, body, ty);
      (Parenthesized(body), u_gen);
    | Lam(_, p, ann, def) =>
      switch (HTyp.matched_arrow(ty)) {
      | Some((ty1_given, ty2)) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1_ann = UHTyp.expand(uty1);
          if (HTyp.consistent(ty1_ann, ty1_given)) {
            let (p, ctx, u_gen) =
              Pat.ana_fix_holes(
                ctx,
                u_gen,
                ~renumber_empty_holes,
                p,
                ty1_ann,
              );
            let (def, u_gen) =
              ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, def, ty2);
            (UHExp.Lam(NotInHole, p, ann, def), u_gen);
          } else {
            let (e', _, u_gen) =
              syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
            let (u, u_gen) = MetaVarGen.next(u_gen);
            (
              UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
              u_gen,
            );
          };
        | None =>
          let (p, ctx, u_gen) =
            Pat.ana_fix_holes(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              p,
              ty1_given,
            );
          let (def, u_gen) =
            ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, def, ty2);
          (UHExp.Lam(NotInHole, p, ann, def), u_gen);
        }
      | None =>
        let (e', _, u_gen) =
          syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
          u_gen,
        );
      }
    | Inj(_, side, body) =>
      switch (HTyp.matched_sum(ty)) {
      | Some((ty1, ty2)) =>
        let (e1, u_gen) =
          ana_fix_holes(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            body,
            InjSide.pick(side, ty1, ty2),
          );
        (Inj(NotInHole, side, e1), u_gen);
      | None =>
        let (e', ty', u_gen) =
          syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
        if (HTyp.consistent(ty, ty')) {
          (UHExp.set_err_status_operand(NotInHole, e'), u_gen);
        } else {
          let (u, u_gen) = MetaVarGen.next(u_gen);
          (
            UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
            u_gen,
          );
        };
      }
    | Case(_, block, rules, Some(uty)) =>
      let ty2 = UHTyp.expand(uty);
      if (HTyp.consistent(ty, ty2)) {
        let (block, ty1, u_gen) =
          syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
        let (rules, u_gen) =
          ana_fix_holes_rules(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            rules,
            ty1,
            ty,
          );
        (
          Case(
            NotInHole,
            block,
            rules,
            switch (ty2) {
            | Hole => None
            | _ => Some(uty)
            },
          ),
          u_gen,
        );
      } else {
        let (e', _, u_gen) =
          syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
          u_gen,
        );
      };
    | Case(_, block, rules, None) =>
      let (e1, ty1, u_gen) =
        syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
      let (rules, u_gen) =
        ana_fix_holes_rules(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          rules,
          ty1,
          ty,
        );
      (Case(NotInHole, e1, rules, None), u_gen);
    | ApPalette(_, _, _, _) =>
      let (e', ty', u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
      if (HTyp.consistent(ty, ty')) {
        (UHExp.set_err_status_operand(NotInHole, e'), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
          u_gen,
        );
      };
    }
  and ana_fix_holes_skel =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        skel: UHExp.skel,
        seq: UHExp.seq,
        ty: HTyp.t,
      )
      : (UHExp.skel, UHExp.seq, MetaVarGen.t) =>
    switch (skel) {
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      let (en, u_gen) =
        ana_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, en, ty);
      let seq = seq |> Seq.update_nth_operand(n, en);
      (skel, seq, u_gen);
    | BinOp(_, Comma, skel1, skel2) =>
      switch (ty) {
      | Hole =>
        let (skel1, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel1,
            seq,
            HTyp.Hole,
          );
        let (skel2, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            HTyp.Hole,
          );
        let skel = Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2);
        (skel, seq, u_gen);
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        let f =
            (
              (skel, ty): (UHExp.skel, HTyp.t),
              (skels, seq, u_gen): (
                ListMinTwo.t(UHExp.skel),
                UHExp.seq,
                MetaVarGen.t,
              ),
            ) => {
          let (skel, seq, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel,
              seq,
              ty,
            );
          (ListMinTwo.Cons(skel, skels), seq, u_gen);
        };
        let f0 =
            (
              (skel1, ty1): (UHExp.skel, HTyp.t),
              (skel2, ty2): (UHExp.skel, HTyp.t),
            ) => {
          let (skel1, seq, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel1,
              seq,
              ty1,
            );
          let (skel2, seq, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel2,
              seq,
              ty2,
            );
          (ListMinTwo.Pair(skel1, skel2), seq, u_gen);
        };
        switch (ListMinTwo.zip_eq(skels, types)) {
        | Some(zipped) =>
          let (skels, seq, u_gen) = ListMinTwo.fold_right(f, zipped, f0);
          let skel = UHExp.make_tuple(NotInHole, skels);
          (skel, seq, u_gen);
        | None =>
          let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
          let (skels1, seq, u_gen) = ListMinTwo.fold_right(f, zipped, f0);
          let (skels2, seq, u_gen) =
            List.fold_right(
              (skel: UHExp.skel, (skels, seq, u_gen)) => {
                let (skel, seq, _, u_gen) =
                  syn_fix_holes_skel(
                    ctx,
                    u_gen,
                    ~renumber_empty_holes,
                    skel,
                    seq,
                  );
                ([skel, ...skels], seq, u_gen);
              },
              remainder,
              ([], seq, u_gen),
            );
          let skels = ListMinTwo.append_list(skels1, skels2);
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let skel = UHExp.make_tuple(InHole(WrongLength, u), skels);
          (skel, seq, u_gen);
        };
      | _ =>
        let (skel1, seq, _, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
        let (skel2, seq, _, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let skel =
          Skel.BinOp(InHole(TypeInconsistent, u), UHExp.Comma, skel1, skel2);
        (skel, seq, u_gen);
      }
    | BinOp(_, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | Some(ty_elt) =>
        let (skel1, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel1,
            seq,
            ty_elt,
          );
        let ty_list = HTyp.List(ty_elt);
        let (skel2, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          );
        let skel = Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2);
        (skel, seq, u_gen);
      | None =>
        let (skel1, seq, ty_elt, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
        let ty_list = HTyp.List(ty_elt);
        let (skel2, seq, u_gen) =
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
          Skel.BinOp(InHole(TypeInconsistent, u), UHExp.Cons, skel1, skel2);
        (skel, seq, u_gen);
      }
    | BinOp(
        _,
        And | Or | Minus | Plus | Times | LessThan | GreaterThan | Equals |
        Space,
        _,
        _,
      ) =>
      let (skel, seq, ty', u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
      if (HTyp.consistent(ty, ty')) {
        (skel, seq, u_gen);
      } else {
        let (OpSeq(skel, seq), u_gen) =
          UHExp.make_inconsistent_opseq(u_gen, OpSeq(skel, seq));
        (skel, seq, u_gen);
      };
    };

  let syn_fix_holes_zblock =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zblock: ZExp.zblock)
      : (ZExp.zblock, HTyp.t, MetaVarGen.t) => {
    let path = CursorPath.Exp.of_zblock(zblock);
    let block = ZExp.erase_zblock(zblock);
    let (block, ty, u_gen) = syn_fix_holes_block(ctx, u_gen, block);
    let zblock = CursorPath.Exp.follow_or_fail(path, block);
    (zblock, ty, u_gen);
  };

  /*
   let syn_fix_holes_zlines =
       (ctx: Contexts.t, u_gen: MetaVarGen.t, zlines: ZExp.zlines)
       : (ZExp.zlines, Contexts.t, MetaVarGen.t) => {
     let path = CursorPath.of_zlines(zlines);
     let lines = ZExp.erase_zlines(zlines);
     let (lines, ctx, u_gen) = syn_fix_holes_lines(ctx, u_gen, lines);
     let zlines = CursorPath.follow_lines_or_fail(path, lines);
     (zlines, ctx, u_gen);
   };
   */

  let syn_fix_holes_zoperand =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zoperand: ZExp.zoperand)
      : (ZExp.zoperand, HTyp.t, MetaVarGen.t) => {
    let path = CursorPath.Exp.of_zoperand(zoperand);
    let operand = ZExp.erase_zoperand(zoperand);
    let (operand, ty, u_gen) = syn_fix_holes_operand(ctx, u_gen, operand);
    let zoperand = CursorPath.Exp.follow_operand_or_fail(path, operand);
    (zoperand, ty, u_gen);
  };

  let ana_fix_holes_zblock =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zblock: ZExp.zblock, ty: HTyp.t)
      : (ZExp.zblock, MetaVarGen.t) => {
    let (steps, _) as path = CursorPath.Exp.of_zblock(zblock);
    let block = ZExp.erase_zblock(zblock);
    let (block, u_gen) = ana_fix_holes_block(ctx, u_gen, block, ty);
    switch (CursorPath.Exp.follow_block(path, block)) {
    | None =>
      // Only way this can happen now is path was originally
      // on case type annotation and ana_fix_holes stripped
      // the annotation, in which case we can just place cursor
      // at end of case node. We might just wanna write a proper
      // recursive traversal for hole-fixing zexps/blocks.
      switch (steps |> split_last) {
      | None => assert(false)
      | Some((case_steps, _)) =>
        switch (CursorPath.Exp.follow_steps(~side=After, case_steps, block)) {
        | None => assert(false)
        | Some(zblock) => (zblock, u_gen)
        }
      }
    | Some(zblock) => (zblock, u_gen)
    };
  };

  let ana_fix_holes_zoperand =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        zoperand: ZExp.zoperand,
        ty: HTyp.t,
      )
      : (ZExp.zoperand, MetaVarGen.t) => {
    let (steps, _) as path = CursorPath.Exp.of_zoperand(zoperand);
    let operand = ZExp.erase_zoperand(zoperand);
    let (operand, u_gen) = ana_fix_holes_operand(ctx, u_gen, operand, ty);
    switch (CursorPath.Exp.follow_operand(path, operand)) {
    | None =>
      // Only way this can happen now is path was originally
      // on case type annotation and ana_fix_holes stripped
      // the annotation, in which case we can just place cursor
      // at end of case node. We might just wanna write a proper
      // recursive traversal for hole-fixing zexps/blocks.
      switch (steps |> split_last) {
      | None => assert(false)
      | Some((case_steps, _)) =>
        switch (
          CursorPath.Exp.follow_steps_operand(
            ~side=After,
            case_steps,
            operand,
          )
        ) {
        | None => assert(false)
        | Some(zoperand) => (zoperand, u_gen)
        }
      }
    | Some(zoperand) => (zoperand, u_gen)
    };
  };

  /* Only to be used on top-level expressions, as it starts hole renumbering at 0 */
  let fix_and_renumber_holes =
      (ctx: Contexts.t, block: UHExp.block)
      : (UHExp.block, HTyp.t, MetaVarGen.t) =>
    syn_fix_holes_block(
      ctx,
      MetaVarGen.init,
      ~renumber_empty_holes=true,
      block,
    );

  let fix_and_renumber_holes_z = (ctx: Contexts.t, ze: ZExp.t): edit_state => {
    let (e, ty, u_gen) = fix_and_renumber_holes(ctx, ze |> ZExp.erase);
    let ze = CursorPath.Exp.follow_or_fail(CursorPath.Exp.of_z(ze), e);
    (ze, ty, u_gen);
  };
};
