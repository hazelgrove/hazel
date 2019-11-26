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

let tpat_wf = (ctx: Contexts.t, tpat: TPat.t): Contexts.t =>
  switch (tpat) {
  | TPat.Var(t) => Contexts.extend_tvars(ctx, t)
  | TPat.Hole(_) => ctx
  };

/*! finish -- put this literally everewhere. like literally after each character. Maybe more. */
/* is type well formed? */
let rec type_wf = (ctx: Contexts.t, t: HTyp.t) =>
  switch (t) {
  | TVar(idx, _) => List.length(ctx.tvars) > idx
  | TVarHole(_, _)
  | Num
  | Unit
  | Bool
  | Hole => true
  | Arrow(a, b)
  | Sum(a, b)
  | Prod(a, b) => type_wf(ctx, a) && type_wf(ctx, b)
  | List(a) => type_wf(ctx, a)
  | Forall(tp, a) =>
    let ctx = tpat_wf(ctx, tp);
    type_wf(ctx, a);
  };

let rec syn_pat =
        (ctx: Contexts.t, p: UHPat.t): option((HTyp.t, Contexts.t)) =>
  switch (p) {
  /* in hole */
  | EmptyHole(_) => Some((Hole, ctx))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let p' = UHPat.set_err_status_t(NotInHole, p);
    switch (syn_pat(ctx, p')) {
    | None => None
    | Some((_, gamma)) => Some((Hole, gamma))
    };
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
      Some((HTyp.Hole, Contexts.extend_vars(ctx, (x, Hole)))),
    )
  | NumLit(NotInHole, _) => Some((Num, ctx))
  | BoolLit(NotInHole, _) => Some((Bool, ctx))
  | ListNil(NotInHole) => Some((List(Hole), ctx))
  | Inj(NotInHole, inj_side, p1) =>
    switch (syn_pat(ctx, p1)) {
    | None => None
    | Some((ty1, ctx)) =>
      let ty =
        switch (inj_side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Some((ty, ctx));
    }
  | Parenthesized(p) => syn_pat(ctx, p)
  | OpSeq(skel, seq) =>
    switch (syn_skel_pat(ctx, skel, seq, None)) {
    | None => None
    | Some((ty, ctx, _)) => Some((ty, ctx))
    }
  }
and syn_skel_pat =
    (ctx: Contexts.t, skel: UHPat.skel_t, seq: UHPat.opseq, monitor) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => None
    | Some(pn) =>
      switch (UHPat.bidelimited(pn)) {
      | false => None
      | true =>
        switch (syn_pat(ctx, pn)) {
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
      }
    }
  | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2)
  | BinOp(InHole(WrongLength, _), Comma as op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel_pat(ctx, skel_not_in_hole, seq, monitor)) {
    | None => None
    | Some((_, ctx, mode)) => Some((HTyp.Hole, ctx, mode))
    };
  | BinOp(InHole(WrongLength, _), _, _, _) => None
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (syn_skel_pat(ctx, skel1, seq, monitor)) {
    | None => None
    | Some((ty1, ctx, mode1)) =>
      switch (syn_skel_pat(ctx, skel2, seq, monitor)) {
      | None => None
      | Some((ty2, ctx, mode2)) =>
        let ty = HTyp.Prod(ty1, ty2);
        let mode = combine_modes(mode1, mode2);
        Some((ty, ctx, mode));
      }
    }
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (ana_skel_pat(ctx, skel1, seq, HTyp.Hole, monitor)) {
    | None => None
    | Some((ctx, mode1)) =>
      switch (ana_skel_pat(ctx, skel2, seq, HTyp.Hole, monitor)) {
      | None => None
      | Some((ctx, mode2)) =>
        let ty = HTyp.Hole;
        let mode = combine_modes(mode1, mode2);
        Some((ty, ctx, mode));
      }
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (syn_skel_pat(ctx, skel1, seq, monitor)) {
    | None => None
    | Some((ty1, ctx, mode1)) =>
      let ty = HTyp.List(ty1);
      switch (ana_skel_pat(ctx, skel2, seq, ty, monitor)) {
      | None => None
      | Some((ctx, mode2)) =>
        let mode = combine_modes(mode1, mode2);
        Some((ty, ctx, mode));
      };
    }
  }
and ana_pat = (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t): option(Contexts.t) =>
  switch (p) {
  /* in hole */
  | EmptyHole(_) => Some(ctx)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) =>
    let p' = UHPat.set_err_status_t(NotInHole, p);
    switch (syn_pat(ctx, p')) {
    | None => None
    | Some((_, ctx)) => Some(ctx)
    };
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
    Var.check_valid(x, Some(Contexts.extend_vars(ctx, (x, ty))))
  | Wild(NotInHole) => Some(ctx)
  | NumLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    switch (syn_pat(ctx, p)) {
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
      ana_pat(ctx, p1, ty1);
    }
  | OpSeq(skel, seq) =>
    switch (ana_skel_pat(ctx, skel, seq, ty, None)) {
    | Some((ctx, _)) => Some(ctx)
    | None => None
    }
  | Parenthesized(p) => ana_pat(ctx, p, ty)
  }
and ana_skel_pat =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      ty: HTyp.t,
      monitor,
    )
    : option((Contexts.t, option(type_mode))) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => None
    | Some(pn) =>
      switch (UHPat.bidelimited(pn)) {
      | false => None
      | true =>
        switch (ana_pat(ctx, pn, ty)) {
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
      }
    }
  | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel_pat(ctx, skel_not_in_hole, seq, monitor)) {
    | None => None
    | Some((_, ctx, mode)) => Some((ctx, mode))
    };
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (ana_skel_pat(ctx, skel1, seq, Hole, monitor)) {
      | None => None
      | Some((ctx, mode1)) =>
        switch (ana_skel_pat(ctx, skel2, seq, Hole, monitor)) {
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
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | None => None
            | Some((ctx, mode)) =>
              let (skel, ty) = skel_ty;
              switch (ana_skel_pat(ctx, skel, seq, ty, monitor)) {
              | None => None
              | Some((ctx, mode')) =>
                let mode = combine_modes(mode, mode');
                Some((ctx, mode));
              };
            },
          ((skel1, ty1), (skel2, ty2)) =>
            switch (ana_skel_pat(ctx, skel1, seq, ty1, monitor)) {
            | None => None
            | Some((ctx, mode1)) =>
              switch (ana_skel_pat(ctx, skel2, seq, ty2, monitor)) {
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
              (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
                switch (opt_result) {
                | None => None
                | Some((ctx, mode)) =>
                  let (skel, ty) = skel_ty;
                  switch (ana_skel_pat(ctx, skel, seq, ty, monitor)) {
                  | None => None
                  | Some((ctx, mode')) =>
                    let mode = combine_modes(mode, mode');
                    Some((ctx, mode));
                  };
                },
              ((skel1, ty1), (skel2, ty2)) =>
                switch (ana_skel_pat(ctx, skel1, seq, ty1, monitor)) {
                | None => None
                | Some((ctx, mode1)) =>
                  switch (ana_skel_pat(ctx, skel2, seq, ty2, monitor)) {
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
                  switch (syn_skel_pat(ctx, skel, seq, monitor)) {
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
    switch (ana_skel_pat(ctx, skel1, seq, HTyp.Hole, monitor)) {
    | None => None
    | Some((ctx, mode1)) =>
      switch (ana_skel_pat(ctx, skel2, seq, HTyp.Hole, monitor)) {
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
      switch (ana_skel_pat(ctx, skel1, seq, ty_elt, monitor)) {
      | None => None
      | Some((ctx, mode1)) =>
        let ty_list = HTyp.List(ty_elt);
        switch (ana_skel_pat(ctx, skel2, seq, ty_list, monitor)) {
        | None => None
        | Some((ctx, mode2)) =>
          let mode = combine_modes(mode1, mode2);
          Some((ctx, mode));
        };
      }
    }
  };

let ctx_for_let =
    (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, block: UHExp.block): Contexts.t =>
  switch (p, block) {
  | (Var(_, NotInVarHole, x), Block([], Lam(_, _, _, _))) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some(_) => Contexts.extend_vars(ctx, (x, ty))
    | None => ctx
    }
  | _ => ctx
  };

/* returns recursive ctx + name of recursively defined var */
let ctx_for_let' =
    (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, block: UHExp.block)
    : (Contexts.t, option(Var.t)) =>
  switch (p, block) {
  | (Var(_, NotInVarHole, x), Block([], Lam(_, _, _, _))) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some(_) => (Contexts.extend_vars(ctx, (x, ty)), Some(x))
    | None => (ctx, None)
    }
  | _ => (ctx, None)
  };

let fix_holes_tpat = (ctx: Contexts.t, tpat: TPat.t): Contexts.t =>
  switch (tpat) {
  | Hole(_) => ctx
  | Var(t) => Contexts.extend_tvars(ctx, t)
  };

let rec fix_holes_ty_skel =
        (
          ctx: Contexts.t,
          skel: UHTyp.skel_t,
          seq: UHTyp.opseq,
          u_gen: MetaVarGen.t,
        )
        : (UHTyp.skel_t, UHTyp.opseq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHTyp.SkelInconsistentWithOpSeq(skel, seq))
    | Some(ty) =>
      let (ty, u_gen) = fix_holes_ty(ctx, ty, u_gen);
      switch (OperatorSeq.seq_update_nth(n, seq, ty)) {
      | None => raise(UHTyp.SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, u_gen)
      };
    }
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, u_gen) = fix_holes_ty_skel(ctx, skel1, seq, u_gen);
    let (skel2, seq, u_gen) = fix_holes_ty_skel(ctx, skel2, seq, u_gen);
    (BinOp(NotInHole, op, skel1, skel2), seq, u_gen);
  }

and fix_holes_ty =
    (ctx: Contexts.t, ty: UHTyp.t, u_gen: MetaVarGen.t)
    : (UHTyp.t, MetaVarGen.t) =>
  switch (ty) {
  | TVar(InVarHole(Free, u), t) =>
    if (TVarCtx.includes(ctx.tvars, t)) {
      (TVar(NotInVarHole, t), u_gen);
    } else {
      (TVar(InVarHole(Free, u), t), u_gen);
    }
  | TVar(_, t) =>
    if (TVarCtx.includes(ctx.tvars, t)) {
      (TVar(NotInVarHole, t), u_gen);
    } else {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (TVar(InVarHole(Free, u), t), u_gen);
    }
  | Hole
  | Num
  | Bool
  | Unit => (ty, u_gen)
  | List(ty) => fix_holes_ty(ctx, ty, u_gen)
  | OpSeq(skel, seq) =>
    let (skel, seq, u_gen) = fix_holes_ty_skel(ctx, skel, seq, u_gen);
    (OpSeq(skel, seq), u_gen);
  | Parenthesized(ty) =>
    let (ty, u_gen) = fix_holes_ty(ctx, ty, u_gen);
    (Parenthesized(ty), u_gen);
  | Forall(tpat, ty) =>
    let ctx = fix_holes_tpat(ctx, tpat);
    let (ty, u_gen) = fix_holes_ty(ctx, ty, u_gen);
    (Forall(tpat, ty), u_gen);
  };

let rec syn_block = (ctx: Contexts.t, block: UHExp.block): option(HTyp.t) =>
  switch (block) {
  | Block(lines, e) =>
    switch (syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) => syn_exp(ctx, e)
    }
  }
and syn_lines = (ctx: Contexts.t, lines: UHExp.lines): option(Contexts.t) =>
  List.fold_left(
    (opt_ctx: option(Contexts.t), line: UHExp.line) =>
      switch (opt_ctx) {
      | None => None
      | Some(ctx) => syn_line(ctx, line)
      },
    Some(ctx),
    lines,
  )
and syn_line = (ctx: Contexts.t, line: UHExp.line): option(Contexts.t) =>
  switch (line) {
  | ExpLine(_) => Some(ctx)
  | EmptyLine => Some(ctx)
  | LetLine(p, ann, block) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.expand(uty);
      let ctx_block = ctx_for_let(ctx, p, ty, block);
      switch (ana_block(ctx_block, block, ty)) {
      | None => None
      | Some(_) => ana_pat(ctx, p, ty)
      };
    | None =>
      switch (syn_block(ctx, block)) {
      | None => None
      | Some(ty) => ana_pat(ctx, p, ty)
      }
    }
  }
/* synthesize a type, if possible, for e */
and syn_exp = (ctx: Contexts.t, e: UHExp.t): option(HTyp.t) =>
  switch (e) {
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
    let e' = UHExp.set_err_status_t(NotInHole, e);
    switch (syn_exp(ctx, e')) {
    | None => None
    | Some(_) => Some(Hole)
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
  | Var(NotInHole, NotInVarHole, x) => VarMap.lookup(ctx.vars, x)
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
    switch (ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx) =>
      switch (syn_block(ctx, block)) {
      | None => None
      | Some(ty2) => Some(HTyp.Arrow(ty1, ty2))
      }
    };
  | TyLam(_error_status, tpat, block) =>
    let ctx = tpat_wf(ctx, tpat);
    switch (syn_block(ctx, block)) {
    | None => None
    | Some(ty) => Some(Forall(tpat, ty))
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
    switch (PaletteCtx.lookup(ctx.palettes, name)) {
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
    }
  | Parenthesized(block) => syn_block(ctx, block)
  | OpSeq(skel, seq) =>
    /* NOTE: doesn't check if skel is the correct parse of seq!!! */
    switch (syn_skel(ctx, skel, seq, None)) {
    | Some((ty, _)) => Some(ty)
    | None => None
    }
  }
and syn_skel =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      monitor: option(int),
    )
    : option((HTyp.t, option(type_mode))) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => None
    | Some(en) =>
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (syn_exp(ctx, en)) {
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
      }
    }
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
          Some(Contexts.extend_vars(splice_ctx, (splice_var, ty)));
        }
      },
    Some(Contexts.empty),
  )
and ana_block =
    (ctx: Contexts.t, block: UHExp.block, ty: HTyp.t): option(unit) =>
  switch (block) {
  | Block(lines, e) =>
    switch (syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) => ana_exp(ctx, e, ty)
    }
  }
and ana_exp = (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t): option(unit) =>
  switch (e) {
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
    let e' = UHExp.set_err_status_t(NotInHole, e);
    switch (syn_exp(ctx, e')) {
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
    let e' = UHExp.set_err_status_t(NotInHole, e);
    switch (syn_exp(ctx, e')) {
    | None => None
    | Some(ty') =>
      if (HTyp.consistent(ty, ty')) {
        Some();
      } else {
        None;
      }
    };
  | TyLam(_error_status, _p, _block) => raise(Failure("unimplemented13"))
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
          switch (ana_pat(ctx, p, ty1_ann)) {
          | None => None
          | Some(ctx) => ana_block(ctx, block, ty2)
          }
        };
      | None =>
        switch (ana_pat(ctx, p, ty1_given)) {
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
    switch (syn_exp(ctx, e)) {
    | None => None
    | Some(ty') =>
      if (HTyp.consistent(ty, ty')) {
        Some();
      } else {
        None;
      }
    }
  | Parenthesized(block) => ana_block(ctx, block, ty)
  | OpSeq(skel, seq) =>
    switch (ana_skel(ctx, skel, seq, ty, None)) {
    | None => None
    | Some(_) => Some()
    }
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
  switch (ana_pat(ctx, p, pat_ty)) {
  | None => None
  | Some(ctx) => ana_block(ctx, block, clause_ty)
  };
}
and ana_skel =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      ty: HTyp.t,
      monitor: option(int),
    )
    : option(option(type_mode)) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => None
    | Some(en) =>
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (ana_exp(ctx, en, ty)) {
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
      }
    }
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
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
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
              (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
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
      And | Or | Minus | Plus | Times | LessThan | GreaterThan | Equals | Space,
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
  };

let syn_zlines = (ctx: Contexts.t, zlines: ZExp.zlines): option(Contexts.t) =>
  syn_lines(ctx, ZExp.erase_lines(zlines));

let rec syn_fix_holes_pat =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          ~renumber_empty_holes=false,
          p: UHPat.t,
        )
        : (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
  let p_nih = UHPat.set_err_status_t(NotInHole, p);
  switch (p) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (EmptyHole(u), Hole, ctx, u_gen);
    } else {
      (p, HTyp.Hole, ctx, u_gen);
    }
  | Wild(_) => (p_nih, Hole, ctx, u_gen)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(Keyword(_), _), _) => (p_nih, Hole, ctx, u_gen)
  | Var(_, NotInVarHole, x) =>
    let ctx = Contexts.extend_vars(ctx, (x, Hole));
    (p_nih, Hole, ctx, u_gen);
  | NumLit(_, _) => (p_nih, Num, ctx, u_gen)
  | BoolLit(_, _) => (p_nih, Bool, ctx, u_gen)
  | ListNil(_) => (p_nih, List(Hole), ctx, u_gen)
  | Parenthesized(p) =>
    let (p, ty, ctx, u_gen) =
      syn_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p);
    (Parenthesized(p), ty, ctx, u_gen);
  | OpSeq(skel, seq) =>
    let (skel, seq, ty, ctx, u_gen) =
      syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
    (OpSeq(skel, seq), ty, ctx, u_gen);
  | Inj(_, side, p1) =>
    let (p1, ty1, ctx, u_gen) =
      syn_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p1);
    let p = UHPat.Inj(NotInHole, side, p1);
    let ty =
      switch (side) {
      | L => HTyp.Sum(ty1, Hole)
      | R => HTyp.Sum(Hole, ty1)
      };
    (p, ty, ctx, u_gen);
  };
}
and syn_fix_holes_pat_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
    )
    : (UHPat.skel_t, UHPat.opseq, HTyp.t, Contexts.t, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
    | Some(pn) =>
      let (pn, ty, ctx, u_gen) =
        syn_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, pn);
      switch (OperatorSeq.seq_update_nth(n, seq, pn)) {
      | None => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, ty, ctx, u_gen)
      };
    }
  | BinOp(_, Comma, skel1, skel2) =>
    let (skel1, seq, ty1, ctx, u_gen) =
      syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    let (skel2, seq, ty2, ctx, u_gen) =
      syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
    let skel = Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2);
    let ty = HTyp.Prod(ty1, ty2);
    (skel, seq, ty, ctx, u_gen);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, u_gen) =
      ana_fix_holes_pat_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.Hole,
      );
    let (skel2, seq, ctx, u_gen) =
      ana_fix_holes_pat_skel(
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
      syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    let ty = HTyp.List(ty_elt);
    let (skel2, seq, ctx, u_gen) =
      ana_fix_holes_pat_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        ty,
      );
    let skel = Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2);
    (skel, seq, ty, ctx, u_gen);
  }
and ana_fix_holes_pat =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      p: UHPat.t,
      ty: HTyp.t,
    )
    : (UHPat.t, Contexts.t, MetaVarGen.t) => {
  let p_nih = UHPat.set_err_status_t(NotInHole, p);
  switch (p) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (EmptyHole(u), ctx, u_gen);
    } else {
      (p, ctx, u_gen);
    }
  | Wild(_) => (p_nih, ctx, u_gen)
  | Var(_, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(_, InVarHole(Keyword(_), _), _) => (p_nih, ctx, u_gen)
  | Var(_, NotInVarHole, x) =>
    let ctx = Contexts.extend_vars(ctx, (x, ty));
    (p_nih, ctx, u_gen);
  | NumLit(_, _)
  | BoolLit(_, _) =>
    let (p', ty', ctx, u_gen) =
      syn_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p);
    if (HTyp.consistent(ty, ty')) {
      (UHPat.set_err_status_t(NotInHole, p'), ctx, u_gen);
    } else {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (UHPat.set_err_status_t(InHole(TypeInconsistent, u), p'), ctx, u_gen);
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
      ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p1, ty);
    (Parenthesized(p1), ctx, u_gen);
  | OpSeq(skel, seq) =>
    let (skel, seq, ctx, u_gen) =
      ana_fix_holes_pat_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel,
        seq,
        ty,
      );
    (OpSeq(skel, seq), ctx, u_gen);
  | Inj(_, side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      let (p1, ctx, u_gen) =
        ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p1, ty1);
      (Inj(NotInHole, side, p1), ctx, u_gen);
    | None =>
      let (p1, _, ctx, u_gen) =
        syn_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (Inj(InHole(TypeInconsistent, u), side, p1), ctx, u_gen);
    }
  };
}
and ana_fix_holes_pat_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      ty: HTyp.t,
    )
    : (UHPat.skel_t, UHPat.opseq, Contexts.t, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
    | Some(pn) =>
      let (pn, ctx, u_gen) =
        ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, pn, ty);
      switch (OperatorSeq.seq_update_nth(n, seq, pn)) {
      | None => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, ctx, u_gen)
      };
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      let (skel1, seq, ctx, u_gen) =
        ana_fix_holes_pat_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Hole,
        );
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_pat_skel(
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
            (skel, ty): (UHPat.skel_t, HTyp.t),
            (skels, seq, ctx, u_gen): (
              ListMinTwo.t(UHPat.skel_t),
              UHPat.opseq,
              Contexts.t,
              MetaVarGen.t,
            ),
          ) => {
        let (skel, seq, ctx, u_gen) =
          ana_fix_holes_pat_skel(
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
            (skel1, ty1): (UHPat.skel_t, HTyp.t),
            (skel2, ty2): (UHPat.skel_t, HTyp.t),
          ) => {
        let (skel1, seq, ctx, u_gen) =
          ana_fix_holes_pat_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel1,
            seq,
            ty1,
          );
        let (skel2, seq, ctx, u_gen) =
          ana_fix_holes_pat_skel(
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
        let (skels, seq, ctx, u_gen) = ListMinTwo.fold_right(f, zipped, f0);
        let skel = UHPat.make_tuple(NotInHole, skels);
        (skel, seq, ctx, u_gen);
      | None =>
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let (skels1, seq, ctx, u_gen) = ListMinTwo.fold_right(f, zipped, f0);
        let (skels2, seq, ctx, u_gen) =
          List.fold_right(
            (skel: UHPat.skel_t, (skels, seq, ctx, u_gen)) => {
              let (skel, seq, _, ctx, u_gen) =
                syn_fix_holes_pat_skel(
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
        syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let (skel2, seq, _, ctx, u_gen) =
        syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let skel =
        Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Comma, skel1, skel2);
      (skel, seq, ctx, u_gen);
    }
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ctx, u_gen) =
      ana_fix_holes_pat_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.Hole,
      );
    let (skel2, seq, ctx, u_gen) =
      ana_fix_holes_pat_skel(
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
        ana_fix_holes_pat_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        );
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_pat_skel(
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
        syn_fix_holes_pat_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, ctx, u_gen) =
        ana_fix_holes_pat_skel(
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
  };

let syn_fix_holes_zpat =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t)
    : (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
  let path = CursorPath.of_zpat(zp);
  let p = ZPat.erase(zp);
  let (p, ty, ctx, u_gen) = syn_fix_holes_pat(ctx, u_gen, p);
  let zp = CursorPath.follow_pat_or_fail(path, p);
  (zp, ty, ctx, u_gen);
};

let ana_fix_holes_zpat =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t, ty: HTyp.t)
    : (ZPat.t, Contexts.t, MetaVarGen.t) => {
  let path = CursorPath.of_zpat(zp);
  let p = ZPat.erase(zp);
  let (p, ctx, u_gen) = ana_fix_holes_pat(ctx, u_gen, p, ty);
  let zp = CursorPath.follow_pat_or_fail(path, p);
  (zp, ctx, u_gen);
};

/* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
 * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
 * regardless.
 */
let rec syn_fix_holes_block =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          ~renumber_empty_holes=false,
          block: UHExp.block,
        )
        : (UHExp.block, HTyp.t, MetaVarGen.t) => {
  let Block(lines, e) = block;
  let (lines, ctx, u_gen) =
    syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, lines);
  let (e, ty, u_gen) =
    syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
  (Block(lines, e), ty, u_gen);
}
and syn_fix_holes_lines =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      lines: UHExp.lines,
    )
    : (UHExp.lines, Contexts.t, MetaVarGen.t) => {
  let (rev_fixed_lines, ctx, u_gen) =
    List.fold_left(
      (
        (fixed_lines, ctx, u_gen): (UHExp.lines, Contexts.t, MetaVarGen.t),
        line: UHExp.line,
      ) => {
        let (fixed_line, ctx, u_gen) =
          syn_fix_holes_line(ctx, u_gen, ~renumber_empty_holes, line);
        ([fixed_line, ...fixed_lines], ctx, u_gen);
      },
      ([], ctx, u_gen),
      lines,
    );
  (List.rev(rev_fixed_lines), ctx, u_gen);
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
      syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
    (ExpLine(e), ctx, u_gen);
  | EmptyLine => (line, ctx, u_gen)
  | LetLine(p, ann, block) =>
    switch (ann) {
    | Some(uty1) =>
      let (uty1, u_gen) = fix_holes_ty(ctx, uty1, u_gen);
      let ann = Some(uty1);
      let ty1 = UHTyp.expand(uty1);
      let ctx_block = ctx_for_let(ctx, p, ty1, block);
      let (block, u_gen) =
        ana_fix_holes_block(
          ctx_block,
          u_gen,
          ~renumber_empty_holes,
          block,
          ty1,
        );
      let (p, ctx, u_gen) =
        ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p, ty1);
      (LetLine(p, ann, block), ctx, u_gen);
    | None =>
      let (block, ty1, u_gen) =
        syn_fix_holes_block(~renumber_empty_holes, ctx, u_gen, block);
      let (p, ctx, u_gen) =
        ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p, ty1);
      (LetLine(p, ann, block), ctx, u_gen);
    }
  }
and syn_fix_holes_exp =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      e: UHExp.t,
    )
    : (UHExp.t, HTyp.t, MetaVarGen.t) => {
  let e_nih = UHExp.set_err_status_t(NotInHole, e);
  switch (e) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (EmptyHole(u), Hole, u_gen);
    } else {
      (e, Hole, u_gen);
    }
  | Var(_, var_err_status, x) =>
    switch (VarMap.lookup(ctx.vars, x)) {
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
    }
  | NumLit(_, _) => (e_nih, Num, u_gen)
  | BoolLit(_, _) => (e_nih, Bool, u_gen)
  | ListNil(_) => (e_nih, List(Hole), u_gen)
  | Parenthesized(block) =>
    let (block, ty, u_gen) =
      syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
    (Parenthesized(block), ty, u_gen);
  | OpSeq(skel, seq) =>
    switch (
      syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq)
    ) {
    | (Placeholder(_), _, _, _) =>
      raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
    | (skel, seq, ty, u_gen) => (OpSeq(skel, seq), ty, u_gen)
    }
  | TyLam(err_status, tpat, block) =>
    let ctx = tpat_wf(ctx, tpat);
    let (block, ty, u_gen) = syn_fix_holes_block(ctx, u_gen, block);
    /* cc: should we do something with the err status here? */
    (TyLam(err_status, tpat, block), Forall(tpat, ty), u_gen);
  | Lam(_, p, ann, block) =>
    let (uty1, u_gen) =
      switch (ann) {
      | Some(uty1) =>
        let (uty1, u_gen) = fix_holes_ty(ctx, uty1, u_gen);

        (uty1, u_gen);
      | None => (UHTyp.Hole, u_gen)
      };
    let ann = Some(uty1);
    let ty1 = UHTyp.expand(uty1);
    let (p, ctx, u_gen) =
      ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p, ty1);
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
      ana_fix_holes_rules(ctx, u_gen, ~renumber_empty_holes, rules, ty1, ty);
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
    (Case(NotInHole, block, rules, Some(Hole)), HTyp.Hole, u_gen);
  | ApPalette(_, name, serialized_model, psi) =>
    switch (PaletteCtx.lookup(ctx.palettes, name)) {
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
    }
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
          ana_rule_fix_holes(
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
and ana_rule_fix_holes =
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
    ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p, pat_ty);
  let (block, u_gen) =
    ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block, clause_ty);
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
and ana_fix_holes_block =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      block: UHExp.block,
      ty: HTyp.t,
    )
    : (UHExp.block, MetaVarGen.t) => {
  let Block(lines, e) = block;
  let (lines, ctx, u_gen) =
    syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, lines);
  let (e, u_gen) =
    ana_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e, ty);
  (Block(lines, e), u_gen);
}
and ana_fix_holes_exp =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      e: UHExp.t,
      ty: HTyp.t,
    )
    : (UHExp.t, MetaVarGen.t) =>
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
      syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
    if (HTyp.consistent(ty, ty')) {
      (UHExp.set_err_status_t(NotInHole, e), u_gen);
    } else {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (UHExp.set_err_status_t(InHole(TypeInconsistent, u), e), u_gen);
    };
  | ListNil(_) =>
    switch (HTyp.matched_list(ty)) {
    | Some(_) => (UHExp.set_err_status_t(NotInHole, e), u_gen)
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (ListNil(InHole(TypeInconsistent, u)), u_gen);
    }
  | Parenthesized(block) =>
    let (block, u_gen) =
      ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block, ty);
    (Parenthesized(block), u_gen);
  | OpSeq(skel, seq) =>
    switch (
      ana_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq, ty)
    ) {
    | (Skel.Placeholder(_), _, _) =>
      raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
    | (Skel.BinOp(_, _, _, _) as skel, seq, u_gen) => (
        OpSeq(skel, seq),
        u_gen,
      )
    }
  | TyLam(_err_status, _p, _block) => raise(Failure("unimplemented15"))
  | Lam(_, p, ann, block) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let (uty1, u_gen) = fix_holes_ty(ctx, uty1, u_gen);
        let ty1_ann = UHTyp.expand(uty1);
        if (HTyp.consistent(ty1_ann, ty1_given)) {
          let (p, ctx, u_gen) =
            ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p, ty1_ann);
          let (block, u_gen) =
            ana_fix_holes_block(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              block,
              ty2,
            );
          (UHExp.Lam(NotInHole, p, ann, block), u_gen);
        } else {
          let (e', _, u_gen) =
            syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
          let (u, u_gen) = MetaVarGen.next(u_gen);
          (UHExp.set_err_status_t(InHole(TypeInconsistent, u), e'), u_gen);
        };
      | None =>
        let (p, ctx, u_gen) =
          ana_fix_holes_pat(ctx, u_gen, ~renumber_empty_holes, p, ty1_given);
        let (block, u_gen) =
          ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block, ty2);
        (UHExp.Lam(NotInHole, p, ann, block), u_gen);
      }
    | None =>
      let (e', _, u_gen) =
        syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (UHExp.set_err_status_t(InHole(TypeInconsistent, u), e'), u_gen);
    }
  | Inj(_, side, block) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((ty1, ty2)) =>
      let (e1, u_gen) =
        ana_fix_holes_block(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          block,
          InjSide.pick(side, ty1, ty2),
        );
      (Inj(NotInHole, side, e1), u_gen);
    | None =>
      let (e', ty', u_gen) =
        syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
      if (HTyp.consistent(ty, ty')) {
        (UHExp.set_err_status_t(NotInHole, e'), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (UHExp.set_err_status_t(InHole(TypeInconsistent, u), e'), u_gen);
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
        syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (UHExp.set_err_status_t(InHole(TypeInconsistent, u), e'), u_gen);
    };
  | Case(_, block, rules, None) =>
    let (e1, ty1, u_gen) =
      syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, block);
    let (rules, u_gen) =
      ana_fix_holes_rules(ctx, u_gen, ~renumber_empty_holes, rules, ty1, ty);
    (Case(NotInHole, e1, rules, None), u_gen);
  | ApPalette(_, _, _, _) =>
    let (e', ty', u_gen) =
      syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, e);
    if (HTyp.consistent(ty, ty')) {
      (UHExp.set_err_status_t(NotInHole, e'), u_gen);
    } else {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (UHExp.set_err_status_t(InHole(TypeInconsistent, u), e'), u_gen);
    };
  }
and syn_fix_holes_exp_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
    )
    : (UHExp.skel_t, UHExp.opseq, HTyp.t, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let (en, ty, u_gen) =
        syn_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en)) {
      | None => raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, ty, u_gen)
      };
    }
  | BinOp(_, (Minus | Plus | Times) as op, skel1, skel2) =>
    let (skel1, seq, u_gen) =
      ana_fix_holes_exp_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.Num,
      );
    let (skel2, seq, u_gen) =
      ana_fix_holes_exp_skel(
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
      ana_fix_holes_exp_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.Bool,
      );
    let (skel2, seq, u_gen) =
      ana_fix_holes_exp_skel(
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
      ana_fix_holes_exp_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.Num,
      );
    let (skel2, seq, u_gen) =
      ana_fix_holes_exp_skel(
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
      syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    switch (HTyp.matched_arrow(ty1)) {
    | Some((ty2, ty)) =>
      let (skel2, seq, u_gen) =
        ana_fix_holes_exp_skel(
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
        ana_fix_holes_exp_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Hole,
        );
      let (skel1, seq, u_gen) =
        UHExp.make_opseq_inconsistent(u_gen, skel1, seq);
      (BinOp(NotInHole, Space, skel1, skel2), seq, Hole, u_gen);
    };
  | BinOp(_, Comma, skel1, skel2) =>
    let (skel1, seq, ty1, u_gen) =
      syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    let (skel2, seq, ty2, u_gen) =
      syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
    let skel = Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2);
    let ty = HTyp.Prod(ty1, ty2);
    (skel, seq, ty, u_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    let (skel1, seq, ty_elt, u_gen) =
      syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    let ty = HTyp.List(ty_elt);
    let (skel2, seq, u_gen) =
      ana_fix_holes_exp_skel(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        ty,
      );
    let skel = Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2);
    (skel, seq, ty, u_gen);
  }
and ana_fix_holes_exp_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      ty: HTyp.t,
    )
    : (UHExp.skel_t, UHExp.opseq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let (en, u_gen) =
        ana_fix_holes_exp(ctx, u_gen, ~renumber_empty_holes, en, ty);
      switch (OperatorSeq.seq_update_nth(n, seq, en)) {
      | None => raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, u_gen)
      };
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_exp_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Hole,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_exp_skel(
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
            (skel, ty): (UHExp.skel_t, HTyp.t),
            (skels, seq, u_gen): (
              ListMinTwo.t(UHExp.skel_t),
              UHExp.opseq,
              MetaVarGen.t,
            ),
          ) => {
        let (skel, seq, u_gen) =
          ana_fix_holes_exp_skel(
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
            (skel1, ty1): (UHExp.skel_t, HTyp.t),
            (skel2, ty2): (UHExp.skel_t, HTyp.t),
          ) => {
        let (skel1, seq, u_gen) =
          ana_fix_holes_exp_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel1,
            seq,
            ty1,
          );
        let (skel2, seq, u_gen) =
          ana_fix_holes_exp_skel(
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
            (skel: UHExp.skel_t, (skels, seq, u_gen)) => {
              let (skel, seq, _, u_gen) =
                syn_fix_holes_exp_skel(
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
        syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let (skel2, seq, _, u_gen) =
        syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let skel =
        Skel.BinOp(InHole(TypeInconsistent, u), UHExp.Comma, skel1, skel2);
      (skel, seq, u_gen);
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | Some(ty_elt) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_exp_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        );
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, u_gen) =
        ana_fix_holes_exp_skel(
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
        syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let ty_list = HTyp.List(ty_elt);
      let (skel2, seq, u_gen) =
        ana_fix_holes_exp_skel(
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
      And | Or | Minus | Plus | Times | LessThan | GreaterThan | Equals | Space,
      _,
      _,
    ) =>
    let (skel, seq, ty', u_gen) =
      syn_fix_holes_exp_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
    if (HTyp.consistent(ty, ty')) {
      (skel, seq, u_gen);
    } else {
      UHExp.make_opseq_inconsistent(u_gen, skel, seq);
    };
  };

let syn_fix_holes_zexp =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t)
    : (ZExp.t, HTyp.t, MetaVarGen.t) => {
  let path = CursorPath.of_zexp(ze);
  let e = ZExp.erase(ze);
  let (e, ty, u_gen) = syn_fix_holes_exp(ctx, u_gen, e);
  let ze = CursorPath.follow_e_or_fail(path, e);
  (ze, ty, u_gen);
};

let syn_fix_holes_zblock =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zblock: ZExp.zblock)
    : (ZExp.zblock, HTyp.t, MetaVarGen.t) => {
  let path = CursorPath.of_zblock(zblock);
  let block = ZExp.erase_block(zblock);
  let (block, ty, u_gen) = syn_fix_holes_block(ctx, u_gen, block);
  let zblock = CursorPath.follow_block_or_fail(path, block);
  (zblock, ty, u_gen);
};

let syn_fix_holes_zlines =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zlines: ZExp.zlines)
    : (ZExp.zlines, Contexts.t, MetaVarGen.t) => {
  let path = CursorPath.of_zlines(zlines);
  let lines = ZExp.erase_lines(zlines);
  let (lines, ctx, u_gen) = syn_fix_holes_lines(ctx, u_gen, lines);
  let zlines = CursorPath.follow_lines_or_fail(path, lines);
  (zlines, ctx, u_gen);
};

let ana_fix_holes_zblock =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zblock: ZExp.zblock, ty: HTyp.t)
    : (ZExp.zblock, MetaVarGen.t) => {
  let (steps, _) as path = CursorPath.of_zblock(zblock);
  let block = ZExp.erase_block(zblock);
  let (block, u_gen) = ana_fix_holes_block(ctx, u_gen, block, ty);
  switch (CursorPath.follow_block(path, block)) {
  | None =>
    // Only way this can happen now is path was originally
    // on case type annotation and ana_fix_holes stripped
    // the annotation, in which case we can just place cursor
    // at end of case node. We might just wanna write a proper
    // recursive traversal for hole-fixing zexps/blocks.
    switch (steps |> split_last) {
    | None => assert(false)
    | Some((case_steps, _)) =>
      switch (CursorPath.follow_block_and_place_after(case_steps, block)) {
      | None => assert(false)
      | Some(zblock) => (zblock, u_gen)
      }
    }
  | Some(zblock) => (zblock, u_gen)
  };
};

let ana_fix_holes_zexp =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t, ty: HTyp.t)
    : (ZExp.t, MetaVarGen.t) => {
  let (steps, _) as path = CursorPath.of_zexp(ze);
  let e = ZExp.erase(ze);
  let (e, u_gen) = ana_fix_holes_exp(ctx, u_gen, e, ty);
  switch (CursorPath.follow_exp(path, e)) {
  | None =>
    // Only way this can happen now is path was originally
    // on case type annotation and ana_fix_holes stripped
    // the annotation, in which case we can just place cursor
    // at end of case node. We might just wanna write a proper
    // recursive traversal for hole-fixing zexps/blocks.
    switch (steps |> split_last) {
    | None => assert(false)
    | Some((case_steps, _)) =>
      switch (CursorPath.follow_exp_and_place_after(case_steps, e)) {
      | None => assert(false)
      | Some(ze) => (ze, u_gen)
      }
    }
  | Some(ze) => (ze, u_gen)
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

let fix_and_renumber_holes_z =
    (ctx: Contexts.t, zblock: ZExp.zblock): edit_state => {
  let (block, ty, u_gen) =
    fix_and_renumber_holes(ctx, zblock |> ZExp.erase_block);
  let zblock =
    CursorPath.follow_block_or_fail(CursorPath.of_zblock(zblock), block);
  (zblock, ty, u_gen);
};
