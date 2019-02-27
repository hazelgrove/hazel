open SemanticsCommon;
open HazelUtil;

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

let rec syn_pat = (ctx, p) =>
  switch (p) {
  | UHPat.Pat(InHole(TypeInconsistent, _), p')
  | UHPat.Pat(
      InHole(WrongLength, _),
      UHPat.OpSeq(Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, _, _), _) as p',
    ) =>
    switch (syn_pat'(ctx, p')) {
    | None => None
    | Some((_, gamma)) => Some((HTyp.Hole, gamma))
    }
  | UHPat.Pat(InHole(WrongLength, _), _) => None
  | UHPat.Pat(NotInHole, p') => syn_pat'(ctx, p')
  | UHPat.Parenthesized(p) => syn_pat(ctx, p)
  }
and syn_pat' = (ctx, p) =>
  switch (p) {
  | UHPat.EmptyHole(_) => Some((HTyp.Hole, ctx))
  | UHPat.Wild => Some((HTyp.Hole, ctx))
  | UHPat.Var(x) =>
    Var.check_valid(
      x,
      Some((HTyp.Hole, Contexts.extend_gamma(ctx, (x, HTyp.Hole)))),
    )
  | UHPat.NumLit(_) => Some((HTyp.Num, ctx))
  | UHPat.BoolLit(_) => Some((HTyp.Bool, ctx))
  | UHPat.Inj(side, p1) =>
    switch (syn_pat(ctx, p1)) {
    | Some((ty1, ctx)) =>
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, HTyp.Hole)
        | R => HTyp.Sum(HTyp.Hole, ty1)
        };

      Some((ty, ctx));
    | None => None
    }
  | UHPat.ListNil => Some((HTyp.List(HTyp.Hole), ctx))
  /* | UHPat.ListLit ps ->
     List.fold_left (fun opt_result elt ->
       match opt_result with
       | None -> None
       | Some (ty, ctx) ->
         match syn_pat ctx elt with
         | None -> None
         | Some (ty_elt, ctx) ->
           match HTyp.join ty ty_elt with
           | Some ty -> Some (ty, ctx)
           | None -> None
           end
         end
       end) ps (Some (HTyp.Hole, ctx)) */
  | UHPat.OpSeq(skel, seq) =>
    switch (syn_skel_pat(ctx, skel, seq, None)) {
    | Some((ty, ctx, _)) => Some((ty, ctx))
    | None => None
    }
  }
and syn_skel_pat = (ctx, skel, seq, monitor) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
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
  | Skel.BinOp(InHole(TypeInconsistent, u), op, skel1, skel2)
  | Skel.BinOp(InHole(WrongLength, u), UHPat.Comma as op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel_pat(ctx, skel_not_in_hole, seq, monitor)) {
    | None => None
    | Some((_, ctx, mode)) => Some((HTyp.Hole, ctx, mode))
    };
  | Skel.BinOp(InHole(WrongLength, u), _, _, _) => None
  | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) =>
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
  | Skel.BinOp(NotInHole, UHPat.Space, skel1, skel2) =>
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
  | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) =>
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
and ana_pat = (ctx, p, ty) =>
  switch (p) {
  | UHPat.Pat(InHole(TypeInconsistent, _), p') =>
    switch (syn_pat'(ctx, p')) {
    | None => None
    | Some((_, ctx)) => Some(ctx)
    }
  | UHPat.Pat(
      InHole(WrongLength, _),
      UHPat.OpSeq(Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, _, _), _) as p',
    )
  | UHPat.Pat(NotInHole, p') => ana_pat'(ctx, p', ty)
  | UHPat.Pat(InHole(WrongLength, _), _) => None
  | UHPat.Parenthesized(p) => ana_pat(ctx, p, ty)
  }
and ana_pat' = (ctx, p, ty) =>
  switch (p) {
  | UHPat.Var(x) =>
    Var.check_valid(x, Some(Contexts.extend_gamma(ctx, (x, ty))))
  | UHPat.EmptyHole(_)
  | UHPat.Wild => Some(ctx)
  | UHPat.NumLit(_)
  | UHPat.BoolLit(_) =>
    switch (syn_pat'(ctx, p)) {
    | None => None
    | Some(p) =>
      let (ty', ctx1) = p;
      if (HTyp.consistent(ty, ty')) {
        Some(ctx1);
      } else {
        None;
      };
    }
  | UHPat.Inj(side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      ana_pat(ctx, p1, ty1);
    }
  | UHPat.ListNil =>
    switch (HTyp.matched_list(ty)) {
    | Some(_) => Some(ctx)
    | None => None
    }
  /* | UHPat.ListLit ps ->
     match HTyp.matched_list ty with
     | None -> None
     | Some ty_elts ->
       List.fold_left (fun optctx p ->
         match optctx with
         | None -> None
         | Some ctx -> ana_pat ctx p ty_elts
         end) ps (Some ctx)
     end */
  | UHPat.OpSeq(skel, seq) =>
    switch (ana_skel_pat(ctx, skel, seq, ty, None)) {
    | Some((ctx, _)) => Some(ctx)
    | None => None
    }
  }
and ana_skel_pat = (ctx, skel, seq, ty, monitor) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
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
  | Skel.BinOp(InHole(TypeInconsistent, u), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel_pat(ctx, skel_not_in_hole, seq, monitor)) {
    | None => None
    | Some((_, ctx, mode)) => Some((ctx, mode))
    };
  | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
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
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      switch (HazelUtil.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
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
          Some((ctx, None)),
          zipped,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, u), UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      let n_types = List.length(types);
      let n_skels = List.length(skels);
      n_types == n_skels
        ? None  /* make sure the lengths are actually different */
        : {
          let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
          let ana_zipped: option((Contexts.t, option(type_mode))) = (
            List.fold_left(
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
              Some((ctx, None)),
              zipped,
            ):
              option((Contexts.t, option(type_mode)))
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
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(_, UHPat.Space, skel1, skel2) =>
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
  | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) =>
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

let ctx_for_let = (ctx, p, ty1, e1) =>
  switch (p, e1) {
  | (UHPat.Pat(_, UHPat.Var(x)), UHExp.Tm(_, UHExp.Lam(_, _, _))) =>
    switch (HTyp.matched_arrow(ty1)) {
    | Some(_) => Contexts.extend_gamma(ctx, (x, ty1))
    | None => ctx
    }
  | _ => ctx
  };

/* returns recursive ctx + name of recursively defined var */
let ctx_for_let' = (ctx, p, ty1, e1) =>
  switch (p, e1) {
  | (UHPat.Pat(_, UHPat.Var(x)), UHExp.Tm(_, UHExp.Lam(_, _, _))) =>
    switch (HTyp.matched_arrow(ty1)) {
    | Some(_) => (Contexts.extend_gamma(ctx, (x, ty1)), Some(x))
    | None => (ctx, None)
    }
  | _ => (ctx, None)
  };

/* synthesize a type, if possible, for e */
let rec syn = (ctx, e) =>
  switch (e) {
  | UHExp.Tm(InHole(TypeInconsistent, _), e')
  | UHExp.Tm(
      InHole(WrongLength, _),
      OpSeq(Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, _, _), _) as e',
    ) =>
    switch (syn'(ctx, e')) {
    | Some(_) => Some(HTyp.Hole)
    | None => None
    }
  | UHExp.Tm(InHole(WrongLength, _), _) => None
  | UHExp.Tm(NotInHole, e') => syn'(ctx, e')
  | UHExp.Parenthesized(e1) => syn(ctx, e1)
  }
and syn' = (ctx, e) =>
  switch (e) {
  | UHExp.EmptyHole(_) => Some(HTyp.Hole)
  | UHExp.Asc(e1, uty) =>
    let ty = UHTyp.expand(uty);
    if (UHExp.bidelimited(e1)) {
      switch (ana(ctx, e1, ty)) {
      | Some(_) => Some(ty)
      | None => None
      };
    } else {
      None;
    };
  | UHExp.Var(NotInVHole, x) =>
    let (gamma, _) = ctx;
    VarMap.lookup(gamma, x);
  | UHExp.Var(InVHole(_), _) => Some(HTyp.Hole)
  | UHExp.Lam(p, ann, e1) =>
    let ty1 =
      switch (ann) {
      | Some(uty) => UHTyp.expand(uty)
      | None => HTyp.Hole
      };

    switch (ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) =>
      switch (syn(ctx1, e1)) {
      | None => None
      | Some(ty2) => Some(HTyp.Arrow(ty1, ty2))
      }
    };
  | UHExp.Inj(side, e1) =>
    switch (syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      switch (side) {
      | L => Some(HTyp.Sum(ty1, HTyp.Hole))
      | R => Some(HTyp.Sum(HTyp.Hole, ty1))
      }
    }
  | UHExp.LineItem(li, e1) =>
    switch (syn_line_item(ctx, li)) {
    | None => None
    | Some(ctx) => syn(ctx, e1)
    }
  | NumLit(_) => Some(HTyp.Num)
  | BoolLit(_) => Some(HTyp.Bool)
  | ListNil => Some(HTyp.List(HTyp.Hole))
  | OpSeq(skel, seq) =>
    /* NOTE: doesn't check if skel is the correct parse of seq!!! */
    switch (syn_skel(ctx, skel, seq, None)) {
    | Some((ty, _)) => Some(ty)
    | None => None
    }
  | Case(_, _) => None
  | ApPalette(name, serialized_model, psi) =>
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
        switch (ana(splice_ctx, expansion, expansion_ty)) {
        | None => None
        | Some(_) => Some(expansion_ty)
        };
      }
    };
  }
and syn_line_item = (ctx, li) =>
  switch (li) {
  | EmptyLine => Some(ctx)
  | ExpLine(_) => Some(ctx)
  | LetLine(p, ann, e) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.expand(uty);
      let ctx1 = ctx_for_let(ctx, p, ty, e);
      switch (ana(ctx1, e, ty)) {
      | None => None
      | Some(_) => ana_pat(ctx, p, ty)
      };
    | None =>
      switch (syn(ctx, e)) {
      | None => None
      | Some(ty) => ana_pat(ctx, p, ty)
      }
    }
  }
and ana_splice_map = (ctx, splice_map) =>
  NatMap.fold(
    splice_map,
    (c, (splice_name, (ty, e))) =>
      switch (c) {
      | None => None
      | Some(splice_ctx) =>
        switch (ana(ctx, e, ty)) {
        | None => None
        | Some(_) =>
          let splice_var = SpliceInfo.var_of_splice_name(splice_name);
          Some(Contexts.extend_gamma(splice_ctx, (splice_var, ty)));
        }
      },
    Some(Contexts.empty),
  )
and ana = (ctx, e, ty) =>
  switch (e) {
  | UHExp.Tm(InHole(TypeInconsistent, _), e') =>
    switch (syn'(ctx, e')) {
    | None => None
    | Some(_) => Some() /* this is a consequence of subsumption and hole universality */
    }
  | UHExp.Tm(
      InHole(WrongLength, _),
      OpSeq(Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, _, _), _) as e',
    )
  | UHExp.Tm(NotInHole, e') => ana'(ctx, e', ty)
  | UHExp.Tm(InHole(WrongLength, _), _) => None
  | UHExp.Parenthesized(e1) => ana(ctx, e1, ty)
  }
and ana' = (ctx, e, ty) =>
  switch (e) {
  | UHExp.LineItem(li, e1) =>
    switch (syn_line_item(ctx, li)) {
    | None => None
    | Some(ctx) => ana(ctx, e1, ty)
    }
  | UHExp.Lam(p, ann, e1) =>
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
          | Some(ctx1) => ana(ctx1, e1, ty2)
          }
        };
      | None =>
        switch (ana_pat(ctx, p, ty1_given)) {
        | None => None
        | Some(ctx1) => ana(ctx1, e1, ty2)
        }
      }
    }
  | UHExp.Inj(side, e') =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) => ana(ctx, e', pick_side(side, ty1, ty2))
    }
  | ListNil =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(_) => Some()
    }
  /* | ListLit es ->
     match HTyp.matched_list ty with
     | None -> None
     | Some ty_elt ->
       List.fold_left (fun optresult elt ->
         match optresult with
         | None -> None
         | Some _ -> ana ctx elt ty_elt
         end) es (Some tt)
     end */
  | Case(e1, rules) =>
    switch (syn(ctx, e1)) {
    | None => None
    | Some(ty1) => ana_rules(ctx, rules, ty1, ty)
    }
  | OpSeq(skel, seq) =>
    switch (ana_skel(ctx, skel, seq, ty, None)) {
    | None => None
    | Some(_) => Some()
    }
  | UHExp.EmptyHole(_)
  | UHExp.Asc(_, _)
  | UHExp.Var(_, _)
  | NumLit(_)
  | BoolLit(_)
  | ApPalette(_, _, _) =>
    switch (syn'(ctx, e)) {
    | None => None
    | Some(ty') =>
      if (HTyp.consistent(ty, ty')) {
        Some();
      } else {
        None;
      }
    }
  }
and ana_rules = (ctx, rules, pat_ty, clause_ty) =>
  List.fold_left(
    (b, r) =>
      switch (b) {
      | None => None
      | Some(_) => ana_rule(ctx, r, pat_ty, clause_ty)
      },
    Some(),
    rules,
  )
and ana_rule = (ctx, rule, pat_ty, clause_ty) => {
  let UHExp.Rule(p, e) = rule;
  switch (ana_pat(ctx, p, pat_ty)) {
  | None => None
  | Some(ctx1) => ana(ctx1, e, clause_ty)
  };
}
and syn_skel = (ctx, skel, seq, monitor) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => None
    | Some(en) =>
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (syn(ctx, en)) {
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
  | Skel.BinOp(InHole(TypeInconsistent, u), op, skel1, skel2)
  | Skel.BinOp(InHole(WrongLength, u), UHExp.Comma as op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_skel(ctx, skel_not_in_hole, seq, monitor)) {
    | None => None
    | Some((ty, mode)) => Some((HTyp.Hole, mode))
    };
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(NotInHole, Plus, skel1, skel2)
  | Skel.BinOp(NotInHole, Times, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Num, monitor)) {
    | None => None
    | Some(mode1) =>
      switch (ana_skel(ctx, skel2, seq, HTyp.Num, monitor)) {
      | None => None
      | Some(mode2) => Some((HTyp.Num, combine_modes(mode1, mode2)))
      }
    }
  | Skel.BinOp(NotInHole, LessThan, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Num, monitor)) {
    | None => None
    | Some(mode1) =>
      switch (ana_skel(ctx, skel2, seq, HTyp.Num, monitor)) {
      | None => None
      | Some(mode2) => Some((HTyp.Bool, combine_modes(mode1, mode2)))
      }
    }
  | Skel.BinOp(NotInHole, Space, skel1, skel2) =>
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
  | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) =>
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
  | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) =>
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
and ana_skel = (ctx, skel, seq, ty, monitor) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => None
    | Some(en) =>
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (ana(ctx, en, ty)) {
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
  | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (ana_skel(ctx, skel1, seq, HTyp.Hole, monitor)) {
      | None => None
      | Some(mode1) =>
        switch (ana_skel(ctx, skel2, seq, HTyp.Hole, monitor)) {
        | None => None
        | Some(mode2) =>
          let mode = combine_modes(mode1, mode2);
          Some(mode);
        }
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      switch (HazelUtil.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
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
          Some(None),
          zipped,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, u), UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      let n_types = List.length(types);
      let n_skels = List.length(skels);
      n_types == n_skels
        ? None  /* make sure the lengths are actually different */
        : {
          let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
          let ana_zipped: option(option(type_mode)) = (
            List.fold_left(
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
              Some(None),
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
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) =>
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
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _)
  | Skel.BinOp(NotInHole, Plus, _, _)
  | Skel.BinOp(NotInHole, Times, _, _)
  | Skel.BinOp(NotInHole, LessThan, _, _)
  | Skel.BinOp(NotInHole, Space, _, _) =>
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

let rec syn_pat_fix_holes = (ctx, u_gen, renumber_empty_holes, p) =>
  switch (p) {
  | UHPat.Pat(_, p') =>
    switch (syn_pat_fix_holes'(ctx, u_gen, renumber_empty_holes, p')) {
    | None => None
    | Some((UHPat.OpSeq(Skel.BinOp(err, _, _, _), _) as p', ty, ctx, u_gen)) =>
      Some((UHPat.Pat(err, p'), ty, ctx, u_gen))
    | Some((p', ty, ctx, u_gen)) =>
      Some((UHPat.Pat(NotInHole, p'), ty, ctx, u_gen))
    }
  | UHPat.Parenthesized(p) =>
    switch (syn_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p)) {
    | None => None
    | Some((p, ty, ctx, u_gen)) =>
      Some((UHPat.Parenthesized(p), ty, ctx, u_gen))
    }
  }
and syn_pat_fix_holes' = (ctx, u_gen, renumber_empty_holes, p) =>
  switch (p) {
  | UHPat.EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((UHPat.EmptyHole(u), HTyp.Hole, ctx, u_gen));
    } else {
      Some((p, HTyp.Hole, ctx, u_gen));
    }
  | UHPat.Wild => Some((p, HTyp.Hole, ctx, u_gen))
  | UHPat.Var(x) =>
    Var.check_valid(
      x,
      {
        let ctx = Contexts.extend_gamma(ctx, (x, HTyp.Hole));
        Some((p, HTyp.Hole, ctx, u_gen));
      },
    )
  | UHPat.NumLit(_) => Some((p, HTyp.Num, ctx, u_gen))
  | UHPat.BoolLit(_) => Some((p, HTyp.Bool, ctx, u_gen))
  | UHPat.ListNil => Some((p, HTyp.List(HTyp.Hole), ctx, u_gen))
  /* | UHPat.ListLit ps ->
     let opt_result = List.fold_left (fun opt_result p ->
       match opt_result with
       | None -> None
       | Some (ps, ty, ctx, u_gen) ->
         match syn_pat_fix_holes ctx u_gen renumber_empty_holes p with
         | Some (p, ty', ctx, u_gen) ->
           match HTyp.join ty ty' with
           | Some ty_joined -> Some (cons p ps, ty_joined, ctx, u_gen)
           | None ->
             match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty with
             | None -> None
             | Some (p, ctx, u_gen) -> Some (cons p ps, ty, ctx, u_gen)
             end
           end
         | None ->
           match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty with
           | None -> None
           | Some (p, ctx, u_gen) -> Some (cons p ps, ty, ctx, u_gen)
           end
         end
       end) ps (Some ([], HTyp.Hole, ctx, u_gen)) in
     match opt_result with
     | None -> None
     | Some (ps, ty, ctx, u_gen) ->
       Some (UHPat.ListLit ps, HTyp.List ty, ctx, u_gen)
     end */
  | UHPat.Inj(side, p1) =>
    switch (syn_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p1)) {
    | None => None
    | Some((p1, ty1, ctx, u_gen)) =>
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, HTyp.Hole)
        | R => HTyp.Sum(HTyp.Hole, ty1)
        };

      Some((UHPat.Inj(side, p1), ty, ctx, u_gen));
    }
  | UHPat.OpSeq(skel, seq) =>
    switch (
      syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel, seq)
    ) {
    | None => None
    | Some((skel, seq, ty, ctx, u_gen)) =>
      Some((UHPat.OpSeq(skel, seq), ty, ctx, u_gen))
    }
  }
and syn_skel_pat_fix_holes = (ctx, u_gen, renumber_empty_holes, skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => None
    | Some(pn) =>
      switch (UHPat.bidelimited(pn)) {
      | false => None
      | true =>
        switch (syn_pat_fix_holes(ctx, u_gen, renumber_empty_holes, pn)) {
        | None => None
        | Some((pn, ty, ctx, u_gen)) =>
          switch (OperatorSeq.seq_update_nth(n, seq, pn)) {
          | None => None
          | Some(seq) => Some((skel, seq, ty, ctx, u_gen))
          }
        }
      }
    }
  | Skel.BinOp(_, UHPat.Comma, skel1, skel2) =>
    switch (
      syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)
    ) {
    | None => None
    | Some((skel1, seq, ty1, ctx, u_gen)) =>
      switch (
        syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel2, seq)
      ) {
      | None => None
      | Some((skel2, seq, ty2, ctx, u_gen)) =>
        let skel = Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2);
        let ty = HTyp.Prod(ty1, ty2);
        Some((skel, seq, ty, ctx, u_gen));
      }
    }
  | Skel.BinOp(_, UHPat.Space, skel1, skel2) =>
    switch (
      ana_skel_pat_fix_holes(
        ctx,
        u_gen,
        renumber_empty_holes,
        skel1,
        seq,
        HTyp.Hole,
      )
    ) {
    | None => None
    | Some((skel1, seq, ctx, u_gen)) =>
      switch (
        ana_skel_pat_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel2,
          seq,
          HTyp.Hole,
        )
      ) {
      | None => None
      | Some((skel2, seq, ctx, u_gen)) =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let skel =
          Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Space, skel1, skel2);
        let ty = HTyp.Hole;
        Some((skel, seq, ty, ctx, u_gen));
      }
    }
  | Skel.BinOp(_, UHPat.Cons, skel1, skel2) =>
    switch (
      syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)
    ) {
    | None => None
    | Some((skel1, seq, ty_elt, ctx, u_gen)) =>
      let ty = HTyp.List(ty_elt);
      switch (
        ana_skel_pat_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel2,
          seq,
          ty,
        )
      ) {
      | None => None
      | Some((skel2, seq, ctx, u_gen)) =>
        let skel = Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2);
        Some((skel, seq, ty, ctx, u_gen));
      };
    }
  }
and ana_pat_fix_holes = (ctx, u_gen, renumber_empty_holes, p, ty) =>
  switch (p) {
  | UHPat.Pat(_, p') =>
    switch (ana_pat_fix_holes'(ctx, u_gen, renumber_empty_holes, p', ty)) {
    | None => None
    | Some((err_status, p', ctx, u_gen)) =>
      Some((UHPat.Pat(err_status, p'), ctx, u_gen))
    }
  | UHPat.Parenthesized(p) =>
    switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p, ty)) {
    | None => None
    | Some((p, ctx, u_gen)) => Some((UHPat.Parenthesized(p), ctx, u_gen))
    }
  }
and ana_pat_fix_holes' = (ctx, u_gen, renumber_empty_holes, p, ty) =>
  switch (p) {
  | UHPat.Wild => Some((NotInHole, p, ctx, u_gen))
  | UHPat.Var(x) =>
    Var.check_valid(
      x,
      {
        let ctx = Contexts.extend_gamma(ctx, (x, ty));
        Some((NotInHole, p, ctx, u_gen));
      },
    )
  | UHPat.EmptyHole(_)
  | UHPat.NumLit(_)
  | UHPat.BoolLit(_) =>
    switch (syn_pat_fix_holes'(ctx, u_gen, renumber_empty_holes, p)) {
    | None => None
    | Some((p', ty', ctx, u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Some((NotInHole, p', ctx, u_gen));
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        Some((InHole(TypeInconsistent, u), p', ctx, u_gen));
      }
    }
  | UHPat.Inj(side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p1, ty1)) {
      | None => None
      | Some((p1, ctx, u_gen)) =>
        Some((NotInHole, UHPat.Inj(side, p1), ctx, u_gen))
      };
    | None =>
      switch (syn_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p1)) {
      | None => None
      | Some((p1, ty, ctx, u_gen)) =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        Some((
          InHole(TypeInconsistent, u),
          UHPat.Inj(side, p1),
          ctx,
          u_gen,
        ));
      }
    }
  | UHPat.ListNil =>
    switch (HTyp.matched_list(ty)) {
    | Some(_) => Some((NotInHole, p, ctx, u_gen))
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((InHole(TypeInconsistent, u), p, ctx, u_gen));
    }
  /* | UHPat.ListLit ps ->
     match HTyp.matched_list ty with
     | Some ty_elt ->
       let ps_result =
         List.fold_left (fun opt_result elt ->
           match opt_result with
           | None -> None
           | Some (ps, ctx, u_gen) ->
             match ana_pat_fix_holes ctx u_gen renumber_empty_holes elt ty_elt with
             | None -> None
             | Some (elt, ctx, u_gen) ->
               Some (cons elt ps, ctx, u_gen)
             end
           end) ps (Some ([], ctx, u_gen)) in
       match ps_result with
       | None -> None
       | Some (ps, ctx, u_gen) ->
         Some (NotInHole, UHPat.ListLit ps, ctx, u_gen)
       end
     | None -> None (* TODO should return InHole *)
     end */
  | UHPat.OpSeq(skel, seq) =>
    switch (
      ana_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel, seq, ty)
    ) {
    | None => None
    | Some((Skel.Placeholder(_), _, _, _)) => None
    | Some((Skel.BinOp(err, _, _, _) as skel, seq, ctx, u_gen)) =>
      let p = UHPat.OpSeq(skel, seq);
      Some((err, p, ctx, u_gen));
    }
  }
and ana_skel_pat_fix_holes = (ctx, u_gen, renumber_empty_holes, skel, seq, ty) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => None
    | Some(pn) =>
      switch (UHPat.bidelimited(pn)) {
      | false => None
      | true =>
        switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, pn, ty)) {
        | None => None
        | Some((pn, ctx, u_gen)) =>
          switch (OperatorSeq.seq_update_nth(n, seq, pn)) {
          | Some(seq) => Some((skel, seq, ctx, u_gen))
          | None => None
          }
        }
      }
    }
  | Skel.BinOp(_, UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (
        ana_skel_pat_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel1,
          seq,
          HTyp.Hole,
        )
      ) {
      | None => None
      | Some((skel1, seq, ctx, u_gen)) =>
        switch (
          ana_skel_pat_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            skel2,
            seq,
            HTyp.Hole,
          )
        ) {
        | None => None
        | Some((skel2, seq, ctx, u_gen)) =>
          let skel = Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2);
          Some((skel, seq, ctx, u_gen));
        }
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      switch (HazelUtil.zip_eq(skels, types)) {
      | Some(zipped) =>
        let fixed =
          List.fold_right(
            (skel_ty: (UHPat.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((skels, seq, ctx, u_gen)) =>
                let (skel, ty) = skel_ty;
                switch (
                  ana_skel_pat_fix_holes(
                    ctx,
                    u_gen,
                    renumber_empty_holes,
                    skel,
                    seq,
                    ty,
                  )
                ) {
                | None => None
                | Some((skel, seq, ctx, u_gen)) =>
                  Some(([skel, ...skels], seq, ctx, u_gen))
                };
              },
            zipped,
            Some(([], seq, ctx, u_gen)),
          );
        switch (fixed) {
        | None => None
        | Some((skels, seq, ctx, u_gen)) =>
          switch (UHPat.make_tuple(NotInHole, skels)) {
          | None => None
          | Some(skel) => Some((skel, seq, ctx, u_gen))
          }
        };
      | None =>
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let fixed1 =
          List.fold_right(
            (skel_ty: (UHPat.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((skels, seq, ctx, u_gen)) =>
                let (skel, ty) = skel_ty;
                switch (
                  ana_skel_pat_fix_holes(
                    ctx,
                    u_gen,
                    renumber_empty_holes,
                    skel,
                    seq,
                    ty,
                  )
                ) {
                | None => None
                | Some((skel, seq, ctx, u_gen)) =>
                  Some(([skel, ...skels], seq, ctx, u_gen))
                };
              },
            zipped,
            Some(([], seq, ctx, u_gen)),
          );
        switch (fixed1) {
        | None => None
        | Some((skels1, seq, ctx, u_gen)) =>
          let fixed2 =
            List.fold_right(
              (skel: UHPat.skel_t, opt_result) =>
                switch (opt_result) {
                | None => None
                | Some((skels, seq, ctx, u_gen)) =>
                  switch (
                    syn_skel_pat_fix_holes(
                      ctx,
                      u_gen,
                      renumber_empty_holes,
                      skel,
                      seq,
                    )
                  ) {
                  | None => None
                  | Some((skel, seq, ty, ctx, u_gen)) =>
                    Some(([skel, ...skels], seq, ctx, u_gen))
                  }
                },
              remainder,
              Some(([], seq, ctx, u_gen)),
            );
          switch (fixed2) {
          | None => None
          | Some((skels2, seq, ctx, u_gen)) =>
            let skels = skels1 @ skels2;
            let (u, u_gen) = MetaVarGen.next(u_gen);
            switch (UHPat.make_tuple(InHole(WrongLength, u), skels)) {
            | None => None
            | Some(skel) => Some((skel, seq, ctx, u_gen))
            };
          };
        };
      };
    | _ =>
      switch (
        syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)
      ) {
      | None => None
      | Some((skel1, seq, _, ctx, u_gen)) =>
        switch (
          syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel2, seq)
        ) {
        | None => None
        | Some((skel2, seq, _, ctx, u_gen)) =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let skel =
            Skel.BinOp(
              InHole(TypeInconsistent, u),
              UHPat.Comma,
              skel1,
              skel2,
            );
          Some((skel, seq, ctx, u_gen));
        }
      }
    }
  | Skel.BinOp(_, UHPat.Space, skel1, skel2) =>
    switch (
      ana_skel_pat_fix_holes(
        ctx,
        u_gen,
        renumber_empty_holes,
        skel1,
        seq,
        HTyp.Hole,
      )
    ) {
    | None => None
    | Some((skel1, seq, ctx, u_gen)) =>
      switch (
        ana_skel_pat_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel2,
          seq,
          HTyp.Hole,
        )
      ) {
      | None => None
      | Some((skel2, seq, ctx, u_gen)) =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let skel =
          Skel.BinOp(InHole(TypeInconsistent, u), UHPat.Space, skel1, skel2);
        Some((skel, seq, ctx, u_gen));
      }
    }
  | Skel.BinOp(_, UHPat.Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | Some(ty_elt) =>
      switch (
        ana_skel_pat_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        )
      ) {
      | None => None
      | Some((skel1, seq, ctx, u_gen)) =>
        let ty_list = HTyp.List(ty_elt);
        switch (
          ana_skel_pat_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          )
        ) {
        | None => None
        | Some((skel2, seq, ctx, u_gen)) =>
          let skel = Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2);
          Some((skel, seq, ctx, u_gen));
        };
      }
    | None =>
      switch (
        syn_skel_pat_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)
      ) {
      | None => None
      | Some((skel1, seq, ty_elt, ctx, u_gen)) =>
        let ty_list = HTyp.List(ty_elt);
        switch (
          ana_skel_pat_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          )
        ) {
        | None => None
        | Some((skel2, seq, ctx, u_gen)) =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let skel =
            Skel.BinOp(
              InHole(TypeInconsistent, u),
              UHPat.Cons,
              skel1,
              skel2,
            );
          Some((skel, seq, ctx, u_gen));
        };
      }
    }
  };

/* need to pass a reference to the ana_fix_holes_internal function here
 * rather than defining it mutually to avoid a stack overflow error seemingly
 * related to too many mutually recursive definitions in Coq */
let ana_rule_fix_holes =
    (
      ctx,
      u_gen,
      renumber_empty_holes,
      rule,
      pat_ty,
      clause_ty,
      ana_fix_holes_internal,
    ) => {
  let UHExp.Rule(pat, e) = rule;
  switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, pat, pat_ty)) {
  | None => None
  | Some((pat', ctx, u_gen)) =>
    switch (
      ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e, clause_ty)
    ) {
    | None => None
    | Some((e', u_gen)) => Some((UHExp.Rule(pat', e'), u_gen))
    }
  };
};

/* see above re: ana_fix_holes_internal */
let ana_rules_fix_holes_internal =
    (
      ctx,
      u_gen,
      renumber_empty_holes,
      rules,
      pat_ty,
      clause_ty,
      ana_fix_holes_internal,
    ) =>
  List.fold_right(
    (r, b) =>
      switch (b) {
      | None => None
      | Some((rules, u_gen)) =>
        switch (
          ana_rule_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            r,
            pat_ty,
            clause_ty,
            ana_fix_holes_internal,
          )
        ) {
        | None => None
        | Some((r, u_gen)) => Some(([r, ...rules], u_gen))
        }
      },
    rules,
    Some(([], u_gen)),
  );

/* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
 * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
 * regardless.
 */
let rec syn_fix_holes_internal = (ctx, u_gen, renumber_empty_holes, e) =>
  switch (e) {
  | UHExp.Tm(_, e') =>
    switch (syn_fix_holes'(ctx, u_gen, renumber_empty_holes, e')) {
    | None => None
    | Some((e'', ty, u_gen')) =>
      Some((UHExp.Tm(NotInHole, e''), ty, u_gen'))
    }
  | UHExp.Parenthesized(e1) =>
    switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1)) {
    | None => None
    | Some((e1', ty, u_gen')) =>
      Some((UHExp.Parenthesized(e1'), ty, u_gen'))
    }
  }
and syn_fix_holes' = (ctx, u_gen, renumber_empty_holes, e) =>
  switch (e) {
  | UHExp.EmptyHole(u) =>
    if (renumber_empty_holes) {
      let (u', u_gen'') = MetaVarGen.next(u_gen);
      Some((UHExp.EmptyHole(u'), HTyp.Hole, u_gen''));
    } else {
      Some((UHExp.EmptyHole(u), HTyp.Hole, u_gen));
    }
  | UHExp.Asc(e1, uty) =>
    switch (UHExp.bidelimited(e1)) {
    | false => None
    | true =>
      let ty = UHTyp.expand(uty);
      switch (
        ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1, ty)
      ) {
      | None => None
      | Some((e1', u_gen')) => Some((UHExp.Asc(e1', uty), ty, u_gen'))
      };
    }
  | UHExp.Var(var_err_status, x) =>
    let gamma = Contexts.gamma(ctx);
    switch (VarMap.lookup(gamma, x)) {
    | Some(ty) => Some((UHExp.Var(NotInVHole, x), ty, u_gen))
    | None =>
      switch (var_err_status) {
      | InVHole(_) => Some((e, HTyp.Hole, u_gen))
      | NotInVHole =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        Some((Var(InVHole(u), x), HTyp.Hole, u_gen));
      }
    };
  | UHExp.Lam(p, ann, e1) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };

    switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p, ty1)) {
    | None => None
    | Some((p, ctx1, u_gen)) =>
      switch (syn_fix_holes_internal(ctx1, u_gen, renumber_empty_holes, e1)) {
      | None => None
      | Some((e1, ty2, u_gen)) =>
        Some((UHExp.Lam(p, ann, e1), HTyp.Arrow(ty1, ty2), u_gen))
      }
    };
  | UHExp.LineItem(li, e1) =>
    switch (syn_fix_holes_line_item(ctx, u_gen, renumber_empty_holes, li)) {
    | None => None
    | Some((li, ctx, u_gen)) =>
      switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1)) {
      | None => None
      | Some((e1, ty, u_gen)) => Some((UHExp.LineItem(li, e1), ty, u_gen))
      }
    }
  | NumLit(_) => Some((e, HTyp.Num, u_gen))
  | BoolLit(_) => Some((e, HTyp.Bool, u_gen))
  | ListNil => Some((e, HTyp.List(HTyp.Hole), u_gen))
  /* | ListLit es ->
     let opt_result = List.fold_left (fun opt_result e ->
       match opt_result with
       | None -> None
       | Some (es, ty, u_gen) ->
         match syn_fix_holes_internal ctx u_gen renumber_empty_holes e with
         | Some (e, ty', u_gen) ->
           match HTyp.join ty ty' with
           | Some ty_joined -> Some (cons e es, ty_joined, u_gen)
           | None ->
             match ana_fix_holes_internal ctx u_gen renumber_empty_holes e ty with
             | None -> None
             | Some (e, u_gen) -> Some (cons e es, ty, u_gen)
             end
           end
         | None ->
           match ana_fix_holes_internal ctx u_gen renumber_empty_holes e ty with
           | None -> None
           | Some (e, u_gen) -> Some (cons e es, ty, u_gen)
           end
         end
       end) es (Some ([], HTyp.Hole, u_gen)) in
     match opt_result with
     | None -> None
     | Some (es, ty, u_gen) ->
       Some (ListLit es, HTyp.List ty, u_gen)
     end */
  | OpSeq(skel, seq) =>
    switch (syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel, seq)) {
    | None => None
    | Some((Skel.Placeholder(_), _, _, _)) => None
    | Some((skel, seq, ty, u_gen)) => Some((OpSeq(skel, seq), ty, u_gen))
    }
  | UHExp.Inj(side, e1) =>
    switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1)) {
    | None => None
    | Some((e1', ty1, u_gen')) =>
      let e' = UHExp.Inj(side, e1');
      let ty' =
        switch (side) {
        | L => HTyp.Sum(ty1, HTyp.Hole)
        | R => HTyp.Sum(HTyp.Hole, ty1)
        };

      Some((e', ty', u_gen'));
    }
  | Case(_, _) => None
  | ApPalette(name, serialized_model, psi) =>
    let palette_ctx = Contexts.palette_ctx(ctx);
    switch (PaletteCtx.lookup(palette_ctx, name)) {
    | None => None /* TODO invalid palette name hole */
    | Some(palette_defn) =>
      switch (
        ana_fix_holes_splice_map(
          ctx,
          u_gen,
          renumber_empty_holes,
          SpliceInfo.splice_map(psi),
        )
      ) {
      | None => None
      | Some((splice_map, u_gen)) =>
        let psi = SpliceInfo.update_splice_map(psi, splice_map);
        let expansion_ty = palette_defn.expansion_ty;
        Some((ApPalette(name, serialized_model, psi), expansion_ty, u_gen));
      }
    };
  }
and syn_fix_holes_line_item = (ctx, u_gen, renumber_empty_holes, li) =>
  switch (li) {
  | EmptyLine => Some((li, ctx, u_gen))
  | ExpLine(e) =>
    switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e)) {
    | None => None
    | Some((e, _, u_gen)) => Some((ExpLine(e), ctx, u_gen))
    }
  | LetLine(p, ann, e1) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = ctx_for_let(ctx, p, ty1, e1);
      switch (
        ana_fix_holes_internal(ctx1, u_gen, renumber_empty_holes, e1, ty1)
      ) {
      | None => None
      | Some((e1, u_gen)) =>
        switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p, ty1)) {
        | None => None
        | Some((p, ctx, u_gen)) => Some((LetLine(p, ann, e1), ctx, u_gen))
        }
      };
    | None =>
      switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1)) {
      | None => None
      | Some((e1, ty1, u_gen)) =>
        switch (ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p, ty1)) {
        | None => None
        | Some((p, ctx, u_gen)) => Some((LetLine(p, ann, e1), ctx, u_gen))
        }
      }
    }
  }
and ana_fix_holes_splice_map = (ctx, u_gen, renumber_empty_holes, splice_map) =>
  NatMap.fold(
    splice_map,
    (c, (splice_name, (ty, e))) =>
      switch (c) {
      | None => None
      | Some((splice_map, u_gen)) =>
        switch (
          ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e, ty)
        ) {
        | None => None
        | Some((e, u_gen)) =>
          let splice_map =
            NatMap.extend_unique(splice_map, (splice_name, (ty, e)));
          Some((splice_map, u_gen));
        }
      },
    Some((splice_map, u_gen)),
  )
and ana_fix_holes_internal = (ctx, u_gen, renumber_empty_holes, e, ty) =>
  switch (e) {
  | UHExp.Tm(_, e1) =>
    switch (ana_fix_holes'(ctx, u_gen, renumber_empty_holes, e1, ty)) {
    | None => None
    | Some((err_status, e1, u_gen)) =>
      Some((UHExp.Tm(err_status, e1), u_gen))
    }
  | UHExp.Parenthesized(e1) =>
    switch (ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1, ty)) {
    | None => None
    | Some((e1, u_gen)) => Some((UHExp.Parenthesized(e1), u_gen))
    }
  }
and ana_fix_holes' = (ctx, u_gen, renumber_empty_holes, e, ty) =>
  switch (e) {
  | UHExp.LineItem(li, e1) =>
    switch (syn_fix_holes_line_item(ctx, u_gen, renumber_empty_holes, li)) {
    | None => None
    | Some((li, ctx, u_gen)) =>
      switch (
        ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1, ty)
      ) {
      | None => None
      | Some((e1, u_gen)) =>
        Some((NotInHole, UHExp.LineItem(li, e1), u_gen))
      }
    }
  | UHExp.Lam(p, ann, e1) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        if (HTyp.consistent(ty1_ann, ty1_given)) {
          switch (
            ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p, ty1_ann)
          ) {
          | None => None
          | Some((p, ctx, u_gen)) =>
            switch (
              ana_fix_holes_internal(
                ctx,
                u_gen,
                renumber_empty_holes,
                e1,
                ty2,
              )
            ) {
            | None => None
            | Some((e1, u_gen)) =>
              Some((NotInHole, UHExp.Lam(p, ann, e1), u_gen))
            }
          };
        } else {
          switch (syn_fix_holes'(ctx, u_gen, renumber_empty_holes, e)) {
          | None => None
          | Some((e, ty, u_gen)) =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            Some((InHole(TypeInconsistent, u), e, u_gen));
          };
        };
      | None =>
        switch (
          ana_pat_fix_holes(ctx, u_gen, renumber_empty_holes, p, ty1_given)
        ) {
        | None => None
        | Some((p, ctx, u_gen)) =>
          switch (
            ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1, ty2)
          ) {
          | None => None
          | Some((e1, u_gen)) =>
            Some((NotInHole, UHExp.Lam(p, ann, e1), u_gen))
          }
        }
      }
    | None =>
      switch (syn_fix_holes'(ctx, u_gen, renumber_empty_holes, e)) {
      | None => None
      | Some((e, ty', u_gen)) =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        Some((InHole(TypeInconsistent, u), e, u_gen));
      }
    }
  | UHExp.Inj(side, e1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((ty1, ty2)) =>
      switch (
        ana_fix_holes_internal(
          ctx,
          u_gen,
          renumber_empty_holes,
          e1,
          pick_side(side, ty1, ty2),
        )
      ) {
      | None => None
      | Some((e1', u_gen')) =>
        Some((NotInHole, UHExp.Inj(side, e1'), u_gen'))
      }
    | None =>
      switch (syn_fix_holes'(ctx, u_gen, renumber_empty_holes, e)) {
      | None => None
      | Some((e', ty', u_gen')) =>
        if (HTyp.consistent(ty, ty')) {
          Some((NotInHole, e', u_gen'));
        } else {
          let (u, u_gen'') = MetaVarGen.next(u_gen');
          Some((InHole(TypeInconsistent, u), e', u_gen''));
        }
      }
    }
  | ListNil =>
    switch (HTyp.matched_list(ty)) {
    | Some(_) => Some((NotInHole, e, u_gen))
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((InHole(TypeInconsistent, u), e, u_gen));
    }
  /* | ListLit es ->
     match HTyp.matched_list ty with
     | Some ty_elt ->
       let opt_es = List.fold_left (fun opt_result elt ->
         match opt_result with
         | None -> None
         | Some (es, u_gen) ->
           match ana_fix_holes_internal ctx u_gen renumber_empty_holes elt ty_elt with
           | None -> None
           | Some (elt, u_gen) ->
             Some (cons elt es, u_gen)
           end
         end) es (Some ([], u_gen)) in
       match opt_es with
       | None -> None
       | Some (es, u_gen) -> Some (NotInHole, ListLit es, u_gen)
       end
     | None -> None (* TODO put in hole if not a list *)
     end */
  | Case(e1, rules) =>
    switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, e1)) {
    | None => None
    | Some((e1', ty1, u_gen)) =>
      switch (
        ana_rules_fix_holes_internal(
          ctx,
          u_gen,
          renumber_empty_holes,
          rules,
          ty1,
          ty,
          ana_fix_holes_internal,
        )
      ) {
      | None => None
      | Some((rules', u_gen)) => Some((NotInHole, Case(e1', rules'), u_gen))
      }
    }
  | OpSeq(skel, seq) =>
    switch (
      ana_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel, seq, ty)
    ) {
    | None => None
    | Some((Skel.Placeholder(_), _, _)) => None
    | Some((Skel.BinOp(err, _, _, _) as skel, seq, u_gen)) =>
      Some((err, OpSeq(skel, seq), u_gen))
    }
  | UHExp.EmptyHole(_)
  | UHExp.Asc(_, _)
  | UHExp.Var(_, _)
  | NumLit(_)
  | BoolLit(_)
  | ApPalette(_, _, _) =>
    switch (syn_fix_holes'(ctx, u_gen, renumber_empty_holes, e)) {
    | None => None
    | Some((e', ty', u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Some((NotInHole, e', u_gen));
      } else {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        Some((InHole(TypeInconsistent, u), e', u_gen));
      }
    }
  }
and syn_skel_fix_holes = (ctx, u_gen, renumber_empty_holes, skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => None
    | Some(en) =>
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (syn_fix_holes_internal(ctx, u_gen, renumber_empty_holes, en)) {
        | None => None
        | Some((en, ty, u_gen)) =>
          switch (OperatorSeq.seq_update_nth(n, seq, en)) {
          | None => None
          | Some(seq) => Some((skel, seq, ty, u_gen))
          }
        }
      }
    }
  | Skel.BinOp(_, Plus as op, skel1, skel2)
  | Skel.BinOp(_, Times as op, skel1, skel2) =>
    switch (
      ana_skel_fix_holes(
        ctx,
        u_gen,
        renumber_empty_holes,
        skel1,
        seq,
        HTyp.Num,
      )
    ) {
    | Some((skel1, seq, u_gen)) =>
      switch (
        ana_skel_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel2,
          seq,
          HTyp.Num,
        )
      ) {
      | Some((skel2, seq, u_gen)) =>
        Some((Skel.BinOp(NotInHole, op, skel1, skel2), seq, HTyp.Num, u_gen))
      | None => None
      }
    | None => None
    }
  | Skel.BinOp(_, LessThan as op, skel1, skel2) =>
    switch (
      ana_skel_fix_holes(
        ctx,
        u_gen,
        renumber_empty_holes,
        skel1,
        seq,
        HTyp.Num,
      )
    ) {
    | Some((skel1, seq, u_gen)) =>
      switch (
        ana_skel_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel2,
          seq,
          HTyp.Num,
        )
      ) {
      | Some((skel2, seq, u_gen)) =>
        Some((
          Skel.BinOp(NotInHole, op, skel1, skel2),
          seq,
          HTyp.Bool,
          u_gen,
        ))
      | None => None
      }
    | None => None
    }
  | Skel.BinOp(_, Space, skel1, skel2) =>
    switch (syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)) {
    | Some((skel1', seq1, ty1, u_gen1)) =>
      switch (HTyp.matched_arrow(ty1)) {
      | Some((ty2, ty)) =>
        switch (
          ana_skel_fix_holes(
            ctx,
            u_gen1,
            renumber_empty_holes,
            skel2,
            seq1,
            ty2,
          )
        ) {
        | Some((skel2', seq2, u_gen2)) =>
          Some((
            Skel.BinOp(NotInHole, Space, skel1', skel2'),
            seq2,
            ty,
            u_gen2,
          ))
        | None => None
        }
      | None =>
        switch (
          ana_skel_fix_holes(
            ctx,
            u_gen1,
            renumber_empty_holes,
            skel2,
            seq1,
            HTyp.Hole,
          )
        ) {
        | Some((skel2', seq2, u_gen2)) =>
          switch (UHExp.make_skel_inconsistent(u_gen2, skel1', seq2)) {
          | Some((skel1'', seq3, u_gen3)) =>
            Some((
              Skel.BinOp(NotInHole, Space, skel1'', skel2'),
              seq3,
              HTyp.Hole,
              u_gen3,
            ))
          | None => None
          }
        | None => None
        }
      }
    | None => None
    }
  | Skel.BinOp(_, UHExp.Comma, skel1, skel2) =>
    switch (syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)) {
    | None => None
    | Some((skel1, seq, ty1, u_gen)) =>
      switch (
        syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel2, seq)
      ) {
      | None => None
      | Some((skel2, seq, ty2, u_gen)) =>
        let skel = Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2);
        let ty = HTyp.Prod(ty1, ty2);
        Some((skel, seq, ty, u_gen));
      }
    }
  | Skel.BinOp(_, UHExp.Cons, skel1, skel2) =>
    switch (syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)) {
    | None => None
    | Some((skel1, seq, ty_elt, u_gen)) =>
      let ty = HTyp.List(ty_elt);
      switch (
        ana_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel2, seq, ty)
      ) {
      | None => None
      | Some((skel2, seq, u_gen)) =>
        let skel = Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2);
        Some((skel, seq, ty, u_gen));
      };
    }
  }
and ana_skel_fix_holes = (ctx, u_gen, renumber_empty_holes, skel, seq, ty) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => None
    | Some(en) =>
      switch (UHExp.bidelimited(en)) {
      | false => None
      | true =>
        switch (
          ana_fix_holes_internal(ctx, u_gen, renumber_empty_holes, en, ty)
        ) {
        | None => None
        | Some((en, u_gen)) =>
          switch (OperatorSeq.seq_update_nth(n, seq, en)) {
          | Some(seq) => Some((skel, seq, u_gen))
          | None => None
          }
        }
      }
    }
  | Skel.BinOp(_, UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (
        ana_skel_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel1,
          seq,
          HTyp.Hole,
        )
      ) {
      | None => None
      | Some((skel1, seq, u_gen)) =>
        switch (
          ana_skel_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            skel2,
            seq,
            HTyp.Hole,
          )
        ) {
        | None => None
        | Some((skel2, seq, u_gen)) =>
          let skel = Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2);
          Some((skel, seq, u_gen));
        }
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      switch (HazelUtil.zip_eq(skels, types)) {
      | Some(zipped) =>
        let fixed =
          List.fold_right(
            (skel_ty: (UHExp.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((skels, seq, u_gen)) =>
                let (skel, ty) = skel_ty;
                switch (
                  ana_skel_fix_holes(
                    ctx,
                    u_gen,
                    renumber_empty_holes,
                    skel,
                    seq,
                    ty,
                  )
                ) {
                | None => None
                | Some((skel, seq, u_gen)) =>
                  Some(([skel, ...skels], seq, u_gen))
                };
              },
            zipped,
            Some(([], seq, u_gen)),
          );
        switch (fixed) {
        | None => None
        | Some((skels, seq, u_gen)) =>
          switch (UHExp.make_tuple(NotInHole, skels)) {
          | None => None
          | Some(skel) => Some((skel, seq, u_gen))
          }
        };
      | None =>
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let fixed1 =
          List.fold_right(
            (skel_ty: (UHExp.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((skels, seq, u_gen)) =>
                let (skel, ty) = skel_ty;
                switch (
                  ana_skel_fix_holes(
                    ctx,
                    u_gen,
                    renumber_empty_holes,
                    skel,
                    seq,
                    ty,
                  )
                ) {
                | None => None
                | Some((skel, seq, u_gen)) =>
                  Some(([skel, ...skels], seq, u_gen))
                };
              },
            zipped,
            Some(([], seq, u_gen)),
          );
        switch (fixed1) {
        | None => None
        | Some((skels1, seq, u_gen)) =>
          let fixed2 =
            List.fold_right(
              (skel: UHExp.skel_t, opt_result) =>
                switch (opt_result) {
                | None => None
                | Some((skels, seq, u_gen)) =>
                  switch (
                    syn_skel_fix_holes(
                      ctx,
                      u_gen,
                      renumber_empty_holes,
                      skel,
                      seq,
                    )
                  ) {
                  | None => None
                  | Some((skel, seq, ty, u_gen)) =>
                    Some(([skel, ...skels], seq, u_gen))
                  }
                },
              remainder,
              Some(([], seq, u_gen)),
            );
          switch (fixed2) {
          | None => None
          | Some((skels2, seq, u_gen)) =>
            let skels = skels1 @ skels2;
            let (u, u_gen) = MetaVarGen.next(u_gen);
            switch (UHExp.make_tuple(InHole(WrongLength, u), skels)) {
            | None => None
            | Some(skel) => Some((skel, seq, u_gen))
            };
          };
        };
      };
    | _ =>
      switch (
        syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)
      ) {
      | None => None
      | Some((skel1, seq, _, u_gen)) =>
        switch (
          syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel2, seq)
        ) {
        | None => None
        | Some((skel2, seq, _, u_gen)) =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let skel =
            Skel.BinOp(
              InHole(TypeInconsistent, u),
              UHExp.Comma,
              skel1,
              skel2,
            );
          Some((skel, seq, u_gen));
        }
      }
    }
  | Skel.BinOp(_, UHExp.Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | Some(ty_elt) =>
      switch (
        ana_skel_fix_holes(
          ctx,
          u_gen,
          renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        )
      ) {
      | None => None
      | Some((skel1, seq, u_gen)) =>
        let ty_list = HTyp.List(ty_elt);
        switch (
          ana_skel_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          )
        ) {
        | None => None
        | Some((skel2, seq, u_gen)) =>
          let skel = Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2);
          Some((skel, seq, u_gen));
        };
      }
    | None =>
      switch (
        syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel1, seq)
      ) {
      | None => None
      | Some((skel1, seq, ty_elt, u_gen)) =>
        let ty_list = HTyp.List(ty_elt);
        switch (
          ana_skel_fix_holes(
            ctx,
            u_gen,
            renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          )
        ) {
        | None => None
        | Some((skel2, seq, u_gen)) =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let skel =
            Skel.BinOp(
              InHole(TypeInconsistent, u),
              UHExp.Cons,
              skel1,
              skel2,
            );
          Some((skel, seq, u_gen));
        };
      }
    }
  | Skel.BinOp(_, Plus, _, _)
  | Skel.BinOp(_, Times, _, _)
  | Skel.BinOp(_, LessThan, _, _)
  | Skel.BinOp(_, Space, _, _) =>
    switch (syn_skel_fix_holes(ctx, u_gen, renumber_empty_holes, skel, seq)) {
    | Some((skel', seq', ty', u_gen')) =>
      if (HTyp.consistent(ty, ty')) {
        Some((skel', seq', u_gen'));
      } else {
        UHExp.make_skel_inconsistent(u_gen', skel', seq');
      }
    | None => None
    }
  };

let syn_fix_holes = (ctx, u_gen, e) =>
  syn_fix_holes_internal(ctx, u_gen, false, e);

let ana_fix_holes = (ctx, u_gen, e, ty) =>
  ana_fix_holes_internal(ctx, u_gen, false, e, ty);

let ana_rules_fix_holes =
    (ctx, u_gen, renumber_empty_holes, rules, pat_ty, clause_ty) =>
  ana_rules_fix_holes_internal(
    ctx,
    u_gen,
    renumber_empty_holes,
    rules,
    pat_ty,
    clause_ty,
    ana_fix_holes_internal,
  );

/* Only to be used on top-level expressions, as it starts hole renumbering at 0 */
let fix_and_renumber_holes = (ctx, e) =>
  syn_fix_holes_internal(ctx, MetaVarGen.init, true, e);
