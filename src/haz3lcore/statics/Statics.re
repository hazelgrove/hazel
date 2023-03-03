open Term;

/* STATICS

     This module determines the statics semantics of the language.
     It takes a term and returns a map which associates the unique
     ids of each term to an Info.t data structure which reflects that
     term's statics. The statics collected depend on the term's sort,
     but every term has a syntactic class (The cls types from Term),
     except Invalid terms which Term could not parse.

     The map generated by this module is intended to be generated once
     from a given term and then reused anywhere there is logic which
     depends on static information.
   */

module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);
};

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let extend_let_def_ctx =
    (ctx: Ctx.t, pat: UPat.t, pat_ctx: Ctx.t, def: UExp.t): Ctx.t =>
  if (UPat.is_tuple_of_arrows(pat) && UExp.is_tuple_of_functions(def)) {
    pat_ctx;
  } else {
    ctx;
  };

let typ_exp_binop_bin_int: UExp.op_bin_int => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Int
  | (LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals) as _op =>
    Bool;

let typ_exp_binop_bin_float: UExp.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Float
  | (LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals) as _op =>
    Bool;

let typ_exp_binop_bin_string: UExp.op_bin_string => Typ.t =
  fun
  | Equals as _op => Bool;

let typ_exp_binop: UExp.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Bool, Bool, Bool)
  | Int(op) => (Int, Int, typ_exp_binop_bin_int(op))
  | Float(op) => (Float, Float, typ_exp_binop_bin_float(op))
  | String(op) => (String, String, typ_exp_binop_bin_string(op));

let typ_exp_unop: UExp.op_un => (Typ.t, Typ.t) =
  fun
  | Int(Minus) => (Int, Int);

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: any, m: Map.t): (Ctx.co, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let (Info.{free, _}, m) = uexp_to_info_map(~ctx, ~ancestors, e, m);
    (free, m);
  | Pat(p) =>
    let m =
      upat_to_info_map(
        ~mode=Typ.Ana(Unknown(SynSwitch)),
        ~is_synswitch=false,
        ~pat_form=Info.Solo,
        ~ancestors,
        ~ctx,
        p,
        m,
      )
      |> snd;
    (VarMap.empty, m);
  | TPat(tp) => (
      VarMap.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      VarMap.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
    )
  | Rul(_)
  | Nul ()
  | Any () => (VarMap.empty, m)
  }
and multi = (~ctx, ~ancestors, m, tms) =>
  List.fold_left(
    ((frees, m), any) => {
      let (free, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (frees @ [free], m);
    },
    ([], m),
    tms,
  )
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~mode=Typ.Ana(Unknown(SynSwitch)),
      ~ancestors,
      {ids, term} as uexp: UExp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  let add' = (~self, ~free, m) => {
    let info = Info.derived_exp(~uexp, ~ctx, ~mode, ~ancestors, ~self, ~free);
    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~free, m) => add'(~self=Common(self), ~free, m);
  let ancestors = [UExp.rep_id(uexp)] @ ancestors;
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let map_m_go = m =>
    List.fold_left2(
      ((es, m), mode, e) =>
        go(~mode, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let atomic = self => add(~self, ~free=[], m);
  switch (term) {
  | MultiHole(tms) =>
    let (frees, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~free=Ctx.union(frees), m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(Unknown(EmptyExp)))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(EmptyList))))
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let modes = Typ.matched_list_lit_modes(mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=Info.join(ty => List(ty), tys, ids, ctx),
      ~free=Ctx.union(List.map(Info.exp_free, es)),
      m,
    );
  | Cons(e1, e2) =>
    let (e1, m) = go(~mode=Typ.matched_list_mode(mode), e1, m);
    let (e2, m) = go(~mode=Ana(List(e1.ty)), e2, m);
    add(~self=Just(List(e1.ty)), ~free=Ctx.union([e1.free, e2.free]), m);
  | Var(name) =>
    add'(
      ~self=Info.self_var(ctx, name),
      ~free=[(name, [{id: UExp.rep_id(uexp), mode}])],
      m,
    )
  | Parens(e) =>
    let (e, m) = go(~mode, e, m);
    add(~self=Just(e.ty), ~free=e.free, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=Ana(ty_in), e, m);
    add(~self=Just(ty_out), ~free=e.free, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m) = go(~mode=Ana(ty1), e1, m);
    let (e2, m) = go(~mode=Ana(ty2), e2, m);
    add(~self=Just(ty_out), ~free=Ctx.union([e1.free, e2.free]), m);
  | Tuple(es) =>
    let modes = Typ.matched_prod_modes(mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=Just(Prod(List.map(Info.exp_ty, es))),
      ~free=Ctx.union(List.map(Info.exp_free, es)),
      m,
    );
  | Test(e) =>
    let (e, m) = go(~mode=Ana(Bool), e, m);
    add(~self=Just(Prod([])), ~free=e.free, m);
  | Seq(e1, e2) =>
    let (e1, m) = go(~mode=Ana(Unknown(SynSwitch)), e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(~self=Just(e2.ty), ~free=Ctx.union([e1.free, e2.free]), m);
  | Tag(tag) => atomic(Info.self_tag(ctx, tag))
  | Ap(fn, arg) =>
    let fn_mode = Typ.ap_mode(ctx, mode, UExp.tag_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m) = go(~mode=Ana(ty_in), arg, m);
    add(~self=Just(ty_out), ~free=Ctx.union([fn.free, arg.free]), m);
  | Fun(p, e) =>
    let (mode_pat, mode_body) = Typ.matched_arrow_mode(mode);
    let (p, m) =
      go_pat(~is_synswitch=true, ~mode=mode_pat, ~pat_form=Solo, p, m);
    let (e, m) = go'(~ctx=p.ctx, ~mode=mode_body, e, m);
    add(
      ~self=Just(Arrow(p.ty, e.ty)),
      ~free=Ctx.free_in(ctx, p.ctx, e.free),
      m,
    );
  | Let(p, def, body) =>
    //NOTE(andrew): switched source of CI for pat
    let (p_syn, m) =
      go_pat(
        ~is_synswitch=true,
        ~pat_form=Info.Solo,
        ~mode=Ana(Unknown(SynSwitch)),
        p,
        m,
      );
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def, m);
    /* Analyze pattern to incorporate def type into ctx */
    let (p_ana, _m) =
      go_pat(
        ~is_synswitch=false,
        ~pat_form=Info.Solo,
        ~mode=Ana(def.ty),
        p,
        m,
      );
    let (body, m) = go'(~ctx=p_ana.ctx, ~mode, body, m);
    add(
      ~self=Just(body.ty),
      ~free=Ctx.union([def.free, Ctx.free_in(ctx, p_ana.ctx, body.free)]),
      m,
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(UExp.rep_id, [e1, e2]);
    let (cond, m) = go(~mode=Ana(Bool), e0, m);
    let (cons, m) = go(~mode, e1, m);
    let (alt, m) = go(~mode, e2, m);
    add(
      ~self=Info.join(Fun.id, [cons.ty, alt.ty], branch_ids, ctx),
      ~free=Ctx.union([cond.free, cons.free, alt.free]),
      m,
    );
  | Match(scrut, rules) =>
    let (scrut, m) = go(~mode=Ana(Unknown(SynSwitch)), scrut, m);
    let (ps, es) = List.split(rules);
    let branch_ids = List.map(UExp.rep_id, es);
    let (ps, m) =
      map_m(
        go_pat(
          ~is_synswitch=false,
          ~mode=Typ.Ana(scrut.ty),
          ~pat_form=Branch,
        ),
        ps,
        m,
      );
    let p_ctxs = List.map(Info.pat_ctx, ps);
    let (es, m) =
      List.fold_left2(
        ((es, m), e, ctx) =>
          go'(~ctx, ~mode, e, m) |> (((e, m)) => (es @ [e], m)),
        ([], m),
        es,
        p_ctxs,
      );
    let e_tys = List.map(Info.exp_ty, es);
    let e_frees =
      List.map2(Ctx.free_in(ctx), p_ctxs, List.map(Info.exp_free, es));
    add(
      ~self=Info.join(Fun.id, e_tys, branch_ids, ctx),
      ~free=Ctx.union([scrut.free] @ e_frees),
      m,
    );
  | TyAlias(typat, utyp, body) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    switch (typat.term) {
    | Var(name) =>
      /* NOTE(andrew): This is a slightly dicey piece of logic, debatably
         errors cancelling out. Right now, to_typ returns Unknown(TypeHole)
         for any type variable reference not in its ctx. So any free variables
         in the definition would be oblierated. But we need to check for free
         variables to decide whether to make a recursive type or not. So we
         tentatively add an abtract type to the ctx, representing the
         speculative rec parameter. */
      let (ty_def, ctx_def, ctx_body) = {
        let ty_pre =
          UTyp.to_typ(Ctx.add_abstract(ctx, name, Id.invalid), utyp);
        switch (utyp.term) {
        | USum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
          let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ctx_def =
            Ctx.add_alias(ctx, name, UTPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ =>
          let ty = UTyp.to_typ(ctx, utyp);
          (ty, ctx, Ctx.add_alias(ctx, name, UTPat.rep_id(typat), ty));
        };
      };
      let ctx_body =
        switch (Typ.get_sum_tags(ctx, ty_def)) {
        | Some(sm) => Ctx.add_tags(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let (Info.{free, ty: ty_body, _}, m) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_escape), ~free, m);
    | _ =>
      let (Info.{free, ty: ty_body, _}, m) = go'(~ctx, ~mode, body, m);
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_body), ~free, m);
    };
  };
}
and upat_to_info_map =
    (
      ~is_synswitch=true,
      ~pat_form: Info.pat_form,
      ~ctx,
      ~ancestors: Info.ancestors,
      ~mode,
      {ids, term} as upat: UPat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add' = (~self, ~ctx, m) => {
    let info = Info.derived_pat(~upat, ~ctx, ~mode, ~ancestors, ~self);
    (info, add_info(ids, InfoPat(info), m));
  };
  let add = (~self, ~ctx, m) => {
    add'(
      ~self=pat_form == Solo ? SoloPos(upat, self) : Common(self),
      ~ctx,
      m,
    );
  };
  let atomic = self => add(~self, ~ctx, m);
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~pat_form, ~is_synswitch, ~ancestors);
  let ctx_fold = (ctx: Ctx.t, m) =>
    List.fold_left2(
      ((ctx, tys, m), e, mode) =>
        go(~ctx, ~mode, e, m)
        |> (((info, m)) => (info.ctx, tys @ [info.ty], m)),
      (ctx, [], m),
    );
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~ctx, m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(Unknown(EmptyPat)))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(EmptyList))))
  | ListLit(ps) =>
    let modes = Typ.matched_list_lit_modes(mode, List.length(ps));
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(
      ~self=Info.join(ty => List(ty), tys, List.map(UPat.rep_id, ps), ctx),
      ~ctx,
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~ctx, ~mode=Typ.matched_list_mode(mode), hd, m);
    let (tl, m) = go(~ctx=hd.ctx, ~mode=Ana(List(hd.ty)), tl, m);
    add(~self=Just(List(hd.ty)), ~ctx=tl.ctx, m);
  | Wild => atomic(Just(Unknown(PatVar)))
  | Var(name) =>
    /* Note the self type assigned to pattern variables (unknown)
       may be SynSwitch, but the type we add to the context is
       always Unknown Intern... NOT NO MORREEEE!!! */
    let ctx_typ =
      Info.ty_after_fix_pat(ctx, mode, Common(Just(Unknown(PatVar))));
    let entry = Ctx.VarEntry({name, id: UPat.rep_id(upat), typ: ctx_typ});
    add'(
      ~self=PatVar(Just(Unknown(PatVar)), name),
      ~ctx=Ctx.extend(entry, ctx),
      m,
    );
  | Tuple(ps) =>
    let modes = Typ.matched_prod_modes(mode, List.length(ps));
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Just(Prod(tys)), ~ctx, m);
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p, m);
    add(~self=Just(p.ty), ~ctx=p.ctx, m);
  | Tag(tag) => atomic(Info.self_tag(ctx, tag))
  | Ap(fn, arg) =>
    let fn_mode = Typ.ap_mode(ctx, mode, UPat.tag_name(fn));
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m) = go(~ctx, ~mode=Ana(ty_in), arg, m);
    add(~self=Just(ty_out), ~ctx=arg.ctx, m);
  | TypeAnn(p, ann) =>
    let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
    let (p, m) = go(~ctx, ~mode=Ana(ann.ty), p, m);
    add(~self=Just(ann.ty), ~ctx=p.ctx, m);
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=Info.TypeExpected,
      ~ancestors,
      {ids, term} as utyp: UTyp.t,
      m: Map.t,
    )
    : (Info.typ, Map.t) => {
  let add = m => {
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
  };
  let ancestors = [UTyp.rep_id(utyp)] @ ancestors;
  let go' = utyp_to_info_map(~ctx, ~ancestors);
  let go = go'(~expects=TypeExpected);
  //TODO(andrew): make this return free, replacing Typ.free_vars
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Int
  | Float
  | Bool
  | String => add(m)
  | Var(_)
  | Tag(_) =>
    /* Names are resolved in Info.status_typ */
    add(m)
  | List(t)
  | Parens(t) => add(go(t, m) |> snd)
  | Arrow(t1, t2) =>
    let m = go(t1, m) |> snd;
    let m = go(t2, m) |> snd;
    add(m);
  | Tuple(ts) =>
    let m = map_m(go, ts, m) |> snd;
    add(m);
  | Ap(t1, t2) =>
    let ty_in = UTyp.to_typ(ctx, t2);
    let t1_mode: Info.typ_expects =
      switch (expects) {
      | VariantExpected(m, sum_ty) => TagExpected(m, Arrow(ty_in, sum_ty))
      | _ => TagExpected(Unique, Arrow(ty_in, Unknown(TagShit)))
      };
    let m = go'(~expects=t1_mode, t1, m) |> snd;
    let m = go'(~expects=TypeExpected, t2, m) |> snd;
    add(m);
  | USum(ts) =>
    let (m, _) =
      List.fold_left(
        ((m, tags), uty) => {
          let (status, tag) =
            switch (UTyp.get_tag(ctx, uty)) {
            | None => (Info.Unique, [])
            | Some(tag) when !List.mem(tag, tags) => (Unique, [tag])
            | Some(tag) => (Duplicate, [tag])
            };
          let ty_sum = UTyp.to_typ(ctx, utyp);
          let m =
            go'(~expects=VariantExpected(status, ty_sum), uty, m) |> snd;
          (m, tags @ tag);
        },
        (m, []),
        ts,
      );
    add(m);
  };
}
and utpat_to_info_map =
    (~ctx, ~ancestors, {ids, term} as utpat: UTPat.t, m: Map.t)
    : (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [UTPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
  };
};

let mk_map =
  Core.Memo.general(~cache_size_bound=1000, e => {
    uexp_to_info_map(
      ~ctx=Builtins.ctx(Builtins.Pervasives.builtins),
      ~ancestors=[],
      e,
      Id.Map.empty,
    )
    |> snd
  });
