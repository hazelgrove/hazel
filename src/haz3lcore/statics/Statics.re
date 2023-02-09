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

let union_m = List.fold_left(Id.Map.disj_union, Id.Map.empty);

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids
  |> List.map(id => Id.Map.singleton(id, info))
  |> List.fold_left(Id.Map.disj_union, m);

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

let joint_self =
    (wrap: Typ.t => Typ.t, tys: list(Typ.t), ids: list(Id.t), ctx: Ctx.t)
    : Info.self_common =>
  switch (Typ.join_all(ctx, tys)) {
  | None => Info.NoJoin(List.map2((id, ty) => Info.{id, ty}, ids, tys))
  | Some(ty) => Just(wrap(ty))
  };

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: any): (Ctx.co, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let (Info.{free, _}, m) = uexp_to_info_map(~ctx, ~ancestors, e);
    (free, m);
  | Pat(p) =>
    let m = upat_to_info_map(~is_synswitch=false, ~ancestors, ~ctx, p) |> snd;
    (VarMap.empty, m);
  | TPat(tp) => (
      VarMap.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp) |> snd,
    )
  | Typ(ty) => (VarMap.empty, utyp_to_info_map(~ctx, ~ancestors, ty) |> snd)
  | Rul(_)
  | Nul ()
  | Any () => (VarMap.empty, Id.Map.empty)
  }
and uexp_to_info_map =
    (~ctx: Ctx.t, ~mode=Typ.Syn, ~ancestors, {ids, term} as uexp: UExp.t)
    : (Info.exp, Map.t) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(Unknown(SynSwitch)) => Typ.Syn
    | _ => mode
    };
  let add' = (~self, ~free, m) => {
    let info = Info.derived_exp(~uexp, ~ctx, ~mode, ~ancestors, ~self, ~free);
    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~free, m) => add'(~self=Common(self), ~free, m);
  let ancestors = [UExp.rep_id(uexp)] @ ancestors;
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let atomic = self => add(~self, ~free=[], Id.Map.empty);
  switch (term) {
  | Invalid(token) => atomic(BadToken(token))
  | MultiHole(tms) =>
    let (frees, ms) =
      tms |> List.map(any_to_info_map(~ctx, ~ancestors)) |> List.split;
    add(~self=IsMulti, ~free=Ctx.union(frees), union_m(ms));
  | EmptyHole => atomic(Just(Unknown(Internal)))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(Internal))))
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let modes = List.init(List.length(es), _ => Typ.matched_list_mode(mode));
    let (es, ms) =
      List.map2((mode, e) => go(~mode, e), modes, es) |> List.split;
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=joint_self(ty => List(ty), tys, ids, ctx),
      ~free=Ctx.union(List.map(Info.exp_free, es)),
      union_m(ms),
    );
  | Cons(e1, e2) =>
    let mode_e1 = Typ.matched_list_mode(mode);
    let (e1, m1) = go(~mode=mode_e1, e1);
    let (e2, m2) = go(~mode=Ana(List(e1.ty)), e2);
    add(
      ~self=Just(List(e1.ty)),
      ~free=Ctx.union([e1.free, e2.free]),
      union_m([m1, m2]),
    );
  | Var(name) =>
    let self =
      switch (Ctx.lookup_var(ctx, name)) {
      | None => Info.FreeVar
      | Some(var) => Common(Just(var.typ))
      };
    add'(
      ~self,
      ~free=[(name, [{id: UExp.rep_id(uexp), mode}])],
      Id.Map.empty,
    );
  | Parens(e) =>
    let (e, m) = go(~mode, e);
    add(~self=Just(e.ty), ~free=e.free, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=Ana(ty_in), e);
    add(~self=Just(ty_out), ~free=e.free, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m1) = go(~mode=Ana(ty1), e1);
    let (e2, m2) = go(~mode=Ana(ty2), e2);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([e1.free, e2.free]),
      union_m([m1, m2]),
    );
  | Tuple(es) =>
    let modes = Typ.matched_prod_mode(mode, List.length(es));
    let (es, ms) =
      List.map2((e, mode) => go(~mode, e), es, modes) |> List.split;
    let frees = List.map(Info.exp_free, es);
    let self = Info.Just(Prod(List.map(Info.exp_ty, es)));
    add(~self, ~free=Ctx.union(frees), union_m(ms));
  | Test(e) =>
    let (e, m1) = go(~mode=Ana(Bool), e);
    add(~self=Just(Prod([])), ~free=e.free, m1);
  | Seq(e1, e2) =>
    let (e1, m1) = go(~mode=Syn, e1);
    let (e2, m2) = go(~mode, e2);
    add(
      ~self=Just(e2.ty),
      ~free=Ctx.union([e1.free, e2.free]),
      union_m([m1, m2]),
    );
  | Tag(tag) => atomic(IsTag(tag, Info.syn_tag_typ(ctx, tag)))
  | Ap(fn, arg) =>
    let fn_mode =
      switch (fn) {
      | {term: Tag(name), _} => Typ.tag_ap_mode(ctx, mode, name)
      | _ => Typ.ap_mode
      };
    let (fn, m_fn) = go(~mode=fn_mode, fn);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m_arg) = go(~mode=Ana(ty_in), arg);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([fn.free, arg.free]),
      union_m([m_fn, m_arg]),
    );
  | Fun(p, e) =>
    let (mode_pat, mode_body) = Typ.matched_arrow_mode(mode);
    let (p, m_pat) = go_pat(~is_synswitch=false, ~mode=mode_pat, p);
    let (e, m_body) = go'(~ctx=p.ctx, ~mode=mode_body, e);
    add(
      ~self=Just(Arrow(p.ty, e.ty)),
      ~free=Ctx.free_in(ctx, p.ctx, e.free),
      union_m([m_pat, m_body]),
    );
  | Let(p, def, body) =>
    let (p_syn, _) = go_pat(~is_synswitch=true, ~mode=Syn, p);
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let (def, m_def) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def);
    /* Analyze pattern to incorporate def type into ctx */
    let (p_ana, m_pat) = go_pat(~is_synswitch=false, ~mode=Ana(def.ty), p);
    let (body, m_body) = go'(~ctx=p_ana.ctx, ~mode, body);
    add(
      ~self=Just(body.ty),
      ~free=Ctx.union([def.free, Ctx.free_in(ctx, p_ana.ctx, body.free)]),
      union_m([m_pat, m_def, m_body]),
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(UExp.rep_id, [e1, e2]);
    let (cond, m0) = go(~mode=Ana(Bool), e0);
    let (cons, m1) = go(~mode, e1);
    let (alt, m2) = go(~mode, e2);
    add(
      ~self=joint_self(Fun.id, [cons.ty, alt.ty], branch_ids, ctx),
      ~free=Ctx.union([cond.free, cons.free, alt.free]),
      union_m([m0, m1, m2]),
    );
  | Match(scrut, rules) =>
    let (scrut, m_scrut) = go(~mode=Syn, scrut);
    let (ps, es) = List.split(rules);
    let branch_ids = List.map(UExp.rep_id, es);
    let (ps, pat_ms) =
      List.map(go_pat(~is_synswitch=false, ~mode=Typ.Ana(scrut.ty)), ps)
      |> List.split;
    let p_ctxs = List.map(Info.pat_ctx, ps);
    let (es, e_ms) =
      List.map2((e, ctx) => go'(~ctx, ~mode, e), es, p_ctxs) |> List.split;
    let e_tys = List.map(Info.exp_ty, es);
    let e_frees =
      List.map2(Ctx.free_in(ctx), p_ctxs, List.map(Info.exp_free, es));
    add(
      ~self=joint_self(Fun.id, e_tys, branch_ids, ctx),
      ~free=Ctx.union([scrut.free] @ e_frees),
      union_m([m_scrut] @ pat_ms @ e_ms),
    );
  | TyAlias(typat, utyp, body) =>
    let m_typat = utpat_to_info_map(~ctx, ~ancestors, typat) |> snd;
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
        let ty_pre = UTyp.to_typ(Ctx.add_abstract(ctx, name, -1), utyp);
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
      let (Info.{free, ty: ty_body, _}, m_body) =
        go'(~ctx=ctx_body, ~mode, body);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let m_typ = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp) |> snd;
      add(~self=Just(ty_escape), ~free, union_m([m_typat, m_body, m_typ]));
    | _ =>
      let (Info.{free, ty: ty_body, _}, m_body) = go'(~ctx, ~mode, body);
      let m_typ = utyp_to_info_map(~ctx, ~ancestors, utyp) |> snd;
      add(~self=Just(ty_body), ~free, union_m([m_typat, m_body, m_typ]));
    };
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      ~ancestors: Info.ancestors,
      ~mode: Typ.mode=Typ.Syn,
      {ids, term} as upat: UPat.t,
    )
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, m) => {
    let info =
      Info.derived_pat(~upat, ~ctx, ~mode, ~ancestors, ~self=Common(self));
    (info, add_info(ids, InfoPat(info), m));
  };
  let atomic = self => add(~self, ~ctx, Id.Map.empty);
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors);
  let unknown = Typ.Unknown(is_synswitch ? SynSwitch : Internal);
  let ctx_fold = (ctx: Ctx.t) =>
    List.fold_left2(
      ((ctx, tys, ms), e, mode) => {
        let (info, m) = go(~ctx, ~mode, e);
        (info.ctx, tys @ [info.ty], ms @ [m]);
      },
      (ctx, [], []),
    );
  switch (term) {
  | Invalid(token) => atomic(BadToken(token))
  | MultiHole(tms) =>
    let ms =
      tms |> List.map(any_to_info_map(~ctx, ~ancestors)) |> List.map(snd);
    add(~self=IsMulti, ~ctx, union_m(ms));
  | EmptyHole => atomic(Just(unknown))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(Internal))))
  | ListLit(ps) =>
    let modes = List.init(List.length(ps), _ => Typ.matched_list_mode(mode));
    let (ctx, tys, ms) = ctx_fold(ctx, ps, modes);
    add(
      ~self=joint_self(ty => List(ty), tys, List.map(UPat.rep_id, ps), ctx),
      ~ctx,
      union_m(ms),
    );
  | Cons(hd, tl) =>
    let (hd, m_hd) = go(~ctx, ~mode=Typ.matched_list_mode(mode), hd);
    let (tl, m_tl) = go(~ctx=hd.ctx, ~mode=Ana(List(hd.ty)), tl);
    add(~self=Just(List(hd.ty)), ~ctx=tl.ctx, union_m([m_hd, m_tl]));
  | Wild => atomic(Just(unknown))
  | Var(name) =>
    let ctx_typ =
      Info.ty_after_fix_pat(ctx, mode, Common(Just(Unknown(Internal))));
    let entry = Ctx.VarEntry({name, id: UPat.rep_id(upat), typ: ctx_typ});
    add(~self=Just(unknown), ~ctx=Ctx.extend(entry, ctx), Id.Map.empty);
  | Tuple(ps) =>
    let modes = Typ.matched_prod_mode(mode, List.length(ps));
    let (ctx, tys, ms) = ctx_fold(ctx, ps, modes);
    add(~self=Just(Prod(tys)), ~ctx, union_m(ms));
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p);
    add(~self=Just(p.ty), ~ctx=p.ctx, m);
  | Tag(tag) => atomic(IsTag(tag, Info.syn_tag_typ(ctx, tag)))
  | Ap(fn, arg) =>
    let fn_mode =
      switch (fn) {
      | {term: Tag(name), _} => Typ.tag_ap_mode(ctx, mode, name)
      | _ => Typ.ap_mode
      };
    let (fn, m_fn) = go(~ctx, ~mode=fn_mode, fn);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m_arg) = go(~ctx, ~mode=Ana(ty_in), arg);
    add(~self=Just(ty_out), ~ctx=arg.ctx, union_m([m_fn, m_arg]));
  | TypeAnn(p, ann) =>
    let (ann, m_ann) = utyp_to_info_map(~ctx, ~ancestors, ann);
    let (p, m_p) = go(~ctx, ~mode=Ana(ann.ty), p);
    add(~self=Just(ann.ty), ~ctx=p.ctx, union_m([m_p, m_ann]));
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=Info.TypeExpected,
      ~ancestors,
      {ids, term} as utyp: UTyp.t,
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
  | Invalid(_)
  | EmptyHole
  | Int
  | Float
  | Bool
  | String => add(Id.Map.empty)
  | Var(_)
  | Tag(_) =>
    /* Names are resolved in Info.status_typ */
    add(Id.Map.empty)
  | List(t)
  | Parens(t) => add(go(t) |> snd)
  | Arrow(t1, t2) => add(union_m([go(t1) |> snd, go(t2) |> snd]))
  | Tuple(ts) => add(ts |> List.map(go) |> List.map(snd) |> union_m)
  | Ap(t1, t2) =>
    let ty_in = UTyp.to_typ(ctx, t2);
    let t1_mode: Info.typ_expects =
      switch (expects) {
      | VariantExpected(m, sum_ty) => TagExpected(m, Arrow(ty_in, sum_ty))
      | _ => TagExpected(Unique, Arrow(ty_in, Unknown(Internal)))
      };
    let m =
      union_m([
        go'(~expects=t1_mode, t1) |> snd,
        go'(~expects=TypeExpected, t2) |> snd,
      ]);
    add(m);
  | USum(ts) =>
    let (ms, _) =
      List.fold_left(
        ((acc, tags), uty) => {
          let (status, tag) =
            switch (UTyp.get_tag(ctx, uty)) {
            | None => (Info.Unique, [])
            | Some(tag) when !List.mem(tag, tags) => (Unique, [tag])
            | Some(tag) => (Duplicate, [tag])
            };
          let ty_sum = UTyp.to_typ(ctx, utyp);
          let m = go'(~expects=VariantExpected(status, ty_sum), uty) |> snd;
          (acc @ [m], tags @ tag);
        },
        ([], []),
        ts,
      );
    add(union_m(ms));
  | MultiHole(tms) =>
    let (_, ms) =
      tms |> List.map(any_to_info_map(~ctx, ~ancestors)) |> List.split;
    add(union_m(ms));
  };
}
and utpat_to_info_map =
    (~ctx, ~ancestors, {ids, term} as utpat: UTPat.t): (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [UTPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let ms =
      tms |> List.map(any_to_info_map(~ctx, ~ancestors)) |> List.split |> snd;
    add(union_m(ms));
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(Id.Map.empty)
  };
};

let mk_map =
  Core.Memo.general(~cache_size_bound=1000, e => {
    uexp_to_info_map(
      ~ctx=Builtins.ctx(Builtins.Pervasives.builtins),
      ~ancestors=[],
      e,
    )
    |> snd
  });
