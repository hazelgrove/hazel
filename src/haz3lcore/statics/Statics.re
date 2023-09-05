open Term;

/* STATICS.re

   This module determines the statics semantics of a program.
   It makes use of the following modules:

   INFO.re: Defines the Info.t type which is used to represent the
   static STATUS of a term. This STATUS can be either OK or ERROR,
   and is determined by reconcilling two sources of typing information,
   the MODE and the SELF.

   MODE.re: Defines the Mode.t type which is used to represent the
   typing expectations imposed by a term's ancestors.

   SELF.re: Define the Self.t type which is used to represent the
   type information derivable from the term itself.

   The point of STATICS.re itself is to derive a map between each
   term's unique id and that term's static INFO. The below functions
   are intended mostly as infrastructure: The point is to define a
   traversal through the syntax tree which, for each term, passes
   down the MODE, passes up the SELF, calculates the INFO, and adds
   it to the map.

   The architectural intention here is that most type-manipulation
   logic is defined in INFO, MODE, and SELF, and the STATICS module
   itself is dedicated to the piping necessary to (A) introduce and
   (B) propagate the necessary information through the syntax tree.

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
        (~ctx: Ctx.t, ~ancestors, any: any, m: Map.t): (CoCtx.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let (Info.{co_ctx, _}, m) = uexp_to_info_map(~ctx, ~ancestors, e, m);
    (co_ctx, m);
  | Pat(p) =>
    let m =
      upat_to_info_map(~is_synswitch=false, ~ancestors, ~ctx, p, m) |> snd;
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
    ((co_ctxs, m), any) => {
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], m);
    },
    ([], m),
    tms,
  )
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~mode=Mode.Syn,
      ~ancestors,
      {ids, term} as uexp: UExp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(Unknown(SynSwitch)) => Mode.Syn
    | _ => mode
    };
  let add' = (~self, ~co_ctx, m) => {
    let info =
      Info.derived_exp(~uexp, ~ctx, ~mode, ~ancestors, ~self, ~co_ctx);
    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~co_ctx, m) => add'(~self=Common(self), ~co_ctx, m);
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
  let go_module = uexp_to_module(~ancestors);
  let atomic = self => add(~self, ~co_ctx=CoCtx.empty, m);
  switch (term) {
  | MultiHole(tms) =>
    let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~co_ctx=CoCtx.union(co_ctxs), m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(Unknown(Internal)))
  | Triv => atomic(Just(Prod([], [])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(Internal))))
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let modes = Mode.of_list_lit(List.length(es), mode);
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=Self.join(ty => List(ty), tys, ids, ctx),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~mode=Mode.of_cons_hd(mode), hd, m);
    let (tl, m) = go(~mode=Mode.of_cons_tl(mode, hd.ty), tl, m);
    add(
      ~self=Just(List(hd.ty)),
      ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
      m,
    );
  | Var(name) =>
    add'(
      ~self=Self.of_exp_var(ctx, name),
      ~co_ctx=CoCtx.singleton(name, UExp.rep_id(uexp), Mode.ty_of(mode)),
      m,
    )
  | Parens(e) =>
    let (e, m) = go(~mode, e, m);
    add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=Ana(ty_in), e, m);
    add(~self=Just(ty_out), ~co_ctx=e.co_ctx, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m) = go(~mode=Ana(ty1), e1, m);
    let (e2, m) = go(~mode=Ana(ty2), e2, m);
    add(~self=Just(ty_out), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  // TODO: Fix this
  | TupLabel(p, e) =>
    let (e, m) = go(~mode=Syn, e, m);
    let (p, m) =
      upat_to_info_map(
        ~is_synswitch=true,
        ~ctx,
        ~ancestors,
        ~mode=Mode.Ana(e.ty),
        p,
        m,
      );
    add(~self=Just(e.ty), ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx), m);
  | Tuple(es) =>
    let modes = Mode.of_prod(mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=Just(Prod([], List.map(Info.exp_ty, es))),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Test(e) =>
    let (e, m) = go(~mode=Ana(Bool), e, m);
    add(~self=Just(Prod([], [])), ~co_ctx=e.co_ctx, m);
  | Seq(e1, e2) =>
    let (e1, m) = go(~mode=Syn, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(~self=Just(e2.ty), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  | Tag(tag) => atomic(Self.of_tag(ctx, tag))
  /*
   Now Tags are not only Constructors but also possibly modules,
   don't know if necessary to add to free as Var.
   */
  | Ap(fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UExp.tag_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m) = go(~mode=Ana(ty_in), arg, m);
    add(
      ~self=Just(ty_out),
      ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
      m,
    );
  | Fun(p, e) =>
    let (mode_pat, mode_body) = Mode.of_arrow(mode);
    let (p, m) = go_pat(~is_synswitch=false, ~mode=mode_pat, p, m);
    let (e, m) = go'(~ctx=p.ctx, ~mode=mode_body, e, m);
    add(
      ~self=Just(Arrow(p.ty, e.ty)),
      ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
      m,
    );
  | Let(p, def, body) =>
    let (p_syn, _) = go_pat(~is_synswitch=true, ~mode=Syn, p, m);
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def, m);
    /* Analyze pattern to incorporate def type into ctx */
    let (p_ana, m) = go_pat(~is_synswitch=false, ~mode=Ana(def.ty), p, m);
    let (body, m) = go'(~ctx=p_ana.ctx, ~mode, body, m);
    add(
      ~self=Just(body.ty),
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
      m,
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(UExp.rep_id, [e1, e2]);
    let (cond, m) = go(~mode=Ana(Bool), e0, m);
    let (cons, m) = go(~mode, e1, m);
    let (alt, m) = go(~mode, e2, m);
    add(
      ~self=Self.join(Fun.id, [cons.ty, alt.ty], branch_ids, ctx),
      ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
      m,
    );
  | Module(p, def, body) =>
    let (p_syn, _) = go_pat(~is_synswitch=true, ~mode=Syn, p, m);
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let mode1: Mode.t = Ana(p_syn.ty);
    let module_name =
      switch (p.term) {
      | Tag(t) => t
      | _ => ""
      };
    let (inner_ctx, def, m) =
      go_module(~ctx=def_ctx, ~mode=mode1, def, m, []);
    let typ_def: Typ.t = Module(Ctx.modulize_ctx(inner_ctx, module_name));
    /* Analyze pattern to incorporate def type into ctx */
    let (p_ana, m) = go_pat(~is_synswitch=false, ~mode=Ana(typ_def), p, m);
    let (body, m) = go'(~ctx=p_ana.ctx, ~mode, body, m);
    add(
      ~self=Just(body.ty),
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
      m,
    );
  | Dot(modul, name) =>
    let (info_modul, m) = go(~mode=Syn, modul, m);
    switch (info_modul.ty) {
    | Module(inner_ctx) =>
      if (Form.is_tag(name)) {
        add(
          ~self=Self.of_tag(inner_ctx, name),
          ~co_ctx=
            CoCtx.singleton(name, UExp.rep_id(uexp), Mode.ty_of(mode)),
          m,
        );
      } else {
        add'(
          ~self=Self.of_exp_var(inner_ctx, name),
          ~co_ctx=
            CoCtx.singleton(name, UExp.rep_id(uexp), Mode.ty_of(mode)),
          m,
        );
      }
    | _ =>
      add'(
        ~self=FreeVar,
        ~co_ctx=CoCtx.singleton(name, UExp.rep_id(uexp), Mode.ty_of(mode)),
        m,
      )
    };

  | Match(scrut, rules) =>
    let (scrut, m) = go(~mode=Syn, scrut, m);
    let (ps, es) = List.split(rules);
    let branch_ids = List.map(UExp.rep_id, es);
    let (ps, m) =
      map_m(go_pat(~is_synswitch=false, ~mode=Mode.Ana(scrut.ty)), ps, m);
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
    let e_co_ctxs =
      List.map2(CoCtx.mk(ctx), p_ctxs, List.map(Info.exp_co_ctx, es));
    add(
      ~self=Self.join(Fun.id, e_tys, branch_ids, ctx),
      ~co_ctx=CoCtx.union([scrut.co_ctx] @ e_co_ctxs),
      m,
    );
  | TyAlias(typat, utyp, body) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    switch (typat.term) {
    | Var(name) when !Ctx.shadows_typ(ctx, name) =>
      /* Currently we disallow all type shadowing */
      /* NOTE(andrew): Currently, UTyp.to_typ returns Unknown(TypeHole)
         for any type variable reference not in its ctx. So any free variables
         in the definition won't be noticed. But we need to check for free
         variables to decide whether to make a recursive type or not. So we
         tentatively add an abtract type to the ctx, representing the
         speculative rec parameter. */
      let (ty_def, ctx_def, ctx_body) = {
        let ty_pre = UTyp.to_typ(Ctx.extend_dummy_tvar(ctx, name), utyp);
        switch (utyp.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
          let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ctx_def =
            Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ =>
          let ty = UTyp.to_typ(ctx, utyp);
          (ty, ctx, Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty));
        };
      };
      let ctx_body =
        switch (Typ.get_sum_tags(ctx, ty_def)) {
        | Some(sm) => Ctx.add_tags(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let (Info.{co_ctx, ty: ty_body, _}, m) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_escape), ~co_ctx, m);
    | Var(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) =>
      let (Info.{co_ctx, ty: ty_body, _}, m) = go'(~ctx, ~mode, body, m);
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_body), ~co_ctx, m);
    };
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      ~ancestors: Info.ancestors,
      ~mode: Mode.t=Mode.Syn,
      {ids, term} as upat: UPat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, m) => {
    let info =
      Info.derived_pat(~upat, ~ctx, ~mode, ~ancestors, ~self=Common(self));
    (info, add_info(ids, InfoPat(info), m));
  };
  let atomic = self => add(~self, ~ctx, m);
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors);
  let unknown = Typ.Unknown(is_synswitch ? SynSwitch : Internal);
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
  | EmptyHole => atomic(Just(unknown))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([], [])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(Internal))))
  | ListLit(ps) =>
    let modes = Mode.of_list_lit(List.length(ps), mode);
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(
      ~self=Self.join(ty => List(ty), tys, List.map(UPat.rep_id, ps), ctx),
      ~ctx,
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~ctx, ~mode=Mode.of_cons_hd(mode), hd, m);
    let (tl, m) =
      go(~ctx=hd.ctx, ~mode=Mode.of_cons_tl(mode, hd.ty), tl, m);
    add(~self=Just(List(hd.ty)), ~ctx=tl.ctx, m);
  | Wild => atomic(Just(unknown))
  | Var(name) =>
    /* Note the self type assigned to pattern variables (unknown)
       may be SynSwitch, but the type we add to the context is
       always Unknown Internal */
    if (Mode.is_module_ana(mode)) {
      atomic(BadToken(name));
    } else {
      let ctx_typ =
        Info.fixed_typ_pat(ctx, mode, Common(Just(Unknown(Internal))));
      let entry = Ctx.VarEntry({name, id: UPat.rep_id(upat), typ: ctx_typ});
      add(~self=Just(unknown), ~ctx=Ctx.extend(ctx, entry), m);
    }
  | Tuple(ps) =>
    let modes = Mode.of_prod(mode, List.length(ps));
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Just(Prod([], tys)), ~ctx, m);
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p, m);
    add(~self=Just(p.ty), ~ctx=p.ctx, m);
  | Tag(tag) =>
    if (Mode.is_module_ana(mode)) {
      let ctx_typ =
        Info.fixed_typ_pat(ctx, mode, Common(Just(Unknown(Internal))));
      let entry =
        Ctx.TagEntry({name: tag, id: UPat.rep_id(upat), typ: ctx_typ});
      add(~self=Just(unknown), ~ctx=Ctx.extend(ctx, entry), m);
    } else {
      atomic(Self.of_tag(ctx, tag));
    }
  | Ap(fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UPat.tag_name(fn));
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m) = go(~ctx, ~mode=Ana(ty_in), arg, m);
    add(~self=Just(ty_out), ~ctx=arg.ctx, m);
  | TypeAnn(p, ann) =>
    let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
    let (p, m) = go(~ctx, ~mode=Ana(ann.ty), p, m);
    add(~self=Just(ann.ty), ~ctx=p.ctx, m);
  | TyAlias(typat, utyp) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    switch (typat.term) {
    | Var(name)
        when !Form.is_base_typ(name) && Ctx.lookup_alias(ctx, name) == None =>
      let (ty_def, ctx_def, ctx_body) = {
        let ty_pre = UTyp.to_typ(Ctx.extend_dummy_tvar(ctx, name), utyp);
        switch (utyp.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
          let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ctx_def =
            Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ =>
          let ty = UTyp.to_typ(ctx, utyp);
          (ty, ctx, Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty));
        };
      };
      let ctx_body =
        switch (Typ.get_sum_tags(ctx, ty_def)) {
        | Some(sm) => Ctx.add_tags(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_def), ~ctx=ctx_body, m);
    | _ =>
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(~self=Just(unknown), ~ctx, m);
    };
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
  | TupLabel(p, t) =>
    let m = go(t, m) |> snd;
    let (_, m) =
      upat_to_info_map(~is_synswitch=true, ~ctx, ~ancestors, ~mode=Syn, p, m);
    add(m);
  | Tuple(ts) =>
    let m = map_m(go, ts, m) |> snd;
    add(m);
  | Ap(t1, t2) =>
    let ty_in = UTyp.to_typ(ctx, t2);
    let t1_mode: Info.typ_expects =
      switch (expects) {
      | VariantExpected(m, sum_ty) => TagExpected(m, Arrow(ty_in, sum_ty))
      | _ => TagExpected(Unique, Arrow(ty_in, Unknown(Internal)))
      };
    let m = go'(~expects=t1_mode, t1, m) |> snd;
    let m = go'(~expects=TypeExpected, t2, m) |> snd;
    add(m);
  | Sum(variants) =>
    let ty_sum = UTyp.to_typ(ctx, utyp);
    let (m, _) =
      List.fold_left(
        variant_to_info_map(~ctx, ~ancestors, ~ty_sum),
        (m, []),
        variants,
      );
    add(m);
  | Module(p) =>
    let (_, m) =
      upat_to_info_map(~is_synswitch=true, ~ctx, ~ancestors, ~mode=Syn, p, m);
    add(m);
  | Dot(exp, _) =>
    let (_, m) = uexp_to_info_map(~ctx, ~ancestors, ~mode=Syn, exp, m);
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
}
/**
This function is used to generate Info for definition expressions as modules.

For most case it simply calls uexp_to_info_map,
But for Let, Module and Type, it not only does what uexp_to_info_map does,
but also does additional work including extend the inner_ctx
and sometimes switch to Ana mode according to the mode of the entire module.

If those memtioned branches in uexp_to_info_map is updated,
corresponding parts here should also be updated.
 */
and uexp_to_module =
    (
      ~ctx: Ctx.t,
      ~mode=Mode.Syn,
      ~ancestors,
      {ids, _} as uexp: UExp.t,
      m: Map.t,
      inner_ctx: Ctx.t,
    )
    : (Ctx.t, Info.exp, Map.t) => {
  let mode =
    switch (mode) {
    | Ana(Unknown(SynSwitch)) => Mode.Syn
    | _ => mode
    };
  let add' = (~self, ~co_ctx, m, ty_module) => {
    let info =
      Info.derived_exp(~uexp, ~ctx, ~mode=Syn, ~ancestors, ~self, ~co_ctx);
    (ty_module, info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~co_ctx, m, ty_module) =>
    add'(~self=Common(self), ~co_ctx, m, ty_module);
  let ancestors = [UExp.rep_id(uexp)] @ ancestors;
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let go_pat' = upat_to_info_map(~ctx=inner_ctx, ~ancestors);
  let go_module = uexp_to_module(~ancestors);
  /**generates mode for let/module patterns according to the outer module mode. */
  let rec out_moded = (out_mode: Mode.t, pat: Term.UPat.t): Mode.t => {
    switch (out_mode) {
    | Ana(Module(m)) =>
      switch (pat.term) {
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | Triv
      | ListLit([])
      | TyAlias(_)
      | Ap(_) => Syn
      | Var(name)
      | Tag(name) =>
        switch (Ctx.lookup_var(m, name)) {
        | Some(t) => Ana(t.typ)
        | None => Syn
        }
      | Parens(pat) => out_moded(out_mode, pat)
      | Tuple(pats) =>
        List.fold_left(
          (mode: Mode.t, p: Term.UPat.t): Mode.t => {
            switch (mode, out_moded(out_mode, p)) {
            | (Ana(Prod(_, ps)), Ana(t)) => Ana(Prod([], ps @ [t]))
            | _ => Syn
            }
          },
          Ana(Prod([], [])),
          pats,
        )
      | ListLit(pats) =>
        List.fold_left(
          (mode: Mode.t, p: Term.UPat.t): Mode.t => {
            switch (mode, out_moded(out_mode, p)) {
            | (Ana(List(t1)), Ana(t2)) =>
              switch (Typ.join(ctx, t1, t2)) {
              | Some(t) => Ana(List(t))
              | None => Syn
              }
            | _ => Syn
            }
          },
          Ana(Prod([], [])),
          pats,
        )
      | TypeAnn(pat, _) => out_moded(out_mode, pat)
      | Cons(p1, p2) =>
        switch (out_moded(out_mode, p1), out_moded(out_mode, p2)) {
        | (Ana(t1), Ana(List(t2))) =>
          switch (Typ.join(ctx, t1, t2)) {
          | Some(t) => Ana(List(t))
          | None => Syn
          }
        | _ => Syn
        }
      }
    | Ana(_)
    | SynFun
    | Syn => Syn
    };
  };
  switch (uexp.term) {
  | Let(p, def, body) =>
    let mode_pat = out_moded(mode, p);
    let (p_syn, _) = go_pat(~is_synswitch=true, ~mode=mode_pat, p, m);
    let ty_pat =
      switch (mode_pat) {
      | Ana(t) => t
      | Syn
      | SynFun => p_syn.ty
      };
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(ty_pat), def, m);
    let ty_def =
      switch (mode_pat) {
      | Ana(t) => t
      | Syn
      | SynFun => def.ty
      };
    let (p_ana1, m1) = go_pat(~is_synswitch=false, ~mode=Ana(ty_def), p, m);
    let (p_ana2, _m) =
      go_pat'(~is_synswitch=false, ~mode=Ana(ty_def), p, m);
    let ctx_body = p_ana1.ctx;
    let new_inner = p_ana2.ctx;
    let (ty_module, body, m) =
      go_module(~ctx=ctx_body, ~mode, body, m1, new_inner);
    add(
      ~self=Just(body.ty),
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana1.ctx, body.co_ctx)]),
      m,
      ty_module,
    );

  | Module(p, def, body) =>
    let mode_pat = out_moded(mode, p);
    let (p_syn, _) = go_pat(~is_synswitch=true, ~mode=mode_pat, p, m);
    let ty_pat =
      switch (mode_pat) {
      | Ana(t) => t
      | Syn
      | SynFun => p_syn.ty
      };
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let mode1: Mode.t = Ana(ty_pat);
    let module_name =
      switch (p.term) {
      | Tag(t) => t
      | _ => ""
      };
    let (inner_ctx, def, m) =
      go_module(~ctx=def_ctx, ~mode=mode1, def, m, []);
    let typ_def: Typ.t = Module(Ctx.modulize_ctx(inner_ctx, module_name));
    let ty_def =
      switch (mode_pat) {
      | Ana(t) => t
      | Syn
      | SynFun => typ_def
      };
    let (p_ana1, m1) = go_pat(~is_synswitch=false, ~mode=Ana(ty_def), p, m);
    let (p_ana2, _m) =
      go_pat'(~is_synswitch=false, ~mode=Ana(ty_def), p, m);
    let ctx_body = p_ana1.ctx;
    let new_inner = p_ana2.ctx;
    let (ty_module, body, m) =
      go_module(~ctx=ctx_body, ~mode, body, m1, new_inner);
    add(
      ~self=Just(body.ty),
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana1.ctx, body.co_ctx)]),
      m,
      ty_module,
    );
  | TyAlias(typat, utyp, body) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    switch (typat.term) {
    | Var(name)
        when
          !Form.is_base_typ(name)
          && Ctx.lookup_alias(inner_ctx, name) == None =>
      /* NOTE(andrew): See TyAlias in uexp_to_info_map */
      let (ty_def, ctx_def, ctx_body, new_inner) = {
        let ty_pre = UTyp.to_typ(Ctx.extend_dummy_tvar(ctx, name), utyp);
        switch (utyp.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
          let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ctx_def =
            Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty_rec);
          (
            ty_rec,
            ctx_def,
            ctx_def,
            Ctx.extend_alias(inner_ctx, name, UTPat.rep_id(typat), ty_rec),
          );
        | _ =>
          let ty = UTyp.to_typ(ctx, utyp);
          (
            ty,
            ctx,
            Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty),
            Ctx.extend_alias(inner_ctx, name, UTPat.rep_id(typat), ty),
          );
        };
      };
      let (ctx_body, new_inner) =
        switch (Typ.get_sum_tags(ctx, ty_def)) {
        | Some(sm) => (
            Ctx.add_tags(ctx_body, name, UTyp.rep_id(utyp), sm),
            Ctx.add_tags(new_inner, name, UTyp.rep_id(utyp), sm),
          )
        | None => (ctx_body, new_inner)
        };
      let (ty_module, Info.{co_ctx, ty: ty_body, _}, m) =
        go_module(~ctx=ctx_body, ~mode, body, m, new_inner);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let expects: Info.typ_expects =
        switch (mode) {
        | Ana(Module(m)) =>
          switch (Ctx.lookup_alias(m, name)) {
          | Some(ty) => AnaTypeExpected(ty)
          | None => TypeExpected
          }
        | _ => TypeExpected
        };
      let m =
        utyp_to_info_map(~ctx=ctx_def, ~expects, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_escape), ~co_ctx, m, ty_module);
    | _ =>
      let (ty_module, Info.{co_ctx, ty: ty_body, _}, m) =
        go_module(~ctx, ~mode, body, m, inner_ctx);
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_body), ~co_ctx, m, ty_module);
    };

  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Triv
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | Tag(_)
  | Fun(_)
  | TupLabel(_)
  | Tuple(_)
  | Var(_)
  | Dot(_)
  | Ap(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | Parens(_)
  | Cons(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_) =>
    let (info, m) = go(~mode=Syn, uexp, m);
    (inner_ctx, info, m);
  };
}

and variant_to_info_map =
    (~ctx, ~ancestors, ~ty_sum, (m, tags), uty: UTyp.variant) => {
  let go = expects => utyp_to_info_map(~ctx, ~ancestors, ~expects);
  switch (uty) {
  | BadEntry(uty) =>
    let m = go(VariantExpected(Unique, ty_sum), uty, m) |> snd;
    (m, tags);
  | Variant(tag, ids, param) =>
    let m =
      go(
        TagExpected(List.mem(tag, tags) ? Duplicate : Unique, ty_sum),
        {term: Tag(tag), ids},
        m,
      )
      |> snd;
    let m =
      switch (param) {
      | Some(param_ty) => go(TypeExpected, param_ty, m) |> snd
      | None => m
      };
    (m, [tag, ...tags]);
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
