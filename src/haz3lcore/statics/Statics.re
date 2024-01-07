open Term;
open Util;

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
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool;

let typ_exp_binop_bin_float: UExp.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Float
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool;

let typ_exp_binop_bin_string: UExp.op_bin_string => Typ.t =
  fun
  | Concat => String
  | Equals => Bool;

let typ_exp_binop: UExp.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Bool, Bool, Bool)
  | Int(op) => (Int, Int, typ_exp_binop_bin_int(op))
  | Float(op) => (Float, Float, typ_exp_binop_bin_float(op))
  | String(op) => (String, String, typ_exp_binop_bin_string(op));

let typ_exp_unop: UExp.op_un => (Typ.t, Typ.t) =
  fun
  | Bool(Not) => (Bool, Bool)
  | Int(Minus) => (Int, Int);

let constrain_branches = (branch_tys: list(Typ.t)): Typ.constraints => {
  // find first element containing hole and constrain it to every other elt
  let elts_with_hole = List.filter(Typ.contains_hole, branch_tys);
  switch (elts_with_hole) {
  | [] => []
  | [hd, ..._] =>
    let constrain_rep_to_elt =
        (acc: Typ.constraints, curr: Typ.t): Typ.constraints => {
      [(hd, curr), ...acc];
    };
    List.fold_left(constrain_rep_to_elt, [], branch_tys);
  };
};

let subsumption_constraints =
    (any: any, ctx: Ctx.t, mode: Mode.t, self: Self.t) => {
  let rep_id = Term.rep_id(any);
  let final_typ =
    switch (Self.typ_of(ctx, self)) {
    | Some(typ) => typ
    | None => Unknown(ExpHole(Error, rep_id), false)
    };
  switch (mode) {
  | Ana(expected_typ) => [(final_typ, expected_typ)]
  | _ => []
  };
};

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: any, m: Map.t)
        : (CoCtx.t, Map.t, Typ.constraints) => {
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, constraints, _}: Info.exp, m) =
      uexp_to_info_map(~ctx, ~ancestors, e, m);
    (co_ctx, m, constraints);
  | Pat(p) =>
    let (Info.{constraints, _}, m) =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~ctx,
        p,
        m,
      );
    (CoCtx.empty, m, constraints);
  | TPat(tp) => (
      CoCtx.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
      [],
    )
  | Typ(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
      [],
    )
  | Rul(_)
  | Nul ()
  | Any () => (CoCtx.empty, m, [])
  };
}
and multi = (~ctx, ~ancestors, m, tms) =>
  List.fold_left(
    ((co_ctxs, acc_constraints, m), any) => {
      let (co_ctx, m, constraints) =
        any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], acc_constraints @ constraints, m);
    },
    ([], [], m),
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
    | Ana(Unknown(_, true)) => Mode.Syn
    | _ => mode
    };
  let add' = (~self: Self.exp, ~co_ctx, ~constraints, m) => {
    let info =
      Info.derived_exp(
        ~uexp,
        ~ctx,
        ~mode,
        ~ancestors,
        ~self,
        ~co_ctx,
        ~constraints,
      );
    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self: Self.t, ~constraints, ~co_ctx, m) =>
    add'(~self=Common(self), ~constraints, ~co_ctx, m);
  let ancestors = [UExp.rep_id(uexp)] @ ancestors;
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let subsumption_constraints = self =>
    subsumption_constraints(Exp(uexp), ctx, mode, self);
  let map_m_go = m =>
    List.fold_left2(
      ((es, m), mode, e) =>
        go(~mode, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let atomic = self => {
    add(
      ~self,
      ~co_ctx=CoCtx.empty,
      m,
      ~constraints=subsumption_constraints(self),
    );
  };
  switch (term) {
  | MultiHole(tms) =>
    let (co_ctxs, constraints, m) = multi(~ctx, ~ancestors, m, tms);
    add(
      ~self=IsMulti,
      ~co_ctx=CoCtx.union(co_ctxs),
      m,
      ~constraints=constraints @ subsumption_constraints(IsMulti),
    );
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole =>
    atomic(Just(Unknown(ExpHole(EmptyHole, UExp.rep_id(uexp)), false)))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let (modes, mode_cs) =
      Mode.of_list_lit(ctx, List.length(es), UExp.rep_id(uexp), mode);
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    let self =
      Self.listlit(~empty=Unknown(NoProvenance, false), ctx, tys, ids);
    add(
      ~self,
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      ~constraints=
        mode_cs
        @ ListUtil.flat_map(Info.exp_constraints, es)
        @ subsumption_constraints(self),
      m,
    );
  | Cons(hd, tl) =>
    let (hd_mode, hd_mode_cs) =
      Mode.of_cons_hd(ctx, mode, UExp.rep_id(uexp));
    let (hd, m) = go(~mode=hd_mode, hd, m);
    let (tl_mode, tl_mode_cs) =
      Mode.of_cons_tl(ctx, mode, hd.ty, UExp.rep_id(uexp));
    let (tl, m) = go(~mode=tl_mode, tl, m);
    add(
      ~self=Just(List(hd.ty)),
      ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
      m,
      ~constraints=hd.constraints @ tl.constraints @ hd_mode_cs @ tl_mode_cs,
    );
  | ListConcat(e1, e2) =>
    let ids = List.map(Term.UExp.rep_id, [e1, e2]);
    let (mode, mode_cs) = Mode.of_list_concat(ctx, UExp.rep_id(uexp), mode);
    let (e1, m) = go(~mode, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(
      ~self=Self.list_concat(ctx, [e1.ty, e2.ty], ids),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      ~constraints=mode_cs @ e1.constraints @ e2.constraints,
      m,
    );
  | Var(name) =>
    let (self: Self.exp, subsumption_constraints) =
      switch (Ctx.lookup_var(ctx, name)) {
      | None =>
        let boundary_hole: Self.t =
          Just(Unknown(ExpHole(Error, UExp.rep_id(uexp)), false));
        (Free(name), subsumption_constraints(boundary_hole));
      | Some(var) =>
        let self: Self.t = Just(var.typ);
        (Common(self), subsumption_constraints(self));
      };
    let (mode_ty, mode_cs) = Mode.ty_of(ctx, mode, UExp.rep_id(uexp));
    add'(
      ~self,
      ~co_ctx=CoCtx.singleton(name, UExp.rep_id(uexp), mode_ty),
      m,
      ~constraints=subsumption_constraints @ mode_cs,
    );
  | Parens(e) =>
    let (e, m) = go(~mode, e, m);
    add(~self=Just(e.ty), ~co_ctx=e.co_ctx, ~constraints=e.constraints, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=Ana(ty_in), e, m);
    add(
      ~self=Just(ty_out),
      ~co_ctx=e.co_ctx,
      ~constraints=e.constraints @ subsumption_constraints(Just(ty_out)),
      m,
    );
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m) = go(~mode=Ana(ty1), e1, m);
    let (e2, m) = go(~mode=Ana(ty2), e2, m);
    add(
      ~self=Just(ty_out),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      ~constraints=
        e1.constraints
        @ e2.constraints
        @ subsumption_constraints(Just(ty_out)),
      m,
    );
  | Tuple(es) =>
    let (modes, constraints) =
      Mode.of_prod(ctx, mode, UExp.rep_id(uexp), List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=Just(Prod(List.map(Info.exp_ty, es))),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      ~constraints=constraints @ ListUtil.flat_map(Info.exp_constraints, es),
      m,
    );
  | Test(e) =>
    let (e, m) = go(~mode=Ana(Bool), e, m);
    add(
      ~self=Just(Prod([])),
      ~co_ctx=e.co_ctx,
      ~constraints=e.constraints,
      m,
    );
  | Seq(e1, e2) =>
    let (e1, m) = go(~mode=Syn, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(
      ~self=Just(e2.ty),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      ~constraints=e1.constraints @ e2.constraints,
      m,
    );
  | Constructor(ctr) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(fn, arg)
  | Pipeline(arg, fn) =>
    let fn_mode = Mode.of_ap(ctx, mode, UExp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let ((ty_in, ty_out), match_constraints) =
      Typ.matched_arrow(ctx, UExp.rep_id(uexp), fn.ty);
    let (arg, m) = go(~mode=Ana(ty_in), arg, m);
    let self: Self.t =
      Id.is_nullary_ap_flag(arg.term.ids)
      && !Typ.is_consistent(ctx, ty_in, Prod([]))
        ? BadTrivAp(ty_in) : Just(ty_out);
    add(
      ~self,
      ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
      ~constraints=
        match_constraints
        @ fn.constraints
        @ arg.constraints
        @ subsumption_constraints(self),
      m,
    );
  | Fun(p, e) =>
    let ((mode_pat, mode_body), match_constraints) =
      Mode.of_arrow(ctx, mode, UExp.rep_id(uexp));
    let (p', _) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~mode=mode_pat,
        ~parent_id=Some(UExp.rep_id(e)),
        p,
        m,
      );
    let (e, m) = go'(~ctx=p'.ctx, ~mode=mode_body, e, m);
    /* add co_ctx to pattern */
    let (p, m) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=e.co_ctx,
        ~mode=mode_pat,
        ~parent_id=Some(UExp.rep_id(e.term)),
        p,
        m,
      );
    add(
      ~self=Just(Arrow(p.ty, e.ty)),
      ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
      ~constraints=match_constraints @ e.constraints @ p.constraints,
      m,
    );
  | Let(p, def, body) =>
    let (p_syn, _) =
      go_pat(
        ~is_synswitch=true,
        ~co_ctx=CoCtx.empty,
        ~mode=Syn,
        ~parent_id=Some(UExp.rep_id(def)),
        p,
        m,
      );
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def, m);
    /* Analyze pattern to incorporate def type into ctx */
    let (p_ana', _) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~mode=Ana(def.ty),
        ~parent_id=Some(UExp.rep_id(def.term)),
        p,
        m,
      );
    let (body, m) = go'(~ctx=p_ana'.ctx, ~mode, body, m);
    /* add co_ctx to pattern */
    let (p_ana, m) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=body.co_ctx,
        ~mode=Ana(def.ty),
        ~parent_id=Some(UExp.rep_id(def.term)),
        p,
        m,
      );
    add(
      ~self=Just(body.ty),
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
      ~constraints=
        p_syn.constraints
        @ p_ana'.constraints
        @ p_ana.constraints
        @ def.constraints
        @ body.constraints,
      m,
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(UExp.rep_id, [e1, e2]);
    let (cond, m) = go(~mode=Ana(Bool), e0, m);
    let (cons, m) = go(~mode, e1, m);
    let (alt, m) = go(~mode, e2, m);
    add(
      ~self=Self.match(ctx, [cons.ty, alt.ty], branch_ids),
      ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
      ~constraints=
        cond.constraints
        @ cons.constraints
        @ alt.constraints
        @ constrain_branches([cons.ty, alt.ty]),
      m,
    );
  | Match(scrut, rules) =>
    let (scrut, m) = go(~mode=Syn, scrut, m);
    let (ps, es) = List.split(rules);
    let branch_ids = List.map(UExp.rep_id, es);
    let (ps', _) =
      map_m(
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~mode=Mode.Ana(scrut.ty),
        ),
        ps,
        m,
      );
    let p_ctxs = List.map(Info.pat_ctx, ps');
    let p_constraints = ListUtil.flat_map(Info.pat_constraints, ps');
    let p_typs = List.map(Info.pat_ty, ps');
    let (es, m) =
      List.fold_left2(
        ((es, m), e, ctx) =>
          go'(~ctx, ~mode, e, m) |> (((e, m)) => (es @ [e], m)),
        ([], m),
        es,
        p_ctxs,
      );
    let e_tys = List.map(Info.exp_ty, es);
    let e_constraints = ListUtil.flat_map(Info.exp_constraints, es);
    let e_typs = List.map(Info.exp_ty, es);
    let e_co_ctxs =
      List.map2(CoCtx.mk(ctx), p_ctxs, List.map(Info.exp_co_ctx, es));
    /* Add co-ctxs to patterns */
    let (_, m) =
      map_m(
        ((p, co_ctx)) =>
          go_pat(~is_synswitch=false, ~co_ctx, ~mode=Mode.Ana(scrut.ty), p),
        List.combine(ps, e_co_ctxs),
        m,
      );
    add(
      ~constraints=
        scrut.constraints
        @ e_constraints
        @ p_constraints
        @ constrain_branches(p_typs)
        @ constrain_branches(e_typs),
      ~self=Self.match(ctx, e_tys, branch_ids),
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
          /* NOTE: When debugging type system issues it may be beneficial to
             use a different name than the alias for the recursive parameter */
          //let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ty_rec = Typ.Rec(name, ty_pre);
          let ctx_def =
            Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ =>
          let ty = UTyp.to_typ(ctx, utyp);
          (ty, ctx, Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty));
        };
      };
      let ctx_body =
        switch (Typ.get_sum_constructors(ctx, ty_def)) {
        | Some(sm) => Ctx.add_ctrs(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let (
        {co_ctx, ty: ty_body, constraints: constraints_body, _}: Info.exp,
        m,
      ) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
      add(
        ~self=Just(ty_escape),
        ~constraints=
          constraints_body @ subsumption_constraints(Just(ty_escape)),
        ~co_ctx,
        m,
      );
    | Var(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) =>
      let (
        {co_ctx, ty: ty_body, constraints: constraints_body, _}: Info.exp,
        m,
      ) =
        go'(~ctx, ~mode, body, m);
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(
        ~self=Just(ty_body),
        ~constraints=
          constraints_body @ subsumption_constraints(Just(ty_body)),
        ~co_ctx,
        m,
      );
    };
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors: Info.ancestors,
      ~mode: Mode.t=Mode.Syn,
      ~annot_pat=false,
      ~parent_id: option(Id.t)=None,
      {ids, term} as upat: UPat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let id = UPat.rep_id(upat);
  let add = (~self, ~ctx, ~constraints, m) => {
    let info =
      Info.derived_pat(
        ~upat,
        ~ctx,
        ~co_ctx,
        ~mode,
        ~ancestors,
        ~constraints,
        ~self=Common(self),
      );
    (info, add_info(ids, InfoPat(info), m));
  };
  let subsumption_constraints = self =>
    subsumption_constraints(Pat(upat), ctx, mode, self);
  let atomic = self => {
    add(~self, ~ctx, m, ~constraints=subsumption_constraints(self));
  };
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors, ~co_ctx, ~parent_id);
  let unknown = Typ.Unknown(ExpHole(Internal, id), is_synswitch);
  let ctx_fold = (ctx: Ctx.t, m) =>
    List.fold_left2(
      (((ctx, cs), tys, m), e, mode) =>
        go(~ctx, ~mode, e, m)
        |> (
          ((info, m)) => (
            (info.ctx, info.constraints @ cs),
            tys @ [info.ty],
            m,
          )
        ),
      ((ctx, []), [], m),
    );
  switch (term) {
  | MultiHole(tms) =>
    let (_, constraints, m) = multi(~ctx, ~ancestors, m, tms);
    add(
      ~self=IsMulti,
      ~ctx,
      ~constraints=constraints @ subsumption_constraints(IsMulti),
      m,
    );
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(unknown))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
  | ListLit(ps) =>
    let ids = List.map(UPat.rep_id, ps);
    let (modes, mode_cs) = Mode.of_list_lit(ctx, List.length(ps), id, mode);
    let ((ctx, constraints), tys, m) = ctx_fold(ctx, m, ps, modes);
    add(
      ~self=Self.listlit(~empty=unknown, ctx, tys, ids),
      ~ctx,
      ~constraints=mode_cs @ constraints,
      m,
    );
  | Cons(hd, tl) =>
    let (mode_hd, mode_cs_hd) = Mode.of_cons_hd(ctx, mode, id);
    let (hd, m) = go(~ctx, ~mode=mode_hd, hd, m);
    let (mode_tl, mode_cs_tl) = Mode.of_cons_tl(ctx, mode_hd, hd.ty, id);
    let (tl, m) = go(~ctx=hd.ctx, ~mode=mode_tl, tl, m);
    add(
      ~self=Just(List(hd.ty)),
      ~ctx=tl.ctx,
      ~constraints=hd.constraints @ tl.constraints @ mode_cs_hd @ mode_cs_tl,
      m,
    );
  | Wild => atomic(Just(Typ.Unknown(NoProvenance, is_synswitch)))
  | Var(name) =>
    /* NOTE: The self type assigned to pattern variables (Unknown)
       may be SynSwitch, but SynSwitch is never added to the context;
       Internal is used in this case */
    let hole_reason: Typ.hole_reason =
      switch (annot_pat, parent_id) {
      | (false, Some(id)) => PatternVar(id)
      | _ => Internal
      };
    let ctx_typ =
      Info.fixed_typ_pat(
        ctx,
        mode,
        Common(Just(Unknown(ExpHole(hole_reason, id), false))),
        id,
      );
    let entry = Ctx.VarEntry({name, id, typ: ctx_typ});
    add(
      ~self=Just(unknown),
      ~ctx=Ctx.extend(ctx, entry),
      ~constraints=
        subsumption_constraints(
          Just(Unknown(ExpHole(hole_reason, id), false)),
        ),
      m,
    );
  | Tuple(ps) =>
    let (modes, mode_cs) =
      Mode.of_prod(ctx, mode, UPat.rep_id(upat), List.length(ps));
    let ((ctx, constraints), tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Just(Prod(tys)), ~ctx, ~constraints=mode_cs @ constraints, m);
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p, m);
    add(~self=Just(p.ty), ~ctx=p.ctx, ~constraints=p.constraints, m);
  | Constructor(ctr) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UPat.ctr_name(fn));
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let ((ty_in, ty_out), match_constraints) =
      Typ.matched_arrow(ctx, id, fn.ty);
    let (arg, m) = go(~ctx, ~mode=Ana(ty_in), arg, m);
    add(
      ~self=Just(ty_out),
      ~ctx=arg.ctx,
      ~constraints=fn.constraints @ arg.constraints @ match_constraints,
      m,
    );
  | TypeAnn(p, ann) =>
    let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
    let (p, m) = go(~ctx, ~mode=Ana(ann.ty), ~annot_pat=true, p, m);
    add(~self=Just(ann.ty), ~ctx=p.ctx, ~constraints=p.constraints, m);
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
    let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Int
  | Float
  | Bool
  | String => add(m)
  | Var(_)
  | Constructor(_) =>
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
      | VariantExpected(m, sum_ty) =>
        ConstructorExpected(m, Arrow(ty_in, sum_ty))
      | _ =>
        ConstructorExpected(
          Unique,
          Arrow(ty_in, Unknown(NoProvenance, false)),
        )
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
    let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
  };
}
and variant_to_info_map =
    (~ctx, ~ancestors, ~ty_sum, (m, ctrs), uty: UTyp.variant) => {
  let go = expects => utyp_to_info_map(~ctx, ~ancestors, ~expects);
  switch (uty) {
  | BadEntry(uty) =>
    let m = go(VariantExpected(Unique, ty_sum), uty, m) |> snd;
    (m, ctrs);
  | Variant(ctr, ids, param) =>
    let m =
      go(
        ConstructorExpected(
          List.mem(ctr, ctrs) ? Duplicate : Unique,
          ty_sum,
        ),
        {term: Constructor(ctr), ids},
        m,
      )
      |> snd;
    let m =
      switch (param) {
      | Some(param_ty) => go(TypeExpected, param_ty, m) |> snd
      | None => m
      };
    (m, [ctr, ...ctrs]);
  };
};

let mk_map =
  Core.Memo.general(~cache_size_bound=1000, e => {
    uexp_to_info_map(~ctx=Builtins.ctx_init, ~ancestors=[], e, Id.Map.empty)
    |> snd
  });

let mk_map_and_inference_solutions =
  Core.Memo.general(
    ~cache_size_bound=1000,
    e => {
      let (info, map) =
        uexp_to_info_map(
          ~ctx=Builtins.ctx_init,
          ~ancestors=[],
          e,
          Id.Map.empty,
        );

      print_endline("~~~Printing constraints:");
      info.constraints |> Typ.constraints_to_string |> print_endline;

      let pts_graph = Inference.solve_constraints(info.constraints);
      let solutions = InferenceResult.get_desired_solutions(pts_graph);

      (map, solutions);
    },
  );

let collect_errors = (map: Map.t): list((Id.t, Info.error)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      Option.to_list(Info.error_of(info) |> Option.map(x => (id, x))) @ acc,
    map,
    [],
  );
