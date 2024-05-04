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

  let error_ids = (term_ranges: TermRanges.t, info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        /* Because of artefacts in Maketerm ID handling,
         * there are be situations where ids appear in the
         * info_map which do not occur in term_ranges. These
         * ids should be purely duplicative, so skipping them
         * when iterating over the info_map should have no
         * effect, beyond supressing the resulting Not_found exs */
        switch (Id.Map.find_opt(id, term_ranges)) {
        | Some(_) when Info.is_error(info) => [id, ...acc]
        | _ => acc
        },
      info_map,
      [],
    );
};

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let rec is_arrow_like = (t: Typ.t) => {
  switch (t) {
  | Unknown(_) => true
  | Arrow(_) => true
  | Forall(_, t) => is_arrow_like(t)
  | _ => false
  };
};

let is_recursive = (ctx, p, def, syn: Typ.t) => {
  switch (Term.UPat.get_num_of_vars(p), Term.UExp.get_num_of_functions(def)) {
  | (Some(num_vars), Some(num_fns))
      when num_vars != 0 && num_vars == num_fns =>
    switch (Typ.normalize(ctx, syn)) {
    | Prod(syns) when List.length(syns) == num_vars =>
      syns |> List.for_all(is_arrow_like)
    | t when is_arrow_like(t) => num_vars == 1
    | _ => false
    }
  | _ => false
  };
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
  | Meta(Unquote) => (Var("$Meta"), Unknown(Free("$Meta")))
  | Bool(Not) => (Bool, Bool)
  | Int(Minus) => (Int, Int);

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: any, m: Map.t): (CoCtx.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, _}: Info.exp, m) =
      uexp_to_info_map(~ctx, ~ancestors, e, m);
    (co_ctx, m);
  | Pat(p) =>
    let m =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~ctx,
        p,
        m,
      )
      |> snd;
    (CoCtx.empty, m);
  | TPat(tp) => (
      CoCtx.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
    )
  | Rul(_)
  | Nul ()
  | Any () => (CoCtx.empty, m)
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
      ~is_in_filter=false,
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
  let uexp_to_info_map =
      (
        ~ctx,
        ~mode=Mode.Syn,
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors,
        uexp: UExp.t,
        m: Map.t,
      ) => {
    uexp_to_info_map(~ctx, ~mode, ~is_in_filter, ~ancestors, uexp, m);
  };
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let map_m_go = m =>
    List.fold_left2(
      ((es, m), mode, e) =>
        go(~mode, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let atomic = self => add(~self, ~co_ctx=CoCtx.empty, m);
  switch (term) {
  | MultiHole(tms) =>
    let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~co_ctx=CoCtx.union(co_ctxs), m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(Unknown(Internal)))
  | Triv => atomic(Just(Prod([])))
  | Deferral(position) =>
    add'(~self=IsDeferral(position), ~co_ctx=CoCtx.empty, m)
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let modes = Mode.of_list_lit(ctx, List.length(es), mode);
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=Self.listlit(~empty=Unknown(Internal), ctx, tys, ids),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~mode=Mode.of_cons_hd(ctx, mode), hd, m);
    let (tl, m) = go(~mode=Mode.of_cons_tl(ctx, mode, hd.ty), tl, m);
    add(
      ~self=Just(List(hd.ty)),
      ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
      m,
    );
  | ListConcat(e1, e2) =>
    let mode = Mode.of_list_concat(ctx, mode);
    let ids = List.map(Term.UExp.rep_id, [e1, e2]);
    let (e1, m) = go(~mode, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(
      ~self=Self.list_concat(ctx, [e1.ty, e2.ty], ids),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
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
  | UnOp(Meta(Unquote), e) when is_in_filter =>
    let e: UExp.t = {
      ids: e.ids,
      term:
        switch (e.term) {
        | Var("e") => UExp.Constructor("$e")
        | Var("v") => UExp.Constructor("$v")
        | _ => e.term
        },
    };
    let ty_in = Typ.Var("$Meta");
    let ty_out = Typ.Unknown(Internal);
    let (e, m) = go(~mode=Ana(ty_in), e, m);
    add(~self=Just(ty_out), ~co_ctx=e.co_ctx, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=Ana(ty_in), e, m);
    add(~self=Just(ty_out), ~co_ctx=e.co_ctx, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m) = go(~mode=Ana(ty1), e1, m);
    let (e2, m) = go(~mode=Ana(ty2), e2, m);
    add(~self=Just(ty_out), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  | Tuple(es) =>
    let modes = Mode.of_prod(ctx, mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=Just(Prod(List.map(Info.exp_ty, es))),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Test(e) =>
    let (e, m) = go(~mode=Ana(Bool), e, m);
    add(~self=Just(Prod([])), ~co_ctx=e.co_ctx, m);
  | Filter(_, cond, body) =>
    let (cond, m) = go(~mode, cond, m, ~is_in_filter=true);
    let (body, m) = go(~mode, body, m);
    add(
      ~self=Just(body.ty),
      ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
      m,
    );
  | Seq(e1, e2) =>
    let (e1, m) = go(~mode=Syn, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(~self=Just(e2.ty), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  | Constructor(ctr) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(fn, arg)
  | Pipeline(arg, fn) =>
    let fn_mode = Mode.of_ap(ctx, mode, UExp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
    let (arg, m) = go(~mode=Ana(ty_in), arg, m);
    let self: Self.t =
      Id.is_nullary_ap_flag(arg.term.ids)
      && !Typ.is_consistent(ctx, ty_in, Prod([]))
        ? BadTrivAp(ty_in) : Just(ty_out);
    add(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]), m);
  | TypAp(fn, utyp) =>
    let typfn_mode = Mode.typap_mode;
    let (fn, m) = go(~mode=typfn_mode, fn, m);
    let (_, m) = utyp_to_info_map(~ctx, ~ancestors, utyp, m);
    let (option_name, ty_body) = Typ.matched_forall(ctx, fn.ty);
    let ty = Term.UTyp.to_typ(ctx, utyp);
    switch (option_name) {
    | Some(name) =>
      add(~self=Just(Typ.subst(ty, name, ty_body)), ~co_ctx=fn.co_ctx, m)
    | None => add(~self=Just(ty_body), ~co_ctx=fn.co_ctx, m) /* invalid name matches with no free type variables. */
    };
  | DeferredAp(fn, args) =>
    let fn_mode = Mode.of_ap(ctx, mode, UExp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
    let num_args = List.length(args);
    let ty_ins = Typ.matched_args(ctx, num_args, ty_in);
    let self: Self.exp = Self.of_deferred_ap(args, ty_ins, ty_out);
    let modes = Mode.of_deferred_ap_args(num_args, ty_ins);
    let (args, m) = map_m_go(m, modes, args);
    let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args));
    add'(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]), m);
  | Fun(p, e) =>
    let (mode_pat, mode_body) = Mode.of_arrow(ctx, mode);
    let (p', _) =
      go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~mode=mode_pat, p, m);
    let (e, m) = go'(~ctx=p'.ctx, ~mode=mode_body, e, m);
    /* add co_ctx to pattern */
    let (p, m) =
      go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~mode=mode_pat, p, m);
    add(
      ~self=Just(Arrow(p.ty, e.ty)),
      ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
      m,
    );
  | TypFun({term: Var(name), _} as utpat, body)
      when !Ctx.shadows_typ(ctx, name) =>
    let mode_body = Mode.of_forall(ctx, Some(name), mode);
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    let ctx_body =
      Ctx.extend_tvar(
        ctx,
        {name, id: Term.UTPat.rep_id(utpat), kind: Abstract},
      );
    let (body, m) = go'(~ctx=ctx_body, ~mode=mode_body, body, m);
    add(~self=Just(Forall(name, body.ty)), ~co_ctx=body.co_ctx, m);
  | TypFun(utpat, body) =>
    let mode_body = Mode.of_forall(ctx, None, mode);
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    let (body, m) = go(~mode=mode_body, body, m);
    add(~self=Just(Forall("?", body.ty)), ~co_ctx=body.co_ctx, m);
  | Let(p, def, body) =>
    let (p_syn, _) =
      go_pat(~is_synswitch=true, ~co_ctx=CoCtx.empty, ~mode=Syn, p, m);
    let (def, p_ana_ctx, m, ty_p_ana) =
      if (!is_recursive(ctx, p, def, p_syn.ty)) {
        let (def, m) = go(~mode=Ana(p_syn.ty), def, m);
        let ty_p_ana = def.ty;
        let (p_ana', _) =
          go_pat(
            ~is_synswitch=false,
            ~co_ctx=CoCtx.empty,
            ~mode=Ana(ty_p_ana),
            p,
            m,
          );
        (def, p_ana'.ctx, m, ty_p_ana);
      } else {
        let (def_base, _) =
          go'(~ctx=p_syn.ctx, ~mode=Ana(p_syn.ty), def, m);
        let ty_p_ana = def_base.ty;
        /* Analyze pattern to incorporate def type into ctx */
        let (p_ana', _) =
          go_pat(
            ~is_synswitch=false,
            ~co_ctx=CoCtx.empty,
            ~mode=Ana(ty_p_ana),
            p,
            m,
          );
        let def_ctx = p_ana'.ctx;
        let (def_base2, _) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def, m);
        let ana_ty_fn = ((ty_fn1, ty_fn2), ty_p) => {
          ty_p == Typ.Unknown(SynSwitch) && !Typ.eq(ty_fn1, ty_fn2)
            ? ty_fn1 : ty_p;
        };
        let ana =
          switch ((def_base.ty, def_base2.ty), p_syn.ty) {
          | ((Prod(ty_fns1), Prod(ty_fns2)), Prod(ty_ps)) =>
            let tys =
              List.map2(ana_ty_fn, List.combine(ty_fns1, ty_fns2), ty_ps);
            Typ.Prod(tys);
          | ((ty_fn1, ty_fn2), ty_p) => ana_ty_fn((ty_fn1, ty_fn2), ty_p)
          };
        let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(ana), def, m);
        (def, def_ctx, m, ty_p_ana);
      };
    let (body, m) = go'(~ctx=p_ana_ctx, ~mode, body, m);
    /* add co_ctx to pattern */
    let (p_ana, m) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=body.co_ctx,
        ~mode=Ana(ty_p_ana),
        p,
        m,
      );
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
      ~self=Self.match(ctx, [cons.ty, alt.ty], branch_ids),
      ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
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
    /* Add co-ctxs to patterns */
    let (_, m) =
      map_m(
        ((p, co_ctx)) =>
          go_pat(~is_synswitch=false, ~co_ctx, ~mode=Mode.Ana(scrut.ty), p),
        List.combine(ps, e_co_ctxs),
        m,
      );
    add(
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
         in the definition would be obliterated. But we need to check for free
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
        /* NOTE(yuchen): Below is an alternative implementation that attempts to
           add a rec whenever type alias is present. It may cause trouble to the
           runtime, so precede with caution. */
        // Typ.lookup_surface(ty_pre)
        //   ? {
        //     let ty_rec = Typ.Rec({item: ty_pre, name});
        //     let ctx_def = Ctx.add_alias(ctx, name, utpat_id(typat), ty_rec);
        //     (ty_rec, ctx_def, ctx_def);
        //   }
        //   : {
        //     let ty = Term.UTyp.to_typ(ctx, utyp);
        //     (ty, ctx, Ctx.add_alias(ctx, name, utpat_id(typat), ty));
        //   };
      };
      let ctx_body =
        switch (Typ.get_sum_constructors(ctx, ty_def)) {
        | Some(sm) => Ctx.add_ctrs(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_escape), ~co_ctx, m);
    | Var(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) =>
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx, ~mode, body, m);
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_body), ~co_ctx, m);
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
      {ids, term} as upat: UPat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, m) => {
    let info =
      Info.derived_pat(
        ~upat,
        ~ctx,
        ~co_ctx,
        ~mode,
        ~ancestors,
        ~self=Common(self),
      );
    (info, add_info(ids, InfoPat(info), m));
  };
  let atomic = self => add(~self, ~ctx, m);
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors, ~co_ctx);
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
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
  | ListLit(ps) =>
    let ids = List.map(UPat.rep_id, ps);
    let modes = Mode.of_list_lit(ctx, List.length(ps), mode);
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Self.listlit(~empty=unknown, ctx, tys, ids), ~ctx, m);
  | Cons(hd, tl) =>
    let (hd, m) = go(~ctx, ~mode=Mode.of_cons_hd(ctx, mode), hd, m);
    let (tl, m) =
      go(~ctx=hd.ctx, ~mode=Mode.of_cons_tl(ctx, mode, hd.ty), tl, m);
    add(~self=Just(List(hd.ty)), ~ctx=tl.ctx, m);
  | Wild => atomic(Just(unknown))
  | Var(name) =>
    /* NOTE: The self type assigned to pattern variables (Unknown)
       may be SynSwitch, but SynSwitch is never added to the context;
       Unknown(Internal) is used in this case */
    let ctx_typ =
      Info.fixed_typ_pat(ctx, mode, Common(Just(Unknown(Internal))));
    let entry = Ctx.VarEntry({name, id: UPat.rep_id(upat), typ: ctx_typ});
    add(~self=Just(unknown), ~ctx=Ctx.extend(ctx, entry), m);
  | Tuple(ps) =>
    let modes = Mode.of_prod(ctx, mode, List.length(ps));
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Just(Prod(tys)), ~ctx, m);
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p, m);
    add(~self=Just(p.ty), ~ctx=p.ctx, m);
  | Constructor(ctr) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UPat.ctr_name(fn));
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
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
      | _ => ConstructorExpected(Unique, Arrow(ty_in, Unknown(Internal)))
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
  | Forall({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {name, id: Term.UTPat.rep_id(utpat), kind: Abstract},
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Forall(utpat, tbody) =>
    let m =
      utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Rec({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {name, id: Term.UTPat.rep_id(utpat), kind: Abstract},
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Rec(utpat, tbody) =>
    let m =
      utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
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

let collect_errors = (map: Map.t): list((Id.t, Info.error)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      Option.to_list(Info.error_of(info) |> Option.map(x => (id, x))) @ acc,
    map,
    [],
  );
