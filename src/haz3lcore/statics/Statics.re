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

  let error_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        /* Second clause is to eliminate non-representative ids,
         * which will not be found in the measurements map */
        Info.is_error(info) && id == Info.id_of(info) ? [id, ...acc] : acc,
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
  switch (t |> Typ.term_of) {
  | Unknown(_) => true
  | Arrow(_) => true
  | Forall(_, t) => is_arrow_like(t)
  | _ => false
  };
};

let is_recursive = (ctx, p, def, syn: Typ.t) => {
  switch (Pat.get_num_of_vars(p), Exp.get_num_of_functions(def)) {
  | (Some(num_vars), Some(num_fns))
      when num_vars != 0 && num_vars == num_fns =>
    let norm = Typ.normalize(ctx, syn);
    switch (norm |> Typ.term_of) {
    | Prod(syns) when List.length(syns) == num_vars =>
      syns |> List.for_all(is_arrow_like)
    | _ when is_arrow_like(norm) => num_vars == 1
    | _ => false
    };
  | _ => false
  };
};

let typ_exp_binop_bin_int: Operators.op_bin_int => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Int |> Typ.temp
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool |> Typ.temp;

let typ_exp_binop_bin_float: Operators.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Float |> Typ.temp
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool |> Typ.temp;

let typ_exp_binop_bin_string: Operators.op_bin_string => Typ.t =
  fun
  | Concat => String |> Typ.temp
  | Equals => Bool |> Typ.temp;

let typ_exp_binop_bin_prop: Operators.op_bin_prop => Typ.t =
  fun
  | And
  | Or
  | Implies => Prop |> Typ.temp;

let typ_exp_binop: Operators.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Bool |> Typ.temp, Bool |> Typ.temp, Bool |> Typ.temp)
  | Int(op) => (Int |> Typ.temp, Int |> Typ.temp, typ_exp_binop_bin_int(op))
  | Float(op) => (
      Float |> Typ.temp,
      Float |> Typ.temp,
      typ_exp_binop_bin_float(op),
    )
  | String(op) => (
      String |> Typ.temp,
      String |> Typ.temp,
      typ_exp_binop_bin_string(op),
    );

let typ_exp_unop: Operators.op_un => (Typ.t, Typ.t) =
  fun
  | Meta(Unquote) => (
      Var("$Meta") |> Typ.temp,
      Unknown(Internal) |> Typ.temp,
    )
  | Bool(Not) => (Bool |> Typ.temp, Bool |> Typ.temp)
  | Int(Minus) => (Int |> Typ.temp, Int |> Typ.temp);

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: Any.t, m: Map.t): (CoCtx.t, Map.t) =>
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
      {ids, copied: _, term} as uexp: UExp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana({term: Unknown(SynSwitch), _}) => Mode.Syn
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
  | Closure(_) =>
    failwith(
      "TODO: implement closure type checking - see how dynamic type assignment does it",
    )
  | MultiHole(tms) =>
    let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~co_ctx=CoCtx.union(co_ctxs), m);
  | Cast(e, t1, t2)
  | FailedCast(e, t1, t2) =>
    let (e, m) = go(~mode=Ana(t1), e, m);
    add(~self=Just(t2), ~co_ctx=e.co_ctx, m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(Unknown(Internal) |> Typ.temp))
  | Deferral(position) =>
    add'(~self=IsDeferral(position), ~co_ctx=CoCtx.empty, m)
  | Undefined => atomic(Just(Unknown(Hole(EmptyHole)) |> Typ.temp))
  | Bool(_) => atomic(Just(Bool |> Typ.temp))
  | Int(_) => atomic(Just(Int |> Typ.temp))
  | Float(_) => atomic(Just(Float |> Typ.temp))
  | String(_) => atomic(Just(String |> Typ.temp))
  | Prop(_) => atomic(Just(Prop |> Typ.temp))
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let modes = Mode.of_list_lit(ctx, List.length(es), mode);
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=
        Self.listlit(~empty=Unknown(Internal) |> Typ.temp, ctx, tys, ids),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~mode=Mode.of_cons_hd(ctx, mode), hd, m);
    let (tl, m) = go(~mode=Mode.of_cons_tl(ctx, mode, hd.ty), tl, m);
    add(
      ~self=Just(List(hd.ty) |> Typ.temp),
      ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
      m,
    );
  | ListConcat(e1, e2) =>
    let mode = Mode.of_list_concat(ctx, mode);
    let ids = List.map(UExp.rep_id, [e1, e2]);
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
  | DynamicErrorHole(e, _)
  | Parens(e) =>
    let (e, m) = go(~mode, e, m);
    add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
  | UnOp(Meta(Unquote), e) when is_in_filter =>
    let e: UExp.t = {
      ids: e.ids,
      copied: false,
      term:
        switch (e.term) {
        | Var("e") => UExp.Constructor("$e", Unknown(Internal) |> Typ.temp)
        | Var("v") => UExp.Constructor("$v", Unknown(Internal) |> Typ.temp)
        | _ => e.term
        },
    };
    let ty_in = Typ.Var("$Meta") |> Typ.temp;
    let ty_out = Typ.Unknown(Internal) |> Typ.temp;
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
  | BuiltinFun(string) =>
    add'(
      ~self=Self.of_exp_var(Builtins.ctx_init, string),
      ~co_ctx=CoCtx.empty,
      m,
    )
  | Tuple(es) =>
    let modes = Mode.of_prod(ctx, mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=Just(Prod(List.map(Info.exp_ty, es)) |> Typ.temp),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Test(e) =>
    let (e, m) = go(~mode=Ana(Bool |> Typ.temp), e, m);
    add(~self=Just(Prod([]) |> Typ.temp), ~co_ctx=e.co_ctx, m);
  | Filter(Filter({pat: cond, _}), body) =>
    let (cond, m) = go(~mode=Syn, cond, m, ~is_in_filter=true);
    let (body, m) = go(~mode, body, m);
    add(
      ~self=Just(body.ty),
      ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
      m,
    );
  | Filter(Residue(_), body) =>
    let (body, m) = go(~mode, body, m);
    add(~self=Just(body.ty), ~co_ctx=CoCtx.union([body.co_ctx]), m);
  | Seq(e1, e2) =>
    let (e1, m) = go(~mode=Syn, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(~self=Just(e2.ty), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  | Constructor(ctr, _) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(_, fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UExp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
    let (arg, m) = go(~mode=Ana(ty_in), arg, m);
    let self: Self.t =
      Id.is_nullary_ap_flag(arg.term.ids)
      && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp)
        ? BadTrivAp(ty_in) : Just(ty_out);
    add(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]), m);
  | TypAp(fn, utyp) =>
    let typfn_mode = Mode.typap_mode;
    let (fn, m) = go(~mode=typfn_mode, fn, m);
    let (_, m) = utyp_to_info_map(~ctx, ~ancestors, utyp, m);
    let (option_name, ty_body) = Typ.matched_forall(ctx, fn.ty);
    switch (option_name) {
    | Some(name) =>
      add(~self=Just(Typ.subst(utyp, name, ty_body)), ~co_ctx=fn.co_ctx, m)
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
  | Fun(p, e, _, _) =>
    let (mode_pat, mode_body) = Mode.of_arrow(ctx, mode);
    let (p', _) =
      go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~mode=mode_pat, p, m);
    let (e, m) = go'(~ctx=p'.ctx, ~mode=mode_body, e, m);
    /* add co_ctx to pattern */
    let (p, m) =
      go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~mode=mode_pat, p, m);
    // TODO: factor out code
    let unwrapped_self: Self.exp =
      Common(Just(Arrow(p.ty, e.ty) |> Typ.temp));
    let is_exhaustive = p |> Info.pat_constraint |> Incon.is_exhaustive;
    let self =
      is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
    add'(~self, ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx), m);
  | TypFun({term: Var(name), _} as utpat, body, _)
      when !Ctx.shadows_typ(ctx, name) =>
    let mode_body = Mode.of_forall(ctx, Some(name), mode);
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    let ctx_body =
      Ctx.extend_tvar(ctx, {name, id: TPat.rep_id(utpat), kind: Abstract});
    let (body, m) = go'(~ctx=ctx_body, ~mode=mode_body, body, m);
    add(
      ~self=Just(Forall(utpat, body.ty) |> Typ.temp),
      ~co_ctx=body.co_ctx,
      m,
    );
  | TypFun(utpat, body, _) =>
    let mode_body = Mode.of_forall(ctx, None, mode);
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    let (body, m) = go(~mode=mode_body, body, m);
    add(
      ~self=Just(Forall(utpat, body.ty) |> Typ.temp),
      ~co_ctx=body.co_ctx,
      m,
    );
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
          Typ.term_of(ty_p) == Typ.Unknown(SynSwitch)
          && !Typ.eq(ty_fn1, ty_fn2)
            ? ty_fn1 : ty_p;
        };
        let ana =
          switch (
            (def_base.ty |> Typ.term_of, def_base2.ty |> Typ.term_of),
            p_syn.ty |> Typ.term_of,
          ) {
          | ((Prod(ty_fns1), Prod(ty_fns2)), Prod(ty_ps)) =>
            let tys =
              List.map2(ana_ty_fn, List.combine(ty_fns1, ty_fns2), ty_ps);
            Typ.Prod(tys) |> Typ.temp;
          | ((_, _), _) => ana_ty_fn((def_base.ty, def_base2.ty), p_syn.ty)
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
    // TODO: factor out code
    let unwrapped_self: Self.exp = Common(Just(body.ty));
    let is_exhaustive = p_ana |> Info.pat_constraint |> Incon.is_exhaustive;
    let self =
      is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
    add'(
      ~self,
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
      m,
    );
  | FixF(p, e, _) =>
    let (p', _) =
      go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~mode, p, m);
    let (e', m) = go'(~ctx=p'.ctx, ~mode=Ana(p'.ty), e, m);
    let (p'', m) =
      go_pat(~is_synswitch=false, ~co_ctx=e'.co_ctx, ~mode, p, m);
    add(
      ~self=Just(p'.ty),
      ~co_ctx=CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx)]),
      m,
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(UExp.rep_id, [e1, e2]);
    let (cond, m) = go(~mode=Ana(Bool |> Typ.temp), e0, m);
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
    let unwrapped_self: Self.exp =
      Common(Self.match(ctx, e_tys, branch_ids));
    let constraint_ty =
      switch (scrut.ty.term) {
      | Unknown(_) =>
        map_m(go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty), ps, m)
        |> fst
        |> List.map(Info.pat_ty)
        |> Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, ctx)
      | _ => Some(scrut.ty)
      };
    let (self, m) =
      switch (constraint_ty) {
      | Some(constraint_ty) =>
        let pats_to_info_map = (ps: list(UPat.t), m) => {
          /* Add co-ctxs to patterns */
          List.fold_left(
            ((m, acc_constraint), (p, co_ctx)) => {
              let p_constraint =
                go_pat(
                  ~is_synswitch=false,
                  ~co_ctx,
                  ~mode=Mode.Ana(constraint_ty),
                  p,
                  m,
                )
                |> fst
                |> Info.pat_constraint;
              let (p, m) =
                go_pat(
                  ~is_synswitch=false,
                  ~co_ctx,
                  ~mode=Mode.Ana(scrut.ty),
                  p,
                  m,
                );
              let is_redundant =
                Incon.is_redundant(p_constraint, acc_constraint);
              let self = is_redundant ? Self.Redundant(p.self) : p.self;
              let info =
                Info.derived_pat(
                  ~upat=p.term,
                  ~ctx=p.ctx,
                  ~co_ctx=p.co_ctx,
                  ~mode=p.mode,
                  ~ancestors=p.ancestors,
                  ~prev_synswitch=None,
                  ~self,
                  // Mark patterns as redundant at the top level
                  // because redundancy doesn't make sense in a smaller context
                  ~constraint_=p_constraint,
                );
              (
                // Override the info for the single upat
                add_info(p.term.ids, InfoPat(info), m),
                is_redundant
                  ? acc_constraint  // Redundant patterns are ignored
                  : Constraint.Or(p_constraint, acc_constraint),
              );
            },
            (m, Constraint.Falsity),
            List.combine(ps, e_co_ctxs),
          );
        };
        let (m, final_constraint) = pats_to_info_map(ps, m);
        let is_exhaustive = Incon.is_exhaustive(final_constraint);
        let self =
          is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
        (self, m);
      | None =>
        /* Add co-ctxs to patterns */
        let (_, m) =
          map_m(
            ((p, co_ctx)) =>
              go_pat(
                ~is_synswitch=false,
                ~co_ctx,
                ~mode=Mode.Ana(scrut.ty),
                p,
              ),
            List.combine(ps, e_co_ctxs),
            m,
          );
        (unwrapped_self, m);
      };
    add'(~self, ~co_ctx=CoCtx.union([scrut.co_ctx] @ e_co_ctxs), m);
  | TyAlias(typat, utyp, body) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    print_endline(Map.show(m));
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
        switch (utyp.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(utyp)) =>
          /* NOTE: When debugging type system issues it may be beneficial to
             use a different name than the alias for the recursive parameter */
          //let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ty_rec =
            Typ.Rec(TPat.Var(name) |> IdTagged.fresh, utyp) |> Typ.temp;
          let ctx_def =
            Ctx.extend_alias(ctx, name, TPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ => (
            utyp,
            ctx,
            Ctx.extend_alias(ctx, name, TPat.rep_id(typat), utyp),
          )
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
      };
      let ctx_body =
        switch (Typ.get_sum_constructors(ctx, ty_def)) {
        | Some(sm) => Ctx.add_ctrs(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, typat, ty_body);
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
      {ids, term, _} as upat: UPat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, ~constraint_, m) => {
    let prev_synswitch =
      switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
      | Some(Info.InfoPat({mode: Syn | SynFun, ty, _})) => Some(ty)
      | Some(Info.InfoPat({mode: Ana(_), prev_synswitch, _})) => prev_synswitch
      | Some(_)
      | None => None
      };
    let info =
      Info.derived_pat(
        ~prev_synswitch,
        ~upat,
        ~ctx,
        ~co_ctx,
        ~mode,
        ~ancestors,
        ~self=Common(self),
        ~constraint_,
      );
    (info, add_info(ids, InfoPat(info), m));
  };
  let atomic = (self, constraint_) => add(~self, ~ctx, ~constraint_, m);
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors, ~co_ctx);
  let unknown = Typ.Unknown(is_synswitch ? SynSwitch : Internal) |> Typ.temp;
  let ctx_fold = (ctx: Ctx.t, m) =>
    List.fold_left2(
      ((ctx, tys, cons, m), e, mode) =>
        go(~ctx, ~mode, e, m)
        |> (
          ((info, m)) => (
            info.ctx,
            tys @ [info.ty],
            cons @ [info.constraint_],
            m,
          )
        ),
      (ctx, [], [], m),
    );
  let hole = self => atomic(self, Constraint.Hole);
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~ctx, ~constraint_=Constraint.Hole, m);
  | Invalid(token) => hole(BadToken(token))
  | EmptyHole => hole(Just(unknown))
  | Int(int) => atomic(Just(Int |> Typ.temp), Constraint.Int(int))
  | Float(float) =>
    atomic(Just(Float |> Typ.temp), Constraint.Float(float))
  | Tuple([]) => atomic(Just(Prod([]) |> Typ.temp), Constraint.Truth)
  | Bool(bool) =>
    atomic(
      Just(Bool |> Typ.temp),
      bool
        ? Constraint.InjL(Constraint.Truth)
        : Constraint.InjR(Constraint.Truth),
    )
  | String(string) =>
    atomic(Just(String |> Typ.temp), Constraint.String(string))
  | ListLit(ps) =>
    let ids = List.map(UPat.rep_id, ps);
    let modes = Mode.of_list_lit(ctx, List.length(ps), mode);
    let (ctx, tys, cons, m) = ctx_fold(ctx, m, ps, modes);
    let rec cons_fold_list = cs =>
      switch (cs) {
      | [] => Constraint.InjL(Constraint.Truth) // Left = nil, Right = cons
      | [hd, ...tl] =>
        Constraint.InjR(Constraint.Pair(hd, cons_fold_list(tl)))
      };
    add(
      ~self=Self.listlit(~empty=unknown, ctx, tys, ids),
      ~ctx,
      ~constraint_=cons_fold_list(cons),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~ctx, ~mode=Mode.of_cons_hd(ctx, mode), hd, m);
    let (tl, m) =
      go(~ctx=hd.ctx, ~mode=Mode.of_cons_tl(ctx, mode, hd.ty), tl, m);
    add(
      ~self=Just(List(hd.ty) |> Typ.temp),
      ~ctx=tl.ctx,
      ~constraint_=
        Constraint.InjR(Constraint.Pair(hd.constraint_, tl.constraint_)),
      m,
    );
  | Wild => atomic(Just(unknown), Constraint.Truth)
  | Var(name) =>
    /* NOTE: The self type assigned to pattern variables (Unknown)
       may be SynSwitch, but SynSwitch is never added to the context;
       Unknown(Internal) is used in this case */
    let ctx_typ =
      Info.fixed_typ_pat(
        ctx,
        mode,
        Common(Just(Unknown(Internal) |> Typ.temp)),
      );
    let entry = Ctx.VarEntry({name, id: UPat.rep_id(upat), typ: ctx_typ});
    add(
      ~self=Just(unknown),
      ~ctx=Ctx.extend(ctx, entry),
      ~constraint_=Constraint.Truth,
      m,
    );
  | Tuple(ps) =>
    let modes = Mode.of_prod(ctx, mode, List.length(ps));
    let (ctx, tys, cons, m) = ctx_fold(ctx, m, ps, modes);
    let rec cons_fold_tuple = cs =>
      switch (cs) {
      | [] => Constraint.Truth
      | [elt] => elt
      | [hd, ...tl] => Constraint.Pair(hd, cons_fold_tuple(tl))
      };
    add(
      ~self=Just(Prod(tys) |> Typ.temp),
      ~ctx,
      ~constraint_=cons_fold_tuple(cons),
      m,
    );
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p, m);
    add(~self=Just(p.ty), ~ctx=p.ctx, ~constraint_=p.constraint_, m);
  | Constructor(ctr, _) =>
    let self = Self.of_ctr(ctx, ctr);
    atomic(self, Constraint.of_ctr(ctx, mode, ctr, self));
  | Ap(fn, arg) =>
    let ctr = UPat.ctr_name(fn);
    let fn_mode = Mode.of_ap(ctx, mode, ctr);
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
    let (arg, m) = go(~ctx, ~mode=Ana(ty_in), arg, m);
    add(
      ~self=Just(ty_out),
      ~ctx=arg.ctx,
      ~constraint_=
        Constraint.of_ap(ctx, mode, ctr, arg.constraint_, Some(ty_out)),
      m,
    );
  | Cast(p, ann, _) =>
    let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
    let (p, m) = go(~ctx, ~mode=Ana(ann.term), p, m);
    add(~self=Just(ann.term), ~ctx=p.ctx, ~constraint_=p.constraint_, m);
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=Info.TypeExpected,
      ~ancestors,
      {ids, term, _} as utyp: UTyp.t,
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
  switch (term) {
  | Unknown(Hole(MultiHole(tms))) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Unknown(_)
  | Int
  | Float
  | Bool
  | Prop
  | String => add(m)
  | Var(_) =>
    /* Names are resolved in Info.status_typ */
    add(m)
  | List(t)
  | Parens(t) => add(go(t, m) |> snd)
  | Arrow(t1, t2) =>
    let m = go(t1, m) |> snd;
    let m = go(t2, m) |> snd;
    add(m);
  | Prod(ts) =>
    let m = map_m(go, ts, m) |> snd;
    add(m);
  | Ap(t1, t2) =>
    let t1_mode: Info.typ_expects =
      switch (expects) {
      | VariantExpected(m, sum_ty) =>
        ConstructorExpected(m, Arrow(t2, sum_ty) |> Typ.temp)
      | _ =>
        ConstructorExpected(
          Unique,
          Arrow(t2, Unknown(Internal) |> Typ.temp) |> Typ.temp,
        )
      };
    let m = go'(~expects=t1_mode, t1, m) |> snd;
    let m = go'(~expects=TypeExpected, t2, m) |> snd;
    add(m);
  | Sum(variants) =>
    let (m, _) =
      List.fold_left(
        variant_to_info_map(~ctx, ~ancestors, ~ty_sum=utyp),
        (m, []),
        variants,
      );
    add(m);
  | Forall({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(ctx, {name, id: TPat.rep_id(utpat), kind: Abstract});
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
      Ctx.extend_tvar(ctx, {name, id: TPat.rep_id(utpat), kind: Abstract});
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
    (~ctx, ~ancestors, {ids, term, _} as utpat: TPat.t, m: Map.t)
    : (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [TPat.rep_id(utpat)] @ ancestors;
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
    (
      ~ctx,
      ~ancestors,
      ~ty_sum,
      (m, ctrs),
      uty: ConstructorMap.variant(UTyp.t),
    ) => {
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
        {term: Var(ctr), ids, copied: false},
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

let mk =
  Core.Memo.general(~cache_size_bound=1000, (ctx, e) => {
    uexp_to_info_map(~ctx, ~ancestors=[], e, Id.Map.empty) |> snd
  });

let mk = (core: CoreSettings.t, ctx, exp) =>
  core.statics ? mk(ctx, exp) : Id.Map.empty;

let get_error_at = (info_map: Map.t, id: Id.t) => {
  id
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => Some(e)
       | _ => None,
     )
  |> Option.bind(_, e =>
       switch (e.status) {
       | InHole(err_info) => Some(err_info)
       | NotInHole(_) => None
       }
     );
};

let get_pat_error_at = (info_map: Map.t, id: Id.t) => {
  id
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoPat(e) => Some(e)
       | _ => None,
     )
  |> Option.bind(_, e =>
       switch (e.status) {
       | InHole(err_info) => Some(err_info)
       | NotInHole(_) => None
       }
     );
};

let collect_errors = (map: Map.t): list((Id.t, Info.error)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      Option.to_list(Info.error_of(info) |> Option.map(x => (id, x))) @ acc,
    map,
    [],
  );
