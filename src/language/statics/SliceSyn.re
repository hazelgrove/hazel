// The synthesis slice judgement, one function per form of the calculus.

type t = {
  kept: bool,
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
};

type pat_slice = {
  kept: bool,
  omitted: Id.Set.t,
  gamma: Ctx.t,
  phi: Typ.t,
  supplied: Typ.t,
};

type env = {
  info_map: StaticsBase.Map.t,
  focus: Id.t,
};

let gap = Typ.gap;
let note = SliceQuery.note;
let union = Id.Set.union;
let unions = (sets: list(Id.Set.t)): Id.Set.t =>
  List.fold_left(union, Id.Set.empty, sets);

let exp_info = (env: env, e: Exp.t): option(Info.exp) =>
  StaticsBase.Map.lookup_exp(Exp.rep_id(e), env.info_map);

let pat_info = (env: env, p: Pat.t): option(Info.pat) =>
  StaticsBase.Map.lookup_pat(Pat.rep_id(p), env.info_map);

let ctx_at = (env: env, e: Exp.t): Ctx.t =>
  switch (exp_info(env, e)) {
  | Some({ctx, _}) => ctx
  | None => Ctx.empty
  };

let ty_at = (env: env, e: Exp.t): Typ.t =>
  switch (exp_info(env, e)) {
  | Some({ty, _}) => ty
  | None => gap
  };

let pat_ctx_at = (env: env, p: Pat.t): Ctx.t =>
  switch (pat_info(env, p)) {
  | Some({ctx, _}) => ctx
  | None => Ctx.empty
  };

let pat_ty_at = (env: env, p: Pat.t): Typ.t =>
  switch (pat_info(env, p)) {
  | Some({ty, _}) => ty
  | None => gap
  };

let omit_exp = (e: Exp.t): Id.Set.t => note(Exp.rep_id(e));
let omit_pat = (p: Pat.t): Id.Set.t => note(Pat.rep_id(p));

let nothing = (e: Exp.t): t => {
  kept: false,
  omitted: omit_exp(e),
  gamma: SliceGamma.empty,
  psi: gap,
};

let nothing_pat = (p: Pat.t): pat_slice => {
  kept: false,
  omitted: omit_pat(p),
  gamma: SliceGamma.empty,
  phi: gap,
  supplied: gap,
};

let keep = (psi: Typ.t): t => {
  kept: true,
  omitted: Id.Set.empty,
  gamma: SliceGamma.empty,
  psi,
};

let rec is_demandless = (ty: Typ.t): bool =>
  SliceQuery.is_empty(ty)
  || (
    switch (Typ.term_of(ty)) {
    | Prod(items)
    | TypTuple(items) => List.for_all(is_demandless, items)
    | Parens(inner)
    | Projector(_, inner) => is_demandless(inner)
    | _ => false
    }
  );

let pat_keep = (~query: Typ.t, ~phi: Typ.t): bool =>
  !is_demandless(phi) || !SliceQuery.is_gap(query);

let rec mentions_tvar = (names: list(string), ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Var(name) => List.mem(name, names)
  | _ => List.exists(mentions_tvar(names), Typ.children(ty))
  };

let peel_poly = (ty: Typ.t): (list(string), Typ.t) =>
  switch (Typ.term_of(ty)) {
  | Poly(binder, body) => (TPat.tyvars_of(binder), body)
  | _ => ([], ty)
  };

let rec schema_query =
        (bound: list(string), schema: Typ.t, demand: Typ.t): Typ.t =>
  switch (Typ.term_of(schema)) {
  | Var(name) when List.mem(name, bound) =>
    SliceQuery.is_gap(demand) ? gap : schema
  | _ =>
    switch (Typ.children(schema), Typ.children(demand)) {
    | (kids, parts)
        when kids != [] && List.length(kids) == List.length(parts) =>
      Typ.rebuild(schema, List.map2(schema_query(bound), kids, parts))
      |> Option.value(~default=demand)
    | _ => demand
    }
  };

let binder_demand =
    (ctx: Ctx.t, constraints: list((string, Typ.t)), name: string): Typ.t =>
  constraints
  |> List.filter_map(((bound, demand)) =>
       bound == name ? Some(demand) : None
     )
  |> Typ.meet_gap_all(ctx);


let gap_tpat_one = (binder: TPat.t): TPat.t => {...binder, term: EmptyHole};

let tpat_omissions = (binder: TPat.t): Id.Set.t =>
  switch (binder.term) {
  | Tuple(binders) =>
    List.fold_left(
      (acc, part: TPat.t) => Id.Set.union(acc, note(TPat.rep_id(part))),
      Id.Set.empty,
      binders,
    )
  | _ => note(TPat.rep_id(binder))
  };

let rec gap_tpat = (binder: TPat.t): TPat.t =>
  switch (binder.term) {
  | Tuple(binders) => {...binder, term: Tuple(List.map(gap_tpat_one, binders))}
  | Parens(inner) => {...binder, term: Parens(gap_tpat(inner))}
  | _ => gap_tpat_one(binder)
  };

let masked_binder =
    (ctx: Ctx.t, constraints: list((string, Typ.t)), binder: TPat.t): TPat.t => {
  let mask = (b: TPat.t): TPat.t =>
    switch (TPat.tyvar_of_utpat(b)) {
    | Some(name) =>
      SliceQuery.is_gap(binder_demand(ctx, constraints, name))
        ? gap_tpat_one(b) : b
    | None => b
    };
  switch (binder.term) {
  | Tuple(binders) => {...binder, term: Tuple(List.map(mask, binders))}
  | _ => mask(binder)
  };
};

let mask_definition =
    (ctx: Ctx.t, ~against: Typ.t, demand: Typ.t, body: Typ.t): Typ.t => {
  let (term, rewrap) = Typ.unwrap(body);
  switch (term) {
  | TypFun(binder, inner) =>
    let binders = TPat.binders_of(binder);
    let names = List.filter_map(TPat.tyvar_of_utpat, binders);
    let schema =
      switch (Typ.term_of(against)) {
      | TypFun(_, schema) => schema
      | _ => against
      };
    if (List.length(names) != List.length(binders)
        || SliceQuery.is_gap(demand)) {
      body;
    } else {
      let (_, constraints) =
        Typ.collect_constraints(
          ~replace_bound=true,
          ctx,
          names,
          schema,
          demand,
        );
      let demanded =
        List.map(
          name => !SliceQuery.is_gap(binder_demand(ctx, constraints, name)),
          names,
        );
      if (List.for_all(kept => !kept, demanded)) {
        body;
      } else {
        let masked =
          List.map2((b, kept) => kept ? b : gap_tpat_one(b), binders, demanded);
        let binder: TPat.t =
          switch (binder.term) {
          | Tuple(_) => {...binder, term: Tuple(masked)}
          | _ =>
            switch (masked) {
            | [only] => only
            | _ => binder
            }
          };
        TypFun(binder, inner) |> rewrap;
      };
    };
  | _ => body
  };
};

let rec variant_shape =
        (ctx: Ctx.t, definition: Typ.t, name: string): Typ.t => {
  let (term, rewrap) = Typ.unwrap(definition);
  switch (term) {
  | TypFun(binder, body) =>
    TypFun(binder, variant_shape(ctx, body, name)) |> rewrap
  | Rec(_, body) => variant_shape(ctx, body, name)
  | Parens(inner) => variant_shape(ctx, inner, name)
  | Sum(variants) =>
    Sum(
      List.map(
        fun
        | ConstructorMap.Variant(found, _, _) as kept when found == name => kept
        | _ => ConstructorMap.BadEntry(gap),
        variants,
      ),
    )
    |> rewrap
  | _ => definition
  };
}

and variant_only =
        (ctx: Ctx.t, definition: Typ.t, name: string, payload: Typ.t): Typ.t => {
  let (term, rewrap) = Typ.unwrap(definition);
  switch (term) {
  | TypFun(binder, body) =>
    TypFun(binder, variant_only(ctx, body, name, payload)) |> rewrap
  | Rec(_, body) => variant_only(ctx, body, name, payload)
  | Parens(inner) => variant_only(ctx, inner, name, payload)
  | Sum(variants) =>
    Sum(
      List.map(
        fun
        | ConstructorMap.Variant(found, ann, carried) when found == name =>
          ConstructorMap.Variant(
            found,
            ann,
            Option.map(_ => payload, carried),
          )
        | _ => ConstructorMap.BadEntry(gap),
        variants,
      ),
    )
    |> rewrap
  | _ => definition
  };
};

let rec peel_typfun = (definition: Typ.t): (list(TPat.t), Typ.t) =>
  switch (Typ.term_of(definition)) {
  | TypFun(binder, body) =>
    let (binders, inner) = peel_typfun(body);
    (TPat.binders_of(binder) @ binders, inner);
  | Rec(_, body)
  | Parens(body) => peel_typfun(body)
  | _ => ([], definition)
  };

let variant_payload = (sum: Typ.t, name: string): Typ.t =>
  switch (Typ.term_of(sum)) {
  | Sum(variants) =>
    List.find_map(
      fun
      | ConstructorMap.Variant(found, _, payload) when found == name =>
        Some(Option.value(~default=gap, payload))
      | _ => None,
      variants,
    )
    |> Option.value(~default=gap)
  | _ => gap
  };

let applied_alias =
    (ctx: Ctx.t, alias: string, definition: Typ.t, name: string, payload: Typ.t)
    : Typ.t => {
  let head = Typ.temp(Var(alias));
  let (binders, body) = peel_typfun(definition);
  let names = List.filter_map(TPat.tyvar_of_utpat, binders);
  if (names == [] || List.length(names) != List.length(binders)) {
    head;
  } else {
    let (_, constraints) =
      Typ.collect_constraints(
        ~replace_bound=true,
        ctx,
        names,
        variant_payload(body, name),
        payload,
      );
    let args = List.map(binder_demand(ctx, constraints), names);
    let argument =
      switch (args) {
      | [only] => only
      | _ => Typ.temp(TypTuple(args))
      };
    Typ.temp(TypParamAp(head, argument));
  };
};

let alias_of_ctr = (ctx: Ctx.t, name: string): option((string, Typ.t)) =>
  switch (Ctx.lookup_ctr(ctx, name)) {
  | None => None
  | Some({typ, _}) =>
    switch (Ctx.result_head_name_of(typ)) {
    | None => None
    | Some(alias) =>
      switch (Ctx.lookup_alias(ctx, alias)) {
      | None => None
      | Some(definition) => Some((alias, definition))
      }
    }
  };

let ctr_payload_ty = (ctx: Ctx.t, name: string): Typ.t =>
  switch (Ctx.lookup_ctr(ctx, name)) {
  | None => gap
  | Some({typ, _}) =>
    let (_, schema) = peel_poly(typ);
    switch (Typ.term_of(schema)) {
    | Arrow(payload, _) => payload
    | _ => gap
    };
  };

let ctr_demands =
    (
      ~resolved: option((string, Typ.t))=None,
      ~keep_payload=false,
      ctx: Ctx.t,
      name: string,
      payload: Typ.t,
      result: Typ.t,
    )
    : Ctx.t => {
  let ctr =
    SliceGamma.ctr(
      name,
      SliceQuery.is_gap(payload)
        ? result : SliceQuery.mk_arrow(payload, result),
    );
  let expanded =
    keep_payload
    && (
      switch (Typ.term_of(SliceQuery.strip(result))) {
      | Sum(_) => true
      | _ => false
      }
    );
  switch (resolved == None ? alias_of_ctr(ctx, name) : resolved) {
  | None => ctr
  | Some((alias, definition)) =>
    SliceGamma.join(
      ctx,
      ctr,
      SliceGamma.alias(
        alias,
        mask_definition(
          ctx,
          ~against=definition,
          result,
          expanded
            ? variant_shape(ctx, definition, name)
            : variant_only(ctx, definition, name, payload),
        ),
      ),
    )
  };
};


let definition_shape = (definition: Typ.t): Typ.t => {
  let rec shape = (definition: Typ.t): Typ.t => {
    let (term, rewrap) = Typ.unwrap(definition);
    switch (term) {
    | TypFun(binder, body) => TypFun(binder, shape(body)) |> rewrap
    | _ => gap
    };
  };
  shape(definition);
};

let rec alias_demands =
        (ctx: Ctx.t, ~omitted=Id.Set.empty, demand: Typ.t, ty: Typ.t): Ctx.t => {
  let named = (name: string, occurrence: Typ.t): Ctx.t =>
    switch (Ctx.lookup_alias(ctx, name)) {
    | None => SliceGamma.empty
    | Some(definition) =>
      let body =
        SliceQuery.is_gap(demand) || Typ.names_a_definition(ctx, demand)
          ? definition_shape(definition)
          : Typ.overlap(ctx, demand, definition);
      SliceGamma.alias(
        name,
        mask_definition(
          ctx,
          ~against=definition,
          SliceQuery.is_gap(demand) ? occurrence : demand,
          body,
        ),
      );
    };
  if (Id.Set.mem(Typ.rep_id(ty), omitted)) {
    SliceGamma.empty;
  } else {
    switch (Typ.term_of(ty)) {
    | Var(name) => named(name, ty)
    | TypParamAp({term: Var(name), _}, args) =>
      SliceGamma.join(
        ctx,
        named(name, ty),
        alias_demands(ctx, ~omitted, gap, args),
      )
    | _ =>
      let parts = Typ.children(SliceQuery.strip(demand));
      let kids = Typ.children(ty);
      let demands =
        List.length(parts) == List.length(kids)
          ? parts : List.map(_ => gap, kids);
      List.fold_left2(
        (acc, part, kid) =>
          SliceGamma.join(ctx, acc, alias_demands(ctx, ~omitted, part, kid)),
        SliceGamma.empty,
        demands,
        kids,
      );
    };
  };
};

let alias_shapes = (ctx: Ctx.t, ty: Typ.t): Ctx.t =>
  alias_demands(ctx, ty, ty);

let rec syn = (env: env, e: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  if (SliceQuery.is_gap(query)) {
    nothing(e);
  } else {
    switch (Exp.term_of(e)) {
    | Parens(inner)
    | Projector(_, inner) =>
      let r = syn(env, inner, query);
      r.kept ? r : nothing(e);
    | Var(x) => syn_var(env, e, x, query)
    | Constructor(name, _) => syn_ctor(env, e, name, query)
    | Ap(_, fn, arg) => syn_ap(env, e, fn, arg, query)
    | TypAp(fn, arg) => syn_typ_ap(env, e, fn, arg, query)
    | DeferredAp(fn, args) => syn_deferred_ap(env, e, fn, args, query)
    | Fun(p, body, _, _) => syn_lam(env, e, p, body, query)
    | TypAbs(binder, body, _) => syn_tlam(env, e, binder, body, query)
    | FixF(p, body, _) => syn_fix(env, e, p, body, query)
    | Tuple(items) => syn_tuple(env, e, items, query)
    | TupLabel(label, inner) => syn_tup_label(env, e, label, inner, query)
    | ListLit(items) => syn_list_lit(env, e, items, query)
    | Cons(head, tail) => syn_cons(env, e, head, tail, query)
    | ListConcat(left, right) => syn_list_concat(env, e, left, right, query)
    | Let(p, def, body) => syn_let(env, e, p, def, body, query)
    | TyAlias(binder, definition, body) =>
      syn_ty_alias(env, e, binder, definition, body, query)
    | Match(scrut, rules) => syn_match(env, e, scrut, rules, query)
    | If(cond, then_, else_) => syn_if(env, e, cond, then_, else_, query)
    | Seq(left, right) =>
      let r = syn(env, right, query);
      {...r, omitted: union(r.omitted, omit_exp(left))};
    | Asc(inner, ann) => syn_asc(env, e, inner, ann, query)
    | Use(_, body) =>
      let r = syn(env, body, query);
      {...r, kept: true};
    | Test(body) => {
        ...keep(ty_at(env, e)),
        omitted: omit_exp(body),
      }
    | HintedTest(hint, body) => {
        ...keep(ty_at(env, e)),
        omitted: union(omit_exp(hint), omit_exp(body)),
      }
    | Filter(kind, body) =>
      let r = syn(env, body, query);
      {
        ...r,
        kept: true,
        omitted: union(r.omitted, filter_kind_ids(kind)),
      };
    | UnOp(_, arg) => {
        ...keep(ty_at(env, e)),
        omitted: omit_exp(arg),
      }
    | BinOp(_, left, right) => {
        ...keep(ty_at(env, e)),
        omitted: union(omit_exp(left), omit_exp(right)),
      }
    | Dot(receiver, field) => syn_dot(env, e, receiver, field, query)
    | TupleExtension(left, right) =>
      syn_tuple_extension(env, e, left, right, query)
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel
    | BuiltinFun(_)
    | LivelitName(_)
    | EmptyHole
    | MultiHole(_)
    | Invalid(_)
    | Undefined
    | Deferral(_)
    | DynamicErrorHole(_, _)
    | Closure(_, _)
    | DrvQuote(_, _) =>
      let psi = ty_at(env, e);
      {...keep(psi), gamma: alias_shapes(ctx, psi)};
    | Forall(p, body) => {
        ...keep(ty_at(env, e)),
        omitted: union(omit_pat(p), omit_exp(body)),
      }
    | Theorem(p, def, body) =>
      let r = syn(env, body, query);
      {
        ...r,
        kept: true,
        omitted: unions([r.omitted, omit_pat(p), omit_exp(def)]),
      };
    | Module(items) => syn_module(env, e, items, query)
    | ModuleExp(mp, def, body) =>
      syn_let(env, e, ExpandModule.mpat_to_pat(mp), def, body, query)
    | ProofObject(_) => keep(ty_at(env, e))
    };
  };
}

and filter_kind_ids = (kind: TermBase.stepper_filter_kind_t): Id.Set.t =>
  switch (kind) {
  | Filter({pat, _}) => omit_exp(pat)
  | Residue(_, _) => Id.Set.empty
  }

and syn_var = (env: env, e: Exp.t, x: Var.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let id =
    switch (Ctx.lookup_var(ctx, x)) {
    | Some({id, _}) => id
    | None => Id.invalid
    };
  {
    kept: true,
    omitted: Id.Set.empty,
    gamma: SliceGamma.var(~id, x, query),
    psi: query,
  };
}

and syn_ctor = (env: env, e: Exp.t, name: string, query: Typ.t): t =>
  switch (Ctx.lookup_ctr(ctx_at(env, e), name)) {
  | None when Ctx.lookup_var(ctx_at(env, e), name) != None =>
    syn_var(env, e, name, query)
  | _ => syn_known_ctor(env, e, name, query)
  }

and syn_known_ctor = (env: env, e: Exp.t, name: string, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let (_, schema) = peel_poly(SliceQuery.whnf(ctx, ty_at(env, e)));
  let stripped = SliceQuery.strip(query);
  let (binder, demand) =
    switch (Typ.term_of(stripped)) {
    | Poly(binder, body) => (Some(binder), body)
    | _ => (None, stripped)
    };
  let applied =
    switch (Typ.term_of(schema)) {
    | Arrow(_, _) => true
    | _ => false
    };
  let (asked, result) =
    applied ? SliceQuery.arrow(ctx, demand) : (gap, demand);
  let payload =
    Typ.meet_gap(
      ctx,
      asked,
      SliceQuery.variant(ctx, result, name) |> Option.value(~default=gap),
    );
  let instantiated_explicitly =
    switch (binder) {
    | None => false
    | Some(b) =>
      List.exists(
        part => TPat.tyvar_of_utpat(part) != None,
        TPat.binders_of(b),
      )
    };
  let supplied =
    switch (Typ.term_of(SliceQuery.whnf(ctx, result))) {
    | Sum(_) =>
      switch (alias_of_ctr(ctx, name)) {
      | Some((alias, definition)) =>
        applied_alias(ctx, alias, definition, name, payload)
      | None => variant_only(ctx, result, name, payload)
      }
    | _ => result
    };
  let body = applied ? SliceQuery.mk_arrow(payload, supplied) : supplied;
  {
    kept: true,
    omitted: Id.Set.empty,
    gamma:
      ctr_demands(~keep_payload=instantiated_explicitly, ctx, name, asked, result),
    psi:
      switch (binder) {
      | Some(binder) => SliceQuery.mk_poly(binder, body)
      | None => body
      },
  };
}

and instantiated = (ctx: Ctx.t, ty: Typ.t): Typ.t =>
  switch (Typ.term_of(SliceQuery.whnf(ctx, ty))) {
  | Poly(_, body) => instantiated(ctx, body)
  | _ => ty
  }

and syn_ap = (env: env, e: Exp.t, fn: Exp.t, arg: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let demand = SliceQuery.mk_arrow(gap, query);
  let fn_ty = ty_at(env, fn);
  let fn_query =
    switch (peel_poly(SliceQuery.whnf(ctx, fn_ty))) {
    | ([], _) => demand
    | (bound, schema) =>
      let (inlined, _) =
        Typ.collect_constraints(~replace_bound=true, ctx, bound, schema, demand);
      switch (Typ.term_of(SliceQuery.whnf(ctx, fn_ty))) {
      | Poly(binder, _) => SliceQuery.mk_poly(gap_tpat(binder), inlined)
      | _ => inlined
      };
    };
  let f = syn(env, fn, fn_query);
  let (_, cod) = SliceQuery.arrow(ctx, instantiated(ctx, f.psi));
  {
    kept: f.kept,
    omitted: union(f.omitted, omit_exp(arg)),
    gamma: f.gamma,
    psi: cod,
  };
}

and syn_typ_ap = (env: env, e: Exp.t, fn: Exp.t, arg: Typ.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let fn_ty = SliceQuery.whnf(ctx, ty_at(env, fn));
  let (bound, schema) = peel_poly(fn_ty);
  let (inlined, constraints) =
    Typ.collect_constraints(~replace_bound=true, ctx, bound, schema, query);
  let body = schema_query(bound, schema, inlined);
  let fn_query =
    switch (Typ.term_of(fn_ty)) {
    | Poly(binder, _) =>
      SliceQuery.mk_poly(masked_binder(ctx, constraints, binder), body)
    | _ => body
    };
  let f = syn(env, fn, bound == [] ? query : fn_query);
  let demands = List.map(binder_demand(ctx, constraints), bound);
  let arg_query =
    switch (Typ.term_of(arg), bound) {
    | (TypTuple(args), _) when List.length(args) == List.length(bound) =>
      Typ.temp(TypTuple(demands))
    | (_, [only]) => binder_demand(ctx, constraints, only)
    | _ => Typ.meet_gap_all(ctx, List.map(snd, constraints))
    };
  let written =
    switch (Typ.term_of(arg), bound) {
    | (TypTuple(args), _) when List.length(args) == List.length(bound) => args
    | (_, [_only]) => [arg]
    | _ => List.map(_ => gap, bound)
    };
  let binders =
    switch (Typ.term_of(fn_ty)) {
    | Poly(binder, _) => TPat.binders_of(binder)
    | _ => []
    };
  let (_, f_body) = peel_poly(f.psi);
  let psi =
    bound == []
      ? f.psi
      : List.length(binders) == List.length(written)
        && List.length(binders) == List.length(demands)
      ? Typ.subst_many(
          List.map2((w, d) => Typ.overlap(ctx, d, w), written, demands),
          binders,
          f_body,
        )
      : inlined;
  {
    kept: f.kept,
    omitted: union(f.omitted, SliceQuery.omit_typ(ctx, arg_query, arg)),
    gamma: f.gamma,
    psi,
  };
}

and syn_deferred_ap =
    (env: env, e: Exp.t, fn: Exp.t, args: list(Exp.t), query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let (deferred_dom, result) = SliceQuery.arrow(ctx, query);
  let supplied_count = List.length(List.filter(Exp.is_deferral, args));
  let demands = SliceQuery.prod(ctx, deferred_dom, supplied_count);
  let rec distribute = (args, demands) =>
    switch (args) {
    | [] => []
    | [arg, ...rest] =>
      if (Exp.is_deferral(arg)) {
        switch (demands) {
        | [] => [gap, ...distribute(rest, [])]
        | [demand, ...others] => [demand, ...distribute(rest, others)]
        };
      } else {
        [gap, ...distribute(rest, demands)];
      }
    };
  let fn_query =
    SliceQuery.mk_arrow(
      SliceQuery.mk_prod(distribute(args, demands)),
      result,
    );
  let f = syn(env, fn, fn_query);
  let omitted =
    List.fold_left(
      (acc, arg) => Exp.is_deferral(arg) ? acc : union(acc, omit_exp(arg)),
      f.omitted,
      args,
    );
  {
    kept: f.kept,
    omitted,
    gamma: f.gamma,
    psi: query,
  };
}

and syn_lam = (env: env, e: Exp.t, p: Pat.t, body: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let (domain, codomain) = SliceQuery.arrow(ctx, query);
  let b = syn(env, body, codomain);
  let ps = slice_pat(env, p, ~query=domain, ~demands=b.gamma);
  {
    kept: true,
    omitted: union(b.omitted, ps.omitted),
    gamma:
      SliceGamma.join(
        ctx,
        ps.gamma,
        SliceGamma.without(b.gamma, Pat.bound_vars(p)),
      ),
    psi: SliceQuery.mk_arrow(ps.supplied, b.psi),
  };
}

and syn_tlam =
    (env: env, e: Exp.t, binder: TPat.t, body: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let (_, inner) = SliceQuery.poly(ctx, query);
  let b = syn(env, body, inner);
  let names = TPat.tyvars_of(binder);
  let used =
    mentions_tvar(names, b.psi)
    || List.exists(
         name => List.mem(name, retained_tvars(b.omitted, body)),
         names,
       );
  {
    kept: true,
    omitted:
      union(b.omitted, used ? Id.Set.empty : tpat_omissions(binder)),
    gamma: SliceGamma.without_tvars(b.gamma, names),
    psi: SliceQuery.mk_poly(used ? binder : gap_tpat(binder), b.psi),
  };
}

and syn_fix = (env: env, _e: Exp.t, p: Pat.t, body: Exp.t, query: Typ.t): t => {
  let ps = slice_pat(env, p, ~query, ~demands=SliceGamma.empty);
  {
    kept: true,
    omitted: union(omit_exp(body), ps.omitted),
    gamma: ps.gamma,
    psi: ps.supplied,
  };
}

and syn_tuple = (env: env, e: Exp.t, items: list(Exp.t), query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let demands = tuple_demands(ctx, items, query);
  let results: list(t) =
    List.map2((item, demand) => syn(env, item, demand), items, demands);
  {
    kept: true,
    omitted: unions(List.map((r: t) => r.omitted, results)),
    gamma: SliceGamma.join_all(ctx, List.map((r: t) => r.gamma, results)),
    psi: SliceQuery.mk_prod(List.map((r: t) => r.psi, results)),
  };
}

and tuple_demands =
    (ctx: Ctx.t, items: list(Exp.t), query: Typ.t): list(Typ.t) => {
  let positional = SliceQuery.prod(ctx, query, List.length(items));
  List.map2(
    (item, demand) =>
      switch (Exp.term_of(item)) {
      | TupLabel({term: Label(name), _}, _) =>
        let labelled = SliceQuery.labelled(ctx, query, name);
        SliceQuery.is_gap(labelled)
          ? demand : SliceQuery.mk_tup_label(name, labelled)
      | _ => demand
      },
    items,
    positional,
  );
}

and syn_tup_label =
    (env: env, e: Exp.t, label: Exp.t, inner: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let (name, payload) = SliceQuery.tup_label(ctx, query);
  let r = syn(env, inner, payload);
  {
    kept: true,
    omitted: r.omitted,
    gamma: r.gamma,
    psi:
      switch (name, Exp.term_of(label)) {
      | (Some(name), _) => SliceQuery.mk_tup_label(name, r.psi)
      | (None, Label(name)) => SliceQuery.mk_tup_label(name, r.psi)
      | _ => r.psi
      },
  };
}

and syn_list_lit = (env: env, e: Exp.t, items: list(Exp.t), query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let element = SliceQuery.list_elem(ctx, query);
  let (omitted, gamma, psi) =
    List.fold_left(
      ((omitted, gamma, psi), item) => {
        let residual = SliceQuery.residual(ctx, element, psi);
        let r = syn(env, item, residual);
        (
          union(omitted, r.omitted),
          SliceGamma.join(ctx, gamma, r.gamma),
          Typ.meet_gap(ctx, psi, r.psi),
        );
      },
      (Id.Set.empty, SliceGamma.empty, gap),
      items,
    );
  {
    kept: true,
    omitted,
    gamma,
    psi: SliceQuery.mk_list(psi),
  };
}

and syn_cons = (env: env, e: Exp.t, head: Exp.t, tail: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let element = SliceQuery.list_elem(ctx, query);
  let h = syn(env, head, element);
  let supplied = SliceQuery.mk_list(h.psi);
  let residual = SliceQuery.residual(ctx, query, supplied);
  let tl = syn(env, tail, residual);
  {
    kept: true,
    omitted: union(h.omitted, tl.omitted),
    gamma: SliceGamma.join(ctx, h.gamma, tl.gamma),
    psi: Typ.meet_gap(ctx, supplied, tl.psi),
  };
}

and syn_list_concat =
    (env: env, e: Exp.t, left: Exp.t, right: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let shape = SliceQuery.mk_list(gap);
  let demand = SliceQuery.residual(ctx, query, shape);
  let l = syn(env, left, demand);
  let residual = SliceQuery.residual(ctx, demand, l.psi);
  let r = syn(env, right, residual);
  {
    kept: true,
    omitted: union(l.omitted, r.omitted),
    gamma: SliceGamma.join(ctx, l.gamma, r.gamma),
    psi: Typ.meet_gap(ctx, shape, Typ.meet_gap(ctx, l.psi, r.psi)),
  };
}

and syn_asc = (env: env, e: Exp.t, inner: Exp.t, ann: Typ.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let supplied = Typ.overlap(ctx, query, ann);
  let residual = SliceQuery.residual(ctx, query, supplied);
  let r = syn(env, inner, residual);
  let dropped = SliceQuery.omit_typ(ctx, query, ann);
  {
    kept: true,
    omitted: union(r.omitted, dropped),
    gamma:
      SliceGamma.join(
        ctx,
        r.gamma,
        alias_demands(ctx, ~omitted=dropped, query, ann),
      ),
    psi: Typ.meet_gap(ctx, supplied, r.psi),
  };
}

and explains = (ctx: Ctx.t, query: Typ.t, psi: Typ.t): bool =>
  !SliceQuery.is_empty(Typ.overlap(ctx, query, psi))

and syn_if =
    (env: env, e: Exp.t, cond: Exp.t, then_: Exp.t, else_: Exp.t, query: Typ.t)
    : t => {
  let ctx = ctx_at(env, e);
  let branch = (b: Exp.t, q: Typ.t): t => {
    let r = syn(env, b, q);
    explains(ctx, q, r.psi)
      ? r
      : {kept: false, omitted: omit_exp(b), gamma: SliceGamma.empty, psi: gap};
  };
  let t_ = branch(then_, query);
  let residual = t_.kept ? SliceQuery.residual(ctx, query, t_.psi) : query;
  let f_ = branch(else_, residual);
  {
    kept: true,
    omitted: unions([omit_exp(cond), t_.omitted, f_.omitted]),
    gamma: SliceGamma.join(ctx, t_.gamma, f_.gamma),
    psi: Typ.meet_gap(ctx, t_.psi, f_.psi),
  };
}

and syn_let =
    (env: env, e: Exp.t, p: Pat.t, def: Exp.t, body: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let b = syn(env, body, query);
  let ps = slice_pat(env, p, ~query=gap, ~demands=b.gamma);
  let def_query = SliceQuery.residual(ctx, ps.phi, ps.supplied);
  let d = syn(env, def, def_query);
  {
    kept: b.kept,
    omitted: unions([b.omitted, ps.omitted, d.omitted]),
    gamma:
      SliceGamma.join_all(
        ctx,
        [ps.gamma, d.gamma, SliceGamma.without(b.gamma, Pat.bound_vars(p))],
      ),
    psi: b.psi,
  };
}

and syn_ty_alias =
    (
      env: env,
      e: Exp.t,
      binder: TPat.t,
      definition: Typ.t,
      body: Exp.t,
      query: Typ.t,
    )
    : t => {
  let ctx = ctx_at(env, e);
  let b = syn(env, body, query);
  let names = TPat.tyvars_of(binder);
  let name =
    switch (TPat.alias_head(binder)) {
    | Some((name, _)) => name
    | None =>
      switch (names) {
      | [name, ..._] => name
      | [] => ""
      }
    };
  let demand = SliceGamma.alias_demand(b.gamma, name);
  let omitted =
    Ctx.lookup_alias(b.gamma, name) == None
      ? unions([
          b.omitted,
          note(TPat.rep_id(binder)),
          note(Typ.rep_id(definition)),
        ])
      : union(b.omitted, alias_omissions(ctx, ~gamma=b.gamma, demand, binder, definition));
  {
    kept: b.kept,
    omitted,
    gamma: SliceGamma.without_tvars(b.gamma, [name]),
    psi: b.psi,
  };
}

and syn_match =
    (
      env: env,
      e: Exp.t,
      scrut: Exp.t,
      rules: list((Pat.t, Exp.t)),
      query: Typ.t,
    )
    : t => {
  let ctx = ctx_at(env, e);
  let (omitted, gamma, psi, scrut_demand) =
    List.fold_left(
      ((omitted, gamma, psi, scrut_demand), (p, body)) => {
        let residual = SliceQuery.residual(ctx, query, psi);
        if (SliceQuery.is_gap(residual)) {
          (
            unions([omitted, omit_pat(p), omit_exp(body)]),
            gamma,
            psi,
            scrut_demand,
          );
        } else {
          let b = syn(env, body, residual);
          if (!explains(ctx, residual, b.psi)) {
            (
              unions([omitted, omit_pat(p), omit_exp(body)]),
              gamma,
              psi,
              scrut_demand,
            );
          } else {
          let ps = slice_pat(env, p, ~query=gap, ~demands=b.gamma);
          (
            unions([omitted, ps.omitted, b.omitted]),
            SliceGamma.join_all(
              ctx,
              [
                gamma,
                ps.gamma,
                SliceGamma.without(b.gamma, Pat.bound_vars(p)),
              ],
            ),
            Typ.meet_gap(ctx, psi, b.psi),
            Typ.meet_gap(
              ctx,
              scrut_demand,
              SliceQuery.residual(ctx, ps.phi, ps.supplied),
            ),
          );
          };
        };
      },
      (Id.Set.empty, SliceGamma.empty, gap, gap),
      rules,
    );
  let s = syn(env, scrut, scrut_demand);
  {
    kept: true,
    omitted: union(omitted, s.omitted),
    gamma: SliceGamma.join(ctx, gamma, s.gamma),
    psi,
  };
}

and syn_dot =
    (env: env, e: Exp.t, receiver: Exp.t, field: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let receiver_ty = Typ.desugar_sig(ctx, ty_at(env, receiver));
  let labels = Typ.get_labels(ctx, receiver_ty);
  let field_name =
    switch (Exp.term_of(field)) {
    | Label(name)
    | Var(name) => Some(name)
    | _ => None
    };
  let demand =
    switch (field_name) {
    | None => gap
    | Some(name) when labels == [] => SliceQuery.mk_tup_label(name, query)
    | Some(name) =>
      SliceQuery.mk_prod(
        List.map(
          fun
          | Some(label) when label == name =>
            SliceQuery.mk_tup_label(label, query)
          | Some(_)
          | None => gap,
          labels,
        ),
      )
    };
  let r = syn(env, receiver, demand);
  {
    kept: true,
    omitted: r.omitted,
    gamma: r.gamma,
    psi: query,
  };
}

and syn_tuple_extension =
    (env: env, e: Exp.t, left: Exp.t, right: Exp.t, query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let labels_of = (side: Exp.t) => Typ.get_labels(ctx, ty_at(env, side));
  let left_labels = labels_of(left);
  let right_labels = labels_of(right);
  let unlabelled =
    List.for_all(label => label == None, left_labels @ right_labels);
  let restrict = (~overridden=[], labels: list(option(string)), demand) =>
    labels == []
      ? gap
      : SliceQuery.mk_prod(
          List.map(
            fun
            | Some(label) =>
              SliceQuery.mk_tup_label(
                label,
                List.mem(Some(label), overridden)
                  ? gap : SliceQuery.labelled(ctx, demand, label),
              )
            | None => gap,
            labels,
          ),
        );
  let (left_query, right_query) =
    if (unlabelled) {
      let parts =
        SliceQuery.prod(
          ctx,
          query,
          List.length(left_labels) + List.length(right_labels),
        );
      let take = (n, items) => List.filteri((i, _) => i < n, items);
      let drop = (n, items) => List.filteri((i, _) => i >= n, items);
      (
        SliceQuery.mk_prod(take(List.length(left_labels), parts)),
        SliceQuery.mk_prod(drop(List.length(left_labels), parts)),
      );
    } else {
      (
        restrict(~overridden=right_labels, left_labels, query),
        restrict(right_labels, query),
      );
    };
  let l = syn(env, left, left_query);
  let r = syn(env, right, right_query);
  {
    kept: true,
    omitted: union(l.omitted, r.omitted),
    gamma: SliceGamma.join(ctx, l.gamma, r.gamma),
    psi: Typ.meet_gap(ctx, l.psi, r.psi),
  };
}

and slice_pat =
    (env: env, p: Pat.t, ~query: Typ.t, ~demands: Ctx.t): pat_slice => {
  let ctx = pat_ctx_at(env, p);
  let composite = (~omitted, ~gamma, ~phi, ~supplied) =>
    pat_keep(~query, ~phi)
      ? {kept: true, omitted, gamma, phi, supplied} : nothing_pat(p);
  let plain = (~supplied=gap, ()) =>
    SliceQuery.is_gap(query)
      ? nothing_pat(p)
      : {
        kept: true,
        omitted: Id.Set.empty,
        gamma: SliceGamma.empty,
        phi: gap,
        supplied,
      };
  switch (Pat.term_of(p)) {
  | Parens(inner)
  | Projector(_, inner) =>
    let r = slice_pat(env, inner, ~query, ~demands);
    r.kept ? r : nothing_pat(p);
  | Var(x) =>
    let phi = SliceGamma.demand(demands, x);
    SliceQuery.is_gap(phi)
      ? nothing_pat(p)
      : {
        kept: true,
        omitted: Id.Set.empty,
        gamma: SliceGamma.empty,
        phi,
        supplied: gap,
      };
  | Wild
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => plain()
  | Atom(_) => plain(~supplied=pat_ty_at(env, p), ())
  | Label(_)
  | ExplicitNonlabel => {
      kept: true,
      omitted: Id.Set.empty,
      gamma: SliceGamma.empty,
      phi: gap,
      supplied: pat_ty_at(env, p),
    }
  | Tuple(items) =>
    let queries = pat_tuple_demands(ctx, items, query);
    let results =
      List.map2(
        (item, q) => slice_pat(env, item, ~query=q, ~demands),
        items,
        queries,
      );
    composite(
      ~omitted=unions(List.map((r: pat_slice) => r.omitted, results)),
      ~gamma=
        SliceGamma.join_all(
          ctx,
          List.map((r: pat_slice) => r.gamma, results),
        ),
      ~phi=SliceQuery.mk_prod(List.map((r: pat_slice) => r.phi, results)),
      ~supplied=
        SliceQuery.mk_prod(List.map((r: pat_slice) => r.supplied, results)),
    );
  | TupLabel(label, inner) =>
    let (name, payload) = SliceQuery.tup_label(ctx, query);
    let r = slice_pat(env, inner, ~query=payload, ~demands);
    let wrap = (ty: Typ.t) =>
      switch (name, Pat.term_of(label)) {
      | (Some(name), _) => SliceQuery.mk_tup_label(name, ty)
      | (None, Label(name)) => SliceQuery.mk_tup_label(name, ty)
      | _ => ty
      };
    let phi = SliceQuery.is_gap(r.phi) ? gap : wrap(r.phi);
    pat_keep(~query, ~phi)
      ? {
        kept: true,
        omitted: r.omitted,
        gamma: r.gamma,
        phi,
        supplied: wrap(r.supplied),
      }
      : {
        kept: false,
        omitted: omit_pat(inner),
        gamma: SliceGamma.empty,
        phi: gap,
        supplied: wrap(gap),
      };
  | ListLit(items) =>
    let element = SliceQuery.list_elem(ctx, query);
    let results =
      List.map(item => slice_pat(env, item, ~query=element, ~demands), items);
    composite(
      ~omitted=unions(List.map((r: pat_slice) => r.omitted, results)),
      ~gamma=
        SliceGamma.join_all(
          ctx,
          List.map((r: pat_slice) => r.gamma, results),
        ),
      ~phi=
        SliceQuery.mk_list(
          Typ.meet_gap_all(ctx, List.map((r: pat_slice) => r.phi, results)),
        ),
      ~supplied=
        SliceQuery.mk_list(
          Typ.meet_gap_all(
            ctx,
            List.map((r: pat_slice) => r.supplied, results),
          ),
        ),
    );
  | Cons(head, tail) =>
    let element = SliceQuery.list_elem(ctx, query);
    let h = slice_pat(env, head, ~query=element, ~demands);
    let t = slice_pat(env, tail, ~query, ~demands);
    composite(
      ~omitted=union(h.omitted, t.omitted),
      ~gamma=SliceGamma.join(ctx, h.gamma, t.gamma),
      ~phi=Typ.meet_gap(ctx, SliceQuery.mk_list(h.phi), t.phi),
      ~supplied=Typ.meet_gap(ctx, SliceQuery.mk_list(h.supplied), t.supplied),
    );
  | Asc({term: Ap(head, arg), _}, ann)
      when
        switch (Pat.term_of(head)) {
        | Var(_) => true
        | _ => false
        } =>
    let h = slice_pat(env, head, ~query, ~demands);
    let (domain, codomain) =
      SliceQuery.arrow(ctx, Typ.meet_gap(ctx, query, h.phi));
    let a = slice_pat(env, arg, ~query=domain, ~demands);
    let supplied =
      SliceQuery.mk_arrow(a.supplied, Typ.overlap(ctx, codomain, ann));
    if (!h.kept && SliceQuery.is_gap(codomain)) {
      nothing_pat(p);
    } else {
      {
        kept: true,
        omitted:
          unions([
            h.omitted,
            a.omitted,
            SliceQuery.omit_typ(ctx, codomain, ann),
          ]),
        gamma: SliceGamma.join(ctx, h.gamma, a.gamma),
        phi: h.phi,
        supplied,
      };
    };
  | Asc(inner, ann) =>
    let r = slice_pat(env, inner, ~query, ~demands);
    let demanded = Typ.meet_gap(ctx, query, r.phi);
    let rec constructed = (p: Pat.t) =>
      switch (Pat.term_of(p)) {
      | Constructor(_, _) => true
      | Parens(inner)
      | Projector(_, inner) => constructed(inner)
      | Ap(head, _) => constructed(head)
      | _ => false
      };
    let ann_query =
      constructed(inner)
        ? Typ.subtract_uncollapsed(ctx, demanded, r.supplied) : demanded;
    if (!r.kept && SliceQuery.is_gap(ann_query)) {
      nothing_pat(p);
    } else {
      {
        kept: true,
        omitted: union(r.omitted, SliceQuery.omit_typ(ctx, ann_query, ann)),
        gamma:
          SliceGamma.join(
            ctx,
            r.gamma,
            alias_demands(
              ctx,
              ~omitted=SliceQuery.omit_typ(ctx, ann_query, ann),
              ann_query,
              ann,
            ),
          ),
        phi: r.phi,
        supplied:
          Typ.meet_gap(ctx, r.supplied, Typ.overlap(ctx, ann_query, ann)),
      };
    };
  | Constructor(name, _) => slice_ctor_pat(env, p, name, None, ~query, ~demands)
  | Ap(head, arg) =>
    switch (Pat.term_of(head)) {
    | Constructor(name, _) =>
      slice_ctor_pat(env, p, name, Some(arg), ~query, ~demands)
    | _ =>
      let h = slice_pat(env, head, ~query, ~demands);
      let a = slice_pat(env, arg, ~query=gap, ~demands);
      composite(
        ~omitted=union(h.omitted, a.omitted),
        ~gamma=SliceGamma.join(ctx, h.gamma, a.gamma),
        ~phi=h.phi,
        ~supplied=h.supplied,
      );
    }
  };
}

and pat_tuple_demands =
    (ctx: Ctx.t, items: list(Pat.t), query: Typ.t): list(Typ.t) => {
  let positional = SliceQuery.prod(ctx, query, List.length(items));
  List.map2(
    (item, demand) =>
      switch (Pat.term_of(item)) {
      | TupLabel({term: Label(name), _}, _) =>
        let labelled = SliceQuery.labelled(ctx, query, name);
        SliceQuery.is_gap(labelled)
          ? demand : SliceQuery.mk_tup_label(name, labelled);
      | _ => demand
      },
    items,
    positional,
  );
}

and alias_named_by =
    (ctx: Ctx.t, ty: Typ.t, ctor: string): option((string, Typ.t)) => {
  let head =
    switch (Typ.term_of(SliceQuery.strip(ty))) {
    | Var(name) => Some(name)
    | TypParamAp({term: Var(name), _}, _) => Some(name)
    | _ => None
    };
  switch (head) {
  | None => None
  | Some(name) =>
    switch (Ctx.lookup_alias(ctx, name)) {
    | Some(definition)
        when SliceQuery.variant(ctx, definition, ctor) != None
             || variant_payload(definition, ctor) != gap =>
      Some((name, definition))
    | _ => None
    }
  };
}

and slice_ctor_pat =
    (
      env: env,
      p: Pat.t,
      name: string,
      arg: option(Pat.t),
      ~query: Typ.t,
      ~demands: Ctx.t,
    )
    : pat_slice => {
  let ctx = pat_ctx_at(env, p);
  let payload_query =
    SliceQuery.variant(ctx, query, name) |> Option.value(~default=gap);
  let a =
    switch (arg) {
    | None => {
        kept: false,
        omitted: Id.Set.empty,
        gamma: SliceGamma.empty,
        phi: gap,
        supplied: gap,
      }
    | Some(arg) => slice_pat(env, arg, ~query=payload_query, ~demands)
    };
  let supplies_shape =
    !SliceQuery.is_empty(Typ.overlap(ctx, query, pat_ty_at(env, p)));
  if (!a.kept && !supplies_shape) {
    nothing_pat(p);
  } else {
    let payload_supplied =
      Typ.meet_gap(
        ctx,
        a.supplied,
        Typ.overlap(ctx, a.phi, ctr_payload_ty(ctx, name)),
      );
    let annotated = alias_named_by(ctx, pat_ty_at(env, p), name);
    switch (alias_of_ctr(ctx, name)) {
    | None => {
        kept: true,
        omitted: a.omitted,
        gamma: a.gamma,
        phi: a.phi,
        supplied: a.supplied,
      }
    | Some((scoped, scoped_definition)) =>
      let shadowed =
        switch (annotated) {
        | Some((found, _)) => found != scoped
        | None => false
        };
      let (alias, definition) =
        switch (annotated) {
        | Some(resolved) when shadowed => resolved
        | _ => (scoped, scoped_definition)
        };
      {
        kept: true,
        omitted: a.omitted,
        gamma:
          SliceGamma.join(
            ctx,
            a.gamma,
            ctr_demands(
              ~resolved=Some((alias, definition)),
              ctx,
              name,
              a.phi,
              gap,
            ),
          ),
        phi: applied_alias(ctx, alias, definition, name, a.phi),
        supplied:
          shadowed
            ? gap
            : applied_alias(ctx, alias, definition, name, payload_supplied),
      };
    };
  };
}

and exp_children = (e: Exp.t): list(Exp.t) =>
  switch (Exp.term_of(e)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Undefined
  | Deferral(_)
  | Atom(_)
  | DrvQuote(_, _)
  | Constructor(_, _)
  | Label(_)
  | ExplicitNonlabel
  | LivelitName(_)
  | Var(_)
  | BuiltinFun(_) => []
  | Module(items) =>
    List.filter_map(
      (item: Mod.t) =>
        switch (item.term) {
        | ModLet(_, inner)
        | ModExp(inner)
        | ModuleMod(_, inner) => Some(inner)
        | _ => None
        },
      items,
    )
  | DynamicErrorHole(inner, _)
  | ProofObject(inner)
  | Forall(_, inner)
  | Closure(_, inner)
  | Parens(inner)
  | Projector(_, inner)
  | Test(inner)
  | TypAp(inner, _)
  | Asc(inner, _)
  | Use(_, inner)
  | TyAlias(_, _, inner)
  | UnOp(_, inner)
  | TypAbs(_, inner, _)
  | FixF(_, inner, _)
  | Fun(_, inner, _, _) => [inner]
  | Filter(kind, inner) =>
    switch (kind) {
    | Filter({pat, _}) => [pat, inner]
    | Residue(_, _) => [inner]
    }
  | Ap(_, left, right)
  | BinOp(_, left, right)
  | Cons(left, right)
  | ListConcat(left, right)
  | Seq(left, right)
  | HintedTest(left, right)
  | TupLabel(left, right)
  | Dot(left, right)
  | TupleExtension(left, right)
  | Theorem(_, left, right)
  | ModuleExp(_, left, right)
  | Let(_, left, right) => [left, right]
  | DeferredAp(fn, args) => [fn, ...args]
  | ListLit(items)
  | Tuple(items) => items
  | If(cond, then_, else_) => [cond, then_, else_]
  | Match(scrut, rules) => [scrut, ...List.map(snd, rules)]
  }

and tvar_occurrences = (names: list(string), ty: Typ.t): Id.Set.t =>
  switch (Typ.term_of(ty)) {
  | Var(name) when List.mem(name, names) => note(Typ.rep_id(ty))
  | _ =>
    List.fold_left(
      (acc, child) => union(acc, tvar_occurrences(names, child)),
      Id.Set.empty,
      Typ.children(ty),
    )
  }

and ctr_result_types = (ctx: Ctx.t, gamma: Ctx.t, alias: string): list(Typ.t) =>
  List.filter_map(
    (entry: Ctx.entry) =>
      switch (entry) {
      | ConstructorEntry({name, typ, _}) =>
        switch (alias_of_ctr(ctx, name)) {
        | Some((found, _)) when found == alias =>
          let (_, schema) = peel_poly(typ);
          switch (Typ.term_of(schema)) {
          | Arrow(_, result) => Some(result)
          | _ => Some(schema)
          };
        | _ => None
        }
      | _ => None
      },
    gamma.entries,
  )

and alias_kept_params =
    (~gamma=Ctx.empty, ~ctx=Ctx.empty, demand: Typ.t, binder: TPat.t)
    : list(string) => {
  let rec body = (ty: Typ.t): Typ.t =>
    switch (Typ.term_of(ty)) {
    | TypFun(_, inner) => body(inner)
    | _ => ty
    };
  switch (TPat.alias_head(binder)) {
  | Some((alias, params)) =>
    let mentioned = [
      body(demand),
      ...ctr_result_types(ctx, gamma, alias),
    ];
    List.filter_map(
      param =>
        switch (TPat.tyvar_of_utpat(param)) {
        | Some(name)
            when List.exists(ty => mentions_tvar([name], ty), mentioned) =>
          Some(name)
        | _ => None
        },
      params,
    );
  | None => []
  };
}

and alias_omissions =
    (
      ctx: Ctx.t,
      ~gamma=Ctx.empty,
      demand: Typ.t,
      binder: TPat.t,
      definition: Typ.t,
    )
    : Id.Set.t => {
  let kept = alias_kept_params(~gamma, ~ctx, demand, binder);
  Id.Set.diff(
    union(
      SliceQuery.omit_typ(ctx, demand, definition),
      alias_binder_omissions(~gamma, ~ctx, demand, binder),
    ),
    tvar_occurrences(kept, definition),
  );
}

and alias_binder_omissions =
    (~gamma=Ctx.empty, ~ctx=Ctx.empty, demand: Typ.t, binder: TPat.t): Id.Set.t => {
  let kept = alias_kept_params(~gamma, ~ctx, demand, binder);
  switch (TPat.alias_head(binder)) {
  | Some((_, params)) when params != [] =>
    List.fold_left(
      (acc, param) =>
        switch (TPat.tyvar_of_utpat(param)) {
        | Some(name) when List.mem(name, kept) => acc
        | _ => union(acc, note(TPat.rep_id(param)))
        },
      Id.Set.empty,
      params,
    )
  | _ => Id.Set.empty
  };
}

and alias_name = (binder: TPat.t): string =>
  switch (TPat.alias_head(binder)) {
  | Some((name, _)) => name
  | None =>
    switch (TPat.tyvars_of(binder)) {
    | [name, ..._] => name
    | [] => ""
    }
  }

and sibling_omissions = (e: Exp.t, child: Exp.t): Id.Set.t =>
  List.fold_left(
    (acc, c) =>
      Exp.rep_id(c) == Exp.rep_id(child) ? acc : union(acc, omit_exp(c)),
    Id.Set.empty,
    exp_children(e),
  )

and focused = (env: env, e: Exp.t, descent: list(Id.t), query: Typ.t): t =>
  switch (descent) {
  | [] => syn(env, e, query)
  | [next, ...rest] =>
    switch (List.find_opt(c => Exp.rep_id(c) == next, exp_children(e))) {
    | None => focused(env, e, rest, query)
    | Some(child) => discharge(env, e, child, focused(env, child, rest, query))
    }
  }

and discharge = (env: env, e: Exp.t, child: Exp.t, inner: t): t => {
  let ctx = ctx_at(env, e);
  let is_child = (c: Exp.t) => Exp.rep_id(c) == Exp.rep_id(child);
  let base = {
    kept: true,
    omitted: union(inner.omitted, sibling_omissions(e, child)),
    gamma: inner.gamma,
    psi: inner.psi,
  };
  let under_binder = (p: Pat.t, omitted: Id.Set.t): t => {
    let ps = slice_pat(env, p, ~query=gap, ~demands=inner.gamma);
    {
      kept: true,
      omitted: union(omitted, ps.omitted),
      gamma:
        SliceGamma.join(
          ctx,
          ps.gamma,
          SliceGamma.without(inner.gamma, Pat.bound_vars(p)),
        ),
      psi: inner.psi,
    };
  };
  switch (Exp.term_of(e)) {
  | Let(p, def, body) when is_child(body) =>
    let ps = slice_pat(env, p, ~query=gap, ~demands=inner.gamma);
    let d = syn(env, def, SliceQuery.residual(ctx, ps.phi, ps.supplied));
    {
      kept: true,
      omitted: unions([inner.omitted, ps.omitted, d.omitted]),
      gamma:
        SliceGamma.join_all(
          ctx,
          [ps.gamma, d.gamma, SliceGamma.without(inner.gamma, Pat.bound_vars(p))],
        ),
      psi: inner.psi,
    };
  | Let(p, _, _) => {...base, omitted: union(base.omitted, omit_pat(p))}
  | ModuleExp(mp, def, body) when is_child(body) =>
    let p = ExpandModule.mpat_to_pat(mp);
    let ps = slice_pat(env, p, ~query=gap, ~demands=inner.gamma);
    let d = syn(env, def, SliceQuery.residual(ctx, ps.phi, ps.supplied));
    {
      kept: true,
      omitted: unions([inner.omitted, ps.omitted, d.omitted]),
      gamma:
        SliceGamma.join_all(
          ctx,
          [
            ps.gamma,
            d.gamma,
            SliceGamma.without(inner.gamma, Pat.bound_vars(p)),
          ],
        ),
      psi: inner.psi,
    };
  | ModuleExp(mp, _, _) => {
      ...base,
      omitted: union(base.omitted, omit_pat(ExpandModule.mpat_to_pat(mp))),
    }
  | Fun(p, _, _, _)
  | FixF(p, _, _) => under_binder(p, base.omitted)
  | TypAbs(binder, body, _) =>
    let names = TPat.tyvars_of(binder);
    let used =
      mentions_tvar(names, inner.psi)
      || List.exists(
           name => List.mem(name, retained_tvars(inner.omitted, body)),
           names,
         );
    {
      ...base,
      omitted:
        union(
          base.omitted,
          used ? Id.Set.empty : tpat_omissions(binder),
        ),
      gamma: SliceGamma.without_tvars(inner.gamma, names),
    };
  | TyAlias(binder, definition, _) =>
    let name = alias_name(binder);
    let demand = SliceGamma.alias_demand(inner.gamma, name);
    {
      ...base,
      omitted:
        Ctx.lookup_alias(inner.gamma, name) == None
          ? unions([
              base.omitted,
              note(TPat.rep_id(binder)),
              note(Typ.rep_id(definition)),
            ])
          : union(
              base.omitted,
              alias_omissions(ctx, ~gamma=inner.gamma, demand, binder, definition),
            ),
      gamma: SliceGamma.without_tvars(inner.gamma, [name]),
    };
  | Match(_, rules) when List.for_all(((_, body)) => !is_child(body), rules) => {
      ...base,
      omitted:
        List.fold_left(
          (acc, (p, _)) => union(acc, omit_pat(p)),
          base.omitted,
          rules,
        ),
    }
  | Match(scrut, rules) =>
    switch (List.find_opt(((_, body)) => is_child(body), rules)) {
    | None => base
    | Some((p, _)) =>
      let ps = slice_pat(env, p, ~query=gap, ~demands=inner.gamma);
      let s = syn(env, scrut, SliceQuery.residual(ctx, ps.phi, ps.supplied));
      let siblings =
        List.fold_left(
          (acc, (q, body)) =>
            Pat.rep_id(q) == Pat.rep_id(p)
              ? acc : unions([acc, omit_pat(q), omit_exp(body)]),
          Id.Set.empty,
          rules,
        );
      {
        kept: true,
        omitted: unions([inner.omitted, siblings, ps.omitted, s.omitted]),
        gamma:
          SliceGamma.join_all(
            ctx,
            [ps.gamma, s.gamma, SliceGamma.without(inner.gamma, Pat.bound_vars(p))],
          ),
        psi: inner.psi,
      };
    }
  | TypAp(_, arg) => {
      ...base,
      omitted: union(base.omitted, SliceQuery.omit_typ(ctx, gap, arg)),
    }
  | DeferredAp(fn, args) when Exp.is_deferral(child) =>
    let position =
      List.fold_left(
        ((index, found), arg) =>
          Exp.is_deferral(arg)
            ? (index + 1, Exp.rep_id(arg) == Exp.rep_id(child) ? index : found)
            : (index, found),
        (0, 0),
        args,
      )
      |> snd;
    let deferrals = List.length(List.filter(Exp.is_deferral, args));
    let domain =
      SliceQuery.mk_prod(
        List.init(deferrals, i => i == position ? inner.psi : gap),
      );
    let f = syn(env, fn, SliceQuery.mk_arrow(domain, gap));
    {
      kept: true,
      omitted:
        List.fold_left(
          (acc, arg) => Exp.is_deferral(arg) ? acc : union(acc, omit_exp(arg)),
          union(inner.omitted, f.omitted),
          args,
        ),
      gamma: SliceGamma.join(ctx, inner.gamma, f.gamma),
      psi: inner.psi,
    };
  | _ => base
  };
}

and query_has_label = (ctx: Ctx.t, query: Typ.t, name: string): bool => {
  let labelled = item =>
    switch (SliceQuery.tup_label(ctx, item)) {
    | (Some(found), _) => found == name
    | _ => false
    };
  switch (Typ.term_of(SliceQuery.whnf(ctx, query))) {
  | Prod(items) => List.exists(labelled, items)
  | _ => labelled(query)
  };
}

and syn_module = (env: env, e: Exp.t, items: list(Mod.t), query: Typ.t): t => {
  let ctx = ctx_at(env, e);
  let (omitted, gamma) =
    List.fold_right(
      (item: Mod.t, (omitted, gamma)) =>
        switch (item.term) {
        | ModLet(p, def) =>
          let member =
            switch (Pat.bound_vars(p)) {
            | [name, ..._] => name
            | [] => ""
            };
          let demand =
            Typ.meet_gap(
              ctx,
              SliceQuery.labelled(ctx, query, member),
              SliceGamma.demand(gamma, member),
            );
          if (!query_has_label(ctx, query, member)
              && SliceQuery.is_gap(demand)) {
            (union(omitted, note(Mod.rep_id(item))), gamma);
          } else {
            let d = syn(env, def, demand);
            (
              union(omitted, d.omitted),
              SliceGamma.join(
                ctx,
                SliceGamma.without(gamma, [member]),
                d.gamma,
              ),
            );
          };
        | ModType(binder, definition) =>
          let name = alias_name(binder);
          Ctx.lookup_alias(gamma, name) == None
            ? (union(omitted, note(Mod.rep_id(item))), gamma)
            : (
              union(
                omitted,
                alias_omissions(
                  ctx,
                  ~gamma,
                  SliceGamma.alias_demand(gamma, name),
                  binder,
                  definition,
                ),
              ),
              SliceGamma.without_tvars(gamma, [name]),
            );
        | ModExp(inner) =>
          let r = syn(env, inner, query);
          (union(omitted, r.omitted), SliceGamma.join(ctx, gamma, r.gamma));
        | ModuleMod(_, inner) =>
          let r = syn(env, inner, query);
          (union(omitted, r.omitted), SliceGamma.join(ctx, gamma, r.gamma));
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) => (omitted, gamma)
        },
      items,
      (Id.Set.empty, SliceGamma.empty),
    );
  {
    kept: true,
    omitted,
    gamma,
    psi: ty_at(env, e),
  };
}

and retained_tvars = (omitted: Id.Set.t, e: Exp.t): list(string) => {
  let found = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, x) =>
          Id.Set.mem(Exp.rep_id(x), omitted) ? x : continue(x),
      ~f_pat=
        (continue, x) =>
          Id.Set.mem(Pat.rep_id(x), omitted) ? x : continue(x),
      ~f_typ=
        (continue, t) =>
          if (Id.Set.mem(Typ.rep_id(t), omitted)) {
            t;
          } else {
            switch (Typ.term_of(t)) {
            | Var(name) => found := [name, ...found^]
            | _ => ()
            };
            continue(t);
          },
      e,
    );
  found^;
}

and index_of = (children: list(Exp.t), child: Exp.t): int => {
  let rec go = (index, children) =>
    switch (children) {
    | [] => 0
    | [c, ...rest] => Exp.rep_id(c) == Exp.rep_id(child) ? index : go(index + 1, rest)
    };
  go(0, children);
}

and embed_at = (arity: int, index: int, demand: Typ.t): Typ.t =>
  SliceQuery.mk_prod(List.init(arity, i => i == index ? demand : gap))

and pattern_of = (e: Exp.t): option(Pat.t) =>
  switch (Exp.term_of(e)) {
  | Fun(p, _, _, _)
  | FixF(p, _, _)
  | Let(p, _, _)
  | Theorem(p, _, _)
  | Forall(p, _) => Some(p)
  | _ => None
  }

and ana_pat_at =
    (
      env: env,
      e: Exp.t,
      ~pat_focus: Id.t,
      ~pat_focus_var: option(string),
      query: Typ.t,
    )
    : (t, Typ.t) =>
  switch (pattern_of(e)) {
  | None => (nothing(e), gap)
  | Some(p) =>
    let lifted =
      pat_lift(p, pat_focus, query) |> Option.value(~default=gap);
    let demands =
      switch (pat_focus_var) {
      | None => SliceGamma.empty
      | Some(x) => SliceGamma.var(x, query)
      };
    let ps = slice_pat(env, p, ~query=lifted, ~demands);
    let ctx = ctx_at(env, e);
    (
      {
        kept: true,
        omitted:
          List.fold_left(
            (acc, c) => union(acc, omit_exp(c)),
            union(ps.omitted, SliceQuery.note(pat_focus)),
            exp_children(e),
          ),
        gamma: SliceGamma.without(ps.gamma, Pat.bound_vars(p)),
        psi: gap,
      },
      SliceQuery.supplies(ctx, lifted, ps.supplied) ? gap : lifted,
    );
  }

and focused_ana_pat =
    (
      env: env,
      e: Exp.t,
      descent: list(Id.t),
      ~pat_focus: Id.t,
      ~pat_focus_var: option(string),
      query: Typ.t,
    )
    : t => {
  let (r, _) =
    ana_up_from(env, e, descent, at =>
      ana_pat_start(env, at, ~pat_focus, ~pat_focus_var, query)
    );
  r;
}

and ana_pat_start =
    (
      env: env,
      e: Exp.t,
      ~pat_focus: Id.t,
      ~pat_focus_var: option(string),
      query: Typ.t,
    )
    : (t, Typ.t) => {
  let (at, unexplained) =
    ana_pat_at(env, e, ~pat_focus, ~pat_focus_var, query);
  switch (Exp.term_of(e)) {
  | Fun(_, _, _, _) when !SliceQuery.is_gap(unexplained) => (
      at,
      SliceQuery.mk_arrow(unexplained, gap),
    )
  | _ => (at, gap)
  };
}

and pat_lift = (p: Pat.t, focus: Id.t, query: Typ.t): option(Typ.t) =>
  if (List.mem(focus, IdTagged.ids(p))) {
    Some(query);
  } else {
    let sub = inner => pat_lift(inner, focus, query);
    let first = items =>
      List.fold_left(
        (acc, item) =>
          switch (acc) {
          | Some(_) => acc
          | None => sub(item)
          },
        None,
        items,
      );
    switch (Pat.term_of(p)) {
    | Parens(inner)
    | Projector(_, inner)
    | Asc(inner, _) => sub(inner)
    | Tuple(items) =>
      let arity = List.length(items);
      let rec go = (index, items) =>
        switch (items) {
        | [] => None
        | [item, ...rest] =>
          switch (sub(item)) {
          | Some(q) =>
            Some(
              SliceQuery.mk_prod(
                List.init(arity, j => j == index ? q : gap),
              ),
            )
          | None => go(index + 1, rest)
          }
        };
      go(0, items);
    | TupLabel(label, inner) =>
      switch (sub(inner)) {
      | None => None
      | Some(q) =>
        switch (Pat.term_of(label)) {
        | Label(name) => Some(SliceQuery.mk_tup_label(name, q))
        | _ => Some(q)
        }
      }
    | ListLit(items) =>
      switch (first(items)) {
      | None => None
      | Some(q) => Some(SliceQuery.mk_list(q))
      }
    | Cons(head, tail) =>
      switch (sub(head)) {
      | Some(q) => Some(SliceQuery.mk_list(q))
      | None => sub(tail)
      }
    | _ => None
    };
  }

and focused_ana = (env: env, e: Exp.t, descent: list(Id.t), query: Typ.t): t => {
  let (r, _) =
    ana_up_from(env, e, descent, at =>
      (
        {
          kept: false,
          omitted: omit_exp(at),
          gamma: alias_shapes(ctx_at(env, at), query),
          psi: gap,
        },
        query,
      )
    );
  r;
}

and ana_up_from =
    (env: env, e: Exp.t, descent: list(Id.t), at_end: Exp.t => (t, Typ.t))
    : (t, Typ.t) =>
  switch (descent) {
  | [] => at_end(e)
  | [next, ...rest] =>
    switch (List.find_opt(c => Exp.rep_id(c) == next, exp_children(e))) {
    | None => ana_up_from(env, e, rest, at_end)
    | Some(child) =>
      let (inner, demand) = ana_up_from(env, child, rest, at_end);
      SliceQuery.is_gap(demand)
        ? (discharge(env, e, child, inner), gap)
        : ana_step(env, e, child, inner, demand);
    }
  }
and ana_step =
    (env: env, e: Exp.t, child: Exp.t, inner: t, demand: Typ.t): (t, Typ.t) => {
  let ctx = ctx_at(env, e);
  let is_child = (c: Exp.t) => Exp.rep_id(c) == Exp.rep_id(child);
  let shell = discharge(env, e, child, inner);
  let locally = (~omitted=Id.Set.empty, ~gamma=SliceGamma.empty, ()) => (
    {
      kept: true,
      omitted: union(shell.omitted, omitted),
      gamma: SliceGamma.join(ctx, shell.gamma, gamma),
      psi: inner.psi,
    },
    gap,
  );
  switch (Exp.term_of(e)) {
  | Asc(body, ann) when is_child(body) =>
    locally(
      ~omitted=SliceQuery.omit_typ(ctx, demand, ann),
      ~gamma=
        alias_demands(
          ctx,
          ~omitted=SliceQuery.omit_typ(ctx, demand, ann),
          demand,
          ann,
        ),
      (),
    )
  | Ap(_, fn, arg) when is_child(arg) =>
    let f = syn(env, fn, SliceQuery.mk_arrow(demand, gap));
    (
      {
        kept: true,
        omitted: union(inner.omitted, f.omitted),
        gamma: SliceGamma.join(ctx, inner.gamma, f.gamma),
        psi: inner.psi,
      },
      gap,
    );
  | Parens(_)
  | Projector(_, _)
  | Use(_, _)
  | Filter(_, _) => (shell, demand)
  | Fun(_, body, _, _) when is_child(body) => (
      shell,
      SliceQuery.mk_arrow(gap, demand),
    )
  | TypAbs(binder, body, _) when is_child(body) => (
      shell,
      SliceQuery.mk_poly(gap_tpat(binder), demand),
    )
  | FixF(p, body, _) when is_child(body) =>
    let ps = slice_pat(env, p, ~query=demand, ~demands=SliceGamma.empty);
    (
      {
        kept: true,
        omitted:
          unions([inner.omitted, sibling_omissions(e, child), ps.omitted]),
        gamma: SliceGamma.join(ctx, inner.gamma, ps.gamma),
        psi: inner.psi,
      },
      Typ.subtract_uncollapsed(ctx, demand, ps.supplied),
    );
  | Dot(receiver, _) when is_child(receiver) => (
      {
        kept: true,
        omitted: inner.omitted,
        gamma: inner.gamma,
        psi: inner.psi,
      },
      gap,
    )
  | If(cond, _, _) when !is_child(cond) => (shell, demand)
  | Seq(left, _) when !is_child(left) => (shell, demand)
  | Let(p, def, _) when is_child(def) =>
    let ps = slice_pat(env, p, ~query=demand, ~demands=SliceGamma.empty);
    (
      {
        kept: true,
        omitted:
          unions([inner.omitted, sibling_omissions(e, child), ps.omitted]),
        gamma: SliceGamma.join(ctx, inner.gamma, ps.gamma),
        psi: inner.psi,
      },
      Typ.subtract_uncollapsed(ctx, demand, ps.supplied),
    );
  | Let(_, _, _) => (shell, demand)
  | ModuleExp(mp, def, _) when is_child(def) =>
    let ps =
      slice_pat(
        env,
        ExpandModule.mpat_to_pat(mp),
        ~query=demand,
        ~demands=SliceGamma.empty,
      );
    (
      {
        kept: true,
        omitted:
          unions([inner.omitted, sibling_omissions(e, child), ps.omitted]),
        gamma: SliceGamma.join(ctx, inner.gamma, ps.gamma),
        psi: inner.psi,
      },
      Typ.subtract_uncollapsed(ctx, demand, ps.supplied),
    );
  | ModuleExp(_, _, _) => (shell, demand)
  | Match(scrut, _) when !is_child(scrut) => (shell, demand)
  | TyAlias(_, _, _) => (shell, demand)
  | Tuple(items) => (
      shell,
      embed_at(List.length(items), index_of(items, child), demand),
    )
  | TupLabel(label, body) when is_child(body) => (
      shell,
      switch (Exp.term_of(label)) {
      | Label(name) => SliceQuery.mk_tup_label(name, demand)
      | _ => demand
      },
    )
  | Module(items) =>
    let member_of = (item: Mod.t) =>
      switch (item.term) {
      | ModLet(p, _) =>
        switch (Pat.bound_vars(p)) {
        | [member, ..._] => Some(member)
        | [] => None
        }
      | _ => None
      };
    let holds_child = (item: Mod.t) =>
      switch (item.term) {
      | ModLet(_, inner) => is_child(inner)
      | _ => false
      };
    let siblings =
      List.fold_left(
        (acc, item: Mod.t) =>
          holds_child(item) ? acc : union(acc, note(Mod.rep_id(item))),
        Id.Set.empty,
        items,
      );
    (
      {...shell, omitted: union(shell.omitted, siblings)},
      SliceQuery.mk_prod(
        List.filter_map(
          (item: Mod.t) =>
            switch (member_of(item)) {
            | None => None
            | Some(member) =>
              Some(
                holds_child(item)
                  ? SliceQuery.mk_tup_label(member, demand) : gap,
              )
            },
          items,
        ),
      ),
    );
  | ListLit(_) => (shell, SliceQuery.mk_list(demand))
  | Cons(head, _) when is_child(head) => (shell, SliceQuery.mk_list(demand))
  | Cons(head, _) =>
    let h = syn(env, head, SliceQuery.list_elem(ctx, demand));
    (
      {
        kept: true,
        omitted: union(inner.omitted, h.omitted),
        gamma: SliceGamma.join(ctx, inner.gamma, h.gamma),
        psi: inner.psi,
      },
      gap,
    );
  | ListConcat(_, _) => (shell, demand)
  | _ => locally()
  };
};
