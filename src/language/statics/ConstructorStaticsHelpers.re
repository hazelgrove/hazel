include StaticsBase;

let free_constructor_syn_ty = (name: Constructor.t): Typ.t =>
  Sum([
    ConstructorMap.Variant(
      name,
      ConstructorMap.mk_variant_ann(~ids=[Id.invalid], ()),
      None,
    ),
    ConstructorMap.BadEntry(SynTy.unknown_internal()),
  ])
  |> Typ.temp;

let syn_marks_match =
    (ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): (Typ.t, list(Mark.t)) =>
  switch (Typ.meet_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, tys)) {
  | None => (
      SynTy.meet_of(Id, SynTy.unknown_internal()),
      [Mark.NoMeet(Id, Typ.add_source(ids, tys))],
    )
  | Some(ty) => (ty, [])
  };

let ctr_ana_typ =
    (ctx: Ctx.t, ty_ana: Typ.t, ctr: Constructor.t): option(Typ.t) => {
  Util.OptUtil.Syntax.(
    switch (ty_ana) {
    | {term: Arrow(_, ty_out), _} =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_out);
      let* ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => None
      | Some(ty_in) => Some(Arrow(ty_in, ty_out) |> Typ.temp)
      };
    | _ =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_ana);
      let+ ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => ty_ana
      | Some(ty_in) => Arrow(ty_in, ty_ana) |> Typ.temp
      };
    }
  );
};

let syn_marks_ctr =
    (ctx: Ctx.t, name: Constructor.t, ana: Typ.t, ty: option(option(Typ.t)))
    : (Typ.t, list(Mark.t)) =>
  switch (ty) {
  | Some(Some(ty)) => (ty, [])
  | Some(None) => (
      free_constructor_syn_ty(name),
      [Mark.FreeConstructor(name)],
    )
  | None =>
    switch (ctr_ana_typ(ctx, ana, name)) {
    | Some(ty) => (ty, [])
    | None =>
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some({typ, _}) => (typ, [])
      | None => (
          free_constructor_syn_ty(name),
          [Mark.FreeConstructor(name)],
        )
      }
    }
  };

/* Left-to-right type-application spine of a `TypParamAp` chain. The
   multi-arg form `TypParamAp(T, TypTuple([a, b]))` flattens to
   `[a, b]`; a curried chain `TypParamAp(TypParamAp(T, a), b)` (no
   longer produced by elaboration but accepted defensively) flattens
   to the same. */
let typ_param_ap_spine = (ty: Typ.t): list(Typ.t) => {
  let rec go = (ty: Typ.t, acc) =>
    switch (ty.term) {
    | TypParamAp(fn, {term: TypTuple(args), _}) => go(fn, args @ acc)
    | TypParamAp(fn, arg) => go(fn, [arg, ...acc])
    | _ => acc
    };
  go(ty, []);
};

/* Type-parameter arity of a constructor's schema. A multi-parameter
   alias has a single `Poly(TPat.Tuple([…]), …)` binder; arity is the
   length of that tuple. */
let schema_arity = (ty: Typ.t): int => {
  let rec go = (ty: Typ.t, n) =>
    switch (ty.term) {
    | Poly(b, body) =>
      let arity = List.length(TPat.binders_of(b));
      go(body, n + arity);
    | _ => n
    };
  go(ty, 0);
};

/* Build a `TypAp` around `ctor` that supplies `args` as a single
   tuple-argument bundle (`TypAp(ctor, arg)` for one arg, else
   `TypAp(ctor, TypTuple(args))`). */
let wrap_typ_param_aps = (ctor: Exp.t, args: list(Typ.t)): Exp.t =>
  switch (args) {
  | [] => ctor
  | [arg] => TypAp(ctor, arg) |> Exp.fresh
  | _ => TypAp(ctor, TypTuple(args) |> Typ.fresh) |> Exp.fresh
  };

/* Resolve surface wrappers and kind-`Type` aliases without unrolling
   `Rec` or unfolding type-constructor aliases. Used by constructor
   instantiation to read the user-visible type-application spine while
   preserving `TypParamAp(Var("List"), Int)` so we can pick `[Int]`
   off as an argument; an alias like `type IntList = List(Int)` still
   reduces to the same spine. */
let rec surface_resolve = (ctx: Ctx.t, ty: Typ.t): Typ.t =>
  switch (ty.term) {
  | Parens(inner)
  | Projector(_, inner) => surface_resolve(ctx, inner)
  | Var(name) =>
    switch (Ctx.lookup_tvar_typ_kind(ctx, name)) {
    | Some(TypKind.Type) =>
      switch (Ctx.lookup_alias(ctx, name)) {
      | Some(aliased) => surface_resolve(ctx, aliased)
      | None => ty
      }
    | Some(TypKind.Unknown | TypKind.Arrow(_, _))
    | None => ty
    }
  | _ => ty
  };

let rec result_of_arrow_surface = (ctx: Ctx.t, ty: Typ.t): Typ.t => {
  let ty = surface_resolve(ctx, ty);
  switch (ty.term) {
  | Arrow(_, out) => result_of_arrow_surface(ctx, out)
  | _ => ty
  };
};

/* Instantiation args for a polymorphic constructor whose schema arity
   is `n` and whose result-type's surface form mentions an analysis
   target. Returns `[]` for monomorphic constructors and for the
   `Poly(_, _)` analysis case (the surrounding `Statics.TypAp` will
   instantiate explicitly). When the analysis target doesn't expose
   a usable `TypParamAp` spine (e.g. `fun x -> Some(x)` where `ana`
   is `Unknown`), falls back to `Unknown(Internal)` for each missing
   argument so the schema still specializes to a monomorphic shape
   the runtime can handle. */
let instantiation_args_for =
    (ctx: Ctx.t, name: Constructor.t, ana: Typ.t): list(Typ.t) =>
  switch (Ctx.lookup_ctr(ctx, name)) {
  | Some({typ, _}) when schema_arity(typ) > 0 =>
    switch (Typ.term_of(ana)) {
    | Poly(_, _) => []
    | _ =>
      let arity = schema_arity(typ);
      let target = result_of_arrow_surface(ctx, ana);
      let args = typ_param_ap_spine(target);
      if (List.length(args) == arity) {
        args;
      } else {
        List.init(arity, _ => Unknown(Internal) |> Typ.fresh);
      };
    }
  | _ => []
  };

// The alias a constructor belongs to is the head of its result type.
let rec result_head = (ty: Typ.t): option(string) =>
  switch (Typ.term_of(ty)) {
  | Arrow(_, out) => result_head(out)
  | Parens(inner)
  | Rec(_, inner)
  | Poly(_, inner)
  | TypFun(_, inner) => result_head(inner)
  | TypParamAp(head, _) => result_head(head)
  | Var(name) => Some(name)
  | _ => None
  };

let alias_of_ctr = (ctx: Ctx.t, ctr: Constructor.t): option(Ctx.tvar_entry) =>
  Util.OptUtil.Syntax.(
    let* {typ, _}: Ctx.var_entry = Ctx.lookup_ctr(ctx, ctr);
    let* name = result_head(typ);
    List.find_map(
      fun
      | Ctx.TVarEntry(entry) when entry.name == name => Some(entry)
      | _ => None,
      ctx.entries,
    )
  );

let rec minimal_definition =
        (
          ctx: Ctx.t,
          ctr: Constructor.t,
          payload: option(Typ.t),
          definition: Typ.t,
        )
        : Typ.t => {
  let (term, rewrap) = Typ.unwrap(definition);
  switch (term) {
  | TypFun(binder, body) =>
    TypFun(binder, minimal_definition(ctx, ctr, payload, body)) |> rewrap
  | Rec(binder, body) =>
    Rec(binder, minimal_definition(ctx, ctr, payload, body)) |> rewrap
  | Poly(binder, body) =>
    Poly(binder, minimal_definition(ctx, ctr, payload, body)) |> rewrap
  | Parens(inner) =>
    Parens(minimal_definition(ctx, ctr, payload, inner)) |> rewrap
  | Sum(variants) =>
    Sum(
      List.map(
        fun
        | ConstructorMap.Variant(name, ann, arg)
            when Constructor.equal(name, ctr) =>
          ConstructorMap.Variant(
            name,
            ann,
            switch (payload) {
            | Some(payload) => Option.map(_ => payload, arg)
            | None => arg
            },
          )
        | _ => ConstructorMap.BadEntry(Typ.gap),
        variants,
      ),
    )
    |> rewrap
  | _ => definition
  };
};

let ctr_payload = (ctx: Ctx.t, ctr: Constructor.t, ty: Typ.t): option(Typ.t) =>
  switch (MatchedTyp.strict2(MatchedTyp.arrow, ctx, ty)) {
  | Some((payload, _)) when !Typ.is_gap(payload) => Some(payload)
  | matched =>
    Typ.get_sum_constructors(ctx, Option.fold(~none=ty, ~some=snd, matched))
    |> Option.map(ConstructorMap.get_entry(ctr))
    |> Option.join
    |> Option.join
  };

let alias_demand_of =
    (
      ctx: Ctx.t,
      ~definition: Typ.t,
      ~fallback: Typ.t,
      lookup_ctr: Constructor.t => Typ.t,
    )
    : Typ.t => {
  let demands =
    switch (Typ.get_sum_constructors(ctx, definition)) {
    | None => []
    | Some(variants) =>
      List.filter_map(
        fun
        | ConstructorMap.BadEntry(_) => None
        | ConstructorMap.Variant(ctr, _, _) => {
            let ty = lookup_ctr(ctr);
            let payload = ctr_payload(ctx, ctr, ty);
            Typ.is_gap(ty)
              ? None
              : Some(minimal_definition(ctx, ctr, payload, definition));
          },
        variants,
      )
    };
  demands == [] ? fallback : Typ.meet_gap_all(ctx, demands);
};

let alias_demand =
    (
      ~from_pattern: bool,
      ctx: Ctx.t,
      ctr: Constructor.t,
      entry: Ctx.tvar_entry,
      query: Typ.t,
    )
    : Typ.t =>
  switch (entry.kind) {
  | Abstract => Typ.gap
  | Singleton(definition) =>
    let payload =
      from_pattern
        ? None
        : Some(
            ctr_payload(ctx, ctr, query) |> Option.value(~default=Typ.gap),
          );
    minimal_definition(ctx, ctr, payload, definition);
  };

let ctr_uses =
    (
      ~from_pattern=false,
      ctx: Ctx.t,
      ~ctr: Constructor.t,
      ~id: Id.t,
      ~ana: Typ.t,
      ~typ: Typ.t,
    )
    : CoCtx.t =>
  [
    CoCtx.singleton(
      ~sort=CoCtx.Constructor,
      ~demanded=from_pattern ? None : Some(typ),
      ctr,
      id,
      ana,
    ),
  ]
  @ (
    switch (alias_of_ctr(ctx, ctr)) {
    | Some(entry) => [
        CoCtx.singleton(
          ~sort=CoCtx.Alias,
          ~demanded=Some(alias_demand(~from_pattern, ctx, ctr, entry, ana)),
          entry.name,
          id,
          ana,
        ),
      ]
    | None => []
    }
  )
  |> CoCtx.union;

// A constructor pattern's payload, as a component of the type it matches.
let payload_former = (ctr: Constructor.t): MatchedTyp.former => {
  match_: (ctx, ty) =>
    Typ.get_sum_constructors(ctx, ty)
    |> Option.map(
         List.find_map(
           fun
           | ConstructorMap.Variant(name, _, payload)
               when Constructor.equal(name, ctr) =>
             Some([payload |> Option.value(~default=Typ.gap)])
           | _ => None,
         ),
       )
    |> Option.join,
  build:
    fun
    | [payload] =>
      Sum([
        ConstructorMap.Variant(
          ctr,
          ConstructorMap.empty_variant_ann,
          Some(payload),
        ),
      ])
      |> Typ.temp
    | _ => Typ.gap,
};
