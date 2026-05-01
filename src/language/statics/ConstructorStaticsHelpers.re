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

/* Extract the result (non-Arrow) type of a schema after peeling arrows. */
let rec result_of_arrow = (ty: Typ.t): Typ.t =>
  switch (ty.term) {
  | Arrow(_, out) => result_of_arrow(out)
  | _ => ty
  };

/* Extract a left-to-right type-application spine from a type. Handles
   both the new multi-arg form `TypParamAp(T, TypTuple([a, b]))` and the
   curried-by-elaboration form `TypParamAp(TypParamAp(T, a), b)` (which can
   still appear in some constructor schemas). For the new form the
   spine is the TypTuple's contents in order. */
let typ_param_ap_spine = (ty: Typ.t): list(Typ.t) => {
  let rec go = (ty: Typ.t, acc) =>
    switch (ty.term) {
    | TypParamAp(fn, {term: TypTuple(args), _}) => go(fn, args @ acc)
    | TypParamAp(fn, arg) => go(fn, [arg, ...acc])
    | _ => acc
    };
  go(ty, []);
};

/* Count the type-parameter arity of a constructor's schema. With the
   multi-binder representation, a multi-parameter type's schema is a
   single `Poly` whose binder is a `TPat.Tuple([…])`, so the arity is
   the length of that tuple. Single-parameter schemas are
   `Poly(<var>, …)` with arity 1, and the legacy curried form
   `Poly(_, Poly(_, …))` (used by explicit nesting) flattens via the
   recursive count. */
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
   tuple-argument bundle. For 1 arg we produce `TypAp(ctor, arg)`; for
   ≥2 args we wrap the args in a `TypTuple` so the elaboration mirrors
   the source-level multi-argument application
   `Cons(0, Nil) : List(Int)`  →  `TypAp(Cons, TypTuple([Int]))` (1 arg
   case keeps a bare single arg for cleaner display) and
   `A(3) : Either(Int, Bool)` →  `TypAp(A, TypTuple([Int, Bool]))`. */
let wrap_typ_param_aps = (ctor: Exp.t, args: list(Typ.t)): Exp.t =>
  switch (args) {
  | [] => ctor
  | [arg] => TypAp(ctor, arg) |> Exp.fresh
  | _ => TypAp(ctor, TypTuple(args) |> Typ.fresh) |> Exp.fresh
  };

/* Resolve surface wrappers and `Type`-kinded aliases without unrolling
   `Rec` or unfolding type-constructor aliases. Used by constructor
   instantiation to read the user-visible type-application spine. Unlike
   `Typ.normalize`, this preserves `TypParamAp(Var("List"), Int)` so we can
   pick out `[Int]` as the argument, while still letting aliases such as
   `type IntList = List(Int)` reduce to the same spine. */
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
    | _ => ty
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

/* Given a constructor name with expected type `ana`, determine the type
   arguments to specialize its schema. Returns [] when the constructor is
   not polymorphic. Kind-Type aliases are resolved so an alias for
   `List(Int)` still exposes the `[Int]` spine.

   When the analysis target doesn't carry a usable `TypParamAp` spine
   (typically because `ana` is `Unknown(_)` from a gradually-typed
   surrounding context, e.g. `fun x -> Some(x)`), fall back to
   inserting `Unknown(Internal)` for each missing type argument so
   the polymorphic schema still gets specialized — the resulting
   `Some@<?>` reduces to a monomorphic `Some : ? -> Option(?)` and
   the runtime can match the constructor as usual.

   Exception: when `ana` is `Poly(_, _)` we are being analyzed
   *as the operand of a `TypAp`* (the surrounding `Statics.TypAp`
   sets `typfn_ana = Poly(EmptyHole, syn)`). The user's explicit
   `e@<T>` will perform the instantiation, so we leave the
   constructor's polymorphic schema in place and don't insert our
   own auto-instantiation. */
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
