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

/* Extract a left-to-right type-application spine from a type, e.g.
   TypApp(TypApp(List, Int), Bool) -> [Int, Bool]. */
let type_app_spine = (ty: Typ.t): list(Typ.t) => {
  let rec go = (ty: Typ.t, acc) =>
    switch (ty.term) {
    | TypApp(fn, arg) => go(fn, [arg, ...acc])
    | _ => acc
    };
  go(ty, []);
};

/* Count the outermost Poly binders in a constructor's schema, e.g.
   `poly a -> poly b -> ...` has arity 2. */
let schema_arity = (ty: Typ.t): int => {
  let rec go = (ty: Typ.t, n) =>
    switch (ty.term) {
    | Poly(_, body) => go(body, n + 1)
    | _ => n
    };
  go(ty, 0);
};

/* Build `TypAp(...TypAp(ctor, arg1), argN)` using fresh wrapping so the
   elaborated form preserves the constructor's specialization after
   re-parsing. */
let wrap_type_apps = (ctor: Exp.t, args: list(Typ.t)): Exp.t =>
  List.fold_left((acc, arg) => TypAp(acc, arg) |> Exp.fresh, ctor, args);

/* Resolve surface wrappers and `Type`-kinded aliases without unrolling
   `Rec` or unfolding type-constructor aliases. Used by constructor
   instantiation to read the user-visible type-application spine. Unlike
   `Typ.normalize`, this preserves `TypApp(Var("List"), Int)` so we can
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
   not polymorphic or we can't determine the args. Kind-Type aliases are
   resolved so an alias for `List(Int)` still exposes the `[Int]` spine. */
let instantiation_args_for =
    (ctx: Ctx.t, name: Constructor.t, ana: Typ.t): list(Typ.t) =>
  switch (Ctx.lookup_ctr(ctx, name)) {
  | Some({typ, _}) when schema_arity(typ) > 0 =>
    let arity = schema_arity(typ);
    let target = result_of_arrow_surface(ctx, ana);
    let args = type_app_spine(target);
    /* Only wrap when the spine length matches the schema arity so we don't
       emit partial specializations that would fail re-statics. */
    List.length(args) == arity ? args : [];
  | _ => []
  };
