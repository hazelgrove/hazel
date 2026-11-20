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
    /* Normalize the head so an alias for an arrow (or a parenthesized
       arrow) is recognized as one. */
    switch (Typ.term_of(Typ.weak_head_normalize(ctx, ty_ana))) {
    | Arrow(_, ty_out) =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_out);
      let* ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => None
      | Some(ty_in) => Some(Arrow(ty_in, ty_out) |> Typ.temp)
      };
    | Unknown(_)
    | Atom(_)
    | DrvQuoteTy(_)
    | Var(_)
    | List(_)
    | Sum(_)
    | Prod(_)
    | ExplicitNonlabel
    | Label(_)
    | TupLabel(_)
    | Parens(_)
    | Projector(_)
    | Rec(_)
    | Poly(_)
    | ProofOf(_)
    | ProdProjection(_)
    | ProdExtension(_)
    | Sig(_) =>
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
