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

/* Replace Rec("name", ...) with Var("name") for unshadowed builtin type
   aliases (HTML, Attr, Cmd, Sub). Builtin aliases use Var references in
   Ctx.add_ctrs so constructor type annotations stay compact. Compactness
   enables the Var-Var fast path in Typ.meet during post-eval statics —
   without it, statics on full HTML apps takes ~2s instead of ~4ms.
   Ascriptions.re resolves the Var lazily via weak_head_normalize. */
let rec compact_builtin_recs = (ctx: Ctx.t, ty: Typ.t): Typ.t => {
  let is_builtin_alias = (name: string): bool =>
    List.exists(
      ~f=((n, _)) => String.equal(n, name),
      BuiltinsADT.type_aliases,
    )
    && Option.equal(
         Id.equal,
         Ctx.lookup_tvar_id(ctx, name),
         Some(Id.invalid),
       );
  switch (Typ.term_of(ty)) {
  | Rec(tp, _) =>
    switch (TPat.tyvar_of_utpat(tp)) {
    | Some(name) when is_builtin_alias(name) => Var(name) |> Typ.temp
    | _ => ty
    }
  | Arrow(t1, t2) =>
    let t1' = compact_builtin_recs(ctx, t1);
    let t2' = compact_builtin_recs(ctx, t2);
    if (phys_equal(t1', t1) && phys_equal(t2', t2)) {
      ty;
    } else {
      Arrow(t1', t2') |> Typ.temp;
    };
  | List(t) =>
    let t' = compact_builtin_recs(ctx, t);
    if (phys_equal(t', t)) {
      ty;
    } else {
      List(t') |> Typ.temp;
    };
  | Prod(ts) =>
    let ts' = List.map(~f=compact_builtin_recs(ctx), ts);
    if (List.for_all2_exn(~f=phys_equal, ts, ts')) {
      ty;
    } else {
      Prod(ts') |> Typ.temp;
    };
  | TupLabel(l, t) =>
    let t' = compact_builtin_recs(ctx, t);
    if (phys_equal(t', t)) {
      ty;
    } else {
      TupLabel(l, t') |> Typ.temp;
    };
  | Parens(t) =>
    let t' = compact_builtin_recs(ctx, t);
    if (phys_equal(t', t)) {
      ty;
    } else {
      Parens(t') |> Typ.temp;
    };
  | _ => ty
  };
};

/* Normalize a constructor's type annotation, but keep it compact when the
   constructor's return type is a builtin alias. If the type already has a
   compact builtin Var return, leave it alone. Otherwise compact any
   expanded builtin Rec types back to Var; if no compaction happened, fall
   back to plain normalize. */
let normalize_ctr_type = (ctx: Ctx.t, ty: Typ.t): Typ.t => {
  let return_type_name =
    switch (Typ.term_of(ty)) {
    | Var(name) => Some(name)
    | Arrow(_, {term: Var(name), _}) => Some(name)
    | _ => None
    };
  switch (return_type_name) {
  | Some(name)
      when
        List.exists(
          ~f=((n, _)) => String.equal(n, name),
          BuiltinsADT.type_aliases,
        )
        && Option.equal(
             Id.equal,
             Ctx.lookup_tvar_id(ctx, name),
             Some(Id.invalid),
           ) => ty
  | _ =>
    let compacted = compact_builtin_recs(ctx, ty);
    if (!phys_equal(compacted, ty)) {
      compacted;
    } else {
      Typ.normalize(ctx, ty);
    };
  };
};
