/* Kinds of types: a type function has an arrow kind, every other type
   `Type`. Type variables take their kind from the context. */

/* The kind of [ty] in [ctx]. A free type variable has the unknown kind,
   so `L(a)` with `L` unbound gets only the free-variable mark, not also
   one for applying it. */
let rec kind_of_typ = (ctx: Ctx.t, ty: Typ.t): TypKind.t => {
  let type_ = TypKind.Type;
  switch (ty.term) {
  | Unknown(_) => TypKind.Unknown
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel => type_
  | Var(name) =>
    switch (Ctx.lookup_tvar_typ_kind(ctx, name)) {
    | Some(kind) => kind
    | None when Ctx.is_base_typ(name) => TypKind.Type
    | None => TypKind.Unknown
    }
  | Parens(t)
  | Projector(_, t) => kind_of_typ(ctx, t)
  | ProdProjection(_) =>
    /* `M.T`: resolve through the module's labeled-tuple exports so
       a parameterized export reports its full `(Type, …) -> Type`
       kind. Same path `weak_head_normalize` follows. */
    let resolved = Typ.weak_head_normalize(ctx, ty);
    switch (resolved.term) {
    | ProdProjection(_) => type_
    | Unknown(_)
    | Atom(_)
    | DrvQuoteTy(_)
    | Label(_)
    | ExplicitNonlabel
    | Var(_)
    | Parens(_)
    | Projector(_)
    | List(_)
    | Arrow(_)
    | TupLabel(_)
    | ProdExtension(_)
    | Prod(_)
    | Sum(_)
    | Poly(_)
    | ProofOf(_)
    | Sig(_)
    | TypTuple(_)
    | TypFun(_)
    | TypParamAp(_)
    | Rec(_) => kind_of_typ(ctx, resolved)
    };
  | List(_)
  | Arrow(_)
  | TupLabel(_)
  | ProdExtension(_)
  | Prod(_)
  | Sum(_)
  | Poly(_)
  | ProofOf(_)
  | Sig(_) => type_
  | TypTuple(_) =>
    /* No kind on its own — only ever the second arg of `TypParamAp`,
       checked there. `Type` is a neutral fallback so an isolated
       `TypTuple` doesn't poison surrounding kind checks. */
    type_
  | TypFun(param, body) =>
    let n = List.length(TPat.binders_of(param));
    let body_ctx = Ctx.extend_dummy_tvar(ctx, param);
    TypKind.arrows(List.init(n, _ => type_), kind_of_typ(body_ctx, body));
  | TypParamAp(fn, arg) =>
    let fn_kind = kind_of_typ(ctx, fn);
    let arg_kinds =
      switch (arg.term) {
      | TypTuple(ts) => List.map(kind_of_typ(ctx), ts)
      | Unknown(_)
      | Atom(_)
      | DrvQuoteTy(_)
      | Label(_)
      | ExplicitNonlabel
      | Var(_)
      | Parens(_)
      | Projector(_)
      | ProdProjection(_)
      | List(_)
      | Arrow(_)
      | TupLabel(_)
      | ProdExtension(_)
      | Prod(_)
      | Sum(_)
      | Poly(_)
      | ProofOf(_)
      | Sig(_)
      | TypFun(_)
      | TypParamAp(_)
      | Rec(_) => [kind_of_typ(ctx, arg)]
      };
    switch (TypKind.apply_all(fn_kind, arg_kinds)) {
    | Some(result) => result
    | None => type_
    };
  | Rec(param, body) =>
    let body_ctx = Ctx.extend_dummy_tvar(ctx, param);
    kind_of_typ(body_ctx, body);
  };
};
