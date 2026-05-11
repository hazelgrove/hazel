/* Module-related helper functions used during statics/elaboration.
   These operate on expression and pattern ASTs to handle module
   type references, signature pattern stripping, and body ID restoration. */

/* Collect module variable references from type annotations.
   For M.T (= ProdProjection(Var("M"), Label("T"))), free_vars returns ["M"].
   We filter to names that are expression variables (not type aliases). */
let collect_module_refs_in_typ = (ctx: Ctx.t, id: Id.t, typ: Typ.t): CoCtx.t => {
  Typ.free_vars(typ)
  |> List.filter_map(name =>
       switch (Ctx.lookup_var(ctx, name)) {
       | Some(_) =>
         Some(
           CoCtx.singleton(
             name,
             id,
             Unknown(Internal |> Prov.fresh) |> Typ.temp,
           ),
         )
       | None => None
       }
     )
  |> CoCtx.union;
};

/* Walk a Pat.t AST and collect module refs from any Asc(_, ann) sub-patterns. */
let rec collect_pat_type_refs = (ctx: Ctx.t, pat: Pat.t): CoCtx.t =>
  switch (pat.term) {
  | Asc(p, ann) =>
    CoCtx.union([
      collect_module_refs_in_typ(ctx, Typ.rep_id(ann), ann),
      collect_pat_type_refs(ctx, p),
    ])
  | Tuple(ps)
  | ListLit(ps) => CoCtx.union(List.map(collect_pat_type_refs(ctx), ps))
  | Cons(p1, p2)
  | TupLabel(p1, p2)
  | Ap(p1, p2) =>
    CoCtx.union([
      collect_pat_type_refs(ctx, p1),
      collect_pat_type_refs(ctx, p2),
    ])
  | Parens(p)
  | Projector(_, p) => collect_pat_type_refs(ctx, p)
  | _ => CoCtx.empty
  };

/* Recursively strip ascription annotations from module patterns
   in let/tyalias expressions. */
let rec strip_module_sig_pats = (exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  | Let({term: Asc(p, _), _}, def, body) =>
    Let(
      strip_module_sig_pats_in_pat(p),
      strip_module_sig_pats(def),
      strip_module_sig_pats(body),
    )
    |> rewrap
  | Let(p, def, body) =>
    Let(
      strip_module_sig_pats_in_pat(p),
      strip_module_sig_pats(def),
      strip_module_sig_pats(body),
    )
    |> rewrap
  | TyAlias(tpat, typ, body) =>
    TyAlias(tpat, typ, strip_module_sig_pats(body)) |> rewrap
  | Parens(inner) => Parens(strip_module_sig_pats(inner)) |> rewrap
  | _ => exp
  };
}
and strip_module_sig_pats_in_pat = (pat: Pat.t): Pat.t => {
  let (term, rewrap) = Pat.unwrap(pat);
  switch (term) {
  | Asc(inner, _) => strip_module_sig_pats_in_pat(inner)
  | Parens(inner) => Parens(strip_module_sig_pats_in_pat(inner)) |> rewrap
  | _ => pat
  };
};

/* Restores a module body's ID when it's been lost during tuple elaboration.
   Walks nested Let/TyAlias/Parens until a Tuple is found, then copies the ID. */
let rec restore_module_body_id = (~id, exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  | Let(p, def, body) =>
    Let(p, def, restore_module_body_id(~id, body)) |> rewrap
  | TyAlias(tpat, typ, body) =>
    TyAlias(tpat, typ, restore_module_body_id(~id, body)) |> rewrap
  | Parens(inner) => Parens(restore_module_body_id(~id, inner)) |> rewrap
  | Tuple(_) => IdTagged.fast_copy(id, exp)
  | _ => exp
  };
};

/* Rewrite InfoExp cls for expanded module items to keep cursor inspector labels. */
let reclassify_expanded_module_items =
    (items: list(Mod.t), m: StaticsBase.Map.t) =>
  List.fold_left(
    (m, item: Mod.t) => {
      let ids = IdTagged.ids(item);
      let mod_cls = Cls.Mod(Mod.cls_of_term(item.term));
      switch (StaticsBase.Map.lookup_exp(IdTagged.rep_id(item), m)) {
      | Some(info) =>
        StaticsBase.Map.add_info(
          ids,
          Info.InfoExp({
            ...info,
            cls: mod_cls,
          }),
          m,
        )
      | None => m
      };
    },
    m,
    items,
  );

/* Construct module export product type from non-shadowed module bindings. */
let module_actual_type = (items: list(Mod.t), m: StaticsBase.Map.t): Typ.t => {
  let non_shadowed = ExpandModule.compute_non_shadowed_bindings(items);
  let fields =
    non_shadowed
    |> List.map(((name, pat)) => {
         let ty =
           switch (StaticsBase.Map.lookup_pat(Pat.rep_id(pat), m)) {
           | Some({ty, ctx: pat_ctx, _}) => Typ.normalize(pat_ctx, ty)
           | None => Typ.temp(Unknown(Internal |> Prov.fresh))
           };
         TupLabel(Label(name) |> Typ.temp, ty) |> Typ.temp;
       });
  Prod(fields) |> Typ.temp;
};

/* Post-process expanded module elaboration to hide expansion-only wrappers. */
let module_elab = (~module_exp_id: Id.t, expanded_elab: Exp.t): Exp.t =>
  expanded_elab
  |> strip_module_sig_pats
  |> restore_module_body_id(~id=module_exp_id);

/* Rebuild ModuleExp elaboration with direct def elaboration preserved. */
let moduleexp_elab = (~def_elab_direct: Exp.t, expanded_elab: Exp.t): Exp.t => {
  let (expanded_term, expanded_rewrap) = Exp.unwrap(expanded_elab);
  switch (expanded_term) {
  | Let(p_elab, _, body_elab) =>
    Let(strip_module_sig_pats_in_pat(p_elab), def_elab_direct, body_elab)
    |> expanded_rewrap
  | _ => strip_module_sig_pats(expanded_elab)
  };
};
