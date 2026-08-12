/* Module-related helper functions used during statics/elaboration. */

type value_export = {
  name: Var.t,
  pat: Pat.t,
};

type lowered = {
  expanded: Exp.t,
  value_exports: list(value_export),
  type_exports: list((Var.t, Typ.t)),
};

/* Collect module variable references from type annotations.
   For M.T (= ProdProjection(Var("M"), Label("T"))), free_vars returns ["M"].
   We filter to names that are expression variables (not type aliases). */
let collect_module_refs_in_typ = (ctx: Ctx.t, id: Id.t, typ: Typ.t): CoCtx.t => {
  Typ.free_vars(typ)
  |> List.filter_map(name =>
       switch (Ctx.lookup_var(ctx, name)) {
       | Some(_) =>
         Some(CoCtx.singleton(name, id, Unknown(Internal) |> Typ.temp))
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

let rec mpat_to_pat = (mp: MPat.t): Pat.t =>
  switch (mp.term) {
  | Var(name) => IdTagged.fast_copy(MPat.rep_id(mp), Pat.fresh(Var(name)))
  | Asc(inner, typ) =>
    IdTagged.fast_copy(
      MPat.rep_id(mp),
      Pat.fresh(Asc(mpat_to_pat(inner), Typ.desugar_sig(Ctx.empty, typ))),
    )
  | _ => IdTagged.fast_copy(MPat.rep_id(mp), Pat.fresh(Wild))
  };

let rec mpat_names = (mp: MPat.t): list(Var.t) =>
  switch (mp.term) {
  | Var(name) => [name]
  | Asc(inner, _) => mpat_names(inner)
  | _ => []
  };

let single_bound_var = (p: Pat.t): option(Var.t) =>
  switch (Pat.bound_vars(p)) {
  | [name] => Some(name)
  | _ => None
  };

let rec pat_for_bound_name = (name: Var.t, pat: Pat.t): Pat.t =>
  switch (pat.term) {
  | Var(n) when n == name => pat
  | Asc(inner, _) => pat_for_bound_name(name, inner)
  | Tuple(ps)
  | ListLit(ps) =>
    ps
    |> List.find_opt(p => List.mem(name, Pat.bound_vars(p)))
    |> Option.map(pat_for_bound_name(name))
    |> Option.value(~default=pat)
  | Cons(p1, p2)
  | TupLabel(p1, p2)
  | Ap(p1, p2) =>
    if (List.mem(name, Pat.bound_vars(p1))) {
      pat_for_bound_name(name, p1);
    } else if (List.mem(name, Pat.bound_vars(p2))) {
      pat_for_bound_name(name, p2);
    } else {
      pat;
    }
  | Parens(inner)
  | Projector(_, inner) => pat_for_bound_name(name, inner)
  | _ => pat
  };

let item_bound_names = (item: Mod.t): list(Var.t) =>
  switch (item.term) {
  | ModLet(pat, _) => Pat.bound_vars(pat)
  | ModuleMod(mp, _) => mpat_names(mp)
  | ModType(_, _)
  | ModExp(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => []
  };

let collect_later_names = (items: list(Mod.t)): list(Var.t) =>
  items |> List.map(item_bound_names) |> List.flatten;

let value_exports = (items: list(Mod.t)): list(value_export) => {
  let rec go = (items: list(Mod.t)): list(value_export) =>
    switch (items) {
    | [] => []
    | [item, ...rest] =>
      let later_names = collect_later_names(rest);
      let keep = name => !List.mem(name, later_names);
      let exports =
        switch (item.term) {
        | ModLet(pat, _) =>
          Pat.bound_vars(pat)
          |> List.filter(keep)
          |> List.map(name =>
               {
                 name,
                 pat: pat_for_bound_name(name, pat),
               }
             )
        | ModuleMod(mp, _) =>
          let pat = mpat_to_pat(mp);
          mpat_names(mp)
          |> List.filter(keep)
          |> List.map(name =>
               {
                 name,
                 pat: pat_for_bound_name(name, pat),
               }
             );
        | ModType(_, _)
        | ModExp(_)
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) => []
        };
      exports @ go(rest);
    };
  go(items);
};

let labeled_tuple_exp = (exports: list(value_export)): Exp.t => {
  let fields =
    exports
    |> List.map(({name, _}) =>
         Exp.fresh(
           TupLabel(Exp.fresh(Label(name)), Exp.fresh(Var(name))),
         )
       );
  Exp.fresh(Tuple(fields));
};

let rec strip_typ_parens = (ty: Typ.t): Typ.t =>
  switch (ty.term) {
  | Parens(inner) => strip_typ_parens(inner)
  | _ => ty
  };

let extract_ana_labels = (ana: Typ.t): list((Var.t, Typ.t)) =>
  switch (strip_typ_parens(ana).term) {
  | Prod(fields) =>
    fields
    |> List.filter_map((field: Typ.t) =>
         switch (field.term) {
         | TupLabel({term: Label(name), _}, typ) => Some((name, typ))
         | _ => None
         }
       )
  | _ => []
  };

let type_exports_type = (exports: list((Var.t, Typ.t))): Typ.t => {
  let deduped =
    List.fold_right(
      ((name, ty), (seen, acc)) =>
        if (List.mem(name, seen)) {
          (seen, acc);
        } else {
          ([name, ...seen], [(name, ty), ...acc]);
        },
      exports,
      ([], []),
    )
    |> snd;
  Prod(
    deduped
    |> List.map(((name, ty)) =>
         TupLabel(Label(name) |> Typ.temp, ty) |> Typ.temp
       ),
  )
  |> Typ.temp;
};

let type_exports_alias_type =
    (exports: list((Var.t, Typ.t))): option(Typ.t) =>
  switch (exports) {
  | [] => None
  | _ => Some(type_exports_type(exports))
  };

let rec collect_type_exports =
        (ctx: Ctx.t, items: list(Mod.t)): list((Var.t, Typ.t)) =>
  items
  |> List.fold_left(
       ((ctx, acc), item: Mod.t) =>
         switch (item.term) {
         | ModType(tpat, typ) =>
           switch (tpat.term) {
           | Var(name) =>
             let (resolved, alias_ty) =
               if (List.mem(name, Typ.free_vars(typ))) {
                 let ty_rec = Rec(Var(name) |> TPat.fresh, typ) |> Typ.temp;
                 (ty_rec, ty_rec);
               } else {
                 let locals = List.map(fst, acc);
                 (
                   Typ.normalize(~expand=n => List.mem(n, locals), ctx, typ),
                   typ,
                 );
               };
             let ctx =
               Ctx.extend_alias(ctx, name, TPat.rep_id(tpat), alias_ty);
             (ctx, [(name, resolved), ...acc]);
           | _ => (ctx, acc)
           }
         | ModuleMod(mp, def) =>
           let rhs_exports_ty =
             switch (def.term) {
             | Module(inner_items) =>
               collect_type_exports(ctx, inner_items)
               |> type_exports_alias_type
             | Var(rhs)
             | Constructor(rhs, _) =>
               switch (Ctx.lookup_tvar(ctx, rhs)) {
               | Some(Singleton(exports_ty)) => Some(exports_ty)
               | _ => None
               }
             | _ => None
             };
           switch (mpat_names(mp), rhs_exports_ty) {
           | ([name], Some(exports_ty)) =>
             let ctx =
               Ctx.extend_alias(ctx, name, MPat.rep_id(mp), exports_ty);
             (ctx, [(name, exports_ty), ...acc]);
           | _ => (ctx, acc)
           };
         | _ => (ctx, acc)
         },
       (ctx, []),
     )
  |> snd
  |> List.rev;

let modlet_pat = (ana_labels: list((Var.t, Typ.t)), pat: Pat.t): Pat.t =>
  switch (pat.term) {
  | Var(name) =>
    switch (List.assoc_opt(name, ana_labels)) {
    | Some(expected_type) => Pat.fresh(Asc(pat, expected_type))
    | None => pat
    }
  | _ => pat
  };

let wrap_item =
    (~ana_labels: list((Var.t, Typ.t)), item: Mod.t, body: Exp.t): Exp.t =>
  switch (item.term) {
  | ModLet(pat, def) =>
    IdTagged.fast_copy(
      Mod.rep_id(item),
      Exp.fresh(Let(modlet_pat(ana_labels, pat), def, body)),
    )
  | ModType(tpat, typ) =>
    IdTagged.fast_copy(
      Mod.rep_id(item),
      Exp.fresh(TyAlias(tpat, typ, body)),
    )
  | ModExp(e) => Exp.fresh(Let(Pat.fresh(Wild), e, body))
  | ModuleMod(mp, def) =>
    IdTagged.fast_copy(
      Mod.rep_id(item),
      Exp.fresh(Let(mpat_to_pat(mp), def, body)),
    )
  | EmptyHole =>
    let e: Exp.t =
      IdTagged.fast_copy(Mod.rep_id(item), Exp.fresh(EmptyHole));
    Exp.fresh(Let(Pat.fresh(Wild), e, body));
  | Invalid(s) =>
    let e: Exp.t =
      IdTagged.fast_copy(Mod.rep_id(item), Exp.fresh(Invalid(s)));
    Exp.fresh(Let(Pat.fresh(Wild), e, body));
  | MultiHole(es) =>
    let e: Exp.t =
      IdTagged.fast_copy(Mod.rep_id(item), Exp.fresh(MultiHole(es)));
    Exp.fresh(Let(Pat.fresh(Wild), e, body));
  };

let lower =
    (~ctx: Ctx.t, ~ana: option(Typ.t)=?, items: list(Mod.t)): lowered => {
  let ana_labels =
    switch (ana) {
    | Some(ana) => extract_ana_labels(ana)
    | None => []
    };
  let value_exports = value_exports(items);
  {
    expanded:
      List.fold_right(
        wrap_item(~ana_labels),
        items,
        labeled_tuple_exp(value_exports),
      ),
    value_exports,
    type_exports: collect_type_exports(ctx, items),
  };
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

/* Construct module export product type from lowered value exports. */
let module_actual_type =
    (
      ~local_names: list(string),
      value_exports: list(value_export),
      m: StaticsBase.Map.t,
    )
    : Typ.t => {
  let fields =
    value_exports
    |> List.map(({name, pat}) => {
         let ty =
           switch (StaticsBase.Map.lookup_pat(Pat.rep_id(pat), m)) {
           | Some({ty, ctx: pat_ctx, _}) =>
             /* Scope escape: only module-LOCAL aliases must be inlined
                (they are unbound outside the braces); globals/builtins
                stay compact per the type-normalization invariant. */
             Typ.normalize(
               ~expand=n => List.mem(n, local_names),
               pat_ctx,
               ty,
             )
           | None => Typ.temp(Unknown(Internal))
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
