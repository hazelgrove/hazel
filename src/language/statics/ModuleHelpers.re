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
  |> List.filter_map(~f=name =>
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
  | ListLit(ps) => CoCtx.union(List.map(~f=collect_pat_type_refs(ctx), ps))
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
  | Var(n) when String.equal(n, name) => pat
  | Asc(inner, _) => pat_for_bound_name(name, inner)
  | Tuple(ps)
  | ListLit(ps) =>
    ps
    |> List.find(~f=p =>
         List.mem(Pat.bound_vars(p), name, ~equal=String.equal)
       )
    |> Option.map(~f=pat_for_bound_name(name))
    |> Option.value(~default=pat)
  | Cons(p1, p2)
  | TupLabel(p1, p2)
  | Ap(p1, p2) =>
    if (List.mem(Pat.bound_vars(p1), name, ~equal=String.equal)) {
      pat_for_bound_name(name, p1);
    } else if (List.mem(Pat.bound_vars(p2), name, ~equal=String.equal)) {
      pat_for_bound_name(name, p2);
    } else {
      pat;
    }
  | Parens(inner)
  | Projector(_, inner) => pat_for_bound_name(name, inner)
  | _ => pat
  };

/* Funlet members (`let f(x) = e`) bind only the head var; Pat.bound_vars
   descends an Ap pattern's ARGUMENT (right for constructor patterns,
   wrong here — it would export the parameters instead of the function). */
let modlet_bound_vars = (pat: Pat.t): list(Var.t) =>
  switch (FunctionSugar.detect(pat)) {
  | Some((f_name, _, _)) => Pat.bound_vars(f_name)
  | None => Pat.bound_vars(pat)
  };

let item_bound_names = (item: Mod.t): list(Var.t) =>
  switch (item.term) {
  | ModLet(pat, _) => modlet_bound_vars(pat)
  | ModuleMod(mp, _) => mpat_names(mp)
  | ModType(_, _)
  | ModExp(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => []
  };

let collect_later_names = (items: list(Mod.t)): list(Var.t) =>
  items |> List.map(~f=item_bound_names) |> List.concat;

let value_exports = (items: list(Mod.t)): list(value_export) => {
  let rec go = (items: list(Mod.t)): list(value_export) =>
    switch (items) {
    | [] => []
    | [item, ...rest] =>
      let later_names = collect_later_names(rest);
      let keep = name => !List.mem(later_names, name, ~equal=String.equal);
      let exports =
        switch (item.term) {
        | ModLet(pat, _) =>
          /* Funlet: the whole binder carries the function's type (see
             FunctionSugar.add_binder_infos). Searching it by name could
             land on a parameter that shadows the function, e.g.
             `let g(x)(g) = …`. */
          let is_funlet = Option.is_some(FunctionSugar.detect(pat));
          modlet_bound_vars(pat)
          |> List.filter(~f=keep)
          |> List.map(~f=name =>
               {
                 name,
                 pat: is_funlet ? pat : pat_for_bound_name(name, pat),
               }
             );
        | ModuleMod(mp, _) =>
          let pat = mpat_to_pat(mp);
          mpat_names(mp)
          |> List.filter(~f=keep)
          |> List.map(~f=name =>
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

/* The export record has no source position, so its ids derive from the
   module's, distinct from the module's own (module_body_with_id gives the
   elaboration's record that one afterwards). A later item's binding of a
   name shadows the earlier export, but one item can bind a name twice
   (`let (x, x) = …`), so each field's ids carry its position as well as
   its name. */
let labeled_tuple_exp =
    (~module_id: Id.t, exports: list(value_export)): Exp.t => {
  let mk = (salt, term: Exp.term): Exp.t =>
    IdTagged.derived(~salt="module-export:" ++ salt, module_id, term);
  let fields =
    exports
    |> List.mapi(~f=(i, {name, _}) => {
         let at = string_of_int(i) ++ ":" ++ name;
         Exp.labeled_field(
           ~salt="module-export:field:" ++ at,
           ~source=module_id,
           name,
           mk("var:" ++ at, Var(name)),
         );
       });
  mk("record", Tuple(fields));
};

let extract_ana_labels = (ana: Typ.t): list((Var.t, Typ.t)) =>
  switch (Typ.strip_wrappers(ana).term) {
  | Prod(fields) => fields |> List.filter_map(~f=Typ.match_tup_label)
  | _ => []
  };

let type_exports_type = (exports: list((Var.t, Typ.t))): Typ.t => {
  let deduped =
    List.fold_right(
      ~f=
        ((name, ty), (seen, acc)) =>
          if (List.mem(seen, name, ~equal=String.equal)) {
            (seen, acc);
          } else {
            ([name, ...seen], [(name, ty), ...acc]);
          },
      ~init=([], []),
      exports,
    )
    |> snd;
  Prod(
    deduped
    |> List.map(~f=((name, ty)) =>
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
  |> List.fold(
       ~f=
         ((ctx, acc), item: Mod.t) =>
           switch (item.term) {
           | ModType(tpat, typ) =>
             switch (tpat.term) {
             | Var(name) =>
               let (resolved, alias_ty) =
                 switch (Typ.recursive_def(name, typ)) {
                 | Typ.Recursive(ty_rec) => (ty_rec, ty_rec)
                 | Typ.Plain =>
                   let locals = List.map(~f=fst, acc);
                   (
                     Typ.normalize(
                       ~expand=n => List.mem(locals, n, ~equal=String.equal),
                       ctx,
                       typ,
                     ),
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
               switch (Exp.strip_wrappers(def).term) {
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
       ~init=(ctx, []),
     )
  |> snd
  |> List.rev;

/* Funlet patterns are deliberately left unascribed: Asc(f(args), member_ty)
   would be read by FunctionSugar.detect as a RETURN-type annotation. */
let modlet_pat = (ana_labels: list((Var.t, Typ.t)), pat: Pat.t): Pat.t =>
  switch (Pat.strip_wrappers(pat).term) {
  | Var(name) =>
    switch (List.Assoc.find(ana_labels, name, ~equal=String.equal)) {
    | Some(expected_type) =>
      IdTagged.derived(
        ~salt="module-item-signature",
        Pat.rep_id(pat),
        Asc(pat, expected_type): Pat.term,
      )
    | None => pat
    }
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | ExplicitNonlabel
  | Atom(_)
  | ListLit(_)
  | Constructor(_)
  | Cons(_)
  | Tuple(_)
  | Label(_)
  | TupLabel(_)
  | Parens(_)
  | Projector(_)
  | Ap(_)
  | Asc(_) => pat
  };

/* The id of the `let` an item lowers to. A binding item's wrapper takes the
   item's id. An expression or hole item is its expression, which already has
   the item's ids, so its wrapper takes an id derived from them instead: on
   the item's id, the wrapper's info would replace the expression's type and
   errors. */
let wrapper_id = (item: Mod.t): Id.t =>
  switch (item.term) {
  | ModLet(_)
  | ModType(_)
  | ModuleMod(_) => Mod.rep_id(item)
  | ModExp(_)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => Id.derive(~salt="module-item-wrapper", Mod.rep_id(item))
  };

let wrapper_ids = (item: Mod.t): list(Id.t) =>
  switch (item.term) {
  | ModLet(_)
  | ModType(_)
  | ModuleMod(_) => IdTagged.ids(item)
  | ModExp(_)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => [wrapper_id(item)]
  };

let wrap_item =
    (~ana_labels: list((Var.t, Typ.t)), item: Mod.t, body: Exp.t): Exp.t => {
  let wrap = (term: Exp.term) =>
    IdTagged.fast_copy(wrapper_id(item), Exp.fresh(term));
  let item_exp = (term: Exp.term) =>
    IdTagged.fast_copy(Mod.rep_id(item), Exp.fresh(term));
  let wild =
    IdTagged.derived(
      ~salt="module-item-wild",
      Mod.rep_id(item),
      Wild: Pat.term,
    );
  switch (item.term) {
  | ModLet(pat, def) => wrap(Let(modlet_pat(ana_labels, pat), def, body))
  | ModType(tpat, typ) => wrap(TyAlias(tpat, typ, body))
  | ModExp(e) => wrap(Let(wild, e, body))
  | ModuleMod(mp, def) => wrap(Let(mpat_to_pat(mp), def, body))
  | EmptyHole => wrap(Let(wild, item_exp(EmptyHole), body))
  | Invalid(s) => wrap(Let(wild, item_exp(Invalid(s)), body))
  | MultiHole(es) => wrap(Let(wild, item_exp(MultiHole(es)), body))
  };
};

/* Whether `pat` is an ascription modlet_pat added to `inner`. It checks the
   item against the signature, but the module's elaboration leaves it out:
   the user didn't write it. */
let is_signature_ascription = (~inner: Pat.t, pat: Pat.t): bool =>
  Id.equal(
    Pat.rep_id(pat),
    Id.derive(~salt="module-item-signature", Pat.rep_id(inner)),
  );

/* TODO: lowering goes away when modules-ii (#2493) gives modules real
   values. */
let lower =
    (
      ~ctx: Ctx.t,
      ~ana: option(Typ.t)=?,
      ~module_id: Id.t,
      items: list(Mod.t),
    )
    : lowered => {
  let ana_labels =
    switch (ana) {
    | Some(ana) => extract_ana_labels(ana)
    | None => []
    };
  let value_exports = value_exports(items);
  {
    expanded:
      List.fold_right(
        ~f=wrap_item(~ana_labels),
        ~init=labeled_tuple_exp(~module_id, value_exports),
        items,
      ),
    value_exports,
    type_exports: collect_type_exports(ctx, items),
  };
};

/* The module's elaboration, with the module's id on the export record it
   evaluates to, so a probe or a cache keyed by the module's id finds it. */
let rec module_body_with_id = (~id, exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  | Let(p, def, body) =>
    Let(p, def, module_body_with_id(~id, body)) |> rewrap
  | TyAlias(tpat, typ, body) =>
    TyAlias(tpat, typ, module_body_with_id(~id, body)) |> rewrap
  | Parens(inner) => Parens(module_body_with_id(~id, inner)) |> rewrap
  | Tuple(_) => IdTagged.fast_copy(id, exp)
  | _ => exp
  };
};

/* Rewrite InfoExp cls for expanded module items to keep cursor inspector labels. */
let reclassify_expanded_module_items =
    (items: list(Mod.t), m: StaticsBase.Map.t) =>
  List.fold(
    ~f=
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
    ~init=m,
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
    |> List.map(~f=({name, pat}) => {
         let ty =
           switch (StaticsBase.Map.lookup_pat(Pat.rep_id(pat), m)) {
           | Some({ty, ctx: pat_ctx, _}) =>
             /* The member's type is the one its name is bound at, which
                is not always the pattern's: `let y = (y=1)` puns, binding
                `y : Int`, and the lowered `let` does the same. */
             let ty =
               switch (Ctx.lookup_var(pat_ctx, name)) {
               | Some({typ, _}) => typ
               | None => ty
               };
             /* Scope escape: only module-LOCAL aliases must be inlined
                (they are unbound outside the braces); globals/builtins
                stay compact per the type-normalization invariant. */
             Typ.normalize(
               ~expand=n => List.mem(local_names, n, ~equal=String.equal),
               pat_ctx,
               ty,
             );
           | None => Typ.temp(Unknown(Internal))
           };
         TupLabel(Label(name) |> Typ.temp, ty) |> Typ.temp;
       });
  Prod(fields) |> Typ.temp;
};

/* Rebuild ModuleExp elaboration with direct def elaboration preserved. */
let moduleexp_elab = (~def_elab_direct: Exp.t, expanded_elab: Exp.t): Exp.t => {
  let (expanded_term, expanded_rewrap) = Exp.unwrap(expanded_elab);
  switch (expanded_term) {
  | Let(p_elab, _, body_elab) =>
    Let(p_elab, def_elab_direct, body_elab) |> expanded_rewrap
  | _ => expanded_elab
  };
};
