/* Module-related helper functions used during statics/elaboration.

   Type checking a module body reuses the Let/TyAlias machinery: the items
   are lowered to a chain of nested Let/TyAlias wrappers (`lower`) that is
   checked in synthesis mode, and the module's signature type is read back
   from the recorded pattern infos (`module_sig_type`). The elaborated module
   is refolded from the checked chain (`refold_module_elab`), so dynamics
   evaluates modules directly as modules. */

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
      Pat.fresh(Asc(mpat_to_pat(inner), typ)),
    )
  | _ => IdTagged.fast_copy(MPat.rep_id(mp), Pat.fresh(Wild))
  };

let rec mpat_names = (mp: MPat.t): list(Var.t) =>
  switch (mp.term) {
  | Var(name) => [name]
  | Asc(inner, _) => mpat_names(inner)
  | _ => []
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
  | ModVal(x, _) => [x]
  | ModType(_, _)
  | ModExp(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => []
  };

let collect_later_names = (items: list(Mod.t)): list(Var.t) =>
  items |> List.map(item_bound_names) |> List.flatten;

/* Names bound by [item] that no later item rebinds (last binding wins),
   each paired with the sub-pattern that binds it. */
let item_exports = (item: Mod.t, ~later: list(Mod.t)): list((Var.t, Pat.t)) => {
  let later_names = collect_later_names(later);
  let of_pat = pat =>
    Pat.bound_vars(pat)
    |> List.filter(name => !List.mem(name, later_names))
    |> List.map(name => (name, pat_for_bound_name(name, pat)));
  switch (item.term) {
  | ModLet(pat, _) => of_pat(pat)
  | ModuleMod(mp, _) => of_pat(mpat_to_pat(mp))
  | ModVal(x, _) => of_pat(Pat.fresh(Var(x)))
  | ModType(_, _)
  | ModExp(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => []
  };
};

/* Whether a later item declares type member [name] again. */
let type_declared_later = (name: Var.t, later: list(Mod.t)): bool =>
  List.exists(
    (item: Mod.t) =>
      switch (item.term) {
      | ModType({term: Var(n), _}, _) => n == name
      | _ => false
      },
    later,
  );

/* Expected types for the module's value members when it is analyzed
   against a signature: each member's declared type with the signature's own
   manifest type members substituted away, so `{ type T = Int; let x : T }`
   expects `x : Int`. An abstract member, or a sibling member a path goes
   through, keeps its bare name: inside the lowered body it resolves to the
   module's own definition (the ML rule that a sealed member is checked
   against the module's realization of T). A member whose type mentions a
   signature name the module does not define ([defined]) gets no expectation:
   the missing member is reported on the module, and a free name here would
   be reported on the signature. Expectations get fresh ids so that checking
   them never records info under the signature's own type nodes. */
let ana_value_types =
    (~defined: list(Var.t), ana_items: option(list(Sig.t)))
    : list((Var.t, Typ.t)) =>
  switch (ana_items) {
  | None => []
  | Some(items) =>
    let members = Sig.members(items);
    /* Names a member's type can only mean through the module's own
       definitions: abstract type members and value members (paths). A
       manifest type member has a definition to substitute instead. */
    let opaque_locals =
      List.filter_map(
        (m: Sig.member) =>
          switch (m) {
          | TypeAbstract(x)
          | Val(x, _) => Some(x)
          | TypeManifest(_) => None
          },
        members,
      );
    let mentions_undefined = (ty: Typ.t) =>
      Typ.free_vars(ty)
      |> List.exists(v =>
           List.mem(v, opaque_locals) && !List.mem(v, defined)
         );
    Sig.value_names(members)
    |> List.filter_map(name =>
         switch (Typ.sig_project_value(~keep_local=_ => true, items, name)) {
         | Some(bare) when mentions_undefined(bare) => None
         | Some(_) =>
           Typ.sig_project_value(
             ~keep_local=n => List.mem(n, defined),
             items,
             name,
           )
           |> Option.map(ty =>
                (
                  name,
                  Grammar.map_typ_annotation(_ => IdTagged.IdTag.fresh(), ty),
                )
              )
         | None => None
         }
       );
  };

/* The module path an expression denotes, if any: a module variable
   (possibly written capitalized), or a member projection out of one. */
let rec path_of_exp = (ctx: Ctx.t, e: Exp.t): option(Typ.t) =>
  switch (e.term) {
  | Var(x)
  | Constructor(x, _) when Ctx.lookup_var(ctx, x) != None =>
    Some(Var(x) |> Typ.temp)
  | Dot(e, {term: Label(l), _}) =>
    path_of_exp(ctx, e)
    |> Option.map(p => ProdProjection(p, Label(l) |> Typ.temp) |> Typ.temp)
  | Parens(e) => path_of_exp(ctx, e)
  | _ => None
  };

/* Annotate each bare variable binder in a pattern with the type its
   signature expects, so a mismatch is reported on the definition (or on the
   component of a destructured definition) rather than on the module:
   `(a, b)` becomes `(a : Int, b : Int)`. A binder the user annotated keeps
   its annotation. */
let rec modlet_pat = (ana_labels: list((Var.t, Typ.t)), pat: Pat.t): Pat.t => {
  let go = modlet_pat(ana_labels);
  let rewrap = (term: Pat.term): Pat.t => {
    ...pat,
    term,
  };
  switch (pat.term) {
  | Var(name) =>
    switch (List.assoc_opt(name, ana_labels)) {
    | Some(expected_type) => Pat.fresh(Asc(pat, expected_type))
    | None => pat
    }
  | Tuple(ps) => rewrap(Tuple(List.map(go, ps)))
  | ListLit(ps) => rewrap(ListLit(List.map(go, ps)))
  | Cons(p1, p2) => rewrap(Cons(go(p1), go(p2)))
  | TupLabel(l, p) => rewrap(TupLabel(l, go(p)))
  | Parens(p) => rewrap(Parens(go(p)))
  | Projector(d, p) => rewrap(Projector(d, go(p)))
  | Ap(ctr, p) => rewrap(Ap(ctr, go(p)))
  | Asc(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | ExplicitNonlabel
  | Atom(_)
  | Constructor(_)
  | Label(_) => pat
  };
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
    /* A sub-module gets its declared signature as expectation too, so a
       member it lacks or defines wrongly is reported inside it. */
    IdTagged.fast_copy(
      Mod.rep_id(item),
      Exp.fresh(Let(modlet_pat(ana_labels, mpat_to_pat(mp)), def, body)),
    )
  | ModVal(x, def) =>
    IdTagged.fast_copy(
      Mod.rep_id(item),
      Exp.fresh(Let(Pat.fresh(Var(x)), def, body)),
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

/* Lower module items to nested Let/TyAlias wrappers for type checking. The
   wrappers carry the Mod item ids. The tail mentions every exported binding
   so that exports count as used; its type is otherwise irrelevant: the
   module's type is computed by `module_sig_type` and its elaboration is
   refolded by `refold_module_elab`. */
let lower = (~ana_items: option(list(Sig.t)), items: list(Mod.t)): Exp.t => {
  let defined =
    List.concat_map(
      (item: Mod.t) =>
        switch (item.term) {
        | ModType({term: Var(n), _}, _) => [n]
        | _ => List.map(fst, item_exports(item, ~later=[]))
        },
      items,
    );
  let ana_labels = ana_value_types(~defined, ana_items);
  let rec exported = (items: list(Mod.t)) =>
    switch (items) {
    | [] => []
    | [item, ...rest] =>
      List.map(fst, item_exports(item, ~later=rest)) @ exported(rest)
    };
  let tail =
    Exp.fresh(
      Tuple(List.map(name => Exp.fresh(Var(name)), exported(items))),
    );
  List.fold_right(wrap_item(~ana_labels), items, tail);
};

/* Rewrite InfoExp cls for expanded module items to keep cursor inspector labels. */
let reclassify_expanded_module_items =
    (items: list(Mod.t), m: StaticsBase.Map.t) =>
  List.fold_left(
    (m, item: Mod.t) => {
      let ids = IdTagged.ids(item);
      let mod_cls = Cls.Mod(Mod.cls_of_term(item.term));
      /* The wrapper's own type is the type of the rest of the chain, which
         is meaningless for the item; show the member it declares instead. */
      let pat_ty = id =>
        StaticsBase.Map.lookup_pat(id, m)
        |> Option.map((info: Info.pat) => info.ty);
      let member_ty =
        switch (item.term) {
        | ModLet(p, _) => pat_ty(Pat.rep_id(p))
        | ModuleMod(mp, _) => pat_ty(MPat.rep_id(mp))
        | ModType(_, ty) => Some(ty)
        | ModVal(_, _)
        | ModExp(_)
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) => None
        };
      switch (StaticsBase.Map.lookup_exp(IdTagged.rep_id(item), m)) {
      | Some(info) =>
        let info =
          switch (member_ty) {
          | Some(ty) => {
              ...info,
              cls: mod_cls,
              elab_syn_ty: ty,
              ty,
              message: Message.Exp(Common(Syn(ty))),
            }
          | None => {
              ...info,
              cls: mod_cls,
            }
          };
        StaticsBase.Map.add_info(ids, Info.InfoExp(info), m);
      | None => m
      };
    },
    m,
    items,
  );

/* Type member names declared more than once in a body. Only these are
   inlined into the types of members defined between the declarations; the
   surviving declarations are exported and bind their name in the signature. */
let shadowed_type_names = (items: list(Mod.t)): list(Var.t) => {
  let names =
    items
    |> List.filter_map((item: Mod.t) =>
         switch (item.term) {
         | ModType({term: Var(n), _}, _) => Some(n)
         | _ => None
         }
       );
  names
  |> List.filter(n => List.length(List.filter((==)(n), names)) > 1)
  |> Sig.dedup_names;
};

/* The signature synthesized for a module body: its exported type and value
   members in source order. Value member types come from the pattern infos
   recorded while checking the lowered body. */
let module_sig_type =
    (~ctx: Ctx.t, items: list(Mod.t), m: StaticsBase.Map.t): Typ.t => {
  let shadowed = shadowed_type_names(items);
  let expand = n => List.mem(n, shadowed);
  let rec go = (ctx: Ctx.t, items: list(Mod.t), acc: list(Sig.t)) =>
    switch (items) {
    | [] => List.rev(acc)
    | [item, ...rest] =>
      switch (item.term) {
      | ModType({term: Var(name), _} as tpat, def) =>
        let def_ty =
          List.mem(name, Typ.free_vars(def))
            ? Rec(Var(name) |> TPat.fresh, def) |> Typ.temp : def;
        let ctx' = Ctx.extend_alias(ctx, name, TPat.rep_id(tpat), def_ty);
        let acc =
          type_declared_later(name, rest)
            ? acc
            : [
              Sig.item_of_member(
                TypeManifest(name, Typ.normalize(~expand, ctx, def_ty)),
              ),
              ...acc,
            ];
        go(ctx', rest, acc);
      | ModLet(_, _)
      | ModuleMod(_, _)
      | ModVal(_, _) =>
        let members =
          item_exports(item, ~later=rest)
          |> List.map(((name, pat)) => {
               let ty =
                 switch (StaticsBase.Map.lookup_pat(Pat.rep_id(pat), m)) {
                 | Some({ty, ctx: pat_ctx, _}) =>
                   Typ.normalize(~expand, pat_ctx, ty)
                 | None => Typ.temp(Unknown(Internal))
                 };
               /* A `module M = ...` item exports a `module M : S` member. */
               switch (item.term) {
               | ModuleMod(_, _) => Sig.module_item(name, ty)
               | _ => Sig.item_of_member(Val(name, ty))
               };
             });
        go(ctx, rest, List.rev_append(members, acc));
      | ModType(_, _)
      | ModExp(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => go(ctx, rest, acc)
      }
    };
  Sig(go(ctx, items, [])) |> Typ.temp;
};

/* Members the analyzed signature requires that the module does not export. */
let missing_members =
    (~ana_items: option(list(Sig.t)), sig_ty: Typ.t): list(Var.t) =>
  switch (ana_items, sig_ty.term) {
  | (Some(ana_items), Sig(items)) =>
    Sig.missing_members(
      ~want=Sig.members(ana_items),
      ~have=Sig.members(items),
    )
  | _ => []
  };

/* Mark each exported `type T = ...` whose definition differs from the
   manifest type the analyzed signature declares for T. Definitions with
   holes are not compared, to stay gradual. The mark lands on the item's
   info: the TyAlias wrapper carries the Mod item id. Also returns the names
   marked, so the module itself is not reported a second time. */
let check_ana_type_members =
    (
      ~ana_items: option(list(Sig.t)),
      items: list(Mod.t),
      m: StaticsBase.Map.t,
    )
    : (StaticsBase.Map.t, list(Var.t)) =>
  switch (ana_items) {
  | None => (m, [])
  | Some(ana_items) =>
    let rec go = (items: list(Mod.t), m, marked) =>
      switch (items) {
      | [] => (m, List.rev(marked))
      | [item, ...rest] =>
        let (m, marked) =
          switch (item.term) {
          | ModType({term: Var(name), _}, def)
              when !type_declared_later(name, rest) =>
            switch (
              Typ.sig_project_type(ana_items, name),
              StaticsBase.Map.lookup_exp(IdTagged.rep_id(item), m),
            ) {
            | (Some(expected), Some(info))
                when
                  Typ.count_unknowns(expected) == 0
                  && Typ.count_unknowns(def) == 0
                  && !Typ.equal_up_to_aliases(info.ctx, def, expected) =>
              let m =
                StaticsBase.Map.add_info(
                  IdTagged.ids(item),
                  Info.InfoExp({
                    ...info,
                    marks: [
                      Mark.ModuleTypeMemberMismatch({
                        name,
                        expected,
                        actual: def,
                      }),
                      ...info.marks,
                    ],
                  }),
                  m,
                );
              (m, [name, ...marked]);
            | _ => (m, marked)
            }
          | _ => (m, marked)
          };
        go(rest, m, marked);
      };
    go(items, m, []);
  };

/* Inverse of `wrap_item` over the checked chain: rebuild the module items
   with their elaborated definitions, in order. Type items have no runtime
   content (TyAlias elaborates to its body) and are dropped. The synthetic
   ascription `modlet_pat` adds to a bare variable binder is stripped; a
   `module M : S = ...` item keeps its elaborated (ascribed) binder. */
let rec refold_module_elab = (items: list(Mod.t), elab: Exp.t): list(Mod.t) => {
  /* Walk the user's pattern and its elaboration together: an ascription the
     elaboration has where the user wrote a bare variable is synthetic. */
  let rec strip_synthetic_asc = (user_pat: Pat.t, p_elab: Pat.t): Pat.t => {
    let rewrap = (term: Pat.term): Pat.t => {
      ...p_elab,
      term,
    };
    let map2 = (us, es) =>
      List.length(us) == List.length(es)
        ? List.map2(strip_synthetic_asc, us, es) : es;
    switch (user_pat.term, p_elab.term) {
    | (Var(_), Asc(inner, _)) => inner
    | (Tuple(us), Tuple(es)) => rewrap(Tuple(map2(us, es)))
    | (ListLit(us), ListLit(es)) => rewrap(ListLit(map2(us, es)))
    | (Cons(u1, u2), Cons(e1, e2)) =>
      rewrap(
        Cons(strip_synthetic_asc(u1, e1), strip_synthetic_asc(u2, e2)),
      )
    | (TupLabel(_, u), TupLabel(l, e)) =>
      rewrap(TupLabel(l, strip_synthetic_asc(u, e)))
    | (Parens(u), Parens(e)) => rewrap(Parens(strip_synthetic_asc(u, e)))
    | (Projector(_, u), Projector(d, e)) =>
      rewrap(Projector(d, strip_synthetic_asc(u, e)))
    | (Ap(_, u), Ap(ctr, e)) =>
      rewrap(Ap(ctr, strip_synthetic_asc(u, e)))
    | (Asc(u, _), Asc(e, ty)) =>
      rewrap(Asc(strip_synthetic_asc(u, e), ty))
    | _ => p_elab
    };
  };
  switch (items) {
  | [] => []
  | [{term: ModType(_, _), _}, ...rest] => refold_module_elab(rest, elab)
  | [item, ...rest] =>
    switch (elab.term) {
    | Let(p, def, body) =>
      let term: Mod.term =
        switch (item.term) {
        | ModLet(user_pat, _) =>
          ModLet(strip_synthetic_asc(user_pat, p), def)
        | ModuleMod(mp, _) =>
          ModLet(strip_synthetic_asc(mpat_to_pat(mp), p), def)
        | ModExp(_) => ModExp(def)
        | ModVal(x, _) => ModVal(x, def)
        | ModType(_, _)
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) => item.term
        };
      [
        {
          ...item,
          term,
        },
        ...refold_module_elab(rest, body),
      ];
    | _ => items
    }
  };
};

/* Rebuild ModuleExp elaboration with direct def elaboration preserved. The
   binder keeps its (normalized) signature ascription: that is what seals the
   module at runtime. */
let moduleexp_elab = (~def_elab_direct: Exp.t, expanded_elab: Exp.t): Exp.t => {
  let (expanded_term, expanded_rewrap) = Exp.unwrap(expanded_elab);
  switch (expanded_term) {
  | Let(p_elab, _, body_elab) =>
    Let(p_elab, def_elab_direct, body_elab) |> expanded_rewrap
  | _ => expanded_elab
  };
};
