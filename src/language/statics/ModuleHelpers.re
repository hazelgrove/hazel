/* Module-related helper functions used during statics/elaboration.

   Type checking a module body reuses the Let/TyAlias machinery: the items
   are lowered to a chain of nested Let/TyAlias wrappers (`lower`) that is
   checked in synthesis mode, and the module's signature type is read back
   from the recorded pattern infos (`module_sig_type`). The elaborated module
   is refolded from the checked chain (`refold_module_elab`), so dynamics
   evaluates modules directly as modules. */

module VarSet = Set.Make(String);
module VarMap = Map.Make(String);

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

/* The module a qualified constructor's path starts from (`M` of `M.N.A`) */
let rec pat_path_head = (p: Pat.t): option((Var.t, Id.t)) =>
  switch (p.term) {
  | Var(name)
  | Constructor(name, _) => Some((name, Pat.rep_id(p)))
  | Parens(p)
  | Dot(p, _) => pat_path_head(p)
  | _ => None
  };

/* Walk a Pat.t AST and collect module refs from any Asc(_, ann) sub-patterns
   and from the paths of qualified constructors (`M.A`). */
let rec collect_pat_type_refs = (ctx: Ctx.t, pat: Pat.t): CoCtx.t =>
  switch (pat.term) {
  | Dot(path, _) =>
    switch (pat_path_head(path)) {
    | Some((name, id)) when Ctx.lookup_var(ctx, name) != None =>
      CoCtx.singleton(name, id, Unknown(Internal) |> Typ.temp)
    | _ => CoCtx.empty
    }
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

/* A `let f(x) = ...` item binds `f`; its binder is read through
   `FunctionSugar.binder`, as the shorthand's parameters are not members. */
let item_bound_names = (item: Mod.t): list(Var.t) =>
  switch (item.term) {
  | ModLet(pat, _) => Pat.bound_vars(FunctionSugar.binder(pat))
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
  | ModLet(pat, _) => of_pat(FunctionSugar.binder(pat))
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
   expects `x : Int`. */
let ana_value_types =
    (~defined: list(Var.t), ana_items: option(list(Sig.t)))
    : list((Var.t, Typ.t)) =>
  switch (ana_items) {
  | None => []
  | Some(items) =>
    let defined = VarSet.of_list(defined);
    /* each name's last declaration (Typ.sig_project_value), in the order
       the names first appear, in one pass */
    let (names, last) =
      List.fold_left(
        ((names, last), (m: Sig.member, ty)) =>
          switch (m) {
          | Val(x, _) => (
              VarMap.mem(x, last) ? names : [x, ...names],
              VarMap.add(x, ty, last),
            )
          | TypeManifest(_) => (names, last)
          },
        ([], VarMap.empty),
        Typ.sig_members_closed(
          ~keep_local=name => VarSet.mem(name, defined),
          items,
        ),
      );
    List.rev_map(x => (x, VarMap.find(x, last)), names);
  };

/* Annotate each bare variable binder in a pattern with the type its
   signature expects, so a mismatch is reported on the definition (or on the
   component of a destructured definition) rather than on the module:
   `(a, b)` becomes `(a : Int, b : Int)`. A binder the user annotated keeps
   its annotation. The function shorthand `let f(x) = ...` is annotated at
   its name: the `Let` case of statics then desugars it as it does any other
   shorthand. */
let rec modlet_pat =
        (~stable=false, ana_labels: list((Var.t, Typ.t)), pat: Pat.t): Pat.t => {
  let go = modlet_pat(~stable, ana_labels);
  let rewrap = (term: Pat.term): Pat.t => {
    ...pat,
    term,
  };
  switch (FunctionSugar.detect(pat)) {
  | Some(_) => FunctionSugar.map_binder(go, pat)
  | None =>
    switch (pat.term) {
    | Var(name) =>
      switch (List.assoc_opt(name, ana_labels)) {
      | Some(expected_type) =>
        let asc = Pat.fresh(Asc(pat, expected_type));
        stable
          ? IdTagged.fast_copy(
              Id.mk_str("modlet-asc:" ++ Id.to_string(Pat.rep_id(pat))),
              asc,
            )
          : asc;
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
    | Dot(_)
    | Label(_) => pat
    }
  };
};

/* [x], re-id'd from [stable] and [tag] when given: lowerings DefStatics
   compares across runs (by id) mint no fresh ids */
let stable_id =
    (stable: option(Id.t), tag: string, x: IdTagged.t('a)): IdTagged.t('a) =>
  switch (stable) {
  | Some(id) => IdTagged.fast_copy(Id.mk_str(tag ++ Id.to_string(id)), x)
  | None => x
  };

/* an item's expectations with copies of the signature's types, which get
   ids of their own (the signature's nodes are in the map already): fresh,
   or when [stable] derived from the item's [rep], the node's and its count
   so far (a type substituted for a signature's type member can repeat a
   node) */
let copy_labels =
    (~stable: bool, rep: Id.t, labels: list((Var.t, Typ.t)))
    : list((Var.t, Typ.t)) => {
  let copy = retag =>
    List.map(
      ((name, ty)) => (name, Grammar.map_typ_annotation(retag, ty)),
      labels,
    );
  switch (labels) {
  | [] => []
  | _ when !stable => copy(_ => IdTagged.IdTag.fresh())
  | _ =>
    let seen = Hashtbl.create(8);
    let derive = id => {
      let k = Option.value(~default=0, Hashtbl.find_opt(seen, id));
      Hashtbl.replace(seen, id, k + 1);
      Id.mk_str(
        "ana:"
        ++ Id.to_string(rep)
        ++ Id.to_string(id)
        ++ (k == 0 ? "" : ":" ++ string_of_int(k)),
      );
    };
    copy((tag: IdTagged.IdTag.t) =>
      IdTagged.IdTag.mk_internal(List.map(derive, tag.ids))
    );
  };
};

let wrap_item =
    (
      ~ana_labels: list((Var.t, Typ.t)),
      ~stable=false,
      item: Mod.t,
      body: Exp.t,
    )
    : Exp.t => {
  let rep = Mod.rep_id(item);
  let from = stable ? Some(rep) : None;
  let ana_labels = copy_labels(~stable, rep, ana_labels);
  /* a non-binding item as `let _ = e in body` */
  let wild = (tag, e) =>
    stable_id(
      from,
      tag ++ "let:",
      Exp.fresh(
        Let(stable_id(from, tag ++ "pat:", Pat.fresh(Wild)), e, body),
      ),
    );
  switch (item.term) {
  | ModLet(pat, def) =>
    IdTagged.fast_copy(
      rep,
      Exp.fresh(Let(modlet_pat(~stable, ana_labels, pat), def, body)),
    )
  | ModType(tpat, typ) =>
    IdTagged.fast_copy(rep, Exp.fresh(TyAlias(tpat, typ, body)))
  | ModExp(e) => wild("modexp-", e)
  | ModuleMod(mp, def) =>
    /* A sub-module gets its declared signature as expectation too, so a
       member it lacks or defines wrongly is reported inside it. */
    IdTagged.fast_copy(
      rep,
      Exp.fresh(
        Let(modlet_pat(~stable, ana_labels, mpat_to_pat(mp)), def, body),
      ),
    )
  | ModVal(x, def) =>
    IdTagged.fast_copy(
      rep,
      Exp.fresh(
        Let(stable_id(from, "modval-pat:", Pat.fresh(Var(x))), def, body),
      ),
    )
  | EmptyHole =>
    wild("modhole-", IdTagged.fast_copy(rep, Exp.fresh(EmptyHole)))
  | Invalid(s) =>
    wild("modinv-", IdTagged.fast_copy(rep, Exp.fresh(Invalid(s))))
  | MultiHole(es) =>
    wild("modmh-", IdTagged.fast_copy(rep, Exp.fresh(MultiHole(es))))
  };
};

let rec exported_names = (items: list(Mod.t)): list(Var.t) =>
  switch (items) {
  | [] => []
  | [item, ...rest] =>
    List.map(fst, item_exports(item, ~later=rest)) @ exported_names(rest)
  };

/* The lowering's tail: a tuple mentioning every exported binding, so that
   exports count as used. [stable] as in `stable_id`. */
let lower_tail_of_names =
    (~stable: option(Id.t)=?, names: list(Var.t)): Exp.t =>
  stable_id(
    stable,
    "modtail:",
    Exp.fresh(
      Tuple(
        List.mapi(
          (i, name) =>
            stable_id(
              stable,
              "modtail-" ++ string_of_int(i) ++ ":",
              Exp.fresh(Var(name)),
            ),
          names,
        ),
      ),
    ),
  );

let lower_tail = (~stable: option(Id.t)=?, items: list(Mod.t)): Exp.t =>
  lower_tail_of_names(~stable?, exported_names(items));

/* Each item's expectations from the analyzed signature, aligned with
   [items]: the signature's own types, which wrap_item copies. Only the
   binders an item exports get one: a definition a later item shadows is
   synthesized, not checked against the signature. */
let item_ana_labels =
    (~ana_items: option(list(Sig.t)), items: list(Mod.t))
    : list(list((Var.t, Typ.t))) =>
  switch (ana_items) {
  | None => List.map(_ => [], items)
  | Some(_) =>
    let defined =
      List.concat_map(
        (item: Mod.t) =>
          switch (item.term) {
          | ModType({term: Var(n), _}, _) => [n]
          | _ => List.map(fst, item_exports(item, ~later=[]))
          },
        items,
      );
    let expected =
      List.fold_left(
        (m, (name, ty)) => VarMap.add(name, ty, m),
        VarMap.empty,
        ana_value_types(~defined, ana_items),
      );
    /* right to left: an item exports the names no later item binds */
    snd(
      List.fold_right(
        (item, (later, acc)) => {
          let labels =
            item_exports(item, ~later=[])
            |> List.filter_map(((name, _)) =>
                 VarSet.mem(name, later)
                   ? None
                   : Option.map(
                       ty => (name, ty),
                       VarMap.find_opt(name, expected),
                     )
               );
          (
            VarSet.union(later, VarSet.of_list(item_bound_names(item))),
            [labels, ...acc],
          );
        },
        items,
        (VarSet.empty, []),
      ),
    );
  };

/* Lower module items to nested Let/TyAlias wrappers for type checking. The
   wrappers carry the Mod item ids, and ids derived from [stable] (the
   module literal's) where an item can't lend its own, so two analyses of
   one module agree, as the per-item analysis's lowering does. The tail
   mentions every exported binding so that exports count as used; its type
   is otherwise irrelevant: the module's type is computed by
   `module_sig_type` and its elaboration is refolded by
   `refold_module_elab`. */
let lower =
    (
      ~stable: option(Id.t)=?,
      ~ana_items: option(list(Sig.t)),
      items: list(Mod.t),
    )
    : Exp.t =>
  List.fold_right2(
    (item, ana_labels, body) =>
      wrap_item(~ana_labels, ~stable=Option.is_some(stable), item, body),
    items,
    item_ana_labels(~ana_items, items),
    lower_tail(~stable?, items),
  );

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
/* The analyzed signature's items the module does not define. */
let missing_items =
    (~ana_items: option(list(Sig.t)), sig_ty: Typ.t): list(Sig.t) =>
  switch (ana_items, sig_ty.term) {
  | (Some(ana_items), Sig(items)) =>
    let have = Sig.members(items);
    ana_items
    |> List.filter(item =>
         switch (Sig.member_of_item(item)) {
         | Some(Val(x, _)) => Option.is_none(Sig.find_value(have, x))
         | Some(TypeManifest(t, _)) =>
           Option.is_none(Sig.find_type_def(have, t))
         | None => false
         }
       );
  | _ => []
  };

let member_names = (items: list(Sig.t)): list(Var.t) => {
  let ms = Sig.members(items);
  Sig.value_names(ms) @ Sig.type_names(ms);
};

let rec mpat_has_hole = (mp: MPat.t): bool =>
  switch (mp.term) {
  | EmptyHole
  | MultiHole(_) => true
  | Asc(inner, _) => mpat_has_hole(inner)
  | Var(_)
  | Invalid(_) => false
  };

/* Whether an item has a hole where a member could be bound: a hole item, a
   hole in a binding position of a `let` pattern, or a hole module or type
   name. Holes in definitions and annotations do not count. */
let has_hole_binder = (items: list(Mod.t)): bool =>
  List.exists(
    (item: Mod.t) =>
      switch (item.term) {
      | EmptyHole
      | MultiHole(_) => true
      | ModLet(pat, _) => Pat.has_hole_binder(pat)
      | ModuleMod(mp, _) => mpat_has_hole(mp)
      | ModType({term: EmptyHole | MultiHole(_), _}, _) => true
      | ModType(_)
      | ModExp(_)
      | ModVal(_)
      | Invalid(_) => false
      },
    items,
  );

/* The module's signature with [items] appended: what the module provides
   once a hole among its items binds them. */
let assume_members = (sig_ty: Typ.t, items: list(Sig.t)): Typ.t =>
  switch (sig_ty.term) {
  | Sig(have) => {
      ...sig_ty,
      term: Sig(have @ items),
    }
  | _ => sig_ty
  };

/* Whether [items] define every member of the analyzed signature by name
   (none missing_items would find; extra members are sealed away). A
   module's member names are syntactic, so this needs no analysis. */
let names_meet = (~ana_items: list(Sig.t), items: list(Mod.t)): bool => {
  let (values, types) =
    List.fold_left(
      ((vs, ts), item: Mod.t) =>
        switch (item.term) {
        | ModType({term: Var(n), _}, _) => (vs, VarSet.add(n, ts))
        | _ => (
            List.fold_left(
              (vs, (x, _)) => VarSet.add(x, vs),
              vs,
              item_exports(item, ~later=[]),
            ),
            ts,
          )
        },
      (VarSet.empty, VarSet.empty),
      items,
    );
  let (want_values, want_types) =
    List.fold_left(
      ((vs, ts), m: Sig.member) =>
        switch (m) {
        | Val(x, _) => (VarSet.add(x, vs), ts)
        | TypeManifest(t, _) => (vs, VarSet.add(t, ts))
        },
      (VarSet.empty, VarSet.empty),
      Sig.members(ana_items),
    );
  VarSet.subset(want_values, values) && VarSet.subset(want_types, types);
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
              let mark =
                Mark.ModuleTypeMemberMismatch({
                  name,
                  expected,
                  actual: def,
                });
              /* The definition type carries the mark; the item only if the
                 type has no info of its own. */
              let m =
                switch (StaticsBase.Map.lookup_typ(IdTagged.rep_id(def), m)) {
                | Some(ti) =>
                  StaticsBase.Map.add_info(
                    IdTagged.ids(def),
                    Info.InfoTyp({
                      ...ti,
                      marks: [mark, ...ti.marks],
                      message: None,
                    }),
                    m,
                  )
                | None =>
                  StaticsBase.Map.add_info(
                    IdTagged.ids(item),
                    Info.InfoExp({
                      ...info,
                      marks: [mark, ...info.marks],
                    }),
                    m,
                  )
                };
              (m, [name, ...marked]);
            | _ => (m, marked)
            }
          | _ => (m, marked)
          };
        go(rest, m, marked);
      };
    go(items, m, []);
  };

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
    rewrap(Cons(strip_synthetic_asc(u1, e1), strip_synthetic_asc(u2, e2)))
  | (TupLabel(_, u), TupLabel(l, e)) =>
    rewrap(TupLabel(l, strip_synthetic_asc(u, e)))
  | (Parens(u), Parens(e)) => rewrap(Parens(strip_synthetic_asc(u, e)))
  | (Projector(_, u), Projector(d, e)) =>
    rewrap(Projector(d, strip_synthetic_asc(u, e)))
  | (Ap(_, u), Ap(ctr, e)) => rewrap(Ap(ctr, strip_synthetic_asc(u, e)))
  | (Asc(u, _), Asc(e, ty)) => rewrap(Asc(strip_synthetic_asc(u, e), ty))
  | _ => p_elab
  };
};

/* One item of `refold_module_elab`: [item], not a type member, with the
   elaborated binder [p] and definition [def] of its link. */
let refold_item = (item: Mod.t, p: Pat.t, def: Exp.t): Mod.t => {
  ...item,
  term:
    switch (item.term) {
    | ModLet(user_pat, _) =>
      ModLet(strip_synthetic_asc(FunctionSugar.binder(user_pat), p), def)
    | ModuleMod(mp, _) =>
      ModLet(strip_synthetic_asc(mpat_to_pat(mp), p), def)
    | ModExp(_) => ModExp(def)
    | ModVal(x, _) => ModVal(x, def)
    | ModType(_, _)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) => item.term
    },
};

/* Inverse of `wrap_item` over the checked chain: rebuild the module items
   with their elaborated definitions, in order. Type items have no runtime
   content (TyAlias elaborates to its body) and are dropped. The synthetic
   ascription `modlet_pat` adds to a bare variable binder is stripped; a
   `module M : S = ...` item keeps its elaborated (ascribed) binder. A
   shorthand item `let f(x) = ...` elaborates to a binding of `f`. */
let rec refold_module_elab = (items: list(Mod.t), elab: Exp.t): list(Mod.t) =>
  switch (items) {
  | [] => []
  | [{term: ModType(_, _), _}, ...rest] => refold_module_elab(rest, elab)
  | [item, ...rest] =>
    switch (elab.term) {
    | Let(p, def, body) => [
        refold_item(item, p, def),
        ...refold_module_elab(rest, body),
      ]
    | _ => items
    }
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
