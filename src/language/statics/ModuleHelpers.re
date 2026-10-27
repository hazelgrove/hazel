/* Helpers for checking a module body (Statics' `Module` arm): what a
   signature expects of its items, what the body exports, and how the two
   compare. Signature types are closed (Typ.close). */

/* The module variables a written annotation refers to, as co-context
   entries: the roots of its paths, which closing it decides. */
let collect_module_refs_in_typ = (ctx: Ctx.t, id: Id.t, typ: Typ.t): CoCtx.t =>
  Typ.close(ctx, typ)
  |> Typ.module_vars
  |> List.map(name =>
       CoCtx.singleton(name, id, Unknown(Internal) |> Typ.temp)
     )
  |> CoCtx.union;

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

/* Whether a later item declares type member [name] again. */
let type_declared_later = (name: Var.t, later: list(Mod.t)): bool =>
  List.exists(
    (item: Mod.t) =>
      switch (item.term) {
      | ModType({term: Var(n), _}, _) => String.equal(n, name)
      | _ => false
      },
    later,
  );

/* Type names the body declares. The exported signature is closed: these
   are expanded in the exported member types. */
let declared_type_names = (items: list(Mod.t)): list(Var.t) =>
  items
  |> List.filter_map((item: Mod.t) =>
       switch (item.term) {
       | ModType({term: Var(n), _}, _) => Some(n)
       | _ => None
       }
     )
  |> Util.ListUtil.dedup_f(String.equal);

/* What the signature expects of each value member. An outer alias named
   like a type the body [declared] is expanded, so the body's does not
   capture it. */
let expected_value_types =
    (~ctx: Ctx.t, ~declared: list(Var.t), ana_items: option(list(Sig.t)))
    : list((Var.t, Typ.t)) =>
  switch (ana_items) {
  | None => []
  | Some(items) =>
    let expand = name => List.exists(String.equal(name), declared);
    Sig.members(items)
    |> List.filter_map((m: Sig.member) =>
         switch (m) {
         | Val(name, ty) => Some((name, Typ.normalize(~expand, ctx, ty)))
         | TypeManifest(_) => None
         }
       );
  };

/* The expectation on a binder pattern, assembled from the expectations on
   the names it binds: none when it binds none of them. A component the
   signature says nothing about is left to synthesis. */
let rec expected_pat_type =
        (expected: list((Var.t, Typ.t)), pat: Pat.t): option(Typ.t) =>
  switch (pat.term) {
  | Var(x) => Util.ListUtil.assoc_opt_by(String.equal, x, expected)
  | Asc(p, _)
  | Parens(p)
  | Projector(_, p) => expected_pat_type(expected, p)
  | TupLabel({term: Label(l), _}, p) =>
    expected_pat_type(expected, p)
    |> Option.map(ty => TupLabel(Label(l) |> Typ.temp, ty) |> Typ.temp)
  | Tuple(ps) =>
    let tys = List.map(expected_pat_type(expected), ps);
    List.exists(Option.is_some, tys)
      ? Some(
          Prod(
            List.map(
              Option.value(~default=Unknown(SynSwitch) |> Typ.temp),
              tys,
            ),
          )
          |> Typ.temp,
        )
      : None;
  | Cons(p1, p2) =>
    switch (
      expected_pat_type(expected, p1),
      expected_pat_type(expected, p2),
    ) {
    | (Some(e1), _) => Some(List(e1) |> Typ.temp)
    | (None, Some(e2)) => Some(e2)
    | (None, None) => None
    }
  | ListLit(ps) =>
    List.find_map(expected_pat_type(expected), ps)
    |> Option.map(e => List(e) |> Typ.temp)
  | TupLabel(_, _)
  | Ap(_, _)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | ExplicitNonlabel
  | Atom(_)
  | Constructor(_)
  | Label(_) => None
  };

/* The members a body exports, with the items that bind them: of several
   bindings of a name and kind, the last. */
let exports =
    (bound: list((Sig.t, Sig.member))): list((Sig.t, Sig.member)) =>
  List.rev(
    Util.ListUtil.dedup_f(
      ((_, a), (_, b)) => Sig.same_kind(a, b),
      List.rev(bound),
    ),
  );

/* The analyzed signature's items the module does not define. */
let missing_items =
    (~ana_items: list(Sig.t), have: list(Sig.t)): list(Sig.t) => {
  let have = Sig.members(have);
  ana_items
  |> List.filter(item =>
       switch (Sig.member_of_item(item)) {
       | Some(Val(x, _)) => Option.is_none(Sig.find_value(have, x))
       | Some(TypeManifest(t, _)) =>
         Option.is_none(Sig.find_type_def(have, t))
       | None => false
       }
     );
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
      | ModLet(pat, _) => Pat.has_hole_binder(Pat.binder(pat))
      | ModuleMod(mp, _) => mpat_has_hole(mp)
      | ModType({term: EmptyHole | MultiHole(_), _}, _) => true
      | ModType(_)
      | ModExp(_)
      | ModVal(_)
      | Invalid(_) => false
      },
    items,
  );

/* Members the module exports that the analyzed signature does not declare. */
let extra_members =
    (~ana_items: list(Sig.t), have: list(Sig.t)): list(Var.t) => {
  let have = Sig.members(have);
  let want = Sig.members(ana_items);
  let extra_values =
    Sig.value_names(have)
    |> List.filter(x => Option.is_none(Sig.find_value(want, x)));
  let extra_types =
    Sig.type_names(have)
    |> List.filter(t => Option.is_none(Sig.find_type_def(want, t)));
  extra_values @ extra_types;
};

/* The module's type under the signature it is checked against: each
   member [checked] against the signature on its item at the type the
   signature gives it where the two disagree, that disagreement having been
   reported there; any other member at its own type, so the module as a
   whole reports the disagreement. */
let sealed =
    (
      ~ctx: Ctx.t,
      ~ana_items: list(Sig.t),
      ~checked: list(Var.t),
      ~mismatched_types: list(Var.t),
      have: list((Sig.t, Sig.member)),
    )
    : list(Sig.t) => {
  let want = Sig.members(ana_items);
  have
  |> List.map(((item, m): (Sig.t, Sig.member)) =>
       switch (m) {
       | Val(x, ty) when List.exists(String.equal(x), checked) =>
         switch (Sig.find_value(want, x)) {
         | Some(expected) =>
           Sig.map_typ(_ => StaticsBase.fixed_typ(ctx, expected, ty), item)
         | None => item
         }
       | Val(_) => item
       | TypeManifest(name, _)
           when List.exists(String.equal(name), mismatched_types) =>
         switch (Sig.find_type_def(want, name)) {
         | Some(expected) => Sig.map_typ(_ => expected, item)
         | None => item
         }
       | TypeManifest(_) => item
       }
     );
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
