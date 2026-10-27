/* Helpers for checking a module body (Statics' `Module` arm): what a
   signature expects of its items, what the body exports, and how the two
   compare. Signature types are closed (Typ.close). */

/* The module variables a closed type [typ] at [id] refers to as path roots
   (`M` in `M.T`): its free variables that are bound as values. */
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

/* The module variables the annotations in [pat] refer to. */
let rec collect_pat_type_refs = (ctx: Ctx.t, pat: Pat.t): CoCtx.t =>
  switch (pat.term) {
  | Asc(p, ann) =>
    CoCtx.union([
      collect_module_refs_in_typ(ctx, Typ.rep_id(ann), Typ.close(ctx, ann)),
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

/* What the signature expects of each member, read in the body's context:
   an outer alias named like a type the body [declared] is expanded, so the
   body's does not capture it. */
let expected_members =
    (~ctx: Ctx.t, ~declared: list(Var.t), ana_items: list(Sig.t))
    : list(Sig.member) => {
  let expand = name => List.exists(String.equal(name), declared);
  Sig.members(ana_items)
  |> List.map((m: Sig.member) =>
       switch (m) {
       | Val(name, ty) => Sig.Val(name, Typ.normalize(~expand, ctx, ty))
       | TypeManifest(name, ty) =>
         Sig.TypeManifest(name, Typ.normalize(~expand, ctx, ty))
       }
     );
};

/* An expectation on a binder pattern and the names it holds to it. */
type pat_expectation = {
  ty: Typ.t,
  names: list(Var.t),
};

/* Expectations on one component, met in order. One that does not meet the
   expectation so far is dropped with its names, so the signature it comes
   from is reported on the module as a whole. */
let meet_expectations =
    (~ctx: Ctx.t, es: list(option(pat_expectation)))
    : option(pat_expectation) =>
  List.fold_left(
    (acc, e) =>
      switch (acc, e) {
      | (None, e) => e
      | (acc, None) => acc
      | (Some(acc), Some(e)) =>
        switch (Typ.meet(ctx, acc.ty, e.ty)) {
        | Some(ty) =>
          Some({
            ty,
            names: acc.names @ e.names,
          })
        | None => Some(acc)
        }
      },
    None,
    es,
  );

/* The expectation on a binder pattern, assembled from the signature's
   expectations on the names it binds: none when it binds none of them. A
   component the signature says nothing about is left to synthesis. */
let rec expected_pat_type =
        (~ctx: Ctx.t, expected: list(Sig.member), pat: Pat.t)
        : option(pat_expectation) => {
  let go = expected_pat_type(~ctx, expected);
  let list_of = (e: pat_expectation) => {
    ...e,
    ty: List(e.ty) |> Typ.temp,
  };
  switch (pat.term) {
  | Var(x) =>
    Sig.find_value(expected, x)
    |> Option.map(ty =>
         {
           ty,
           names: [x],
         }
       )
  | Asc(p, _)
  | Parens(p)
  | Projector(_, p) => go(p)
  | TupLabel({term: Label(l), _}, p) =>
    go(p)
    |> Option.map(e =>
         {
           ...e,
           ty: TupLabel(Label(l) |> Typ.temp, e.ty) |> Typ.temp,
         }
       )
  | Tuple(ps) =>
    let es = List.map(go, ps);
    List.exists(Option.is_some, es)
      ? Some({
          ty:
            Prod(
              List.map(
                Option.fold(
                  ~none=Unknown(SynSwitch) |> Typ.temp,
                  ~some=(e: pat_expectation) =>
                  e.ty
                ),
                es,
              ),
            )
            |> Typ.temp,
          names:
            List.concat_map(
              Option.fold(~none=[], ~some=(e: pat_expectation) => e.names),
              es,
            ),
        })
      : None;
  | Cons(p1, p2) =>
    meet_expectations(~ctx, [Option.map(list_of, go(p1)), go(p2)])
  | ListLit(ps) =>
    meet_expectations(~ctx, List.map(p => Option.map(list_of, go(p)), ps))
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
};

/* The items a body exports: of several bindings of a name and kind, the
   last; an item that binds no member is kept. */
let exports = (bound: list(Sig.t)): list(Sig.t) =>
  List.rev(
    Util.ListUtil.dedup_f(
      (a, b) =>
        switch (Sig.member_of_item(a), Sig.member_of_item(b)) {
        | (Some(a), Some(b)) => Sig.same_kind(a, b)
        | (None, _)
        | (_, None) => false
        },
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
      have: list(Sig.t),
    )
    : list(Sig.t) => {
  let want = Sig.members(ana_items);
  have
  |> List.map(item =>
       switch (Sig.member_of_item(item)) {
       | Some(Val(x, ty)) when List.exists(String.equal(x), checked) =>
         switch (Sig.find_value(want, x)) {
         | Some(expected) =>
           Sig.map_typ(_ => StaticsBase.fixed_typ(ctx, expected, ty), item)
         | None => item
         }
       | Some(TypeManifest(name, _))
           when List.exists(String.equal(name), mismatched_types) =>
         switch (Sig.find_type_def(want, name)) {
         | Some(expected) => Sig.map_typ(_ => expected, item)
         | None => item
         }
       | Some(Val(_) | TypeManifest(_))
       | None => item
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
