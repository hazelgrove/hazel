/* Statics is the scope authority. A prepared transform is accepted only
 * if every reference that survives it (by id, or as a copy of one)
 * resolves to the same binder as before, and it introduces no static
 * error. Transforms move terms; whether names still mean the same thing
 * is statics' call, not a rule re-derived here. Run once per invocation
 * (one statics pass); gating and drag previews stay on the cheap tier. */
open Language;
open RefactorBase;

/* how the check holds a reference: an expression variable to its exact
   binder; a type variable bound by typfun/poly (Abstract) to staying
   bound by one — analysis against an annotation may legitimately hand
   it the annotation's own poly binder, but it must never come unbound;
   constructor and alias references not at all (their entries are ids
   inside a type definition that inline/feed/rename may move) — the
   new-error rule covers those */
type held =
  | Exact
  | AbstractTVar
  | Free;

let resolutions = (info_map: Statics.Map.t): Id.Map.t((option(Id.t), held)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      if (id != Info.id_of(info)) {
        acc;
      } else {
        let held =
          switch (info) {
          | InfoExp({user_term: {term: Var(_), _}, _}) => Exact
          | InfoTyp({user_term: {term: Var(t), _}, ctx, _})
              when Ctx.lookup_tvar(ctx, t) == Some(Abstract) =>
            AbstractTVar
          | _ => Free
          };
        switch (resolution_of(info)) {
        | Some(b) => Id.Map.add(id, (b, held), acc)
        | None => acc
        };
      },
    info_map,
    Id.Map.empty,
  );

let rec origin = (id: Id.t): Id.t =>
  switch (Id.Map.find_opt(id, id_origin^)) {
  | Some(o) when o != id => origin(o)
  | _ => id
  };

/* off only for measurement */
let enabled = ref(true);

let preserves =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~before: Exp.t,
      after: Exp.t,
    )
    : bool =>
  switch (Id.Map.find_opt(Exp.rep_id(before), info_map)) {
  | Some(root) when settings.statics && enabled^ =>
    let (info', _) = Statics.mk(settings, Info.ctx_of(root), after);
    let old = resolutions(info_map);
    let bound_same =
      Id.Map.for_all(
        (ref', (binder', held')) =>
          switch (Id.Map.find_opt(origin(ref'), old)) {
          | Some((Some(b), Exact)) =>
            let expected =
              Id.Map.find_opt(b, binder_redirect^)
              |> Option.value(~default=b);
            Option.map(origin, binder') == Some(expected);
          | Some((Some(_), AbstractTVar)) =>
            binder' != None && held' == AbstractTVar
          /* was unbound, isn't held, or is new */
          | _ => true
          },
        resolutions(info'),
      );
    /* a number literal's meaning can depend on scope too (`use Nat in`
       makes 2 a Nat, no binder involved): a surviving number literal
       keeps its numeric kind. Only the kind — other type differences
       (an inferred type that moved) aren't the literal's meaning. */
    let numeric_kinds = (m: Statics.Map.t) =>
      Id.Map.fold(
        (id, info: Info.t, acc) =>
          switch (info) {
          | InfoExp({user_term: {term: Atom(_), _}, ty, ctx, _})
              when id == Info.id_of(info) =>
            switch (IdTagged.term_of(Typ.normalize(ctx, ty))) {
            | Atom((Int | SInt | Nat | Float) as k) => Id.Map.add(id, k, acc)
            | _ => acc
            }
          | _ => acc
          },
        m,
        Id.Map.empty,
      );
    let old_kinds = numeric_kinds(info_map);
    let literals_same =
      Id.Map.for_all(
        (id, k') =>
          switch (Id.Map.find_opt(origin(id), old_kinds)) {
          | Some(k) => k == k'
          | None => true
          },
        numeric_kinds(info'),
      );
    let old_errors =
      Statics.Map.error_ids(info_map)
      |> List.fold_left((s, id) => Id.Map.add(id, (), s), Id.Map.empty);
    let no_new_errors =
      Statics.Map.error_ids(info')
      |> List.for_all(id => Id.Map.mem(origin(id), old_errors));
    bound_same && literals_same && no_new_errors;
  /* no statics to consult (disabled, or no root info): cheap tier only */
  | _ => true
  };
