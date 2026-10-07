/* Statics is the scope authority. A prepared transform is accepted only
 * if every reference that survives it (by id, or as a copy of one)
 * resolves to the same binder as before, and it introduces no static
 * error. Transforms move terms; whether names still mean the same thing
 * is statics' call, not a rule re-derived here. Run once per invocation
 * (one statics pass); gating and drag previews stay on the cheap tier. */
open Language;
open RefactorBase;

/* reference id -> (binder id or None when unbound, lexical?). Lexical
   references are the ones the check holds to their binder: expression
   variables, and type variables bound by typfun/poly (Abstract).
   Constructor and alias entries are ids inside a particular type
   definition, so moving that definition (inline/feed/rename an alias)
   changes them without changing meaning — those stay with the
   new-error rule, where a lost name shows up unbound. */
let resolutions = (info_map: Statics.Map.t): Id.Map.t((option(Id.t), bool)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      if (id != Info.id_of(info)) {
        acc;
      } else {
        let lexical =
          switch (info) {
          | InfoExp({user_term: {term: Var(_), _}, _}) => true
          | InfoTyp({user_term: {term: Var(t), _}, ctx, _}) =>
            Ctx.lookup_tvar(ctx, t) == Some(Abstract)
          | _ => false
          };
        switch (resolution_of(info)) {
        | Some(b) => Id.Map.add(id, (b, lexical), acc)
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
        (ref', (binder', _)) =>
          switch (Id.Map.find_opt(origin(ref'), old)) {
          | Some((Some(b), true)) =>
            let expected =
              Id.Map.find_opt(b, binder_redirect^)
              |> Option.value(~default=b);
            Option.map(origin, binder') == Some(expected);
          /* was unbound, isn't lexical, or is new */
          | _ => true
          },
        resolutions(info'),
      );
    let old_errors =
      Statics.Map.error_ids(info_map)
      |> List.fold_left((s, id) => Id.Map.add(id, (), s), Id.Map.empty);
    let no_new_errors =
      Statics.Map.error_ids(info')
      |> List.for_all(id => Id.Map.mem(origin(id), old_errors));
    bound_same && no_new_errors;
  /* no statics to consult (disabled, or no root info): cheap tier only */
  | _ => true
  };
