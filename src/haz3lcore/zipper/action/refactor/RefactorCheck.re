/* Statics is the scope authority. A prepared transform is accepted only
 * if every reference that survives it (by id, or as a copy of one)
 * resolves to the same binder as before, and it introduces no static
 * error. Transforms move terms; whether names still mean the same thing
 * is statics' call, not a rule re-derived here. Run once per invocation
 * (one statics pass); gating and drag previews stay on the cheap tier. */
open Language;
open RefactorBase;

/* reference id -> binder id (None: unbound), for var, constructor and
   type-variable references */
let resolutions = (info_map: Statics.Map.t): Id.Map.t(option(Id.t)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      if (id != Info.id_of(info)) {
        acc;
      } else {
        switch (resolution_of(info)) {
        | Some(b) => Id.Map.add(id, b, acc)
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

let preserves =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~before: Exp.t,
      after: Exp.t,
    )
    : bool =>
  switch (Id.Map.find_opt(Exp.rep_id(before), info_map)) {
  | Some(root) when settings.statics =>
    let (info', _) = Statics.mk(settings, Info.ctx_of(root), after);
    let old = resolutions(info_map);
    let bound_same =
      Id.Map.for_all(
        (ref', binder') =>
          switch (Id.Map.find_opt(origin(ref'), old)) {
          | Some(Some(b)) =>
            let expected =
              Id.Map.find_opt(b, binder_redirect^)
              |> Option.value(~default=b);
            Option.map(origin, binder') == Some(expected);
          /* was unbound or is new: binding it is not a change of meaning */
          | Some(None)
          | None => true
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
