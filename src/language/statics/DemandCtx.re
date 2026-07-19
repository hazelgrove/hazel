open TypQuery;

let close_sum_gaps = ty =>
  Typ.map_term(
    ~f_typ=
      (continue, ty) =>
        switch (Typ.term_of(ty)) {
        | Sum(items)
            when
              List.exists(
                fun
                | ConstructorMap.Variant(_, _, _) => true
                | _ => false,
                items,
              ) => {
            ...ty,
            term:
              Sum(
                List.filter(
                  fun
                  | ConstructorMap.BadEntry(_) => false
                  | _ => true,
                  items,
                ),
              ),
          }
        | _ => continue(ty)
        },
    ty,
  );

let context_key =
  fun
  | Ctx.VarEntry({name, _}) => Some((0, name))
  | Ctx.ConstructorEntry({name, _}) => Some((1, name))
  | Ctx.TVarEntry({name, _}) => Some((2, name))
  | Ctx.LivelitEntry(_) => None;

let tvar_entry = (ctx: Ctx.t, name) =>
  List.find_map(
    fun
    | Ctx.TVarEntry(entry) when entry.name == name => Some(entry)
    | _ => None,
    ctx.entries,
  );

let minimal_tvar = (entry: Ctx.tvar_entry) => {
  ...entry,
  kind: Singleton(gap),
};

let merge_context = (~merge, left: Ctx.t, right: Ctx.t): Ctx.t => {
  ...left,
  entries:
    List.fold_left(
      (entries, entry) =>
        switch (context_key(entry)) {
        | Some(key)
            when List.exists(e => context_key(e) == Some(key), entries) =>
          List.map(
            old => context_key(old) == Some(key) ? merge(old, entry) : old,
            entries,
          )
        | _ => entries @ [entry]
        },
      left.entries,
      right.entries,
    ),
};

let context_join = (left, right) =>
  merge_context(~merge=(old, _) => old, left, right);

let context_join_branches = (ctx, left, right) =>
  merge_context(
    ~merge=
      (old, entry) =>
        switch (old, entry) {
        | (
            Ctx.TVarEntry({name, kind: Singleton(a), _} as old),
            Ctx.TVarEntry({name: other, kind: Singleton(b), _}),
          )
            when name == other =>
          Ctx.TVarEntry({
            ...old,
            kind: Singleton(close_sum_gaps(meet(ctx, a, b))),
          })
        | (Ctx.VarEntry(old), Ctx.VarEntry(entry)) =>
          Ctx.VarEntry({
            ...old,
            typ: meet(ctx, old.typ, entry.typ),
          })
        | _ => old
        },
    left,
    right,
  );

let gamma_join = (ctx: Ctx.t, left: Ctx.t, right: Ctx.t): Ctx.t =>
  merge_context(
    ~merge=
      (old, entry) =>
        switch (old, entry) {
        | (Ctx.VarEntry(old), Ctx.VarEntry(entry)) =>
          Ctx.VarEntry({
            ...old,
            typ: meet(ctx, old.typ, entry.typ),
          })
        | _ => old
        },
    left,
    right,
  );

let gamma_remove = (gamma: Ctx.t, names: list(string)): Ctx.t => {
  ...gamma,
  entries:
    List.filter(
      fun
      | Ctx.VarEntry({name, _}) => !List.mem(name, names)
      | _ => true,
      gamma.entries,
    ),
};

let lookup_demand = (gamma: Ctx.t, name: string): option(Typ.t) =>
  Option.map(
    (entry: Ctx.var_entry) => entry.typ,
    Ctx.lookup_var(gamma, name),
  );

let demand_entry = (gamma: Ctx.t, ~use: Id.t, name: string, ty: Typ.t): Ctx.t =>
  is_gap(ty)
    ? gamma
    : merge_context(
        ~merge=
          (old, entry) =>
            switch (old, entry) {
            | (Ctx.VarEntry(old), Ctx.VarEntry(entry)) =>
              Ctx.VarEntry({
                ...old,
                typ: entry.typ,
              })
            | _ => old
            },
        gamma,
        Ctx.extend(
          Ctx.empty,
          Ctx.VarEntry({
            name,
            id: use,
            typ: ty,
            custom_statics: None,
          }),
        ),
      );

let context_has_constructor = (context: Ctx.t) =>
  List.exists(
    fun
    | Ctx.ConstructorEntry(_) => true
    | _ => false,
    context.entries,
  );
