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

let rec sum_definition = (params, ty) =>
  switch (Typ.term_of(ty)) {
  | TypFun(param, body) =>
    sum_definition(params @ TPat.binders_of(param), body)
  | Parens(body)
  | Rec(_, body) => sum_definition(params, body)
  | Sum(constructors) => Some((params, constructors))
  | _ => None
  };

let rec constructor_payload = (query: Typ.t) =>
  switch (Typ.term_of(query)) {
  | Poly(_, body)
  | Parens(body) => constructor_payload(body)
  | Arrow(payload, _) => Some(payload)
  | _ => None
  };

let rec minimal_alias = (name, payload, definition: Typ.t): Typ.t =>
  switch (Typ.term_of(definition)) {
  | TypFun(param, body) => {
      ...definition,
      term: TypFun(param, minimal_alias(name, payload, body)),
    }
  | Rec(_, body) => minimal_alias(name, payload, body)
  | Sum(constructors) => {
      ...definition,
      term:
        Sum(
          List.map(
            fun
            | ConstructorMap.Variant(constructor, ann, arg)
                when constructor == name =>
              ConstructorMap.Variant(
                constructor,
                ann,
                Option.map(
                  _ =>
                    switch (payload) {
                    | Some(payload) when !is_gap(payload) => payload
                    | _ => gap
                    },
                  arg,
                ),
              )
            | _ => ConstructorMap.BadEntry(gap),
            constructors,
          ),
        ),
    }
  | _ => definition
  };

let constructor_alias = (ctx: Ctx.t, name: string) =>
  List.find_map(
    fun
    | Ctx.TVarEntry({name: alias, kind: Singleton(definition), _} as entry) => {
        let constructor =
          switch (Typ.term_of(definition)) {
          | Var(constructor)
              when
                constructor == name
                && Ctx.lookup_alias(ctx, constructor) == None =>
            Some(
              {
                name,
                id: Typ.rep_id(definition),
                typ: Var(alias) |> Typ.temp,
                custom_statics: None,
              }: Ctx.var_entry,
            )
          | TypParamAp({term: Var(constructor), _}, payload)
              when
                constructor == name
                && Ctx.lookup_alias(ctx, constructor) == None =>
            Some(
              {
                name,
                id: Typ.rep_id(definition),
                typ: Arrow(payload, Var(alias) |> Typ.temp) |> Typ.temp,
                custom_statics: None,
              }: Ctx.var_entry,
            )
          | _ =>
            Option.bind(
              sum_definition([], definition), ((params, constructors)) =>
              Ctx.add_ctrs_with_params(Ctx.empty, alias, params, constructors)
              |> Ctx.lookup_ctr(_, name)
            )
          };
        Option.map(constructor => (constructor, entry), constructor);
      }
    | _ => None,
    ctx.entries,
  );

let constructor_from_alias = (ctx: Ctx.t, name: string, query) =>
  Option.map(
    ((constructor, entry: Ctx.tvar_entry)) =>
      switch (entry.kind) {
      | Singleton(definition) => (
          constructor,
          {
            ...entry,
            kind:
              Singleton(
                minimal_alias(name, constructor_payload(query), definition),
              ),
          },
        )
      | Abstract => (constructor, entry)
      },
    constructor_alias(ctx, name),
  );

let reference = (ctx: Ctx.t, ~use, name): Info.slice_reference =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(value) => {
      name,
      use,
      value: Some(value),
      constructor: None,
      alias: None,
      demand_as_value: true,
    }
  | None =>
    switch (constructor_alias(ctx, name)) {
    | Some((constructor, alias)) => {
        name,
        use,
        value: None,
        constructor: Some(constructor),
        alias: Some(alias),
        demand_as_value: false,
      }
    | None => {
        name,
        use,
        value: None,
        constructor: Ctx.lookup_ctr(ctx, name),
        alias: None,
        demand_as_value: Ctx.lookup_ctr(ctx, name) == None,
      }
    }
  };

let constructor_reference = (ctx: Ctx.t, ~use, name): Info.slice_reference =>
  switch (constructor_alias(ctx, name)) {
  | Some((constructor, alias)) => {
      name,
      use,
      value: None,
      constructor: Some(constructor),
      alias: Some(alias),
      demand_as_value: false,
    }
  | None => {
      name,
      use,
      value: None,
      constructor: Ctx.lookup_ctr(ctx, name),
      alias: None,
      demand_as_value: Ctx.lookup_ctr(ctx, name) == None,
    }
  };

let references = (ctx: Ctx.t, ~use, co_ctx: CoCtx.t) =>
  co_ctx |> List.map(((name, _)) => reference(ctx, ~use, name));

let context_for_name = (ctx: Ctx.t, name: string, query): Ctx.t =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) => Ctx.extend(Ctx.empty, Ctx.VarEntry(entry))
  | None =>
    switch (constructor_from_alias(ctx, name, query)) {
    | Some((constructor, alias)) =>
      Ctx.empty
      |> Ctx.extend(_, Ctx.TVarEntry(alias))
      |> Ctx.extend(_, Ctx.ConstructorEntry(constructor))
    | None =>
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some(entry) => Ctx.extend(Ctx.empty, Ctx.ConstructorEntry(entry))
      | None => Ctx.empty
      }
    }
  };

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

let context_for_reference = (reference: Info.slice_reference, query): Ctx.t => {
  let context =
    switch (reference.value, reference.constructor, reference.alias) {
    | (Some(value), _, _) => Ctx.extend(Ctx.empty, Ctx.VarEntry(value))
    | (
        _,
        Some(constructor),
        Some({kind: Singleton(definition), _} as alias),
      ) =>
      Ctx.empty
      |> Ctx.extend(
           _,
           Ctx.TVarEntry({
             ...alias,
             kind:
               Singleton(
                 minimal_alias(
                   reference.name,
                   constructor_payload(query),
                   definition,
                 ),
               ),
           }),
         )
      |> Ctx.extend(_, Ctx.ConstructorEntry(constructor))
    | (_, Some(constructor), _) =>
      Ctx.extend(Ctx.empty, Ctx.ConstructorEntry(constructor))
    | _ => Ctx.empty
    };
  reference.demand_as_value
    ? demand_entry(context, ~use=reference.use, reference.name, query)
    : context;
};

let context_has_constructor = (context: Ctx.t) =>
  List.exists(
    fun
    | Ctx.ConstructorEntry(_) => true
    | _ => false,
    context.entries,
  );
