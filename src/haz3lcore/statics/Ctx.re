include TypBase.Ctx;
open Util;

let empty: t = VarMap.empty;

let extend: (entry, t) => t = List.cons;

let get_id: entry => int =
  fun
  | VarEntry({id, _})
  | TagEntry({id, _})
  | TVarEntry({id, _}) => id;

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx,
  );

let add_tags = (ctx: t, name: Token.t, id: Id.t, tags: Typ.sum_map): t =>
  List.map(
    ((tag, typ)) =>
      TagEntry({
        name: tag,
        id,
        typ:
          switch (typ) {
          | None => Var(name)
          | Some(typ) => Arrow(typ, Var(name))
          },
      }),
    tags,
  )
  @ ctx;

let added_bindings = (ctx_after: t, ctx_before: t): t => {
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count = List.length(ctx_after) - List.length(ctx_before);
  switch (ListUtil.split_n_opt(new_count, ctx_after)) {
  | Some((ctx, _)) => ctx
  | _ => []
  };
};

let free_in = (ctx_before: t, ctx_after, free: co): co => {
  let added_bindings = added_bindings(ctx_after, ctx_before);
  VarMap.filter(
    ((k, _)) =>
      switch (lookup_var(added_bindings, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );
};

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx);
  let ctx_length = List.length(ctx);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
      ),
    );
  };
};

/* Note: this currently shadows in the case of duplicates */
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);

module VarSet = Set.Make(Token);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), entry) => {
         switch (entry) {
         | VarEntry({name, _})
         | TagEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
         | TVarEntry({name, _}) =>
           VarSet.mem(name, typ_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));
