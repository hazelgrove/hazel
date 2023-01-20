open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  name: Token.t,
  id: Id.t,
  typ: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry(var_entry)
  | TVarEntry({
      name: Token.t,
      id: Id.t,
      kind: Kind.t,
    })
  /* TagEntries are listed right at the position of TVar;
     the relative order doesn't matter, but the idx to the Tag is the same as
     the index to the TVarEntry, which is the type of a tag of a data ctor. */
  | TagEntry({
      name: Token.t,
      id: Id.t,
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type co_item = {
  id: Id.t,
  mode: Typ.mode,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type co_entry = list(co_item);

[@deriving (show({with_path: false}), sexp, yojson)]
type co = VarMap.t_(co_entry);

let lookup_tvar = (ctx: t, t: Token.t) =>
  List.find_map(
    fun
    | TVarEntry({name, kind, _}) when name == t => Some(kind)
    | _ => None,
    ctx,
  );

let rec lookup_tvar_idx = (~i=0, ctx: t, x: Token.t) => {
  switch (ctx) {
  | [] => None
  | [TVarEntry({name, _}), ..._] when Token.compare(name, x) == 0 =>
    Some(i)
  | [TVarEntry(_), ...ctx] => lookup_tvar_idx(ctx, x, ~i=i + 1)
  | [_entry, ...ctx] => lookup_tvar_idx(ctx, x, ~i)
  };
};

let rec lookup_tag_typ = (~i=0, ctx: t, x: Token.t): option(Typ.t) => {
  switch (ctx) {
  | [] => None
  | [TVarEntry(_), ...ctx] => lookup_tag_typ(ctx, x, ~i=i + 1)
  | [TagEntry({name, _}), ..._] when Token.compare(name, x) == 0 =>
    Some(Typ.Var(Typ.{item: Some(i), ann: name}))
  | [_entry, ...ctx] => lookup_tag_typ(ctx, x, ~i)
  };
};

let is_tvar = (ctx: t, name: Token.t) =>
  switch (lookup_tvar(ctx, name)) {
  | Some(_) => true
  | None => false
  };

let get_id =
  fun
  | VarEntry({id, _}) => id
  | TVarEntry({id, _}) => id
  | TagEntry({id, _}) => id;
let empty: t = VarMap.empty;

let extend = (entry: entry, ctx: t): t => [entry, ...ctx];

let extend_utsum = (sum: TermBase.UTSum.t, ctx: t): t => {
  let rec utsum_tags = (term: TermBase.UTSum.term) =>
    switch (term) {
    | Invalid(_)
    | MultiHole(_) => []
    | EmptyHole => []
    | Ap(tag, _) => [tag]
    | Sum(ts) =>
      List.map((TermBase.UTSum.{term, _}) => utsum_tags(term), ts)
      |> List.flatten
    };
  let tags =
    List.map(name => {TagEntry({name, id: 0})}, utsum_tags(sum.term));
  // List.map2(
  //   (name, id) => {TagEntry({name, id})},
  //   utsum_tags(sum.term),
  //   sum.ids,
  // );
  tags @ ctx;
};

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    entry =>
      switch (entry) {
      | VarEntry(var) =>
        if (var.name == name) {
          Some(var);
        } else {
          None;
        }
      | TVarEntry(_)
      | TagEntry(_) => None
      },
    ctx,
  );

let subtract_typ = (ctx: t, free: co): co =>
  VarMap.filter(
    ((k, _)) =>
      switch (lookup_var(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx);
  let ctx_length = List.length(ctx);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        Util.ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
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
         | VarEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
         | TVarEntry({name, _}) =>
           VarSet.mem(name, typ_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
         | TagEntry(_) => ([entry, ...ctx], term_set, typ_set)
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));
