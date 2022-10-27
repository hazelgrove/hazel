open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type value =
  | Typ(Typ.t)
  | Kind(Kind.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: Token.t,
  id: Id.t,
  value,
};

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

let empty = VarMap.empty;

let lookup_var = (ctx: t, x) =>
  List.find_map(
    ({name, value, _}) =>
      switch (value) {
      | Typ(t) =>
        if (name == x) {
          Some(t);
        } else {
          None;
        }
      | Kind(_) => None
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

//TODO(andrew): is this correct in the case of duplicates?
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);

module VarSet = Set.Make(Token);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), entry) => {
         switch (entry.value) {
         | Typ(_) =>
           VarSet.mem(entry.name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], VarSet.add(entry.name, term_set), typ_set)
         | Kind(_) =>
           VarSet.mem(entry.name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(entry.name, typ_set))
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));
