open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind = Typ.t;
//TODO(andrew)

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry({
      name: Token.t,
      id: Id.t,
      typ: Typ.t,
    })
  | TVarEntry({
      name: Token.t,
      id: Id.t,
      kind,
    }); //: Kind.t,

let get_id = (entry: entry) =>
  switch (entry) {
  | VarEntry({id, _}) => id
  | TVarEntry({id, _}) => id
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

let extend = (entry: entry, ctx: t) => [entry, ...ctx];

let lookup_var = (ctx: t, x) =>
  List.find_map(
    entry =>
      switch (entry) {
      | VarEntry({name, typ, _}) =>
        if (name == x) {
          Some(typ);
        } else {
          None;
        }
      | TVarEntry(_) => None
      },
    ctx,
  );

let lookup_tvar = (ctx: t, x) =>
  List.find_map(
    entry =>
      switch (entry) {
      | TVarEntry({name, kind, _}) =>
        if (name == x) {
          Some(kind);
        } else {
          None;
        }
      | VarEntry(_) => None
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
         switch (entry) {
         | VarEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
         | TVarEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));

let tags = (ctx: t): list((string, Typ.t)) => {
  let adts: list(BuiltinADTs.adt) =
    ctx
    |> List.filter_map(entry =>
         switch (entry) {
         | VarEntry(_) => None
         | TVarEntry({name, kind, _}) =>
           switch (kind) {
           | Typ.LSum(ts) =>
             let guys =
               List.map(
                 ({label, typ}: Typ.tsum) => {
                   let arg =
                     switch (typ) {
                     | Prod([]) => None
                     | ty => Some(ty)
                     };
                   BuiltinADTs.{name: label, arg};
                 },
                 ts,
               );
             Some((name, guys));
           | _ => None
           }
         }
       );
  List.map(
    ((name, tags)) => {
      List.map(
        (adt: BuiltinADTs.tag) =>
          (
            adt.name,
            switch (adt.arg) {
            | None => Typ.Var(name)
            | Some(typ) => Arrow(typ, Var(name))
            },
          ),
        tags,
      )
    },
    adts,
  )
  |> List.flatten;
};
