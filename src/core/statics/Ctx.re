open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  typ: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(entry);

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

let subtract = (ctx: t, free: co): co =>
  VarMap.filter(
    ((k, _)) =>
      switch (VarMap.lookup(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );

//TODO(andrew): is this correct in the case of duplicates?
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);
