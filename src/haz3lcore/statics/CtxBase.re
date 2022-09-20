open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry('item) = {
  id: Id.t,
  item: 'item,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type ctx('item) = VarMap.t_(entry('item));

[@deriving (show({with_path: false}), sexp, yojson)]
type co_item('mode) = {
  id: Id.t,
  mode: 'mode,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type co_entry('mode) = list(co_item('mode));

[@deriving (show({with_path: false}), sexp, yojson)]
type co_ctx('mode) = VarMap.t_(co_entry('mode));

let empty = VarMap.empty;
let extend = VarMap.extend;
let lookup = VarMap.lookup;
let union = VarMap.union;

let subtract = (ctx: ctx('item), free: co_ctx('mode)): co_ctx('mode) =>
  VarMap.filter(
    ((k, _)) =>
      switch (VarMap.lookup(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );

let subtract_prefix =
    (ctx: ctx('item), prefix_ctx: ctx('item)): option(ctx('item)) => {
  // NOTE: does not check that the prefix is an actual prefix
  // TODO: does not correctly handle shadowing!! (will be fixed with new context in type aliases branch so not worrying about it for now)
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
let co_union = (co_ctxs: list(co_ctx('mode))): co_ctx('mode) =>
  List.fold_left((free1, free2) => free1 @ free2, [], co_ctxs);
