open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // Map from position to index in elems.
  tree: Tree.t(int),
  idles: list(int),
};

let mk_tree = (l, t) => Tree.map(i => List.nth(l, i), t);

// @raise `Failure` if len = 0
// @raise `Invalid_argument` if len < 0
let init = len => {
  tree: Tree.init(Fun.const(0)),
  idles: List.init(len, Fun.id) |> List.tl,
};

let init_with = (max_len, t) => {
  let flattened = t |> Tree.flatten;
  let (len, tree) = t |> Tree.fold_left_map((acc, _) => (acc + 1, acc), 0);
  let idles = List.init(max_len - len, i => i + len);
  (flattened, {tree, idles});
};

[@deriving (show({with_path: false}), sexp, yojson)]
type add_error =
  | InvalidPosition(Tree.pos)
  | RunOutOfElements;

let add_opt = (pos, {tree, idles}) =>
  Tree.exists_pos(tree, pos)
    ? switch (ListUtil.split_first_opt(idles)) {
      | Some((i, idles)) => Ok({tree: Tree.add(i, tree, pos), idles})
      | None => Error(RunOutOfElements)
      }
    : Error(InvalidPosition(pos));

let add = (pos, state) =>
  switch (add_opt(pos, state)) {
  | Ok(state) => state
  | Error(_) => failwith("FlatTree.add Errer")
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type del_error =
  | InvalidPosition(Tree.pos)
  | NoChildToDel(Tree.pos);

let del_opt = (pos, {tree, idles}) =>
  Tree.exists_pos(tree, pos)
    ? switch (Tree.del_opt(tree, pos)) {
      | Some((hd, tree)) =>
        Ok({tree, idles: idles |> Tree.fold_right(List.cons, hd)})
      | None => Error(NoChildToDel(pos))
      }
    : Error(InvalidPosition(pos));

let del = (pos, state) =>
  switch (del_opt(pos, state)) {
  | Ok(state) => state
  | Error(_) => failwith("FlatTree.del Error")
  };
