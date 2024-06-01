open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // Map from position to index in elems.
  tree: Tree.t(int),
  idles: list(int),
};

let mk_tree = l => l |> List.nth |> Tree.map;

// @raise `Failure` if len = 0
// @raise `Invalid_argument` if len < 0
let init = len => {
  tree: Tree.init(Fun.const(0)),
  idles: List.init(len, Fun.id) |> List.tl,
};

type add_error =
  | InvalidPosition(Tree.pos)
  | RunOutOfElements;

let add = (pos, {tree, idles}) =>
  Tree.exists_pos(tree, pos)
    ? switch (ListUtil.split_first_opt(idles)) {
      | Some((i, idles)) => Ok({tree: Tree.add(i, tree, pos), idles})
      | None => Error(RunOutOfElements)
      }
    : Error(InvalidPosition(pos));

type del_error =
  | InvalidPosition(Tree.pos)
  | NoChildToDel(Tree.pos);

let del = (pos, {tree, idles}) =>
  Tree.exists_pos(tree, pos)
    ? switch (Tree.del_opt(tree, pos)) {
      | Some((hd, tree)) =>
        Ok({tree, idles: idles |> Tree.fold_right(List.cons, hd)})
      | None => Error(NoChildToDel(pos))
      }
    : Error(InvalidPosition(pos));
