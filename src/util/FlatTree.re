open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // Map from position to index in elems.
  tree: Tree.t(int),
  // Map to index (identity) if the elem is not yet assigned, otherwise 0.
  // This is designed s.t. the first elem is always assigned.
  assignment: list(int),
};

let map = ({tree, _}, l) => Tree.map(i => List.nth(l, i), tree);

let init = len => {tree: Node(0, []), assignment: List.init(len, Fun.id)};

let find_empty = l =>
  try(Some(List.find(i => i != 0, l))) {
  | Not_found => None
  };

// Note: pos not in tree behavior => no change
// Note: pos already empty behavior => no change
let set_empty = (l, i) => l |> ListUtil.put_nth(i, i);

// Note: pos not in tree behavior => no change
// Note: pos already assigned behavior => no change
let set_assigned = (l, i) => l |> ListUtil.put_nth(i, 0);

type add_error =
  | InvalidPosition(Tree.pos)
  | RunOutOfElements;

let add = (pos, {tree, assignment}) =>
  switch (find_empty(assignment)) {
  | Some(i) =>
    Tree.exists_pos(tree, pos)
      ? Ok({
          tree: Tree.add(i, tree, pos),
          assignment: set_assigned(assignment, i),
        })
      : Error(InvalidPosition(pos))
  | None => Error(RunOutOfElements)
  };

type del_error =
  | InvalidPosition(Tree.pos);

// Note: it is not an error if the node has no children => no change
let del = (pos, {tree, assignment}) =>
  Tree.exists_pos(tree, pos)
    ? Ok({
        tree: Tree.del(tree, pos),
        assignment:
          Tree.get_tree(tree, pos)
          |> Tree.flatten
          |> List.fold_left(set_empty, assignment),
      })
    : Error(InvalidPosition(pos));
