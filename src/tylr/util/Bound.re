[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) =
  | Root
  | Node('a);

let map = f =>
  fun
  | Root => Root
  | Node(a) => Node(f(a));

let to_opt =
  fun
  | Root => None
  | Node(a) => Some(a);
