type t('a) =
  | Root
  | Node('a);

let map = f =>
  fun
  | Root => Root
  | Node(a) => Node(f(a));

let to_option =
  fun
  | Root => None
  | Node(a) => Some(a);
