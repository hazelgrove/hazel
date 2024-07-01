open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type p('a) =
  | Node('a, list(p('a)));

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Value
  | Children(int, pos);

let value = (Node(v, _)) => v;

let children = (Node(_, c)) => c;

// @raise `Failure` if children is empty
let hd_children = t => t |> children |> List.hd;

let hd_children_opt = t => t |> children |> ListUtil.hd_opt;

// @raise `Failure` if children is empty
let tl_children = t => t |> children |> List.tl;

let tl_children_opt = t =>
  t
  |> children
  |> (
    fun
    | [] => None
    | [_, ...tl] => Some(tl)
  );

// @raise `Failure` if pos not exists in the tree
let rec nth_node = (Node(v, c)) =>
  fun
  | Value => Node(v, c)
  | Children(i, pos) => pos |> nth_node(List.nth(c, i));

let nth_node_opt = (t, pos) =>
  try(Some(nth_node(t, pos))) {
  | Failure(_) => None
  };

// @raise `Failure` if pos not exists in the tree
let nth = (t, pos) => nth_node(t, pos) |> value;

let nth_opt = (t, pos) =>
  try(Some(nth(t, pos))) {
  | Failure(_) => None
  };

let init = f => Node(f(), []);

let rec flatten = (Node(v, c)) =>
  [v] @ (c |> List.map(flatten) |> List.concat);

/* Two Tree */

// @return `false` if the two trees have different structures
let rec equal = (eq, Node(v1, c1), Node(v2, c2)) =>
  eq(v1, v2) && List.equal(equal(eq), c1, c2);

let equal_struct = (n1, n2) => equal((_, _) => true, n1, n2);

// @raise `Invalid_argument` if the two trees have different structures
let rec combine = ((Node(v1, c1), Node(v2, c2))) =>
  Node((v1, v2), List.combine(c1, c2) |> List.map(combine));

/* Iterators */

let rec map = (f, Node(v, c)) => Node(f(v), c |> List.map(map(f)));

let mapi = f => {
  let rec aux = (f, acc_pos, Node(v, c)) =>
    Node(
      v |> f(acc_pos(Value)),
      c |> List.mapi(i => aux(f, pos => acc_pos(Children(i, pos)))),
    );
  aux(f, Fun.id);
};

let rec fold_deep = (f, Node(v, c)) => f(v, c |> List.map(fold_deep(f)));

let rec fold_left_map = (f, init, Node(v, c)) => {
  let (init, v) = f(init, v);
  let (final, c) = c |> ListUtil.fold_left_map(fold_left_map(f), init);
  (final, Node(v, c));
};

let fold_right = (f, n) => n |> flatten |> List.fold_right(f);

let fold_left = (f, init, n) => n |> flatten |> List.fold_left(f, init);

/* Scanning */

let rec exists = (f, Node(v, c)) =>
  f(v) || c |> List.exists(node => exists(f, node));

let rec for_all = (f, Node(v, c)) =>
  f(v) && c |> List.for_all(node => for_all(f, node));

/* Position */

let flatten_pos = t => t |> mapi((pos, _) => pos) |> flatten;

let exists_pos = (t, pos) =>
  try(nth_node(t, pos) |> Fun.const(true)) {
  | Failure(_) => false
  };

// For all functions below:
// @failwith("out of bounds") if pos not exists in the tree 😱

let rec map_nth_node = (f, Node(v, c)) =>
  fun
  | Value => f(Node(v, c))
  | Children(i, pos) =>
    Node(v, c |> ListUtil.map_nth(i, t => map_nth_node(f, t, pos)));

let map_nth = f => map_nth_node((Node(v, c)) => Node(f(v), c));

let put_nth_node = t' => map_nth_node(_ => t');

let put_nth = v' => map_nth(_ => v');

let rec split_n = (f, Node(v, c)) =>
  fun
  | Value => f(Node(v, c))
  | Children(i, pos) => {
      let (v', t) = pos |> split_n(f, List.nth(c, i));
      (v', Node(v, c |> ListUtil.put_nth(i, t)));
    };

// Add a new child to the node at the given position
let add = v' =>
  map_nth_node((Node(v, c)) => Node(v, [init(Fun.const(v')), ...c]));

// Remove a child from the node at the given position
// @raise `Failure` if children is empty
let del = pos =>
  pos |> split_n((Node(v, c)) => (List.hd(c), Node(v, List.tl(c))));

let del_opt = (t, pos) =>
  try(Some(del(t, pos))) {
  | Failure(_) => None
  };

// Insert a new child at the given position
let insert = (v', i) =>
  map_nth_node((Node(v, c)) =>
    Node(v, ListUtil.insert(init(Fun.const(v')), c, i))
  );

let remove = i =>
  split_n((Node(v, c)) =>
    (List.nth(c, i), Node(v, ListUtil.remove(c, i)))
  );
