open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type p('a) =
  | Node('a, list(p('a)));

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Value
  | Children(int, pos);

// Example:
// Input: Children(2, Children(1, Value))
// Output: 1, Children(2, Value)
let rec pos_split_last =
  fun
  | Value => failwith("Tree.pos_split: cannot split")
  | Children(i, Value) => (i, Value)
  | Children(i, p) => {
      let (i', p) = pos_split_last(p);
      (i', Children(i, p));
    };

let rec farthest_cond = (f, Node(_, c)) =>
  fun
  | _ when c == [] => Value
  | Value => Value
  | Children(i, pos) => {
      let i = min(i, List.length(c) - 1);
      let Node(v, _) as p = List.nth(c, i);
      f(v) ? Children(i, pos |> farthest_cond(f, p)) : Value;
    };

let value = (Node(v, _)) => v;

// @raise `Failure` if pos not exists in the tree
let rec nth_node = (Node(v, c)) =>
  fun
  | Value => Node(v, c)
  | Children(i, pos) => pos |> nth_node(List.nth(c, i));

// @raise `Failure` if pos not exists in the tree
let nth = (t, pos) => nth_node(t, pos) |> value;

let empty = v => Node(v, []);

let rec flatten = (Node(v, c)) =>
  [v] @ (c |> List.map(flatten) |> List.concat);

/* Two Tree */

// @return `false` if the two trees have different structures
let rec equal = (eq, Node(v1, c1), Node(v2, c2)) =>
  eq(v1, v2) && List.equal(equal(eq), c1, c2);

let equal_struct = (n1, n2) => equal((_, _) => true, n1, n2);

// @raise `Invalid_argument` if the two trees have different structures
let rec combine = (Node(v1, c1), Node(v2, c2)) =>
  Node((v1, v2), List.map2(combine, c1, c2));

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

/* Position */

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

// Insert a new child at the given position
let insert = (v', i) =>
  map_nth_node((Node(v, c)) => Node(v, ListUtil.insert(empty(v'), c, i)));

let remove = i =>
  split_n((Node(v, c)) =>
    (List.nth(c, i), Node(v, ListUtil.remove(c, i)))
  );
