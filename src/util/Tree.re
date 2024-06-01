open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | Node('a, list(t('a)));

let rec fold = (f, Node(v, c)) => f(v, c |> List.map(fold(f)));

let rec combine = ((Node(v1, c1), Node(v2, c2))) =>
  Node((v1, v2), List.combine(c1, c2) |> List.map(combine));

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Value
  | Children(int, pos);

let rec map = (f, Node(v, c)) => Node(f(v), c |> List.map(map(f)));

let mapi = f => {
  let rec aux = (f, acc_pos, Node(v, c)) =>
    Node(
      v |> f(acc_pos(Value)),
      c |> List.mapi(i => aux(f, pos => acc_pos(Children(i, pos)))),
    );
  aux(f, Fun.id);
};

let rec flatten = (Node(v, c)) =>
  [v] @ (c |> List.map(flatten) |> List.concat);

let flatten_pos = t => t |> mapi((pos, _) => pos) |> flatten;

let rec exists = (f, Node(v, c)) =>
  f(v) || c |> List.exists(node => exists(f, node));

let rec for_all = (f, Node(v, c)) =>
  f(v) && c |> List.for_all(node => for_all(f, node));

// Note: pos not in tree behavior => raise Failure
let rec get = (f, Node(v, c)) =>
  fun
  | Value => f(Node(v, c))
  | Children(i, pos) => pos |> get(f, List.nth(c, i));

// Note: pos not in tree behavior => no change
let rec set = (f, Node(v, c)) =>
  fun
  | Value => f(Node(v, c))
  | Children(i, pos) =>
    Node(v, c |> List.mapi((j, t) => j == i ? set(f, t, pos) : t));

let get_value = pos => pos |> get((Node(v, _)) => v);

let set_value = v' => set((Node(_, c)) => Node(v', c));

let get_children = pos => pos |> get((Node(_, c)) => c);

let set_children = c' => set((Node(v, _)) => Node(v, c'));

let get_tree = pos => pos |> get(Fun.id);

let set_tree = t' => set(_ => t');

/* Mutation Utilities */

let mk = v => Node(v, []);

let add = v' => set((Node(v, c)) => Node(v, [mk(v'), ...c]));

let del = pos =>
  pos
  |> set((Node(v, c)) =>
       Node(
         v,
         switch (c) {
         // Note: this case should be handled by the caller
         | [] => failwith("del: empty children")
         | [_, ...cs] => cs
         },
       )
     );

let exists_pos = (t, pos) =>
  try(
    {
      ignore(get(ignore, t, pos));
      true;
    }
  ) {
  | Failure(_) => false
  };
