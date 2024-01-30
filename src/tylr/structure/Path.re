open Sexplib.Std;

module Base = {
  // top-down
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cells: list(int),
    token: int,
  };
  let rec compare = (l, r) =>
    switch (l.cells, r.cells) {
    | ([hd_l, ..._], [hd_r, ..._]) when hd_l < hd_r => (-1)
    | ([hd_l, ..._], [hd_r, ..._]) when hd_l > hd_r => 1
    | ([_, ...tl_l], [_, ...tl_r]) =>
      compare({...l, cells: tl_l}, {...r, cells: tl_r})
    | ([], [hd_r, ..._]) => l.token < hd_r ? (-1) : 1
    | ([hd_l, ..._], []) => hd_l <= r.token ? (-1) : 1
    | ([], []) => Int.compare(l.token, r.token)
    };
};
include Base;

exception Invalid;

module Map = Util.MapUtil.Make(Base);

let mk = (~cells=[], ~token=0, ()) => {cells, token};
let empty = mk();

let cons = (n, path) => {...path, cells: [n, ...path.cells]};

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Base.t, int);
  let compare = ((p_l, c_l): t, (p_r, c_r): t) => {
    let c = compare(p_l, p_r);
    c == 0 ? Int.compare(c_l, c_r) : c;
  };
  let mk = (~path=empty, ~char=0, ()) => (path, char);
  let cons = (n, (path, char)) => (cons(n, path), char);
  let uncons = (n, (path, char)) =>
    switch (path.cells) {
    | [m, ...cells] when m == n => Some(({...path, cells}, char))
    | _ => None
    };
};
module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Point(Cursor.t)
    | Select(Cursor.t, Cursor.t);
  let point = c => Point(c);
  let cons = n =>
    fun
    | Point(c) => Point(Cursor.cons(n, c))
    | Select(l, r) => Select(Cursor.cons(n, l), Cursor.cons(n, r));
  let uncons = n =>
    fun
    | Point(c) => Option.map(point, Cursor.uncons(n, c))
    | Select(l, r) => {
        open Util.OptUtil.Syntax;
        let+ l = Cursor.uncons(n, l)
        and+ r = Cursor.uncons(n, r);
        Select(l, r);
      };
};
module Ghosts = {
  include Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Map.t(Mold.t);
  let to_list = bindings;
  let of_list = bindings => of_seq(List.to_seq(bindings));
  let cons = (n, ghosts) =>
    to_list(ghosts)
    |> List.rev_map(((path, mold)) => (cons(n, path), mold))
    |> of_list;
  let uncons = (n, ghosts) =>
    to_list(ghosts)
    |> List.filter_map(((path, mold)) =>
         switch (path.cells) {
         | [m, ...cells] when m == n => Some(({...path, cells}, mold))
         | _ => None
         }
       )
    |> of_list;
  let union = union((_, m, _) => Some(m));
};
module Marks = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    focus: option(Focus.t),
    ghosts: Ghosts.t,
  };
  let mk = (~focus=?, ~ghosts=Ghosts.empty, ()) => {focus, ghosts};
  let empty = mk();
  let cons = (n, {focus, ghosts}) => {
    focus: Option.map(Focus.cons(n), focus),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let uncons = (n, {focus, ghosts}) => {
    focus: Option.bind(focus, Focus.uncons(n)),
    ghosts: Ghosts.uncons(n, ghosts),
  };
  let union = (l: t, r: t) => {
    focus: Cursor.union(l.cursor, r.cursor),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
  let union_all = List.fold_left(union, empty);
};
