open Sexplib.Std;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // top-down
    slots: list(int),
    piece: int,
  };
  let compare = (_, _) => failwith("todo");
};
include Base;

exception Invalid;

module Map = Map.Make(Base);

let cons = (n, path) => {...path, slots: [n, ...path.slots]};

module Cursor = {
  type offset = int;
  type t = option((Base.t, offset));
  let cons = n => Option.map(((path, offset)) => (cons(n, path), offset));
  let uncons = (n, c) =>
    Option.bind(c, ((path, offset)) =>
      switch (path.slots) {
      | [m, ...slots] when m == n => Some(({...path, offset}, offset))
      | _ => None
      }
    );
};
module Ghosts = {
  include Map;
  type t = Map.t(Mold.t);
  let cons = (n, ghosts) =>
    to_list(ghosts)
    |> List.rev_map(((path, mold)) => (cons(n, path), mold))
    |> of_list;
  let uncons = (n, ghosts) =>
    to_list(ghosts)
    |> List.filter_map(((path, mold)) =>
         switch (path.slots) {
         | [m, ...slots] when m == n => Some(({...path, slots}, mold))
         | _ => None
         }
       )
    |> of_list;
};
module Marks = {
  type t = {
    cursor: Cursor.t,
    ghosts: Ghosts.t,
  };
  let cons = (n, {cursor, ghosts}) => {
    cursor: Cursor.cons(n, cursor),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let uncons = (n, {cursor, ghosts}) => {
    cursor: Cursor.uncons(n, cursor),
    ghosts: Ghosts.uncons(n, ghosts),
  };
  let union = (l: t, r: t) => {
    cursor: Cursor.union(l.cursor, r.cursor),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
};

module Marked = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = (Marks.t, 'a);
};
