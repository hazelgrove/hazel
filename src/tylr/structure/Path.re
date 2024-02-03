open Sexplib.Std;
open Util;

module Int_ppx = {
  include Int;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};

module Cell = {
  module Idx = Int_ppx;
  // absolute path from root cell to a subcell
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Idx.t);
  let compare = List.compare(Idx.compare);
  let here = [];
  let cons = List.cons;
  let uncons =
    fun
    | [] => None
    | [n, ...ns] => Some((n, ns));
  let peel = n =>
    fun
    | [m, ...ms] when m == n => Some(ms)
    | _ => None;
};

module Token = {
  module Idx = Int_ppx;
  module Base = {
    // absolute path from root cell to a token within
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (Cell.t, Idx.t);
    let compare = ((c_l, t_l), (c_r, t_r)) => {
      let c = Cell.compare(c_l, c_r);
      c == 0 ? Idx.compare(t_l, t_r) : 0;
    };
  };
  include Base;
  module Map = MapUtil.Make(Base);
  let cons = n => PairUtil.map_fst(Cell.cons(n));
  let uncons = ((cell, t)) =>
    Cell.uncons(cell) |> Option.map(((n, cell)) => (n, (cell, t)));
  let peel = (n, t: t) =>
    switch (uncons(t)) {
    | Some((m, t)) when m == n => Some(t)
    | _ => None
    };
};

module Point = {
  module Idx = {
    // relative path from a given cell to zero-width point at
    // its root, either within a root token or one of the ends.
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | End(Dir.t)
      | Tok(Token.Idx.t, int);
    let compare = (l, r) =>
      switch (l, r) {
      | (End(l), End(r)) => l == r ? 0 : Dir.pick(l, ((-1), 1))
      | (End(d), _) => Dir.pick(d, ((-1), 1))
      | (_, End(d)) => Dir.pick(d, (1, (-1)))
      | (Tok(i, j), Tok(k, l)) =>
        let c = Token.Idx.compare(i, k);
        c == 0 ? Int.compare(j, l) : c;
      };
  };
  // absolute path from root cell to a zero-width point within
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Cell.t, Idx.t);
  let compare = ((c_l, p_l), (c_r, p_r)) => {
    let c = Cell.compare(c_l, c_r);
    c == 0 ? Idx.compare(p_l, p_r) : c;
  };
  let min = (l, r) => compare(l, r) <= 0 ? l : r;
  let here = (Cell.here, Idx.End(L));
  let cons = n => PairUtil.map_fst(Cell.cons(n));
  let uncons = ((cell, p)) =>
    Cell.uncons(cell) |> Option.map(((n, cell)) => (n, (cell, p)));
  let peel = (n, p: t) =>
    switch (uncons(p)) {
    | Some((m, p)) when m == n => Some(p)
    | _ => None
    };
};

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Point.t, Point.t);
  let origin = fst;
  let compare = (l, r) => Point.compare(origin(l), origin(r));
  let point = p => (p, p);
  let is_point = ((l, r)) => l == r;
  let here = point(Point.here);
  let cons = (n, (l, r): t) => Point.(cons(n, l), cons(n, r));
  let uncons = ((l, r): t) => {
    open OptUtil.Syntax;
    let* (m, l) = Point.uncons(l);
    let* (n, r) = Point.uncons(r);
    m == n ? Some((m, (l, r))) : None;
  };
  let peel = (n, (l, r)) =>
    switch (Point.peel(n, l), Point.peel(n, r)) {
    | (None, None) => None
    | (None, Some(r)) => Some(point(r))
    | (Some(l), None) => Some(point(l))
    | (Some(l), Some(r)) => Some((l, r))
    };
  let union = ((l, _), (_, r)) => (l, r);
};

// ----------------------------------------------------------------

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = option((Dir.t, Range.t));
  let cons = n => Option.map(PairUtil.map_snd(Range.cons(n)));
  open OptUtil.Syntax;
  let uncons = c => {
    let* (d, r) = c;
    let+ (n, r) = Range.uncons(r);
    (n, (d, r));
  };
  let peel = (n, c) => {
    let* (d, r) = c;
    let+ r = Range.peel(n, r);
    (d, r);
  };
  let union = (l, r) =>
    switch (l, r) {
    | (None, None) => None
    | (None, Some(r)) => Some(r)
    | (Some(l), None) => Some(l)
    | (Some((d, l)), Some((_, r))) => Some((d, Range.union(l, r)))
    };
};

module Ghosts = {
  include Token.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Token.Map.t(Mold.t);
  let to_list = bindings;
  let of_list = bindings => of_seq(List.to_seq(bindings));
  let cons = (n, ghosts) =>
    to_list(ghosts)
    |> List.rev_map(((tok, mold)) => (Token.cons(n, tok), mold))
    |> of_list;
  let peel = (n, ghosts) =>
    to_list(ghosts)
    |> List.filter_map(((tok, mold)) =>
         switch (tok) {
         | ([m, ...ms], t) when m == n => Some(((ms, t), mold))
         | _ => None
         }
       )
    |> of_list;
  let union = union((_, m, _) => Some(m));
};
module Marks = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cursor: Cursor.t,
    ghosts: Ghosts.t,
  };
  let mk = (~cursor=?, ~ghosts=Ghosts.empty, ()) => {cursor, ghosts};
  let empty = mk();
  let cursor = mk(~cursor=Range.here, ());
  let cons = (n, {cursor, ghosts}) => {
    cursor: Cursor.cons(n, cursor),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let peel = (n, {cursor, ghosts}) => {
    cursor: Cursor.peel(n, cursor),
    ghosts: Ghosts.peel(n, ghosts),
  };
  let union = (l: t, r: t) => {
    cursor: Cursor.union(l.cursor, r.cursor),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
  let union_all = List.fold_left(union, empty);
};

exception Invalid;
