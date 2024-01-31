open Sexplib.Std;
open Util;

module Cell = {
  // top-down path from root cell to a subcell
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | C(list(int));
  let compare = (C(ls), C(rs)) => List.compare(Int.compare, ls, rs);
  let here = C([]);
  let cons = (n, C(ns)) => C([n, ...ns]);
  let uncons =
    fun
    | C([]) => None
    | C([n, ...ns]) => Some((n, C(ns)));
  let peel = (n, c: t) =>
    switch (uncons(c)) {
    | Some((m, c)) when m == n => Some(c)
    | _ => None
    };
};
module Token = {
  module Base = {
    // path to a token within root cell
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | T(int, Cell.t);
    let compare = (T(t_l, c_l), T(t_r, c_r)) => {
      let c = Cell.compare(c_l, c_r);
      c == 0 ? Int.compare(t_l, t_r) : c;
    };
  };
  include Base;
  module Map = MapUtil.Make(Base);
  let cons = (n, T(tok, cell)) => T(tok, Cell.cons(n, cell));
  let uncons = (T(tok, cell)) =>
    Cell.uncons(cell) |> Option.map(((n, cell)) => (n, T(tok, cell)));
  let peel = (n, t: t) =>
    switch (uncons(t)) {
    | Some((m, t)) when m == n => Some(t)
    | _ => None
    };
};
module Point = {
  // path to a zero-width point within root cell.
  // int index indicates point between characters within given token path.
  // we wish to consider empty cells as containing a single point, which
  // has the cell path [], while any token and point indices are considered
  // valid (ie they are ignored when a cell path arrives at an empty cell).
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | P(int, Token.t);
  let compare = (P(i_l, t_l): t, P(i_r, t_r): t) => {
    let c = Token.compare(t_l, t_r);
    c == 0 ? Int.compare(i_l, i_r) : c;
  };
  let min = (l, r) => compare(l, r) <= 0 ? l : r;
  let here = P(0, T(0, Cell.here));
  let cons = (n, P(i, tok)) => P(i, Token.cons(n, tok));
  let uncons = (P(i, tok)) =>
    Token.uncons(tok) |> Option.map(((n, tok)) => (n, P(i, tok)));
  let peel = (n, p: t) =>
    switch (uncons(p)) {
    | Some((m, p)) when m == n => Some(p)
    | _ => None
    };
};

module Cursor = {
  // path to user selection range, possibly empty
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Point.t, Point.t);
  let origin = fst;
  let compare = (l, r) => Point.compare(origin(l), origin(r));
  let point = p => (p, p);
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

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = option(Cursor.t);
  let cons = n => Option.map(Cursor.cons(n));
  let uncons = foc => Option.bind(foc, Cursor.uncons);
  let peel = (n, foc) => Option.bind(foc, Cursor.peel(n));
  let union = (l, r) =>
    switch (l, r) {
    | (None, None) => None
    | (None, Some(r)) => Some(r)
    | (Some(l), None) => Some(l)
    | (Some(l), Some(r)) => Some(Cursor.union(l, r))
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
         | Token.T(idx, C([m, ...ms])) when m == n =>
           Some((Token.T(idx, C(ms)), mold))
         | _ => None
         }
       )
    |> of_list;
  let union = union((_, m, _) => Some(m));
};
module Marks = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    focus: Focus.t,
    ghosts: Ghosts.t,
  };
  let mk = (~focus=?, ~ghosts=Ghosts.empty, ()) => {focus, ghosts};
  let empty = mk();
  let cursor = mk(~focus=Cursor.here, ());
  let cons = (n, {focus, ghosts}) => {
    focus: Focus.cons(n, focus),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let peel = (n, {focus, ghosts}) => {
    focus: Focus.peel(n, focus),
    ghosts: Ghosts.peel(n, ghosts),
  };
  let union = (l: t, r: t) => {
    focus: Focus.union(l.focus, r.focus),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
  let union_all = List.fold_left(union, empty);
};

exception Invalid;
