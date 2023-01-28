open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | S(Space.t)
  | G(Grout.t)
  | T(Tile.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type s = list(t);

let is_porous =
  fun
  | S(_)
  | G(_) => true
  | T(_) => false;

let is_space =
  fun
  | G(_)
  | T(_) => None
  | S(s) => Some(s);

let token =
  fun
  | G(g) => g.prefix
  | T(t) => t.token
  | S(s) => Space.to_string(s);

let id =
  fun
  | G(g) => g.id
  | T(t) => t.id
  | S(s) => s.id;

let s_of_space: Space.s => s = List.map(s => S(s));

// postcond: output is nonempty
let s_of_piece = ({shape, space: (l, r)}: Piece.t) => {
  let of_shape =
    switch (shape) {
    | T(t) => T(t)
    | G(g) => G(g)
    };
  s_of_space(l) @ [of_shape, ...s_of_space(r)];
};

let uncons_char = (lx: t): option((t, t)) =>
  switch (lx) {
  | S(_) => None
  | G(g) =>
    Grout.uncons_char(g) |> Option.map(((hd, tl)) => (G(hd), G(tl)))
  | T(t) =>
    Tile.uncons_char(t) |> Option.map(((hd, tl)) => (T(hd), T(tl)))
  };
let uncons_char_s = (ls: s): option((t, s)) => {
  open OptUtil.Syntax;
  let+ (tl, hd) = ListUtil.split_last_opt(ls);
  switch (uncons_char(hd)) {
  | None => (hd, tl)
  | Some((c, hd)) => (c, [hd, ...tl])
  };
};

let unsnoc_char = (lx: t): option((t, t)) =>
  switch (lx) {
  | S(_) => None
  | G(g) =>
    Grout.unsnoc_char(g) |> Option.map(((tl, hd)) => (G(tl), G(hd)))
  | T(t) =>
    Tile.unsnoc_char(t) |> Option.map(((tl, hd)) => (T(tl), T(hd)))
  };
let unsnoc_char_s = (ls: s): option((s, t)) => {
  open OptUtil.Syntax;
  let+ (tl, hd) = ListUtil.split_last_opt(ls);
  switch (unsnoc_char(hd)) {
  | None => (tl, hd)
  | Some((hd, c)) => (tl @ [hd], c)
  };
};
