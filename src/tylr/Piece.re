open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | T(Tile.t)
  | G(Grout.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  shape,
  space: (Space.t, Space.t),
};

let mk = (~l=Space.empty, ~r=Space.empty, shape) => {shape, space: (l, r)};

let pad = (~l=Space.empty, ~r=Space.empty, {shape, space: (l', r')}) => {
  shape,
  space: (l @ l', r' @ r),
};

let is_porous = _ => failwith("todo");

let space = ({space: (l, r), _}) => l @ r;

let pop_space_l = ({shape, space: (l, r)}: t) => (
  l,
  {shape, space: (Space.empty, r)},
);
let pop_space_r = ({shape, space: (l, r)}: t) => (
  {shape, space: (l, Space.empty)},
  r,
);

type fp =
  | Fill(Dir.t)
  | Pass(Dir.t);

let fills_or_passes = (_, _): option(fp) =>
  failwith("todo fills_or_passes");

let mold = p =>
  switch (p.shape) {
  | T(t) => t.mold
  | G(g) => Some(g.mold)
  };
let sort = p => Option.map(Mold.sort_, mold(p));
let prec = p => Option.map(Mold.prec_, mold(p));

let expected_sort = (d, p) =>
  mold(p) |> OptUtil.and_then(Mold.expected_sort(d));

let length = (~with_space as _=false, p: t) => {
  switch (p.shape) {
  | T(t) => Tile.length(t)
  | G(g) => Grout.length(g)
  };
};

let shape_to_lexeme: shape => Lexeme.t =
  fun
  | T(t) => T(t)
  | G(g) => G(g);

let to_lexemes = ({shape, space: (l, r)}: t) =>
  Lexeme.[S(l), shape_to_lexeme(shape), S(r)];

type rel =
  | Fills(t)
  | Passes(Dir.t)
  | Prec(Cmp.t);

let cmp = (_, _): Cmp.Result.t(unit, Sort.t, Sort.t, Sort.t) =>
  failwith("todo Piece.cmp");
let rel = (_, _): rel => failwith("todo Piece.rel");
