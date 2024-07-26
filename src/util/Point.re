open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type row = int;
[@deriving (show({with_path: false}), sexp, yojson)]
type col = int;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  row,
  col,
};
let zero = {row: 0, col: 0};

let equals: (t, t) => bool = (p, q) => p.row == q.row && p.col == q.col;

type comparison =
  | Exact
  | Under
  | Over;

let comp = (current, target): comparison =>
  switch () {
  | _ when current == target => Exact
  | _ when current < target => Under
  | _ => Over
  };
let compare = (p1, p2) =>
  switch (comp(p1, p2)) {
  | Exact => 0
  | Under => (-1)
  | Over => 1
  };

let dcomp = (direction: Direction.t, a, b) =>
  switch (direction) {
  | Right => comp(a, b)
  | Left => comp(b, a)
  };
