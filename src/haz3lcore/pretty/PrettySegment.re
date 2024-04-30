open Util;

// invariant: always has at least one option
type pretty = list(Segment.t);

let p_concat = (pretty2, pretty1) =>
  List.map(piece1 => List.map(piece2 => piece1 @ piece2, pretty2), pretty1)
  |> List.flatten;
let p_or = (pretty2, pretty1) => pretty1 @ pretty2;
let p_orif = (cond, pretty2, pretty1) => if (cond) {pretty1} else {pretty2};
let p_just = segment => [segment];

let p_concat = (pretties: list(pretty)) =>
  List.fold_left(p_concat, [[]], pretties);

let (let+) = (pretty, f) => List.map(f, pretty);
let (and+) = (pretty1, pretty2) => ListUtil.cross(pretty1, pretty2);

let ( let* ) = (pretty, f) => List.map(f, pretty) |> List.flatten;
let ( and* ) = (pretty1, pretty2) => ListUtil.cross(pretty1, pretty2);
