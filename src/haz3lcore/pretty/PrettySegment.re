/* This file is a placeholder, ideally an algorithm would be implemented here that allows
   efficient calculation of the best way to add linebreaks etc, but that hasn't been implemented yet, so
   none of these functions do anything yet. (Matt) */

type pretty = Segment.t;

let p_concat = (pretty2, pretty1) => pretty1 @ pretty2;
let p_or = (_pretty2, pretty1) => pretty1;
let p_orif = (cond, pretty2, pretty1) => if (cond) {pretty1} else {pretty2};
let p_just = segment => segment;

let p_concat = (pretties: list(pretty)) =>
  List.fold_left(p_concat, [], pretties);

let (let+) = (pretty, f) => f(pretty);
let (and+) = (pretty1, pretty2) => (pretty1, pretty2);

let ( let* ) = (pretty, f) => f(pretty);
let ( and* ) = (pretty1, pretty2) => (pretty1, pretty2);

let all = x => x;

let select: pretty => Segment.t = x => x;
