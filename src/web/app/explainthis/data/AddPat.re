open Haz3lcore;
open Example;
open ExplainThisForm;

let pat_left = pat("p1");
let pat_right = pat("p2");
let add_pat = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: AddPat,
  syntactic_form: [pat_left, space(), plus_pat(), space(), pat_right],
  colorings: [
    (Piece.id(pat_left), left_id),
    (Piece.id(pat_right), right_id),
  ],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only whole numbers that are the sum of the [*left*](%s) and [*right*](%s) patterns match this *addition pattern*. One side must be a number k, and the other side is matched against the number minus k. A natural number less than k doesn't match.",
      Id.to_string(left_id),
      Id.to_string(right_id),
    ),
  examples: [],
};

let add = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(add_pat(~left_id, ~right_id));
