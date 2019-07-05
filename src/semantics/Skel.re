open SemanticsCommon;
open Sexplib.Std;

[@deriving sexp]
type t('op) =
  | Placeholder(int)
  | BinOp(err_status, 'op, t('op), t('op));

let rec leftmost_op =
  fun
  | Placeholder(_) => None
  | BinOp(_, op, skel1, _) =>
    switch (leftmost_op(skel1)) {
    | Some(op1) => Some(op1)
    | None => Some(op)
    };

let rec rightmost_op =
  fun
  | Placeholder(_) => None
  | BinOp(_, op, _, skel2) =>
    switch (rightmost_op(skel2)) {
    | Some(op1) => Some(op1)
    | None => Some(op)
    };

let rec size = (skel: t(_)): int =>
  switch (skel) {
  | Placeholder(_) => 1
  | BinOp(_, _, skel1, skel2) => size(skel1) + size(skel2)
  };

let rec leftmost_tm_index = (skel: t(_)): int =>
  switch (skel) {
  | Placeholder(n) => n
  | BinOp(_, _, skel1, _) => leftmost_tm_index(skel1)
  };

let rec range = (skel: t(_)): (int, int) =>
  switch (skel) {
  | Placeholder(n) => (n, n)
  | BinOp(_, _, skel1, skel2) =>
    let (a, _) = range(skel1);
    let (_, b) = range(skel2);
    (a, b);
  };

let rec _range_of_skel_rooted_at_op =
        (op_index: op_index, skel: t(_)): (bool, (int, int)) =>
  switch (skel) {
  | Placeholder(k) => (false, (k, k))
  | BinOp(_, _, skel1, skel2) =>
    let (found1, (a1, b1)) = _range_of_skel_rooted_at_op(op_index, skel1);
    let (found2, (a2, b2)) = _range_of_skel_rooted_at_op(op_index, skel2);
    switch (found1, found2) {
    | (true, _) => (true, (a1, b1))
    | (_, true) => (true, (a2, b2))
    | (false, false) => (a2 == op_index, (a1, b2))
    };
  };
let range_of_skel_rooted_at_op =
    (op_index: op_index, skel: t(_)): (int, int) => {
  let (_, (a, b)) = _range_of_skel_rooted_at_op(op_index, skel);
  (a, b);
};
