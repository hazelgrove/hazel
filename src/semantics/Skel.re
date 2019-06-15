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
