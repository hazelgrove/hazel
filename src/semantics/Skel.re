open SemanticsCommon;
open Sexplib.Std;

[@deriving (sexp, show)]
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
