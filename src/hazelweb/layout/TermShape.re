open Sexplib.Std;

[@deriving (sexp, show)]
type t =
  | Rule
  | Case
  | Operand
  | BinOp({op_index: int})
  | NTuple({comma_indices: list(int)})
  | SubBlock({hd_index: int});
