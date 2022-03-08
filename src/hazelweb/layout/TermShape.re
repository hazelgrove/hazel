open Sexplib.Std;

[@deriving sexp]
type t =
  | Rule
  | Match
  | Operand
  | BinOp({op_index: int})
  | NTuple({comma_indices: list(int)})
  | SumBody({plus_indices: list(int)})
  | SubBlock({hd_index: int});
