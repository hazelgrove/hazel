[@deriving sexp]
type t =
  | Rule
  | Case
  | Operand
  | FreeLivelit
  | ApLivelit
  | LivelitExpression({hd_index: int})
  | BinOp({op_index: int})
  | NTuple({comma_indices: list(int)})
  | SubBlock({hd_index: int});
