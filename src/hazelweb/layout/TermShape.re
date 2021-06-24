open Sexplib.Std;

[@deriving sexp]
type t =
  | Rule
  | Case
  | Operand
  | FreeLivelit
  | ApLivelit
  | LivelitExpression({
      hd_index: int,
      view_shape: LivelitShape.t,
    })
  | BinOp({op_index: int})
  | NTuple({comma_indices: list(int)})
  | Line
  | SubBlock({hd_index: int});
