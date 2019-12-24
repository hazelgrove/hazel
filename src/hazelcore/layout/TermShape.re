open Sexplib.Std;

[@deriving sexp]
type t =
  | Rule
  | Operand({
      err: ErrStatus.t,
      verr: VarErrStatus.t,
    })
  | BinOp({
      op_index: int,
      err: ErrStatus.t,
    })
  | NTuple({
      comma_indices: list(int),
      err: ErrStatus.t,
    })
  | SubBlock({hd_index: int});

let mk_Operand =
    (~err: ErrStatus.t=NotInHole, ~verr: VarErrStatus.t=NotInVarHole, ()) =>
  Operand({err, verr});
