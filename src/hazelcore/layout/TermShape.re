open Sexplib.Std;

[@deriving sexp]
type t =
  | Rule
  | Invalid
  | Case({err: CaseErrStatus.t})
  | Var({
      err: ErrStatus.t,
      verr: VarErrStatus.t,
      vwarn: VarWarnStatus.t,
      show_def: bool,
      show_use: bool,
    })
  | Operand({err: ErrStatus.t})
  | BinOp({
      op_index: int,
      err: ErrStatus.t,
    })
  | NTuple({
      comma_indices: list(int),
      err: ErrStatus.t,
    })
  | SubBlock({hd_index: int});

let mk_Var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~verr: VarErrStatus.t=NotInVarHole,
      ~vwarn: VarWarnStatus.t=NoWarning,
      ~show_use=false,
      ~show_def=false,
      (),
    ) =>
  Var({err, verr, vwarn, show_def, show_use});

let mk_Operand = (~err: ErrStatus.t=NotInHole, ()) => Operand({err: err});
