open Sexplib.Std;

[@deriving sexp]
type t =
  | Rule
  | Case({err: ErrStatus.t})
  | PatVar({
      err: ErrStatus.t,
      verr: PatVarErrStatus.t,
      show_use: bool,
    })
  | ExpVar({
      err: ErrStatus.t,
      verr: ExpVarErrStatus.t,
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

let mk_ExpVar =
    (
      ~err: ErrStatus.t=NotInHole,
      ~verr: ExpVarErrStatus.t=NotInVarHole,
      ~show_use=false,
      (),
    ) =>
  ExpVar({err, verr, show_use});

let mk_PatVar =
    (
      ~err: ErrStatus.t=NotInHole,
      ~verr: PatVarErrStatus.t=NotInVarHole,
      ~show_use=false,
      (),
    ) =>
  PatVar({err, verr, show_use});

let mk_Operand = (~err: ErrStatus.t=NotInHole, ()) => Operand({err: err});
