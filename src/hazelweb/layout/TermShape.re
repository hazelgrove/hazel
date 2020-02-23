type t =
  | Rule
  | Case({err: ErrStatus.t})
  | Var({
      err: ErrStatus.t,
      verr: VarErrStatus.t,
      show_use: bool,
    })
  | Operand({err: ErrStatus.t})
  | FreeLivelit
  | ApLivelit({
      lln: LivelitName.t,
      llview: Livelits.LivelitView.t,
      steps: CursorPath.steps,
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

let mk_Var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~verr: VarErrStatus.t=NotInVarHole,
      ~show_use=false,
      (),
    ) =>
  Var({err, verr, show_use});

let mk_Operand = (~err: ErrStatus.t=NotInHole, ()) => Operand({err: err});

let mk_ApLivelit =
    (
      ~lln: LivelitName.t,
      ~llview: Livelits.LivelitView.t,
      ~steps: CursorPath.steps,
    ) =>
  ApLivelit({lln, llview, steps});
