open ViewUtil;
module Doc = Pretty.Doc;

type t =
  | Indent
  | Padding
  | HoleLabel({len: int})
  | Text({
      steps: CursorPath.steps,
      length: int,
      caret: option(int),
    })
  | Delim({
      path: delim_path,
      caret: option(Side.t),
    })
  | Op({
      steps: CursorPath.steps,
      caret: option(Side.t),
    })
  | SpaceOp
  | UserNewline
  | OpenChild({is_inline: bool})
  | ClosedChild({is_inline: bool})
  | DelimGroup
  | EmptyLine
  | LetLine
  | Step(int)
  | Term(term_data)
and term_data = {
  has_cursor: bool,
  shape: term_shape,
  sort: TermSort.t,
}
and term_shape =
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
      splice_docs: NatMap.t(Doc.t(t)),
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
      ~splice_docs: NatMap.t(Doc.t(t)),
      ~steps: CursorPath.steps,
    ) =>
  ApLivelit({lln, llview, splice_docs, steps});

let mk_Delim = (~caret: option(Side.t)=?, ~path: delim_path, ()): t =>
  Delim({caret, path});
let mk_Op = (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): t =>
  Op({caret, steps});
let mk_Text =
    (~caret: option(int)=?, ~steps: CursorPath.steps, ~length: int, ()): t =>
  Text({caret, steps, length});
let mk_Term =
    (~has_cursor=false, ~shape: term_shape, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
