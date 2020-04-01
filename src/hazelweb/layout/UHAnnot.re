module Doc = Pretty.Doc;

type t =
  | Indent
  | Padding
  | HoleLabel({len: int})
  | CursorPosition({
      has_cursor: bool,
      cursor: CursorPosition.t,
    })
  | Text({cursor: option(int)})
  | Delim
  | Op
  | SpaceOp
  | UserNewline
  | OpenChild({is_inline: bool})
  | ClosedChild({is_inline: bool})
  | DelimGroup
  | EmptyLine({has_cursor: bool})
  | LetLine
  | Step(int)
  | Term(term_data)
  | LivelitView({
      llview: Livelits.LivelitView.t,
      splice_docs: NatMap.t(Doc.t(t)),
    })
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
  | ApLivelit
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

let mk_Text = (~cursor: option(int)=?, ()): t => Text({cursor: cursor});
let mk_EmptyLine = (~has_cursor=false, ()) =>
  EmptyLine({has_cursor: has_cursor});
let mk_Term =
    (~has_cursor=false, ~shape: term_shape, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
