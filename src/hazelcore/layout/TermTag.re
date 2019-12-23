open Sexplib.Std;
open ViewUtil;

[@deriving sexp]
type term_shape =
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

[@deriving sexp]
type t =
  | Indent
  | Padding
  | HoleLabel({u: MetaVar.t})
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
  | OpenChild({is_inline: bool})
  | ClosedChild({is_inline: bool})
  | DelimGroup
  | Step(int)
  | Term({
      has_cursor: bool,
      shape: term_shape,
    });

let mk_Delim = (~caret: option(Side.t)=?, ~path: delim_path, ()): t =>
  Delim({caret, path});
let mk_Op = (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): t =>
  Op({caret, steps});
let mk_Text =
    (~caret: option(int)=?, ~steps: CursorPath.steps, ~length: int, ()): t =>
  Text({caret, steps, length});
let mk_Term = (~has_cursor=false, ~shape: term_shape, ()): t =>
  Term({has_cursor, shape});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
