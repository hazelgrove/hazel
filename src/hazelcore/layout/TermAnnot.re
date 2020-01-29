open Sexplib.Std;
open ViewUtil;

[@deriving sexp]
type term_data = {
  has_cursor: bool,
  shape: TermShape.t,
  family: TermFamily.t,
};

[@deriving sexp]
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
  | Term(term_data);

let mk_Delim = (~caret: option(Side.t)=?, ~path: delim_path, ()): t =>
  Delim({caret, path});
let mk_Op = (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): t =>
  Op({caret, steps});
let mk_Text =
    (~caret: option(int)=?, ~steps: CursorPath.steps, ~length: int, ()): t =>
  Text({caret, steps, length});
let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~family: TermFamily.t, ()): t =>
  Term({has_cursor, shape, family});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
