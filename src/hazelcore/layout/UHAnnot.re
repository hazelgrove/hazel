open Sexplib.Std;

[@deriving sexp]
type term_data = {
  has_cursor: bool,
  shape: TermShape.t,
  sort: TermSort.t,
};

[@deriving sexp]
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
  | Term(term_data);

let mk_Text = (~cursor: option(int)=?, ()): t => Text({cursor: cursor});
let mk_EmptyLine = (~has_cursor=false, ()) =>
  EmptyLine({has_cursor: has_cursor});
let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
