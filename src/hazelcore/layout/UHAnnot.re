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
  | Text({
      len: int,
      has_cursor: option(int),
    })
  | Delim
  | Op
  | SpaceOp
  | UserNewline
  | OpenChild({is_inline: bool})
  | ClosedChild({is_inline: bool})
  | DelimGroup
  // TODO remove param, use CursorPosition
  | EmptyLine
  | LetLine
  | Step(int)
  | Term(term_data);

let mk_Text = (~has_cursor: option(int)=?, ~len: int, ()): t =>
  Text({has_cursor, len});
let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
