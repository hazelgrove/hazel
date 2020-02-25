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
  | Text({
      length: int,
      caret: option(int),
    })
  | Delim({
      index: DelimIndex.t,
      caret: option(Side.t),
    })
  | Op({caret: option(Side.t)})
  | SpaceOp
  | UserNewline
  | OpenChild({is_inline: bool})
  | ClosedChild({is_inline: bool})
  | DelimGroup
  | EmptyLine({has_caret: bool})
  | LetLine
  | Step(int)
  | Term(term_data);

let mk_Delim = (~caret: option(Side.t)=?, ~index: DelimIndex.t, ()): t =>
  Delim({caret, index});
let mk_Op = (~caret: option(Side.t)=?, ()): t => Op({caret: caret});
let mk_Text = (~caret: option(int)=?, ~length: int, ()): t =>
  Text({caret, length});
let mk_EmptyLine = (~has_caret=false, ()) =>
  EmptyLine({has_caret: has_caret});
let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
