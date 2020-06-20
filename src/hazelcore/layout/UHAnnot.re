open Sexplib.Std;

[@deriving sexp]
type term_data = {
  has_cursor: bool,
  shape: TermShape.t,
  sort: TermSort.t,
};

[@deriving sexp]
type token_shape =
  | Text({start_index: CharIndex.t})
  | Op
  | Delim(DelimIndex.t);

[@deriving sexp]
type t =
  | Indent
  | Padding
  | HoleLabel({len: int})
  | Token({
      shape: token_shape,
      len: int,
      has_cursor: option(int),
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
  | ValidSeq({
      start_line: int,
      start_seq: int,
      len: int,
      has_cursor: option(int),
    })
  | InvalidSeq({
      start_line: int,
      start_seq: int,
      len: int,
      has_cursor: option(int),
    });

let mk_Token = (~has_cursor=None, ~len: int, ~shape: token_shape, ()) =>
  Token({has_cursor, len, shape});
let mk_ValidSeq =
    (~has_cursor=None, ~len: int, ~start_seq: int, ~start_line: int, ()) =>
  ValidSeq({has_cursor, len, start_seq, start_line});
let mk_InvalidSeq =
    (~has_cursor=None, ~len: int, ~start_seq: int, ~start_line: int, ()) =>
  InvalidSeq({has_cursor, len, start_seq, start_line});
let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
