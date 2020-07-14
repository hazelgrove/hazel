open Sexplib.Std;

[@deriving sexp]
type term_data = {
  has_cursor: bool,
  shape: TermShape.t,
  sort: TermSort.t,
};

[@deriving sexp]
type token_shape =
  | Text
  | Op
  | Delim(DelimIndex.t);
[@deriving sexp]
type token_data = {
  shape: token_shape,
  len: int,
  has_cursor: option(int),
};

[@deriving sexp]
type t =
  | Indent
  | Padding
  | HoleLabel({len: int})
  | Token(token_data)
  | SpaceOp
  | UserNewline
  | OpenChild({
      // TODO consider whether necessary
      is_inline: bool,
      // TODO consider whether necessary
      is_enclosed: bool,
    })
  | ClosedChild({
      // TODO consider whether necessary
      is_inline: bool,
      sort: TermSort.t,
    })
  | Tessera
  | EmptyLine
  | LetLine
  | Step(int)
  | Term(term_data);

let mk_Token = (~has_cursor=None, ~len: int, ~shape: token_shape, ()) =>
  Token({has_cursor, len, shape});

let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_enclosed=true, ~is_inline: bool, ()) =>
  OpenChild({is_inline, is_enclosed});
