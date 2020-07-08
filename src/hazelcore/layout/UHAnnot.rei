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
  | Term(term_data);

let mk_Token:
  (~has_cursor: option(int)=?, ~len: int, ~shape: token_shape, unit) => t;

let mk_Term:
  (~has_cursor: bool=?, ~shape: TermShape.t, ~sort: TermSort.t, unit) => t;

let mk_OpenChild: (~is_inline: bool, unit) => t;

let mk_ClosedChild: (~is_inline: bool, unit) => t;
