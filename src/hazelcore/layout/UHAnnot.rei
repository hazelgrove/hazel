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

type open_child_format =
  | InlineWithoutBorder
  | InlineWithBorder
  | Multiline;

[@deriving sexp]
type t =
  | HoleLabel({len: int})
  | Token(token_data)
  | UserNewline
  | OpenChild(open_child_format)
  | ClosedChild({
      is_inline: bool,
      sort: TermSort.t,
    })
  | Tessera
  | Step(int)
  | Term(term_data);

let mk_Token:
  (~has_cursor: option(int)=?, ~len: int, ~shape: token_shape, unit) => t;

let mk_Term:
  (~has_cursor: bool=?, ~shape: TermShape.t, ~sort: TermSort.t, unit) => t;
