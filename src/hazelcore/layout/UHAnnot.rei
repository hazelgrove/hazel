[@deriving sexp]
type term_data = {
  shape: TermShape.t,
  sort: TermSort.t,
};

[@deriving sexp]
type token_shape =
  | Keyword
  | Text
  | Op
  | Delim(DelimIndex.t);
[@deriving sexp]
type token_data = {
  shape: token_shape,
  len: int,
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
  | ExternalLineBreak
  | OpenChild(open_child_format)
  | ClosedChild({
      is_inline: bool,
      sort: TermSort.t,
    })
  | Tessera
  | CommentLine
  | Step(int)
  | Term(term_data);

let mk_Token: (~len: int, ~shape: token_shape, unit) => t;

let mk_Term: (~shape: TermShape.t, ~sort: TermSort.t, unit) => t;
