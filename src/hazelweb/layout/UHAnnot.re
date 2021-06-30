open Sexplib.Std;

[@deriving sexp]
type term_data = {
  shape: TermShape.t,
  sort: TermSort.t,
};

[@deriving sexp]
type token_shape =
  | Text
  | Op
  | Delim(DelimIndex.t);

[@deriving sexp]
type open_child_format =
  | InlineWithoutBorder
  | InlineWithBorder
  | Multiline;

/*
 CLASS LIST 1: on everything:
 ann-text : exact text content as class
 ann-parent: parent syntatic form name
 ann-sort-exp
 ann-sort-pat
 ann-sort-typ

  */

[@deriving sexp]
type delimiter =
  | Keyword
  | Structural;

let string_of_delimiter: delimiter => string =
  fun
  | Keyword => "keyword"
  | Structural => "structural";

[@deriving sexp]
type signifier =
  | Operator
  | Variable
  | Literal
  | Invalid
  | Comment
  | Hole;

let string_of_signifier: signifier => string =
  fun
  | Variable => "variable"
  | Literal => "literal"
  | Operator => "operator"
  | Invalid => "invalid"
  | Comment => "comment"
  | Hole => "hole";

[@deriving sexp]
type ann_class =
  | Delimiter(delimiter)
  | Signifier(signifier);

let string_of_ann_class =
  fun
  | Delimiter(d) => "delimiter-" ++ string_of_delimiter(d)
  | Signifier(s) => "signifier-" ++ string_of_signifier(s);

[@deriving sexp]
type token_data = {
  shape: token_shape,
  len: int,
  ann_class,
  sort: TermSort.t,
};

[@deriving sexp]
type t =
  | HoleLabel({len: int})
  | Token(token_data)
  | UserNewline
  | OpenChild(open_child_format)
  | ClosedChild({
      // TODO consider whether necessary
      is_inline: bool,
      sort: TermSort.t,
    })
  | Tessera
  | CommentLine
  | Step(int)
  | Term(term_data);

let mk_Token =
    (
      ~sort: TermSort.t,
      ~len: int,
      ~ann_class: ann_class,
      ~shape: token_shape,
      (),
    ) =>
  Token({len, shape, ann_class, sort});

let mk_Term = (~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({shape, sort});
