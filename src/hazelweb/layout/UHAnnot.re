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
      is_inline: bool,
      is_enclosed: bool,
    })
  | ClosedChild({is_inline: bool})
  | Tessera
  | EmptyLine
  | LetLine
  | AbbrevLine
  | Step(int)
  | Term(term_data)
  | LivelitView({
      llu: MetaVar.t,
      base_llname: LivelitName.t,
      llname: LivelitName.t,
      shape: Livelits.LivelitView.shape,
      model: SerializedModel.t,
      hd_step: int,
    })
  | ValidSeq
  | InvalidSeq
  | String;

let mk_Token = (~has_cursor=None, ~len: int, ~shape: token_shape, ()) =>
  Token({has_cursor, len, shape});
let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_enclosed=false, ~is_inline: bool, ()) =>
  OpenChild({is_enclosed, is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});
