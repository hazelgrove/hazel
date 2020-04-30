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

type uhid = Uhid(int)
let max_uhid : ref(int) = ref 0;
let mk_uhid = () -> { let id = max_uid^ + 1; max_uhid := id; Uhid(id) }

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
  | Id(uhid);

let mk_Token = (~has_cursor=None, ~len: int, ~shape: token_shape, ()) =>
  Token({has_cursor, len, shape});

let mk_Term =
    (~has_cursor=false, ~shape: TermShape.t, ~sort: TermSort.t, ()): t =>
  Term({has_cursor, shape, sort});
let mk_OpenChild = (~is_inline: bool, ()) =>
  OpenChild({is_inline: is_inline});
let mk_ClosedChild = (~is_inline: bool, ()) =>
  ClosedChild({is_inline: is_inline});

let wrap_uhid = (doc: Doc.t(t)): (uhid, Doc.t(t)) = {
  let id = mk_uhid();
  (id, Doc.Annot(Id(id), doc))
}