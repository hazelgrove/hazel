[@deriving sexp]
type t = zoperand
and zoperand =
  | CursorTag(CursorPosition.t, UHTag.t);

let valid_cursors: UHTag.t => list(CursorPosition.t) =
  fun
  | Tag(s) => CursorPosition.text_cursors(String.length(s))
  | TagHole(_) => CursorPosition.delim_cursors(1);

let is_valid_cursor = (cursor: CursorPosition.t, tag: UHTag.t): bool =>
  valid_cursors(tag) |> List.mem(cursor);

let is_before = (ztag: t): bool =>
  switch (ztag) {
  | CursorTag(cursor, Tag(_)) => cursor == OnText(0)
  | CursorTag(cursor, TagHole(_)) => cursor == OnDelim(0, Before)
  };

let is_after = (ztag: t): bool =>
  switch (ztag) {
  | CursorTag(cursor, Tag(t)) => cursor == OnText(String.length(t))
  | CursorTag(cursor, TagHole(_)) => cursor == OnDelim(1, After)
  };

let place_before = (tag: UHTag.t): t =>
  switch (tag) {
  | TagHole(_) => CursorTag(OnDelim(0, Before), tag)
  | Tag(_) => CursorTag(OnText(0), tag)
  };

let place_after = (tag: UHTag.t): t =>
  switch (tag) {
  | TagHole(_) => CursorTag(OnDelim(0, After), tag)
  | Tag(t) => CursorTag(OnText(String.length(t)), tag)
  };

let place_cursor = (cursor: CursorPosition.t, tag: UHTag.t): option(t) =>
  is_valid_cursor(cursor, tag) ? Some(CursorTag(cursor, tag)) : None;

let erase = (CursorTag(_, tag): t): UHTag.t => tag;

let new_TagHole = (u_gen: MetaVarGen.t): (zoperand, MetaVarGen.t) => {
  let (hole, u_gen) = UHTag.new_TagHole(u_gen);
  (place_before(hole), u_gen);
};

let move_cursor_left: t => option(t) =
  fun
  | CursorTag(OnText(j), tag) => Some(CursorTag(OnText(j - 1), tag))
  | CursorTag(OnDelim(_0, After), tag) =>
    Some(CursorTag(OnDelim(0, Before), tag))
  | CursorTag(OnDelim(_, Before) | OnOp(_), _) => None;

let move_cursor_right: t => option(t) =
  fun
  | CursorTag(OnText(j), tag) => Some(CursorTag(OnText(j + 1), tag))
  | CursorTag(OnDelim(_0, Before), tag) =>
    Some(CursorTag(OnDelim(0, After), tag))
  | CursorTag(OnDelim(_, After) | OnOp(_), _) => None;
