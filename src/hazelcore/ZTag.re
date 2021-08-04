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
  | CursorTag(cursor, Tag(_) | TagHole(_)) => cursor == OnDelim(0, Before)
  };

let is_after = (ztag: t): bool =>
  switch (ztag) {
  | CursorTag(cursor, Tag(_) | TagHole(_)) => cursor == OnDelim(1, After)
  };

let erase = (CursorTag(_, tag): t): UHTag.t => tag;

let move_cursor_left = (CursorTag(pos, tag): t): option(t) =>
  switch (pos) {
  | OnText(0) => None
  | OnText(n) => Some(CursorTag(OnText(n - 1), tag))
  | OnDelim(_0, After) =>
    switch (tag) {
    | TagHole(_) => Some(CursorTag(OnDelim(0, Before), tag))
    | Tag(_) => None
    }
  | OnDelim(_, Before)
  | OnOp(_) => None
  };

let move_cursor_right = (CursorTag(pos, tag): t): option(t) =>
  switch (pos, tag) {
  | (OnText(n), Tag(id)) =>
    if (n < String.length(id) - 1) {
      Some(CursorTag(OnText(n + 1), tag));
    } else {
      None;
    }
  | (OnText(_), TagHole(_)) => None
  | (OnDelim(_0, Before), TagHole(_)) =>
    Some(CursorTag(OnDelim(0, After), tag))
  | (OnDelim(_, After), TagHole(_))
  | (OnDelim(_, Before | After), Tag(_))
  | (OnOp(_), _) => None
  };

let place_before = (tag: UHTag.t): t =>
  switch (tag) {
  | TagHole(_) => CursorTag(OnDelim(0, Before), tag)
  | Tag(_) => CursorTag(OnText(0), tag)
  };

let place_after = (tag: UHTag.t): t =>
  switch (tag) {
  | TagHole(_) => CursorTag(OnDelim(0, After), tag)
  | Tag(id) => CursorTag(OnText(String.length(id) - 1), tag)
  };

let new_TagHole = (u_gen: MetaVarGen.t): (zoperand, MetaVarGen.t) => {
  let (hole, u_gen) = UHTag.new_TagHole(u_gen);
  (place_before(hole), u_gen);
};

let is_after_zoperand = (CursorTag(cursor, _): zoperand): bool =>
  cursor == OnDelim(0, After);
