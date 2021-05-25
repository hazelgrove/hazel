[@deriving sexp]
type t = zoperand
and zoperand =
  | CursorTag(CursorPosition.t, UHTag.t);

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
