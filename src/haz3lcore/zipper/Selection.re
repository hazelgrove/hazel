open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type buffer =
  | Parsed
  | Unparsed;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type mode =
  | Normal
  | Buffer(buffer);

/* Anchor caret offset: None = Outer (piece boundary),
 * Some(n) = Inner(n) (n chars into the anchor-side boundary piece).
 * Mirrors ZipperBase.caret but avoids the dependency cycle. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type anchor_caret =
  | Anchor_outer
  | Anchor_inner(int);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  focus: Direction.t,
  content: Segment.t,
  mode,
  anchor_caret,
};

/* NOTE: backpack no longer uses selection focus */
let mk =
    (
      ~mode=Normal,
      ~focus=Direction.Left,
      ~anchor_caret=Anchor_outer,
      content: Segment.t,
    ) => {
  focus,
  content,
  mode,
  anchor_caret,
};

let mk_buffer = buffer =>
  mk(~mode=Buffer(buffer), ~focus=Direction.Left, ~anchor_caret=Anchor_outer);

let is_buffer: t => bool =
  fun
  | {mode: Buffer(_), _} => true
  | _ => false;

let non_empty_parsed_buffer: t => bool =
  fun
  | {mode: Buffer(Parsed), content: [_, ..._], _} => true
  | _ => false;

let buffer_cls: t => string =
  fun
  | {mode: Buffer(Unparsed), _} => "buffer-unparsed"
  | {mode: Buffer(Parsed), _} => "buffer-parsed"
  | _ => "not-buffer";

let selection_ids = (sel: t): list(Id.t) => Segment.ids(sel.content);

let empty = mk(Segment.empty);

let map = (f, sel) => {
  ...sel,
  content: f(sel.content),
};

let toggle_focus = selection => {
  ...selection,
  focus: Util.Direction.toggle(selection.focus),
};

let is_empty = (selection: t) => selection.content == Segment.empty;

let push = (p: Piece.t, {focus, content, mode, anchor_caret}: t): t => {
  let content =
    Segment.reassemble(
      switch (focus) {
      | Left => Segment.cons(p, content)
      | Right => Segment.snoc(content, p)
      },
    );
  {
    focus,
    content,
    mode,
    anchor_caret,
  };
};

let pop = (sel: t): option((Piece.t, t)) => {
  let reset_anchor = (content: Segment.t, sel: t): t =>
    content == []
      ? {...sel, content, anchor_caret: Anchor_outer} : {...sel, content};
  switch (sel.focus, sel.content, ListUtil.split_last_opt(sel.content)) {
  | (_, [], _)
  | (_, _, None) => None
  | (Left, [p, ...content], _) =>
    let (p, rest) = Piece.pop_l(p);
    let content = rest @ content;
    Some((p, reset_anchor(content, sel)));
  | (Right, _, Some((content, p))) =>
    let (rest, p) = Piece.pop_r(p);
    let content = content @ rest;
    Some((p, reset_anchor(content, sel)));
  };
};
