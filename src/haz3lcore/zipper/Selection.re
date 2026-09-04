open Util_web;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type buffer =
  | Parsed
  | Unparsed;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type mode =
  | Normal
  | Buffer(buffer);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type anchor_caret = CaretBase.t;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  focus: Direction.t,
  content: Segment.t,
  mode,
  /* The position offset of the anchor caret during range selections */
  anchor_caret,
  /* Smart-rounded selection: when true, the anchor is rendered at the
   * outer boundary of the anchor-side piece even if anchor_caret is
   * still Inner(n). Set when focus has crossed past the starting
   * token's edge; cleared when the selection shrinks back to a single
   * piece so the original inner anchor position re-displays. */
  smart_rounded: bool,
};

/* NOTE: backpack no longer uses selection focus */
let mk =
    (
      ~mode=Normal,
      ~focus=Direction.Left,
      ~anchor_caret=CaretBase.Outer,
      ~smart_rounded=false,
      content: Segment.t,
    ) => {
  focus,
  content,
  mode,
  anchor_caret,
  smart_rounded,
};

let mk_buffer = buffer =>
  mk(
    ~mode=Buffer(buffer),
    ~focus=Direction.Left,
    ~anchor_caret=CaretBase.Outer,
  );

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

let toggle_focus = selection => {
  ...selection,
  focus: Util_web.Direction.toggle(selection.focus),
};

let is_empty = (selection: t) => selection.content == Segment.empty;

/* The focus-side boundary piece of the selection content */
let focus_piece = (sel: t): option(Piece.t) =>
  switch (sel.focus, sel.content) {
  | (_, []) => None
  | (Right, content) => ListUtil.last_opt(content)
  | (Left, content) => ListUtil.hd_opt(content)
  };

/* The anchor-side boundary piece of the selection content */
let anchor_piece = (sel: t): option(Piece.t) =>
  switch (sel.focus, sel.content) {
  | (_, []) => None
  | (Right, content) => ListUtil.hd_opt(content)
  | (Left, content) => ListUtil.last_opt(content)
  };

let push =
    (p: Piece.t, {focus, content, mode, anchor_caret, smart_rounded}: t): t => {
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
    smart_rounded,
  };
};

/* Like push but without reassembly — used during char-level selection
 * to prevent matching shards from merging into multi-shard tiles,
 * which would break Inner(n) position tracking. */
let push_raw =
    (p: Piece.t, {focus, content, mode, anchor_caret, smart_rounded}: t): t => {
  let content =
    switch (focus) {
    | Left => Segment.cons(p, content)
    | Right => Segment.snoc(content, p)
    };
  {
    focus,
    content,
    mode,
    anchor_caret,
    smart_rounded,
  };
};

let pop = (sel: t): option((Piece.t, t)) => {
  let reset_anchor = (content: Segment.t, sel: t): t =>
    content == []
      ? {
        ...sel,
        content,
        anchor_caret: CaretBase.Outer,
        smart_rounded: false,
      }
      : {
        ...sel,
        content,
      };
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
