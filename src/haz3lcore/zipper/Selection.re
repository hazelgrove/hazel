open Util;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type buffer =
//   //| Parsed
//   | Unparsed;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type mode =
//   | Normal
//   | Buffer(buffer);

open Base.Selection;

type t = Base.Selection.t;
type buffer = Base.Selection.buffer;

/* NOTE: backpack no longer uses selection focus */
let mk = (~mode=Normal, ~focus=Direction.Left, content: Segment.t) => {
  focus,
  content,
  mode,
};

let mk_buffer = buffer => mk(~mode=Buffer(buffer), ~focus=Direction.Left);

let is_buffer: t => bool =
  fun
  | {mode: Buffer(_), _} => true
  | _ => false;

let selection_ids = (sel: t): list(Id.t) => Segment.ids(sel.content);

let empty = mk(Segment.empty);

let map = (f, sel) => {...sel, content: f(sel.content)};

let toggle_focus = selection => {
  ...selection,
  focus: Util.Direction.toggle(selection.focus),
};

let is_empty = (selection: t) => selection.content == Segment.empty;

let push = (p: Base.piece, {focus, content, mode}: t): t => {
  let content =
    Segment.reassemble(
      switch (focus) {
      | Left => Segment.cons(p, content)
      | Right => Segment.snoc(content, p)
      },
    );
  {focus, content, mode};
};

let pop = Base.Selection.pop;
