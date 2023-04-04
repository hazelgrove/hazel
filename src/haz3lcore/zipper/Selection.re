open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type buffer =
  | Amorphous
  | Solid;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Normal
  | Buffer(buffer);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  focus: Direction.t,
  content: Segment.t,
  mode,
};

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

let empty = mk(Segment.empty);

let map = (f, sel) => {...sel, content: f(sel.content)};

let toggle_focus = selection => {
  ...selection,
  focus: Util.Direction.toggle(selection.focus),
};

let is_empty = (selection: t) => selection.content == Segment.empty;

let push = (p: Piece.t, {focus, content, mode}: t): t => {
  let content =
    Segment.reassemble(
      switch (focus) {
      | Left => Segment.cons(p, content)
      | Right => Segment.snoc(content, p)
      },
    );
  {focus, content, mode};
};

let pop = (sel: t): option((Piece.t, t)) =>
  switch (sel.focus, sel.content, ListUtil.split_last_opt(sel.content)) {
  | (_, [], _)
  | (_, _, None) => None
  | (Left, [p, ...content], _) =>
    let (p, rest) = Piece.pop_l(p);
    Some((p, {...sel, content: rest @ content}));
  | (Right, _, Some((content, p))) =>
    let (rest, p) = Piece.pop_r(p);
    Some((p, {...sel, content: content @ rest}));
  };

let split_piece = _: option((Piece.t, t)) => failwith("todo split_piece");
