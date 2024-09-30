open Util;

include Base.Selection;

/* NOTE: backpack no longer uses selection focus */
let mk = (~mode=Base.Normal, ~focus=Direction.Left, content: Segment.t): t => {
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

let map = (f, sel: t): t => {...sel, content: f(sel.content)};

let toggle_focus = (selection: t): t => {
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
