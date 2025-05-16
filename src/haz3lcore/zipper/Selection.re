open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type buffer =
  //| Parsed
  | Unparsed;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type mode =
  | Normal
  | Buffer(buffer);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('p) = {
  focus: Direction.t,
  content: Segment.t('p),
  mode,
};

/* NOTE: backpack no longer uses selection focus */
let mk = (~mode=Normal, ~focus=Direction.Left, content: Segment.t('p)) => {
  focus,
  content,
  mode,
};

let mk_buffer = buffer => mk(~mode=Buffer(buffer), ~focus=Direction.Left);

let is_buffer: t('p) => bool =
  fun
  | {mode: Buffer(_), _} => true
  | _ => false;

let selection_ids = (sel: t('p)): list(Id.t) => Segment.ids(sel.content);

let empty = mk(Segment.empty);

let map = (f, sel) => {
  ...sel,
  content: f(sel.content),
};

let toggle_focus = selection => {
  ...selection,
  focus: Util.Direction.toggle(selection.focus),
};

let is_empty = (selection: t('p)) => selection.content == Segment.empty;

let push = (p: Piece.t('p), {focus, content, mode}: t('p)): t('p) => {
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
  };
};

let pop = (sel: t('p)): option((Piece.t('p), t('p))) =>
  switch (sel.focus, sel.content, ListUtil.split_last_opt(sel.content)) {
  | (_, [], _)
  | (_, _, None) => None
  | (Left, [p, ...content], _) =>
    let (p, rest) = Piece.pop_l(p);
    Some((
      p,
      {
        ...sel,
        content: rest @ content,
      },
    ));
  | (Right, _, Some((content, p))) =>
    let (rest, p) = Piece.pop_r(p);
    Some((
      p,
      {
        ...sel,
        content: content @ rest,
      },
    ));
  };
