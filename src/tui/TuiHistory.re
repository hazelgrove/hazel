open Haz3lcore;

/* Undo/redo over editor snapshots. The web History module wraps the
   whole Page model so it isn't reusable here; this is the same policy
   (snapshot per historic action, capped stack) over Editor.Model.t.
   Snapshots are persistent data structures, so retention is cheap. */

type t = {
  undo: list(Editor.Model.t),
  redo: list(Editor.Model.t),
};

let empty: t = {
  undo: [],
  redo: [],
};

/* Matches History.capped_undo_stack_size on the web side */
let cap = 250;

let push = (ed: Editor.Model.t, h: t): t => {
  undo: Util.ListUtil.take(cap, [ed, ...h.undo]),
  redo: [],
};

let undo = (current: Editor.Model.t, h: t): option((Editor.Model.t, t)) =>
  switch (h.undo) {
  | [] => None
  | [ed, ...rest] =>
    Some((
      ed,
      {
        undo: rest,
        redo: [current, ...h.redo],
      },
    ))
  };

let redo = (current: Editor.Model.t, h: t): option((Editor.Model.t, t)) =>
  switch (h.redo) {
  | [] => None
  | [ed, ...rest] =>
    Some((
      ed,
      {
        undo: [current, ...h.undo],
        redo: rest,
      },
    ))
  };
