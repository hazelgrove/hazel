open Haz3lcore;
open Util;

/* A program divided into open cells. Abstract: a whole-program CellEditor
   comes back only through [join] or a [Joined] result. */

[@deriving (show({with_path: false}), sexp, yojson)]
type side = ScratchCell.side = | Header | Body;

[@deriving (show({with_path: false}), sexp, yojson)]
type t;

[@deriving (show({with_path: false}), sexp, yojson)]
type after_close =
  | Still(t)
  | Joined(CellEditor.Model.t);

/* no cells yet: only for [open_] or [open_run] to open the first */
let of_whole: CellEditor.Model.t => t;
/* with [prev], an earlier join: its parse and measure caches carry over */
let join: (~prev: CellEditor.Model.t=?, t) => CellEditor.Model.t;
/* one editor holding a program: the probes still anchored in it, the
   caret beside the anchor; with [prev], an earlier editor's caches */
let editor_of:
  (
    ~prev: CellEditor.Model.t=?,
    ~root: Sort.t,
    ~probes: Refractors.RefractorList.t,
    ~anchor: option((Direction.t, Id.t)),
    Segment.t
  ) =>
  CellEditor.Model.t;
/* [editor_of] for an edit of [d]'s program made whole (outline ops), with
   the caches of its shell, the whole editor it was cut from */
let shell_editor_of:
  (
    ~probes: Refractors.RefractorList.t,
    ~anchor: option((Direction.t, Id.t)),
    Segment.t,
    t
  ) =>
  CellEditor.Model.t;
let document: t => Segment.t;

let cells: t => list(ScratchCell.t);
/* the cell holding [id]: its item, a run member or any piece of its text */
let owner: (Id.t, t) => option(ScratchCell.t);
/* the open cell and its pane whose text holds [id] */
let pane_of: (Id.t, t) => option((ScratchCell.t, ScratchCell.side));
/* the cell keyed by [id] */
let cell: (Id.t, t) => option(ScratchCell.t);
let root: t => Sort.t;
let result: t => EvalResult.Model.t;
let with_result: (EvalResult.Model.t, t) => t;
let statics: t => CachedStatics.t;
let has_fresh_statics: t => bool;
let with_statics: (CachedStatics.t, t) => t;
let probes: t => Refractors.RefractorList.t;
let active: t => option((Id.t, side));
/* the pane with the focus: the active one, else the first cell's body */
let focused: t => option((ScratchCell.t, side));
let active_editor: t => CellEditor.Model.t;
let outside_editor: t => CodeEditable.Model.t;

let open_:
  (
    ~info_map: Language.Statics.Map.t,
    ~outline: OutlineTree.t,
    ~sym: string=?,
    ~inner: bool=?,
    Id.t,
    t
  ) =>
  option(t);
let open_run:
  (~info_map: Language.Statics.Map.t, ~outline: OutlineTree.t, Id.t, t) =>
  option(t);
let close: (Id.t, t) => after_close;
let anchor_of: Zipper.t => option((Direction.t, Id.t));
let place_caret: ((Direction.t, Id.t), t) => t;
let resplit:
  (
    ~info_map: Language.Statics.Map.t,
    ~outline: OutlineTree.t,
    CellEditor.Model.t,
    t
  ) =>
  after_close;

let same_content: (t, t) => bool;

let map_cells: (ScratchCell.t => ScratchCell.t, t) => t;
/* headerless cells take the headerless row id now in their text (an
   edit can replace an expression's root); the (old, new) ids */
let follow_headless:
  (~rows: Id.Map.t(unit), ~headless: Id.Map.t(unit), t) =>
  (t, list((Id.t, Id.t)));

let update_cell: (Id.t, ScratchCell.t => ScratchCell.t, t) => t;
let set_active: (Id.t, side, t) => t;
/* after cells of [prev] opened and closed: [prev]'s focused pane keeps
   the focus while open */
let keep_focus: (~prev: t, t) => t;
let map_editors: (CellEditor.Model.t => CellEditor.Model.t, t) => t;
/* the probe drawer stepping a sample, in whichever editor holds it */
let stepping: t => option(ProjectorBase.stepping);
/* one stepping drawer per program: opening one closes the others */
let keep_stepping: (Language.Sample.span_ref, t) => t;
let compact: (CellEditor.Model.t => CellEditor.Model.t, t) => t;
/* [d]'s editors with the parse and measure caches of [from]'s in the
   same place: the shell's, and each open cell's panes by item and side */
let with_caches_of: (~from: t, t) => t;
/* the shell's caches from, and into, a whole editor */
let with_shell_caches_of: (~from: CellEditor.Model.t, t) => t;
let shell_caches_into: (t, CellEditor.Model.t) => CellEditor.Model.t;
/* [d] showing [from]'s statics and run until it's analyzed: the
   program's, and each open cell's panes by item and side */
let with_statics_of: (~from: t, t) => t;
/* the same, from and into a whole editor */
let with_shell_statics_of: (~from: CellEditor.Model.t, t) => t;
let shell_statics_into: (t, CellEditor.Model.t) => CellEditor.Model.t;
