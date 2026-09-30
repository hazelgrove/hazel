open Haz3lcore;
open Util;

/* A program divided into open cells; see Divided.re. Abstract: the
   only ways back to a single editor are [close] and [join]. */

[@deriving (show({with_path: false}), sexp, yojson)]
type side =
  | Header
  | Body;

[@deriving (show({with_path: false}), sexp, yojson)]
type t;

[@deriving (show({with_path: false}), sexp, yojson)]
type after_close =
  | Still(t)
  | Joined(CellEditor.Model.t);

let split:
  (
    ~info_map: Language.Statics.Map.t,
    ~sym: string=?,
    CellEditor.Model.t,
    Id.t
  ) =>
  option(t);
let split_run:
  (~info_map: Language.Statics.Map.t, CellEditor.Model.t, Id.t) => option(t);
let join: t => CellEditor.Model.t;
let document: t => Segment.t;

let cells: t => list(ScratchCell.t);
let owner: (Id.t, t) => option(ScratchCell.t);
let position: (~term: Language.Exp.t, Id.t, t) => int;
let root: t => Sort.t;
let result: t => EvalResult.Model.t;
let with_result: (EvalResult.Model.t, t) => t;
let statics: t => CachedStatics.t;
let has_fresh_statics: t => bool;
let with_statics: (CachedStatics.t, t) => t;
let probes: t => Refractors.RefractorList.t;
let active: t => option((Id.t, side));
let active_editor: t => CellEditor.Model.t;
let outside_editor: t => CodeEditable.Model.t;

let open_:
  (
    ~info_map: Language.Statics.Map.t,
    ~term: Language.Exp.t,
    ~sym: string=?,
    Id.t,
    t
  ) =>
  option(t);
let open_run:
  (~info_map: Language.Statics.Map.t, ~term: Language.Exp.t, Id.t, t) =>
  option(t);
let close: (Id.t, t) => after_close;
let anchor_of: Zipper.t => option((Direction.t, Id.t));
let place_caret: ((Direction.t, Id.t), t) => t;
let toggle_run:
  (~info_map: Language.Statics.Map.t, ~term: Language.Exp.t, Id.t, t) =>
  after_close;
let resplit:
  (
    ~info_map: Language.Statics.Map.t,
    ~term: Language.Exp.t,
    CellEditor.Model.t,
    t
  ) =>
  after_close;

let same_content: (t, t) => bool;

let map_cells: (ScratchCell.t => ScratchCell.t, t) => t;
let update_cell: (int, ScratchCell.t => ScratchCell.t, t) => t;
let set_active: (int, side, t) => t;
let map_editors: (CellEditor.Model.t => CellEditor.Model.t, t) => t;
let compact: (CellEditor.Model.t => CellEditor.Model.t, t) => t;
