open Sexplib.Std;
open Util;

module Meta = {
  type t = {
    col_target: int,
    touched: Touched.t,
    segment_real: Segment.t,
    measured_real: Measured.t,
    z_projected: Zipper.t,
    segment_projected: Segment.t,
    measured_projected: Measured.t,
    term_projected: Term.UExp.t,
    term_ranges: TermRanges.t,
    terms: TermMap.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    buffer_ids: list(Id.t),
  };

  let init = (z: Zipper.t) => {
    let segment_real = Zipper.unselect_and_zip(z);
    let measured_real = Measured.of_segment(segment_real);
    let is_proj = !Id.Map.is_empty(z.projectors);
    let z_projected = is_proj ? ProjectorAction.of_zipper(z) : z;
    let segment_projected =
      is_proj ? Zipper.unselect_and_zip(z_projected) : segment_real;
    let (term_projected, terms) = MakeTerm.go(segment_projected);
    {
      col_target: 0,
      touched: Touched.empty,
      z_projected,
      segment_real,
      measured_real,
      measured_projected: Measured.of_segment(segment_projected),
      segment_projected,
      term_ranges: TermRanges.mk(segment_projected),
      tiles: TileMap.mk(segment_projected),
      term_projected,
      terms,
      holes: Segment.holes(segment_projected),
      buffer_ids: Selection.buffer_ids(z.selection),
    };
  };

  module type S = {
    let touched: Touched.t;
    let measured_projected: Measured.t;
    let term_ranges: TermRanges.t;
    let col_target: int;
  };
  let module_of_t = (m: t): (module S) =>
    (module
     {
       let touched = m.touched;
       let measured_projected = m.measured_projected;
       let term_ranges = m.term_ranges;
       let col_target = m.col_target;
     });

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let next =
      (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, meta: t): t => {
    let {touched, measured_real, measured_projected, col_target, _} = meta;
    let touched = Touched.update(Time.tick(), effects, touched);
    //TODO(andrew): gate appropriate things on edit
    let is_edit = Action.is_edit(a);
    let is_proj = !Id.Map.is_empty(z.projectors);
    let segment_real =
      is_edit ? Zipper.unselect_and_zip(z) : meta.segment_real;
    let measured_real =
      Measured.of_segment(~touched, ~old=measured_real, segment_real);
    let z_projected = is_proj ? ProjectorAction.of_zipper(z) : z;
    let segment_projected =
      is_proj
        ? Projector.of_segment(z.projectors, segment_real) : segment_real;
    let measured_projected =
      is_proj
        ? Measured.of_segment(
            ~touched,
            ~old=measured_projected,
            segment_projected,
          )
        : measured_real;
    let term_ranges =
      is_edit ? TermRanges.mk(segment_projected) : meta.term_ranges;
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => col_target
      | _ =>
        // switch (Indicated.index(z_projected)) {
        // | Some(i) => print_endline("indicated_id:" ++ Id.show(i))
        // | None => print_endline("no indicated_id")
        // };
        print_endline("Editor.next.caret_point");
        Zipper.caret_point(measured_projected, z_projected).col;
      };
    let (term_projected, terms) =
      is_edit
        ? MakeTerm.go(segment_projected) : (meta.term_projected, meta.terms);
    {
      col_target,
      touched,
      z_projected,
      measured_real,
      measured_projected,
      segment_real,
      segment_projected,
      term_ranges,
      tiles: is_edit ? TileMap.mk(segment_projected) : meta.tiles,
      term_projected,
      terms,
      //NOTE(andrew): this is seg_project bc otherwise Code.of_hole crashes
      holes: is_edit ? Segment.holes(segment_projected) : meta.holes,
      buffer_ids: Selection.buffer_ids(z.selection),
    };
  };
};

module State = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    zipper: Zipper.t,
    [@opaque]
    meta: Meta.t,
  };

  let init = zipper => {zipper, meta: Meta.init(zipper)};

  let next = (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, state) => {
    zipper: z,
    meta: Meta.next(~effects, a, z, state.meta),
  };
};

module History = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type affix = list((Action.t, State.t));
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (affix, affix);

  let empty = ([], []);

  let add = (a: Action.t, state: State.t, (pre, _): t): t => (
    [(a, state), ...pre],
    [],
  );
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  state: State.t,
  history: History.t,
  read_only: bool,
};

let init = (~read_only=false, z) => {
  state: State.init(z),
  history: History.empty,
  read_only,
};
let empty = id => init(~read_only=false, Zipper.init(id));

let update_z = (f: Zipper.t => Zipper.t, ed: t) => {
  ...ed,
  state: {
    ...ed.state,
    zipper: f(ed.state.zipper),
  },
};
let put_z = (z: Zipper.t) => update_z(_ => z);

let update_z_opt = (f: Zipper.t => option(Zipper.t), ed: t) => {
  open OptUtil.Syntax;
  let+ z = f(ed.state.zipper);
  put_z(z, ed);
};

let new_state =
    (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, ed: t): t => {
  let state = State.next(~effects, a, z, ed.state);
  let history = History.add(a, ed.state, ed.history);
  {state, history, read_only: ed.read_only};
};

let undo = (ed: t) =>
  switch (ed.history) {
  | ([], _) => None
  | ([(a, prev), ...before], after) =>
    Some({
      state: prev,
      history: (before, [(a, ed.state), ...after]),
      read_only: ed.read_only,
    })
  };
let redo = (ed: t) =>
  switch (ed.history) {
  | (_, []) => None
  | (before, [(a, next), ...after]) =>
    Some({
      state: next,
      history: ([(a, ed.state), ...before], after),
      read_only: ed.read_only,
    })
  };

let can_undo = ed => Option.is_some(undo(ed));
let can_redo = ed => Option.is_some(redo(ed));

let set_read_only = (ed, read_only) => {...ed, read_only};

let trailing_hole_ctx = (ed: t, info_map: Statics.Map.t) => {
  let segment = Zipper.unselect_and_zip(ed.state.zipper);
  let convex_grout = Segment.convex_grout(segment);
  // print_endline(String.concat("; ", List.map(Grout.show, convex_grout)));
  let last = Util.ListUtil.last_opt(convex_grout);
  switch (last) {
  | None => None
  | Some(grout) =>
    let id = grout.id;
    let info = Id.Map.find_opt(id, info_map);
    switch (info) {
    | Some(info) => Some(Info.ctx_of(info))
    | _ => None
    };
  };
};

let get_projectors = (ed: t) => ed.state.zipper.projectors;

let map_projectors = (f: (Id.t, Projector.t) => Projector.t, ed: t) =>
  update_z(
    z => {...z, projectors: Projector.Map.mapi(f, z.projectors)},
    ed,
  );

//TODO(andrew): use or lose
let get_projected_piece = (ed: t, id: Id.t): option(Piece.t) => {
  /* Assumes for the moment that the projected thing is either
   * a tile or a grout (not secondary or segment) */
  switch (Id.Map.find_opt(id, ed.state.meta.tiles)) {
  | Some(tile) => Some(Tile(tile))
  | None =>
    List.find_opt((g: Grout.t) => g.id == id, ed.state.meta.holes)
    |> Option.map(g => Piece.Grout(g))
  };
};
