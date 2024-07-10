open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Meta = {
  /* Derived data for projected zipper */
  type projected = {
    z: Zipper.t,
    segment: Segment.t,
    measured: Measured.t,
    term: Term.UExp.t,
    term_ranges: TermRanges.t,
    terms: TermMap.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    buffer_ids: list(Id.t),
    syntax_map: Id.Map.t(Piece.t),
  };

  type t = {
    col_target: int,
    touched: Touched.t,
    projected,
  };

  let init_projected = (z_projected: Projector.proj_ret): projected => {
    let segment = Zipper.unselect_and_zip(z_projected.z);
    let (term, terms) = MakeTerm.go(segment);
    {
      z: z_projected.z,
      segment,
      term,
      terms,
      term_ranges: TermRanges.mk(segment),
      tiles: TileMap.mk(segment),
      holes: Segment.holes(segment),
      measured: Measured.of_segment(segment),
      buffer_ids: Selection.buffer_ids(z_projected.z.selection),
      syntax_map: z_projected.syntax_map,
    };
  };

  let init = (z: Zipper.t) => {
    {
      col_target: 0,
      touched: Touched.empty,
      projected: Projector.Project.go(z) |> init_projected,
    };
  };

  module type S = {
    let touched: Touched.t;
    let measured: Measured.t;
    let term_ranges: TermRanges.t;
    let col_target: int;
  };
  let module_of_t = (m: t): (module S) =>
    (module
     {
       let touched = m.touched;
       let measured = m.projected.measured;
       let term_ranges = m.projected.term_ranges;
       let col_target = m.col_target;
     });

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let next_projected = (z_projected: Projector.proj_ret, ~touched, ~old) => {
    let segment = Zipper.unselect_and_zip(z_projected.z);
    let (term, terms) = MakeTerm.go(segment);
    let measured = Measured.of_segment(~touched, ~old, segment);
    {
      z: z_projected.z,
      segment,
      term,
      terms,
      measured,
      term_ranges: TermRanges.mk(segment),
      tiles: TileMap.mk(segment),
      holes: Segment.holes(segment),
      buffer_ids: Selection.buffer_ids(z_projected.z.selection),
      syntax_map: z_projected.syntax_map,
    };
  };

  let next =
      (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, meta: t): t => {
    let touched = Touched.update(Time.tick(), effects, meta.touched);
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => meta.col_target
      | _ => Zipper.caret_point(meta.projected.measured, meta.projected.z).col
      };
    let z_projected = Projector.Project.go(z);
    let projected =
      switch (Action.is_edit(a)) {
      //TODO(andrew): reenable
      //TODO: andrew figure out why core desyncs from view on measure length.. prob use diff statics
      //| false => {...meta.projected, z: z_projected.z}
      | _ =>
        next_projected(z_projected, ~touched, ~old=meta.projected.measured)
      };
    {touched, col_target, projected};
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
  switch (Id.Map.find_opt(id, ed.state.meta.projected.tiles)) {
  | Some(tile) => Some(Tile(tile))
  | None =>
    List.find_opt((g: Grout.t) => g.id == id, ed.state.meta.projected.holes)
    |> Option.map(g => Piece.Grout(g))
  };
};
