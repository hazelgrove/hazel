open Sexplib.Std;
open Util;

module Meta = {
  type t = {
    col_target: int,
    touched: Touched.t,
    z_projected: Zipper.t,
    measured_real: Measured.t,
    measured_projected: Measured.t,
    term_ranges_real: TermRanges.t,
    term_ranges_projected: TermRanges.t,
    segment_real: Segment.t,
    segment_projected: Segment.t,
    view_term: Term.UExp.t,
    terms: TermMap.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    buffer_ids: list(Id.t),
  };

  let init = (z: Zipper.t) => {
    let segment_real = Zipper.unselect_and_zip(z);
    let segment_projected = Projector.of_segment(z.projectors, segment_real);
    /*NOTE(andrew): consider using segment' for view_term (but terms problematic) */
    let (view_term, terms) = MakeTerm.go(segment_projected);
    {
      col_target: 0,
      touched: Touched.empty,
      z_projected: ProjectorAction.of_zipper(z),
      measured_real: Measured.of_segment(segment_real),
      measured_projected: Measured.of_segment(segment_projected),
      segment_real,
      segment_projected,
      term_ranges_real: TermRanges.mk(segment_real),
      term_ranges_projected: TermRanges.mk(segment_projected),
      tiles: TileMap.mk(segment_projected),
      view_term,
      terms,
      holes: Segment.holes(segment_projected),
      buffer_ids: Selection.buffer_ids(z.selection),
    };
  };

  //TODO(andrew): what is this module used for?
  module type S = {
    let touched: Touched.t;
    let measured_projected: Measured.t;
    let measured_real: Measured.t;
    let term_ranges_projected: TermRanges.t;
    let col_target: int;
  };
  let module_of_t = (m: t): (module S) =>
    (module
     {
       let touched = m.touched;
       let measured_projected = m.measured_projected;
       let measured_real = m.measured_real;
       let term_ranges_projected = m.term_ranges_projected;
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
    let is_edit = Action.is_edit(a);
    let z_projected = ProjectorAction.of_zipper(z);
    let segment_real =
      is_edit ? Zipper.unselect_and_zip(z) : meta.segment_real;
    let segment_projected = Projector.of_segment(z.projectors, segment_real);
    //print_endline("projected segment:");
    //segment |> Segment.show |> print_endline;
    let measured_real =
      Measured.of_segment(~touched, ~old=measured_real, segment_real);
    let measured_projected =
      Measured.of_segment(
        ~touched,
        ~old=measured_projected,
        segment_projected,
      );
    let term_ranges_real =
      is_edit ? TermRanges.mk(segment_real) : meta.term_ranges_real;
    let term_ranges_projected =
      is_edit ? TermRanges.mk(segment_projected) : meta.term_ranges_projected;
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => col_target
      | _ =>
        switch (Indicated.index(z_projected)) {
        | Some(i) => print_endline("indicated_id:" ++ Id.show(i))
        //let x = Id.Map.find(i, measured.tiles);
        | None => print_endline("no indicated_id")
        };
        print_endline("Editor.next.caret_point");
        Zipper.caret_point(measured_projected, z_projected).col;
      };
    let (view_term, terms) =
      //NOTE(andrew): could use unprojected version here, might be dangerous
      is_edit
        ? MakeTerm.go(segment_projected) : (meta.view_term, meta.terms);
    {
      col_target,
      touched,
      z_projected,
      measured_real,
      measured_projected,
      segment_real,
      segment_projected,
      term_ranges_real,
      term_ranges_projected,
      tiles: is_edit ? TileMap.mk(segment_real) : meta.tiles,
      view_term,
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

let get_projector = (id: Id.t, ed: t) =>
  Projector.Map.find(id, ed.state.zipper.projectors);

let add_projector = (id: Id.t, p: Projector.t, ed: t) =>
  update_z(
    z => {...z, projectors: Projector.Map.add(id, p, z.projectors)},
    ed,
  );
