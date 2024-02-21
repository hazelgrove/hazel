open Sexplib.Std;
open Util;

module Meta = {
  type t = {
    col_target: int,
    touched: Touched.t,
    measured: Measured.t,
    term_ranges: TermRanges.t,
    segment: Segment.t,
    view_term: Term.UExp.t,
    terms: TermMap.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    buffer_ids: list(Id.t),
    start_map: Projector.start_map,
    last_map: Projector.start_map,
  };

  let init = (z: Zipper.t) => {
    let segment' = Zipper.unselect_and_zip(z);
    let segment = Projector.of_segment(z.projectors, segment');
    /*NOTE(andrew): consider using segment' for view_term (but terms problematic) */
    let (view_term, terms) = MakeTerm.go(segment);
    let term_ranges = TermRanges.mk(segment);
    let measured = Measured.of_segment(segment);
    {
      col_target: 0,
      touched: Touched.empty,
      measured,
      segment,
      term_ranges,
      tiles: TileMap.mk(segment),
      view_term,
      terms,
      holes: Segment.holes(segment),
      buffer_ids: Selection.buffer_ids(z.selection),
      start_map: Projector.mk_start_map(z.projectors, term_ranges),
      last_map: Projector.mk_last_map(z.projectors, term_ranges),
    };
  };

  module type S = {
    let touched: Touched.t;
    let measured: Measured.t;
    let term_ranges: TermRanges.t;
    let start_map: Projector.start_map;
    let last_map: Projector.start_map;
    let col_target: int;
  };
  let module_of_t = (m: t): (module S) =>
    (module
     {
       let touched = m.touched;
       let measured = m.measured;
       let term_ranges = m.term_ranges;
       let col_target = m.col_target;
       let start_map = m.start_map;
       let last_map = m.last_map;
     });

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let next =
      (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, meta: t): t => {
    let {touched, measured, col_target, _} = meta;
    let touched = Touched.update(Time.tick(), effects, touched);
    let is_edit = Action.is_edit(a);
    let segment' = is_edit ? Zipper.unselect_and_zip(z) : meta.segment;
    let segment = Projector.of_segment(z.projectors, segment');
    print_endline("projected segment:");
    segment |> Segment.show |> print_endline;
    let measured =
      is_edit
        ? Measured.of_segment(~touched, ~old=measured, segment) : measured;
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => col_target
      | _ =>
        switch (Indicated.index(z)) {
        | Some(i) => print_endline("indicated_id:" ++ Id.show(i))
        //let x = Id.Map.find(i, measured.tiles);
        | None => print_endline("no indicated_id")
        };
        Zipper.caret_point(measured, z).col;
      };
    let (view_term, terms) =
      //NOTE(andrew): could use unprojected version here, might be dangerous
      is_edit ? MakeTerm.go(segment) : (meta.view_term, meta.terms);
    let term_ranges = is_edit ? TermRanges.mk(segment) : meta.term_ranges;
    {
      col_target,
      touched,
      measured,
      segment,
      term_ranges,
      tiles: is_edit ? TileMap.mk(segment) : meta.tiles,
      view_term,
      terms,
      holes: is_edit ? Segment.holes(segment) : meta.holes,
      buffer_ids: Selection.buffer_ids(z.selection),
      start_map: Projector.mk_start_map(z.projectors, term_ranges),
      last_map: Projector.mk_last_map(z.projectors, term_ranges),
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
  update_z(_ => Zipper.add_projector(id, p, ed.state.zipper), ed);
