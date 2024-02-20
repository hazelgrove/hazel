open Sexplib.Std;
open Util;

module Meta = {
  type t = {
    col_target: int,
    touched: Touched.t,
    measured: Measured.t,
    term_ranges: TermRanges.t,
    unselected: Segment.t,
    segment: Segment.t,
    view_term: Term.UExp.t,
    terms: TermMap.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    buffer_ids: list(Id.t),
  };

  let init = (z: Zipper.t) => {
    let unselected = Zipper.unselect_and_zip(z);
    let (view_term, terms) = MakeTerm.go(unselected);
    let term_ranges = TermRanges.mk(unselected);
    let projectors = Projector.mk_nu_proj_map(z.projectors, term_ranges);
    print_endline("AAAAA");
    let unselected = Projector.of_segment(projectors, unselected);
    let term_ranges = TermRanges.mk(unselected);
    let measured = Measured.of_segment(unselected);
    print_endline("BBBBB");
    {
      col_target: 0,
      touched: Touched.empty,
      measured,
      unselected,
      term_ranges,
      segment: Projector.of_segment(projectors, Zipper.zip(z)),
      tiles: TileMap.mk(unselected),
      view_term,
      terms,
      holes: Segment.holes(unselected),
      buffer_ids: Selection.buffer_ids(z.selection),
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
       let measured = m.measured;
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
    let {touched, measured, col_target, term_ranges, _} = meta;
    let touched = Touched.update(Time.tick(), effects, touched);
    let is_edit = Action.is_edit(a);
    let unselected = is_edit ? Zipper.unselect_and_zip(z) : meta.unselected;
    let projectors = Projector.mk_nu_proj_map(z.projectors, term_ranges);
    let unselected = Projector.of_segment(projectors, unselected);
    let measured =
      is_edit
        ? Measured.of_segment(~touched, ~old=measured, unselected) : measured;
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => col_target
      | _ => Zipper.caret_point(measured, z).col
      };
    let (view_term, terms) =
      is_edit ? MakeTerm.go(unselected) : (meta.view_term, meta.terms);
    {
      col_target,
      touched,
      measured,
      unselected,
      term_ranges: is_edit ? TermRanges.mk(unselected) : meta.term_ranges,
      segment: Projector.of_segment(projectors, Zipper.zip(z)),
      tiles: is_edit ? TileMap.mk(unselected) : meta.tiles,
      view_term,
      terms,
      holes: is_edit ? Segment.holes(unselected) : meta.holes,
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

let caret_point = (ed: t): Measured.Point.t => {
  let State.{zipper, meta} = ed.state;
  Zipper.caret_point(meta.measured, zipper);
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
