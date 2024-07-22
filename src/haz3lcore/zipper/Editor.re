open Util;

module CachedStatics = {
  type t = {
    term: Term.UExp.t,
    info_map: Statics.Map.t,
    error_ids: list(Id.t),
  };

  let empty: t = {
    term: Term.UExp.{ids: [Id.invalid], term: Triv},
    info_map: Id.Map.empty,
    error_ids: [],
  };

  let mk = (~settings: CoreSettings.t, z: Zipper.t): t => {
    // Modify here to allow passing in an initial context
    let ctx_init = Builtins.ctx_init;
    let term = MakeTerm.from_zip_for_sem(z) |> fst;
    let info_map = Statics.mk(settings, ctx_init, term);
    let error_ids = Statics.Map.error_ids(info_map);
    {term, info_map, error_ids};
  };
};

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
    syntax_map: Id.Map.t(Piece.t),
  };

  type t = {
    col_target: int,
    touched: Touched.t,
    selection_ids: list(Id.t),
    statics: CachedStatics.t,
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
      syntax_map: z_projected.syntax_map,
    };
  };

  let mk_statics = (~settings: CoreSettings.t, z: Zipper.t) =>
    settings.statics ? CachedStatics.mk(~settings, z) : CachedStatics.empty;

  let init = (~settings: CoreSettings.t, z: Zipper.t) => {
    let statics = mk_statics(~settings, z);
    let projected =
      Projector.Project.go(z, statics.info_map) |> init_projected;
    {
      col_target: 0,
      touched: Touched.empty,
      selection_ids: Selection.selection_ids(projected.z.selection),
      statics,
      projected,
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
      syntax_map: z_projected.syntax_map,
    };
  };

  let next_statics =
      (
        ~settings: CoreSettings.t,
        a: Action.t,
        z: Zipper.t,
        old_statics: CachedStatics.t,
      ) =>
    if (!settings.statics) {
      CachedStatics.empty;
    } else if (!Action.is_edit(a)) {
      old_statics;
    } else {
      CachedStatics.mk(~settings, z);
    };

  let next =
      (
        ~effects as _: list(Effect.t)=[],
        ~settings: CoreSettings.t,
        a: Action.t,
        z: Zipper.t,
        meta: t,
      )
      : t => {
    print_endline("Editor.next. Action:" ++ Action.show(a));
    // Effects disabled below; if nothing breaks due to this then rip them out
    let touched = meta.touched; //Touched.update(Time.tick(), effects, meta.touched);
    let statics = next_statics(~settings, a, z, meta.statics);
    let z_projected = Projector.Project.go(z, statics.info_map);
    let projected =
      switch (Action.is_edit(a)) {
      | false => {...meta.projected, z: z_projected.z}
      | _ =>
        next_projected(z_projected, ~touched, ~old=meta.projected.measured)
      };
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => meta.col_target
      | _ => Zipper.caret_point(projected.measured, projected.z).col
      };
    {
      touched,
      col_target,
      projected,
      selection_ids: Selection.selection_ids(projected.z.selection),
      statics,
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

  let init = (zipper, ~settings: CoreSettings.t) => {
    zipper,
    meta: Meta.init(zipper, ~settings),
  };

  let next =
      (
        ~effects: list(Effect.t)=[],
        ~settings: CoreSettings.t,
        a: Action.t,
        z: Zipper.t,
        state,
      ) => {
    zipper: z,
    meta: Meta.next(~effects, ~settings, a, z, state.meta),
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

let init = (~read_only=false, z, ~settings: CoreSettings.t) => {
  state: State.init(z, ~settings),
  history: History.empty,
  read_only,
};

let update_z = (f: Zipper.t => Zipper.t, ed: t) => {
  ...ed,
  state: {
    ...ed.state,
    zipper: f(ed.state.zipper),
  },
};

let new_state =
    (
      ~effects: list(Effect.t)=[],
      ~settings: CoreSettings.t,
      a: Action.t,
      z: Zipper.t,
      ed: t,
    )
    : t => {
  let state = State.next(~effects, ~settings, a, z, ed.state);
  let history =
    Action.is_historic(a)
      ? History.add(a, ed.state, ed.history) : ed.history;
  {state, history, read_only: ed.read_only};
};

let update_statics = (~settings: CoreSettings.t, ed: t): t => {
  /* Use this function to force a statics update when (for example)
   * changing the statics settings */
  let statics = Meta.mk_statics(~settings, ed.state.zipper);
  {
    ...ed,
    state: {
      ...ed.state,
      meta: {
        ...ed.state.meta,
        statics,
      },
    },
  };
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

let map_projectors =
    (f: (Id.t, Projector.Map.entry) => Projector.Map.entry, ed: t) =>
  update_z(
    z => {...z, projectors: Projector.Map.mapi(f, z.projectors)},
    ed,
  );
