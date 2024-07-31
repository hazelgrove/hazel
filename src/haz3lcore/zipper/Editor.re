open Util;

module CachedStatics = {
  type t = {
    term: UExp.t,
    info_map: Statics.Map.t,
    error_ids: list(Id.t),
  };

  let empty: t = {
    term: UExp.{ids: [Id.invalid], copied: false, term: Tuple([])},
    info_map: Id.Map.empty,
    error_ids: [],
  };

  let init = (~settings: CoreSettings.t, z: Zipper.t): t => {
    // Modify here to allow passing in an initial context
    let ctx_init = Builtins.ctx_init;
    let term = MakeTerm.from_zip_for_sem(z).term;
    let info_map = Statics.mk(settings, ctx_init, term);
    let error_ids = Statics.Map.error_ids(info_map);
    {term, info_map, error_ids};
  };

  let init = (~settings: CoreSettings.t, z: Zipper.t) =>
    settings.statics ? init(~settings, z) : empty;

  let next =
      (~settings: CoreSettings.t, a: Action.t, z: Zipper.t, old_statics: t): t =>
    if (!settings.statics) {
      empty;
    } else if (!Action.is_edit(a)) {
      old_statics;
    } else {
      init(~settings, z);
    };
};

module CachedSyntax = {
  type t = {
    segment: Segment.t,
    measured: Measured.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    selection_ids: list(Id.t),
    term: UExp.t,
    term_ranges: TermRanges.t,
    terms: TermMap.t,
    projectors: Id.Map.t(Base.projector),
  };

  let init = (z, info_map): t => {
    let segment = Zipper.unselect_and_zip(z);
    let MakeTerm.{term, terms, projectors} = MakeTerm.go(segment);
    {
      segment,
      term_ranges: TermRanges.mk(segment),
      tiles: TileMap.mk(segment),
      holes: Segment.holes(segment),
      measured: Measured.of_segment(segment, info_map),
      selection_ids: Selection.selection_ids(z.selection),
      term,
      terms,
      projectors,
    };
  };

  let next = (a: Action.t, z: Zipper.t, info_map, old: t) =>
    Action.is_edit(a)
      ? init(z, info_map)
      : {...old, selection_ids: Selection.selection_ids(z.selection)};
};

module Meta = {
  type t = {
    col_target: int,
    statics: CachedStatics.t,
    syntax: CachedSyntax.t,
  };

  let init = (~settings: CoreSettings.t, z: Zipper.t) => {
    let statics = CachedStatics.init(~settings, z);
    {col_target: 0, statics, syntax: CachedSyntax.init(z, statics.info_map)};
  };

  module type S = {
    let measured: Measured.t;
    let term_ranges: TermRanges.t;
    let col_target: int;
  };
  let module_of_t = (m: t): (module S) =>
    (module
     {
       let measured = m.syntax.measured;
       let term_ranges = m.syntax.term_ranges;
       let col_target = m.col_target;
     });

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let next = (~settings: CoreSettings.t, a: Action.t, z: Zipper.t, meta: t): t => {
    //print_endline("Editor.next. Action:" ++ Action.show(a));
    let syntax = CachedSyntax.next(a, z, meta.statics.info_map, meta.syntax);
    let statics = CachedStatics.next(~settings, a, z, meta.statics);
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) => meta.col_target
      | _ => (Zipper.caret_point(syntax.measured))(. z).col
      };
    {col_target, syntax, statics};
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

  let next = (~settings: CoreSettings.t, a: Action.t, z: Zipper.t, state) => {
    zipper: z,
    meta: Meta.next(~settings, a, z, state.meta),
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

let new_state =
    (~settings: CoreSettings.t, a: Action.t, z: Zipper.t, ed: t): t => {
  let state = State.next(~settings, a, z, ed.state);
  let history =
    Action.is_historic(a)
      ? History.add(a, ed.state, ed.history) : ed.history;
  {state, history, read_only: ed.read_only};
};

let update_statics = (~settings: CoreSettings.t, ed: t): t => {
  /* Use this function to force a statics update when (for example)
   * changing the statics settings */
  let statics = CachedStatics.init(~settings, ed.state.zipper);
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

let indicated_projector = (editor: t) =>
  Projector.indicated(editor.state.zipper);
