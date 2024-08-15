open Util;

module CachedSyntax = {
  type t = {
    segment: Segment.t,
    measured: Measured.t,
    tiles: TileMap.t,
    holes: list(Grout.t),
    selection_ids: list(Id.t),
    term: UExp.t,
    /* This term, and the term-derived data structured below, may differ
     * from the term used for semantics. These terms are identical when
     * the backpack is empty. If the backpack is non-empty, then when we
     * make the term for semantics, we attempt to empty the backpack
     * according to some simple heuristics (~ try to empty it greedily
     * while moving rightwards from the current caret position).
     * this is currently necessary to have the cursorinfo/completion
     * workwhen the backpack is nonempty.
     *
     * This is a brittle part of the current implementation. there are
     * some other comments at some of the weakest joints; the biggest
     * issue is that dropping the backpack can add/remove grout, causing
     * certain ids to be present/non-present unexpectedly. */
    term_ranges: TermRanges.t,
    terms: TermMap.t,
    projectors: Id.Map.t(Base.projector),
  };

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

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

  let calculate = (~is_edited, z: Zipper.t, info_map, old: t) =>
    is_edited
      ? init(z, info_map)
      : {...old, selection_ids: Selection.selection_ids(z.selection)};
};

module State = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    zipper: Zipper.t,
    col_target: option(int),
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

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated
    state: State.t,
    history: History.t,
    // Calculated
    [@opaque]
    syntax: CachedSyntax.t,
  };

  let mk = zipper => {
    state: {
      zipper,
      col_target: None,
    },
    history: History.empty,
    syntax: CachedSyntax.init(zipper, Id.Map.empty),
  };

  type persistent = PersistentZipper.t;
  let persist = (model: t) => model.state.zipper |> PersistentZipper.persist;
  let unpersist = p => p |> PersistentZipper.unpersist |> mk;

  let to_move_s = (model: t): (module Move.S) => {
    module M: Move.S = {
      let measured = model.syntax.measured;
      let term_ranges = model.syntax.term_ranges;
      let col_target = model.state.col_target |> Option.value(~default=0);
    };
    (module M);
  };

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
};

module Update = {
  type t = Action.t;

  let update =
      (
        ~settings: CoreSettings.t,
        a: Action.t,
        old_statics,
        {state, history, syntax}: Model.t,
      )
      : Action.Result.t(Model.t) => {
    open Result.Syntax;

    // 1. Clear the autocomplete buffer if relevant
    let state =
      settings.assist && settings.statics && a != Buffer(Accept)
        ? {
          ...state,
          zipper:
            Perform.go_z(
              ~settings,
              old_statics,
              Buffer(Clear),
              Model.to_move_s({state, history, syntax}),
              state.zipper,
            )
            |> Action.Result.ok
            |> Option.value(~default=state.zipper),
        }
        : state;

    // 2. Add to undo history
    let history =
      Action.is_historic(a) ? History.add(a, state, history) : history;

    // 3. Record target column if moving up/down
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) =>
        switch (state.col_target) {
        | Some(col) => Some(col)
        | None => Some(Zipper.caret_point(syntax.measured, state.zipper).col)
        }
      | _ => None
      };
    let state = {...state, col_target};

    // 4. Update the zipper
    let+ zipper =
      Perform.go_z(
        ~settings,
        old_statics,
        a,
        Model.to_move_s({state, history, syntax}),
        state.zipper,
      );

    // Recombine
    Model.{
      state: {
        zipper,
        col_target,
      },
      history,
      syntax,
    };
  };

  let undo = (ed: Model.t) =>
    switch (ed.history) {
    | ([], _) => None
    | ([(a, prev), ...before], after) =>
      Some(
        Model.{
          state: prev,
          history: (before, [(a, ed.state), ...after]),
          syntax: ed.syntax // Will be recalculated in calculate
        },
      )
    };
  let redo = (ed: Model.t) =>
    switch (ed.history) {
    | (_, []) => None
    | (before, [(a, next), ...after]) =>
      Some(
        Model.{
          state: next,
          history: ([(a, ed.state), ...before], after),
          syntax: ed.syntax // Will be recalculated in calculate
        },
      )
    };

  let can_undo = ed => Option.is_some(undo(ed));
  let can_redo = ed => Option.is_some(redo(ed));

  let calculate =
      (~settings: CoreSettings.t, ~is_edited, new_statics, ed: Model.t) => {
    // 1. Recalculate the autocomplete buffer if necessary
    let zipper =
      if (settings.assist && settings.statics && is_edited) {
        switch (
          Perform.go_z(
            ~settings,
            new_statics,
            Buffer(Set(TyDi)),
            Model.to_move_s(ed),
            ed.state.zipper,
          )
        ) {
        | Ok(z) => z
        | Error(_) => ed.state.zipper
        };
      } else {
        ed.state.zipper;
      };
    // 2. Recalculate syntax cache
    let syntax =
      CachedSyntax.calculate(
        ~is_edited,
        zipper,
        new_statics.info_map,
        ed.syntax,
      );
    // Recombine
    {
      ...ed,
      state: {
        ...ed.state,
        zipper,
      },
      syntax,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Model.t;
