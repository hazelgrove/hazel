open Util;

module CachedSyntax = {
  type t('p) = {
    old: bool,
    segment: Segment.t('p),
    measured: Measured.t,
    tiles: TileMap.t('p),
    selection_ids: list(Id.t),
    /* The term-derived data structured below, may differ
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
    term_ranges: TermRanges.t('p),
    terms: TermMap.t,
    /* Since the introduction of shape_map below, caching projectors
     * here is almost vesigial (currently used only for error deco) */
    projectors: Id.Map.t(Base.projector('p)),
    /* The shape_map is used to leave space for projectors in the
     * underlying editor. In principle calculating this can involve
     * both static and dynamic information, so we cache this for perf */
    shape_map: ProjectorShape.Map.t,
  };

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let init =
      (
        type p,
        ~shape_of_projector,
        ~projector_to_term,
        ~info_map,
        ~dyn_map,
        ~sort: Sort.t,
        z: Zipper.t(p),
      )
      : t(p) => {
    let segment = Zipper.unselect_and_zip(z);
    let MakeTerm.{term: _, terms, projectors} =
      MakeTerm.go(sort, segment, ~of_projector=projector_to_term);
    let projector_shapes =
      ProjectorInfo.ShapeMapSemantics.mk(
        ~shape_of_projector,
        projectors,
        info_map,
        dyn_map,
      );
    {
      old: false,
      segment,
      term_ranges: TermRanges.mk(segment),
      tiles: TileMap.mk(segment),
      measured: Measured.of_segment(segment, projector_shapes),
      selection_ids: Selection.selection_ids(z.selection),
      terms,
      projectors,
      shape_map: projector_shapes,
    };
  };

  let mark_old: t('p) => t('p) =
    old => {
      ...old,
      old: true,
    };

  let calculate =
      (
        ~shape_of_projector,
        ~projector_to_term,
        z: Zipper.t('p),
        info_map,
        dyn_map,
        sort,
        old: t('p),
      ) =>
    old.old
      ? init(
          z,
          ~sort,
          ~shape_of_projector,
          ~projector_to_term,
          ~info_map,
          ~dyn_map,
        )
      : {
        ...old,
        selection_ids: Selection.selection_ids(z.selection),
      };
};

module State = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t('p) = {
    zipper: Zipper.t('p),
    col_target: option(int),
  };
};

module History = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type affix('p_k, 'p, 'p_a) =
    list((Action.t('p_k, 'p, 'p_a), State.t('p)));
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t('p_k, 'p, 'p_a) = (affix('p_k, 'p, 'p_a), affix('p_k, 'p, 'p_a));

  let empty = ([], []);

  let add =
      (
        a: Action.t('p_k, 'p, 'p_a),
        state: State.t('p),
        (pre, _): t('p_k, 'p, 'p_a),
      )
      : t('p_k, 'p, 'p_a) => (
    [(a, state), ...pre],
    [],
  );
};

[@warning "-20"]
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('p_k, 'p, 'p_a) = {
    // Updated
    state: State.t('p),
    history: History.t('p_k, 'p, 'p_a),
    // Calculated
    [@opaque]
    syntax: CachedSyntax.t('p),
  };

  let mk =
      (
        type a,
        ~sort,
        ~shape_of_projector,
        ~projector_to_term,
        zipper: Zipper.t(a),
      ) => {
    state: {
      zipper,
      col_target: None,
    },
    history: History.empty,
    syntax:
      CachedSyntax.init(
        zipper,
        ~sort,
        ~projector_to_term,
        ~shape_of_projector,
        ~info_map=Id.Map.empty,
        ~dyn_map=Id.Map.empty,
      ),
  };

  type persistent = PersistentZipper.t;
  let persist = (f: 'p => 'q, model: t('p_k, 'p, 'p_a)) =>
    model.state.zipper |> PersistentZipper.persist(f);
  let unpersist = (f, p) => p |> PersistentZipper.unpersist(f) |> mk;

  let to_move_s =
      (type p', model: t('p_k, p', 'p_a)): (module Move.S with type p = p') => {
    module M: Move.S with type p = p' = {
      type p = p';
      let measured = model.syntax.measured;
      let term_ranges = model.syntax.term_ranges;
      let col_target = model.state.col_target |> Option.value(~default=0);
    };
    (module M);
  };

  let trailing_hole_ctx = (ed: t('p_k, 'p, 'p_a), info_map: Statics.Map.t) => {
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
};

module Update = {
  type t('p_k, 'p, 'p_a) = Action.t('p_k, 'p, 'p_a);

  let update =
      (
        type p,
        type p_k,
        type p_a,
        ~settings: CoreSettings.t,
        ~sort,
        ~projector_init,
        ~seg_of_projector,
        ~shape_of_projector,
        ~projector_to_term,
        ~get_focusable,
        ~update_projector,
        ~livelit_projectors,
        a: Action.t(p_k, p, p_a),
        old_statics,
        {state, history, syntax}: Model.t(p_k, p, p_a),
      )
      : Action.Result.t(Model.t(p_k, p, p_a)) => {
    let seg_to_ed = seg =>
      Zipper.unzip(seg)
      |> Model.mk(~sort, ~shape_of_projector, ~projector_to_term)
      |> Option.some;
    open Result.Syntax;
    // 1. Clear the autocomplete buffer if relevant
    let state =
      settings.assist && settings.statics && a != Buffer(Accept)
        ? {
          ...state,
          zipper:
            Perform.go_z(
              ~settings,
              ~seg_to_ed,
              ~projector_init,
              ~seg_of_projector,
              ~get_focusable,
              ~update_projector,
              ~livelit_projectors,
              old_statics,
              Buffer(Clear),
              Model.to_move_s({
                state,
                history,
                syntax,
              }),
              state.zipper,
            )
            |> Action.Result.ok
            |> Option.value(~default=state.zipper),
        }
        : state;
    let syntax =
      if (settings.assist && settings.statics && a != Buffer(Accept)) {
        CachedSyntax.mark_old(syntax);
      } else {
        syntax;
      };

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
    let state = {
      ...state,
      col_target,
    };

    // 4. Update the zipper
    let+ zipper =
      Perform.go_z(
        ~settings,
        ~projector_init,
        ~seg_of_projector,
        ~seg_to_ed,
        ~get_focusable,
        ~update_projector,
        ~livelit_projectors,
        old_statics,
        a,
        Model.to_move_s({
          state,
          history,
          syntax,
        }),
        state.zipper,
      );

    settings.flip_animations && Action.should_animate(a)
      ? Animation.request([Animation.Actions.move("caret")]) : ();

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

  let undo = (ed: Model.t('p_k, 'p, 'p_a)) =>
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
  let redo = (ed: Model.t('p_k, 'p, 'p_a)) =>
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
      (
        type p,
        ~common,
        ~settings: CoreSettings.t,
        ~projector_init,
        ~projector_to_term,
        ~seg_of_projector,
        ~shape_of_projector,
        ~get_focusable,
        ~livelit_projectors,
        ~update_projector,
        ~calculate_projector,
        ~is_edited,
        ~sort,
        new_statics,
        dyn_map,
        {syntax, state, history}: Model.t('p_k, p, 'p_a),
      ) => {
    let seg_to_ed = seg =>
      Zipper.unzip(seg)
      |> Model.mk(~sort, ~shape_of_projector, ~projector_to_term)
      |> Option.some;

    // 1. Recalculate the autocomplete buffer if necessary
    let zipper =
      if (settings.assist && settings.statics && is_edited) {
        switch (
          Perform.go_z(
            ~settings,
            ~seg_to_ed,
            ~projector_init,
            ~seg_of_projector,
            ~get_focusable,
            ~update_projector,
            ~livelit_projectors,
            new_statics,
            Buffer(Set(TyDi)),
            Model.to_move_s({
              syntax,
              state,
              history,
            }),
            state.zipper,
          )
        ) {
        | Ok(z) => z
        | Error(_) => state.zipper
        };
      } else {
        state.zipper;
      };

    // 2. Recalculate Projector models

    // TODO[Matt]: Get sorts right here.
    let zipper =
      ZipperBase.MapPiece.go(
        fun
        | Projector(p) => [
            Projector({
              ...p,
              model: calculate_projector(~common, ~sort=Sort.Any, p.model),
            }),
          ]
        | x => [x],
        zipper,
      );

    // 3. Recalculate syntax cache
    let syntax = is_edited ? CachedSyntax.mark_old(syntax) : syntax;

    let syntax =
      CachedSyntax.calculate(
        ~projector_to_term,
        ~shape_of_projector,
        zipper,
        new_statics.info_map,
        dyn_map,
        sort,
        syntax,
      );

    // Recombine
    Model.{
      history,
      state: {
        ...state,
        zipper,
      },
      syntax,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('p_k, 'p, 'p_a) = Model.t('p_k, 'p, 'p_a);
