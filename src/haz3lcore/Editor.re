open Util;

module CachedSyntax = {
  type t = {
    old: bool,
    segment: Segment.t,
    measured: Measured.t,
    tiles: TileMap.t,
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
    term_ranges: TermRanges.t,
    terms: TermMap.t,
    /* Since the introduction of shape_map below, caching projectors
     * here is almost vesigial (currently used only for error deco) */
    projectors: Id.Map.t(Base.projector),
    /* The shape_map is used to leave space for projectors in the
     * underlying editor. In principle calculating this can involve
     * both static and dynamic information, so we cache this for perf */
    shape_map: ProjectorCore.Shape.Map.t,
    cached_backpack: list(Tile.t),
  };

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let init = (~info_map, ~dyn_map, z): t => {
    let segment = Zipper.unselect_and_zip(z);
    //TODO(andrew): maybe need extra_probes here for real?
    let extra_probes = [];
    let MakeTerm.{term: _, terms, projectors} =
      MakeTerm.go(extra_probes, segment);
    let projector_shapes =
      ProjectorInfo.ShapeMapSemantics.mk(projectors, info_map, dyn_map);
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
      cached_backpack: Segment.global_missing_shards(segment),
    };
  };

  let mark_old: t => t =
    old => {
      ...old,
      old: true,
    };

  let calculate = (z: Zipper.t, info_map, dyn_map, old: t) =>
    old.old
      ? init(z, ~info_map, ~dyn_map)
      : {
        ...old,
        selection_ids: Selection.selection_ids(z.selection),
      };
};

module State = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    zipper: Zipper.t,
    col_target: option(int),
    /* Like projectors but not replacing syntax */
    refractors: Id.Map.t(Base.projector),
  };

  let mk = zipper => {
    zipper,
    col_target: None,
    refractors: Id.Map.empty,
  };
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated
    state: State.t,
    // Calculated
    [@opaque]
    syntax: CachedSyntax.t,
  };

  let mk = zipper => {
    state: State.mk(zipper),
    syntax:
      CachedSyntax.init(
        zipper,
        ~info_map=Id.Map.empty,
        ~dyn_map=Id.Map.empty,
      ),
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

  let trailing_hole_ctx = (ed: t, info_map: Language.Statics.Map.t) => {
    let segment = Zipper.unselect_and_zip(ed.state.zipper);
    let convex_grout = Segment.convex_grout(segment);
    // print_endline(String.concat("; ", List.map(Grout.show, convex_grout)));
    let last = Util.ListUtil.last_opt(convex_grout);
    switch (last) {
    | None => None
    | Some(grout) =>
      let id = grout.id;
      let info = Language.Statics.Map.lookup(id, info_map);
      switch (info) {
      | Some(info) => Some(Language.Info.ctx_of(info))
      | _ => None
      };
    };
  };
};

let ids_of_refractors = (refractors: Id.Map.t(Base.projector)): list(Id.t) =>
  refractors |> Id.Map.to_list |> List.map(((id, _p)) => id);

let mk_refractor_probe = (id: Id.t): option(Base.projector) => {
  open OptUtil.Syntax;
  let kind = ProjectorCore.Kind.Probe;
  let (module P) = ProjectorInit.to_module(kind);
  let seg: Segment.t = [Piece.mk_grout(Convex)];
  let piece: Base.piece = Segment.parenthesize(seg);
  let* any = MakeTerm.for_projection(seg);
  let+ model = P.init(any);
  {
    ...ProjectorCore.mk(kind, piece, model),
    id //TODO(andrew): betterify
  };
};

module Update = {
  type t = Action.t;

  let update =
      (
        ~settings: Language.CoreSettings.t,
        a: Action.t,
        old_statics,
        {state, syntax}: Model.t,
      )
      : Action.Result.t(Model.t) => {
    open Result.Syntax;
    let old_zipper = state.zipper;
    /* 1. Clear the autocomplete buffer if relevant. We clear the TyDi
     * (unparsed) buffer on every action except accept; for the LLM
     * (parsed) buffer, we accept resize actions to permit incremental
     * accepteance token-by-token or line-by-line */
    let clear_condition =
      (settings.assist && settings.statics && a != Buffer(Accept))
      && !(
           Selection.non_empty_parsed_buffer(state.zipper.selection)
           && (
             switch (a) {
             | Select(Resize(Local(_))) => true
             | _ => false
             }
           )
         );

    let state =
      clear_condition
        ? {
          ...state,
          zipper:
            Perform.go_z(
              ~settings,
              old_statics,
              Buffer(Clear),
              Model.to_move_s({
                state,
                syntax,
              }),
              state.zipper,
            )
            |> Action.Result.ok
            |> Option.value(~default=state.zipper),
        }
        : state;
    let syntax =
      if (clear_condition) {
        CachedSyntax.mark_old(syntax);
      } else {
        syntax;
      };
    /* TODO(andrew): Apologize to matt for below.
       If a buffer clear happens above then we must recalculate the
       syntax cache as otherwise the measured, in particular caret_point,
       will be looking for tiles inside the buffer, for example if we try
       to click or move down to dismiss a completion.*/
    let syntax =
      if (clear_condition
          && Selection.non_empty_parsed_buffer(old_zipper.selection)) {
        CachedSyntax.calculate(
          state.zipper,
          old_statics.info_map,
          Id.Map.empty, //TODO
          syntax,
        );
      } else {
        syntax;
      };

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

    //3.5 apply refractor
    let state =
      switch (a) {
      | Refractor(SetRefProbe) =>
        switch (Indicated.index(state.zipper)) {
        | None => state
        | Some(id) =>
          switch (Id.Map.find_opt(id, state.refractors)) {
          | Some(_) =>
            print_endline("removing refractor probe, id: " ++ Id.str8(id));
            {
              ...state,
              refractors: Id.Map.remove(id, state.refractors),
            };
          | None =>
            switch (mk_refractor_probe(id)) {
            | None => state
            | Some(p) =>
              print_endline("set refractor probe, id: " ++ Id.str8(id));
              {
                ...state,
                refractors: Id.Map.add(id, p, state.refractors),
              };
            }
          }
        }
      | _ => state
      };

    // 4. Update the zipper
    let+ zipper =
      Perform.go_z(
        ~settings,
        old_statics,
        a,
        Model.to_move_s({
          state,
          syntax,
        }),
        state.zipper,
      );

    // Recombine
    Model.{
      state: {
        zipper,
        col_target,
        refractors: state.refractors,
      },
      syntax,
    };
  };

  let calculate =
      (
        ~settings: Language.CoreSettings.t,
        ~is_edited,
        new_statics,
        dyn_map,
        {syntax, state}: Model.t,
      ) => {
    // 1. Recalculate the autocomplete buffer if necessary
    let zipper =
      if (settings.assist && settings.statics && is_edited) {
        switch (
          Perform.go_z(
            ~settings,
            new_statics,
            Buffer(Set(TyDi)),
            Model.to_move_s({
              syntax,
              state,
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
    // 2. Recalculate syntax cache
    let syntax = is_edited ? CachedSyntax.mark_old(syntax) : syntax;

    let syntax =
      CachedSyntax.calculate(zipper, new_statics.info_map, dyn_map, syntax);

    // Recombine
    Model.{
      state: {
        ...state,
        zipper,
      },
      syntax,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Model.t;
