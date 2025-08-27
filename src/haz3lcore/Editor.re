open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated
    state: Perform.State.t,
    // Calculated
    [@opaque]
    syntax: CachedSyntax.t,
  };

  let mk = zipper => {
    state: {
      zipper,
      col_target: None,
    },
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
              ~syntax,
              ~statics=old_statics,
              Buffer(Clear),
              state,
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
      | Move(Spatial(Up | Down))
      | Select(Resize(Spatial(Up | Down))) =>
        switch (state.col_target) {
        | Some(col) => Some(col)
        | None => Some(Zipper.Caret.point(syntax.measured, state.zipper).col)
        }
      | _ => None
      };
    let state = {
      ...state,
      col_target,
    };

    // 4. Update the zipper
    let+ zipper =
      Perform.go_z(~settings, ~statics=old_statics, ~syntax, a, state);

    // Recombine
    Model.{
      state: {
        zipper,
        col_target,
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
            ~statics=new_statics,
            ~syntax,
            Buffer(Set(TyDi)),
            state,
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
