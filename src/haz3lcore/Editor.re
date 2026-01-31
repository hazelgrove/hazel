open Util;
open OptUtil.Syntax;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Perform.state;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    state,
    [@opaque]
    syntax: CachedSyntax.t /* Calculated */
  };

  let mk = (zipper: Zipper.t): t => {
    state: {
      zipper,
      col_target: None,
    },
    syntax: CachedSyntax.init(zipper),
  };

  type persistent = PersistentZipper.t;

  let persist = (model: t): persistent =>
    model.state.zipper |> PersistentZipper.persist;

  let unpersist = (p: persistent): t => p |> PersistentZipper.unpersist |> mk;

  let trailing_hole_ctx =
      (ed: t, info_map: Language.Statics.Map.t): option(Ctx.t) => {
    let* grout =
      ed.state.zipper
      |> Zipper.unselect_and_zip
      |> Segment.convex_grout
      |> Util.ListUtil.last_opt;
    let+ info = Language.Statics.Map.lookup(grout.id, info_map);
    Language.Info.ctx_of(info);
  };
};

module Update = {
  type t = Action.t;

  let update_col_target =
      (~measured: Measured.t, a: Action.t, state: Model.state): Model.state => {
    let col_target =
      switch (a) {
      | Move(Vertical(Up | Down))
      | Select(Resize(Vertical(Up | Down))) =>
        switch (state.col_target) {
        | Some(col) => Some(col)
        | None => Some(Zipper.Caret.point(measured, state.zipper).col)
        }
      | _ => None
      };
    {
      ...state,
      col_target,
    };
  };

  let should_clear_buffer =
      (~settings: Language.CoreSettings.t, ~a: Action.t, state: Model.state) => {
    /* We clear the TyDi (unparsed) buffer on every action except Accept.
     * For the LLM (parsed) buffer, we accept resize actions to permit
     * incremental acceptance token-by-token or line-by-line. */
    let is_local_resize = (a: Action.t) =>
      switch (a) {
      | Select(Resize(Local(_))) => true
      | _ => false
      };
    settings.assist
    && settings.statics
    && a != Buffer(Accept)
    && !(
         Selection.non_empty_parsed_buffer(state.zipper.selection)
         && is_local_resize(a)
       );
  };

  let clear_buffer =
      (
        ~settings: Language.CoreSettings.t,
        ~old_zipper: Zipper.t,
        ~old_statics: CachedStatics.t,
        ~old_dynamics: Dynamics.Map.t,
        ~a: Action.t,
        state: Model.state,
        syntax: CachedSyntax.t,
      )
      : (Model.state, CachedSyntax.t) =>
    if (should_clear_buffer(~settings, ~a, state)) {
      let syntax =
        if (Selection.non_empty_parsed_buffer(old_zipper.selection)) {
          /* If a buffer clear happens above then we must recalculate the
             syntax cache as otherwise the measured, in particular caret_point,
             will be looking for tiles inside the buffer, for example if we try
             to click or move down to dismiss a completion.*/
          CachedSyntax.calculate(
            state.zipper,
            old_statics.info_map,
            old_dynamics,
            syntax,
          );
        } else {
          syntax;
        };
      (
        {
          ...state,
          zipper: Buffer.buffer_clear(state.zipper),
        },
        CachedSyntax.mark_old(syntax),
      );
    } else {
      (state, syntax);
    };

  let update =
      (
        ~settings: Language.CoreSettings.t,
        a: Action.t,
        old_statics: CachedStatics.t,
        old_dynamics: Dynamics.Map.t,
        {state, syntax}: Model.t,
      )
      : Action.Result.t(Model.t) => {
    open Result.Syntax;

    /* 1. Clear the autocomplete buffer when relevant */
    let (state, syntax) =
      clear_buffer(
        ~settings,
        ~old_zipper=state.zipper,
        ~old_statics,
        ~old_dynamics,
        ~a,
        state,
        syntax,
      );

    /* 2. Record target column if moving up/down */
    let state = update_col_target(~measured=syntax.measured, a, state);

    /* 3. Update the zipper */
    let+ zipper = Perform.go(~statics=old_statics, ~syntax, a, state);

    SyncReplace.send_state(a, state.zipper, zipper);
    SyncReplace.send_caret(a, zipper);

    Model.{
      state: {
        ...state,
        zipper,
      },
      syntax,
    };
  };

  let calculate =
      (
        ~settings: Language.CoreSettings.t,
        ~is_edited,
        new_statics: CachedStatics.t,
        new_dynamics: Dynamics.Map.t,
        {syntax, state}: Model.t,
      )
      : Model.t => {
    /* 1. Recalculate the autocomplete buffer if necessary */
    let zipper =
      if (settings.assist && settings.statics && is_edited) {
        Buffer.set_tydi_buffer(
          Indicated.ci_of(state.zipper, new_statics.info_map),
          state.zipper,
        );
      } else {
        state.zipper;
      };

    /* 2. Recalculate syntax cache */
    let syntax = is_edited ? CachedSyntax.mark_old(syntax) : syntax;
    let syntax =
      CachedSyntax.calculate(
        zipper,
        new_statics.info_map,
        new_dynamics,
        syntax,
      );

    /* 3. Probe effects: collision cleanup, auto-probe regeneration,
     *    step-into focus resolution, and cursor reset */
    let zipper =
      ProbePerform.editor_effects(
        ~syntax,
        ~info_map=new_statics.info_map,
        ~dynamics=new_dynamics,
        zipper,
      );

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
