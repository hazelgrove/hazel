open Util_web;
open OptUtil.Syntax;
open Language;

/* The `root` field records the sort of this editor's root context. We
   can't recover it from the zipper alone: an empty zipper has no surviving
   ancestors to infer sort from. Two places need it:
     1. Ancestors.sort falls back to `root` when the ancestor stack is
        empty, which remolding and regrouting rely on to pick Drv(Exp)
        molds inside derivation editors.
     2. Editor.Model.mk(~root) propagates this sort to the term builder so
        the initial zipper is constructed with the correct sort. */
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Perform.state;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    root: Sort.t,
    state,
    [@opaque]
    syntax: CachedSyntax.t /* Calculated */
  };

  let mk = (zipper: Zipper.t, ~root): t => {
    root,
    state: {
      zipper,
      col_target: None,
    },
    syntax: CachedSyntax.init(zipper),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    root: Sort.t,
    zipper: PersistentZipper.t,
  };

  let persist = (model: t): persistent => {
    root: model.root,
    zipper: model.state.zipper |> PersistentZipper.persist,
  };

  let unpersist = (p: persistent): t =>
    p.zipper |> PersistentZipper.unpersist(~root=p.root) |> mk(~root=p.root);

  let mk_persistent = (zipper: PersistentZipper.t, ~root): persistent => {
    root,
    zipper,
  };

  let to_string = (model: t): string =>
    model.state.zipper |> PersistentZipper.to_string;

  let trailing_hole_ctx =
      (ed: t, info_map: Language.Statics.Map.t): option(Ctx.t) => {
    let* grout =
      ed.state.zipper
      |> Zipper.unselect_and_zip
      |> Segment.convex_grout
      |> Util_web.ListUtil.last_opt;
    Language.Statics.Map.ctx_of(grout.id, info_map);
  };
};

module Update = {
  type t = Action.t;

  let update_col_target =
      (~measured: Measured.t, a: Action.t, state: Model.state): Model.state => {
    let col_target =
      switch (a) {
      | Move(Vertical(Up | Down, _))
      | Select(Resize(Vertical(Up | Down, _))) =>
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
            ~elaborated=Some(old_statics.elaborated),
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
        {state, syntax, root}: Model.t,
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
    let+ zipper =
      Perform.go(~settings, ~statics=old_statics, ~syntax, a, state, ~root);

    Model.{
      root,
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
        ~autoprobe_mode: bool,
        ~is_edited,
        statics: CachedStatics.t,
        new_dynamics: Dynamics.Map.t,
        {syntax, state, root}: Model.t,
      )
      : Model.t => {
    /* 1. Recalculate the autocomplete buffer if necessary.
     * Uses ci_for_completion (which prefers the left-neighbor tile,
     * falling back to ci_of) so that the automatic post-edit buffer
     * recompute is consistent with Perform.go's explicit
     * Buffer(Set(TyDi)) path. */
    let zipper =
      if (settings.assist && settings.statics && is_edited) {
        Buffer.set_tydi_buffer(
          Indicated.ci_for_completion(state.zipper, statics.info_map),
          state.zipper,
        );
      } else {
        state.zipper;
      };

    /* 2. Recalculate syntax cache. `CachedSyntax.calculate` detects
     * input changes (info_map/dyn_map/elaborated refs) and chooses
     * between full `mk`, shape-only refresh, or cheap selection-only
     * update — so callers don't need to plumb "statics changed" signals. */
    let syntax = is_edited ? CachedSyntax.mark_old(syntax) : syntax;
    let syntax =
      CachedSyntax.calculate(
        zipper,
        statics.info_map,
        new_dynamics,
        ~elaborated=Some(statics.elaborated),
        syntax,
      );

    /* 3. Probe effects: collision cleanup, auto-probe regeneration,
     *    step-into focus resolution, and cursor reset. May mutate
     *    refractors (manuals/ephemerals). */
    let zipper =
      ProbePerform.editor_effects(
        ~is_edited,
        ~syntax,
        ~info_map=statics.info_map,
        ~dynamics=new_dynamics,
        zipper,
      );

    /* 4. Handle auto probe: probe follows cursor to current def */
    let zipper =
      if (autoprobe_mode) {
        let z =
          ProbePerform.update_autoprobe(
            ~syntax,
            ~info_map=statics.info_map,
            zipper,
          );
        /* Resolve pending_probe_cursor again since update_autoprobe
           may have set it after editor_effects already ran */
        ProbePerform.resolve_pending_probe_cursor(
          ~dynamics=new_dynamics,
          ~syntax,
          ~info_map=statics.info_map,
          z,
        );
      } else {
        /* If mode is off, clear any existing auto probe */
        ProbePerform.clear_autoprobe(
          ~syntax,
          ~info_map=statics.info_map,
          zipper,
        );
      };

    Model.{
      root,
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
