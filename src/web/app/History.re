open Util;

/* compacted snapshots still hold zippers, frozen ctxs and master
   segments: a deep stack runs out of memory on large programs */
let capped_undo_stack_size = 250;

/* snapshots drop derived caches (syntax, statics, eval states), which
   would pin memory per edit; restore rebuilds them from the zipper:
   syntax via the mark_old dummy, statics on the next edited calculate,
   results by re-evaluating */
let dummy_syntax =
  lazy(
    Haz3lcore.CachedSyntax.mark_old(
      Haz3lcore.CachedSyntax.init(
        Haz3lcore.Zipper.unzip(
          ~direction=Left,
          [
            Haz3lcore.Piece.Grout({
              id: Haz3lcore.Id.mk(),
              shape: Convex,
            }),
          ],
        ),
      ),
    )
  );

let compact_cell = (c: CellEditor.Model.t): CellEditor.Model.t => {
  editor: {
    editor: {
      ...c.editor.editor,
      /* its own incremental caches: restored editors sharing the dummy's
         would keep evicting each other's */
      syntax: {
        ...Lazy.force(dummy_syntax),
        m_cache: Haz3lcore.Measured.Incr.mk_cache(),
        t_cache: Haz3lcore.MakeTerm.Incr.mk_cache(),
      },
    },
    statics: Haz3lcore.CachedStatics.empty,
    dynamics: Language.Dynamics.Map.empty,
    context_menu: c.editor.context_menu,
  },
  /* what autosave keeps (stepper position, theorem progress) survives
     undo; the value re-evaluates */
  result: EvalResult.Model.unpersist(EvalResult.Model.persist(c.result)),
};

let compact_program = (p: Program.t): Program.t =>
  switch (p) {
  | Whole(e) => Whole(compact_cell(e))
  | Divided(d) => Divided(Divided.compact(compact_cell, d))
  };

let compact_scratch = (m: ScratchMode.Model.t): ScratchMode.Model.t => {
  ...m,
  scratchpads:
    List.map(
      (sp: ScratchMode.Scratchpad.t) =>
        switch (sp.kind) {
        | Code({program, _} as code) => {
            ...sp,
            kind:
              Code({
                ...code,
                program: compact_program(program),
              }),
          }
        | Drv(_) => sp
        },
      m.scratchpads,
    ),
};

let compact = (m: Page.Model.t): Page.Model.t => {
  ...m,
  editors:
    switch (m.editors) {
    | Scratch(sm) => Scratch(compact_scratch(sm))
    | Documentation(sm) => Documentation(compact_scratch(sm))
    | (Tutorial(_) | Exercises(_) | Config(_)) as e => e
    },
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    undo_stack: list(Updated.t(state)),
    redo_stack: list(Updated.t(state)),
  };

  let equal = (===);

  let load = () => {
    current: Page.Store.load(),
    undo_stack: [],
    redo_stack: [],
  };

  let reset = (~font_metrics=?, ()) => {
    current: Page.Model.reset(~font_metrics?, ()),
    undo_stack: [],
    redo_stack: [],
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Page.Update.t;

  /* only slide decks have views to realize; a flag copied out of the
     settings follows them back */
  let realize_view = (~schedule_action: t => unit, m: Page.Model.t) => {
    Language.EvalWorklist.compute_enabled :=
      m.globals.settings.show_incremental_deco;
    switch (m.editors) {
    | Scratch(_)
    | Documentation(_) =>
      schedule_action(Editors(Scratch(Workspace(RealizeView))))
    | Tutorial(_)
    | Exercises(_)
    | Config(_) => ()
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let update =
      (
        ~import_log,
        ~get_log_and,
        ~schedule_action: t => unit,
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) =>
    switch (action) {
    | Globals(Undo) =>
      switch (model.undo_stack) {
      | [] =>
        print_endline("Cannot undo");
        model |> Updated.raise_invalid_action;
      | [x, ...rest] =>
        realize_view(~schedule_action, x.model);
        {
          ...x,
          model: {
            current: Page.carry_views(~from=model.current, x.model),
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: compact(model.current),
              },
              ...model.redo_stack,
            ],
          },
        };
      }
    | Globals(Redo) =>
      switch (model.redo_stack) {
      | [] =>
        print_endline("Cannot redo");
        model |> Updated.raise_invalid_action;
      | [x, ...rest] =>
        realize_view(~schedule_action, x.model);
        {
          ...x,
          model: {
            current: Page.carry_views(~from=model.current, x.model),
            undo_stack: [
              {
                ...x,
                model: compact(model.current),
              },
              ...model.undo_stack,
            ],
            redo_stack: rest,
          },
        };
      }
    | action =>
      let current =
        Page.Update.update(
          ~import_log,
          ~get_log_and,
          ~schedule_action,
          action,
          model.current,
        );
      if (current.historic) {
        let new_stack = [
          {
            ...current,
            model: compact(model.current),
          },
          ...model.undo_stack,
        ];
        /* capped even when cap_undo_stack is off */
        let undo_stack =
          List.filteri((i, _) => i < capped_undo_stack_size, new_stack);
        {
          ...current,
          model: {
            current: current.model,
            undo_stack,
            redo_stack: [],
          },
        };
      } else {
        {
          ...current,
          model: {
            current: current.model,
            undo_stack: model.undo_stack,
            redo_stack: model.redo_stack,
          },
        };
      };
    };

  let calculate =
      (
        ~schedule_action: t => unit,
        ~is_edited: bool,
        ~dynamics,
        model: Model.t,
      )
      : Model.t => {
    let current =
      model.current
      |> Page.Update.calculate(~schedule_action, ~is_edited, ~dynamics);
    /* Undo/redo depth for the Editor & Memory panel. Ordered after the calculate
       above, which syncs PerfMetrics' gating for this frame. */
    PerfMetrics.record_history(
      ~undo=model.undo_stack,
      ~redo=model.redo_stack,
    );
    {
      current,
      undo_stack: model.undo_stack,
      redo_stack: model.redo_stack,
    };
  };
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    Page.View.view(~get_log_and, ~inject, model.current);
  };
};
