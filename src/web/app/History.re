open Util;

let capped_undo_stack_size = 1000;

/* Undo entries are whole page models. Restoring one rebuilds its syntax
   (marked stale) and statics (forced on undo/redo), so entries drop them.
   The newest few keep their evaluation results, so undoing recent edits
   doesn't re-evaluate; older entries drop those too. */
let entries_with_results = 8;

let stale_syntax =
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

let slim_cell =
    (~drop_results: bool, c: CellEditor.Model.t): CellEditor.Model.t => {
  editor: {
    ...c.editor,
    editor: {
      ...c.editor.editor,
      syntax: Lazy.force(stale_syntax),
    },
    statics: Haz3lcore.CachedStatics.empty,
    dynamics: drop_results ? Language.Dynamics.Map.empty : c.editor.dynamics,
  },
  result: drop_results ? EvalResult.Model.init : c.result,
};

let slim_editor = (e: Haz3lcore.Editor.t): Haz3lcore.Editor.t => {
  ...e,
  syntax: Lazy.force(stale_syntax),
};

/* Tutorial stitching reads terms from the zippers, and calculate rebuilds
   stale syntax in both the cells and the part editors. */
let slim_tutorial =
    (~drop_results: bool, m: TutorialMode.Model.t): TutorialMode.Model.t => {
  ...m,
  editors: Tutorial.map(m.editors, slim_editor, slim_editor),
  cells:
    Tutorial.map_stitched((_, c) => slim_cell(~drop_results, c), m.cells),
};

let slim_scratch =
    (~drop_results: bool, m: ScratchMode.Model.t): ScratchMode.Model.t => {
  ...m,
  scratchpads:
    List.map(
      (sp: ScratchMode.Scratchpad.t) =>
        switch (sp.kind) {
        | Code(code) => {
            ...sp,
            kind:
              Code({
                ...code,
                editor: slim_cell(~drop_results, code.editor),
              }),
          }
        | Drv(_) => sp
        },
      m.scratchpads,
    ),
};

/* Exercise models are left whole. */
let slim = (~drop_results=false, m: Page.Model.t): Page.Model.t => {
  ...m,
  editors:
    switch (m.editors) {
    | Scratch(sm) => Scratch(slim_scratch(~drop_results, sm))
    | Documentation(sm) => Documentation(slim_scratch(~drop_results, sm))
    | Config(cm) =>
      Config({
        ...cm,
        configs:
          List.map(
            ((ty, c)) => (ty, slim_cell(~drop_results, c)),
            cm.configs,
          ),
      })
    | Tutorial(tm) =>
      Tutorial({
        ...tm,
        exercises: List.map(slim_tutorial(~drop_results), tm.exercises),
      })
    | Exercises(_) as e => e
    },
};

/* Restored entries have no statics: compute them now, not after the typing
   debounce, so types and errors don't blink off. */
let restore = (m: Page.Model.t): Page.Model.t => {
  CodeWithStatics.StaticsDebounce.force_on_next := true;
  m;
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
      | [x, ...rest] => {
          ...x,
          model: {
            current: restore(x.model),
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: slim(model.current),
              },
              ...model.redo_stack,
            ],
          },
        }
      }
    | Globals(Redo) =>
      switch (model.redo_stack) {
      | [] =>
        print_endline("Cannot redo");
        model |> Updated.raise_invalid_action;
      | [x, ...rest] => {
          ...x,
          model: {
            current: restore(x.model),
            undo_stack: [
              {
                ...x,
                model: slim(model.current),
              },
              ...model.undo_stack,
            ],
            redo_stack: rest,
          },
        }
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
            model: slim(model.current),
          },
          ...model.undo_stack,
        ];
        /* the entry just past the newest few drops its results too; the
           cap bounds the rest */
        let undo_stack =
          new_stack
          |> List.filteri((i, _) => i < capped_undo_stack_size)
          |> List.mapi((i, e: Updated.t(Page.Model.t)) =>
               i == entries_with_results
                 ? {
                   ...e,
                   model: slim(~drop_results=true, e.model),
                 }
                 : e
             );
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
