open Util;

let capped_undo_stack_size = 250;

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

  /* Undo and redo stay in the current mode, and on the current lesson
     (tutorial or exercise slide). Scratch and documentation slides are
     parts of one document: adding or deleting one stays undoable. */
  let same_slide = (a: Model.state, b: Model.state): bool =>
    switch (a.editors, b.editors) {
    | (Tutorial(_), Tutorial(_))
    | (Exercises(_), Exercises(_)) =>
      Editors.Model.editor_key(a.editors)
      == Editors.Model.editor_key(b.editors)
    | (Scratch(_), Scratch(_))
    | (Documentation(_), Documentation(_))
    | (Config(_), Config(_)) => true
    | _ => false
    };

  /* Entries are whole pages, so restoring one would also roll back the
     other slides' carets and evaluation results (their ✔). */
  let keep_other_slides =
      (~from: Model.state, state: Model.state): Model.state =>
    switch (from.editors, state.editors) {
    | (Tutorial(f), Tutorial(s)) => {
        ...state,
        editors:
          Tutorial({
            ...s,
            exercises:
              ListUtil.put_nth(
                s.current,
                List.nth(s.exercises, s.current),
                f.exercises,
              ),
          }),
      }
    | _ => state
    };

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
      | [x, ...rest] when same_slide(x.model, model.current) => {
          ...x,
          model: {
            current: x.model |> keep_other_slides(~from=model.current),
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: model.current,
              },
              ...model.redo_stack,
            ],
          },
        }
      | _ =>
        print_endline("Cannot undo");
        model |> Updated.raise_invalid_action;
      }
    | Globals(Redo) =>
      switch (model.redo_stack) {
      | [x, ...rest] when same_slide(x.model, model.current) => {
          ...x,
          model: {
            current: x.model |> keep_other_slides(~from=model.current),
            undo_stack: [
              {
                ...x,
                model: model.current,
              },
              ...model.undo_stack,
            ],
            redo_stack: rest,
          },
        }
      | _ =>
        print_endline("Cannot redo");
        model |> Updated.raise_invalid_action;
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
            model: model.current,
          },
          ...model.undo_stack,
        ];
        let undo_stack =
          if (model.current.globals.settings.cap_undo_stack) {
            List.filteri((i, _) => i < capped_undo_stack_size, new_stack);
          } else {
            new_stack;
          };
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
