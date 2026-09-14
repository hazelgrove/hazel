open Util;

/* 50: even compacted snapshots cost ~5MB each on mega-scale programs
   (zippers, frozen ctxs, master segments); a deep stack still OOMs.
   Proper fix = zipper-level undo entries (docketed). */
let capped_undo_stack_size = 50;

/* Undo snapshots are COMPACTED: a raw Page.Model.t pins its
   generation's derived caches — CachedSyntax (measured/term_data,
   MBs per keystroke on large programs), statics maps, and decoded
   worker eval states. None of that is needed to undo: the zipper is
   the source of truth and everything else recomputes on restore
   (syntax via the mark_old dummy, statics on the next edited
   calculate, results by re-evaluating). Without this, editing a
   mega-scale program leaked hundreds of MB within a few edits. */
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
      syntax: Lazy.force(dummy_syntax),
    },
    statics: Haz3lcore.CachedStatics.empty,
    dynamics: Language.Dynamics.Map.empty,
    context_menu: c.editor.context_menu,
  },
  result: EvalResult.Model.init,
};

let compact_scratch = (m: ScratchMode.Model.t): ScratchMode.Model.t => {
  ...m,
  scratchpads:
    List.map(
      (sp: ScratchMode.Scratchpad.t) =>
        switch (sp.kind) {
        | Code({editor, agent}) => {
            ...sp,
            kind:
              Code({
                editor: compact_cell(editor),
                agent,
              }),
          }
        | Drv(_) => sp
        },
      m.scratchpads,
    ),
  focus:
    Option.map(
      (f: ScratchMode.Model.focus_t) =>
        ScratchMode.Model.{
          ...f,
          f_entries:
            List.map(
              (e: ScratchMode.Model.stack_entry) =>
                ScratchMode.Model.{
                  ...e,
                  e_header: compact_cell(e.e_header),
                  e_body: compact_cell(e.e_body),
                },
              f.f_entries,
            ),
        },
      m.focus,
    ),
};

let compact = (m: Page.Model.t): Page.Model.t => {
  ...m,
  editors:
    switch (m.editors) {
    | Scratch(sm) => Scratch(compact_scratch(sm))
    | Documentation(sm) => Documentation(compact_scratch(sm))
    | (Tutorial(_) | Exercises(_)) as e => e
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
            current: x.model,
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: compact(model.current),
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
            current: x.model,
            undo_stack: [
              {
                ...x,
                model: compact(model.current),
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
            model: compact(model.current),
          },
          ...model.undo_stack,
        ];
        /* ALWAYS capped: unbounded full-model history was the other
           half of the mega-scale OOM (the setting used to gate this) */
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
    current:
      model.current
      |> Page.Update.calculate(~schedule_action, ~is_edited, ~dynamics),
    undo_stack: model.undo_stack,
    redo_stack: model.redo_stack,
  };
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    Page.View.view(~get_log_and, ~inject, model.current);
  };
};
