open Virtual_dom.Vdom;
open Util;

/* Focus module for code editors.
 *
 * This module provides the new keyboard event handling pattern where
 * handlers return Effect.t(unit) directly (instead of option(Update.t))
 * and support escape callbacks for boundary navigation.
 *
 * Usage:
 *   Key.handler(~f=EditorView.Focus.handle_key_event(
 *     ~inject, ~escape, model
 *   ))
 *
 * The escape callback is called when arrow keys hit an editor boundary
 * (caret at Outer position with no further movement possible),
 * allowing the parent to navigate to adjacent cells or projectors.
 *
 * NOTE on escape direction: the direction parameter means "which side
 * to escape TO", matching the arrow key direction. Left arrow at the
 * start of an editor calls escape(Left). Getting this backwards causes
 * the caret to jump to the wrong side of adjacent projectors.
 *
 * NOTE on tabindex/scroll: Key.handler adds tabindex(0) which makes
 * the element focusable. This means arrow keys will scroll the element
 * by default. Call Prevent_default on handled arrow key events to
 * avoid unwanted scrolling. */
module Focus = {
  let handle_key_event =
      (
        ~inject: CodeEditable.Update.t => Effect.t(unit),
        ~escape: Direction.t => Effect.t(unit),
        model: CodeEditable.Model.t,
        key: Key.t,
      )
      : Effect.t(unit) => {
    /* First try the existing CodeEditable handler chain
     * (context menu → projector handoff → Keyboard.handle_key_event) */
    switch (
      CodeEditable.Selection.handle_key_event(~selection=(), model, key)
    ) {
    | Some(action) =>
      Effect.Many([Effect.Prevent_default, Effect.Stop_propagation, inject(action)])
    | None =>
      /* Check for arrow key escape at editor boundaries */
      switch (key) {
      | {key: D("ArrowLeft"), shift: Up, meta: Up, ctrl: Up, alt: Up, _} =>
        let z = model.editor.state.zipper;
        switch (z.caret, Haz3lcore.Siblings.neighbors(z.relatives.siblings)) {
        | (Outer, (None, _)) =>
          Effect.Many([Effect.Prevent_default, escape(Left)])
        | _ => Effect.Ignore
        };
      | {key: D("ArrowRight"), shift: Up, meta: Up, ctrl: Up, alt: Up, _} =>
        let z = model.editor.state.zipper;
        switch (z.caret, Haz3lcore.Siblings.neighbors(z.relatives.siblings)) {
        | (Outer, (_, None)) =>
          Effect.Many([Effect.Prevent_default, escape(Right)])
        | _ => Effect.Ignore
        };
      | _ => Effect.Ignore
      }
    };
  };

  let get_cursor_info =
      (
        ~inject: CodeEditable.Update.t => Effect.t(unit),
        model: CodeEditable.Model.t,
      )
      : list(ContextualAction.t) => {
    /* Projector-specific contextual actions based on indicated piece */
    let projector_actions =
      switch (Haz3lcore.Indicated.piece(model.editor.state.zipper)) {
      | Some(_) => [
          ContextualAction.mk(
            ~hotkey="alt+f",
            ~mdIcon="camera",
            ~section="Projection",
            ~action=
              inject(
                Perform(Project(SetIndicated(Specific(Fold)))),
              ),
            "Fold",
          ),
        ]
      | None => []
      };
    projector_actions;
  };
};
