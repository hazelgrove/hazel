open Virtual_dom.Vdom;
open Util;

/* Focus module for code editors.
 *
 * Provides effect-based keyboard handling where each editor handles
 * its own key events directly. Unhandled events bubble to the parent
 * (Page.re) for page-level shortcuts like undo/redo.
 *
 * The escape callback is called when arrow keys hit an editor boundary
 * (caret at Outer position with no neighbor on that side), allowing
 * the parent to navigate to adjacent cells or projectors.
 *
 * NOTE on escape direction: the direction parameter means "which side
 * to escape TO", matching the arrow key direction. Left arrow at the
 * start of an editor calls escape(Left). Getting this backwards causes
 * the caret to jump to the wrong side of adjacent projectors.
 *
 * NOTE on tabindex/scroll: Key.handler adds tabindex(0) which makes
 * the element focusable. Arrow keys will scroll by default — call
 * Prevent_default on handled arrow key events to avoid this. */
module Focus = {
  let handle_key_event =
      (
        ~handle: Key.t => option('action),
        ~inject: 'action => Effect.t(unit),
        ~escape: Direction.t => Effect.t(unit),
        ~is_at_boundary: Direction.t => bool,
        key: Key.t,
      )
      : Effect.t(unit) => {
    /* 1. Check for arrow key escape at editor boundaries FIRST.
     *    Must be before the normal handler because Keyboard.handle_key_event
     *    always returns Some for arrow keys (Move actions), which would
     *    prevent escape from ever triggering. */
    switch (key) {
    | {key: D("ArrowLeft"), shift: Up, meta: Up, ctrl: Up, alt: Up, _}
        when is_at_boundary(Left) =>
      Effect.Many([Effect.Prevent_default, escape(Left)])
    | {key: D("ArrowRight"), shift: Up, meta: Up, ctrl: Up, alt: Up, _}
        when is_at_boundary(Right) =>
      Effect.Many([Effect.Prevent_default, escape(Right)])
    | _ =>
      /* 2. Normal editor key handling */
      switch (handle(key)) {
      | Some(action) =>
        Effect.Many([
          Effect.Prevent_default,
          Effect.Stop_propagation,
          inject(action),
        ])
      | None => Effect.Ignore
      }
    };
  };
};
