/* Deferred DOM focus restoration for probe elements.
 *
 * The DOM update from a virtual-dom render happens AFTER all the
 * synchronous action processing for a frame. So if an action mutates
 * state that causes a focused DOM node to be unmounted (e.g. drawer-
 * mode toggle moves the focusable .live-offside between the offside
 * and `below` slots), calling `elem.focus()` during action handling
 * targets the about-to-be-removed element and the focus is lost when
 * the render finishes.
 *
 * `schedule` stashes a target in a ref; Main.re's `after_display`
 * hook calls `execute` once the new DOM is in place. We use a ref
 * (not model state) because dispatching actions from after_display
 * would loop.
 *
 * Extracted from ProbePerform so ProbeProj (which needs it for the
 * drawer-mode toggle) can call it without creating a dependency
 * cycle through the projector machinery.
 */
open Util;

type target =
  | Editor
  | Probe(Id.t);

let scheduled: ref(option(target)) = ref(None);

let schedule = (probe_id: Id.t): unit => {
  scheduled := Some(Probe(probe_id));
};

let schedule_editor = (): unit => {
  scheduled := Some(Editor);
};

let execute = (): bool =>
  switch (scheduled^) {
  | Some(Editor) =>
    scheduled := None;
    /* Focus the editor element itself (not the page-level clipboard
       shim) so the caret is restored — the shim only captures keys,
       it does not satisfy the `.code-editor:focus` caret CSS. */
    JsUtil.focus_active_editor();
    true;
  | Some(Probe(probe_id)) =>
    scheduled := None;
    let elem_id = Id.cls(probe_id);
    switch (JsUtil.get_elem_by_id_opt(elem_id)) {
    | Some(elem) =>
      elem##focus;
      true;
    | None => false
    };
  | None => false
  };
