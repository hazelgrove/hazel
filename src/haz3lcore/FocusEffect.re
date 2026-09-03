open Util;
open Language;

/* Scheduled DOM focus, run from Main.re's after_display hook.

   Lives in its own module rather than inside ProbePerform because
   projector implementations need it too, and ProbePerform sits above
   them in the dependency graph (ProbePerform -> CachedSyntax ->
   ProjectorInfo -> ProjectorInit -> the implementations). It depends on
   nothing but Id and JsUtil. */
/* Scheduled focus for probe or editor elements after step-into.
 * This ref is set when step-into resolves and cleared when focus is executed.
 * We use a ref (not model state) because DOM focus must happen AFTER render,
 * and we can't dispatch actions from after_display without causing loops. */
type target =
  | Editor
  | Cell
  | Probe(Id.t);

let scheduled: ref(option(target)) = ref(None);

/* Schedule DOM focus on a probe element (called from resolve_pending_focus) */
let schedule = (probe_id: Id.t): unit => {
  scheduled := Some(Probe(probe_id));
};

/* Schedule DOM focus on the main editor (called from step_into_sample) */
let schedule_editor = (): unit => {
  scheduled := Some(Editor);
};

/* Schedule DOM focus on the active code-editor cell (called after a
   sidebar jump, which moves the model selection to a different cell
   without moving DOM focus). */
let schedule_cell = (): unit => {
  scheduled := Some(Cell);
};

/* Execute any scheduled focus (called from Main.re after_display).
 * Returns whether focus was executed. */
let execute = (): bool =>
  switch (scheduled^) {
  | Some(Editor) =>
    scheduled := None;
    JsUtil.focus_clipboard_shim();
    true;
  | Some(Cell) =>
    scheduled := None;
    JsUtil.focus_active_cell();
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
