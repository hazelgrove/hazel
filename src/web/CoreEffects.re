open Util;

/* Installs the web implementations of effect handlers that the core
   libraries expose as injection points (so haz3lcore/language stay
   free of DOM/JS dependencies). Called once at startup from Main. */

let install = (): unit => {
  /* DOM focus for step-into / probe navigation */
  Haz3lcore.ProbePerform.FocusEffect.handler :=
    fun
    | Editor => {
        JsUtil.focus_clipboard_shim();
        true;
      }
    | Cell => JsUtil.focus_active_cell()
    | Probe(probe_id) =>
      switch (JsUtil.get_elem_by_id_opt(Id.cls(probe_id))) {
      | Some(elem) =>
        elem##focus;
        true;
      | None => false
      };

  /* Wall-clock for probe sample timestamps */
  TimeUtil.now_ms := JsUtil.precise_timestamp;

  /* Per-kind projector focus behavior (DOM focus callbacks) */
  ProjectorViews.install();
};
