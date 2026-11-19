/* Document-level dismissal for the probe sample dropdown (alt-click menu). */
include MenuListener.Make({
  let menu_class = "sample-context-menu";
  let supports_keys = false;
  let scroll_into_view = false;
  /* Drawer-mode dropdowns are position:fixed and re-anchor only on render,
   * so they lag during scroll — dismiss instead of tracking. */
  let close_on_scroll = true;
});
