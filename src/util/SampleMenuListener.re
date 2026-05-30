/* Document-level dismissal for the probe sample dropdown (the per-sample
 * context menu opened by alt-click). Mirrors ContextMenuListener but with no
 * keyboard handling (the probe projector's own key_handler owns keys) and no
 * scroll-into-view (the dropdown is anchored to its sample). See MenuListener. */
include MenuListener.Make({
  let menu_class = "sample-context-menu";
  let supports_keys = false;
  let scroll_into_view = false;
});
