/* Document-level click-outside handler for the editor context menu.
 * See MenuListener for the shared machinery. */

include MenuListener.Make({
  let menu_class = "context-menu";
  let supports_keys = false;
  let scroll_into_view = false;
});
